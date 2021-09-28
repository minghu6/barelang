use std::error::Error;
use std::rc::Rc;

use indexmap::IndexMap;
use m6stack::{stack, Stack};

use crate::datair::*;
use crate::datalsp::*;
use crate::error::TrapCode;
use crate::gram::*;
use crate::lexer::Token;
use crate::rules::{barelang_gram, bopprecmap};
use crate::*;
use crate::utils::*;

thread_local! {
    pub static BOP_PREC_MAP: IndexMap<BaBOp, usize> = bopprecmap();
}

pub struct Parser {
    cursor: usize,
    history_record: Vec<usize>,
    scope_level: usize,
    tokens: Vec<Token>,
    gram: Gram,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            cursor: 0,
            history_record: vec![],
            tokens,
            scope_level: 0,
            gram: barelang_gram(),
        }
    }

    pub fn restart(&mut self) {
        self.cursor = 0;
    }

    fn advance(&mut self) -> Result<&Token, Box<dyn Error>> {
        if self.is_end() {
            Err(TrapCode::UnfinishedDerivation.emit_box_err())
        } else {
            let nxt_tok = &self.tokens[self.cursor];
            self.cursor += 1;

            Ok(nxt_tok)
        }
    }

    fn try_peek1(&self) -> Result<&Token, Box<dyn Error>> {
        if self.cursor >= self.tokens.len() {
            Err(TrapCode::UnfinishedDerivation.emit_box_err())
        } else {
            Ok(&self.tokens[self.cursor])
        }
    }

    fn try_peek2(&self) -> Result<&Token, Box<dyn Error>> {
        if self.cursor + 1 >= self.tokens.len() {
            Err(TrapCode::UnfinishedDerivation.emit_box_err())
        } else {
            Ok(&self.tokens[self.cursor + 1])
        }
    }

    fn peek1_t(&self, tokname: &str) -> bool {
        if let Ok(tok) = self.try_peek1() {
            tok.name() == tokname
        } else {
            false
        }
    }

    fn peek2_t(&self, tokname: &str) -> bool {
        if let Ok(tok) = self.try_peek2() {
            tok.name() == tokname
        } else {
            false
        }
    }

    fn is_end(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    #[allow(unused)]
    fn is_tail(&self) -> bool {
        self.cursor + 1 == self.tokens.len()
    }

    fn record(&mut self) {
        self.history_record.push(self.cursor);
    }

    fn restore(&mut self) {
        self.cursor = self.history_record.pop().unwrap();
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Debug mode

    #[inline]
    fn is_try_mode(&self) -> bool {
        self.history_record.len() > 0
    }

    #[inline]
    fn verbose_enable(&self) -> bool {
        verbose_enable_v2() && !self.is_try_mode()
    }

    #[inline]
    fn debug_running(&mut self, prod_name: &str, prod_idx: usize, step: usize) {
        if self.verbose_enable() {
            let prod_vec = self.gram.idx(prod_name);
            if prod_idx >= prod_vec.len() {
                panic!("{}[{}] index out of bounds", prod_name, prod_idx)
            }

            let running
            = self.gram.idx(prod_name)[prod_idx].running(step);

            if running.is_start() {
                self.debug_push();
            }

            let indent = "  ".repeat(self.scope_level - 1);
            let cur_tok_str = match self.try_peek1() {
                Ok(cur_tok) => format!("{}", cur_tok),
                Err(_) => "<$>".to_owned()
            };

            println!(
                "{}({}){} | {}",
                indent,
                self.scope_level - 1,
                running,
                cur_tok_str
            );

            if running.is_end() {
                self.debug_pop();
            }
            if running.prod.is_terminal_prod() {
                self.debug_pop();
            }
        }
    }

    #[inline]
    fn debug_push(&mut self) {
        self.scope_level += 1;
    }

    #[inline]
    fn debug_pop(&mut self) {
        self.scope_level -= 1;
    }

    pub fn parse(&mut self) -> Result<ModuleLisp, Box<dyn Error>> {
        self.parse_prog()
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Entry point

    /// ```none
    /// Prog:
    /// 0 -> BlockStmts;
    /// ```
    fn parse_prog(&mut self) -> Result<ModuleLisp, Box<dyn Error>> {
        Ok(match self.try_peek1()?.name().as_str() {
            "<lbrace>" => {
                todo!()  // parse Block
            }
            _ => {
                self.debug_running("[Prog]", 0, 0);

                let blkstmts = self.parse_block_stmts()?;
                self.debug_running("[Prog]", 0, 1);

                ModuleLisp::BlockStmts(blkstmts)
            }
        })
    }

    /// ```none
    /// BlockStmts:
    ///   0 -> BlockStmtsSpan TailOptionExpr;
    ///   2 -> ε;
    /// ```
    fn parse_block_stmts(&mut self) -> Result<LspBlockStmts, Box<dyn Error>> {
        let tok1st = self.try_peek1()?;

        Ok(match tok1st.name().as_str() {
            "<rbrace>" => {
                self.debug_running("[BlockStmts]", 1, 0);

                LspBlockStmts::empty()
            },
            _ => {
                self.debug_running("[BlockStmts]", 0, 0);

                let block_stmts_span = self.parse_block_stmts_span()?;
                self.debug_running("[BlockStmts]", 0, 1);

                let tail_expr = self.parse_tail_option_expr()?;
                self.debug_running("[BlockStmts]", 0, 2);

                LspBlockStmts {
                    block_stmts: block_stmts_span,
                    tail_expr,
                }
            }
        })
    }

    /// ```none
    /// BlockStmtsSpan:
    ///   0 -> BlockStmt BlockStmtsSpan;
    ///   1 -> ε;
    /// ```
    fn parse_block_stmts_span(&mut self) -> Result<Vec<LspBlockStmtRef>, Box<dyn Error>> {
        self.record();

        if (self.parse_expr().is_ok() && self.reach_brace_end())
        || self.reach_brace_end()  // Follow (Block)
        || self.is_end()          // Follow (Prog -> BlockStmts)
        {
            self.restore();
            self.debug_running("[BlockStmtsSpan]", 1, 0);

            return Ok(vec![])
        }

        self.restore();

        self.debug_running("[BlockStmtsSpan]", 0, 0);

        let blkstmt = self.parse_block_stmt()?;
        self.debug_running("[BlockStmtsSpan]", 0, 1);

        let blkstmtsrem = self.parse_block_stmts_span()?;
        self.debug_running("[BlockStmtsSpan]", 0, 2);

        Ok(ht![blkstmt | blkstmtsrem])
    }

    /// ```none
    /// BlockStmt:
    ///   0 -> Item;  // defn
    ///   1 -> Stmt;  // a=2;
    ///   2 -> Block; // {...}
    /// ```
    fn parse_block_stmt(&mut self) -> Result<LspBlockStmtRef, Box<dyn Error>> {
        let tok1st = self.try_peek1()?;

        Ok(LspBlockStmtRef::new(match tok1st.name().as_str() {
            "<f>" => {
                // Item-defn
                self.debug_running("[BlockStmt]", 0, 0);

                let item = self.parse_item()?;
                self.debug_running("[BlockStmt]", 0, 1);

                LspBlockStmt::Item(item)
            },
            "<lbrace>" => {
                // Block
                self.debug_running("[BlockStmt]", 2, 0);

                let block = self.parse_block()?;
                self.debug_running("[BlockStmt]", 2, 1);

                LspBlockStmt::Block(block)
            }
            _ => {
                self.debug_running("[BlockStmt]", 1, 0);

                let stmt = self.parse_stmt()?;
                self.debug_running("[BlockStmt]", 1, 1);

                LspBlockStmt::Stmt(stmt)
            }
        }))
    }

    /// ```none
    /// TailOptionExpr:
    ///   0 -> Expr;
    ///   1 -> ε;
    /// ```
    fn parse_tail_option_expr(&mut self) -> Result<Option<LspExpr>, Box<dyn Error>> {
        Ok(if self.reach_brace_end() || self.is_end() {
            self.debug_running("[TailOptionExpr]", 1, 0);
            None
        } else {
            self.debug_running("[TailOptionExpr]", 0, 0);

            let expr = self.parse_expr()?;
            self.debug_running("[TailOptionExpr]", 0, 1);

            Some(expr)
        })
    }

    /// ```none
    /// Item:
    ///   0 -> Defn;
    /// ```
    fn parse_item(&mut self) -> Result<LspItem, Box<dyn Error>> {
        let tok1st = self.try_peek1()?;

        Ok(match tok1st.name().as_str() {
            "<f>" => {
                self.debug_running("[Item]", 0, 0);

                let bafn = self.parse_defn()?;
                self.debug_running("[Item]", 0, 1);

                LspItem::DefFun(bafn)
            }
            _ => return Err(TrapCode::UnexpectedToken(tok1st, "[Item]").emit_box_err()),
        })
    }

    /// ```none
    /// Stmt:
    ///   0 -> semi;
    ///   1 -> Expr semi;
    ///   2 -> Declare semi;
    /// ```
    fn parse_stmt(&mut self) -> Result<LspStmt, Box<dyn Error>> {
        self.record();
        if self.parse_declare().is_ok() {
            self.restore();
            self.debug_running("[Stmt]", 2, 0);

            let declare = self.parse_declare()?;
            self.debug_running("[Stmt]", 2, 1);

            self.parse_t("<semi>")?;
            self.debug_running("[Stmt]", 2, 2);

            return Ok(LspStmt::Declare(Rc::new(declare)))
        }
        self.restore();

        let tok1st = self.try_peek1()?;

        Ok(match tok1st.name().as_str() {
            "<semi>" => {
                self.debug_running("[Stmt]", 0, 0);

                self.advance()?;
                self.debug_running("[Stmt]", 0, 1);

                LspStmt::Empty
            }
            _ => {
                self.debug_running("[Stmt]", 1, 0);

                let expr = self.parse_expr()?;
                self.debug_running("[Stmt]", 1, 1);

                self.parse_t("<semi>")?;
                self.debug_running("[Stmt]", 1, 2);

                LspStmt::Expr(expr)
            }
        })
    }

    /// ```none
    /// Declare:
    ///   0 -> id eq Expr;
    ///   1 -> TypedId eq Expr;
    /// ```
    fn parse_declare(&mut self) -> Result<LspDeclare, Box<dyn Error>> {
        if self.peek2_t("<colon>") {
            self.debug_running("[Declare]", 1, 0);

            let decid = self.parse_typed_id()?;
            self.debug_running("[Declare]", 1, 1);

            self.parse_t("<eq>")?;
            self.debug_running("[Declare]", 1, 2);

            let decval = self.parse_expr()?;
            self.debug_running("[Declare]", 1, 3);

            return Ok(LspDeclare {
                id: decid,
                val: decval
            })
        }

        self.debug_running("[Declare]", 0, 0);

        let decid = self.parse_t_id()?;
        self.debug_running("[Declare]", 0, 1);

        self.parse_t("<eq>")?;
        self.debug_running("[Declare]", 0, 2);

        let decval = self.parse_expr()?;
        self.debug_running("[Declare]", 0, 3);

        Ok(LspDeclare {
            id: decid,
            val: decval
        })
    }

    /// ```none
    /// Expr:
    ///   0 -> Pri ExprRem;
    ///   1 -> FunCall;
    /// ```
    fn parse_expr(&mut self) -> Result<LspExpr, Box<dyn Error>> {
        // check if it's a funcall;
        self.record();

        if let Ok(_) = self.parse_id() {
            if self.reach_paren_start() {
                self.restore();
                self.debug_running("[Expr]", 1, 0);

                let funcall = self.parse_fun_call()?;
                self.debug_running("[Expr]", 1, 1);

                return Ok(LspExpr::FunCall(funcall));
            }
        }

        self.restore();

        self.debug_running("[Expr]", 0, 0);

        let pri = self.parse_pri()?;
        self.debug_running("[Expr]", 0, 1);

        let (rem_pris, rem_bops) = self.parse_expr_rem()?;
        self.debug_running("[Expr]", 0, 2);

        // Combine the LspExpr
        // Resort the Infix expression by precedence (they don't change left most expr srcloc)
        let mut out_bop_stack = Stack::from(rem_bops);
        let mut out_pri_stack = Stack::from(rem_pris);

        let mut staging_bop_stack: Stack<BaBOp> = stack![];
        let mut expr_stack = stack![LspExpr::Pri(pri)];

        // out_bop_stack would be same with out_pri_stack in size.
        while !(out_bop_stack.is_empty() && staging_bop_stack.is_empty()) {
            // Reduce
            if out_bop_stack.is_empty()
                || !staging_bop_stack.is_empty()
                    && staging_bop_stack.peek().unwrap().precedence()
                        // >= for left associative operator
                        >= out_bop_stack.peek().unwrap().precedence()
            {
                let bop = staging_bop_stack.pop().unwrap();
                let rhexpr = expr_stack.pop().unwrap();
                let lfexpr = expr_stack.pop().unwrap();

                expr_stack.push(LspExpr::TwoPri(bop, Rc::new(lfexpr), Rc::new(rhexpr)));
            }
            // Shift
            else {
                staging_bop_stack.push(out_bop_stack.pop().unwrap());
                expr_stack.push(LspExpr::Pri(out_pri_stack.pop().unwrap()));
            }
        }

        Ok(expr_stack.pop().unwrap())
    }

    /// ```none
    /// ExprRem:
    ///   0 -> BOp Pri ExprRem;  // 中缀表达式仅限基本操作符, 这是为了方便起见
    ///   1 -> ε;
    /// ```
    fn parse_expr_rem(&mut self) -> Result<(Vec<LspPri>, Vec<BaBOp>), Box<dyn Error>> {
        self.record();

        if self.parse_bop().is_ok() {
            self.restore();
            self.debug_running("[ExprRem]", 0, 0);

            let bop = self.parse_bop()?;
            self.debug_running("[ExprRem]", 0, 1);

            let pri = self.parse_pri()?;
            self.debug_running("[ExprRem]", 0, 2);

            let (pris, bops) = self.parse_expr_rem()?;
            self.debug_running("[ExprRem]", 0, 3);

            Ok((ht![pri | pris], ht![bop | bops]))
        }
        else {
            self.restore();
            self.debug_running("[ExprRem]", 1, 0);

            Ok((vec![], vec![]))
        }
    }

    /// ```none
    /// BOp:
    ///   0 -> add;
    ///   1 -> sub;
    ///   2 -> mul;
    ///   3 -> div;
    ///   4 -> percent;
    ///   5 -> dot;
    /// ```
    fn parse_bop(&mut self) -> Result<BaBOp, Box<dyn Error>> {
        let tok1st = self.advance()?;

        Ok(match tok1st.name().as_str() {
            "<add>" => {
                self.debug_running("[BOp]", 0, 0);
                BaBOp::Add
            },
            "<sub>" => {
                self.debug_running("[BOp]", 1, 0);
                BaBOp::Sub
            },
            "<mul>" => {
                self.debug_running("[BOp]", 2, 0);
                BaBOp::Mul
            },
            "<div>" => {
                self.debug_running("[BOp]", 3, 0);
                BaBOp::Div
            },
            "<percent>" => {
                self.debug_running("[BOp]", 4, 0);
                BaBOp::Mod
            },
            _ => {
                return Err(TrapCode::UnexpectedToken(tok1st, "[BOp]").emit_box_err())
            },
        })
    }


    /// ```none
    /// FunCall:
    ///   0 -> Id Arguments;
    /// ```
    fn parse_fun_call(&mut self) -> Result<LspFunCall, Box<dyn Error>> {
        self.debug_running("[FunCall]", 0, 0);

        let funid = self.parse_id()?;
        self.debug_running("[FunCall]", 0, 1);

        let arguments = self.parse_arguments()?;
        self.debug_running("[FunCall]", 0, 2);

        Ok(LspFunCall {
            name: funid,
            args: arguments,
        })
    }

    /// ```none
    /// Arguments:
    ///   0 -> lparen ArgumentList rparen;
    ///   1 -> ε;
    /// ```
    fn parse_arguments(&mut self) -> Result<Vec<LspExpr>, Box<dyn Error>> {
        self.debug_running("[Arguments]", 0, 0);

        self.parse_t("<lparen>")?;
        self.debug_running("[Arguments]", 0, 1);

        let expr_list = self.parse_argument_list()?;
        self.debug_running("[Arguments]", 0, 2);

        self.parse_t("<rparen>")?;
        self.debug_running("[Arguments]", 0, 3);

        Ok(expr_list)
    }

    /// ```none
    /// ArgumentList:
    ///   0 -> Expr ArgumentListRem;
    ///   1 -> ε;
    /// ```
    fn parse_argument_list(&mut self) -> Result<Vec<LspExpr>, Box<dyn Error>> {
        if self.reach_paren_end() {
            self.debug_running("[ArgumentList]", 1, 0);
            return Ok(vec![]);
        }

        self.debug_running("[ArgumentList]", 0, 0);

        let argument = self.parse_expr()?;
        self.debug_running("[ArgumentList]", 0, 1);

        let argument_list_rem = self.parse_argument_list_rem()?;
        self.debug_running("[ArgumentList]", 0, 2);

        Ok(ht![argument | argument_list_rem])
    }


    /// ```none
    /// ArgumentListRem:
    ///   0 -> comma Argument ArgumentListRem;
    ///   1 -> ε;
    /// ```
    fn parse_argument_list_rem(&mut self) -> Result<Vec<LspExpr>, Box<dyn Error>> {
        if self.reach_paren_end() {
            self.debug_running("[ArgumentListRem]", 1, 0);

            return Ok(vec![]);
        }

        self.debug_running("[ArgumentListRem]", 0, 0);

        self.parse_t("<comma>")?;
        self.debug_running("[ArgumentListRem]", 0, 1);

        let argument = self.parse_expr()?;
        self.debug_running("[ArgumentListRem]", 0, 2);

        let argument_list_rem = self.parse_argument_list_rem()?;
        self.debug_running("[ArgumentListRem]", 0, 3);

        Ok(ht![argument | argument_list_rem])
    }

    /// ```none
    /// Pri:
    ///   0 -> Lit;
    ///   1 -> Id;
    ///   2 -> lparen Expr rparen;
    /// ```
    fn parse_pri(&mut self) -> Result<LspPri, Box<dyn Error>> {
        let tok1st = self.try_peek1()?;

        Ok(match tok1st.name().as_str() {
            "<id>" | "<splid>" => {
                self.debug_running("[Pri]", 1, 0);

                let id = self.parse_id()?;
                self.debug_running("[Pri]", 1, 1);

                LspPri::Id(Rc::new(id))
            },
            "<lparen>" => {
                self.debug_running("[Pri]", 2, 0);

                self.parse_t("<lparen>")?;
                self.debug_running("[Pri]", 2, 1);

                let expr = self.parse_expr()?;
                self.debug_running("[Pri]", 2, 2);

                self.parse_t("<rparen>")?;
                self.debug_running("[Pri]", 2, 3);

                LspPri::Expr(Rc::new(expr))
            }
            _ => {
                self.debug_running("[Pri]", 0, 0);

                let lit = self.parse_lit()?;
                self.debug_running("[Pri]", 0, 1);

                LspPri::Lit(lit)
            }
        })
    }

    /// ```none
    /// Id:
    ///   0 -> id;
    ///   1 -> splid id;
    /// ```
    fn parse_id(&mut self) -> Result<BaId, Box<dyn Error>> {
        let splid = if self.peek1_t("<splid>") {
            self.debug_running("[Id]", 1, 0);

            let splid = self.parse_t_splid()?;
            self.debug_running("[Id]", 1, 1);

            Some(splid)
        } else {
            None
        };

        let mut id = self.parse_t_id()?;

        if splid.is_some() {
            self.debug_running("[Id]", 1, 2);
        }
        else {
            self.debug_running("[Id]", 0, 0);
        }

        id.splid = splid;

        Ok(id)
    }

    /// ```none
    /// Defn:
    ///   0 -> f colon id Parameters colon DataType Block;
    /// ```
    fn parse_defn(&mut self) -> Result<LspDefFun, Box<dyn Error>> {
        let mut auto = gen_counter();
        self.debug_running("[Defn]", 0, auto());

        self.parse_t("<f>")?;
        self.debug_running("[Defn]", 0, auto());

        self.parse_t("<colon>")?;
        self.debug_running("[Defn]", 0, auto());

        let bafn = self.parse_t_id()?;
        self.debug_running("[Defn]", 0, auto());

        let parameters = self.parse_parameters()?;
        self.debug_running("[Defn]", 0, auto());

        self.parse_t("<colon>")?;
        self.debug_running("[Defn]", 0, auto());

        let ret = self.parse_data_ty()?;
        self.debug_running("[Defn]", 0, auto());

        let body = self.parse_block()?;
        self.debug_running("[Defn]", 0, auto());

        Ok(LspDefFun::from((bafn, parameters, ret, body)))
    }

    /// ```none
    /// Parameters:
    ///   0 -> lparen ParameterList rparen;
    /// ```
    fn parse_parameters(&mut self) -> Result<Vec<BaParam>, Box<dyn Error>> {
        self.debug_running("[Parameters]", 0, 0);

        self.parse_t("<lparen>")?;
        self.debug_running("[Parameters]", 0, 1);

        let params = self.parse_parameter_list()?;
        self.debug_running("[Parameters]", 0, 2);

        self.parse_t("<rparen>")?;
        self.debug_running("[Parameters]", 0, 3);

        Ok(params)
    }

    /// ```none
    /// ParameterList:
    ///   0 -> Parameter ParameterListRem;
    ///   1 -> ε;
    /// ```
    fn parse_parameter_list(&mut self) -> Result<Vec<BaParam>, Box<dyn Error>> {
        if self.reach_paren_end() {
            self.debug_running("[ParameterList]", 0, 0);

            return Ok(vec![]);
        }

        self.debug_running("[ParameterList]", 0, 0);

        let param1st = self.parse_parameter()?;
        self.debug_running("[ParameterList]", 0, 1);

        let paramsrem = self.parse_parameter_list_rem()?;
        self.debug_running("[ParameterList]", 0, 2);

        Ok(ht![param1st | paramsrem])
    }

    /// ```none
    /// Parameter:
    ///   0 -> TypedId;
    /// ```
    fn parse_parameter(&mut self) -> Result<BaParam, Box<dyn Error>> {
        Ok(BaParam::from(self.parse_typed_id()?))
    }

    /// ```none
    /// TypedId:
    ///   0 -> id colon DataType;
    /// ```
    fn parse_typed_id(&mut self) -> Result<BaId, Box<dyn Error>> {
        self.debug_running("[TypedId]", 0, 0);

        let mut id = self.parse_t_id()?;
        self.debug_running("[TypedId]", 0, 1);

        self.parse_t("<colon>")?;
        self.debug_running("[TypedId]", 0, 2);

        let ty = self.parse_data_ty()?;
        self.debug_running("[TypedId]", 0, 3);

        id.ty = Some(ty);

        Ok(id)
    }

    /// ```none
    /// ParameterListRem:
    ///   0 -> comma Parameter ParameterListRem;
    ///   1 -> ε;
    /// ```
    fn parse_parameter_list_rem(&mut self) -> Result<Vec<BaParam>, Box<dyn Error>> {
        if self.reach_paren_end() {
            self.debug_running("[ParameterListRem]", 1, 0);

            return Ok(vec![]);
        }

        self.debug_running("[ParameterListRem]", 0, 0);

        let parameter = self.parse_parameter()?;
        self.debug_running("[ParameterListRem]", 0, 1);

        let parameter_list_rem = self.parse_parameter_list_rem()?;
        self.debug_running("[ParameterListRem]", 0, 2);

        Ok(ht![parameter | parameter_list_rem])
    }

    /// ```none
    /// DataType:
    ///   0 -> id;
    ///   1 -> int;
    ///   2 -> u64;
    ///   3 -> u8;
    ///   4 -> float;
    ///   5 -> str;
    /// ```
    fn parse_data_ty(&mut self) -> Result<BaType, Box<dyn Error>> {
        let tok1st = self.advance()?;

        Ok(match tok1st.name().as_str() {
            "<int>" => {
                self.debug_running("[DataType]", 1, 0);

                BaType::Int
            },
            "<u8>" => {
                self.debug_running("[DataType]", 3, 0);

                BaType::U8
            },
            "<float>" => {
                self.debug_running("[DataType]", 4, 0);

                BaType::Float
            },
            "<str>" => {
                self.debug_running("[DataType]", 5, 0);

                BaType::Str
            },
            "<id>" => {
                self.debug_running("[DataType]", 0, 0);

                let id = self.parse_t_id()?;
                BaType::Customized(Rc::new(id))
            },
            _ => {
                return Err(
                    TrapCode::UnexpectedToken(tok1st, "[DataType]").emit_box_err()
                )
            }
        })
    }

    /// ```none
    /// Block:
    ///   0 -> lbrace BlockStmts rbrace;
    /// ```
    fn parse_block(&mut self) -> Result<LspBlock, Box<dyn Error>> {
        self.debug_running("[Block]", 0, 0);

        self.parse_t("<lbrace>")?;
        self.debug_running("[Block]", 0, 1);

        let block_stmts = self.parse_block_stmts()?;
        self.debug_running("[Block]", 0, 2);

        self.parse_t("<rbrace>")?;
        self.debug_running("[Block]", 0, 3);

        Ok(LspBlock { block_stmts })
    }

    /// ```none
    /// Lit:
    ///   0 -> intlit;
    ///   1 -> dqstr;
    /// ```
    fn parse_lit(&mut self) -> Result<BaLit, Box<dyn Error>> {
        let tok1st = self.try_peek1()?;

        let lit = match tok1st.name().as_str() {
            "<intlit>" => {
                self.debug_running("[Lit]", 0, 0);
                self.parse_t_intlit()?
            },
            "<dqstr>" => {
                self.debug_running("[Lit]", 1, 0);
                self.parse_t_dqstr()?
            },
            _ => return Err(TrapCode::UnexpectedToken(tok1st, "[Lit]").emit_box_err()),
        };

        Ok(lit)
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Helper Function

    fn reach_paren_start(&self) -> bool {
        if let Ok(tok) = self.try_peek1() {
            tok.name() == "<lparen>"
        } else {
            false
        }
    }

    fn reach_paren_end(&self) -> bool {
        if let Ok(tok) = self.try_peek1() {
            tok.name() == "<rparen>"
        } else {
            false
        }
    }

    fn reach_brace_end(&self) -> bool {
        if let Ok(tok) = self.try_peek1() {
            tok.name() == "<rbrace>"
        } else {
            false
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Parse Token

    fn parse_t(&mut self, tokname: &str) -> Result<&Token, Box<dyn Error>> {
        let tok1st = self.advance()?;

        if tok1st.name() == tokname {
            Ok(tok1st)
        } else {
            Err(TrapCode::UnexpectedToken(tok1st, tokname).emit_box_err())
        }
    }

    fn parse_t_splid(&mut self) -> Result<BaSplId, Box<dyn Error>> {
        let tok1st = self.advance()?;

        if tok1st.name() != "<splid>" {
            return Err(TrapCode::UnexpectedToken(tok1st, tok1st.name().as_str()).emit_box_err());
        }

        Ok(match tok1st.value() {
            "rs#" => BaSplId::RS,
            _ => unimplemented!("{}", tok1st),
        })
    }

    fn parse_t_id(&mut self) -> Result<BaId, Box<dyn Error>> {
        let tok1st = self.advance()?;

        if tok1st.name() != "<id>" {
            return Err(TrapCode::UnexpectedToken(tok1st, tok1st.name().as_str()).emit_box_err());
        }

        Ok(BaId {
            name: tok1st.value().to_owned(),
            splid: None,
            ty: None,
            loc: tok1st.loc(),
        })
    }

    fn parse_t_intlit(&mut self) -> Result<BaLit, Box<dyn Error>> {
        let tok = self.advance()?;
        let mut tokv = tok.value();

        let is_neg = if tokv.starts_with("-") { true } else { false };

        // Handle Hex Number Literal
        let bai32val = if tokv.contains("0x") {
            if tokv.starts_with("-") {
                tokv = tokv.trim_start_matches("-");
            } else if tokv.starts_with("+") {
                tokv = tokv.trim_start_matches("+");
            }

            tokv = tokv.trim_start_matches("0x");

            if is_neg {
                -i32::from_str_radix(tokv, 16).unwrap()
            } else {
                i32::from_str_radix(tokv, 16).unwrap()
            }
        } else {
            tokv.parse::<i32>().unwrap()
        };

        Ok(BaLit::I32(BaI32 {
            val: bai32val,
            loc: tok.loc(),
        }))
    }

    fn parse_t_dqstr(&mut self) -> Result<BaLit, Box<dyn Error>> {
        let tok = self.advance()?;
        let strv = String::from(tok.value());

        Ok(BaLit::Str(BaStr {
            val: strv,
            loc: tok.loc(),
        }))
    }
}

#[cfg(test)]
mod test {

    use std::path::PathBuf;

    use crate::lexer::{tokenize, SrcFileInfo};
    use crate::manual_parser::Parser;
    use crate::{VerboseLv, VERBOSE};

    #[test]
    fn test_parse_format_msg() {
        let prod = ["<(>", "[Expr]", "<)>"];

        println!("{:?}", prod);
    }

    #[test]
    fn test_parser() {
        unsafe { VERBOSE = VerboseLv::V2 };

        let file = SrcFileInfo::new(PathBuf::from("./examples/exp1.ba")).unwrap();

        let tokens = tokenize(&file);
        let tokens = tokens
            .into_iter()
            .filter(|tok| tok.name() != "<sp>" && !tok.name().contains("comment"))
            .collect();

        let mut parser = Parser::new(tokens);
        let ml = parser.parse().unwrap();

        println!("ML: {:#?}", ml);
    }
}
