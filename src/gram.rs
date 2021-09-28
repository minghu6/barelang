//! Meta Grammar Processor

use std::fmt;

use indexmap::{ IndexSet, indexset};
use itertools::Itertools;


////////////////////////////////////////////////////////////////////////////////
//// Grammar Symbol

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum GramSym {
    Terminal(String),
    NonTerminal(String),
}

impl GramSym {
    pub fn name(&self) -> &str {
        match self {
            Self::Terminal(name) => name,
            Self::NonTerminal(name) => name,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            Self::Terminal(_) => true,
            Self::NonTerminal(_) => false,
        }
    }

    pub fn is_nonterminal(&self) -> bool {
        !self.is_terminal()
    }
}

impl fmt::Display for GramSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sym_id = if self.is_terminal() {
            format!("<{}>", self.name())
        } else {
            format!("[{}]", self.name())
        };

        write!(f, "{}", sym_id)
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Grammar Symbol String

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum GramSymStr {
    Str(Vec<GramSym>),
    Epsilon,
}

impl GramSymStr {
    pub fn is_normal(&self) -> bool {
        match self {
            Self::Str(_) => true,
            _ => false,
        }
    }

    pub fn is_epsilon(&self) -> bool {
        match self {
            Self::Epsilon => true,
            _ => false,
        }
    }

    pub fn get_normal(&self) -> Option<&Vec<GramSym>> {
        match self {
            Self::Str(normal) => Some(normal),
            _ => None,
        }
    }

    /// This symstr contains just one terminal symbol
    pub fn is_terminal_symstr(&self) -> bool {
        match self {
            Self::Str(normal) => {
                let fstsym = normal.first().unwrap();
                fstsym.is_terminal() && normal.len() == 1
            },
            _ => false,
        }
    }
}

impl fmt::Display for GramSymStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Epsilon => write!(f, "epsilon"),
            Self::Str(str_vec) => {
                for (i, sym) in str_vec.iter().enumerate() {
                    let sym_id = if sym.is_terminal() {
                        format!("<{}>", sym.name())
                    } else {
                        format!("[{}]", sym.name())
                    };

                    if i < str_vec.len() - 1 {
                        write!(f, "{} ", sym_id)?
                    } else {
                        write!(f, "{}", sym_id)?
                    }
                }
                Ok(())
            }
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Grammar Production (derivation branch)

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct GramProd {
    pub lfsym: GramSym,
    pub rhstr: GramSymStr
}

impl GramProd {
    pub fn running(&self, idx: usize) -> GramProdRunning {
        GramProdRunning::new(self.clone(), idx)
    }

    /// This production only contains one terminal symbol
    pub fn is_terminal_prod(&self) -> bool {
        self.rhstr.is_terminal_symstr()
    }
}

impl fmt::Display for GramProd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.lfsym, self.rhstr)
    }
}

impl fmt::Debug for GramProd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Grammar Production Running (aka production with dot)

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct GramProdRunning {
    pub prod: GramProd,
    pub pos: usize,  // dot pos in right side gram str
}

impl GramProdRunning {
    pub fn new(prod: GramProd, pos: usize) -> Self {
        Self {
            prod,
            pos
        }
    }

    pub fn symstr(&self) -> &GramSymStr {
        &self.prod.rhstr
    }

    /// the right hand side symbol immediately follows dot
    /// None indicated that dot points to the end
    pub fn rhs_sym(&self) -> Option<&GramSym> {
        let symstr = &self.prod.rhstr;

        if let GramSymStr::Str(normal_vec) = symstr {
            if self.pos < normal_vec.len() {
                Some(&normal_vec[self.pos])
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn do_inc(&mut self) {
        if let Some(_) = self.rhs_sym() {
            self.pos += 1;
        }
    }

    pub fn inc(&self) -> Self {
        let mut other = self.clone();
        other.do_inc();
        other
    }

    pub fn lhs_sym(&self) -> &GramSym {
        &self.prod.lfsym
    }

    pub fn prod_len(&self) -> usize {
        match &self.prod.rhstr {
            GramSymStr::Str(symstr) => symstr.len(),
            GramSymStr::Epsilon => 0
        }
    }

    pub fn is_end(&self) -> bool {
        self.pos >= self.prod_len()
    }

    pub fn is_start(&self) -> bool {
        self.pos == 0
    }

    pub fn to_gram_prod(&self) -> GramProd {
        self.prod.to_owned()
    }
}

impl From<GramProd> for GramProdRunning {
    fn from(prod: GramProd) -> Self {
        Self {
            prod,
            pos: 0
        }
    }
}

impl fmt::Debug for GramProdRunning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: ", &self.prod.lfsym)?;

        if let GramSymStr::Str(symstr) = &self.prod.rhstr {
            let mut str_vec
            = symstr
            .iter()
            .map(|sym| format!("{}", sym))
            .collect_vec();

            str_vec.insert(self.pos, "\u{00B7}".to_string());
            write!(f, "{}", str_vec.join(" "))
        } else {
            write!(f, "ε")
        }
    }
}

impl fmt::Display for GramProdRunning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Grammar

#[derive(Debug, Clone)]
pub struct Gram {
    name: String,
    prods: IndexSet<GramProd>,
}

impl Gram {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            prods: indexset! {},
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    // move method
    pub fn extend_gram(&mut self, income_gram: Gram) {
        self.extend(income_gram.into_iter());
    }

    pub fn insert_prod(&mut self, prod: GramProd) {
        self.prods.insert(prod);
    }

    pub fn get_prod_index(&self, prod_ind: usize) -> Option<&GramProd> {
        self.prods.get_index(prod_ind)
    }

    pub fn nonterm_syms(&self) -> Vec<GramSym> {
        self.prods
            .clone()
            .into_iter()
            .map(|prod| prod.lfsym)
            .collect::<Vec<GramSym>>()
    }

    pub fn term_syms(&self) -> Vec<GramSym> {
        let mut term_syms = IndexSet::<GramSym>::new();

        for prod in self.prods.iter() {
            if let GramSymStr::Str(normal_str) = &prod.rhstr {
                for str_sym in normal_str.iter() {
                    if str_sym.is_terminal() {
                        term_syms.insert(str_sym.clone());
                    }
                }
            }
        }

        term_syms.into_iter().collect()
    }

    pub fn syms(&self) -> Vec<GramSym> {
        let mut all_syms = IndexSet::<GramSym>::new();

        for prod in self.prods.iter() {
            all_syms.insert(prod.lfsym.clone());

            if let GramSymStr::Str(normal_str) = &prod.rhstr {
                all_syms.extend(normal_str.clone().into_iter());
            }
        }

        all_syms.into_iter().collect()
    }

    pub fn iter(&self) -> indexmap::set::Iter<GramProd> {
        self.prods.iter()
    }

    /// get start symbol， return None if prods is empty.
    pub fn start_sym(&self) -> Option<&GramSym> {
        match self.prods.get_index(0) {
            Some(prod) => Some(&prod.lfsym),
            None => None,
        }
    }

    pub fn start_prod(&self) -> Option<&GramProd> {
        match self.prods.get_index(0) {
            Some(prod) => Some(prod),
            None => None,
        }
    }

    pub fn sym_has_epsilon(&self, sym: &GramSym) -> bool {
        self.prods
            .iter()
            .any(|prod| prod.lfsym == *sym && prod.rhstr == GramSymStr::Epsilon)
    }

    /// 性能上可能应该需要一个Iterator Wrapper
    pub fn find_prod_by_sym(&self, lfs: &GramSym)
    -> Vec<GramProd>
    {
        self.prods.iter().filter( |prod: &&GramProd| {
            &prod.lfsym == lfs
        }).cloned().collect_vec()
    }

    /// "[derivname]"", for an example: [Prog]
    pub fn idx(&self, symname: &str) -> Vec<GramProd> {
        let deriv_name = symname.strip_prefix("[").unwrap().strip_suffix("]").unwrap();

        self.find_prod_by_sym(
            &GramSym::NonTerminal(deriv_name.to_string())
        )
    }
}


impl Extend<GramProd> for Gram {
    fn extend<I: IntoIterator<Item = GramProd>>(&mut self, iter: I) {
        for item in iter {
            self.prods.insert(item);
        }
    }
}

impl std::iter::IntoIterator for Gram {
    type Item = GramProd;
    type IntoIter = indexmap::set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.prods.into_iter()
    }
}



#[cfg(test)]
#[allow(unused_imports)]
#[allow(unused_variables)]
mod test {
    use indexmap::{IndexMap, IndexSet, indexmap, indexset};
    use itertools::Itertools;

    use crate::rules::barelang_gram;

    use super::Gram;

    #[test]
    fn test_format_gram() {
        let gram = barelang_gram();

        println!("{}", gram.idx("Lit")[1]);
    }
}

