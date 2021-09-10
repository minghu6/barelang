//! Meta Grammar Processor

use std::fmt;
use std::fmt::Write;
use std::error::Error;

use indexmap::{IndexMap, IndexSet, indexmap, indexset};
use itertools::Itertools;

use crate::{VERBOSE, VerboseLv, error::TrapCode};

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

    pub fn to_fst_set_sym(&self) -> FstSetSym {
        FstSetSym::Sym(self.name().to_string())
    }

    pub fn to_foll_set_sym(&self) -> FollSetSym {
        FollSetSym::Sym(self.name().to_string())
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        PredSetSym::Sym(self.name().to_string())
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

impl GramProd {
    pub fn lookahead(&self, fstsets: &FstSets, follsets: &FollSets) -> IndexSet<PredSetSym> {
        let mut res = indexset! {};

        match &self.rhstr {
            GramSymStr::Str(rhsym_vec) => {
                let fstsym = rhsym_vec.first().unwrap();
                let thefstset = fstsets.get(fstsym).unwrap();

                res.extend(
                    thefstset
                    .iter()
                    .map(|fstsym| {
                        if let Some(predsetsym) = fstsym.to_pred_set_sym() {
                            vec![predsetsym]
                        }
                        else {
                            let thefollset
                            = follsets.get(&self.lfsym).unwrap();

                            thefollset
                            .into_iter()
                            .map(|follsym| follsym.to_pred_set_sym() )
                            .collect_vec()
                        }
                    })
                    .flatten()
                );
            },
            GramSymStr::Epsilon => {
                let thefollset
                = follsets.get(&self.lfsym).unwrap();

                res.extend(
                thefollset
                .into_iter()
                .map(|follsym| follsym.to_pred_set_sym())
                );
            }
        }

        res
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
    pub fn find_prod(&self, lfs: &GramSym)
    -> Vec<GramProd>
    {
        self.prods.iter().filter( |prod: &&GramProd| {
            &prod.lfsym == lfs
        }).cloned().collect_vec()
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

////////////////////////////////////////////////////////////////////////////////
//// First Sets

pub type FstSets = IndexMap<GramSym, IndexSet<FstSetSym>>;
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FstSetSym {
    Sym(String),
    Epsilon,
}

impl FstSetSym {
    pub fn is_epsilon(&self) -> bool {
        match self {
            Self::Epsilon => true,
            _ => false,
        }
    }

    pub fn to_pred_set_sym(&self) -> Option<PredSetSym> {
        match self {
            Self::Epsilon => None,
            Self::Sym(value) => Some(PredSetSym::Sym(value.clone())),
        }
    }
}

impl fmt::Display for FstSetSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(value) => write!(f, "{}", value),
            Self::Epsilon => write!(f, "ε"),
        }
    }
}

pub fn display_fstsets(fst_sets: &FstSets) {
    for (lhs_sym, prodset) in fst_sets.iter() {
        print!("{}: \n", lhs_sym);

        print!("{}\n",
            prodset.iter().map(|x| format!("| {}", x)).collect_vec().join("\n")
        );

        println!();
    }
}

impl Gram {
    pub fn first_sets(&self) -> FstSets {
        let mut first_sets = self
            .syms()
            .into_iter()
            .map(|sym| match &sym {
                GramSym::NonTerminal(_) => (sym.clone(), indexset! {}),
                GramSym::Terminal(_) => (
                    sym.clone(),
                    indexset! { FstSetSym::Sym(sym.name().to_string()) },
                ),
            })
            .collect();

        let mut round = 0usize;
        loop {
            round += 1;
            if _calc_first_sets_turn(&self.prods, &mut first_sets) {
                break;
            }
        }

        if round > 2 {
            if unsafe { VERBOSE >= VerboseLv::V1 } {
                println!(
                    "{}: calc firstsets additional rounds: {}",
                    self.name(),
                    round - 1
                );
            }
        }

        // filter terminal entry
        // first_sets.into_iter()
        //     .filter(|(k, _v)| !k.is_terminal())
        //     .collect()
        first_sets
    }
}

///```none
///     1.If X is terminal, then FIRST(X) is {X}.
///     2.If X → ε is a production, then add ε to FIRST(X).
///     3.If X is nonterminal and X →Y1 Y2 ... Yk.
///       从求取First(Y1)开始,如果Y1的某个产生式有ε,就继续求取First(Y2)......,
///       如果整个串都已经耗尽了,就把ε也加入.
///```
fn _calc_first_sets_turn(
    productions: &IndexSet<GramProd>,
    first_sets: &mut IndexMap<GramSym, IndexSet<FstSetSym>>,
) -> bool {
    let mut stable = true;

    for prod in productions.iter() {
        let x = &prod.lfsym;
        let str = &prod.rhstr;
        let mut x_first_set = first_sets.get(x).unwrap().clone();
        let x_first_set_old_size = x_first_set.len();

        match str {
            GramSymStr::Epsilon => {
                x_first_set.insert(FstSetSym::Epsilon);
            }

            GramSymStr::Str(normal_str) => {
                match x {
                    GramSym::Terminal(_) => {
                        x_first_set.insert(FstSetSym::Sym(x.name().to_string()));
                    }

                    GramSym::NonTerminal(_) => {
                        let mut str_iter = normal_str.iter();

                        loop {
                            let cur_sym;
                            if let Some(_sym) = str_iter.next() {
                                cur_sym = _sym;
                            } else {
                                x_first_set.insert(FstSetSym::Epsilon);
                                break;
                            }

                            let cur_sym_first_set = first_sets.get(cur_sym).unwrap();
                            x_first_set.extend(cur_sym_first_set.clone().into_iter());

                            if cur_sym_first_set.contains(&FstSetSym::Epsilon) {
                                // continue
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }

        if x_first_set_old_size < x_first_set.len() {
            stable = false;
        }

        // 更新first_sets
        first_sets
            .get_mut(x)
            .unwrap()
            .extend(x_first_set.into_iter());
    }

    stable
}


////////////////////////////////////////////////////////////////////////////////
//// Follow Sets

pub type FollSets = IndexMap<GramSym, IndexSet<FollSetSym>>;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FollSetSym {
    Sym(String),
    EndMarker,
}

impl FollSetSym {
    pub fn to_fst_set_sym(&self) -> Option<FstSetSym> {
        match self {
            Self::EndMarker => None,
            Self::Sym(value) => Some(FstSetSym::Sym(value.clone())),
        }
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        match self {
            Self::EndMarker => PredSetSym::EndMarker,
            Self::Sym(value) => PredSetSym::Sym(value.clone()),
        }
    }
}

impl fmt::Display for FollSetSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(value) => write!(f, "{}", value),
            Self::EndMarker => write!(f, "$"),
        }
    }
}

pub fn display_follsets(foll_sets: &FollSets) {
    for (lhs_sym, prodset) in foll_sets.iter() {
        print!("{}: \n", lhs_sym);

        print!("{}\n",
            prodset.iter().map(|x| format!("| {}", x)).collect_vec().join("\n")
        );

        println!();
    }
}

impl Gram {
    pub fn follow_sets(&self, first_sets: &FstSets) -> FollSets {
        let mut foll_sets = self
            .nonterm_syms()
            .into_iter()
            .map(|sym| (sym.clone(), indexset! {}))
            .collect::<FollSets>();

        foll_sets
            .get_mut(self.start_sym().unwrap())
            .unwrap()
            .insert(FollSetSym::EndMarker);

        let mut round = 0usize;
        loop {
            round += 1;
            if _calc_follow_sets_turn(&self.prods, &mut foll_sets, first_sets) {
                break;
            }
        }
        if round > 2 {
            if unsafe { VERBOSE >= VerboseLv::V1 } {
                println!(
                    "{}: calc followsets additional rounds: {}",
                    self.name(),
                    round - 1
                );
            }
        }
        foll_sets
    }
}

/// ```none
/// 1. Place $ in FOLLOW(S), where S is the start symbol and $ is the input right endmarker.
/// 2. If there is a production A -> αΒβ, then everything in FIRST(β), except for ε, is placed in FOLLOW(B).
/// 3. If there is a production A -> αΒ, or a production A -> αΒβ where FIRST(β) contains ε(i.e., β -> ε),
///    then everything in FOLLOW(A) is in FOLLOW(B).
///    如同计算First一样迭代，如果整个串都已经耗尽了,就把ε也加入Follow(B).
/// ```
fn _calc_follow_sets_turn(
    productions: &IndexSet<GramProd>,
    follow_sets: &mut FollSets,
    first_sets: &FstSets,
) -> bool {
    let mut stable = true;

    for prod in productions.iter() {
        let x = &prod.lfsym;
        let str = &prod.rhstr;
        let x_follow_set = follow_sets.get(x).unwrap().clone();

        if str.is_normal() && x.is_nonterminal() {
            let normal_str = str.get_normal().unwrap();
            let lastpos = normal_str.len() - 1;
            for (i, str_x) in normal_str.iter().enumerate().rev() {
                if str_x.is_terminal() {
                    continue;
                }

                if !follow_sets.contains_key(str_x) {
                    assert!(false, "get {} None", str_x);
                }

                let mut here_set = follow_sets.get(str_x).unwrap().clone();
                let here_set_old_size = here_set.len();

                // apply follow rule 2+
                let mut j = i;
                while j < lastpos {
                    let next_first = first_sets.get(&normal_str[j + 1]).unwrap();
                    here_set.extend(
                        next_first
                            .clone()
                            .into_iter()
                            .filter(|x| !x.is_epsilon())
                            .map(|x| match x {
                                FstSetSym::Sym(value) => FollSetSym::Sym(value),
                                _ => unreachable!(),
                            }),
                    );

                    // 没有epsilon转换就停止穿透
                    if !next_first.iter().any(|x| x.is_epsilon()) {
                        break;
                    }
                    j += 1;
                }
                // apply follow rule 3
                if (i + 1..normal_str.len())
                    .map(|j| &normal_str[j])
                    .all(|x| first_sets.get(x).unwrap().contains(&FstSetSym::Epsilon))
                {
                    here_set.extend(x_follow_set.clone().into_iter());

                    if stable && here_set_old_size < here_set.len() {
                        stable = false;
                    }
                }

                // rewrite
                follow_sets
                    .get_mut(str_x)
                    .unwrap()
                    .extend(here_set.into_iter());
            }
        }
    }

    stable
}

////////////////////////////////////////////////////////////////////////////////
//// Predication Sets

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum PredSetSym {
    Sym(String),
    EndMarker,
}

impl PredSetSym {
    pub fn is_sym(&self) -> bool {
        match self {
            Self::Sym(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for PredSetSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(value) => write!(f, "{}", value),
            Self::EndMarker => write!(f, "$"),
        }
    }
}

pub struct PredSet {
    predsets: IndexMap<GramSym, IndexMap<PredSetSym, GramProd>>
}

impl PredSet {
    pub fn predict(&self, lfsym: &GramSym, la: PredSetSym) -> Option<&GramProd> {
        if let Some(deriv_pred_set) = self.predsets.get(lfsym) {
            if let Some(prod) = deriv_pred_set.get(&la) {
                Some(prod)
            }
            else {
                None
            }
        }
        else {
            None
        }
    }
}

impl fmt::Display for PredSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (deriv, deriv_map) in self.predsets.iter() {
            writeln!(f, "{}:", deriv)?;

            for (predsym, prod) in deriv_map.iter() {
                writeln!(f, "  | {}: {}", predsym, prod.rhstr)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

impl Gram {
    pub fn prediction_sets(&self, fstsets: &FstSets, follsets: &FollSets) -> PredSet {
        let dt = self.derivation_tree();
        let mut predsets = indexmap! {};

        predsets.extend(
            dt.into_iter().map(|(sym, prodset)| {
                let mut deriv_map = indexmap! {};

                deriv_map.extend(
                prodset
                    .into_iter()
                    .map(|prod| {
                        prod.lookahead(fstsets, follsets)
                        .into_iter()
                        .map(move |la| (la, prod.clone()))
                    })
                    .flatten()
                );

                (sym, deriv_map)
            })
        );

        PredSet { predsets }
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Derivation Tree

pub type DerivationTree = IndexMap<GramSym, IndexSet<GramProd>>;

impl Gram {
    /// Flat Form => Nested Form
    pub fn derivation_tree(&self) -> DerivationTree {
        let mut dt = indexmap! {};

        for prod in self.prods.iter() {
            let prodset
            = dt.entry(prod.lfsym.clone()).or_insert(indexset! {});

            prodset.insert(prod.clone());
        }

        dt
    }
}

pub fn display_dt(dt: &DerivationTree, fstsets: &FstSets, follsets: &FollSets)
-> Result<String, Box<dyn Error>> {
    let mut s = String::new();

    for (lfsym, deriv_prods) in dt.into_iter() {
        writeln!(&mut s, "{}:", lfsym)?;

        for prod in deriv_prods {
            writeln!(&mut s, "  -> {}", prod.rhstr)?;

            let laset = prod.lookahead(fstsets, follsets);
            for predsym in laset.iter() {
                writeln!(&mut s, "  | {}", predsym)?;
            }

            writeln!(&mut s)?;
        }

        writeln!(&mut s)?;
    }

    Ok(s)
}

////////////////////////////////////////////////////////////////////////////////
//// Grammar Check

impl Gram {
    pub fn duplicate_dt(&self, fstsets: &FstSets, follsets: &FollSets) -> DerivationTree {
        let dt = self.derivation_tree();
        let mut dup_dt = indexmap! {};

        for (deriv_sym, deriv_items) in dt.iter() {
            for mut group_vec in deriv_items
            .iter()
            .combinations(2) {
                let y = group_vec.pop().unwrap();
                let x = group_vec.pop().unwrap();

                let xlaset = x.lookahead(fstsets, follsets);
                let ylaset = y.lookahead(fstsets, follsets);

                if !xlaset.is_disjoint(&ylaset) {
                    let dup_items
                    = dup_dt.entry(deriv_sym.clone()).or_insert(indexset![]);

                    dup_items.insert(x.clone());
                    dup_items.insert(y.clone());
                }
            }
        }

        dup_dt
    }

    pub(crate) fn do_check1(&self, fstsets: &FstSets, follsets: &FollSets) -> Result<(), Box<dyn Error>> {
        let dup_dt = self.duplicate_dt(&fstsets, &follsets);

        let mut s = String::new();

        if dup_dt.len() > 0 {
            writeln!(&mut s, "Duplicated LL Rule:\n")?;

            writeln!(&mut s, "{}", display_dt(&dup_dt, &fstsets, &follsets)?)?;

            return Err(TrapCode::AmbigousLLRule(s).emit_box_err())
        }

        Ok(())
    }

    pub fn do_check(&self) -> Result<(), Box<dyn Error>> {
        let fstsets = self.first_sets();
        let follsets = self.follow_sets(&fstsets);

        self.do_check1(&fstsets, &follsets)
    }
}




#[cfg(test)]
#[allow(non_snake_case)]
#[allow(unused_imports)]
#[allow(unused_variables)]
mod test {
    use indexmap::{IndexMap, IndexSet, indexmap, indexset};
    use itertools::Itertools;

    use super::Gram;

    #[test]
    fn test_grammar_first_follow_any_set() {
        use crate::rules::{
            barelang_gram
        };
        use super::{
            GramSymStr,
            FstSetSym,
            GramSym,
            GramProd,
            display_dt,
            display_fstsets,
            display_follsets
        };


        let barelang = barelang_gram();

        let fstsets = barelang.first_sets();
        let follsets = barelang.follow_sets(&fstsets);

        // println!("FIRST:\n");
        // display_fstsets(&fstsets);
        // println!();
        // println!("FOLLOW:\n");
        // display_follsets(&follsets);

        // let dt = barelang.derivation_tree();
        // println!("DT: {}", display_dt(&dt, &fstsets, &follsets).unwrap());

        match barelang.do_check1(&fstsets, &follsets) {
            Ok(()) => {},
            Err(err) => {
                println!("{}", err);
            }
        }

        let predsets
        = barelang.prediction_sets(&fstsets, &follsets);

    }
}

