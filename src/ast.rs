use super::types;
use super::values;
use heapless::{consts::*, String, Vec};

#[derive(Debug)]
pub enum IUnOp {
    Clz,
    Ctz,
    Popcnt,
}

#[derive(Debug)]
pub enum FUnOp {
    Neg,
    Abs,
    Ceil,
    Floor,
    Trunc,
    Nearest,
    Sqrt,
}

#[derive(Debug)]
pub enum IBinOp {
    Add,
    Sub,
    Mul,
    DivS,
    DivU,
    RemS,
    RemU,
    And,
    Or,
    Xor,
    Shl,
    ShrS,
    ShrU,
    Rotl,
    Rotr,
}

#[derive(Debug)]
pub enum FBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
    CopySign,
}

#[derive(Debug)]
pub enum ITestOp {
    Eqz,
}

#[derive(Debug)]
pub enum IRelOp {
    Eq_,
    Ne,
    LtS,
    LtU,
    GtS,
    GtU,
    LeS,
    LeU,
    GeS,
    GeU,
}

#[derive(Debug)]
pub enum FRelOp {
    Eq_,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug)]
pub enum ConvertOp {
    I32WrapI64,
    I64ExtendUI32,
    I64ExtendSI32,
    Trunc {
        from: types::Float,
        to: types::Int,
        signed: bool,
    },
    Convert {
        from: types::Int,
        to: types::Float,
        signed: bool,
    },
    Reinterpret {
        from: types::Value,
        to: types::Value,
    },
    F32DemoteF64,
    F64PromoteF32,
}

#[derive(Debug)]
pub struct MemOp<T> {
    pub align: u32,
    pub offset: u32,
    pub type_: types::Value,
    pub opt: Option<T>,
}

/// A memory load with optional size and sign
pub type LoadOp = MemOp<(u32, bool)>;

/// A memory store with optional size
pub type StoreOp = MemOp<u32>;

#[derive(Debug)]
pub enum Instr {
    Unreachable,                                                  // trap unconditionally
    Nop,                                                          // do nothing
    Block(Vec<U32, types::Value>, Vec<U32, Instr>),               // execute in sequence
    Loop(Vec<U32, types::Value>, Vec<U32, Instr>),                // loop header
    If(Vec<U32, types::Value>, Vec<U32, Instr>, Vec<U32, Instr>), // conditional
    Br(Index),                                                    // break to n-th surrounding label
    BrIf(Index),                                                  // conditional break
    BrTable(Vec<U32, Index>, Index),                              // indexed break
    Return,                                                       // break from function body
    Call(Index),                                                  // call function
    CallIndirect(Index),                                          // call function through table
    Drop_,                                                        // forget a value
    Select,                                                       // branchless conditional
    GetLocal(Index),                                              // read local variable
    SetLocal(Index),                                              // write local variable
    TeeLocal(Index),             // write local variable and keep value
    GetGlobal(Index),            // read global variable
    SetGlobal(Index),            // write global variable
    Load(LoadOp),                // read memory at address
    Store(StoreOp),              // write memory at address
    CurrentMemory,               // size(linear memory
    GrowMemory,                  // grow linear memory
    Const(values::Value),        // constant
    IUnary(types::Int, IUnOp),   // integer unary numeric operators
    FUnary(types::Float, FUnOp), // floating unary numeric operators
    IBin(types::Int, IBinOp),    // integer binary numeric operators
    FBin(types::Float, FBinOp),  // floating binary numeric operators
    ITest(types::Int, ITestOp),  // integer numeric test
    IRel(types::Int, IRelOp),    // integer numeric comparison
    FRel(types::Float, FRelOp),  // floating numeric comparison
    Convert(ConvertOp),          // conversion
}

pub type Expr = Vec<U32, Instr>;

#[derive(Debug)]
pub struct Module {
    pub types: Vec<U32, types::Func>,
    pub funcs: Vec<U32, Func>,
    pub tables: Vec<U32, Table>,
    pub memories: Vec<U32, Memory>,
    pub globals: Vec<U32, Global>,
    pub elems: Vec<U32, Segment<Index>>, // initial values for tables
    pub data: Vec<U32, Segment<u8>>,     // initial values for memories
    pub start: Option<Index>,            // optionnal index to a start function
    pub imports: Vec<U32, Import>,
    pub exports: Vec<U32, Export>,
}

pub type Index = u32;

#[derive(Debug)]
pub struct Func {
    pub type_index: Index,
    pub locals: Vec<U32, types::Value>,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Table {
    pub type_: types::Table,
}

#[derive(Debug)]
pub struct Memory {
    pub type_: types::Memory,
}

#[derive(Debug)]
pub struct Global {
    pub type_: types::Global,
    pub value: Expr, // NB: Must be constant
}

#[derive(Debug)]
pub struct Segment<T> {
    pub index: Index,
    pub offset: Expr, // NB: Must be constant
    pub init: Vec<U32, T>,
}

#[derive(Debug)]
pub struct Export {
    pub name: String<U32>,
    pub desc: ExportDesc,
}

#[derive(Debug)]
pub enum ExportDesc {
    Func(Index),
    Table(Index),
    Memory(Index),
    Global(Index),
}

#[derive(Debug)]
pub struct Import {
    pub module: String<U32>,
    pub name: String<U32>,
    pub desc: ImportDesc,
}

#[derive(Debug)]
pub enum ImportDesc {
    Func(Index),
    Table(types::Table),
    Memory(types::Memory),
    Global(types::Global),
}

impl Import {
    pub fn type_(&self, module: &Module) -> types::Extern {
        match self.desc {
            ImportDesc::Func(idx) => types::Extern::Func(module.types[idx as usize].clone()),
            ImportDesc::Table(ref t) => types::Extern::Table(t.clone()),
            ImportDesc::Memory(ref t) => types::Extern::Memory(t.clone()),
            ImportDesc::Global(ref t) => types::Extern::Global(t.clone()),
        }
    }
}

// Helper function for tests
impl Module {
    // Right now, we cannot only publish this function for test
    // See https://github.com/rust-lang/rust/issues/45599
    // #[cfg(test)]
    pub fn empty() -> Module {
        Module {
            types: Vec::new(),
            funcs: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
            elems: Vec::new(),
            data: Vec::new(),
            start: None,
            imports: Vec::new(),
            exports: Vec::new(),
        }
    }
}
