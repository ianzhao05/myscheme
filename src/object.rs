use crate::datum::Datum;
use crate::expr::ProcData;

#[derive(Debug, Clone)]
pub enum Object {
    Representable(Datum),
    Procedure(ProcData),
    Void,
}
