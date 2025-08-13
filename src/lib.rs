use std::rc::Rc;

pub fn unwrap_rc<T>(rc: Rc<T>, fallback: impl FnOnce(Rc<T>) -> T) -> T {
    match Rc::try_unwrap(rc) {
        Ok(inner) => inner,
        Err(rc) => fallback(rc),
    }
}
