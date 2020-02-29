//! Code to evaluate a term or type. These are relatively simple because of the lack
//! of operations: on terms, the only possible thing is to resolve an identifier. For
//! types, we can resolve identifiers and apply type functions. It's up to the caller
//! to ensure that the term/type they're trying to evaluate is well typed/kinded, otherwise
//! the evaluated term/type can get "stuck" (or could recurse infinitely)
//!
//! Also of note, is that these two functions are called pretty liberally whenever needed,
//! but if it ever becomes expensive, we can precompute the evaled version of each term/type.
use super::errors::Error;
use crate::flat::*;
use crate::raw::Spanned;

// TODO: we can either pass in evaled terms/types or have the functions recurse
// themselves
// TODO: cloning?

pub fn eval_term<'a>(
    term: Spanned<&'a Term>,
    scope: &'a Libraries,
) -> Result<Spanned<&'a Term>, Error> {
    match &term.value {
        Term::Identifier(name) => scope.get_term(term.span.wrap(name)).map(|term| term.into()),
        _ => Ok(term),
    }
}

// NOTE the behaviors are a little inconsistent here: for identifiers, we recursively
// evaluate fully. For type function application, we only do one "step" of eval (i.e. we
// apply the function to its args, and then stop there, since all current use cases only
// care about the top level type). The post condition is simply that the result is not an
// identifier or a type function application
pub fn eval_type(ty: Spanned<&Type>, scope: &Libraries) -> Result<Spanned<Type>, Error> {
    match ty.value {
        Type::Identifier(name) => eval_type(scope.get_type(ty.span.wrap(name))?.into(), scope),
        Type::TypeSubstitution(TypeSubstitution {
            ref func,
            layout,
            constraint,
        }) => match eval_type(func.into(), scope)?.value {
            Type::Array(Array { element_type, size }) => Ok(ty.span.wrap(Type::Array(Array {
                element_type: element_type.or(layout.clone()),
                size: size.or(constraint.clone()),
            }))),
            Type::Vector(Vector {
                element_type,
                bounds,
            }) => Ok(ty.span.wrap(Type::Vector(Vector {
                element_type: element_type.or(layout.clone()),
                bounds: bounds.or(constraint.clone()),
            }))),
            Type::Str(Str { bounds }) => Ok(ty.span.wrap(Type::Str(Str {
                bounds: bounds.or(constraint.clone()),
            }))),
            _ => Ok(ty.span.wrap(ty.value.clone())),
        },
        _ => Ok(ty.span.wrap(ty.value.clone())),
    }
}
