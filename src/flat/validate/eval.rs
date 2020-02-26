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

pub fn eval_type(ty: Spanned<&Type>, scope: &Libraries) -> Result<Spanned<Type>, Error> {
    match ty.value {
        Type::Identifier(name) => eval_type(scope.get_type(ty.span.wrap(name))?.into(), scope),
        Type::TypeSubstitution(TypeSubstitution {
            ref func,
            layout,
            constraint,
        }) => {
            // we don't recursively eval layout and constraint because generally
            // we only care about the top level type
            // NOTE: this code assumes this ty is well kinded
            match eval_type(func.into(), scope)?.value {
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
            }
        }
        _ => Ok(ty.span.wrap(ty.value.clone())),
    }
}
