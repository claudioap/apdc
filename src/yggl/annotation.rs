use std::rc::Rc;
use crate::yggl::statement::Statement;

pub fn insert_declarations(statements: &mut Vec<Statement>) {
    let mut index = 0;
    let mut declarations = vec![];
    for statement in &*statements {
        if let Statement::Assignment(ref var, _) = statement {
            if !var.is_declared() {
                declarations.push((index, Statement::Declaration(Rc::clone(var))));
                var.set_declared();
            }
        }
        index += 1;
    }

    let mut offset = 0;
    for declaration in declarations {
        statements.insert(declaration.0 + offset, declaration.1);
        offset += 1;
    }
}

pub fn insert_allocations(statements: &mut Vec<Statement>) {
    let mut index = 0;
    let mut allocations = vec![];
    for statement in &*statements {
        if let Statement::StructDef(var, struct_def)  = statement {
            allocations.push((
                index + 1,
                Statement::Allocation(
                    Rc::clone(var),
                    struct_def.get_declaration())));
        }
        index += 1;
    }

    let mut offset = 0;
    for allocation in allocations {
        statements.insert(allocation.0 + offset, allocation.1);
        offset += 1;
    }
}

pub fn propagate_types(statements: &Vec<Statement>){
    loop {
        for statement in &*statements {
            match statement {
                Statement::Assignment(ref variable, ref exp) => {
                    let dtype = exp.data_type();
                    if dtype.is_none() {
                        continue;
                    }
                    if !variable.has_type() {
                        variable.set_type(dtype.unwrap());
                    }
                }
                _ => {}
            }
        }
        break;
    }
}