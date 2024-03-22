use std::{collections::HashMap, hash::Hash, ops::Deref};

use crate::parser::{self, BaseType, Def, Node, Parser, ParserResult};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    // pub parser_result: ParserResult,
    pub diagnostics: Diagnostics,
}

#[derive(Debug)]
pub struct Diagnostics {}

impl SemanticAnalyzer {
    pub fn run(result: &mut ParserResult) -> SemanticAnalyzer {
        Self::transform_ast(result)
    }

    pub fn transform_ast(result: &mut ParserResult) -> SemanticAnalyzer {
        let mut attribute_index = HashMap::new();
        let mut method_index = HashMap::new();

        match &mut result.module {
            Node::Module(module) => {
                populate_class_index(&result.index.class_index, &mut attribute_index);
                populate_method_index(module, &mut method_index);
                run_type_inference(module, method_index, attribute_index);
            }
            _ => todo!(),
        }

        SemanticAnalyzer {
            diagnostics: Diagnostics {},
        }
    }
}

fn populate_class_index(
    class_index: &HashMap<String, parser::Class>,
    attribute_index: &mut HashMap<String, (i32, BaseType)>,
) {
    class_index.values().for_each(|class| {
        for attribute_node in &class.attributes {
            if let Node::Attribute(attribute) = attribute_node {
                attribute_index.insert(
                    format!("{}.{}", class.name, attribute.name),
                    (attribute.index, attribute.return_type.clone()),
                );
            }
        }
    });
}

fn populate_method_index(
    module: &mut crate::parser::Module,
    method_index: &mut HashMap<String, Option<BaseType>>,
) {
    module.methods.iter_mut().for_each(|node| match node {
        Node::Def(def_node) => {
            method_index.insert(
                def_node.prototype.name.clone(),
                def_node.prototype.return_type.clone(),
            );
        }
        Node::DefE(def_e_node) => {
            method_index.insert(
                def_e_node.prototype.name.clone(),
                def_e_node.prototype.return_type.clone(),
            );
        }
        _ => {}
    });
}

fn run_type_inference(
    module: &mut crate::parser::Module,
    mut method_index: HashMap<String, Option<BaseType>>,
    mut attribute_index: HashMap<String, (i32, BaseType)>,
) {
    module.methods.iter_mut().for_each(|node| {
        match node {
            Node::Def(def_node) => {
                let mut lvar_index = HashMap::new();

                def_node.prototype.args.iter().for_each(|arg| {
                    lvar_index.insert(arg.name.clone(), Some(arg.return_type.clone()));
                });

                def_node.body.iter_mut().for_each(|node| match node {
                    Node::Access(access_node) => {
                        visit_access_node(&attribute_index, &lvar_index, access_node);
                    }
                    Node::AssignLocalVar(assignlocalvar_node) => {
                        let return_type = match assignlocalvar_node.value.as_mut() {
                            Node::Binary(binary_node) => visit_binary_node(
                                &attribute_index,
                                &method_index,
                                &lvar_index,
                                binary_node,
                            ),
                            Node::Call(call_node) => visit_call_node(
                                &attribute_index,
                                &method_index,
                                &lvar_index,
                                call_node,
                            ),
                            Node::Send(send_node) => visit_send_node(
                                &attribute_index,
                                &method_index,
                                &lvar_index,
                                send_node,
                            ),
                            Node::Access(access_node) => {
                                visit_access_node(&attribute_index, &lvar_index, access_node)
                            }
                            Node::AssignLocalVar(_) => todo!(),
                            Node::Attribute(_) => todo!(),
                            Node::Class(_) => todo!(),
                            Node::Def(_) => todo!(),
                            Node::DefE(_) => todo!(),
                            Node::Impl(_) => todo!(),
                            Node::Int(_) => todo!(),
                            Node::StringLiteral(_) => Some(BaseType::Class("Str".to_string())),
                            Node::LocalVar(_) => todo!(),
                            Node::Module(_) => todo!(),
                            Node::Ret(_) => todo!(),
                            Node::SelfRef(_) => todo!(),
                            Node::Trait(_) => todo!(),
                            Node::AssignAttribute(_) => todo!(),
                            Node::Const(_) => todo!(),
                        };

                        lvar_index.insert(assignlocalvar_node.name.clone(), return_type);
                    }
                    Node::Binary(binary_node) => {
                        visit_binary_node(
                            &attribute_index,
                            &method_index,
                            &lvar_index,
                            binary_node,
                        );
                    }
                    Node::Call(call_node) => {
                        visit_call_node(&attribute_index, &method_index, &lvar_index, call_node);
                    }
                    Node::Send(send_node) => {
                        visit_send_node(&attribute_index, &method_index, &lvar_index, send_node);
                    }
                    Node::Ret(ret_node) => {
                        visit_ret_node(&attribute_index, &method_index, &lvar_index, ret_node);
                    }
                    Node::Attribute(_) => todo!(),
                    Node::Class(_) => todo!(),
                    Node::Def(_) => todo!(),
                    Node::DefE(_) => todo!(),
                    Node::Impl(_) => todo!(),
                    Node::Int(_) => todo!(),
                    Node::StringLiteral(_) => todo!(),
                    Node::LocalVar(node) => {
                        println!("{:#?}", node);
                        todo!();
                    }
                    Node::Module(_) => todo!(),
                    Node::SelfRef(_) => todo!(),
                    Node::Trait(_) => todo!(),
                    Node::AssignAttribute(assign_attr_node) => {
                        match assign_attr_node.value.as_mut() {
                            Node::Binary(binary_node) => visit_binary_node(
                                &attribute_index,
                                &method_index,
                                &lvar_index,
                                binary_node,
                            ),
                            Node::Call(call_node) => visit_call_node(
                                &attribute_index,
                                &method_index,
                                &lvar_index,
                                call_node,
                            ),
                            Node::Send(send_node) => visit_send_node(
                                &attribute_index,
                                &method_index,
                                &lvar_index,
                                send_node,
                            ),
                            Node::Access(access_node) => {
                                visit_access_node(&attribute_index, &lvar_index, access_node)
                            }
                            Node::AssignLocalVar(_) => todo!(),
                            Node::Attribute(_) => todo!(),
                            Node::Class(_) => todo!(),
                            Node::Def(_) => todo!(),
                            Node::DefE(_) => todo!(),
                            Node::Impl(_) => todo!(),
                            Node::Int(_) => todo!(),
                            Node::StringLiteral(_) => todo!(),
                            Node::LocalVar(lvar) => lvar.return_type.clone(),
                            Node::Module(_) => todo!(),
                            Node::Ret(_) => todo!(),
                            Node::SelfRef(_) => todo!(),
                            Node::Trait(_) => todo!(),
                            Node::AssignAttribute(_) => todo!(),
                            Node::Const(_) => todo!(),
                        };
                    }
                    Node::Const(_) => todo!(),
                })
            }
            _ => {}
        };
    });
}

fn visit_ret_node(
    attribute_index: &HashMap<String, (i32, BaseType)>,
    method_index: &HashMap<String, Option<BaseType>>,
    lvar_index: &HashMap<String, Option<BaseType>>,
    ret_node: &mut crate::parser::Ret,
) {
    match ret_node.value.as_mut() {
        Node::Access(access_node) => visit_access_node(attribute_index, lvar_index, access_node),
        Node::Binary(node) => visit_binary_node(attribute_index, method_index, lvar_index, node),
        Node::Call(node) => visit_call_node(attribute_index, method_index, lvar_index, node),
        Node::Send(node) => visit_send_node(attribute_index, method_index, lvar_index, node),
        _ => todo!(),
    };
}

fn visit_access_node(
    attribute_index: &HashMap<String, (i32, BaseType)>,
    lvar_index: &HashMap<String, Option<BaseType>>,
    access_node: &mut crate::parser::Access,
) -> Option<BaseType> {
    let class_name = match access_node.receiver.as_mut() {
        Node::LocalVar(lvar) => {
            let latest_return_type = lvar_index.get(&lvar.name).unwrap();
            lvar.return_type = latest_return_type.clone();

            nilla_class_name(&lvar.return_type.as_ref().unwrap())
        }
        Node::Access(_) => todo!(),
        Node::AssignLocalVar(_) => todo!(),
        Node::Attribute(_) => todo!(),
        Node::Binary(_) => todo!(),
        Node::Call(_) => todo!(),
        Node::Class(_) => todo!(),
        Node::Def(_) => todo!(),
        Node::DefE(_) => todo!(),
        Node::Impl(_) => todo!(),
        Node::Int(_) => todo!(),
        Node::StringLiteral(_) => todo!(),
        Node::Module(_) => todo!(),
        Node::Ret(_) => todo!(),
        Node::SelfRef(self_ref) => nilla_class_name(&self_ref.return_type),
        Node::Send(_) => todo!(),
        Node::Trait(_) => todo!(),
        Node::AssignAttribute(_) => todo!(),
        Node::Const(_) => todo!(),
    };

    let attribute_name = match access_node.message.as_mut() {
        Node::Attribute(attr_node) => attr_node.name.clone(),
        _ => todo!(),
    };

    let attr_key = format!("{}.{}", class_name, attribute_name);
    let (index, return_type) = attribute_index.get(&attr_key).unwrap();

    access_node.index = *index;
    access_node.return_type = Some(return_type.clone());

    Some(return_type.clone())
}

fn visit_binary_node(
    attribute_index: &HashMap<String, (i32, BaseType)>,
    method_index: &HashMap<String, Option<BaseType>>,
    lvar_index: &HashMap<String, Option<BaseType>>,
    binary_node: &mut crate::parser::Binary,
) -> Option<BaseType> {
    match binary_node.left.as_mut() {
        Node::Access(access_node) => visit_access_node(attribute_index, lvar_index, access_node),
        Node::Binary(node) => visit_binary_node(attribute_index, method_index, lvar_index, node),
        Node::Call(node) => visit_call_node(attribute_index, method_index, lvar_index, node),
        Node::Send(node) => visit_send_node(attribute_index, method_index, lvar_index, node),
        _ => todo!(),
    };

    match binary_node.right.as_mut() {
        Node::Access(access_node) => visit_access_node(attribute_index, lvar_index, access_node),
        Node::Binary(node) => visit_binary_node(attribute_index, method_index, lvar_index, node),
        Node::Call(node) => visit_call_node(attribute_index, method_index, lvar_index, node),
        Node::Send(node) => visit_send_node(attribute_index, method_index, lvar_index, node),
        _ => todo!(),
    }
}

fn visit_call_node(
    attribute_index: &HashMap<String, (i32, BaseType)>,
    method_index: &HashMap<String, Option<BaseType>>,
    lvar_index: &HashMap<String, Option<BaseType>>,
    call_node: &mut crate::parser::Call,
) -> Option<BaseType> {
    let base_type = method_index.get(&call_node.fn_name).unwrap();
    call_node.return_type = base_type.clone();

    for arg in &mut call_node.args {
        match arg {
            Node::Access(access_node) => {
                visit_access_node(attribute_index, lvar_index, access_node);
            }
            Node::Call(node) => {
                visit_call_node(attribute_index, method_index, lvar_index, node);
            }
            Node::Send(node) => {
                visit_send_node(attribute_index, &method_index, lvar_index, node);
            }
            Node::Binary(node) => {
                visit_binary_node(attribute_index, method_index, lvar_index, node);
            }
            Node::LocalVar(lvar) => {
                let latest_return_type = lvar_index.get(&lvar.name).unwrap();
                lvar.return_type = latest_return_type.clone();
            }
            _ => todo!(),
        };
    }

    base_type.clone()
}

fn visit_send_node(
    attribute_index: &HashMap<String, (i32, BaseType)>,
    method_index: &HashMap<String, Option<BaseType>>,
    lvar_index: &HashMap<String, Option<BaseType>>,
    send_node: &mut crate::parser::Send,
) -> Option<BaseType> {
    let fn_name = match send_node.message.as_mut() {
        Node::Call(node) => &node.fn_name,
        _ => todo!(),
    };

    let basetype = match send_node.receiver.as_mut() {
        Node::Access(access_node) => visit_access_node(attribute_index, lvar_index, access_node),
        Node::Call(node) => visit_call_node(attribute_index, method_index, lvar_index, node),
        Node::Send(node) => visit_send_node(attribute_index, &method_index, lvar_index, node),
        Node::Binary(node) => visit_binary_node(attribute_index, method_index, lvar_index, node),
        Node::LocalVar(lvar) => {
            let latest_return_type = lvar_index.get(&lvar.name).unwrap();
            lvar.return_type = latest_return_type.clone();
            latest_return_type.clone()
        }
        Node::Const(node) => {
            if fn_name == "new" {
                send_node.return_type = Some(BaseType::Class(node.name.clone()));
                Some(BaseType::Class(node.name.clone()))
                // return;
            } else {
                todo!("class methods")
            }
        }
        _ => None,
    };

    let class_name = nilla_class_name(&basetype.unwrap());
    let message_name = match send_node.message.as_mut() {
        Node::Call(node) => {
            format!("{}.{}", class_name, &node.fn_name)
        }
        _ => "".to_string(),
    };

    let base_type = method_index.get(&message_name).unwrap();
    match base_type {
        Some(bt) => {
            send_node.return_type = Some(bt.clone());
            Some(bt.clone())
        }
        None => None,
    }
}

pub fn nilla_class_name(base_type: &BaseType) -> String {
    match base_type {
        BaseType::BytePtr => "BytePtr".to_string(),
        BaseType::Int => "Int".to_string(),
        BaseType::Void => "".to_string(),
        BaseType::Class(class_name) => class_name.clone(),
    }
}
