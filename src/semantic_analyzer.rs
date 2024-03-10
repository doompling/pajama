use std::{collections::HashMap, ops::Deref, hash::Hash};

use crate::parser::{ParserResult, Node, Def, Parser, BaseType};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    // pub parser_result: ParserResult,
    pub diagnostics: Diagnostics,
}

#[derive(Debug)]
pub struct Diagnostics {}

impl SemanticAnalyzer {
    pub fn run(nodes: &mut ParserResult) -> SemanticAnalyzer {
        Self::transform_ast(nodes)
    }

    pub fn transform_ast(nodes: &mut ParserResult) -> SemanticAnalyzer {
        let mut attribute_index = HashMap::new();
        let mut method_index = HashMap::new();

        match &mut nodes.ast {
            Node::Module(module) => {
                // todo: right now `module.body` is a flat list. It should
                // probably be split out so functions and classes are separate
                // so classes can always be parsed first.
                populate_indicies(module, &mut attribute_index, &mut method_index);
                run_type_inference(module, method_index, attribute_index);
            },
            _ => todo!(),
        }

        SemanticAnalyzer {
            diagnostics: Diagnostics {},
        }
    }
}

fn populate_indicies(module: &mut crate::parser::Module, attribute_index: &mut HashMap<String, (i32, BaseType)>, method_index: &mut HashMap<String, Option<BaseType>>) {
    module.body.iter_mut().for_each(|node| {
        match node {
            Node::Class(class_node) => {
                for attribute_node in &class_node.attributes {
                    if let Node::Attribute(attribute) = attribute_node {
                        attribute_index.insert(
                            format!("{}.{}", class_node.name, attribute.name),
                            (attribute.index, attribute.return_type.clone())
                        );
                    }
                }
            }
            Node::Def(def_node) => {
                method_index.insert(
                    def_node.prototype.name.clone(),
                    def_node.prototype.return_type.clone()
                );
            }
            Node::DefE(def_e_node) => {
                method_index.insert(
                    def_e_node.prototype.name.clone(),
                    def_e_node.prototype.return_type.clone()
                );
            }
            _ => {}
        }
    });
}

fn run_type_inference(module: &mut crate::parser::Module, mut method_index: HashMap<String, Option<BaseType>>, mut attribute_index: HashMap<String, (i32, BaseType)>) {
    module.body.iter_mut().for_each(|node| {
        match node {
            Node::Def(def_node) => def_node.body.iter_mut().for_each(|node| {
                match node {
                    Node::Access(access_node) => visit_access_node(&attribute_index, access_node),
                    Node::AssignLocalVar(assignlocalvar_node) => {
                        match assignlocalvar_node.value.as_mut() {
                            Node::Binary(binary_node) => visit_binary_node(&attribute_index, &method_index, binary_node),
                            Node::Call(call_node) => visit_call_node(&attribute_index, &method_index, call_node),
                            Node::Send(send_node) => visit_send_node(&attribute_index, &method_index, send_node),
                            _ => {}
                        }
                    },
                    Node::Binary(binary_node) => visit_binary_node(&attribute_index, &method_index, binary_node),
                    Node::Call(call_node) => visit_call_node(&attribute_index, &method_index, call_node),
                    Node::Send(send_node) => visit_send_node(&attribute_index, &method_index, send_node),
                    _ => {}
                }
            }),
            _ => {}
        }
    });
}

fn visit_access_node(attribute_index: &HashMap<String, (i32, BaseType)>, access_node: &mut crate::parser::Access) {
    let class_name =
        match access_node.receiver.as_mut() {
            Node::Access(_) => todo!(),
            Node::Attribute(_) => todo!(),
            Node::SelfRef(_) => todo!(),
            Node::AssignLocalVar(_) => todo!(),
            Node::Binary(_) => todo!(),
            Node::Call(_) => todo!(),
            Node::Send(_) => todo!(),
            Node::Def(_) => todo!(),
            Node::DefE(_) => todo!(),
            Node::Int(_) => todo!(),
            Node::InterpolableString(_) => todo!(),
            Node::Module(_) => todo!(),
            Node::Impl(_) => todo!(),
            Node::Class(_) => todo!(),
            Node::Trait(_) => todo!(),
            Node::LocalVar(lvar) => nilla_class_name(lvar.return_type.clone().unwrap()),
        };

    let attribute_name =
        match access_node.message.as_mut() {
            Node::Attribute(attr_node) => attr_node.name.clone(),
            _ => todo!(),
        };

    let attr_key = format!("{}.{}", class_name, attribute_name);
    let (index, return_type) = attribute_index.get(&attr_key).unwrap();

    access_node.index = *index;
    access_node.return_type = Some(return_type.clone());
}

fn visit_binary_node(attribute_index: &HashMap<String, (i32, BaseType)>, method_index: &HashMap<String, Option<BaseType>>, binary_node: &mut crate::parser::Binary) {
    match binary_node.left.as_mut() {
        Node::Access(access_node) => visit_access_node(attribute_index, access_node),
        Node::Binary(node) => visit_binary_node(attribute_index, method_index, node),
        Node::Call(node) => visit_call_node(attribute_index, method_index, node),
        Node::Send(node) => visit_send_node(attribute_index, method_index, node),
        _ => {},
    }

    match binary_node.right.as_mut() {
        Node::Access(access_node) => visit_access_node(attribute_index, access_node),
        Node::Binary(node) => visit_binary_node(attribute_index, method_index, node),
        Node::Call(node) => visit_call_node(attribute_index, method_index, node),
        Node::Send(node) => visit_send_node(attribute_index, method_index, node),
        _ => {},
    }
}

fn visit_call_node(attribute_index: &HashMap<String, (i32, BaseType)>, method_index: &HashMap<String, Option<BaseType>>, call_node: &mut crate::parser::Call) {
    let base_type = method_index.get(&call_node.fn_name).unwrap();
    call_node.return_type = base_type.clone();

    for arg in &mut call_node.args {
        match arg {
            Node::Access(access_node) => visit_access_node(attribute_index, access_node),
            Node::Call(node) => visit_call_node(attribute_index, method_index, node),
            Node::Send(node) => visit_send_node(attribute_index, &method_index, node),
            _ => {}
        }
    }
}

fn visit_send_node(attribute_index: &HashMap<String, (i32, BaseType)>, method_index: &HashMap<String, Option<BaseType>>, send_node: &mut crate::parser::Send) {
    if let Some(rt) = &send_node.return_type {
        if rt == &BaseType::Undef("".to_string()) {
            let message_name = match send_node.message.as_ref() {
                Node::Call(node) => {
                    &node.fn_name
                },
                _ => ""
            };

            // Crude method lookup
            let base_type = method_index.get(message_name).unwrap();
            match base_type {
                Some(bt) => {
                    send_node.return_type = Some(bt.clone())
                },
                None => {},
            }
        }
    }
}

pub fn nilla_class_name(base_type: BaseType) -> String {
    match base_type {
        BaseType::BytePtr => "BytePtr".to_string(),
        BaseType::Int => "Int".to_string(),
        BaseType::StringType => "Str".to_string(),
        BaseType::Void => "".to_string(),
        BaseType::Undef(class_name) => class_name.clone(),
    }
}
