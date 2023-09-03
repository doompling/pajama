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
        let mut method_index = HashMap::new();

        match &mut nodes.ast {
            Node::Module(module) => {
                populate_method_index(module, &mut method_index);
                run_type_inference(module, method_index);
            },
            _ => todo!(),
        }

        SemanticAnalyzer {
            diagnostics: Diagnostics {},
        }
    }
}

fn populate_method_index(module: &mut crate::parser::Module, method_index: &mut HashMap<String, Option<BaseType>>) {
    module.body.iter_mut().for_each(|node| {
        match node {
            Node::Def(def_node) => {
                method_index.insert(
                    def_node.prototype.name.clone(),
                    def_node.prototype.return_type.clone()
                );
            }
            _ => {}
        }
    });
}

fn run_type_inference(module: &mut crate::parser::Module, mut method_index: HashMap<String, Option<BaseType>>) {
    module.body.iter_mut().for_each(|node| {
        match node {
            Node::Def(def_node) => def_node.body.iter_mut().for_each(|node| {
                match node {
                    Node::AssignLocalVar(assignlocalvar_node) => {
                        match assignlocalvar_node.value.as_mut() {
                            Node::Binary(binary_node) => visit_binary_node(&method_index, binary_node),
                            Node::Call(call_node) => visit_call_node(&method_index, call_node),
                            Node::Send(send_node) => visit_send_node(&method_index, send_node),
                            _ => {}
                        }
                    },
                    Node::Binary(binary_node) => visit_binary_node(&method_index, binary_node),
                    Node::Call(call_node) => visit_call_node(&method_index, call_node),
                    Node::Send(send_node) => visit_send_node(&method_index, send_node),
                    _ => {}
                }
            }),
            _ => {}
        }
    });
}

fn visit_binary_node(method_index: &HashMap<String, Option<BaseType>>, binary_node: &mut crate::parser::Binary) {
    match binary_node.left.as_mut() {
        Node::Binary(node) => visit_binary_node(method_index, node),
        Node::Call(node) => visit_call_node(method_index, node),
        Node::Send(node) => visit_send_node(method_index, node),
        _ => {},
    }

    match binary_node.right.as_mut() {
        Node::Binary(node) => visit_binary_node(method_index, node),
        Node::Call(node) => visit_call_node(method_index, node),
        Node::Send(node) => visit_send_node(method_index, node),
        _ => {},
    }
}

fn visit_call_node(method_index: &HashMap<String, Option<BaseType>>, call_node: &mut crate::parser::Call) {
    let base_type = method_index.get(&call_node.fn_name).unwrap();
    call_node.return_type = base_type.clone();

    for arg in &mut call_node.args {
        match arg {
            Node::Call(node) => visit_call_node(method_index, node),
            Node::Send(node) => visit_send_node(&method_index, node),
            _ => {}
        }
    }
}

fn visit_send_node(method_index: &HashMap<String, Option<BaseType>>, send_node: &mut crate::parser::Send) {
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
