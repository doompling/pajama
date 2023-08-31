use std::{collections::HashMap, ops::Deref, hash::Hash};

use crate::parser::{ParserResult, Node, Def, Parser};

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
        // let mut traits = HashMap::new();

        // match &mut nodes.ast {
        //     Node::Module(module) => {
        //         // Step 1: Populate trait method references
        //         for node in &mut module.body {
        //             match node {
        //                 Node::Trait(trait_node) => {
        //                     for node in &mut trait_node.body {
        //                         match node {
        //                             Node::Def(def_node) => {
        //                                 traits.insert(
        //                                     format!("{}_{}", trait_node.name, def_node.prototype.name),
        //                                     def_node
        //                                 )
        //                             }
        //                             _ => panic!("Expected only Def nodes in traits")
        //                         };
        //                     }
        //                 },

        //                 // Step 2: Expand class impl blocks
        //                 Node::Class(class_node) => {
        //                     for node in &mut class_node.body {
        //                         match node {
        //                             Node::Impl(impl_node) => {
        //                                 for node in &mut impl_node.body {
        //                                     match node {
        //                                         Node::Def(def_node) => {
        //                                             let prefixed_def_name = format!("{}_{}", impl_node.name, def_node.prototype.name);
        //                                             let def = self.traits.get(&prefixed_def_name);

        //                                             match def {
        //                                                 Some(trait_def_node) => {
        //                                                     def_node.prototype.name = "asd".to_string();
        //                                                 },
        //                                                 None => panic!("Trait def missing for {}", prefixed_def_name),
        //                                             }
        //                                         }
        //                                         _ => {}
        //                                     }
        //                                 }
        //                             },
        //                             _ => {},
        //                         }
        //                     }
        //                 },

        //                 _ => {}
        //             }
        //         }

        //         // Step 2: Expand class impl blocks
        //         // for node in &mut module.body {
        //         //     match node {
        //         //         _ => {},
        //         //     }
        //         // }
        //     },
        //     _ => todo!(),
        // }

        SemanticAnalyzer {
            diagnostics: Diagnostics {},
        }
    }
}
