#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

 class function_call_node : public cdk::expression_node {
 std::string _identifier;
 cdk::sequence_node *_arguments;

 public:
 function_call_node(int lineno, const std::string &identifier, cdk::sequence_node *arguments)
 : cdk::expression_node(lineno), _identifier(identifier), _arguments(arguments) {}

 const std::string &identifier() const {
 return _identifier;
 }

 cdk::sequence_node *arguments() {
 return _arguments;
 }

 void accept(basic_ast_visitor *visitor, int level) {
 visitor->do_function_call_node(this, level);
 }
 };

} 