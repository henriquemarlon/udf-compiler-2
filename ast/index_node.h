#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

 class index_node : public cdk::lvalue_node {
 cdk::expression_node *_pointer;
 cdk::expression_node *_index;

 public:
 index_node(int lineno, cdk::expression_node *pointer, cdk::expression_node *index)
 : cdk::lvalue_node(lineno), _pointer(pointer), _index(index) {}

 cdk::expression_node *pointer() {
 return _pointer;
 }

 cdk::expression_node *index() {
 return _index;
 }

 void accept(basic_ast_visitor *visitor, int level) {
 visitor->do_index_node(this, level);
 }
 };

} 