#pragma once

#include <cdk/ast/lvalue_node.h>

namespace udf {

 class address_node : public cdk::expression_node {
 cdk::lvalue_node *_lvalue;

 public:
 address_node(int lineno, cdk::lvalue_node *lvalue)
 : cdk::expression_node(lineno), _lvalue(lvalue) {}

 cdk::lvalue_node *lvalue() {
 return _lvalue;
 }

 void accept(basic_ast_visitor *visitor, int level) {
 visitor->do_address_node(this, level);
 }
 };

} 