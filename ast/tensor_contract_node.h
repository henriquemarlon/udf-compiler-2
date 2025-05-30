#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  class tensor_contract_node : public cdk::expression_node {
    cdk::expression_node *_left, *_right;

  public:
    tensor_contract_node(int lineno, cdk::expression_node *left, cdk::expression_node *right)
        : cdk::expression_node(lineno), _left(left), _right(right) {}

    cdk::expression_node *left() { return _left; }
    cdk::expression_node *right() { return _right; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_contract_node(this, level);
    }
  };

} // udf