#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  class stack_alloc_node : public cdk::expression_node {
    cdk::expression_node *_size;

  public:
    stack_alloc_node(int lineno, cdk::expression_node *size)
        : cdk::expression_node(lineno), _size(size) {}

    cdk::expression_node *size() { return _size; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_stack_alloc_node(this, level);
    }
  };
} // udf