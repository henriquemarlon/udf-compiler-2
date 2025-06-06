#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {


  class return_node : public cdk::basic_node {
    cdk::expression_node *_argument;

  public:
    inline return_node(int lineno, cdk::expression_node *argument) :
        cdk::basic_node(lineno), _argument(argument) {
    }

    inline cdk::expression_node *argument() {
      return _argument;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_return_node(this, level);
    }
  };
} // udf