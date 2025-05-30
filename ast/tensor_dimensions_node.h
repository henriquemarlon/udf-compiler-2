#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  class tensor_dimensions_node : public cdk::expression_node {
    cdk::expression_node *_tensor;

  public:
    tensor_dimensions_node(int lineno, cdk::expression_node *tensor)
        : cdk::expression_node(lineno), _tensor(tensor){}

    cdk::expression_node *tensor() { return _tensor; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_dimensions_node(this, level);
    }
  };

}