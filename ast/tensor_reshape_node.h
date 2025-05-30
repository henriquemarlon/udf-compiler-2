#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  class tensor_reshape_node : public cdk::expression_node {
    cdk::expression_node *_tensor;
    cdk::sequence_node *_new_shape;

  public:
    tensor_reshape_node(int lineno, cdk::expression_node *tensor, cdk::sequence_node *new_shape)
        : cdk::expression_node(lineno), _tensor(tensor), _new_shape(new_shape) {}

    cdk::expression_node *tensor() { return _tensor; }
    cdk::sequence_node *new_shape() { return _new_shape; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_reshape_node(this, level);
    }
  };

} //  udf