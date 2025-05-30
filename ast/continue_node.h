#pragma once

#include <cdk/ast/basic_node.h>

namespace udf {

  class continue_node: public cdk::basic_node {
    size_t _continue_level;

  public:
    continue_node(int lineno, size_t continue_level = 1) :
        cdk::basic_node(lineno), _continue_level(continue_level) {
    }

  public:
    size_t continue_level() const {
      return _continue_level;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_continue_node(this, level);
    }

  };

} // udf