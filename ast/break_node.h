#pragma once

#include <cdk/ast/basic_node.h>

namespace udf {

  class break_node: public cdk::basic_node {
    size_t _break_level;

  public:
    break_node(int lineno, size_t break_level = 1) :
        cdk::basic_node(lineno), _break_level(break_level) {
    }

  public:
    int break_level() const {
      return _break_level;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_break_node(this, level);
    }

  };

} // udf