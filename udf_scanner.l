%option c++ prefix="udf_scanner_" outfile="udf_scanner.cpp"  
%option stack noyywrap yylineno 8bit debug  
%{   
#include <string>  
#include <cdk/ast/sequence_node.h>  
#include <cdk/ast/expression_node.h>  
#include <cdk/ast/lvalue_node.h>  
#include "udf_parser.tab.h"  
  
#define yyerror LexerError  
%}  
%x X_STRING X_COMMENT  
%%  
  yydebug=1; set_debug(1);  
  
"//".*                 ; /* line comments */  
"/*"                   yy_push_state(X_COMMENT);  
<X_COMMENT>"/*"        yy_push_state(X_COMMENT); /* nested comments */  
<X_COMMENT>"*/"        yy_pop_state();  
<X_COMMENT>.|\n        ; /* ignore comment content */  
  
">="                   return tGE;   
"<="                   return tLE;   
"=="                   return tEQ;   
"!="                   return tNE;  
"**"                   return tCONTRACT;  
"@"                    return tINDEX;  
"&&"                   return tAND;
"||"                   return tOR;
  
"int"                  return tINT;  
"real"                 return tREAL;  
"string"               return tSTRING_TYPE;  
"tensor"               return tTENSOR;  
"ptr"                  return tPTR;  
"void"                 return tVOID;  
"auto"                 return tAUTO;  
  
"public"               return tPUBLIC;  
"forward"              return tFORWARD;  
  
"if"                   return tIF;  
"elif"                 return tELIF;  
"else"                 return tELSE;  
"for"                  return tFOR;  
"break"                return tBREAK;  
"continue"             return tCONTINUE;  
"return"               return tRETURN;  
  
"write"                return tWRITE;  
"writeln"              return tWRITELN;  
"input"                return tINPUT;  
  
"nullptr"              return tNULLPTR;  
"objects"              return tOBJECTS;  
"sizeof"               return tSIZEOF;  
  
"capacity"             return tCAPACITY;  
"rank"                 return tRANK;  
"dims"                 return tDIMS;  
"dim"                  return tDIM;  
"reshape"              return tRESHAPE;  
  
[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;  
  
\"                     yy_push_state(X_STRING); yylval.s = new std::string("");  
<X_STRING>\"           yy_pop_state(); return tSTRING;  
<X_STRING>\\\"         *yylval.s += "\"";  
<X_STRING>\\\\         *yylval.s += "\\";  
<X_STRING>\\n          *yylval.s += "\n";  
<X_STRING>\\r          *yylval.s += "\r";  
<X_STRING>\\t          *yylval.s += "\t";  
<X_STRING>\\[0-9a-fA-F]{1,2} {   
    int val = strtol(yytext + 1, nullptr, 16);  
    *yylval.s += (char)val;  
}  
<X_STRING>.            *yylval.s += yytext;  
<X_STRING>\n           yyerror("newline in string");  
  
0x[0-9a-fA-F]+         yylval.i = strtol(yytext, nullptr, 16); return tINTEGER;  
[0-9]+                 yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;  
[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?  yylval.d = strtod(yytext, nullptr); return tDOUBLE;  
[0-9]+[eE][+-]?[0-9]+  yylval.d = strtod(yytext, nullptr); return tDOUBLE;  
  
[-()<>=+*/%;{}.,\[\]?~&|] return *yytext;
  
[ \t\n\r]+             ; /* ignore whitespace */  
  
.                      yyerror("Unknown character");  
  
%%