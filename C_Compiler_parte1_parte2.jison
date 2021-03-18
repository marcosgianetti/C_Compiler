%lex

%%

\s+                                 {/* ignorar */}
"int"                               {return 'INT';}
"double"                            {return 'DOUBLE';}
"float"                             {return 'FLOAT';}
"char"                              {return 'CHAR';}
"*"                                 {return '*';}
"+"                                 {return '+';}
"-"                                 {return '-';}
"/"                                 {return '/';}
","                                 {return ',';}
";"                                 {return ';';}
":"                                 {return ':';}
"."                                 {return '.';}
"'"                                 {return 'QUOTE';}
'"'                                 {return 'DQUOTE';}
"("                                 {return '(';}
")"                                 {return ')';}
"["                                 {return '[';}
"]"                                 {return ']';}
"{"                                 {return '{';}
"}"                                 {return '}';}
"< "                                {return '<';}
"> "                                {return '>';}
"= "                                {return '=';}
"<="                                {return 'LE';}
">="                                {return 'GE';}
"=="                                {return 'EQ';}
"!="                                {return 'NE';}
"&&"                                {return 'AND';}
"||"                                {return 'OR';}
"!"                                 {return 'NOT';}
"if"                                {return 'IF';}
"switch"                            {return 'SWITCH';}
"case"                              {return 'CASE';}
"break"                             {return 'BREAK';}
"default"                           {return 'DEFAULT';}
"else"                              {return 'ELSE';}
"while"                             {return 'WHILE';}
"for"                               {return 'FOR';}
(?:\w+\s+)([a-zA-Z_][a-zA-Z0-9_]*)  {return 'VAR';}
"#"                                 {return '#';}
"define"                            {return 'DEFINE';}

[a-zA-Z][a-zA-Z0-9_]*               {return 'IDF';}
[0-9]*\.[0-9]+([eE][+-][0-9]+)?     {return 'F_LIT';}
[0-9]+                              {return 'INT_LIT';}
"'"[a-zA-Z0-9_]"'"                  {return 'CHAR_LIT';}
.                                   {console.log('Erro léxico: caractere [', yytext, '] não reconhecido.');}
<<EOF>>                             {console.log('Token EOF'); return 'EOF';}

/lex

%start corpo

%% /* Gramática */

corpo
    : statements EOF {console.log(1)}
    ; 

statements
    : statements statement
    | statement
    ;

statement
    : exp_stmt ';' {console.log('Expressao')}
    | BREAK ';' {console.log('Break Statement')}
    | CONTINUE ';' {console.log('Continue Statement')}
    | if_stmt
    | loop_stmt
    | switch_stmt {console.log('SWITCH Statement')}
    | statement_composto
    ;

statement_composto 
    : '{' statements '}'
      {console.log('COMPOSTO')}
    | '{' '}'
      {console.log('COMPOSTO')}
    ;

/* Declaração de variável, atribuição de valor, expressão condicional */
exp_stmt
    : declaracao_variavel
    | expressao_atribuicao
    | expressao_condicional
    ;

/* Gramática do IF */
if_stmt
    : IF '(' expressao_condicional ')' statement ELSE statement
      {console.log('IF ELSE')}
    | IF '(' expressao_condicional ')' statement
      {console.log('IF')}
    ;

/* Gramática do CASE */
cases
    : CASE valor_lit ':' statement
      {console.log('Case Statement Simples')}
    | CASE valor_lit ':' statement cases
      {console.log('Case Statement Multiplo')}
    | default_stmt
      {console.log('Default')}
    ;

default_stmt
    : DEFAULT statement
    ;

/* Gramática do SWITCH */
switch_stmt
    : SWITCH '(' IDF ')' '{' cases '}'
    ;

/* Gramática do WHILE e do FOR */
loop_stmt
    : WHILE '(' expressao_condicional ')' '{' statement '}'
      {console.log('WHILE Statement')}
    | FOR '(' exp_stmt ';' exp_stmt ';' exp_stmt ')' statement
      {console.log('FOR Statement')}
    | FOR '(' exp_stmt ';' exp_stmt ';' ')' statement
      {console.log('FOR Statement')}
    | FOR '(' exp_stmt ';' ';' exp_stmt ')' statement
      {console.log('FOR Statement')}
    | FOR '(' exp_stmt ';' ';' ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' exp_stmt ';' exp_stmt ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' exp_stmt ';' ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' ';' exp_stmt ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' ';' ')' statement
      {console.log('FOR Statement')}
    ;

/* Valor literal */
valor_lit
    : F_LIT
    | INT_LIT
    | CHAR_LIT
    ;

/* Tipo da variável */
tipo_var
    : INT
    {console.log('Variavel int')}
    | DOUBLE
    {console.log('Variavel double')}
    | FLOAT
    {console.log('Variavel float')}
    | CHAR
    {console.log('Variavel char')}
    ;

/* Declaração de variável com ou sem inicialização */
declaracao_variavel
    : tipo_var IDF 
    {console.log('Declaração de variável')}
    | tipo_var expressao_atribuicao
    {console.log('Inicialização de variável')}
    ;

/* Atribuição de valor */
expressao_atribuicao
    : IDF '=' valor_lit 
    {console.log('Atribuição de valor')}
    | IDF '=' IDF 
    {console.log('Atribuição de valor')}
    ;

expressao_condicional
    : IDF operador_relacional IDF
      {console.log('Expressão condicional')}
    | IDF operador_relacional valor_lit
      {console.log('Expressão condicional')}
    | operacao_and
      {console.log('Expressão condicional')}
    | operacao_or
      {console.log('Expressão condicional')}
    | operacao_not
      {console.log('Expressão condicional')}
    ;

operacao_and
    : expressao_logica AND expressao_logica
    ;

operacao_or
    : expressao_logica OR expressao_logica
    ;

operacao_not
    : NOT expressao_logica
    ;

operador_relacional
    : LE | GE  | EQ  | NE | '>' | '<'
    {console.log('Operador relacional')}
    ;
