%{
    var escopoAtual = 0;
    var tabelaSimbolos = [];
    var tac = [];
    var erros = [];

    
    function criarVariavel(tipo, nome, valor, isLiteral){
    	if(!isLiteral){
    		tabelaSimbolos.map((dictAtual) => {
    			if (dictAtual.id == valor){
    				tabelaSimbolos.push({ tipo: tipo, id: nome, val: dictAtual.val, escopo:escopoAtual});
    			}
    		})
    	} else {
        tabelaSimbolos.push({ tipo: tipo, id: nome, val: valor, escopo:escopoAtual});
        }
        console.log(tabelaSimbolos)
    }
    
    function atribuirValor(nome, valor){

    }

    function somaEscopo(){
    	escopoAtual++
    }

    function subtraiEscopo(){
    	escopoAtual--
    }
   
    
%}


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
"="                                 {return '=';}
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
"do"                                {return 'DO';}
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
    : exp_stmt 
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
    : declaracao_variavel ';' {console.log('Declaração de variável')}
    | declaracao_funcao {console.log('Declaração de função')}
    | expressao_atribuicao ';'{console.log('Atribuição de valor')}
    | expressao_condicional ';' {console.log('Expressao condicional')}
    | chamada_funcao ';' {console.log('Chamada de função')}
    ;

/* Gramática do IF */
if_stmt
    : IF '(' expressao_condicional ')' '{' statement '}' ELSE  statement 
      {console.log('IF ELSE')}
    | IF '(' expressao_condicional ')' '{' statement '}'
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
    : DEFAULT ':' statement
    ;

/* Gramática do SWITCH */
switch_stmt
    : SWITCH '(' IDF ')' '{' cases '}'
    ;

/* Gramática do WHILE e do FOR */
loop_stmt
    : WHILE '(' expressao_condicional ')' '{' statement '}'
       {console.log('WHILE Statement')}
    | FOR '(' exp_stmt  exp_stmt  IDF ')' statement
      {console.log('FOR Statement')}
    | FOR '(' exp_stmt  exp_stmt  expressao_atribuicao ')' statement
      {console.log('FOR Statement')}
    | FOR '(' exp_stmt  exp_stmt  ')' statement
      {console.log('FOR Statement')}
    | FOR '(' exp_stmt  ';' IDF ')' statement 
      {console.log('FOR Statement')}
    | FOR '(' exp_stmt  ';' ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' exp_stmt  IDF ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' exp_stmt  expressao_atribuicao ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' exp_stmt  ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' ';' expressao_atribuicao ')' statement
      {console.log('FOR Statement')}
    | FOR '(' ';' ';' ')' statement
      {console.log('FOR Statement')}
    
    | DO '{' statement '}' WHILE '(' expressao_condicional ')' ';'
      {console.log('DO WHILE statement')}
      
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
    | DOUBLE
    | FLOAT
    | CHAR
    ;

/* Declaração de variável com ou sem inicialização */
declaracao_variavel
    : tipo_var IDF 
    {criarVariavel($1, $2, 'Null', 'Null')}
    | tipo_var IDF '=' F_LIT 
    {criarVariavel($1, $2, parseFloat($4), true)}
    | tipo_var IDF '=' INT_LIT 
    {criarVariavel($1, $2, parseInt($4), true)}
    | tipo_var IDF '=' CHAR_LIT 
    {criarVariavel($1, $2, $4, true)}
    | tipo_var IDF '=' operacao_aritmetica 
    | tipo_var IDF '=' IDF 
    {criarVariavel($1, $2, $4, false)}    
    ;

operacao_aritmetica
  : INT_LIT '+' INT_LIT {criarVariavel($-2, $-1, parseInt($1)+parseInt($3), true)}
  | INT_LIT '-' INT_LIT {criarVariavel($-2, $-1, parseInt($1)-parseInt($3), true)}
  | INT_LIT '*' INT_LIT {criarVariavel($-2, $-1, parseInt($1)*parseInt($3), true)}
  | INT_LIT '/' INT_LIT {criarVariavel($-2, $-1, parseInt($1)/parseInt($3), true)}
  | F_LIT '+' F_LIT {criarVariavel($-2, $-1, parseFloat($1)+parseFloat($3), true)}
  | F_LIT '-' F_LIT {criarVariavel($-2, $-1, parseFloat($1)-parseFloat($3), true)}
  | F_LIT '*' F_LIT {criarVariavel($-2, $-1, parseFloat($1)*parseFloat($3), true)}
  | F_LIT '/' F_LIT {criarVariavel($-2, $-1, parseFloat($1)/parseFloat($3), true)}
  | '(' operacao_aritmetica ')'
  ;

declaracao_funcao
	: tipo_var IDF '(' ')' '{' statements '}'
	| tipo_var IDF '(' definicao_parametros ')' '{' statements '}'
	| tipo_var IDF '(' ')' ';'
	| tipo_var IDF '(' definicao_parametros ')' ';'
	;

definicao_parametros
	: declaracao_variavel
	| declaracao_variavel ',' definicao_parametros
	;

chamada_funcao
	: IDF '(' passagem_parametros ')'
	| IDF '(' ')'
	;

passagem_parametros
	: IDF
	| valor_lit
	| IDF ',' passagem_parametros
	| valor_lit ',' passagem_parametros
	;

/* Atribuição de valor */
expressao_atribuicao
    : IDF operador_atribuicao expressao_aritmetica   
    | IDF operador_atribuicao '*' expressao_aritmetica 
    | IDF operador_atribuicao chamada_funcao 
    | expressao_in_decrement
    ;

operador_atribuicao	
	: '=' | '*''=' | '/''=' | '%''=' | '+''=' | '-''=' 
	;		

expressao_in_decrement
  : IDF '+''+' 
    {console.log('Incremento ++')}
  | IDF '-''-'
    {console.log('Decremento --')}
  ;
    
expressao_aritmetica
    : termo
    | expressao_aritmetica '+' termo 
    | expressao_aritmetica '-' termo 
    ;

termo
    : fator
    | termo '*' fator 
    | termo '/' fator 
    ;

fator
    : IDF
    | valor_lit
    | '(' expressao_aritmetica ')'
    ;

expressao_condicional
    : IDF operador_relacional IDF expressao_condicional
    | IDF operador_relacional IDF
    | IDF operador_relacional valor_lit expressao_condicional
    | IDF operador_relacional valor_lit
    | operacao_and expressao_condicional
    | operacao_or
    | operacao_not
    ;

operacao_and
    : AND 
    ;

operacao_or
    : OR
    ;

operacao_not
    : NOT expressao_logica
    ;

operador_relacional
    : LE | GE  | EQ  | NE | '>' | '<'
    {console.log('Operador relacional')}
    ;
