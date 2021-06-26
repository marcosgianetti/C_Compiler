%{
    var escopoAtual = 0;
    var tabelaSimbolos = [];
    var tac = [];
    var erros = [];
    var posOrdem = []
    var inOrdem = []
    var byLevel = []

    class AST {

  		constructor(root){
  		this.root = root
  		}

  		postorder(node){
		    if(node !== null)
		    {
		        this.postorder(node.leftChild);
		        this.postorder(node.rightChild);
		        posOrdem.push(node.type);
		    }
		}

		inorder(node)
		{
		    if(node !== null)
		    {
		        this.inorder(node.leftChild);
		        inOrdem.push(node.type);
		        this.inorder(node.rightChild);
		    }
		}

  	}

    class Node {

  		constructor(type, leftChild = null, rightChild = null) {
  		this.type = type
  		this.leftChild = leftChild
    	this.rightChild = rightChild;
  	}

	}
    
    function criarVariavel(tipo, nome, valor){
    	if(typeof valor === 'string'){
    		tabelaSimbolos.map((dictAtual) => {
    			if (dictAtual.id == valor){
    				tabelaSimbolos.push({ tipo: tipo, id: nome, val: dictAtual.val, escopo:escopoAtual});
    			}
    		})
    	} else {
        tabelaSimbolos.push({ tipo: tipo, id: nome, val: valor, escopo:escopoAtual});
        }
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
    | BREAK ';' 
    | CONTINUE ';' 
    | if_stmt
    | loop_stmt
    | switch_stmt 
    | statement_composto
    ;

statement_composto 
    : '{' statements '}'
    | '{' '}'
    ;

/* Declaração de variável, atribuição de valor, expressão condicional */
exp_stmt
    : declaracao_variavel ';' 
    | declaracao_funcao 
    | expressao_atribuicao ';'
    | expressao_condicional ';' 
    | chamada_funcao ';' 
    ;

/* Gramática do IF */
if_stmt
    : IF '(' expressao_condicional ')' '{' statement '}' ELSE  statement 
    | IF '(' expressao_condicional ')' '{' statement '}'
    ;

/* Gramática do CASE */
cases
    : CASE valor_lit ':' statement
    | CASE valor_lit ':' statement cases
    | default_stmt
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
    | FOR '(' exp_stmt  exp_stmt  IDF ')' statement
    | FOR '(' exp_stmt  exp_stmt  expressao_atribuicao ')' statement
    | FOR '(' exp_stmt  exp_stmt  ')' statement
    | FOR '(' exp_stmt  ';' IDF ')' statement 
    | FOR '(' exp_stmt  ';' ')' statement
    | FOR '(' ';' exp_stmt  IDF ')' statement
    | FOR '(' ';' exp_stmt  expressao_atribuicao ')' statement
    | FOR '(' ';' exp_stmt  ')' statement
    | FOR '(' ';' ';' expressao_atribuicao ')' statement
    | FOR '(' ';' ';' ')' statement

    
    | DO '{' statement '}' WHILE '(' expressao_condicional ')' ';'
      {console.log('DO WHILE statement')}
      
    ;

/* Valor literal */
valor_lit
    : F_LIT
    {$$ = parseFloat($1)}
    | INT_LIT
    {$$ = parseInt($1)}
    | CHAR_LIT
    {$$ = $1}
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
	{criarVariavel($1, $2, null)}
	| tipo_var IDF '=' expressao_aritmetica
	{
	criarVariavel($1, $2, $4.value)
	const leftChild = new Node($2)
    const node = new Node($3, leftChild, $4.node)
	const arvore = new AST(node)
	arvore.postorder(node)
	arvore.inorder(node)
	arvore.bylevel(node)
	console.log(posOrdem)
	console.log(inOrdem)
	console.log(byLevel)
	}
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
    {	$$ = {
    	type: $1.type,
    	value: $1.value,
    	arguments: $1.arguments,
    	node: $1.node
    	}
    }
    | expressao_aritmetica '+' termo
    {	$$ = {
    	type: $2,
    	value: $1.value + $3.value,
    	arguments: [$1, $3],
    	node: new Node($2, $1.node, $3.node)
    	}
    }
    | expressao_aritmetica '-' termo
    {	$$ = {
    	type: $2,
    	value: $1.value - $3.value,
    	arguments: [$1, $3],
    	node: new Node($2, $1.node, $3.node)
    	}
    }
    ;

termo
    : fator
    {	$$ = {
    	type: $1.type,
    	value: $1.value,
    	arguments: $1.arguments,
    	node: $1.node
    	}
    }
    | termo '*' fator
    {	$$ = {
    	type: $2,
    	value: $1.value * $3.value,
    	arguments: [$1, $3],
    	node: new Node($2, $1.node, $3.node)
    	}
    }
    | termo '/' fator
    {	
    	if($1.type == $3.type == 'IDF'){
    		//chamar função de verificar se variáveis foram declaradas
    		//chamar função que acha o valor de uma variável já declarada
    	}
    	$$ = {
    	type: $2,
    	value: $1.value / $3.value,
    	arguments: [$1, $3],
    	node: new Node($2, $1.node, $3.node)
    	}
    }
    ;

fator
    : IDF
    {	
    	$$ = {
    	type: $1,
    	value: $1,
    	arguments: [],
    	node: new Node($1)
    	}
    }
    | valor_lit
    {	$$ = {
    	type: $1,
    	value: $1,
    	arguments: [],
    	node: new Node($1)
    	}
    }
    | '(' expressao_aritmetica ')'
    {	$$ = {
    	type: '$2.type',
    	value: $2.value,
    	arguments: $2.arguments,
    	node: $2.node
    	}
    }
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
    {console.log($1)}
    ;
