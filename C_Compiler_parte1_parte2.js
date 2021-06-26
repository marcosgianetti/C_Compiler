/* parser generated by jison 0.4.18 */
/*
  Returns a Parser object of the following structure:

  Parser: {
    yy: {}
  }

  Parser.prototype: {
    yy: {},
    trace: function(),
    symbols_: {associative list: name ==> number},
    terminals_: {associative list: number ==> name},
    productions_: [...],
    performAction: function anonymous(yytext, yyleng, yylineno, yy, yystate, $$, _$),
    table: [...],
    defaultActions: {...},
    parseError: function(str, hash),
    parse: function(input),

    lexer: {
        EOF: 1,
        parseError: function(str, hash),
        setInput: function(input),
        input: function(),
        unput: function(str),
        more: function(),
        less: function(n),
        pastInput: function(),
        upcomingInput: function(),
        showPosition: function(),
        test_match: function(regex_match_array, rule_index),
        next: function(),
        lex: function(),
        begin: function(condition),
        popState: function(),
        _currentRules: function(),
        topState: function(),
        pushState: function(condition),

        options: {
            ranges: boolean           (optional: true ==> token location info will include a .range[] member)
            flex: boolean             (optional: true ==> flex-like lexing behaviour where the rules are tested exhaustively to find the longest match)
            backtrack_lexer: boolean  (optional: true ==> lexer regexes are tested in order and for each matching regex the action code is invoked; the lexer terminates the scan when a token is returned by the action code)
        },

        performAction: function(yy, yy_, $avoiding_name_collisions, YY_START),
        rules: [...],
        conditions: {associative list: name ==> set},
    }
  }


  token location info (@$, _$, etc.): {
    first_line: n,
    last_line: n,
    first_column: n,
    last_column: n,
    range: [start_number, end_number]       (where the numbers are indexes into the input string, regular zero-based)
  }


  the parseError function receives a 'hash' object with these members for lexer and parser errors: {
    text:        (matched text)
    token:       (the produced terminal token, if any)
    line:        (yylineno)
  }
  while parser (grammar) errors will also provide these members, i.e. parser errors deliver a superset of attributes: {
    loc:         (yylloc)
    expected:    (string describing the set of expected tokens)
    recoverable: (boolean: TRUE when the parser has a error recovery rule available for this particular error)
  }
*/
var C_Compiler_parte1_parte2 = (function(){
var o=function(k,v,o,l){for(o=o||{},l=k.length;l--;o[k[l]]=v);return o},$V0=[1,5],$V1=[1,6],$V2=[1,21],$V3=[1,12],$V4=[1,15],$V5=[1,16],$V6=[1,20],$V7=[1,23],$V8=[1,17],$V9=[1,18],$Va=[1,19],$Vb=[1,28],$Vc=[1,29],$Vd=[1,30],$Ve=[1,31],$Vf=[1,32],$Vg=[1,33],$Vh=[1,34],$Vi=[5,8,10,15,16,18,21,22,32,33,34,35,36,41,42,43,44,60,61,62],$Vj=[5,8,10,15,16,18,21,22,27,31,32,33,34,35,36,41,42,43,44,60,61,62],$Vk=[5,8,9,10,15,16,18,21,22,24,27,31,32,33,34,35,36,41,42,43,44,60,61,62],$Vl=[1,55],$Vm=[1,56],$Vn=[1,57],$Vo=[1,58],$Vp=[1,53],$Vq=[1,54],$Vr=[1,59],$Vs=[1,60],$Vt=[1,61],$Vu=[1,62],$Vv=[1,63],$Vw=[1,64],$Vx=[9,24],$Vy=[1,66],$Vz=[1,83],$VA=[1,81],$VB=[1,84],$VC=[1,85],$VD=[1,86],$VE=[21,23,33,37,38,39,48],$VF=[33,37,38,39],$VG=[1,105],$VH=[1,106],$VI=[9,24,52,53],$VJ=[1,108],$VK=[1,109],$VL=[9,24,48,50,52,53],$VM=[9,24,29,33,48,50,52,53,60,61,62],$VN=[1,147],$VO=[1,149];
var parser = {trace: function trace () { },
yy: {},
symbols_: {"error":2,"corpo":3,"statements":4,"EOF":5,"statement":6,"exp_stmt":7,"BREAK":8,";":9,"CONTINUE":10,"if_stmt":11,"loop_stmt":12,"switch_stmt":13,"statement_composto":14,"{":15,"}":16,"declaracao_variavel":17,"declaracao_funcao":18,"expressao_atribuicao":19,"expressao_condicional":20,"chamada_funcao":21,"IF":22,"(":23,")":24,"ELSE":25,"cases":26,"CASE":27,"valor_lit":28,":":29,"default_stmt":30,"DEFAULT":31,"SWITCH":32,"IDF":33,"WHILE":34,"FOR":35,"DO":36,"F_LIT":37,"INT_LIT":38,"CHAR_LIT":39,"tipo_var":40,"INT":41,"DOUBLE":42,"FLOAT":43,"CHAR":44,"=":45,"expressao_aritmetica":46,"operador_atribuicao":47,"*":48,"expressao_in_decrement":49,"/":50,"%":51,"+":52,"-":53,"termo":54,"fator":55,"operador_relacional":56,"operacao_and":57,"operacao_or":58,"operacao_not":59,"AND":60,"OR":61,"NOT":62,"expressao_logica":63,"LE":64,"GE":65,"EQ":66,"NE":67,">":68,"<":69,"$accept":0,"$end":1},
terminals_: {2:"error",5:"EOF",8:"BREAK",9:";",10:"CONTINUE",15:"{",16:"}",18:"declaracao_funcao",21:"chamada_funcao",22:"IF",23:"(",24:")",25:"ELSE",27:"CASE",29:":",31:"DEFAULT",32:"SWITCH",33:"IDF",34:"WHILE",35:"FOR",36:"DO",37:"F_LIT",38:"INT_LIT",39:"CHAR_LIT",41:"INT",42:"DOUBLE",43:"FLOAT",44:"CHAR",45:"=",48:"*",50:"/",51:"%",52:"+",53:"-",60:"AND",61:"OR",62:"NOT",63:"expressao_logica",64:"LE",65:"GE",66:"EQ",67:"NE",68:">",69:"<"},
productions_: [0,[3,2],[4,2],[4,1],[6,1],[6,2],[6,2],[6,1],[6,1],[6,1],[6,1],[14,3],[14,2],[7,2],[7,1],[7,2],[7,2],[7,2],[11,9],[11,7],[26,4],[26,5],[26,1],[30,3],[13,7],[12,7],[12,7],[12,7],[12,6],[12,7],[12,6],[12,7],[12,7],[12,6],[12,7],[12,6],[12,9],[28,1],[28,1],[28,1],[40,1],[40,1],[40,1],[40,1],[17,2],[17,4],[19,3],[19,4],[19,3],[19,1],[47,1],[47,2],[47,2],[47,2],[47,2],[47,2],[49,3],[49,3],[46,1],[46,3],[46,3],[54,1],[54,3],[54,3],[55,1],[55,1],[55,3],[20,4],[20,3],[20,4],[20,3],[20,2],[20,1],[20,1],[57,1],[58,1],[59,2],[56,1],[56,1],[56,1],[56,1],[56,1],[56,1]],
performAction: function anonymous(yytext, yyleng, yylineno, yy, yystate /* action[1] */, $$ /* vstack */, _$ /* lstack */) {
/* this == yyval */

var $0 = $$.length - 1;
switch (yystate) {
case 1:
console.log(1)
break;
case 36:
console.log('DO WHILE statement')
break;
case 37:
this.$ = parseFloat($$[$0])
break;
case 38:
this.$ = parseInt($$[$0])
break;
case 39:
this.$ = $$[$0]
break;
case 44:
criarVariavel($$[$0-1], $$[$0], null)
break;
case 45:

	criarVariavel($$[$0-3], $$[$0-2], $$[$0].value)
	const leftChild = new Node($$[$0-2])
    const node = new Node($$[$0-1], leftChild, $$[$0].node)
	const arvore = new AST(node)
	arvore.postorder(node)
	arvore.inorder(node)
	arvore.bylevel(node)
	console.log(posOrdem)
	console.log(inOrdem)
	console.log(byLevel)
	
break;
case 56:
console.log('Incremento ++')
break;
case 57:
console.log('Decremento --')
break;
case 58: case 61:
	this.$ = {
    	type: $$[$0].type,
    	value: $$[$0].value,
    	arguments: $$[$0].arguments,
    	node: $$[$0].node
    	}
    
break;
case 59:
	this.$ = {
    	type: $$[$0-1],
    	value: $$[$0-2].value + $$[$0].value,
    	arguments: [$$[$0-2], $$[$0]],
    	node: new Node($$[$0-1], $$[$0-2].node, $$[$0].node)
    	}
    
break;
case 60:
	this.$ = {
    	type: $$[$0-1],
    	value: $$[$0-2].value - $$[$0].value,
    	arguments: [$$[$0-2], $$[$0]],
    	node: new Node($$[$0-1], $$[$0-2].node, $$[$0].node)
    	}
    
break;
case 62:
	this.$ = {
    	type: $$[$0-1],
    	value: $$[$0-2].value * $$[$0].value,
    	arguments: [$$[$0-2], $$[$0]],
    	node: new Node($$[$0-1], $$[$0-2].node, $$[$0].node)
    	}
    
break;
case 63:
	
    	if($$[$0-2].type == $$[$0].type == 'IDF'){
    		//chamar função de verificar se variáveis foram declaradas
    		//chamar função que acha o valor de uma variável já declarada
    	}
    	this.$ = {
    	type: $$[$0-1],
    	value: $$[$0-2].value / $$[$0].value,
    	arguments: [$$[$0-2], $$[$0]],
    	node: new Node($$[$0-1], $$[$0-2].node, $$[$0].node)
    	}
    
break;
case 64:
	
    	this.$ = {
    	type: $$[$0],
    	value: $$[$0],
    	arguments: [],
    	node: new Node($$[$0])
    	}
    
break;
case 65:
	this.$ = {
    	type: $$[$0],
    	value: $$[$0],
    	arguments: [],
    	node: new Node($$[$0])
    	}
    
break;
case 66:
	this.$ = {
    	type: '$$[$0-1].type',
    	value: $$[$0-1].value,
    	arguments: $$[$0-1].arguments,
    	node: $$[$0-1].node
    	}
    
break;
case 82:
console.log($$[$0])
break;
}
},
table: [{3:1,4:2,6:3,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{1:[3]},{5:[1,35],6:36,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},o($Vi,[2,3]),o($Vj,[2,4]),{9:[1,37]},{9:[1,38]},o($Vj,[2,7]),o($Vj,[2,8]),o($Vj,[2,9]),o($Vj,[2,10]),{9:[1,39]},o($Vk,[2,14]),{9:[1,40]},{9:[1,41]},{9:[1,42]},{23:[1,43]},{23:[1,44]},{23:[1,45]},{15:[1,46]},{23:[1,47]},{4:48,6:3,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,16:[1,49],17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{33:[1,50]},{45:$Vl,47:51,48:$Vm,50:$Vn,51:$Vo,52:$Vp,53:$Vq,56:52,64:$Vr,65:$Vs,66:$Vt,67:$Vu,68:$Vv,69:$Vw},o($Vx,[2,49]),{20:65,33:$Vy,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},o($Vx,[2,72]),o($Vx,[2,73]),{33:[2,40]},{33:[2,41]},{33:[2,42]},{33:[2,43]},o([33,60,61,62],[2,74]),o($Vx,[2,75]),{63:[1,67]},{1:[2,1]},o($Vi,[2,2]),o($Vj,[2,5]),o($Vj,[2,6]),o($Vk,[2,13]),o($Vk,[2,15]),o($Vk,[2,16]),o($Vk,[2,17]),{20:68,33:$Vy,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{20:69,33:$Vy,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{7:70,9:[1,71],17:11,18:$V3,19:13,20:14,21:$V4,33:$V7,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{6:72,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{33:[1,73]},{6:36,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,16:[1,74],17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},o($Vj,[2,12]),{9:[2,44],45:[1,75]},{21:[1,78],23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,46:76,48:[1,77],54:79,55:80},{28:88,33:[1,87],37:$VB,38:$VC,39:$VD},{45:[1,90],52:[1,89]},{45:[1,92],53:[1,91]},o($VE,[2,50]),{45:[1,93]},{45:[1,94]},{45:[1,95]},o($VF,[2,77]),o($VF,[2,78]),o($VF,[2,79]),o($VF,[2,80]),o($VF,[2,81]),o($VF,[2,82]),o($Vx,[2,71]),{56:52,64:$Vr,65:$Vs,66:$Vt,67:$Vu,68:$Vv,69:$Vw},o($Vx,[2,76]),{24:[1,96]},{24:[1,97]},{7:98,9:[1,99],17:11,18:$V3,19:13,20:14,21:$V4,33:$V7,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{7:100,9:[1,101],17:11,18:$V3,19:13,20:14,21:$V4,33:$V7,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{16:[1,102]},{24:[1,103]},o($Vj,[2,11]),{23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,46:104,54:79,55:80},o($Vx,[2,46],{52:$VG,53:$VH}),{23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,46:107,54:79,55:80},o($Vx,[2,48]),o($VI,[2,58],{48:$VJ,50:$VK}),o($VL,[2,61]),o($VL,[2,64]),o($VL,[2,65]),{23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,46:110,54:79,55:80},o($VM,[2,37]),o($VM,[2,38]),o($VM,[2,39]),o($Vx,[2,68],{57:25,58:26,59:27,20:111,33:$Vy,60:$Vf,61:$Vg,62:$Vh}),o($Vx,[2,70],{57:25,58:26,59:27,20:112,33:$Vy,60:$Vf,61:$Vg,62:$Vh}),o($Vx,[2,56]),o($VE,[2,54]),o($Vx,[2,57]),o($VE,[2,55]),o($VE,[2,51]),o($VE,[2,52]),o($VE,[2,53]),{15:[1,113]},{15:[1,114]},{19:116,24:[1,117],33:[1,115],49:24},{24:[1,119],33:[1,118]},{19:121,24:[1,122],33:[1,120],49:24},{19:123,24:[1,124],33:[1,125],49:24},{34:[1,126]},{15:[1,127]},{9:[2,45],52:$VG,53:$VH},{23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,54:128,55:80},{23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,54:129,55:80},o($Vx,[2,47],{52:$VG,53:$VH}),{23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,55:130},{23:$Vz,28:82,33:$VA,37:$VB,38:$VC,39:$VD,55:131},{24:[1,132],52:$VG,53:$VH},o($Vx,[2,67]),o($Vx,[2,69]),{6:133,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{6:134,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{24:[1,135],45:$Vl,47:51,48:$Vm,50:$Vn,51:$Vo,52:$Vp,53:$Vq},{24:[1,136]},{6:137,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{24:[1,138]},{6:139,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{24:[1,140],45:$Vl,47:51,48:$Vm,50:$Vn,51:$Vo,52:$Vp,53:$Vq},{24:[1,141]},{6:142,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{24:[1,143]},{6:144,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{45:$Vl,47:51,48:$Vm,50:$Vn,51:$Vo,52:$Vp,53:$Vq},{23:[1,145]},{26:146,27:$VN,30:148,31:$VO},o($VI,[2,59],{48:$VJ,50:$VK}),o($VI,[2,60],{48:$VJ,50:$VK}),o($VL,[2,62]),o($VL,[2,63]),o($VL,[2,66]),{16:[1,150]},{16:[1,151]},{6:152,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{6:153,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},o($Vj,[2,28]),{6:154,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},o($Vj,[2,30]),{6:155,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{6:156,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},o($Vj,[2,33]),{6:157,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},o($Vj,[2,35]),{20:158,33:$Vy,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{16:[1,159]},{28:160,37:$VB,38:$VC,39:$VD},{16:[2,22]},{29:[1,161]},o($Vj,[2,19],{25:[1,162]}),o($Vj,[2,25]),o($Vj,[2,26]),o($Vj,[2,27]),o($Vj,[2,29]),o($Vj,[2,31]),o($Vj,[2,32]),o($Vj,[2,34]),{24:[1,163]},o($Vj,[2,24]),{29:[1,164]},{6:165,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{6:166,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{9:[1,167]},{6:168,7:4,8:$V0,10:$V1,11:7,12:8,13:9,14:10,15:$V2,17:11,18:$V3,19:13,20:14,21:$V4,22:$V5,32:$V6,33:$V7,34:$V8,35:$V9,36:$Va,40:22,41:$Vb,42:$Vc,43:$Vd,44:$Ve,49:24,57:25,58:26,59:27,60:$Vf,61:$Vg,62:$Vh},{16:[2,23]},o($Vj,[2,18]),o($Vj,[2,36]),{16:[2,20],26:169,27:$VN,30:148,31:$VO},{16:[2,21]}],
defaultActions: {28:[2,40],29:[2,41],30:[2,42],31:[2,43],35:[2,1],148:[2,22],165:[2,23],169:[2,21]},
parseError: function parseError (str, hash) {
    if (hash.recoverable) {
        this.trace(str);
    } else {
        var error = new Error(str);
        error.hash = hash;
        throw error;
    }
},
parse: function parse(input) {
    var self = this, stack = [0], tstack = [], vstack = [null], lstack = [], table = this.table, yytext = '', yylineno = 0, yyleng = 0, recovering = 0, TERROR = 2, EOF = 1;
    var args = lstack.slice.call(arguments, 1);
    var lexer = Object.create(this.lexer);
    var sharedState = { yy: {} };
    for (var k in this.yy) {
        if (Object.prototype.hasOwnProperty.call(this.yy, k)) {
            sharedState.yy[k] = this.yy[k];
        }
    }
    lexer.setInput(input, sharedState.yy);
    sharedState.yy.lexer = lexer;
    sharedState.yy.parser = this;
    if (typeof lexer.yylloc == 'undefined') {
        lexer.yylloc = {};
    }
    var yyloc = lexer.yylloc;
    lstack.push(yyloc);
    var ranges = lexer.options && lexer.options.ranges;
    if (typeof sharedState.yy.parseError === 'function') {
        this.parseError = sharedState.yy.parseError;
    } else {
        this.parseError = Object.getPrototypeOf(this).parseError;
    }
    function popStack(n) {
        stack.length = stack.length - 2 * n;
        vstack.length = vstack.length - n;
        lstack.length = lstack.length - n;
    }
    _token_stack:
        var lex = function () {
            var token;
            token = lexer.lex() || EOF;
            if (typeof token !== 'number') {
                token = self.symbols_[token] || token;
            }
            return token;
        };
    var symbol, preErrorSymbol, state, action, a, r, yyval = {}, p, len, newState, expected;
    while (true) {
        state = stack[stack.length - 1];
        if (this.defaultActions[state]) {
            action = this.defaultActions[state];
        } else {
            if (symbol === null || typeof symbol == 'undefined') {
                symbol = lex();
            }
            action = table[state] && table[state][symbol];
        }
                    if (typeof action === 'undefined' || !action.length || !action[0]) {
                var errStr = '';
                expected = [];
                for (p in table[state]) {
                    if (this.terminals_[p] && p > TERROR) {
                        expected.push('\'' + this.terminals_[p] + '\'');
                    }
                }
                if (lexer.showPosition) {
                    errStr = 'Parse error on line ' + (yylineno + 1) + ':\n' + lexer.showPosition() + '\nExpecting ' + expected.join(', ') + ', got \'' + (this.terminals_[symbol] || symbol) + '\'';
                } else {
                    errStr = 'Parse error on line ' + (yylineno + 1) + ': Unexpected ' + (symbol == EOF ? 'end of input' : '\'' + (this.terminals_[symbol] || symbol) + '\'');
                }
                this.parseError(errStr, {
                    text: lexer.match,
                    token: this.terminals_[symbol] || symbol,
                    line: lexer.yylineno,
                    loc: yyloc,
                    expected: expected
                });
            }
        if (action[0] instanceof Array && action.length > 1) {
            throw new Error('Parse Error: multiple actions possible at state: ' + state + ', token: ' + symbol);
        }
        switch (action[0]) {
        case 1:
            stack.push(symbol);
            vstack.push(lexer.yytext);
            lstack.push(lexer.yylloc);
            stack.push(action[1]);
            symbol = null;
            if (!preErrorSymbol) {
                yyleng = lexer.yyleng;
                yytext = lexer.yytext;
                yylineno = lexer.yylineno;
                yyloc = lexer.yylloc;
                if (recovering > 0) {
                    recovering--;
                }
            } else {
                symbol = preErrorSymbol;
                preErrorSymbol = null;
            }
            break;
        case 2:
            len = this.productions_[action[1]][1];
            yyval.$ = vstack[vstack.length - len];
            yyval._$ = {
                first_line: lstack[lstack.length - (len || 1)].first_line,
                last_line: lstack[lstack.length - 1].last_line,
                first_column: lstack[lstack.length - (len || 1)].first_column,
                last_column: lstack[lstack.length - 1].last_column
            };
            if (ranges) {
                yyval._$.range = [
                    lstack[lstack.length - (len || 1)].range[0],
                    lstack[lstack.length - 1].range[1]
                ];
            }
            r = this.performAction.apply(yyval, [
                yytext,
                yyleng,
                yylineno,
                sharedState.yy,
                action[1],
                vstack,
                lstack
            ].concat(args));
            if (typeof r !== 'undefined') {
                return r;
            }
            if (len) {
                stack = stack.slice(0, -1 * len * 2);
                vstack = vstack.slice(0, -1 * len);
                lstack = lstack.slice(0, -1 * len);
            }
            stack.push(this.productions_[action[1]][0]);
            vstack.push(yyval.$);
            lstack.push(yyval._$);
            newState = table[stack[stack.length - 2]][stack[stack.length - 1]];
            stack.push(newState);
            break;
        case 3:
            return true;
        }
    }
    return true;
}};

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

		bylevel(node)
		{
		    if(node !== null)
		    {
		    	bylevel.push(node.type);
		        this.bylevel(node.leftChild);
		        this.bylevel(node.rightChild);
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
   
    
/* generated by jison-lex 0.3.4 */
var lexer = (function(){
var lexer = ({

EOF:1,

parseError:function parseError(str, hash) {
        if (this.yy.parser) {
            this.yy.parser.parseError(str, hash);
        } else {
            throw new Error(str);
        }
    },

// resets the lexer, sets new input
setInput:function (input, yy) {
        this.yy = yy || this.yy || {};
        this._input = input;
        this._more = this._backtrack = this.done = false;
        this.yylineno = this.yyleng = 0;
        this.yytext = this.matched = this.match = '';
        this.conditionStack = ['INITIAL'];
        this.yylloc = {
            first_line: 1,
            first_column: 0,
            last_line: 1,
            last_column: 0
        };
        if (this.options.ranges) {
            this.yylloc.range = [0,0];
        }
        this.offset = 0;
        return this;
    },

// consumes and returns one char from the input
input:function () {
        var ch = this._input[0];
        this.yytext += ch;
        this.yyleng++;
        this.offset++;
        this.match += ch;
        this.matched += ch;
        var lines = ch.match(/(?:\r\n?|\n).*/g);
        if (lines) {
            this.yylineno++;
            this.yylloc.last_line++;
        } else {
            this.yylloc.last_column++;
        }
        if (this.options.ranges) {
            this.yylloc.range[1]++;
        }

        this._input = this._input.slice(1);
        return ch;
    },

// unshifts one char (or a string) into the input
unput:function (ch) {
        var len = ch.length;
        var lines = ch.split(/(?:\r\n?|\n)/g);

        this._input = ch + this._input;
        this.yytext = this.yytext.substr(0, this.yytext.length - len);
        //this.yyleng -= len;
        this.offset -= len;
        var oldLines = this.match.split(/(?:\r\n?|\n)/g);
        this.match = this.match.substr(0, this.match.length - 1);
        this.matched = this.matched.substr(0, this.matched.length - 1);

        if (lines.length - 1) {
            this.yylineno -= lines.length - 1;
        }
        var r = this.yylloc.range;

        this.yylloc = {
            first_line: this.yylloc.first_line,
            last_line: this.yylineno + 1,
            first_column: this.yylloc.first_column,
            last_column: lines ?
                (lines.length === oldLines.length ? this.yylloc.first_column : 0)
                 + oldLines[oldLines.length - lines.length].length - lines[0].length :
              this.yylloc.first_column - len
        };

        if (this.options.ranges) {
            this.yylloc.range = [r[0], r[0] + this.yyleng - len];
        }
        this.yyleng = this.yytext.length;
        return this;
    },

// When called from action, caches matched text and appends it on next action
more:function () {
        this._more = true;
        return this;
    },

// When called from action, signals the lexer that this rule fails to match the input, so the next matching rule (regex) should be tested instead.
reject:function () {
        if (this.options.backtrack_lexer) {
            this._backtrack = true;
        } else {
            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. You can only invoke reject() in the lexer when the lexer is of the backtracking persuasion (options.backtrack_lexer = true).\n' + this.showPosition(), {
                text: "",
                token: null,
                line: this.yylineno
            });

        }
        return this;
    },

// retain first n characters of the match
less:function (n) {
        this.unput(this.match.slice(n));
    },

// displays already matched input, i.e. for error messages
pastInput:function () {
        var past = this.matched.substr(0, this.matched.length - this.match.length);
        return (past.length > 20 ? '...':'') + past.substr(-20).replace(/\n/g, "");
    },

// displays upcoming input, i.e. for error messages
upcomingInput:function () {
        var next = this.match;
        if (next.length < 20) {
            next += this._input.substr(0, 20-next.length);
        }
        return (next.substr(0,20) + (next.length > 20 ? '...' : '')).replace(/\n/g, "");
    },

// displays the character position where the lexing error occurred, i.e. for error messages
showPosition:function () {
        var pre = this.pastInput();
        var c = new Array(pre.length + 1).join("-");
        return pre + this.upcomingInput() + "\n" + c + "^";
    },

// test the lexed token: return FALSE when not a match, otherwise return token
test_match:function(match, indexed_rule) {
        var token,
            lines,
            backup;

        if (this.options.backtrack_lexer) {
            // save context
            backup = {
                yylineno: this.yylineno,
                yylloc: {
                    first_line: this.yylloc.first_line,
                    last_line: this.last_line,
                    first_column: this.yylloc.first_column,
                    last_column: this.yylloc.last_column
                },
                yytext: this.yytext,
                match: this.match,
                matches: this.matches,
                matched: this.matched,
                yyleng: this.yyleng,
                offset: this.offset,
                _more: this._more,
                _input: this._input,
                yy: this.yy,
                conditionStack: this.conditionStack.slice(0),
                done: this.done
            };
            if (this.options.ranges) {
                backup.yylloc.range = this.yylloc.range.slice(0);
            }
        }

        lines = match[0].match(/(?:\r\n?|\n).*/g);
        if (lines) {
            this.yylineno += lines.length;
        }
        this.yylloc = {
            first_line: this.yylloc.last_line,
            last_line: this.yylineno + 1,
            first_column: this.yylloc.last_column,
            last_column: lines ?
                         lines[lines.length - 1].length - lines[lines.length - 1].match(/\r?\n?/)[0].length :
                         this.yylloc.last_column + match[0].length
        };
        this.yytext += match[0];
        this.match += match[0];
        this.matches = match;
        this.yyleng = this.yytext.length;
        if (this.options.ranges) {
            this.yylloc.range = [this.offset, this.offset += this.yyleng];
        }
        this._more = false;
        this._backtrack = false;
        this._input = this._input.slice(match[0].length);
        this.matched += match[0];
        token = this.performAction.call(this, this.yy, this, indexed_rule, this.conditionStack[this.conditionStack.length - 1]);
        if (this.done && this._input) {
            this.done = false;
        }
        if (token) {
            return token;
        } else if (this._backtrack) {
            // recover context
            for (var k in backup) {
                this[k] = backup[k];
            }
            return false; // rule action called reject() implying the next rule should be tested instead.
        }
        return false;
    },

// return next match in input
next:function () {
        if (this.done) {
            return this.EOF;
        }
        if (!this._input) {
            this.done = true;
        }

        var token,
            match,
            tempMatch,
            index;
        if (!this._more) {
            this.yytext = '';
            this.match = '';
        }
        var rules = this._currentRules();
        for (var i = 0; i < rules.length; i++) {
            tempMatch = this._input.match(this.rules[rules[i]]);
            if (tempMatch && (!match || tempMatch[0].length > match[0].length)) {
                match = tempMatch;
                index = i;
                if (this.options.backtrack_lexer) {
                    token = this.test_match(tempMatch, rules[i]);
                    if (token !== false) {
                        return token;
                    } else if (this._backtrack) {
                        match = false;
                        continue; // rule action called reject() implying a rule MISmatch.
                    } else {
                        // else: this is a lexer rule which consumes input without producing a token (e.g. whitespace)
                        return false;
                    }
                } else if (!this.options.flex) {
                    break;
                }
            }
        }
        if (match) {
            token = this.test_match(match, rules[index]);
            if (token !== false) {
                return token;
            }
            // else: this is a lexer rule which consumes input without producing a token (e.g. whitespace)
            return false;
        }
        if (this._input === "") {
            return this.EOF;
        } else {
            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. Unrecognized text.\n' + this.showPosition(), {
                text: "",
                token: null,
                line: this.yylineno
            });
        }
    },

// return next match that has a token
lex:function lex () {
        var r = this.next();
        if (r) {
            return r;
        } else {
            return this.lex();
        }
    },

// activates a new lexer condition state (pushes the new lexer condition state onto the condition stack)
begin:function begin (condition) {
        this.conditionStack.push(condition);
    },

// pop the previously active lexer condition state off the condition stack
popState:function popState () {
        var n = this.conditionStack.length - 1;
        if (n > 0) {
            return this.conditionStack.pop();
        } else {
            return this.conditionStack[0];
        }
    },

// produce the lexer rule set which is active for the currently active lexer condition state
_currentRules:function _currentRules () {
        if (this.conditionStack.length && this.conditionStack[this.conditionStack.length - 1]) {
            return this.conditions[this.conditionStack[this.conditionStack.length - 1]].rules;
        } else {
            return this.conditions["INITIAL"].rules;
        }
    },

// return the currently active lexer condition state; when an index argument is provided it produces the N-th previous condition state, if available
topState:function topState (n) {
        n = this.conditionStack.length - 1 - Math.abs(n || 0);
        if (n >= 0) {
            return this.conditionStack[n];
        } else {
            return "INITIAL";
        }
    },

// alias for begin(condition)
pushState:function pushState (condition) {
        this.begin(condition);
    },

// return the number of states currently on the stack
stateStackSize:function stateStackSize() {
        return this.conditionStack.length;
    },
options: {},
performAction: function anonymous(yy,yy_,$avoiding_name_collisions,YY_START) {
var YYSTATE=YY_START;
switch($avoiding_name_collisions) {
case 0:/* ignorar */
break;
case 1:return 41;
break;
case 2:return 42;
break;
case 3:return 43;
break;
case 4:return 44;
break;
case 5:return 48;
break;
case 6:return 52;
break;
case 7:return 53;
break;
case 8:return 50;
break;
case 9:return ',';
break;
case 10:return 9;
break;
case 11:return 29;
break;
case 12:return '.';
break;
case 13:return 'QUOTE';
break;
case 14:return 'DQUOTE';
break;
case 15:return 23;
break;
case 16:return 24;
break;
case 17:return '[';
break;
case 18:return ']';
break;
case 19:return 15;
break;
case 20:return 16;
break;
case 21:return 69;
break;
case 22:return 68;
break;
case 23:return 45;
break;
case 24:return 64;
break;
case 25:return 65;
break;
case 26:return 66;
break;
case 27:return 67;
break;
case 28:return 60;
break;
case 29:return 61;
break;
case 30:return 62;
break;
case 31:return 22;
break;
case 32:return 32;
break;
case 33:return 27;
break;
case 34:return 8;
break;
case 35:return 31;
break;
case 36:return 25;
break;
case 37:return 34;
break;
case 38:return 36;
break;
case 39:return 35;
break;
case 40:return 'VAR';
break;
case 41:return '#';
break;
case 42:return 'DEFINE';
break;
case 43:return 33;
break;
case 44:return 37;
break;
case 45:return 38;
break;
case 46:return 39;
break;
case 47:console.log('Erro léxico: caractere [', yy_.yytext, '] não reconhecido.');
break;
case 48:console.log('Token EOF'); return 5;
break;
}
},
rules: [/^(?:\s+)/,/^(?:int\b)/,/^(?:double\b)/,/^(?:float\b)/,/^(?:char\b)/,/^(?:\*)/,/^(?:\+)/,/^(?:-)/,/^(?:\/)/,/^(?:,)/,/^(?:;)/,/^(?::)/,/^(?:\.)/,/^(?:')/,/^(?:")/,/^(?:\()/,/^(?:\))/,/^(?:\[)/,/^(?:\])/,/^(?:\{)/,/^(?:\})/,/^(?:< )/,/^(?:> )/,/^(?:=)/,/^(?:<=)/,/^(?:>=)/,/^(?:==)/,/^(?:!=)/,/^(?:&&)/,/^(?:\|\|)/,/^(?:!)/,/^(?:if\b)/,/^(?:switch\b)/,/^(?:case\b)/,/^(?:break\b)/,/^(?:default\b)/,/^(?:else\b)/,/^(?:while\b)/,/^(?:do\b)/,/^(?:for\b)/,/^(?:(?:\w+\s+)([a-zA-Z_][a-zA-Z0-9_]*))/,/^(?:#)/,/^(?:define\b)/,/^(?:[a-zA-Z][a-zA-Z0-9_]*)/,/^(?:[0-9]*\.[0-9]+([eE][+-][0-9]+)?)/,/^(?:[0-9]+)/,/^(?:'[a-zA-Z0-9_]')/,/^(?:.)/,/^(?:$)/],
conditions: {"INITIAL":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48],"inclusive":true}}
});
return lexer;
})();
parser.lexer = lexer;
function Parser () {
  this.yy = {};
}
Parser.prototype = parser;parser.Parser = Parser;
return new Parser;
})();


if (typeof require !== 'undefined' && typeof exports !== 'undefined') {
exports.parser = C_Compiler_parte1_parte2;
exports.Parser = C_Compiler_parte1_parte2.Parser;
exports.parse = function () { return C_Compiler_parte1_parte2.parse.apply(C_Compiler_parte1_parte2, arguments); };
exports.main = function commonjsMain (args) {
    if (!args[1]) {
        console.log('Usage: '+args[0]+' FILE');
        process.exit(1);
    }
    var source = require('fs').readFileSync(require('path').normalize(args[1]), "utf8");
    return exports.parser.parse(source);
};
if (typeof module !== 'undefined' && require.main === module) {
  exports.main(process.argv.slice(1));
}
}