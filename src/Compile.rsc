module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM;
import lang::html::AST;
import lang::html::IO;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTMLElement type and the `str writeHTMLString(HTMLElement x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
  UseDef def = resolve(f).useDef;
	return html("
		'\<head\>
		'  \<title\><f.name>\</title\>
		'  \<script src=\"<f.src[extension="js"].file>\"\><endTag("script")>
		'\</head\>
		'\<body\>
		'  <item("div", "\<<h1>\><"">\</<h1>\>\n")>
		'  <startTag("div", " class=\"main\"")>
		'    <convert(f.aQuestions, def)>
		'	 <endTag("div")>
		'\</body\>");
}


str convert(list[AQuestion] questions, UseDef def) {
  str converted = "";

	for(AQuestion questionn <- questions) {
    
		switch(questionn) {
      
			case question(str questionQuery, AId varName, AType typee):
				converted += "
          '<startTag("div", " id=\"<varName.src.offset>Q\"")>
          '		<item("\<<p>\><questionQuery>\</<p>\>\n")>
          '  	<inputMaker(typee, varName, questionQuery)>
          '<endTag("div")>";

			case compQuestion(str questionQuery, AId varName, AType typee, AExpr expr):
        converted += "<startTag("div", " id=\"<varName.src.offset>CQ\"")>";
        switch(typee) {
          case stringType():
            converted += "  \<type=text id=<varName.name>\>";
          case integerType():
            converted += "  \<type=number id=<varName.name>\>";
        }
        converted += "<endTag("div")>";

			case block(list[AQuestion] aQuestions):
				converted += "<startTag("div", " id=\"Block\"")>
                    '  <convert(aQuestions, def)>
                    '<endTag("div")>";

  		case ifThen(AExpr expr, list[AQuestion] aQuestions):
        converted += "
                    '  <startTag("div", " id=\"IF_<offset>_true\" style=display:None")>
                    '    <convert(aQuestions, def)>
                    '	 <endTag("div")>
                    '<endTag("div")>";

  		case IfElse(AExpr expr, list[AQuestion] aQuestions, list[AQuestion] aQuestions2):
        converted += "<startTag("div", " id=\"IF_<offset>\"")>
                    '  <startTag("div", " id=\"IF_<offset>_true\" style=display:None")>
                    '    <convert(aQuestions, def)>
                    '	 <endTag("div")>
                    '	 <startTag("div", " id=\"IF_<offset>_false\" style=display:None")>
                    '    <convert(aQuestions2, def)>
                    '	 <endTag("div")>
                    '<endTag("div")>";
		};
		converted += "";
	}
	return converted;
}


str form2js(AExpr expr) {
  switch (expr) {
		case ref(AId id): return "this.<id.name>";
		case boolean(AType b): return "<b>";
    case string(str string): return "<string>";
    case integer(int integer): return "<integer>";
    case par(AExpr left): return "(<form2js(left)>)";
    case add(AExpr left, AExpr right): return "<form2js(left)>+<form2js(right)>";
    case sub(AExpr left, AExpr right): return "<form2js(left)>-<form2js(right)>";
    case mul(AExpr left, AExpr right): return "<form2js(left)>*<form2js(right)>";
    case div(AExpr left, AExpr right): return "<form2js(left)>/<form2js(right)>";
    case eq(AExpr left, AExpr right): return "<form2js(left)>===<form2js(right)>";
    case gt(AExpr left, AExpr right): return "<form2js(left)>\><form2js(right)>";
    case lt(AExpr left, AExpr right): return "<form2js(left)>\<<form2js(right)>";
	  case geq(AExpr left, AExpr right): return "<form2js(left)>\>=<form2js(right)>";
	  case leq(AExpr left, AExpr right): return "<form2js(left)>\<=<form2js(right)>";
	  case neq(AExpr left, AExpr right): return "<form2js(left)>!==<form2js(right)>";
    case not(AExpr left, AExpr right): return "!<form2js(left)>";
  	case and(AExpr left, AExpr right): return "<form2js(left)> && <form2js(right)>";
  	case or(AExpr left, AExpr right): return "<form2js(left)> || <form2js(right)>";		
		default: 
      throw "";
	}
}

str val(AType typ) {
  switch (typ) {
    case integer():
      return ("0");
    case boolean():
      return "false";
    case string():
      return "\"\"";
  }
  return "";
}


