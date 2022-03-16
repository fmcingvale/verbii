/*
	Syntax - takes words from Reader, recognizes any syntax forms, and returns
	objects to Interpreter.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;
using System.Collections.Generic;
#nullable enable

public class Syntax {

	public Reader reader;

	public Syntax() {
		reader = new Reader();
	}

	// mirrored from Reader
	public void addText(string text) { reader.addText(text); }
	public void clearAll() { reader.clearAll(); }
	public void pushObjList(List<LangObject> objs) { reader.pushObjList(objs); }
	public void popObjList() { reader.popObjList(); }
	public bool hasPushedObjLists() { return reader.hasPushedObjLists(); }
 
	public LangObject nextObjOrFail() {
		if(peekObj() == null) {
			throw new LangError("Unexpected end of input");
		}
		return nextObj()!;
	}

	public LangSymbol nextSymbolOrFail() {
		var obj = nextObjOrFail();
		if(!(obj is LangSymbol)) {
			throw new LangError("Expecting symbol but got " + obj.fmtStackPrint());
		}
		return (obj as LangSymbol)!;
	}

	public LangObject prevObjOrFail() {
		if(peekPrevObj() == null) {
			throw new LangError("Unable to find previous word");
		}
		return prevObj()!;
	}

	// see C++ version for verbose comments; minimal comments here
	public LangObject parse_lambda() {
		// turn { ... } into an anonymous wordlist
		
		// delete the { that was just read
		reader.deletePrevObj();

		var objlist = new List<LangObject>();
		int nesting = 1;
		while(true) {
			var obj = nextObj();
			if(obj == null) {
				throw new LangError("Unexpected end of input inside { .. }");
			}
			var sym = obj as LangSymbol;
			// delete the { ... } as I read it -- will replace it with a lambda reference
			reader.deletePrevObj();
			if(sym != null && sym.match("{")) {
				// if I find inner lambdas, just copy them for now and later when they are run, 
				// this same process will happen for them
				++nesting;
				objlist.Add(obj);
			}
			else if(sym != null && sym.match("}")) {
				if(--nesting > 0) {
					objlist.Add(obj);
					continue;
				}

				var _lambda = new LangLambda(objlist);
				//Console.WriteLine("NEW LAMBDA IS:");
				//foreach(var o in objlist) {
				//	Console.WriteLine(o.fmtStackPrint());
				//}

				// replace { .. } in source objlist with lambda.
				reader.insertPrevObj(_lambda);
				// return lambda as the read object. on subsequent passes, the interpreter
				// will see the lambda directly in the objlist
				return _lambda;
			}
			else {
				objlist.Add(obj);
			}
		}
	}

	public LangObject? parse_comment() {
		int nesting = 1;
		while(true) {
			var sym = reader.nextObj() as LangSymbol;
			//if(sym != null) {
			//	Console.WriteLine("COMMENT READ:" + sym.fmtStackPrint());
			//}
			if(sym == null) {
				throw new LangError("Unexpected end of input inside comment");
			}
			else if(sym.value == ")") {
				if(--nesting == 0) {
					return nextObj();
				}
			}
			else if(sym.value == "(") {
				++nesting;
			}
		}
	}

	public LangObject? parse_quote_printstring() {
		// ." some string here " -- rewrite to '"some string here" .'
		//Console.WriteLine("PARSING .\" ...");
		string s = "";
		// delete ."
		reader.deletePrevObj();
		while(true) {
			var obj = reader.nextObj(); // *NOT* Syntax::nextObj() - don't want any processing, i.e.
										// don't want numbers in middle of string to be converted to ints
			if(obj == null) {
				throw new LangError("Unexpected end of input inside .\"");
			}
			var sym = obj as LangSymbol;
			reader.deletePrevObj(); // delete each obj as I read it, will replace at end
			if(sym == null) {
				throw new LangError("Got non-symbol from Reader (!!):" + obj.fmtStackPrint());
			}
			else if(sym.match("\"")) {
				// end of string - write new code
				//Console.WriteLine("FINISHED STRING: " + s);
				reader.insertPrevObj(new LangString(s));
				// use only builtin functions so it works even with -noinit
				reader.insertPrevObj(new LangSymbol("puts"));
				// (will have blank at end, due to concatenation code below, so don't need to add one)
				// since i rewrote into multiple objects (unlike the lambda case above), 
				// backup the reader to string and return it
				for(int i=0; i<2; ++i) {
					reader.prevObj();
				}
				//reader.debug_print_objlist();
				return reader.nextObj();
			}
			else {
				// since i called reader.nextObj(), this will always be a symbol
				s += sym.value + " ";
			}
		}
	}

	// DON'T use regex here -- this is run for potentially every word so has to be fast
	public bool isInteger(string text) {
		int i=0;
		bool hasdigits=false;
		if(text[i] == '+' || text[i] == '-') {
			++i;
		}
		while(i < text.Length && text[i] >= '0' && text[i] <= '9') {
			++i;
			hasdigits=true;
		}
		return (i == text.Length && hasdigits);
	}

	public LangObject? nextObj() {
		var obj = reader.nextObj();
		if (obj is LangSymbol) {
			var sym = obj as LangSymbol;	

			// floats: #NNN.NN
			if(sym!.match("#",1)) {
				var fobj = new LangFloat(double.Parse(sym!.value.Substring(1)));
				reader.deletePrevObj();
				reader.insertPrevObj(fobj);
				return fobj;
			}	
			else if(sym!.match("{")) {
				return parse_lambda();
			}
			else if(sym!.match("(")) {
				return parse_comment();
			}
			else if(sym!.match(".\"")) {
				return parse_quote_printstring();
			}
			// integers (last since test is more expensive than those above)
			// [in practice this doesn't help too much since MOST words in a real
			// program won't match any of these rules, so all have to run for nearly every word]
			// may help more later if/when most words are NOT left as symbols but converted
			// to some other object
			else if(isInteger(sym!.value)) {
				var intobj = new LangInt(int.Parse(sym!.value));
				reader.deletePrevObj();
				reader.insertPrevObj(intobj);
				return intobj;
			}
		}
		return obj;
	}

	public LangObject? peekObj() {
		// I need the above processing to occur on peeked objects,
		// so do it this way ....
		var obj = nextObj();
		if(obj != null) {
			reader.prevObj(); // rewind
		}
		return obj;
	}

	public LangObject? prevObj() { return reader.prevObj(); }
	public LangObject? peekPrevObj() { return reader.peekPrevObj(); }

	public void debug_print_objlist() { reader.debug_print_objlist(); }
};

