/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ version.
*/
using System;
using System.Collections.Generic;

class Builtins {
	public static LangInt popInt(Interpreter intr) {
		var obj = intr.pop();
		var i = obj as LangInt;
		if(i == null) {
			throw new LangError("Expecting int but got: " + obj.repr());
		}
		return i;
	}

	// see C++ version for extensive comments on divmod
	public static void int_divmod(Interpreter intr) {
		var b = popInt(intr).value;
		var a = popInt(intr).value;
	
		if(b == 0) {
			throw new LangError("Divide by zero");
		}
		int quot = (int)Math.Floor(((double)(Math.Abs(a))) / ((double)(Math.Abs(b))));

		bool samesign = (a < 0 && b < 0) || (a >=0 && b >= 0);
		int mod;
		if(samesign) {
			mod = a - quot*b;
		}
		else {
			mod = a + quot*b;
			quot = -quot;
		}

		intr.push(new LangInt(mod));
		intr.push(new LangInt(quot));
	}

	public static void define_word(Interpreter intr) {
		var name = intr.nextWordOrFail();
		var words = new List<string>();
	
		while(true) {
			var w = intr.nextWordOrFail();
			if(w == ";") {
				intr.WORDS[name] = words;
				return;
			}
			else {
				words.Add(w);
			}
		}
	}

	public static void printchar(Interpreter intr) {
		int c = popInt(intr).value;
		char ch = (char)c;
		Console.Write(ch);
		if(c == 10 || c == 13) {
			Console.Out.Flush();
		}
	}

	// ." some string here " -- print string
	public static void print_string(Interpreter intr) {
		while(true) {
			var word = intr.nextWordOrFail();
			if(word == "\"") {
				return; // end of string
			}
			else {
				Console.Write(word + " ");
			}
		}
	}

	public static Dictionary<string,Action<Interpreter>> builtins = 
		new Dictionary<string,Action<Interpreter>> { 
		{"+", intr => intr.push(new LangInt(popInt(intr).value + popInt(intr).value))},
		{"-", intr => intr.push(new LangInt(-popInt(intr).value + popInt(intr).value))},
		{"*", intr => intr.push(new LangInt(popInt(intr).value * popInt(intr).value))},
		{"/mod", int_divmod},
		{"==", intr => intr.push(new LangBool(popInt(intr).value == popInt(intr).value))},
		{">", intr => intr.push(new LangBool(popInt(intr).value < popInt(intr).value))},
		{":", define_word},
		{".c", printchar},
		{".\"", print_string},
		{"repr", intr => Console.Write(intr.pop().repr())},
		{"depth", intr => intr.push(new LangInt(intr.SP_EMPTY - intr.SP))},
		{"SP", intr => intr.push(new LangInt(intr.SP))},
		{"LP", intr => intr.push(new LangInt(intr.LP))},
		
	};
}
