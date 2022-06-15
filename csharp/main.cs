/*
	main.cs - Verbii frontend that only loads & runs boot.verb.b

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;
using System.Text.RegularExpressions;

public class MinRepl {
	public static string BOOTFILE = "../lib/boot.verb.b";
	public static bool SHOW_RUNTIME_STATS = false;

	static void backtrace_curframe(Interpreter intr) {
		string trace = "";
		int nr = 7; // number of words to print in each frame
		while(nr-- > 0) {
			var o = intr.prevObj();
			if(o is LangVoid) {
				Console.WriteLine(trace);
				return;
			}
			else {
				trace = o.fmtStackPrint() + " " + trace;
			}
		}
		Console.WriteLine(trace);
	}

	public static void print_backtrace(Interpreter intr) {
		int i=0;
		while(true) {
			Console.WriteLine("FRAME " + i.ToString() + ": ");
			++i;
			backtrace_curframe(intr);
			if(intr.havePushedFrames()) {
				intr.code_return();
			}
			else {
				return;
			}
		}
	}

	public static void deserialize_and_run(Interpreter intr, string filename) {
		var fileIn = System.IO.File.OpenText(filename);
		Deserializer.deserialize_stream(intr, fileIn);
		// run __main__ in initlib to setup any globals
		var code = intr.lookupWordOrFail("__main__");
		// delete __main__ *before* running code so code can redefine __main__
		intr.deleteWord("__main__");

		intr.run(code, null);
	}
}

public class MainProgram
{
    public static void Main(string[] args)
    {
		var args_to_script = new LangList();
		for(int i=0; i<args.Length; ++i) {
			if(args[i] == "-stats") {
				MinRepl.SHOW_RUNTIME_STATS = true;
			}
			else if(args[i] == "--") {
				// rest of args (including '--') go to script
				while(i < args.Length) {
					args_to_script.objlist.Add(new LangString(args[i++]));
				}
				break;
			}
			else {
				// unknown arg - goes to script
				args_to_script.objlist.Add(new LangString(args[i]));
			}			
		}
		
		while(true) {
			var intr = new Interpreter();
			try {
				// boot.verb expects cmdline args on top of stack
				intr.push(args_to_script);
				MinRepl.deserialize_and_run(intr, MinRepl.BOOTFILE);
				break;
			}
			catch (LangError err) {
				var errmsg = "*** " + err.Message + " ***";
				if(Builtins.STACKTRACE_ON_EXCEPTION)
					MinRepl.print_backtrace(intr);
					
				Console.WriteLine(errmsg);
				if(Builtins.EXIT_ON_EXCEPTION)
					break;
			} 
		}
    }
}