/*
	main.cs - Verbii frontend that only loads & runs boot.verb.b

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;
using System.IO;
using System.Text.RegularExpressions;

public class MinRepl {
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
		int i=0;
		string BOOTFILE = "";
		while(i<args.Length) {
			//Console.Write("ARG:" + args[i]);
			if(args[i] == "-stats") {
				MinRepl.SHOW_RUNTIME_STATS = true;
			}
			else if(args[i] == "-libdir") {
				if(i >= (args.Length-1)) {
					Console.WriteLine("Missing path after -libdir");
					return;
				}
				if(args[i+1][args[i+1].Length-1] != '/' &&
					args[i+1][args[i+1].Length-1] != '\\') {
					Console.WriteLine("-libdir paths must end with \\ or /");
					return;
					}
				string name = args[i+1] + "boot.verb.b";
				// use the first path where boot.verb.b exists
				//Console.WriteLine("TRYING PATH:" + name);
				if(File.Exists(name)) {
					//Console.WriteLine("Exists!");
					BOOTFILE = name;
				}
				
				// pass same args on to script since it needs to see the -libdirs too
				args_to_script.objlist.Add(new LangString(args[i]));
				args_to_script.objlist.Add(new LangString(args[i+1]));
				
				++i;
				
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
			++i;	
		}
		
		//Console.WriteLine("BOOTFILE:" + BOOTFILE);
		if(BOOTFILE.Length == 0) {
			Console.WriteLine("Cannot find boot.verb.b (you may need to add '-libdir PATH' to command line)");
			return;
		}

		while(true) {
			var intr = new Interpreter();
			try {
				// boot.verb expects cmdline args on top of stack
				intr.push(args_to_script);
				MinRepl.deserialize_and_run(intr, BOOTFILE);
				if(MinRepl.SHOW_RUNTIME_STATS)
					intr.printStats();
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