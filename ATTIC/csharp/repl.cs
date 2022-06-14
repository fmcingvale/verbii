/*
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;
using System.Text.RegularExpressions;

public class Repl {
	static string INITLIB = "../lib/init.verb.b";
	static string COMPILERLIB = "../lib/compiler.verb.b";
	static string PATCHESLIB = "../lib/patches.verb";

	public static bool SHOW_RUNTIME_STATS = false;

	public static void deserialize_and_run(Interpreter intr, string filename) {
		var fileIn = System.IO.File.OpenText(filename);
		Deserializer.deserialize_stream(intr, fileIn);
		// run __main__ in initlib to setup any globals
		var code = intr.lookupWordOrFail("__main__");
		intr.run(code, null);
		// always delete __main__ after running, else next file will fail to load
		intr.deleteWord("__main__");
	}

	public static void compile_and_load(Interpreter intr, string text, bool allow_overwrite) {
		// set flag so make-word can overwrite existing words
		Builtins.ALLOW_OVERWRITING_WORDS = allow_overwrite;
		intr.push(new LangString(text));
		var code = intr.lookupWordOrFail("compile-and-load-string");
		intr.run(code, null);
		// turn flag back off (default)
		Builtins.ALLOW_OVERWRITING_WORDS = false;
	}

	public static int debug_hook(Interpreter intr, LangObject obj) {
		Console.WriteLine("=> " + intr.reprStack());
		Console.WriteLine("Run: " + obj.fmtStackPrint());
		Console.Write("press ENTER to continue ...");
		Console.ReadLine();
		return 0;
	}

	// use safe_ version below
	public static void compile_and_run(Interpreter intr, string text, bool singlestep, bool allow_overwrite=false) {
		compile_and_load(intr, text, allow_overwrite);
		
		// run __main__
		var code = intr.lookupWordOrFail("__main__");

		if(singlestep)
			intr.run(code, debug_hook);
		else
			intr.run(code, null);

		// as above, delete __main__ when done
		intr.deleteWord("__main__");
	}

	public static Interpreter make_interpreter() {
		var intr = new Interpreter();
		// load byte-compiled init.verb and compiler.verb to bootstrap interpreter
		deserialize_and_run(intr, INITLIB);
		deserialize_and_run(intr, COMPILERLIB);

		// compile & run patches, allowing it to overwrite existing words so that
		// init/compiler can be patched without affecting their .b files
		var text = System.IO.File.ReadAllText(PATCHESLIB);
		compile_and_run(intr, text, false, true);
		
		return intr;
	}

	void backtrace_curframe(Interpreter intr) {
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

	void print_backtrace(Interpreter intr) {
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

	public string safe_compile_and_run(Interpreter intr, string text, bool singlestep,
										bool backtrace_on_error) {
		try {
			compile_and_run(intr, text, singlestep);
			return "";
		}
		catch (LangError err) {
			var errmsg = "*** " + err.Message + " ***";
			if(backtrace_on_error) {
				print_backtrace(intr);
			}
			return errmsg;
		} 
	}

	public void run_repl(bool singlestep) {
		Console.WriteLine("Verbii running on C#");
		var intr = make_interpreter();
	
		while(true) {
			Console.Write(">> ");
			Console.Out.Flush();
			var line = Console.In.ReadLine();
			if(line == null || line == "quit" || line == ",q") {
				if(SHOW_RUNTIME_STATS)
					intr.printStats();
				return;
			}
			if(line != null) {
				var errmsg = safe_compile_and_run(intr, line, singlestep, true);
				if(errmsg != "") {
					Console.WriteLine(errmsg);
					intr = make_interpreter(); // restart on error
				}
				else {
					Console.Write("=> " + intr.reprStack() + "\n");
				}
			}
		}
	} 

	// like a non-interactive repl, reads a line at a time, prints it,
	// runs it, then prints the stack. this is intented for unittesting.
	public void run_test_mode(string filename) {

		var re_blankline = new Regex(@"(^[ \t\r\n]*$)");
		var intr = make_interpreter();
		var all_lines = System.IO.File.ReadAllLines(filename);

		foreach(var line in all_lines) {
			// skip blank lines
			if(re_blankline.IsMatch(line)) {
				continue;
			}
			
			Console.WriteLine(">> " + line);
			// no backtrace here - if an unexpected error occurs, just rerun without -test to see
			// the backtrace
			var errmsg = safe_compile_and_run(intr, line, false, false);
			if(errmsg != "") {
				Console.WriteLine(errmsg);
				intr = make_interpreter(); // restart on error
			}
			else {
				Console.WriteLine("=> " + intr.reprStack());
			}
		}
	}

	public void run_file(Interpreter intr, string filename, bool singlestep) {
		// run file
		var text = System.IO.File.ReadAllText(filename);
		var errmsg = safe_compile_and_run(intr, text, singlestep, true);
		if(errmsg != "") {
			Console.WriteLine(errmsg);
		}
		else if(SHOW_RUNTIME_STATS)
			intr.printStats();
	}
}

public class MainProgram
{
    public static void Main(string[] args)
    {
		bool testMode = false;
		string filename = "";
		bool singlestep = false;
		var args_to_script = new LangList();
		for(int i=0; i<args.Length; ++i) {
			if(args[i] == "-test") {
				testMode = true;
			}
			else if(args[i] == "-step") {
				singlestep = true;
			}
			else if(args[i] == "-stats") {
				Repl.SHOW_RUNTIME_STATS = true;
			}
			else if(args[i] == "--") {
				// rest of args go to script
				++i;
				while(i < args.Length) {
					args_to_script.objlist.Add(new LangString(args[i++]));
				}
				break;
			}
			else if(filename == "" && System.IO.File.Exists(args[i])) {
				filename = args[i];
			}
			else {
				Console.WriteLine("Unknown argument: " + args[i]);
				return;
			}
		}
		Builtins.NATIVE_CMDLINE_ARGS = args_to_script;

		if(filename == "") {
			var r = new Repl();
			r.run_repl(singlestep);
		}
		else if(testMode) {
			var r = new Repl();
			r.run_test_mode(filename);
		}
		else {		
			var intr = Repl.make_interpreter();
			var r = new Repl();
			r.run_file(intr, filename, singlestep);
		}
    }
}