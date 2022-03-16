/*
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;
using System.Text.RegularExpressions;

public class Repl {
	static string INITLIB = "../lib/init.verb.b";
	static string COMPILERLIB = "../lib/compiler.verb.b";

	public static Interpreter make_interpreter() {
		var intr = new Interpreter();
		// load byte-compiled init.verb and compiler.verb to bootstrap interpreter
		var fileIn = System.IO.File.OpenText(INITLIB);
		Deserializer.deserialize_stream(intr, fileIn);
		// run __main__ in initlib to setup any globals
		var code = intr.WORDS["__main__"];
		intr.run(code);

		fileIn = System.IO.File.OpenText(COMPILERLIB);
		Deserializer.deserialize_stream(intr, fileIn);
		// do NOT run compiler __main__ since that is used for compiling from the cmdline

		return intr;
	}

	public void run_repl() {
		var intr = make_interpreter();
	
		while(true) {
			Console.Write(">> ");
			Console.Out.Flush();
			var line = Console.In.ReadLine();
			if(line == null || line == "quit") {
				return;
			}
			if(line != null) {
				// push string, then call byte-compile-string
				intr.push(new LangString(line));
				var code = intr.WORDS["byte-compile-string"];
				intr.run(code);

				//Console.WriteLine("AFTER COMPILATION ... STACK: " + intr.reprStack());

				// byte-compile-string leaves list of words on stack -- used by serializer -- but i
				// don't need them here
				intr.pop();

				// run __main__
				code = intr.WORDS["__main__"];

				intr.run(code);

				Console.Write("=> " + intr.reprStack() + "\n");
			}
		}
	} 

	// like a non-interactive repl, reads a line at a time, prints it,
	// runs it, then prints the stack. this is intented for unittesting.
	// maxline is maximum line that ran OK last time so i ca restart.
	public void run_test_mode(string filename, bool noinit, ref int maxrunline, ref bool done) {

		//cout << "Test mode starting ... " << endl;
		done = false;

		var re_blankline = new Regex(@"(^[ \t\r\n]*$)");

		var intr = make_interpreter();

		var all_lines = System.IO.File.ReadAllLines(filename);

		int runnable_lines = 0; // how many runnable lines have i seen (i.e. not counting blank lines)
		foreach(var line in all_lines) {
			// skip blank lines
			if(re_blankline.IsMatch(line)) {
				continue;
			}
			++runnable_lines;
			//cout << "On runnable line# " << runnable_lines << ", maxrunline " << maxrunline << endl;

			if(runnable_lines <= maxrunline) {
				//cout << "Skipping line ... " << endl;
				// counts as running, since if i fail i want to restart at the NEXT line
				maxrunline = Math.Max(maxrunline,runnable_lines);
				continue;
			}
			Console.WriteLine(">> " + line);
			//intr.syntax.clearAll(); // remove any leftover text from previous line run
			//intr.addText(line);
			//intr.run();
			// push string, then call byte-compile-string
			intr.push(new LangString(line));
			var code = intr.WORDS["byte-compile-string"];
			intr.run(code);

			//Console.WriteLine("AFTER COMPILATION ... STACK: " + intr.reprStack());

			// byte-compile-string leaves list of words on stack -- used by serializer -- but i
			// don't need them here
			intr.pop();

			// run __main__
			code = intr.WORDS["__main__"];
			intr.run(code);

			Console.WriteLine("=> " + intr.reprStack());
			// update maxline only after the above runs ok
			maxrunline = runnable_lines;
		}
		done = true; // done
	}

	public void run_file(Interpreter intr, string filename, bool singlestep) {
		// run file
		var text = System.IO.File.ReadAllText(filename);
		//intr.addText(text);
		//intr.run();
		// push string, then call byte-compile-string
		intr.push(new LangString(text));
		var code = intr.WORDS["byte-compile-string"];
		intr.run(code);

		//Console.WriteLine("AFTER COMPILATION ... STACK: " + intr.reprStack());
		
		// byte-compile-string leaves list of words on stack -- used by serializer -- but i
		// don't need them here
		intr.pop();

		// run __main__
		code = intr.WORDS["__main__"];

		intr.run(code);
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
			bool exited = false;
			while(!exited) {
				// restart repl on exceptions - continue until it exits OK.
				// at least in the C++ implementation, the interpreter state gets
				// confused after exceptions, so probably best to restart here as well
				try {
					var r = new Repl();
					r.run_repl();
					exited = true;
				}
				catch (LangError err) {
					Console.WriteLine("*** " + err.Message + " ***");
				}
			}
		}
		else if(testMode) {
			int maxrunline = 0;
			bool done = false;
			while(!done) {
				// as above, restart interpreter on exceptions. track max line 
				// that i ran before so i can restart on next line
				try {
					var r = new Repl();
					r.run_test_mode(filename, false, ref maxrunline, ref done);
				}
				catch (LangError err) {
					Console.WriteLine("*** " + err.Message + " ***");
					++maxrunline; // skip line that failed on next try
				}
			}
		}
		else {		
			var intr = Repl.make_interpreter();

			try {
				var r = new Repl();
				r.run_file(intr, filename, singlestep);
			}
			catch (LangError err) {
				Console.WriteLine("*** " + err.Message + " ***");
				// TODO print_backtrace(intr);
			}
		}
    }
}