/*
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;
using System.Text.RegularExpressions;

public class Repl {
	static string INITFILE = "../lib/init.txt";

	public static Interpreter make_interpreter(bool noinit) {
		var intr = new Interpreter();
		if(!noinit) {
			var text = System.IO.File.ReadAllText(INITFILE);
			intr.addText(text);
			intr.run();
			// don't want initlib in the backtrace history, once it has successfully loaded
			//intr.reader.clearAll();
		}
		return intr;
	}

	public void run_repl(bool noinit) {
		var intr = make_interpreter(noinit);
	
		while(true) {
			Console.Write(">> ");
			Console.Out.Flush();
			var line = Console.In.ReadLine();
			if(line == null || line == "quit") {
				return;
			}
			if(line != null) {
				intr.addText(line);
				intr.run();
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

		var intr = make_interpreter(noinit);

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
			intr.syntax.clearAll(); // remove any leftover text from previous line run
			intr.addText(line);
			intr.run();
			Console.WriteLine("=> " + intr.reprStack());
			// update maxline only after the above runs ok
			maxrunline = runnable_lines;
		}
		done = true; // done
	}

	public void run_file(Interpreter intr, string filename, bool singlestep) {
		// run file
		var text = System.IO.File.ReadAllText(filename);
		intr.addText(text);
		intr.run();
	}

}

public class MainProgram
{
    public static void Main(string[] args)
    {
		bool testMode = false;
		string filename = "";
		bool noinit = false;
		bool singlestep = false;
		foreach(var arg in args) {
			if(arg == "-test") {
				testMode = true;
			}
			else if(arg == "-noinit") {
				noinit = true;
			}
			else if(arg == "-step") {
				singlestep = true;
			}
			else if(filename == "" && System.IO.File.Exists(arg)) {
				filename = arg;
			}
			else {
				Console.WriteLine("Unknown argument: " + arg);
				return;
			}
		}
		
		if(filename == "") {
			bool exited = false;
			while(!exited) {
				// restart repl on exceptions - continue until it exits OK.
				// at least in the C++ implementation, the interpreter state gets
				// confused after exceptions, so probably best to restart here as well
				try {
					var r = new Repl();
					r.run_repl(noinit);
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
					r.run_test_mode(filename, noinit, ref maxrunline, ref done);
				}
				catch (LangError err) {
					Console.WriteLine("*** " + err.Message + " ***");
					++maxrunline; // skip line that failed on next try
				}
			}
		}
		else {		
			var intr = Repl.make_interpreter(noinit);

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