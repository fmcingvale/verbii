/*
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;

public class Repl {
	public void run_repl(bool noinit) {
		var intr = new Interpreter();
	
		while(true) {
			Console.Write(">> ");
			Console.Out.Flush();
			var line = Console.In.ReadLine();
			if(line == "quit") {
				return;
			}
			if(line != null) {
				intr.addText(line);
				intr.run();
				Console.Write("=> " + intr.reprStack() + "\n");
			}
		}
	} 
}

public class MainProgram
{
    public static void Main(string[] args)
    {
        var r = new Repl();
		r.run_repl(false);
    }
}