/*
	test_reader.cs - very basic check of Reader.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
using System;
using System.Collections.Generic;

public class HelloWorld
{
    public static void Main(string[] args)
    {
        Console.WriteLine ("Hello Reader");
		var reader = new Reader();
		reader.addText("   one  two \t  three ; : 123\nfour   ");
		while(true) {
			var word = reader.nextWord();
			if(word == "") {
				break;
			}
			Console.WriteLine(word);
		}
		Console.WriteLine("Reverse ...");
		while(true) {
			var word = reader.prevWord();
			if(word == "") {
				break;
			}
			Console.WriteLine(word);
		}
		Console.WriteLine("Push ...");
		Console.WriteLine(reader.nextWord());
		Console.WriteLine(reader.nextWord());
		
		List<string> more = new List<string>(){"111", "222", "333"};
		reader.pushWords(more);
		Console.WriteLine(reader.nextWord());
		Console.WriteLine(reader.nextWord());
		Console.WriteLine(reader.nextWord());
		Console.WriteLine(reader.nextWord());
		reader.popWords();
		Console.WriteLine(reader.nextWord());
		Console.WriteLine(reader.nextWord());
		Console.WriteLine("Peek ...");
		Console.WriteLine(reader.peekWord());
		reader.deletePrevWord();
		reader.insertPrevWord("AAA");
		reader.insertPrevWord("BBB");
		Console.WriteLine(reader.nextWord());
		Console.WriteLine(reader.nextWord());
		
		Console.WriteLine("Reverse ...");
		while(true) {
			var word = reader.prevWord();
			if(word == "") {
				break;
			}
			Console.WriteLine(word);
		}
		
    }
}