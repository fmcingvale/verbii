/*
	Exceptions

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	This is based on the C++ version.
*/
public class LangError : System.Exception {
	public LangError(string message) : base(message) { }
};
