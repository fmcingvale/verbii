
* Get the Lua sources, from e.g.:
	https://www.lua.org/ftp/lua-5.4.4.tar.gz
	OR
	https://www.lua.org/ftp/lua-5.3.6.tar.gz
	
	(Versions 5.2.x and older cannot run verbii)
	
* Unpack to e.g. extern/lua/lua-5.4.4

* MOVE extern/lua/CMakeLists.txt to extern/lua/lua-5.4.4/src

* MOVE remainder of extern/lua/ files to extern/lua/lua-5.4.4

* Move extern/lua/lua-5.4.4/CMakeLists.txt to 

* Windows:
	* From Start menu, open Visual Studio 2022 -> Developer Command Prompt for VS 2022
	
	* cd into the lua-5.4.4 directory
	
	$ vc-setup.bat
	$ vc-build.bat
	
	* Copy lua.exe to somewhere in your PATH
	
* Linux:
	
	* cd into the lua-5.4.4 directory
	
	$ ./setup-linux.sh
	$ ./make.sh
	
	* Copy lua to somewhere in your PATH
	
* Mingw:
	
	* cd into the lua-5.4.4 directory
	
	$ ./setup-mingw.sh
	$ ./make.sh
	
	* Copy lua to somewhere in your PATH
