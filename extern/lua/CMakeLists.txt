
cmake_minimum_required(VERSION 3.10)

project(lua)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_CRT_SECURE_NO_WARNINGS")

add_library(LUA_LIB STATIC lapi.c lauxlib.c lbaselib.c lcode.c lcorolib.c lctype.c ldblib.c ldebug.c ldo.c ldump.c lfunc.c lgc.c
			linit.c liolib.c llex.c lmathlib.c lmem.c loadlib.c lobject.c lopcodes.c loslib.c lparser.c lstate.c lstring.c 
			lstrlib.c ltable.c ltablib.c ltm.c lundump.c lutf8lib.c lvm.c lzio.c)
			
add_executable(lua lua.c)
find_library(MATH_LIB 
			NAMES m libm libm.a)
			
target_link_libraries(lua LUA_LIB)

if(MATH_LIB)
	target_link_libraries(lua ${MATH_LIB})
endif()

# copy executable to root dir once built
add_custom_command(
  TARGET lua
  POST_BUILD
  COMMAND ${CMAKE_COMMAND}
  ARGS -E copy $<TARGET_FILE:lua> ${PROJECT_BINARY_DIR}/..
  )
