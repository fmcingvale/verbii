
cmake_minimum_required(VERSION 3.10)

project(verbii)

include(CheckSymbolExists)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_CRT_SECURE_NO_WARNINGS")

# specify the C++ standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED True)


find_library(GC_LIBRARY
			NAMES gc gc.lib gc64_dll gc64_dll.lib
			PATHS ${PROJECT_SOURCE_DIR}/../libgc)

# on linux (ubuntu) it also needs -lgccpp
find_library(GCCPP_LIBRARY
			NAMES gccpp
			PATHS ${PROJECT_SOURCE_DIR}/../libgc)
			
find_path(GC_INCLUDES 
			gc.h
			PATHS ${PROJECT_SOURCE_DIR}/../libgc/include /usr/include/gc /usr/local/gc
          )

# weird ... C++ port doesn't need this ...
find_library(MATH_LIBRARY
			NAMES m libm
)

# look for strncmp (missing under mingw for some reason)
check_symbol_exists(strndup "string.h" HAVE_STRNDUP)

add_executable(main main.c langtypes.c interpreter.c deserialize.c errors.c native.c opcodes.c util.c gc_object.c xmalloc.c)

if(MATH_LIBRARY)
	target_link_libraries(main ${MATH_LIBRARY})
endif()

# decide which garbage collector to use	
if(USE_GC_OBJECT)
	# use custom garbage collector
	add_definitions(-DUSE_GC_OBJECT)
	message("** Using GC-OBJECT")
elseif(GC_LIBRARY AND GC_INCLUDES AND (NOT USE_NO_GC))
	include_directories(${GC_INCLUDES})
	add_definitions(-DUSE_BOEHM_GC)
	message("** Using BOEHM-GC")
	target_link_libraries(main ${GC_LIBRARY})	
	message("** GC_INCLUDES ${GC_INCLUDES}")
	message("** GC_LIBRARY ${GC_LIBRARY}")
else()
	message("** Building without garbage collection")
endif()

if(GCCPP_LIBRARY)
	target_link_libraries(main ${GCCPP_LIBRARY})	
endif()
	
# copy executable to root dir once built
add_custom_command(
  TARGET main
  POST_BUILD
  COMMAND ${CMAKE_COMMAND}
  ARGS -E copy $<TARGET_FILE:main> ${PROJECT_BINARY_DIR}/..
  )

#message("Hello CMake!")
#message("Platform: ${CMAKE_CXX_PLATFORM_ID}")
#message("Version: ${CMAKE_CXX_COMPILER_VERSION}")
  
