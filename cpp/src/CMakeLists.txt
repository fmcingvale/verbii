
cmake_minimum_required(VERSION 3.10)

project(verbii)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_CRT_SECURE_NO_WARNINGS")

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

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

include_directories(${GC_INCLUDES})
	
add_executable(main main.cpp deserialize.cpp interpreter.cpp langtypes.cpp native.cpp opcodes.cpp util.cpp xmalloc.cpp)


# can build without gc if needed
if(GC_LIBRARY AND GC_INCLUDES)
	add_definitions(-DUSE_GCMALLOC)
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
  
