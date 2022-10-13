#include "compiler.h"

static void print_help() {
	printf("usage: ayin <FILE>\n");
	printf("\t-c\t\tCompile only\n");
	printf("\t-debug\t\tOutput debug information\n");
	printf("\t-emit-llvm\tEmit llvm ir\n");
	printf("\t-help\t\tPrint this help\n");
	printf("\t-l\t\t\tLink with library\n");
	printf("\t-L\t\t\tAdd linker path\n");
	printf("\t-o\t\tSpecify output file name\n");
	printf("\t-release\tOptimized release build\n");
}

/*
* parses command line input and fills in the CompileOptions
* then runs the compiler with those options
*/
int main(int argc, char **argv) {
	CompileOptions options;
	options.input_file = to_string("");
	options.output_file = to_string("main");

	argc--;
	argv++;

	if (argc <= 0) {
        options.input_file = to_string("/Users/niko/Desktop/dev/ayin/examples/example.ay");
        options.libraries.add(to_string("glfw"));
        options.linker_paths.add(to_string("/opt/homebrew/Cellar/glfw/3.3.7/lib"));
        
        //print_help();
		//return 0;
	}

	while (argc--) {
		char *arg = *argv++;

		if (strcmp(arg, "-help") == 0) {
			print_help();
			return 0;
		} else if (strcmp(arg, "-c") == 0) {
			options.compile_only = true;
		} else if (strcmp(arg, "-release") == 0) {
			options.optimize = true;
		} else if (strcmp(arg, "-debug") == 0) {
			options.debug = true;
		} else if (strcmp(arg, "-emit-llvm") == 0) {
			options.emit_llvm = true;
		} else if (strcmp(arg, "-l") == 0) {
			if (argc <= 0) {
				printf("Missing library name after '-l'\n");
				return 1;
			}
			
			argc--;
			options.libraries.add(to_string(*argv++));
		} else if (strcmp(arg, "-L") == 0) {
			if (argc <= 0) {
				printf("Missing linker path after '-o'\n");
				return 1;
			}
			
			argc--;
			options.linker_paths.add(to_string(*argv++));
		} else if (strcmp(arg, "-o") == 0) {
			if (argc <= 0) {
				printf("Missing argument after '-o'\n");
				return 1;
			}
			
			argc--;
			options.output_file = to_string(*argv++);
		} else {
			options.input_file = to_string(arg);
		}
	}

	if (options.input_file == to_string("")) {
		printf("No input file specified!\n");
		return 1;
	}

	if (options.debug && options.optimize) {
		printf("Cannot specify -debug and -release at the same time!\n");
		return 1;
	}

	Compiler compiler(&options);
	compiler.run();

	return 0;
}
