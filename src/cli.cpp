#include <string>

#include "compiler.h"


static String convert_string(std::string str) {
	String string;

	string.data = strdup(str.c_str());
	string.length = str.length();

	return string;
}

static void print_help() {
	printf("usage: ayin <FILE>\n");
	printf("\t-c\t\tCompile only\n");
	printf("\t-debug\t\tOutput debug information\n");
	printf("\t-emit-llvm\tEmit llvm ir\n");
	printf("\t-help\t\tPrint this help\n");
	printf("\t-o\t\tSpecify output file name\n");
	printf("\t-release\tOptimized release build\n");
}

int main(int argc, char **argv) {
	/* TODO: replace with own CLI parsing so we don't use std::string */
	

	CompileOptions options;
	options.input_file = to_string("");
	options.output_file = to_string("main");

	argc--;
	argv++;

	if (argc <= 0) {
		print_help();
		return 0;
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

	Compiler compiler(options);
	compiler.run();

	return 0;
}
