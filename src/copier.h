#ifndef COPIER_H_
#define COPIER_H_

#include "ast.h"
#include "common.h"

struct Compiler;

struct Copier {
    Compiler *compiler;

    Copier(Compiler *compiler);
    
    Ast *copy(Ast *ast);
};

#endif