// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

// Importing libraries for the program
#include <stdio.h> // Standard Input/Output library used for basic input/output operations such as scanf, printf, etc.
#include <stdlib.h> // Standard Library used for dynamic memory allocation, process control, conversions, and others
#include <memory.h> // Memory Library used for memory operations such as copying, setting, comparing, etc.
// #include <unistd.h> // POSIX Operating System API library used for system calls, file operations, etc.
#include <fcntl.h> // File Control library used for file operations such as open, close, etc.
#define int long long 

// Pointers for source code and data sections
char *p, *lp, // *p represnets current position in source code and *lp represents last position in source code
     *data;   // data/bss pointer used to manage data and BSS sections

     
int *e, *le,  // Pointers to the current and last positions in the emitted code.
    *id,      // currently parsed identifier used for symbol table lookup
    *sym,     // symbol table (simple list of identifiers) used for global/local/static variables and functions
    tk,       // current token type used for parsing and tokenizing
    ival,     // current token value used for parsing and tokenizing
    ty,       // current expression type used for parsing and type checking
    loc,      // local variable offset used for local variables
    line,     // current line number used for error messages
    src,      // print source and assembly flag used for debugging
    debug;    // print executed instructions flag used for debugging

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

// next function which is a lexical analyzer for the C language
// it Identifies keywords, identifiers, numbers, operators, and special characters.
void next()
{
//creating a pointer to the current position in the code
char *pp;
// the while loop will run until the end of the code is reached 
while (tk = *p) {
    ++p;
    // if we encounter a new line character we go to the next line
    if (tk == '\n') { 
        ++line;
        //This one checks if the token is a # which is used for preprocessor directives (#define, #include)
    } else if (tk == '#') { 
        //this one skips the preprocessor directives
        while (*p != 0 && *p != '\n') ++p;
        //this checks if the token is from the alphabet or an underscore
    } else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
        // Found a word (identifier or keyword)
        pp = p - 1;
        // This loop runs until the end of the identifier is reached
        while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
            tk = tk * 147 + *p++; // Hashing the identifier
        tk = (tk << 6) + (p - pp); // Unique value for identifier
        id = sym;
        // This loop runs until the end of the symbol table 
        while (id[Tk]) {
            if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
            id = id + Idsz;
        }
        id[Name] = (int)pp;
        id[Hash] = tk;
        tk = id[Tk] = Id;
        return;
    }
    // if the token is a number we convert it to an integer by checking if it's a decimal, hex, or octal number
    // and then we store the value of the number in the ival variable and set the token to Num
    //Num is a token that represents a number
    else if (tk >= '0' && tk <= '9') {
        // Handling numbers (integers, hex, octal)
        if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; } // Decimal numbers
        else if (*p == 'x' || *p == 'X') { // Hex numbers
            while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
                ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
        } else { // Octal numbers
            while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0';
        }
        tk = Num;
        return;
    }

    // this block of code is used to check if the token is a division operator
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        //you go to the end of the line or identifier
        while (*p != 0 && *p != '\n') ++p;
      }
      // use the division operator
      else {
        tk = Div;
        return; // exit the function
      }
    }
    // this block of code is used to check if the token is a single or double quote
    else if (tk == '\'' || tk == '"') {
      pp = data;
      // this loop runs until the end of the string is reached
      while (*p != 0 && *p != tk) {
        // this block of code is used to check if the token is an escape character 
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';
        }
        // this block of code is used to check if the token is a double quote
        if (tk == '"') *data++ = ival;
      }
      // go to the next character in the code
      ++p;
      // if it's a double quote set the ival to the address of the data 
      // else set the token to be a number
      if (tk == '"') ival = (int)pp; else tk = Num;
      return;
    }
    // this block of code is used to check if the token is an assignment operator
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    // this block of code is used to check if the token is an addition operator
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    // this block of code is used to check if the token is a subtraction operator
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    // this block of code is used to check if the token is a not equal operator
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    // this block of code is used to check if the token is a less than operator  
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    // this block of code is used to check if the token is a greater than operator
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    // this checks if the token is an OR operator
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    // this checks if the token is an AND operator
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    // this checks if the token is an XOR operator
    else if (tk == '^') { tk = Xor; return; }
    // this checks if the token is a modulo operator
    else if (tk == '%') { tk = Mod; return; }
    // this checks if the token is a multiplication operator
    else if (tk == '*') { tk = Mul; return; }
    // this checks if the token is a bracket
    else if (tk == '[') { tk = Brak; return; }
    // this checks if the token is a conditional operator
    else if (tk == '?') { tk = Cond; return; }
    // this checks if the token is a special character
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

// this function is used to parse the expressions 
// the variable lev is used to determine the level of the expression 
// the level of expression is used to determine the precedence of the operators and the order of evaluation
void expr(int lev)
{
  int t, *d;

  // this block of code checks if the token is null which also prints a message
  // if the token is null it means that the end of the file is reached and exits the program
  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  //if the token is a number it will store the value of the number in the ival variable
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  //if the token is a double quote it will store the value of the string in the data pointer
  else if (tk == '"') {
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
  }
  //if the token is a sizeof operator it will store the size of the type in the emitted code
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
    ty = INT;
  }
  //if the token is an identifier it will check if it is a function or a variable
  else if (tk == Id) {
    d = id; next();
    if (tk == '(') {
      next();
      t = 0;
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
      next();
      if (d[Class] == Sys) *++e = d[Val];
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }
      ty = d[Type];
    }
    //if the token is a number it will store the value of the number in the ival variable
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  //if the token is an open parenthesis it will check if it is a cast or an expression
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char) {
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      // bad cast means that the cast is not valid because the token is not a close parenthesis
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      //this block of code checks if the token is a close parenthesis and prints a message if it is not
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  //if the token is a multiplication operator it will evaluate the expression
  else if (tk == Mul) {
    next(); expr(Inc);
    //this block of code checks if the type is greater than a pointer and the type is same to ty
    // and prints a message if it is not
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  //if the token is an AND operator it will evaluate the expression
  else if (tk == And) {
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  //if the token is a NOT (!) operator it will evaluate the expression
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  //if the token is a NOT (~) operator it will evaluate the expression
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  //if the token is an addition operator it will evaluate the expression
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  //if the token is a subtraction operator it will evaluate the expression
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  //if the token is an increment or decrement operator it will evaluate the expression
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    // the if statement checks if the current token is a left parenthesis
    if (*e == LC) { *e = PSH; *++e = LC; }
    // this if statement checks if the current token is an identifier
    else if (*e == LI) { *e = PSH; *++e = LI; }
    // this else statement prints a message if the current token is not an identifier
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  //print a message if the token is not an expression
  else { printf("%d: bad expression\n", line); exit(-1); }

  // this block of code is used to check if the token is a bracket
  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    // this block of code checks if the token is an assignment operator
    // if the token is an assignment operator it will evaluate the expression
    if (tk == Assign) {
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    // this block of code checks if the token is a conditional operator
    // if the token is a ternary/conditional operator it will evaluate the expression
    else if (tk == Cond) {
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    // this block of code checks if the token is a logical OR operator and evaluates the expression
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    // this block of code checks if the token is a logical AND operator and evaluates the expression
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    // this block of code checks if the token is an OR operator and evaluates the expression
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    // this block of code checks if the token is an XOR operator and evaluates the expression
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    // this block of code checks if the token is an AND operator and evaluates the expression
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    // this block of code checks if the token is an equal operator and evaluates the expression
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    // this block of code checks if the token is a not equal operator and evaluates the expression
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    // this block of code checks if the token is a less than operator and evaluates the expression
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    // this block of code checks if the token is a greater than operator and evaluates the expression
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    // this block of code checks if the token is a less than or equal operator and evaluates the expression
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    // this block of code checks if the token is a greater than or equal operator and evaluates the expression
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    // this block of code checks if the token is a shift left operator and evaluates the expression
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    // this block of code checks if the token is a shift right operator and evaluates the expression
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    // this block of code checks if the token is an addition operator and evaluates the expression
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    // this block of code check if the token is a subtraction
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    // this block of code checks if the token is a multiplication operator and evaluates the expression
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    // this block of code checks if the token is a division operator and evaluates the expression
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    // this block of code checks if the token is a modulo operator and evaluates the expression
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }

    //this code checks post-increment and post-decrement operators
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    //this block handle array indexing
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

// Parses and processes statements such as if-else, loops, return statements, 
// compound blocks, and standalone expressions.

void stmt()
{
  int *a, *b;

  if (tk == If) {
    next();
    // this block of code checks if the token is an open parenthesis
    // if the token is an open parenthesis it will assign the next token to the token variable
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    //if the token is a close parenthesis it will assign the next token to the token variable
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    //this block of code checks if the token is an else token
    if (tk == Else) {
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (int)(e + 1);
  }
  //this block of code checks if the token is a while token
  else if (tk == While) {
    next();
    a = e + 1;
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    *++e = JMP; *++e = (int)a;
    *b = (int)(e + 1);
  }
  //this block of code checks if the token is a return token
  else if (tk == Return) {
    next();
    if (tk != ';') expr(Assign);
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  //this block of code checks if the token is a open brace
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();
    next();
  }
  //this block of code checks if the token is a semicolon
  else if (tk == ';') {
    next();
  }
  //this block of code checks if the token is a open brace
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

// this is the main function of the compiler
int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  //argc is the number of arguments passed to the program
  //argv is an array of pointers to the strings which are those arguments
  --argc; ++argv;
  // Check if there are any command-line arguments and handle options
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  // If no filename is provided, print usage instructions and exit
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  // Try to open the input file, if unsuccessful, print an error and exit
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  // Allocate memory for the symbol table, the emitted code, the data area, and the stack
  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  //put zeroes for the symbol table, the emitted code, the data area, and the stack
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // read the keywords and add them to the symbol table
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main

  // read the source file and add it to the source area
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // parse declarations
  line = 1;
  next();
  //while loop that loops until the end of the file is reached
  while (tk) {
    bt = INT; // basetype
    //this block of code checks if the token is an integer and assigns the next token to the token variable
    if (tk == Int) next();
    //this block of code checks if the token is a character and assigns the next token to the token variable
    else if (tk == Char) { next(); bt = CHAR; }
    //this block of code checks if the token is an enum
    else if (tk == Enum) {
      next();
      //this block of code checks if the token is an open brace which means that the enum has started
      if (tk != '{') next();
      // this is for double checking if the token is an open brace
      if (tk == '{') {
        next();
        i = 0;
        //this while loop runs until the end of the enum is reached
        while (tk != '}') {
          //this block of code checks if the token is an identifier and prints a message if it is not
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          //this block of code checks if the token is an assignment operator 
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          //this block of code checks if the token is a comma
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    
    //This section checks if its globally declared variables or functions
    //It helps us in determining if the identifier is a function, or a global variable
    while (tk != ';' && tk != '}') {
      //setting the type of the identifier to the base type
      ty = bt;
      //check if it's a pointer and set to pointer if so
      while (tk == Mul) { next(); ty = ty + PTR; }
      // If it's not an identifier, it's an invalid declaration
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      // Check if the identifier is already declared
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      //go to next token
      next();
      id[Type] = ty;
      if (tk == '(') { // this means its the delcaration of a function
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        //go to next token
        next(); 
        // Counter for function parameters
        i = 0;
        // Parse function parameters
        while (tk != ')') {
          ty = INT;
          //if token is an int go next
          if (tk == Int) next();
          //handling characters and setting the type to char if the token is a character
          else if (tk == Char) { next(); ty = CHAR; }
          //handling the pointer and setting the type to pointer if the token is a pointer
          while (tk == Mul) { next(); ty = ty + PTR; }
          //this one checks for bad parameter declaration as it expects an identifier
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          //this block of code checks if the token is a duplicate parameter definition
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          //store parameter information 
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        // Handle local variable declarations inside the function

        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            // Handle pointer types for local variables
            while (tk == Mul) { next(); ty = ty + PTR; }
            //this one checks for bad parameter declaration as it expects an identifier
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            // Check if the identifier is already declared
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            //store local variable information
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        // Generate function prologue
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt();
        // Generate function epilogue
        *++e = LEV;
        id = sym; // unwind symbol table locals
        // Restore symbol table (unwind local variables)
        while (id[Tk]) {
          if (id[Class] == Loc) {
            
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      // If it's not a function, then it's a global variable declaration

      else {
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      // If there are multiple declarations separated by commas, continue

      if (tk == ',') next();
    }
    next();
  }

   // Check if the 'main' function is defined
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0;

  // setup stack 
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // run...
 // Virtual Machine Execution Loop:
// - Fetch an instruction (i) from the instruction pointer (pc).
// - Decode and execute it.
// - Adjust the stack, registers, or memory as required.
// - If an EXIT instruction is encountered, print the cycle count and terminate.

  cycle = 0; 
  while (1) {
    i = *pc++; ++cycle;
    // If debug mode is enabled, print executed instruction
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    //Memory management operations 
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    //Function stack operations
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    //Memory loading and storing opeartions
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH) *--sp = a;                                         // push

    //Bitwise and arithmetic operations
    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    //file related operations
    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == FREE) free((void *)*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}