#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long


char *p, *lp, // current position in source code -> p is the current position, and lp is the starting position of the current line (line pointer).
     *data;   // data/bss pointer


int *e, *le,  // current position in emitted (Intermediate) code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers) -> Stores the identifiers and their properties (name, type, scope, memory location, ...).
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag -> Enables/Disables the printing of the source code and assembly code.
    debug;    // print executed instructions


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


void next()   // This function reads the next token from the source code and handles different types of input.
{
  char *pp;   // Local variable used as a temporary store (More on it in later comments).


  while (tk = *p) { // tk is the current token, and *p is a pointer to the current position. So this loop reads the next character from the source code.
    ++p;  // Move to the next character in the source code.
    if (tk == '\n') {   // If the character is a new line, do the following:
      if (src) {   // If the source flag is set (Allows for debugging or logging), do the following:
        printf("%d: %.*s", line, p - lp, lp);   // Print the current line number and the current line contents.
        lp = p; // Set the starting position of the line (line pointer) to be the current position.
        while (le < e) {  // Ensures iterating over all the emitted code/instructions that have been generated.
          // The following code represents the mnemonics for the emitted code. These are stored in a SINGLE STRING, 4 CHARACTERS LONG (Including spaces for shorter ones), separated by commas.
          // This prints the mnemonic of the current instruction. The mnemonic is determined by the value of the current instruction.
          // The value of the current instruction (opcode) is gained by dereferencing the pointer (*++le); then it is multiplied by 5 to get the index of the mnemonic in the string.
          // It is multiplied by 5 because each mnemonic is 4 char long, 1 more char for the comma that separates it from the next mnemonic.
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");  // The first 8 mnemonics (LEA until ADJ) require arguments.
          // If the mnemonic is one of the first 8, print the mnemonic followed by the argument. Otherwise, just print the mnemonic.
        }
      }
      ++line;   // Increment the line number.
    }
    else if (tk == '#') {   // If the character is a hash (preprocessor directive [tell the compiler to preprocess the source code before compiling]), do the following:
      while (*p != 0 && *p != '\n') ++p;  // While the character is not the end of the file (null character) and not a new line, skip the rest of the line.
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {   // Otherwise if the current token is a letter from a-z (regardless of case) or an underscore, do the following:
      pp = p - 1; // pp here is used to store the starting position of the identifier (sequence of letters, digits, and underscores).
      // The previous line is necessary in order to keep track of the length of the identifier mainly, along with some other extra uses.
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')   // While the current token is a letter, digit, or underscore, (probably symbolizes the end of the identifier), do the following:
        // The following line is used to compute the HASH value of each character in the identifier.
        // This is necessary to be able to compare identifiers quickly without having to compare each character.
        // The hash value is computed by multiplying the current hash value by 147 and adding the ASCII value of the current character (Then incrementing to the next character pointer (p)).
        // The value 147 is arbitrary, but it is a prime number, which is good for hashing.
        tk = tk * 147 + *p++;
      tk = (tk << 6) + (p - pp);  // Finalize the hash value by shifting it 6 bits to the left (Equivalent to multiplying by 2^6) and adding the length of the identifier.
      // The previous step is helpful to further spread the hash values and make them more unique to avoid collisions as much as possible.
      // The variable (((tk))) now holds the hash value of the identifier.
      id = sym;   // Set the symbol table pointer (id) to the start of the symbol table.
      while (id[Tk]) {  // While there are symbols in the table:
        // The following line is used to check if an identifier already exists in the symbol table.
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        // Note: memcmp is a function that compares two memory blocks (first and second arguments, with the length defined in the third argument) and returns 0 if they are equal.
        // The statement comprises two parts:
        // 1. Check if the hash value of the current identifier (which is held in tk) is equal to the one stored in the symbol table.
        // 2. Compares the pointers of the current identifier (pp) and the one stored in the symbol table (id[Name]) for the same length (p - pp).
        // memcmp would return 0 if both are equal, and !memcmp would make it 1; making the statement true.
        // If both conditions are true, the token is set to the token of the identifier in the symbol table, and the function returns.
       
        id = id + Idsz;   // Idsz is the size of the identifier, so we increment the current id pointer by adding its size to it. (Moving to the next symbol)
      }
      // The following lines are executed if the identifier is not found in the symbol table.
      id[Name] = (int)pp;   // Set the name of the identifier to the starting position of the identifier.
      id[Hash] = tk;    // Set the hash value of the identifier.
      tk = id[Tk] = Id;  // Set the token to Id (Identifier).
      return;
    }
    else if (tk >= '0' && tk <= '9') {   // Otherwise, if the current token is a digit:
      // The following statement is used to parse numeric literals from the source code
      // The condition checks if the current token is a digit, and converts it from a character representation to an equivalent integer value
      // So if we have the input as '3' (Character), this will make it 3 (Integer)
      // Then the loop iterates as long as the pointer is on a digit from 0 to 9
      // (At first, ival holds the value of the initial (FIRST) digit. The loop will multiply the current ival by 10 and adding the current integer value to it.
      // This would cause the previous digits to shift to the left, and adds the new digits to the right
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      else if (*p == 'x' || *p == 'X') {  // Otherwise if it is facing a hexadecimal value (Would be in the form 0x.....)
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))   // While there still exists some more hexadecimal digits (0-9 or A-F): (Parse the hexadecimal value)
          // The following calculation is used to convert a hexadecimal value to its corresponding integer value
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
          // The value of ival is initially multiplied by 16 (Each new digit in a hexadecimal number represents a new place value (16^0, 16^1, 16^2, etc.))
          // (tk & 15) performs bitwise AND operation. It returns the lower 4 bits of the value
          // This is because the lower 4 bits of the ASCII values of '0'-'9', 'a'-'f', and 'A'-'F' corresponds to their hex values
          // The final part (tk >= 'A' ? 9 : 0) will add 9 to the value if the current token is a letter ('A' to 'F')
          // So, all the values from the three parts are added together to get the final integer value
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }   // Otherwise if it is in octal form; multiply ival by 8 and add the current digit to it.
      tk = Num; // Set the token to Num (Number).
      return;
    }
    else if (tk == '/') {   // Handle when a forward slash (/) is encountered (Either comment or division)
      if (*p == '/') {    // If the next character is also a forward slash:
        ++p;
        while (*p != 0 && *p != '\n') ++p;    // Ignore the text that comes in the comment as long as it is not a null character or a new line (which indicates the end of the comment)
      }
      else {
        tk = Div;   // Otherwise if it was only one forward slash, then this would indicate that we are trying to perform division, so set the token to Division (Div)
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {     // Otherwise if the character was a (Single or Double) quote:
      // When the code encounters a string literal, it needs to store the entire sequence of characters in memory and keep track of its starting address.
      pp = data;    // Store the starting position of the string (data) in the temporary store (pp)
      while (*p != 0 && *p != tk) {   // While the current character is not a null character and is not the same character as the one opening the quote (' or "") (which indicates the end of the string):
        if ((ival = *p++) == '\\') {    // Check if the current character is a backslash (\) [It says '\\' because we read it as an escape sequence]
          if ((ival = *p++) == 'n') ival = '\n';  // If the next character after the \ was 'n' (indicating starting a new line), then set the current character to the new line representation '\n'
        }
        if (tk == '"') *data++ = ival;   // If it was a double quote, set ival to the starting position of the string (Address of the string literal)
        // We only do the previous step for double quotes since single quotes denote character literals.
      }
      ++p;  // Move past the closing quote
      if (tk == '"') ival = (int)pp;    // If it was a double quote, set ival
      // The Num token type is used to indicate that the current token is a numeric literal or a character literal.
      else tk = Num;    // If it was a single quote, setting tk to Num indicates that the character literal has been processed.
      return;
    }
    // The follwing statements handle the rest of the operators and special characters
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}


void expr(int lev)
{
  int t, *d;
  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }  // Error if no token
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }    // Handle numeric literals (save as an immediate value)
  else if (tk == '"') {   // Handle string literals
    *++e = IMM; *++e = ival; next();  
    while (tk == '"') next();
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;    // Align data and set type to pointer
  }
  else if (tk == Sizeof) {    // Handle Sizeof operator
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }    // If the next token is not an open parenthesis, then there is an error
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }    // If the next token is an integer or a character, then set the type to the corresponding type
    while (tk == Mul) { next(); ty = ty + PTR; }    // While the token is * which represents a pointer, increment the type by PTR (constant representing a pointer type)
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);   // Emit size
    ty = INT;   // Set the type to integer
  }
  else if (tk == Id) {    // Handle identifiers (Variable names, function names, ...)
    d = id; next();
    if (tk == '(') {    // If it is a function call
      next();   // Move to the first char of the argument
      t = 0;
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }   // While the function call is not closed, push the function argument onto the stack. The variable t keeps track of the number of arguments
      next();
      if (d[Class] == Sys) *++e = d[Val];   // Handle a system function
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }    // Handle a user-defined function
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }  // Adjust the stack for the arguments
      ty = d[Type];   // Set type to function return type
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }  // Handle numeric constants
    // The arguments in the previous line does the following:
    // 1. Emit the IMM (immediate) opcode and the value of the identifier (d[Val]) to the emitted (executable) code
    // 2. Set the type of the expression to INT
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; } // Handle local variables
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }  // Handle global variables
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;  // Emit the opcode LC (Load Character) if the current type is character, otherwise LI (Load Integer)
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char) {  // Handle type casting
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }  // Handle pointer types
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);  // Parse the expression that follows the cast
      ty = t;   // Set the type to the cast type
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {   // Handle dereference operator
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {   // Handle address-of operator
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;  // Increment the type by PTR (pointer type)
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }   // Logical NOT
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; } // Bitwise NOT
  else if (tk == Add) { next(); expr(Inc); ty = INT; }  // Unary plus
  else if (tk == Sub) {   // Unary minus
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {    // Handle increment and decrement operators (PRE)
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }  // Push the value of the variable onto the stack
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);   // Emit size
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;  // Store the result into memory (SC for character, SI for integer)
  }
  else { printf("%d: bad expression\n", line); exit(-1); }


  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method (Parsing expressions of operators with different precedence levels)
    t = ty;
    if (tk == Assign) {   // If the token is an assignment operator
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;    // Store the value of the expression into memory
    }
    else if (tk == Cond) {    // Handle conditional operator
      next();
      *++e = BZ; d = ++e;   // Emit Branch if Zero opcode. And store the address of the branch instruction in d
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;   // Emit Jump opcode
      expr(Cond);
      *d = (int)(e + 1);  // Move to the next instruction after the Jump instruction
      // In the previous lines, +3 and +1 are used for the following reasons:
      // +3 is used to skip the BZ and the address of the branch instruction (d) and the JMP instruction
      // +1 is used to skip the JMP instruction (Move to the next instruction)
    }
    // Hnadle all other operators
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    else if (tk == Inc || tk == Dec) {  // Handle increment and decrement operators (POST)
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
    else if (tk == Brak) {  // Handle array indexing (Square brackets)
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }   // Handle pointer arithmetic
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

void stmt() {
  int *a, *b; // Temporary pointers for handling control flow
  // Handle 'if' statements
  if (tk == If) {
      next(); // Move to the next token after "if"
      if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); } // Expect '(' after 'if'
      expr(Assign); // Parse the condition expression inside the parentheses
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); } // Expect ')' after the condition
      *++e = BZ; b = ++e; // Emit a Branch if Zero instruction where 'b' points to the address to be filled later
      stmt();  // Parse and execute the 'if' statement
      if (tk == Else) { // Check for optional 'else' clause
          *b = (int)(e + 3); *++e = JMP; b = ++e; // Jump to end of 'else' block
          next(); // Move to the next token after 'else'
          stmt(); // Parse the statement block for the 'else' clause
      }
      *b = (int)(e + 1); // Fill the jump address for the 'if' or 'else' block
  }
  // Handle 'while' statements
  else if (tk == While) {
      next(); // Move to the next token
      a = e + 1;  // Save the starting address for the loop
      if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); } // Expect '(' after 'while'
      expr(Assign); // Parse the condition expression inside the parentheses
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); } // Expect ')' after the condition
      *++e = BZ; b = ++e;  // Emit a Branch if Zero instruction
      stmt();  // Parse the statement block for the 'while' loop
      *++e = JMP; *++e = (int)a; // Emit a Jump instruction to loop back to the start
      *b = (int)(e + 1);  // Fill the jump address for exiting the loop
  }
  // Handle 'return' statements
  else if (tk == Return) {
      next(); // Move to the next token after return
      if (tk != ';') expr(Assign); // Parse the return value if present
      *++e = LEV; // Emit a Leave instruction to return from the function
      if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); } // Expect ';' after 'return'
  }
  // Handle block statements (enclosed in '{' and '}')
  else if (tk == '{') {
      next();  // Move to the next token after '{'
      while (tk != '}') stmt(); // Parse all statements inside the block until '}' is encountered
      next(); // Move to the next token after '}'
  }
  // Handle empty statements (just a semicolon)
  else if (tk == ';') {
      next();  // Move to the next token after ';'
  }
  // Handle expression statements (e.g., assignments, function calls)
  else {
      expr(Assign); // Parse the expression
      if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }  // Expect ';' after the expression
  }
}


int main(int argc, char **argv) {
  int fd, bt, ty, poolsz, *idmain; // File descriptor, base type, type, memory pool size,
  // and pointer to main function
  int *pc, *sp, *bp, a, cycle; // vm registers such as program counter, stack pointer, base pointer, accumulator, and cycle counter
  int i, *t; // temporary variables for loops and pointers
  // Parse command-line arguments
  --argc; ++argv; // Skip the program name
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; } // Enable source printing if -s flag is present
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; } // Enable debug mode if -d flag is present
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; } // Print usage if no input file is provided
  // Open the input file
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; } // Open the file or exit on failure
  // Allocate memory pools for the compiler
  poolsz = 256*1024; // arbitrary size for memory pools
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; } // Allocate memory for the symbol table
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; } // Allocate memory for the code/text area
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }  // Allocate memory for the data area
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; } // Allocate memory for the stack
  // Initialize memory pools to zero
  memset(sym,  0, poolsz); // Clear the symbol table
  memset(e,    0, poolsz); // Clear the code/text area
  memset(data, 0, poolsz);  // Clear the data area
  // Add keywords and library functions to the symbol table
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main"; // Keywords and library functions
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main function
  // Allocate memory for the source code and read the input file
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }  // Allocate memory for the source code
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }  // Read source code from the file into allocated memory.. handle read failure
  p[i] = 0; // Null-terminate the source code
  close(fd); // Close the file
  // Parse declarations in the source code
  line = 1; // Initialize line counter
  next();  // Start parsing tokens
  while (tk) {  // Continue parsing until no more tokens are left
      bt = INT; //  Default base type is int
      if (tk == Int) next(); // Handle 'int' type
      else if (tk == Char) { next(); bt = CHAR; } // Handle 'char' type
      else if (tk == Enum) { // Handle 'enum' type
          next();
          if (tk != '{') next();  // Skip if no opening brace
          if (tk == '{') {        // Handle enum block
              next();  // Move to the next token
              i = 0;            // Initialize enum value counter
              while (tk != '}') { // Parse enum members until closing brace
                  if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; } // Ensure valid identifier
                  next(); // Move to the next token
                  if (tk == Assign) { // Handle explicit enum value assignment
                      next();// Move to the next token
                      if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; } // Ensure valid number
                      i = ival;  // Set enum value
                      next();// Move to the next token
                  }
                  id[Class] = Num; id[Type] = INT; id[Val] = i++; // Add enum member to symbol table
                  if (tk == ',') next(); // Handle comma separator
              }
              next(); // Move past the closing brace
          }
      }
      // Loop to parse global declarations or function definitions
      while (tk != ';' && tk != '}') {
          ty = bt; // Set the current type to the base type (INT or CHAR)
          // Handle pointer types (e.g., int*, char**)
          while (tk == Mul) { next(); ty = ty + PTR; }
          // Ensure the current token is an identifier (variable or function name)
          if (tk != Id) { printf("%d: bad global declaration\n", line); return -1;}
          // Check if the identifier is already defined (duplicate declaration)
          if (id[Class]) {
              printf("%d: duplicate global definition\n", line); // Print error message
              return -1; // Exit with error
          }
          next(); // Move to the next token after the identifier
          id[Type] = ty; // Set the type of the identifier in the symbol table
          // Handle function definitions
          if (tk == '(') { // function
              id[Class] = Fun; // Mark the identifier as a function in the symbol table
              id[Val] = (int)(e + 1); // Set the function's value to the current code position (e + 1)
              next();  // Move to the next token
              i = 0;// Initialize parameter counter
              // Parse function parameters
              while (tk != ')') {
                  ty = INT; // Default parameter type is INT
                  if (tk == Int) next(); // Handle 'int' type
                  else if (tk == Char) { next(); ty = CHAR; } // Handle 'char' type
                  // Handle pointer types for parameters (e.g., int*, char**)
                  while (tk == Mul) { next(); ty = ty + PTR; }
                  // Ensure the parameter is a valid identifier
                  if (tk != Id) {
                      printf("%d: bad parameter declaration\n", line); // Print error message
                      return -1; // Exit with error
                  }
                  if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
                  // Save the current class, type, and value of the identifier and update the identifier as a local variable
                  id[HClass] = id[Class]; id[Class] = Loc;
                  id[HType]  = id[Type];  id[Type] = ty;
                  id[HVal]   = id[Val];   id[Val] = i++; // Assign a unique index to the parameter
                  next(); // Move to the next token
                  if (tk == ',') next();   // Handle comma-separated parameters
              }
              next(); // Move past the closing parenthesis
              // Ensure the function body starts with '{'
              if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
              loc = ++i; // Set the local variable offset (starting after parameters)
              next(); // Move to the next token
              // Parse local variable declarations inside the function
              while (tk == Int || tk == Char) {
                  bt = (tk == Int) ? INT : CHAR; // Set the base type for local variables
                  next(); // Move to the next token
                  while (tk != ';') {    // Parse multiple local variables separated by commas
                      ty = bt; // Set the current type to the base type
                      while (tk == Mul) { next(); ty = ty + PTR; }
                      // Ensure the local variable is a valid identifier
                      if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
                      if (id[Class] == Loc) {  printf("%d: duplicate local definition\n", line); return -1; }
                      // Save the current class, type, and value of the identifier and update the identifier as a local variable
                      id[HClass] = id[Class]; id[Class] = Loc;
                      id[HType]  = id[Type];  id[Type] = ty;
                      id[HVal]   = id[Val];   id[Val] = ++i; // Assign a unique index to the local variable
                      next();  // Move to the next token
                      if (tk == ',') next();         // Handle comma-separated local variables
                  }
                  next();  // Move past the semicolon
              }
              *++e = ENT; // Emit the Enter instruction to set up the functions stack frame
              *++e = i - loc;  // Emit the size of the local variable area
              while (tk != '}') stmt(); // Parse the function body (statements inside '{' and '}')
              *++e = LEV;  // Emit Leave instruction to clean up the stack frame and return
              id = sym; // unwind symbol table locals .. reset the symbol table pointer
              while (id[Tk]) {
                  if (id[Class] == Loc) {
                  id[Class] = id[HClass]; // Restore the original class
                  id[Type] = id[HType];   // Restore the original type
                  id[Val] = id[HVal];     // Restore the original value
              }
              id = id + Idsz; // Move to the next symbol table entry
          }
        }
          // Handle global variable declarations
          else {
              id[Class] = Glo; // Mark the identifier as a global variable
              id[Val] = (int)data; // Set the variable's address in the data area
              data = data + sizeof(int); // Allocate space for the variable in the data area
          }
          // Handle comma-separated global declarations
          if (tk == ',') next(); // If a comma is found, move to the next token for additional declarations
      }
      next(); // Move to the next token after the declaration block
  }
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; } // Check for the existence of the main function; if not defined, print error message and return -1
  if (src) return 0;  // If the source code is not null, return 0 indicating successful execution
  // Set up the stack for the virtual machine
  bp = sp = (int *)((int)sp + poolsz); // Initialize base pointer "bp" and stack pointer "sp" to the top of the stack
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp; // Push PSH instruction and save the stack pointer
  *--sp = argc;  // Push argc.. number of command-line arguments
  *--sp = (int)argv; // Push argv .. command-line arguments
  *--sp = (int)t; // Push the saved stack pointer onto the stack for later use
  // run...
  cycle = 0; // Initialize the cycle counter to keep track of the number of executed instructions
  while (1) {
      i = *pc++; ++cycle;  // Fetch the next instruction and increment the program counter (pc) then increment the cycle counter
      // Debugging: Print the current instruction and its details
      if (debug) {
          printf("%d> %.4s", cycle, // Print cycle number and instruction mnemonic
              &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
               "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
               "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);  // Look up instruction name
          if (i <= ADJ) printf(" %d\n", *pc); else printf("\n"); // Print operand if applicable
      }
      // Execute the fetched instruction
      if (i == LEA) a = (int)(bp + *pc++); // Load local address: a = bp + offset
      else if (i == IMM) a = *pc++; // Load immediate value: a = value
      else if (i == JMP) pc = (int *)*pc; // Jump to the address specified by the next instruction
      else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; } // Jump to subroutine, saving the return address on the stack
      else if (i == BZ) pc = a ? pc + 1 : (int *)*pc; // Branch if zero: if (a == 0) pc = address
      else if (i == BNZ) pc = a ? (int *)*pc : pc + 1; // Branch if not zero: if (a != 0) pc = address
      else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; } // Enter a subroutine, saving the base pointer and adjusting the stack
      else if (i == ADJ) sp = sp + *pc++; // Adjust stack: sp = sp + value
      else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // Leave the subroutine, restoring the base pointer and program counter
      else if (i == LI) a = *(int *)a; // Load integer: a = *a
      else if (i == LC) a = *(char *)a; // Load character: a = *a
      else if (i == SI) *(int *)*sp++ = a; // Store integer: *sp = a
      else if (i == SC) a = *(char *)*sp++ = a; // Store character: *sp = a
      else if (i == PSH) *--sp = a; // Push value onto stack: *--sp = a
      // Arithmetic and logical operations
      else if (i == OR)  a = *sp++ |  a; // Bitwise OR: a = *sp | a
      else if (i == XOR) a = *sp++ ^  a; // Bitwise XOR: a = *sp ^ a
      else if (i == AND) a = *sp++ &  a; // Bitwise AND: a = *sp & a
      else if (i == EQ)  a = *sp++ == a; // Equal: a = (*sp == a)
      else if (i == NE)  a = *sp++ != a; // Not equal: a = (*sp != a)
      else if (i == LT)  a = *sp++ <  a; // Less than: a = (*sp < a)
      else if (i == GT)  a = *sp++ >  a; // Greater than: a = (*sp > a)
      else if (i == LE)  a = *sp++ <= a; // Less than or equal: a = (*sp <= a)
      else if (i == GE)  a = *sp++ >= a; // Greater than or equal: a = (*sp >= a)
      else if (i == SHL) a = *sp++ << a; // Shift left: a = *sp << a
      else if (i == SHR) a = *sp++ >> a; // Shift right: a = *sp >> a
      else if (i == ADD) a = *sp++ +  a; // Add: a = *sp + a
      else if (i == SUB) a = *sp++ -  a; // Subtract: a = *sp - a
      else if (i == MUL) a = *sp++ *  a; // Multiply: a = *sp * a
      else if (i == DIV) a = *sp++ /  a; // Divide: a = *sp / a
      else if (i == MOD) a = *sp++ %  a; // Modulo: a = *sp % a
      // System calls and library functions
      else if (i == OPEN) a = open((char *)sp[1], *sp); // Open a file and store the result in 'a'
      else if (i == READ) a = read(sp[2], (char *)sp[1], *sp); // Read from a file and store the result in 'a'
      else if (i == CLOS) a = close(*sp);  // Close a file and store the result in 'a'
      else if (i == PRTF) { // Print formatted string
          t = sp + pc[1]; // Calculate argument pointer
          a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); // Call printf
      }
      else if (i == MALC) a = (int)malloc(*sp); // Allocate memory: a = malloc(size) and store the address in 'a'
      else if (i == FREE) free((void *)*sp); // Free allocated memory: free(ptr)
      else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp); // Set memory to a specified value
      else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp); // Compare memory
      else if (i == EXIT) { // Exit program
          printf("exit(%d) cycle = %d\n", *sp, cycle); // Print exit status and cycle count
          return *sp; // Return exit status
      }
      else { // Handle unknown instructions
          printf("unknown instruction = %d! cycle = %d\n", i, cycle); // Print error message
          return -1; // Exit with error
      }
  }
}
