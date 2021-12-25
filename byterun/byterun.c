/* Lama SM Bytecode interpreter */

# include <string.h>
# include <stdio.h>
# include <errno.h>
# include <malloc.h>
# include "../runtime/runtime.h"

void *__start_custom_data;
void *__stop_custom_data;

/* The unpacked representation of bytecode file */
typedef struct {
  char *string_ptr;              /* A pointer to the beginning of the string table */
  int  *public_ptr;              /* A pointer to the beginning of publics table    */
  char *code_ptr;                /* A pointer to the bytecode itself               */
  int  *global_ptr;              /* A pointer to the global area                   */
  int   stringtab_size;          /* The size (in bytes) of the string table        */
  int   global_area_size;        /* The size (in words) of global area             */
  int   public_symbols_number;   /* The number of public symbols                   */
  char  buffer[0];               
} bytefile;

/* Gets a string from a string table by an index */
char* get_string (bytefile *f, int pos) {
  return &f->string_ptr[pos];
}

/* Gets a name for a public symbol */
char* get_public_name (bytefile *f, int i) {
  return get_string (f, f->public_ptr[i*2]);
}

/* Gets an offset for a publie symbol */
int get_public_offset (bytefile *f, int i) {
  return f->public_ptr[i*2+1];
}

/* Reads a binary bytecode file by name and unpacks it */
bytefile* read_file (char *fname) {
  FILE *f = fopen (fname, "rb");
  long size;
  bytefile *file;

  if (f == 0) {
    failure ("%s\n", strerror (errno));
  }
  
  if (fseek (f, 0, SEEK_END) == -1) {
    failure ("%s\n", strerror (errno));
  }

  file = (bytefile*) malloc (sizeof(int)*4 + (size = ftell (f)));

  if (file == 0) {
    failure ("*** FAILURE: unable to allocate memory.\n");
  }
  
  rewind (f);

  if (size != fread (&file->stringtab_size, 1, size, f)) {
    failure ("%s\n", strerror (errno));
  }
  
  fclose (f);
  
  file->string_ptr  = &file->buffer [file->public_symbols_number * 2 * sizeof(int)];
  file->public_ptr  = (int*) file->buffer;
  file->code_ptr    = &file->string_ptr [file->stringtab_size];
  file->global_ptr  = (int*) malloc (file->global_area_size * sizeof (int));
  
  return file;
}

/* Bytecode aliases */
typedef enum opcode {
  BINOP, GROUP1, LD, LDA, ST, GROUP2, PATTERN, RUNTIME, STOP = 15
} opcode;

typedef enum binop {
  PLUS = 1, MINUS, MULT, DIV, MOD, LT, LE, GT, GE, EQ, NEQ, AND, OR
} binop;

typedef enum group1 {
  CONST, STRING, SEXP, STI, STA, JMP, END, RET, DROP, DUP, SWAP, ELEM
} group1;

typedef enum group2 {
  CJMPz, CJMPnz, BEGIN, CBEGIN, CLOSURE, CALLC, CALL, TAG, ARRAY, FAIL, LINE
} group2;

typedef enum runtime {
  LREAD, LWRITE, LLENGTH, LSTRING, BARRAY
} runtime;

typedef enum designation {
  GLOBAL, LOCAL, ARG, ACCESS
} designation;

typedef enum pattern {
  str, string, array, sexp, ref, val, fun
} pattern;

/* Disassembles the bytecode pool */
void disassemble (FILE *f, bytefile *bf) {
  
# define INT       (ip += sizeof (int), *(int*)(ip - sizeof (int)))
# define BYTE      *ip++
# define GETSTRING get_string (bf, INT)
# define FAILURE   failure ("ERROR: invalid opcode %d-%d\n", h, l)
  
  char *ip     = bf->code_ptr;
  char *ops [] = {"+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "!!"};
  char *pats[] = {"=str", "#string", "#array", "#sexp", "#ref", "#val", "#fun"};
  char *lds [] = {"LD", "LDA", "ST"};
  do {
    char x = BYTE,
         h = (x & 0xF0) >> 4,
         l = x & 0x0F;

    fprintf (f, "0x%.8x:\t", ip-bf->code_ptr-1);
    
    switch (h) {
    case STOP:
      goto stop;
      
    /* BINOP */
    case BINOP:
      fprintf (f, "BINOP\t%s", ops[l-1]);
      break;
      
    case GROUP1:
      switch (l) {
      case CONST:
        fprintf (f, "CONST\t%d", INT);
        break;
        
      case STRING:
        fprintf (f, "STRING\t%s", GETSTRING);
        break;
          
      case SEXP:
        fprintf (f, "SEXP\t%s ", GETSTRING);
        fprintf (f, "%d", INT);
        break;
        
      case  STI:
        fprintf (f, "STI");
        break;
        
      case  STA:
        fprintf (f, "STA");
        break;
        
      case  JMP:
        fprintf (f, "JMP\t0x%.8x", INT);
        break;
        
      case  END:
        fprintf (f, "END");
        break;
        
      case  RET:
        fprintf (f, "RET");
        break;
        
      case DROP:
        fprintf (f, "DROP");
        break;
        
      case  DUP:
        fprintf (f, "DUP");
        break;
        
      case SWAP:
        fprintf (f, "SWAP");
        break;

      case ELEM:
        fprintf (f, "ELEM");
        break;
        
      default:
        FAILURE;
      }
      break;
      
    case LD:
    case LDA:
    case ST:
      fprintf (f, "%s\t", lds[h-2]);
      switch (l) {
      case GLOBAL: fprintf (f, "G(%d)", INT); break;
      case  LOCAL: fprintf (f, "L(%d)", INT); break;
      case    ARG: fprintf (f, "A(%d)", INT); break;
      case ACCESS: fprintf (f, "C(%d)", INT); break;
      default: FAILURE;
      }
      break;
      
    case GROUP2:
      switch (l) {
      case CJMPz:
        fprintf (f, "CJMPz\t0x%.8x", INT);
        break;
        
      case CJMPnz:
        fprintf (f, "CJMPnz\t0x%.8x", INT);
        break;
        
      case BEGIN:
        fprintf (f, "BEGIN\t%d ", INT);
        fprintf (f, "%d", INT);
        break;
        
      case CBEGIN:
        fprintf (f, "CBEGIN\t%d ", INT);
        fprintf (f, "%d", INT);
        break;
        
      case CLOSURE:
        fprintf (f, "CLOSURE\t0x%.8x", INT);
        {int n = INT;
         for (int i = 0; i<n; i++) {
         switch (BYTE) {
           case GLOBAL: fprintf (f, "G(%d)", INT); break;
           case  LOCAL: fprintf (f, "L(%d)", INT); break;
           case    ARG: fprintf (f, "A(%d)", INT); break;
           case ACCESS: fprintf (f, "C(%d)", INT); break;
           default: FAILURE;
         }
         }
        };
        break;
          
      case CALLC:
        fprintf (f, "CALLC\t%d", INT);
        break;
        
      case CALL:
        fprintf (f, "CALL\t0x%.8x ", INT);
        fprintf (f, "%d", INT);
        break;
        
      case TAG:
        fprintf (f, "TAG\t%s ", GETSTRING);
        fprintf (f, "%d", INT);
        break;
        
      case ARRAY:
        fprintf (f, "ARRAY\t%d", INT);
        break;
        
      case FAIL:
        fprintf (f, "FAIL\t%d", INT);
        fprintf (f, "%d", INT);
        break;
        
      case LINE:
        fprintf (f, "LINE\t%d", INT);
        break;

      default:
        FAILURE;
      }
      break;
      
    case PATTERN:
      fprintf (f, "PATT\t%s", pats[l]);
      break;

    case RUNTIME: {
      switch (l) {
      case LREAD:
        fprintf (f, "CALL\tLread");
        break;
        
      case LWRITE:
        fprintf (f, "CALL\tLwrite");
        break;

      case LLENGTH:
        fprintf (f, "CALL\tLlength");
        break;

      case LSTRING:
        fprintf (f, "CALL\tLstring");
        break;

      case BARRAY:
        fprintf (f, "CALL\tBarray\t%d", INT);
        break;

      default:
        FAILURE;
      }
    }
    break;
      
    default:
      FAILURE;
    }

    fprintf (f, "\n");
  }
  while (1);
 stop: fprintf (f, "<end>\n");
}

/* Dumps the contents of the file */
void dump_file (FILE *f, bytefile *bf) {
  int i;
  
  fprintf (f, "String table size       : %d\n", bf->stringtab_size);
  fprintf (f, "Global area size        : %d\n", bf->global_area_size);
  fprintf (f, "Number of public symbols: %d\n", bf->public_symbols_number);
  fprintf (f, "Public symbols          :\n");

  for (i=0; i < bf->public_symbols_number; i++) 
    fprintf (f, "   0x%.8x: %s\n", get_public_offset (bf, i), get_public_name (bf, i));

  fprintf (f, "Code:\n");
  disassemble (f, bf);
}

/* Activation frame */
typedef struct {
  struct frame *previousFP;
  int *previousIP;
  int *args;
  int *locals;
  int *access;
} frame;

/* Stack machine iterative interpreter */
void interpreter (char *fname, bytefile *bf) {
  int *stack = malloc(10 * 1024 * 1024);
  int *sp = stack;
  frame *fp = sp;
  char *ip = bf->code_ptr;
  __gc_init();

# define POP         *(--sp)
# define PEEK        *(sp - 1)
# define PUSH(x)     *(sp++) = x

# define UNBOX(x)    (((int) (x)) >> 1)
# define BOX(x)      ((((int) (x)) << 1) | 0x0001)

  do {
    char x = BYTE,
         h = (x & 0xF0) >> 4,
         l = x & 0x0F;

    switch (h) {
    case STOP:
      goto stop;
      
    /* binary operator */
    case BINOP:
    {
      int y = UNBOX(POP);
      int x = UNBOX(POP);
      switch (l) {
      case  PLUS: PUSH(BOX(x +  y)); break;
      case MINUS: PUSH(BOX(x -  y)); break;
      case  MULT: PUSH(BOX(x *  y)); break;
      case   DIV: PUSH(BOX(x /  y)); break;
      case   MOD: PUSH(BOX(x %  y)); break;
      case    LT: PUSH(BOX(x <  y)); break;
      case    LE: PUSH(BOX(x <= y)); break;
      case    GT: PUSH(BOX(x >  y)); break;
      case    GE: PUSH(BOX(x >= y)); break;
      case    EQ: PUSH(BOX(x == y)); break;
      case   NEQ: PUSH(BOX(x != y)); break;
      case   AND: PUSH(BOX(x && y)); break;
      case    OR: PUSH(BOX(x || y)); break;
      default: FAILURE; 
      }
      break;
    }
      
    case GROUP1:
      switch (l) {
      /* put a constant on the stack */
      case CONST:
        PUSH(BOX(INT));
        break;

      /* put a string on the stack */  
      case STRING:
        PUSH(Bstring(GETSTRING));
        break;
          
      /* create an S-expression */    
      case SEXP:
      {
        int h = LtagHash(GETSTRING);
        int n = INT;
        int *array = malloc(n * sizeof(int));
        for (int i = n - 1; i >= 0; --i) 
          array[i] = POP;
        PUSH(Bsexp2(BOX(n), array, h));
        break;
      }
        
      /* store a value into a reference */
      case  STI:
      {
        int x = POP;
        int v = POP;
        PUSH(x);
        bf->global_ptr[v] = x;
        break;
      }
        
      /* store a value into array/sexp/string */
      case  STA:
      {
        int v = POP;
        int i = POP;
        int x = POP;
        PUSH(Bsta(v, i, x));
        break;
      }
        
      /* unconditional jump */
      case  JMP:
        ip = bf->code_ptr + INT;
        break;
        
      /* end procedure definition */
      case  END:
      /* returns from a function */
      case  RET:
        if (fp->args == stack) goto stop;
        int result = POP;
        sp = fp->args;
        ip = fp->previousIP;
        fp = fp->previousFP;
        PUSH(result);
        break;
        
      /* drops the top element off */
      case DROP:
        POP;
        break;
        
      /* duplicates the top element */
      case  DUP:
      {
        int x = POP;
        PUSH(x);
        PUSH(x);
        break;
      }
        
      /* swaps two top elements */
      case SWAP:
      {
        int y = POP;
        int x = POP;
        PUSH(y);
        PUSH(x);
        break;
      }

      /* takes an element of array/string/sexp */
      case ELEM:
      {
        int i = POP;
        int p = POP;
        PUSH(Belem(p, i));
        break;
      }
        
      default: FAILURE;
      }
      break;
      
    /* load a variable to the stack */
    case LD:
      switch (l) {
      case GLOBAL: PUSH(bf->global_ptr[INT]); break;
      case  LOCAL: PUSH(fp->locals[INT]); break;
      case    ARG: PUSH(fp->args[INT]); break;
      case ACCESS: printf ("C(%d)", INT); break;
      default: FAILURE;
      }
      break;

    /* load a variable address to the stack */
    case LDA:
    {
      int *address;
      switch (l) {
      case GLOBAL: address = bf->global_ptr + INT; break;
      case  LOCAL: address = fp->locals + INT; break;
      case    ARG: address = fp->args + INT; break;
      case ACCESS: printf ("C(%d)", INT); break;
      default: FAILURE;
      }
      PUSH(address); PUSH(address);
      break;
    }

    /* store a value into a variable */
    case ST:
      switch (l) {
      case GLOBAL: bf->global_ptr[INT] = PEEK; break;
      case  LOCAL: fp->locals[INT] = PEEK; break;
      case    ARG: fp->args[INT] = PEEK; break;
      case ACCESS: printf ("C(%d)", INT); break;
      default: FAILURE;
      }
      break;
      
    case GROUP2:
      switch (l) {
      /* conditional jump */
      case CJMPz:
      {
        int l = INT;
        if (!UNBOX(POP)) ip = bf->code_ptr + l;
        break;
      }
        
      /* conditional jump */
      case CJMPnz:
      {
        int l = INT; 
        if (UNBOX(POP)) ip = bf->code_ptr + l;
        break;
      }
        
      /* begins procedure definition */
      case BEGIN:
      {
        int args = INT;
        int locals = INT;
        int operands = args + 1;
        sp += operands;
        int *previousIP = POP;
        frame *newFP = sp;
        newFP->previousFP = fp;
        newFP->previousIP = previousIP;
        newFP->args = sp - args;
        sp += sizeof(frame);
        newFP->locals = sp;
        sp += locals;
        fp = newFP;
        break;
      }
        
      /* begins procedure definition with closure */
      case CBEGIN:
        printf ("CBEGIN\t%d ", INT);
        printf ("%d", INT);
        break;
        
      /* create a closure */
      case CLOSURE:
        printf ("CLOSURE\t0x%.8x", INT);
        {int n = INT;
         for (int i = 0; i<n; i++) {
         switch (BYTE) {
           case GLOBAL: printf ("G(%d)", INT); break;
           case  LOCAL: printf ("L(%d)", INT); break;
           case    ARG: printf ("A(%d)", INT); break;
           case ACCESS: printf ("C(%d)", INT); break;
           default: FAILURE;
         }
         }
        };
        break;
          
      /* calls a closure */
      case CALLC:
        printf ("CALLC\t%d", INT);
        break;
        
      /* calls a function/procedure */
      case CALL:
      {
        int l = INT;
        int args = INT;
        int operands = args + 1;
        PUSH(ip);
        sp -= operands;
        ip = bf->code_ptr + l;
        break;
      }
        
      /* checks the tag and arity of S-expression */
      case TAG:
      {
        int d = POP;
        int h = LtagHash(GETSTRING);
        int n = INT;
        PUSH(Btag(d, h, BOX(n)));
        break;
      }
        
      /* checks the tag and size of array */
      case ARRAY:
      {
        int d = POP;
        int n = INT;
        PUSH(Barray_patt(d, BOX(n)));
        break;
      }
        
      /* match failure location, leave a value */
      case FAIL:
      {
        int v = POP;
        int line = INT;
        int col = INT;
        Bmatch_failure(v, fname, BOX(line), BOX(col));
        break;
      }
        
      /* line info */
      case LINE:
        INT;
        break;

      default: FAILURE;
      }
      break;
      
    /* checks various patterns */
    case PATTERN:
      switch (l) {
      case    str: PUSH(Bstring_patt(POP, POP)); break;
      case string: PUSH(Bstring_tag_patt(POP)); break;
      case  array: PUSH(Barray_tag_patt(POP)); break;
      case   sexp: PUSH(Bsexp_tag_patt(POP)); break;
      case    ref: PUSH(Bboxed_patt(POP)); break;
      case    val: PUSH(Bunboxed_patt(POP)); break;
      case    fun: PUSH(Bclosure_tag_patt(POP)); break;
      default: FAILURE;
      }
      // printf ("PATT\t%s", pats[l]);
      break;

    case RUNTIME: {
      switch (l) {
      case   LREAD: PUSH(Lread()); break;
      case  LWRITE: PUSH(Lwrite(POP)); break;
      case LLENGTH: PUSH(Llength(POP)); break;
      case LSTRING: PUSH(Lstring(POP)); break;
      case  BARRAY:
      {
        int n = INT;
        int *array = malloc(n * sizeof(int));
        for (int i = n - 1; i >= 0; --i) 
          array[i] = POP;
        PUSH(Barray2(BOX(n), array));
        break;
      }

      default: FAILURE;
      }
    }
    break;
      
    default: FAILURE;
    }
  }
  while (1);
 stop: free(stack);
}

/* 
  Disassemble or interpret Lama SM bytecode 
  Usage: byterun <input file> <mode>
  Modes:
    -d --- disassemble (default)
    -i --- interpret
*/
int main (int argc, char* argv[]) {
  char* fname = argv[1];
  char* mode = argc > 2 ? argv[2] : "-d";
  bytefile *f = read_file (fname);
  if (!strcmp(mode, "-d")) {
    dump_file (stdout, f);
  } else if (!strcmp(mode, "-i")) {
    interpreter (fname, f);
  }
  return 0;
}
