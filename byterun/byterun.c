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

typedef enum calls {
  LREAD, LWRITE, LLENGTH, LSTRING, BARRAY
} calls;

typedef enum designation {
  GLOBAL, LOCAL, ARG, ACCESS
} designation;

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

/* Stack machine iterative interpreter */
void interpreter (bytefile *bf) {
  int *stack = malloc(10 * 1024 * 1024);
  int *sp = stack;
  char *ip = bf->code_ptr;

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
      // TODO
      return;
      
    case BINOP:
      int y = POP;
      int x = POP;
      switch (l) {
      case  PLUS: PUSH(x +  y); break;
      case MINUS: PUSH(x -  y); break;
      case  MULT: PUSH(x *  y); break;
      case   DIV: PUSH(x /  y); break;
      case   MOD: PUSH(x %  y); break;
      case    LT: PUSH(x <  y); break;
      case    LE: PUSH(x <= y); break;
      case    GT: PUSH(x >  y); break;
      case    GE: PUSH(x >= y); break;
      case    EQ: PUSH(x == y); break;
      case   NEQ: PUSH(x != y); break;
      case   AND: PUSH(x && y); break;
      case    OR: PUSH(x || y); break;
      default: FAILURE; 
      }
      break;
      
    case GROUP1:
      switch (l) {
      case CONST:
        PUSH(INT);
        break;
        
      case STRING:
        printf ("STRING\t%s", GETSTRING);
        break;
          
      case SEXP:
        printf ("SEXP\t%s ", GETSTRING);
        printf ("%d", INT);
        break;
        
      case  STI:
        printf ("STI");
        break;
        
      case  STA:
        printf ("STA");
        break;
        
      case  JMP:
        printf ("JMP\t0x%.8x", INT);
        break;
        
      case  END:
        printf ("END");
        break;
        
      case  RET:
        printf ("RET");
        break;
        
      case DROP:
        POP;
        break;
        
      case  DUP:
        printf ("DUP");
        break;
        
      case SWAP:
        printf ("SWAP");
        break;

      case ELEM:
        printf ("ELEM");
        break;
        
      default:
        FAILURE;
      }
      break;
      
    case LD:
      switch (l) {
      case GLOBAL: PUSH(UNBOX(*(bf->global_ptr + INT))); break;
      case  LOCAL: printf ("L(%d)", INT); break;
      case    ARG: printf ("A(%d)", INT); break;
      case ACCESS: printf ("C(%d)", INT); break;
      default: FAILURE;
      }
      break;
    case LDA:
    case ST:
      switch (l) {
      case GLOBAL: *(bf->global_ptr + INT) = BOX(PEEK); break;
      case  LOCAL: printf ("L(%d)", INT); break;
      case    ARG: printf ("A(%d)", INT); break;
      case ACCESS: printf ("C(%d)", INT); break;
      default: FAILURE;
      }
      break;
      
    case GROUP2:
      switch (l) {
      case CJMPz:
        printf ("CJMPz\t0x%.8x", INT);
        break;
        
      case CJMPnz:
        printf ("CJMPnz\t0x%.8x", INT);
        break;
        
      case BEGIN:
        INT; INT;
        break;
        
      case CBEGIN:
        printf ("CBEGIN\t%d ", INT);
        printf ("%d", INT);
        break;
        
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
          
      case CALLC:
        printf ("CALLC\t%d", INT);
        break;
        
      case CALL:
        printf ("CALL\t0x%.8x ", INT);
        printf ("%d", INT);
        break;
        
      case TAG:
        printf ("TAG\t%s ", GETSTRING);
        printf ("%d", INT);
        break;
        
      case ARRAY:
        printf ("ARRAY\t%d", INT);
        break;
        
      case FAIL:
        printf ("FAIL\t%d", INT);
        printf ("%d", INT);
        break;
        
      case LINE:
        INT;
        break;

      default:
        FAILURE;
      }
      break;
      
    case PATTERN:
      // printf ("PATT\t%s", pats[l]);
      break;

    case RUNTIME: {
      switch (l) {
      case LREAD:
        PUSH(UNBOX(Lread()));
        break;
        
      case LWRITE:
        PUSH(UNBOX(Lwrite(BOX(POP))));
        break;

      case LLENGTH:
        PUSH(UNBOX(Llength(POP)));
        break;

      case LSTRING:
        PUSH(UNBOX(Lstring(POP)));
        break;

      case BARRAY:
        printf ("CALL\tBarray\t%d", INT);
        break;

      default:
        FAILURE;
      }
    }
    break;
      
    default:
      FAILURE;
    }
  }
  while (1);
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
    interpreter (f);
  }
  return 0;
}
