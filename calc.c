// Gulnur Yasemin UYGUN 231ADB101
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
//
// -----------------------------------------------------------------------------
// WHAT THIS PROGRAM DOES (brief):
// - Reads arithmetic expressions from .txt files, evaluates, and writes either
//   the numeric result or `ERROR:<pos>` (1-based char index; '\n' counts as 1).
// - Operators: +, -, *, /, ** (right-assoc), parentheses ( ), unary +/-, floats.
// - Pythonic line comments: if the first non-space on a line is '#', that line
//   is ignored.
// - CLI:
//   calc [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt
//   • If -d is given, processes all *.txt files in DIR (non-recursive).
//   • If -o omitted, output dir becomes: <input_base>_<username>_<STUDENT_ID>/
//   • For each input task1.txt -> task1_<Name>_<Lastname>_<StudentID>.txt
// - Division by zero: we report ERROR at the '/' token position (documented).
// - Single source file; uses only standard C/POSIX headers (no bison/flex).
// -----------------------------------------------------------------------------

#define STUDENT_NAME     "Gulnur Yasemin"
#define STUDENT_LASTNAME "UYGUN"
#define STUDENT_ID       "231ADB101"

#include <stdio.h> // For input/output
#include <stdlib.h> // For memory allocation, exit()
#include <string.h> // For string functions
#include <ctype.h> // For isdigit(), isspace()
#include <errno.h> // For error handling
#include <math.h>       // For pow(), fabs()
#include <dirent.h>     // For directory handling
#include <sys/stat.h>   // For file status
#include <sys/types.h>  // For system types

// ============================ Value (int/double) =============================
// This section defines a structure to hold numeric values (either int or double)
// and a set of helper functions for arithmetic operations.

typedef struct {
    int is_float;      // 0 means integer (long long), 1 means floating-point (double)
    long long i;       // Stores the integer value when is_float == 0
    double d;          // Stores the floating value when is_float == 1
} Value;

// Creates an integer Value from a long long
static Value make_int(long long x){ Value v={0,x,(double)x}; return v; }

// Creates a floating Value from a double
static Value make_double(double x){ Value v={1,(long long)x,x}; return v; }

// Checks if the given value is zero (handles both int and double)
static int is_zero(Value v){ return v.is_float ? fabs(v.d)==0.0 : (v.i==0); }

// Adds two Value objects, performing type promotion if needed
static Value v_add(Value a, Value b){
    if(a.is_float || b.is_float)
        return make_double((a.is_float?a.d:a.i) + (b.is_float?b.d:b.i));
    return make_int(a.i + b.i);
}

// Subtracts b from a, with automatic type conversion if one is float
static Value v_sub(Value a, Value b){
    if(a.is_float || b.is_float)
        return make_double((a.is_float?a.d:a.i) - (b.is_float?b.d:b.i));
    return make_int(a.i - b.i);
}

// Multiplies two Value objects, promoting to double if necessary
static Value v_mul(Value a, Value b){
    if(a.is_float || b.is_float)
        return make_double((a.is_float?a.d:a.i) * (b.is_float?b.d:b.i));
    return make_int(a.i * b.i);
}

// Divides a by b. If b is zero, sets an error position (err_pos)
static Value v_div(Value a, Value b, size_t *err_pos, size_t slash_pos){
    if(is_zero(b)){                      // Check for division by zero
        if(*err_pos==0) *err_pos = slash_pos;  // Store error position only once
        return make_int(0);              // Return dummy value (not used)
    }
    return make_double((a.is_float?a.d:(double)a.i)/(b.is_float?b.d:(double)b.i));
}

// Exponentiation (a ** b), always returns a double
static Value v_pow(Value base, Value exp){
    double bd = base.is_float ? base.d : (double)base.i; // Convert base to double
    double ed = exp.is_float ? exp.d : (double)exp.i;   // Convert exponent to double
    return make_double(pow(bd, ed));                    // Use math.h pow()
}

// ================================ Tokenizer =================================
// Converts raw text into tokens like numbers, operators, and parentheses.

typedef enum {
    T_EOF=0, T_NUM, T_PLUS, T_MINUS, T_STAR, T_SLASH, T_POW, T_LPAREN, T_RPAREN, T_INVALID
} TokType;  // Enumeration for token types

typedef struct {
    TokType type;         // Token type (operator, number, etc.)
    size_t start_pos;     // Position in the input (1-based index)
    int is_float;         // Whether number is float (for T_NUM)
    long long i;          // Integer value if applicable
    double d;             // Floating-point value if applicable
} Token;

// Scanner structure to manage parsing progress
typedef struct {
    const char *src; size_t len;   // Input source string and its length
    size_t pos;                    // 1-based current position
    size_t idx0;                   // 0-based index into src
    size_t err_pos;                // Position of the first error (if any)
    Token  cur;                    // Currently scanned token
} Scanner;

// Sets the first error position if it hasn't been set yet
static void set_error(Scanner *S, size_t p){ if(!S->err_pos) S->err_pos = p; }

// Skips whitespace and comment lines (lines starting with '#')
static void skip_ws_and_comments(Scanner *S){
    for(;;){
        // Skip whitespace characters
        while(S->idx0 < S->len && (
            S->src[S->idx0]==' '  ||
            S->src[S->idx0]=='\t' ||
            S->src[S->idx0]=='\r' ||
            S->src[S->idx0]=='\n'
        )){ S->idx0++; S->pos++; }

        // If current char is '#', skip the entire line
        if(S->idx0 < S->len && S->src[S->idx0]=='#'){
            while(S->idx0 < S->len && S->src[S->idx0] != '\n'){
                S->idx0++; S->pos++;
            }
            continue; // Continue to check for whitespace/comments again
        }
        break; // Exit when no more whitespace or comments
    }
}

// Creates a simple non-number token (like +, -, *, etc.)
static Token make_simple(TokType t, size_t p){
    Token x; memset(&x,0,sizeof x);
    x.type=t; x.start_pos=p;
    return x;
}

// Parses numeric literals (integers and floats)
static Token scan_number(Scanner *S){
    size_t start = S->pos;
    errno = 0;
    char *end = NULL;
    double dv = strtod(S->src + S->idx0, &end); // Try to parse as double
    if(end == S->src + S->idx0) return make_simple(T_INVALID, start); // No digits found

    size_t used = (size_t)(end - (S->src + S->idx0));
    size_t token_start = S->idx0;
    S->idx0 += used; S->pos += used;

    Token t; memset(&t,0,sizeof t);
    t.type = T_NUM; t.start_pos = start;

    int saw_dot_or_exp = 0;
    for(size_t i=0;i<used;i++){
        char c = S->src[token_start + i];
        if(c=='.' || c=='e' || c=='E'){ saw_dot_or_exp = 1; break; }
    }

    if(saw_dot_or_exp){ t.is_float=1; t.d=dv; }  // It's a float
    else{
        errno = 0;
        long long iv = strtoll(S->src + token_start, NULL, 10); // Try integer conversion
        if(errno==ERANGE){ t.is_float=1; t.d=dv; }              // Overflow => use double
        else { t.is_float=0; t.i=iv; t.d=dv; }                  // Normal integer
    }
    return t;
}

// Reads next token from input
static Token next_token(Scanner *S){
    skip_ws_and_comments(S);
    if(S->idx0 >= S->len) return make_simple(T_EOF, S->pos);

    char c = S->src[S->idx0];
    size_t p = S->pos;

    if(isdigit((unsigned char)c) || c=='.') return scan_number(S);
    if(c=='+'){ S->idx0++; S->pos++; return make_simple(T_PLUS, p); }
    if(c=='-'){ S->idx0++; S->pos++; return make_simple(T_MINUS, p); }
    if(c=='('){ S->idx0++; S->pos++; return make_simple(T_LPAREN, p); }
    if(c==')'){ S->idx0++; S->pos++; return make_simple(T_RPAREN, p); }
    if(c=='/'){ S->idx0++; S->pos++; return make_simple(T_SLASH, p); }
    if(c=='*'){
        // Check for '**' operator (power)
        if(S->idx0+1 < S->len && S->src[S->idx0+1]=='*'){
            S->idx0+=2; S->pos+=2;
            return make_simple(T_POW, p);
        }
        S->idx0++; S->pos++;
        return make_simple(T_STAR, p);
    }
    // If none matched, invalid character
    S->idx0++; S->pos++;
    return make_simple(T_INVALID, p);
}

// Advances to the next token in the stream
static void advance(Scanner *S){ S->cur = next_token(S); }

// ================================= Parser ===================================
// Recursive-descent parser for the arithmetic grammar.
// Grammar with unary:
//   expr  := term { ('+'|'-') term }
//   term  := power { ('*'|'/') power }
//   power := unary ( '**' power )?      // RIGHT-ASSOCIATIVE
//   unary := ('+'|'-') unary | primary
//   primary := NUMBER | '(' expr ')'

static Value parse_expr(Scanner *S);
static Value parse_term(Scanner *S);
static Value parse_power(Scanner *S);
static Value parse_unary(Scanner *S);
static Value parse_primary(Scanner *S);

// Parses an expression with + and - operators
static Value parse_expr(Scanner *S){
    Value v = parse_term(S);
    while(S->cur.type==T_PLUS || S->cur.type==T_MINUS){
        TokType op = S->cur.type;
        advance(S);
        Value r = parse_term(S);
        if(S->err_pos) return make_int(0);
        v = (op==T_PLUS)? v_add(v,r) : v_sub(v,r);
    }
    return v;
}

// Parses terms with * and / operators
static Value parse_term(Scanner *S){
    Value v = parse_power(S);
    while(S->cur.type==T_STAR || S->cur.type==T_SLASH){
        TokType op = S->cur.type;
        size_t slash_pos = S->cur.start_pos;
        advance(S);
        Value r = parse_power(S);
        if(S->err_pos) return make_int(0);
        v = (op==T_STAR)? v_mul(v,r) : v_div(v,r,&S->err_pos,slash_pos);
        if(S->err_pos) return make_int(0);
    }
    return v;
}

// Parses power expressions like a ** b
static Value parse_power(Scanner *S){
    Value left = parse_unary(S);
    if(S->cur.type==T_POW){
        advance(S);
        Value right = parse_power(S); // Right-associative
        left = v_pow(left,right);
    }
    return left;
}

// Parses unary operators (+a or -a)
static Value parse_unary(Scanner *S){
    if(S->cur.type==T_PLUS){ advance(S); return parse_unary(S); }
    if(S->cur.type==T_MINUS){
        advance(S);
        Value v=parse_unary(S);
        return v.is_float? make_double(-v.d) : make_int(-v.i);
    }
    return parse_primary(S);
}

// Parses numbers or parenthesized expressions
static Value parse_primary(Scanner *S){
    if(S->cur.type==T_NUM){
        Value v=S->cur.is_float? make_double(S->cur.d) : make_int(S->cur.i);
        advance(S);
        return v;
    }
    if(S->cur.type==T_LPAREN){
        advance(S);
        Value inside = parse_expr(S);
        if(S->err_pos) return make_int(0);
        if(S->cur.type!=T_RPAREN){
            if(S->cur.type==T_EOF) set_error(S, S->pos);
            else set_error(S, S->cur.start_pos);
            return make_int(0);
        }
        advance(S);
        return inside;
    }
    set_error(S, S->cur.start_pos);
    advance(S);
    return make_int(0);
}
// ============================== Evaluation API ==============================
// This section defines the evaluation interface. It runs the expression parser
// and returns either a computed numeric value or an error with position info.

typedef struct { int ok; Value v; size_t err_pos; } EvalResult; // Result struct: ok=1 if success, else error

// Evaluates an expression from a given input buffer
static EvalResult eval_buffer(const char *buf, size_t len){
    Scanner S; memset(&S,0,sizeof S);           // Initialize scanner
    S.src=buf; S.len=len; S.pos=1; S.idx0=0; S.err_pos=0; // Set input and reset positions
    advance(&S);                                // Load first token
    Value v = parse_expr(&S);                   // Parse the expression

    // If an error was encountered during parsing
    if(S.err_pos){
        EvalResult r={0,make_int(0),S.err_pos}; // Return error with position
        return r;
    }

    // If there are leftover tokens after the expression, mark as syntax error
    if(S.cur.type != T_EOF){
        set_error(&S, S.cur.start_pos);
        EvalResult r={0,make_int(0),S.err_pos};
        return r;
    }

    // Otherwise return success with computed value
    EvalResult r={1,v,0};
    return r;
}

// =============================== Printing ===================================
// Functions for printing evaluated results in human-readable form.

static int is_integral_double(double x){
    double r = llround(x);
    return fabs(x - r) < 1e-12;   // Check if double is almost an integer
}

// Prints Value to file with correct formatting (integer or float)
static void print_value(FILE *out, Value v){
    if(!v.is_float) fprintf(out, "%lld\n", v.i); // Integer: print directly
    else if(is_integral_double(v.d)) fprintf(out, "%lld\n", (long long)llround(v.d)); // Float but integral
    else fprintf(out, "%.15g\n", v.d);           // True floating value
}

// ================================ File I/O ==================================
// Handles reading input files, writing results, and directory management.

static int read_entire_file(const char *path, char **out_buf, size_t *out_len){
    FILE *f = fopen(path, "rb");                 // Open file in binary mode
    if(!f) return -1;
    if(fseek(f, 0, SEEK_END)!=0){ fclose(f); return -1; }
    long l = ftell(f); if(l<0){ fclose(f); return -1; } // Get file length
    if(fseek(f, 0, SEEK_SET)!=0){ fclose(f); return -1; }

    *out_buf = (char*)malloc((size_t)l + 1);     // Allocate buffer
    if(!*out_buf){ fclose(f); return -1; }

    size_t rd = fread(*out_buf, 1, (size_t)l, f); (void)rd; // Read all content
    fclose(f);
    (*out_buf)[l] = '\0'; *out_len = (size_t)l; // Null-terminate and set length
    return 0;
}

// Ensures that a directory exists, creates if missing
static int ensure_dir(const char *path){
    struct stat st;
    if(stat(path,&st)==0){                      // If already exists
        if(S_ISDIR(st.st_mode)) return 0;       // OK if it's a directory
        errno=ENOTDIR; return -1;               // Error if not
    }
    return mkdir(path, 0775);                   // Create directory
}

// Returns current username from environment or "user" if not found
static const char* get_username(){
    const char *u = getenv("USER");
    if(!u||!*u) u="user";
    return u;
}

// Returns base filename (without directory path)
static const char* base_name(const char *p){
    const char *s = strrchr(p,'/');
    return s? s+1 : p;
}

// Removes extension from filename (e.g., file.txt -> file)
static void strip_ext(const char *fname, char *out, size_t outsz){
    snprintf(out, outsz, "%s", fname);
    char *dot = strrchr(out, '.');
    if(dot) *dot = '\0';
}

// Checks if a filename ends with .txt
static int ends_with_txt(const char *name){
    size_t n=strlen(name);
    return (n>=4 && strcmp(name+n-4, ".txt")==0);
}

// Builds default output directory name: <input_base>_<username>_<STUDENT_ID>
static void build_default_outdir(const char *input_path, char *out, size_t outsz){
    char base[256];
    strip_ext(base_name(input_path), base, sizeof base);
    snprintf(out, outsz, "%s_%s_%s", base, get_username(), STUDENT_ID);
}

// Builds output filename: <input>_<Name>_<Lastname>_<StudentID>.txt
static void build_output_filename(const char *input_path, char *out, size_t outsz){
    char base[256];
    strip_ext(base_name(input_path), base, sizeof base);
    snprintf(out, outsz, "%s_%s_%s_%s.txt", base, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);
}

// ================================= CLI ======================================
// Handles command-line interface and argument parsing.

typedef struct { const char *dir; const char *outdir; const char *input; } Options;

// Prints program usage instructions
static void usage(const char *prog){
    fprintf(stderr,
      "Usage: %s [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt\n"
      "If -d is given, processes all *.txt in DIR (non-recursive).\n"
      "If -o omitted, output dir is <input_base>_<username>_%s\n",
      prog, STUDENT_ID);
}

// Parses command-line arguments and fills the Options struct
static int parse_args(int argc, char **argv, Options *opt){
    memset(opt,0,sizeof *opt);
    for(int i=1;i<argc;i++){
        if(strcmp(argv[i],"-d")==0 || strcmp(argv[i],"--dir")==0){
            if(i+1>=argc){ usage(argv[0]); return -1; }
            opt->dir = argv[++i];                 // Directory input mode
        } else if(strcmp(argv[i],"-o")==0 || strcmp(argv[i],"--output-dir")==0){
            if(i+1>=argc){ usage(argv[0]); return -1; }
            opt->outdir = argv[++i];              // Custom output directory
        } else if(argv[i][0]=='-'){               // Unknown option
            usage(argv[0]);
            return -1;
        } else {
            opt->input = argv[i];                 // Input file (default mode)
        }
    }

    // Require either a single file or a directory
    if(!opt->dir && !opt->input){
        usage(argv[0]);
        return -1;
    }
    return 0;
}
// =============================== Processing =================================
// These functions handle file processing: reading input, evaluating expressions,
// and writing output files (either results or error messages).

// Processes a single input file and writes the corresponding output file
static int process_one_file(const char *in_path, const char *out_dir){
    char *buf=NULL; size_t len=0;

    // Read the entire input file into memory
    if(read_entire_file(in_path,&buf,&len)!=0){
        fprintf(stderr,"read fail: %s\n", in_path);
        return -1;
    }

    // Evaluate the arithmetic expression(s) from the file buffer
    EvalResult R = eval_buffer(buf,len);

    // Build output file name and path
    char outname[512];
    build_output_filename(in_path, outname, sizeof outname);
    char outpath[1024];
    if(out_dir && *out_dir)
        snprintf(outpath,sizeof outpath,"%s/%s",out_dir,outname); // Output inside given dir
    else
        snprintf(outpath,sizeof outpath,"%s",outname);            // Output in current dir

    // Open output file for writing result
    FILE *f = fopen(outpath, "wb");
    if(!f){
        fprintf(stderr,"write fail: %s\n", outpath);
        free(buf);
        return -1;
    }

    // Write either the computed result or the error position
    if(!R.ok)
        fprintf(f, "ERROR:%zu\n", R.err_pos);
    else
        print_value(f, R.v);

    fclose(f);
    free(buf);
    return 0;
}

// Processes all *.txt files in a directory (non-recursively)
static int process_dir(const char *dir_path, const char *out_dir){
    DIR *d = opendir(dir_path);
    if(!d){
        fprintf(stderr,"open dir fail: %s\n", dir_path);
        return -1;
    }

    struct dirent *ent; int rc=0;

    // Iterate through each file in directory
    while((ent=readdir(d))!=NULL){
        // Skip "." and ".." (standard directory entries)
        if(strcmp(ent->d_name,".")==0 || strcmp(ent->d_name,"..")==0)
            continue;

        // Skip non-.txt files
        if(!ends_with_txt(ent->d_name))
            continue;

        // Build full input file path
        char inpath[1024];
        snprintf(inpath,sizeof inpath,"%s/%s", dir_path, ent->d_name);

        // Process each .txt file; record if any failed
        if(process_one_file(inpath,out_dir)!=0)
            rc=-1;
    }

    closedir(d);
    return rc; // 0 if all succeeded, -1 if any error occurred
}

// ================================== main ====================================
// Program entry point: parses CLI args, sets up directories, and starts processing.

int main(int argc, char **argv){
    Options opt;
    if(parse_args(argc,argv,&opt)!=0)
        return 1; // Exit if argument parsing failed

    char outdir_buf[512]={0};
    const char *outdir = opt.outdir;

    // Determine output directory: use CLI option or build default
    if(!outdir){
        if(opt.dir)
            build_default_outdir(opt.dir, outdir_buf, sizeof outdir_buf);
        else
            build_default_outdir(opt.input, outdir_buf, sizeof outdir_buf);
        outdir = outdir_buf;
    }

    // Ensure the output directory exists (create if missing)
    if(ensure_dir(outdir)!=0){
        fprintf(stderr,"cannot create/access output dir: %s\n", outdir);
        return 1;
    }

    int rc=0;

    // If -d/--dir provided: process all .txt files in that directory
    if(opt.dir)
        rc = process_dir(opt.dir, outdir);

    // If a single input file provided: process it individually
    if(opt.input){
        if(process_one_file(opt.input,outdir)!=0)
            rc=1;
    }

    return rc; // Return 0 for success, 1 for any error
}
