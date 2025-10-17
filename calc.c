#define STUDENT_NAME "Gulnur Yasemin"
#define STUDENT_LASTNAME "UYGUN"
#define STUDENT_ID "231ADB101"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

// -------- Value type ----------
typedef struct {
    int is_float;
    long long i;
    double d;
} Value;

static Value make_int(long long x){ Value v={0,x,(double)x}; return v; }
static Value make_double(double x){ Value v={1,(long long)x,x}; return v; }
static int is_zero(Value v){ return v.is_float ? fabs(v.d)==0.0 : v.i==0; }
static Value v_add(Value a,Value b){ return (a.is_float||b.is_float)?make_double((a.is_float?a.d:a.i)+(b.is_float?b.d:b.i)):make_int(a.i+b.i); }
static Value v_sub(Value a,Value b){ return (a.is_float||b.is_float)?make_double((a.is_float?a.d:a.i)-(b.is_float?b.d:b.i)):make_int(a.i-b.i); }
static Value v_mul(Value a,Value b){ return (a.is_float||b.is_float)?make_double((a.is_float?a.d:a.i)*(b.is_float?b.d:b.i)):make_int(a.i*b.i); }
static Value v_div(Value a,Value b,size_t *err_pos,size_t divisor_pos){
    if(is_zero(b)){ if(*err_pos==0)*err_pos=divisor_pos; return make_int(0);}
    return make_double((a.is_float?a.d:a.i)/(b.is_float?b.d:b.i));
}
static Value v_pow(Value a,Value b){ return make_double(pow(a.is_float?a.d:a.i,b.is_float?b.d:b.i)); }

// -------- Tokenizer ----------
typedef enum {T_EOF=0,T_NUM,T_PLUS,T_MINUS,T_STAR,T_SLASH,T_POW,T_LPAREN,T_RPAREN,T_INVALID} TokType;
typedef struct {TokType type;size_t start_pos;int is_float;long long i;double d;} Token;
typedef struct {const char *src;size_t len,pos,idx0,err_pos;Token cur;} Scanner;

static void set_error(Scanner *S,size_t p){if(!S->err_pos)S->err_pos=p;}
static void skip_ws(Scanner *S){
  for(;;){
    while(S->idx0<S->len&&(S->src[S->idx0]==' '||S->src[S->idx0]=='\\t'||S->src[S->idx0]=='\\r'||S->src[S->idx0]=='\\n')){S->idx0++;S->pos++;}
    if(S->idx0<S->len && S->src[S->idx0]=='#'){ while(S->idx0<S->len && S->src[S->idx0]!='\\n'){S->idx0++;S->pos++;} continue; }
    break;
  }
}
static Token make_simple(TokType t,size_t p){Token x={t,p,0,0,0};return x;}
static Token scan_number(Scanner *S){
  size_t start=S->pos;char *end=NULL;double dv=strtod(S->src+S->idx0,&end);
  if(end==S->src+S->idx0)return make_simple(T_INVALID,start);
  size_t used=end-(S->src+S->idx0);S->idx0+=used;S->pos+=used;
  Token t={T_NUM,start,0,0,0};int f=0;for(size_t i=0;i<used;i++){char c=S->src[S->idx0-used+i];if(c=='.'||c=='e'||c=='E')f=1;}
  if(f){t.is_float=1;t.d=dv;}else{t.i=strtoll(S->src+S->idx0-used,NULL,10);t.d=dv;}
  return t;
}
static Token next_token(Scanner *S){
  skip_ws(S);
  if(S->idx0>=S->len)return make_simple(T_EOF,S->pos);
  char c=S->src[S->idx0];size_t p=S->pos;
  if(isdigit((unsigned char)c)||c=='.')return scan_number(S);
  if(c=='+'){S->idx0++;S->pos++;return make_simple(T_PLUS,p);}
  if(c=='-'){S->idx0++;S->pos++;return make_simple(T_MINUS,p);}
  if(c=='*'){if(S->idx0+1<S->len&&S->src[S->idx0+1]=='*'){S->idx0+=2;S->pos+=2;return make_simple(T_POW,p);}S->idx0++;S->pos++;return make_simple(T_STAR,p);}
  if(c=='/'){S->idx0++;S->pos++;return make_simple(T_SLASH,p);}
  if(c=='('){S->idx0++;S->pos++;return make_simple(T_LPAREN,p);}
  if(c==')'){S->idx0++;S->pos++;return make_simple(T_RPAREN,p);}
  S->idx0++;S->pos++;return make_simple(T_INVALID,p);
}
static void advance(Scanner *S){S->cur=next_token(S);}

// -------- Parser ----------
static Value parse_expr(Scanner *S);
static Value parse_term(Scanner *S);
static Value parse_power(Scanner *S);
static Value parse_unary(Scanner *S);
static Value parse_primary(Scanner *S);

static Value parse_expr(Scanner *S){
  Value v=parse_term(S);
  while(S->cur.type==T_PLUS||S->cur.type==T_MINUS){
    TokType op=S->cur.type;advance(S);
    Value r=parse_term(S);if(S->err_pos)return make_int(0);
    v=(op==T_PLUS)?v_add(v,r):v_sub(v,r);
  }return v;
}
static Value parse_term(Scanner *S){
  Value v=parse_power(S);
  while(S->cur.type==T_STAR||S->cur.type==T_SLASH){
    TokType op=S->cur.type;size_t op_pos=S->cur.start_pos;advance(S);
    Value r=parse_power(S);if(S->err_pos)return make_int(0);
    v=(op==T_STAR)?v_mul(v,r):v_div(v,r,&S->err_pos,op_pos);
    if(S->err_pos)return make_int(0);
  }return v;
}
static Value parse_power(Scanner *S){
  Value left=parse_unary(S);
  if(S->cur.type==T_POW){advance(S);Value right=parse_power(S);left=v_pow(left,right);}
  return left;
}
static Value parse_unary(Scanner *S){
  if(S->cur.type==T_PLUS){advance(S);return parse_unary(S);}
  if(S->cur.type==T_MINUS){advance(S);Value v=parse_unary(S);return v.is_float?make_double(-v.d):make_int(-v.i);}
  return parse_primary(S);
}
static Value parse_primary(Scanner *S){
  if(S->cur.type==T_NUM){Value v=S->cur.is_float?make_double(S->cur.d):make_int(S->cur.i);advance(S);return v;}
  if(S->cur.type==T_LPAREN){advance(S);Value v=parse_expr(S);if(S->cur.type!=T_RPAREN){set_error(S,S->pos);return make_int(0);}advance(S);return v;}
  set_error(S,S->cur.start_pos);advance(S);return make_int(0);
}

// -------- Evaluate one buffer ----------
typedef struct{int ok;Value v;size_t err;}Eval;
static Eval eval_buf(const char *buf,size_t len){
  Scanner S={buf,len,1,0,0};advance(&S);
  Value v=parse_expr(&S);
  if(S.err_pos||S.cur.type!=T_EOF){if(!S.err_pos)set_error(&S,S.cur.start_pos);return (Eval){0,make_int(0),S.err_pos};}
  return (Eval){1,v,0};
}

// -------- Helpers ----------
static int is_int_double(double x){return fabs(x-llround(x))<1e-12;}
static void print_val(FILE *f,Value v){
  if(!v.is_float)fprintf(f,\"%lld\\n\",v.i);
  else if(is_int_double(v.d))fprintf(f,\"%lld\\n\",(long long)llround(v.d));
  else fprintf(f,\"%.15g\\n\",v.d);
}
static int read_file(const char *path,char **buf,size_t *len){
  FILE *f=fopen(path,\"rb\");if(!f)return-1;fseek(f,0,SEEK_END);long l=ftell(f);rewind(f);
  *buf=malloc(l+1);if(!*buf){fclose(f);return-1;}fread(*buf,1,l,f);fclose(f);(*buf)[l]='\\0';*len=l;return 0;
}

// -------- Main ----------
int main(int c,char **v){
  if(c<2){fprintf(stderr,\"usage: ./calc input.txt\\n\");return 1;}
  char *buf;size_t len;if(read_file(v[1],&buf,&len)){fprintf(stderr,\"cannot read %s\\n\",v[1]);return 1;}
  Eval R=eval_buf(buf,len);
  if(!R.ok)printf(\"ERROR:%zu\\n\",R.err);
  else print_val(stdout,R.v);
  free(buf);
  return 0;
}
