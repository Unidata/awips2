/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: atofeedt.c,v 1.39.10.1.2.5 2008/10/09 19:55:56 steve Exp $ */

#include "ldmconfig.h"

#include <ctype.h>
#include <string.h>

#include "ldm.h"        /* feedtypet */
#include "atofeedt.h"

/*
 * Needs to stay in sync with ldm.h feedtypet.  The names for feeds must
 * consist of only alphanumeric characters, to permit parsing feed-type
 * expressions.  This is also used in ldmprint.c, where it is assumed that
 * more inclusive sets (e.g. WMO) appear in this list *after* the sets they
 * include (e.g. DDPLUS, HDS, IDS). 
 */
struct fal fassoc[] = {
        {"none",     NONE},
        {"pps",      FT0},
        {"dds",      FT1},
        {"ddplus",   DDPLUS},
        {"hds",      FT2},      /* used to be HRS, and before that NPS */
        {"ids",      FT3},      /* used to be INTNL */
        {"spare",    FT4},
        {"wmo",      WMO},
        {"uniwisc",  FT5},
        {"unidata",  UNIDATA},
        {"pcws",     FT6},
        {"fsl2",     FT7},
        {"fsl3",     FT8},
        {"fsl4",     FT9},
        {"fsl5",     FT10},
        {"fsl",      FSL},
        {"gpssrc",   FT11},
        {"conduit",  FT12},
        {"fnexrad",  FT13},
        {"nmc",      NMC},
        {"lightning",FT14},
        {"wsi",      FT15},
        {"difax",    FT16},
        {"faa604",   FT17},
        {"gps",      FT18},
        {"fnmoc",    FT19},
        {"gem",      FT20},
        {"nimage",   FT21},
        {"ntext",    FT22},
        {"ngrid",    FT23},
        {"npoint",   FT24},
        {"ngraph",   FT25},
        {"nother",   FT26},
        {"nport",    NPORT},
        {"nexrad3",  FT27},
        {"nexrad2",  FT28},
        {"nxrdsrc",  FT29},
        {"exp",      FT30},
        {"any",      ANY},
        /* aliases, these must be *after* the "ANY" entry  */
        {"ft0",      FT0},
        {"domestic", FT1},
        {"ft1",      FT1},
        {"hrs",      FT2},
        {"ft2",      FT2},
        {"intnl",    FT3},
        {"ft3",      FT3},
        {"nps",      FT4}, /* backward compatibility only */
        {"ft4",      FT4},
        {"ft5",      FT5},
        {"mcidas",   FT5},
        {"acars",    FT6},
        {"ft6",      FT6},
        {"profiler", FT7},
        {"ft7",      FT7},
        {"ft8",      FT8},
        {"ft9",      FT9},
        {"ft10",     FT10},
        {"profs",    FSL},
        {"afos",     FT11},
        {"nmc1",     FT11},
        {"ft11",     FT11},
        {"nmc2",     FT12},
        {"nceph",    FT12},
        {"ft12",     FT12},
        {"nmc3",     FT13},
        {"ft13",     FT13},
        {"ft14",     FT14},
        {"nldn",     FT14},
        {"ft15",     FT15},
        {"ft16",     FT16},
        {"faa",      FT17},
        {"604",      FT17},
        {"ft17",     FT17},
        {"ft18",     FT18},
        {"nogaps",   FT19},
        {"seismic",  FT19},
        {"ft19",     FT19},
        {"cmc",      FT20},
        {"ft20",     FT20},
        {"image",    FT21},
        {"ft21",     FT21},
        {"text",     FT22},
        {"ft22",     FT22},
        {"grid",     FT23},
        {"ft23",     FT23},
        {"point",    FT24},
        {"nbufr",    FT24},
        {"bufr",     FT24},
        {"ft24",     FT24},
        {"graph",    FT25},
        {"ft25",     FT25},
        {"other",    FT26},
        {"ft26",     FT26},
        {"nnexrad",  FT27},
        {"nexrad",   FT27},
        {"ft27",     FT27},
        {"nexrd2",   FT28},
        {"craft",    FT28},
        {"ft28",     FT28},
        {"ft29",     FT29},
        {"ft30",     FT30},
        /* Terminator */
        {NULL, NONE}
};

/*
 * Implements a recursive descent parser for feedtype expressions that involve
 * set operations.  These include union (| or +), set difference (-), and set 
 * complement (~).  A grammar for these expressions is:
 *
 *      feedexp:
 *              feedexp | feedterm
 *              feedterm
 *      
 *      feedterm:
 *              feedterm - feedprimary
 *              feedprimary
 *      
 *      feedprimary:
 *              NAME
 *              ~ feedprimary
 *              ( feedexp )
 *
 *      NAME:
 *              NONE
 *              PPS
 *              DDS
 *              DDPLUS
 *               ...
 *              ANY
 *
 * This implementation is adapted from an example of a recursive descent
 * parser for arithetic expressions in Stroustrup's "The C++ Programming
 * Language".
 */

enum token_value 
{
    NAME, END, UNION = '|', DIFF = '-', COMPLEMENT = '~', LP = '(', RP = ')'
};

struct parse_state {
    int err;                    /* 0 if no errors encountered */
    enum token_value tok;       /* current token */
    char name[MAXFEEDTYPELEN];  /* feed name string, if tok == NAME */
    const char *cp;             /* current position in string being parsed */
};
typedef struct parse_state parse_state;


#ifdef __cplusplus
static feedtypet feedexpr(parse_state *);
static feedtypet feedprimary(parse_state *);
static feedtypet feedterm(parse_state *);
static feedtypet feed_type(char *, parse_state *);
static enum token_value get_token(parse_state *);
#elif defined(__STDC__)
static feedtypet feedexpr(parse_state *);
static feedtypet feedprimary(parse_state *);
static feedtypet feedterm(parse_state *);
static feedtypet feed_type(char *, parse_state *);
static enum token_value get_token(parse_state *);
#else /* Old Style C */
static feedtypet feedexpr();
static feedtypet feedprimary();
static feedtypet feedterm();
static feedtypet feed_type();
static enum token_value get_token();
#endif


/*
 * Convert string to feedtypet.  Set err if unrecognized feedtype string. 
 */
static feedtypet 
feed_type(
     char *str,
     parse_state *psp)
{
    char buf[MAXFEEDTYPELEN];
    char *in, *cp;
    struct fal *ap;

    if(psp->err)
      return NONE;

    for( in = str, cp = buf;
        *in != 0 && cp < &buf[MAXFEEDTYPELEN-1] && isalnum(*in);
        cp++, in++)
      {
          *cp = (char)(isupper(*in) ? (char)tolower(*in) : *in);
      }
    *cp = 0;
    for(ap = fassoc; ap->name != NULL; ap ++)
      {
          if(strcmp(ap->name, buf) == 0)
            return ap->type;
      }
    psp->err = FEEDTYPE_ERR_UKFT;
    return NONE;
}


/* 
 * Return current token value and advance token pointer in input string.
 * Also if token is NAME, store name string in psp->name.
 * Store token in psp->tok, so other functions can look
 * ahead one token.
 */
static enum token_value 
get_token(
     parse_state *psp)
{
    char* np=psp->name;

    if(psp->err)
      return END;

    while (isspace(*psp->cp))
      psp->cp++;                /* skip white space */

    if (*psp->cp == 0)
      return psp->tok=END;      /* end of string */

    switch (*psp->cp) {
      case '|':
      case '+':                 /* synonym for '|' */
        psp->cp++;
        return psp->tok=UNION;
      case '-':
        psp->cp++;
        return psp->tok=DIFF;
      case '(':
        psp->cp++;
        return psp->tok=LP;
      case ')':
        psp->cp++;
        return psp->tok=RP;
      case '~':
        psp->cp++;
        return psp->tok=COMPLEMENT;
      default:                  /* must be a feed name */
          while (isalnum(*psp->cp)) {
              *np++ = *psp->cp++;
          }
        *np = '\0';
        return psp->tok=NAME;
    }
}

/* handle primaries */
static feedtypet
feedprimary(
    parse_state *psp)
{
    feedtypet expr;

    if(psp->err)
      return feed_type("NONE", psp);

    switch (psp->tok) {
      case NAME:
        (void)get_token(psp);
        return feed_type(psp->name, psp);
      case COMPLEMENT:
        (void)get_token(psp);
        return ~ feedprimary(psp);
      case LP:
        (void)get_token(psp);
        expr = feedexpr(psp);
        if (psp->tok != RP) {
            psp->err = FEEDTYPE_ERR_RP; /* right paren expected */
            return feed_type("NONE", psp);
        }
        (void)get_token(psp);
        return expr;
      default:
        psp->err = FEEDTYPE_ERR_PRIM; /* feedprimary expected */
        return feed_type("NONE", psp);
    }
}


static feedtypet
feedterm(
     parse_state *psp)
{
    feedtypet left = feedprimary(psp);

    if(psp->err)
      return feed_type("NONE", psp);

    for(;;) {
        switch (psp->tok) {
          case DIFF:
            (void)get_token(psp);     /* eat '-' */
            left &= ~ feedprimary(psp); /* set difference */
            break;
          default:
            return left; 
        }
    }
    /*NOTREACHED*/
}


static feedtypet
feedexpr(
     parse_state *psp)
{
    feedtypet left = feedterm(psp);

    if(psp->err)
      return feed_type("NONE", psp);

    for(;;) {
        switch (psp->tok) {
          case UNION:
            (void)get_token(psp);     /* eat '|' */
            if(psp->err)
              return feed_type("NONE", psp);
            left |= feedterm(psp);
            break;
          default:
            return left;
        }
    }
    /*NOTREACHED*/
}


/*
 * The old, deprecated interface.  Given a string representing a feedtype
 * expression, this function returns a feedtype.  If there is an error in
 * parsing the feedtype expression, returns the feedtype NONE.  The problem
 * with this is there is no way to distinguish between the feedtype from an
 * explicitly specified NONE, and NONE resulting from an erroneous
 * feedype expression.  Use strfeedtypet() instead, which returns an error
 * indication.
 */
feedtypet
atofeedtypet(const char *str)
{
    feedtypet result = NONE;
    (void) strfeedtypet(str, &result);
    return result;
}

 
/*
 * Given the string str, representing a feedtype expression, and a pointer
 * to a feedtypet, stores the feedtype specified by the expression.  The
 * return value of the function indicates whether an error was encountered
 * parsing the expression.  FEEDTYPE_OK is returned in case there were no
 * errors.  If an error is encountered, the value of the feedtypet pointed
 * at is not changed. 
 */
int 
strfeedtypet(
     const char *str,
     feedtypet *result)
{
    feedtypet left;
    parse_state ps;

    ps.cp = str;
    ps.err = FEEDTYPE_OK;

    (void)get_token(&ps);
    left = feedexpr(&ps);
    if (ps.err == 0 && ps.tok != END) {
        ps.err = FEEDTYPE_ERR_GARB; /* garbage at end of expression */
        left = feed_type("NONE", &ps);
    }
 
    if (ps.err == 0)
      *result = left;
   return ps.err;
}


char *
strfeederr(
     int err)
{
    switch (err) {
      case FEEDTYPE_OK:
        return "";
      case FEEDTYPE_ERR_RP:
        return "missing right paren in feedtype expression";
      case FEEDTYPE_ERR_PRIM:
        return "bad syntax in feedtype expression";
      case FEEDTYPE_ERR_GARB:
        return "garbage at end of feedtype expression";
      case FEEDTYPE_ERR_UKFT:
        return "unknown feed name in feedtype expression";
      default:
        return "error in feedtype expression";
    }
}


#ifdef TEST_STRFEEDTYPET
#include <stdio.h>

int
main()
{
#define MAXEXPRLEN      100
    char str[MAXEXPRLEN];
    int err;
    feedtypet result;

    while ( fgets(str, MAXEXPRLEN, stdin) ) {
        str[strlen(str)-1] = '\0'; /* replace newline with NUL */
        printf("\"%s\"\t\t", str);
        err = strfeedtypet(str, &result);
        if (err == FEEDTYPE_OK)
          printf("%x\n", result);
        else
          printf("%s\n", strfeederr(err));
    }
    return 0;
}

#endif
