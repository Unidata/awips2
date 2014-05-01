/* File: get_apps_defaults.c
 *
 *  Program to resolve the value of a requested string.
 *
 *  The requested string to be resolved is supplied as the string
 *   variable <request>, the resolved request is returned as the string
 *   variable <reply>.
 *
 *  Request resolution occurs in one of three ways:
 *
 *   1. an environment variable matching in name to <request> is found;
 *      <reply>  is then the value of that env. variable unless the
 *      value is null,
 *
 *   2. <request> is found as a match in a file that establishes
 *      token - resource (t-r) relationships.  Three files may be scanned in
 *      this order:
 *         APPS_DEFAULTS_USER ..... a personal users set of tokens
 *         APPS_DEFAULTS_PROG ..... a program specific set of tokens
 *         APPS_DEFAULTS_SITE ..... a site wide set of tokens
 *         APPS_DEFAULTS .......... a system-wide (national) set of tokens
 *      to find the first token match to get a request.
 *
 *   3. if <request> can not be resolved, <reply> is assigned as the
 *      null string.
 *
 *  Each file is scanned from top to bottom looking for the first match
 *   between <request> and a defined token.  The syntax needed in either
 *   file is:
 *
 *        <token> <delimiter> <resource>
 *
 *   where:
 *    <token> is defined as a string delimited by white space or <delimiter>,
 *    <delimiter>  is the : (colon),
 *    <resource> is any string, the value returned depends
 *               on certain file conventions:
 *
 *      1. A valid t-r requires a valid token followed by a valid
 *         resource,
 *      2. the t-r relationship must be contained on a single line,
 *      3. no white space needs to surround <delimiter>,
 *      4. comments are indicated by a #,
 *      5. neither <token> nor <resource> can begin with a # or :,
 *      6. a # or a : can be embedded within <resource>,
 *      7. <resource> can contain white space if it is bounded by
 *         the ' or " characters,
 *      8. blank lines are allowed in the file,
 *      9. referbacks are indicated by $(...). The '...' is resolved
 *         the same way any other token is, and is substituted for
 *         the $(...) string to compose the final resource value.
 *     10. Multiple referbacks are allowed in <resource>, but embedded
 *         referbacks are not allowed (i.e. no $($(...)) allowed).
 *     11. First in wins.  That is, first finding of <token>
 *         matching <request> uses that resource value, even if null.
 *
 *  A sample of a t-r file:
#-----------------------------------------------------------------------
#  This is a comment line; so was previous line. Blank lines are
#   intentional and are allowed in file.

ofs_level     : testcase         # this is a comment on valid t-r
ofs_reor_lvl  : test:reor        # ':' allowed in body of <resource>
ofs_inpt_grp  : "test  case"     # white space allowed in <resource>

ofs_file_grp  : /home/$(ofs_level)/files # referback to prior token;
                                         # returned resource will be
                                         #  /home/testcase/files

ofs_xxx       xxx       # invalid t-r, no delimiter
ofs_yyy    : #yyy       # invalid t-r, no resource

#  This is comment line; so is following line
#-----------------------------------------------------------------------

 * Function written by JTOstrowski - HRL - 11/92
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define  LEN_TOKEN         128          /* maximum length of token in file */
#define  LEN_REPLY         512          /* maximum length of reply in single line in file */
                                        /* (also the max size of the reply output, incl \0) */
                                        /* (must be <= LEN_TOTREPLY) */
#define  LEN_LINE          520          /* maximum length of line in file */
#define  LEN_TOTREPLY      600          /* maximum length of all concatenated reply strings */

#define  RECUR_LIMIT        40          /* limit number of referback recursions */

#define  ENV_VAR_1 "APPS_DEFAULTS_USER" /* env. var. for personal t-r file */
#define  ENV_VAR_2 "APPS_DEFAULTS_PROG" /* env. var. for specific program */
#define  ENV_VAR_3 "APPS_DEFAULTS_SITE" /* env. var. for local site t-r file */
#define  ENV_VAR_4 "APPS_DEFAULTS"      /* env. var. for default t-r file */
#define  ENV_VAR_LENGTH     20          /* default length of env. var. */
#define  NUM_ENV_VAR         4          /* no. of env. vars. to use */
 
#define  RFR_OPEN    "$("               /* referback opening string */
#define  RFR_CLOSE   ")"                /* referback closing string */
#define  DELIM       ':'                /* delimiter character */
#define  COMMENT     '#'                /* comment character */
#define  QUOTE1      '\"'               /* 1st valid quote character */
#define  QUOTE2      '\''               /* 2nd valid quote character */
#define  BSLASH      '\\'               /* back slash */

#define  QPHRASE1 (opt_line[ilast] == QUOTE1 && ilast > 0 && opt_line[ilast - 1] != BSLASH)
#define  QPHRASE2 (opt_line[ilast] == QUOTE2 && ilast > 0 && opt_line[ilast - 1] != BSLASH)
#define  NPHRASE2 (isspace(opt_line[ilast])  && ilast > 0 && opt_line[ilast - 1] != BSLASH)

static int     r_cou = 0;               /* counter to limit recursion */
static int     ifile = 0;               /* file loop counter */
static int     i = 0;                   /* miscellaneous counter */
static int     ilast = 0;               /* last character position holder */
static int     iphrase = 0;             /* conditional phrase-ending indicator */
static char    token[LEN_TOKEN+1];      /* working token array */
static char    *as_env_var;             /* returned env. var. value */
static char    env_var_array[NUM_ENV_VAR][ENV_VAR_LENGTH];
static int     r_len = 0;               /* length of reply in referback */
static int     e_len = 0;               /* length of end of reply after a referback */

static FILE    *in[NUM_ENV_VAR];               /* file descripter */
static char    *opts_file[NUM_ENV_VAR];        /* file name holder */


/*------------------------------------------------------------------------------------------------*/
int get_apps_defaults(char *request, int *request_len, char *reply, int *reply_len)

{
  void get_apps_defaults_r(char *, char *);

  char    inquest[LEN_TOKEN+1];     /* entered token string recopied */
  char    resource[LEN_TOTREPLY+1]; /* working resource array */

/*  Set output to null in case something goes wrong, check input length for bad number */

    reply[0] = '\0';
    if ( *request_len>0  &&  *request_len<=LEN_TOKEN  &&  *request_len<=LEN_TOTREPLY )
    {

/*  Place entered string into local variable; append '\0';set recursion count global */

      (void)strncpy(inquest,request,*request_len);
      inquest[*request_len] = '\0';
      r_cou = 0;

/*  Fill the environment variable array */

      for (i = 0; i < NUM_ENV_VAR; i++)
      {
        (void)memset(env_var_array[i], '\0', ENV_VAR_LENGTH);
      }

      (void)strcpy(env_var_array[0], ENV_VAR_1);
      (void)strcpy(env_var_array[1], ENV_VAR_2);
      (void)strcpy(env_var_array[2], ENV_VAR_3);
      (void)strcpy(env_var_array[3], ENV_VAR_4);

/*  Make sure apps files are initialized as not-opened  */

      for (ifile = 0; ifile < NUM_ENV_VAR; ifile++)
      {
        in[ifile] = NULL;
        opts_file[ifile] = '\0';
      }

/*  Call true "C" routine that can be recursive using global variable "r_cou" */

      get_apps_defaults_r(inquest,resource);

/*  Close any apps files that may have been opened  */

      for (ifile = 0; ifile < NUM_ENV_VAR; ifile++)
      {
        if (in[ifile] != NULL)  (void)fclose(in[ifile]);
      }

/*  Place local output string into returned string */

      if ( (r_cou <= RECUR_LIMIT)  &&  ((int)strlen(resource) < LEN_REPLY) )
      {
        (void)strcpy(reply, &resource[0]);
      }
    }

/*  Get length, set return error status ( 0=token found, 1=error or no token ) */

    *reply_len = strlen(reply);
    return ( (*reply)  ?  0  :  1 );
}

/*------------------------------------------------------------------------------------------------*/
void get_apps_defaults_r(char inquest[], char resource[])

{
  char    *pOpen;                     /* referback opening position holder */
  char    *pClose;                    /* referback closing position holder */
  int     diff = 0;                   /* string comparison result */
  char    referback[LEN_TOKEN+1];     /* referback token array */
  char    refer_val[LEN_TOTREPLY+1];  /* referback value array */
  char    substitute[LEN_TOTREPLY+1]; /* expanded referback-ed resource */

/*  Initialize the result to NULL */

  (void)memset(resource, '\0', LEN_TOTREPLY+1);

/*  Check for the requested variable found as an environment variable */

  as_env_var = getenv(inquest);

  if ( as_env_var )        /* SAM, RTi */
    (void)strcpy(resource, as_env_var);
  else
  {

/*  Increment recursive counter (needed to avoid referbacks calling itself) */
 
    r_cou += 1;
    if (r_cou <= RECUR_LIMIT+1)
    {

/*  The resource file to be read is indicated by the value of an
     environment variable */

      for (ifile = 0;  ifile < NUM_ENV_VAR; ifile++)
      {
        char    opt_line[LEN_LINE+1];           /* t-r file line array */

/*  See if file can be opened for reading */

        if (in[ifile] == NULL)
        {
          opts_file[ifile] = getenv(env_var_array[ifile]);
          if ( opts_file[ifile] != NULL )
            in[ifile] = fopen(opts_file[ifile], "r");
        }
        else
        {
          rewind(in[ifile]);
        }

        if (in[ifile] != NULL)
        {

/*  Read file until either match is found or EOF reached */

          while (fgets(opt_line, LEN_LINE+1, in[ifile]) != NULL)
          {
            i = 0;

/*  Ignore blank lines (nl only) */

            if (strlen(opt_line) > 1)
            {

/*  Only scan lines with the delimiter in them */

              if (strchr(opt_line, DELIM) != NULL)
              {
                i = 0;

/*  Look for first non-blank character on line */
                while (i < strlen(opt_line)-1 && isspace(opt_line[i]) != 0)
                  i++;
                if (i < strlen(opt_line))
                {

/*  Discard line if first character is either delimiter or comment indicator */

                  if (opt_line[i] != COMMENT && opt_line[i] != DELIM)
                  {
                    ilast    = i;
                    i        = 0;
                    token[0] = '\0';

/*  Look for token based on rules for delimiting tokens */

                    while (isprint(opt_line[ilast]) != 0 &&
                           isspace(opt_line[ilast]) == 0 &&
                           opt_line[ilast]          != DELIM)
                    {
                      token[i++] = opt_line[ilast++];
                    }
                    token[i] = '\0';

/*  See if token on line is one to be retrieved */

                    if (strlen(inquest) == strlen(token) &&
                       strncmp(token, inquest, i) == 0)
                    {
/*  Match found, now determine associated resource.
     Resource can not start with DELIM or COMMENT characters
     or any non-printing characters */

                      i = ilast;
                      resource[0] = '\0';
                      while (i < strlen(opt_line)-1     &&
                             (isspace(opt_line[i]) != 0 ||
                             opt_line[i]          == DELIM))
                      {
                        i++;
                      }

/* Determine contents of resource until:
      1. End of line is reached, or
      2. White space is found for resources not quoted, or
      3. Closing matching quote character is found for quoted strings. */

                      if (i < strlen(opt_line))
                      {
                        if (opt_line[i] != COMMENT)
                        {
                          ilast  = i;
                          i      = 0;

/* Check to see if resource string is quoted (single or double) */

                          if (QPHRASE1)
                            iphrase = 1;
                          else if (QPHRASE2)
                            iphrase = 2;
                          else
                            iphrase = 0;

/*  Complete resource based on start character conditions */

                          switch (iphrase)
                          {
                            case 0:
                              while (isprint(opt_line[ilast]) && !NPHRASE2)
                                resource[i++] =  opt_line[ilast++];
                              break;
                            case 1:
                              ilast++;
                              while (isprint(opt_line[ilast]) && !QPHRASE1)
                                resource[i++] = opt_line[ilast++];
                              break;
                            case 2:
                              ilast++;
                              while (isprint(opt_line[ilast]) && !QPHRASE2)
                                resource[i++] = opt_line[ilast++];
                              break;
                          }
                          resource[i] = '\0';

/* Now look for any embedded referbacks in the resource string */

                          while ( ((pOpen  = strstr(resource, RFR_OPEN)) != NULL) &&
                                 ((pClose = strstr(pOpen,   RFR_CLOSE)) != NULL) &&
                                 ((diff   = (int)(pClose - pOpen) - 2)  >= 0   )    )
                          {
                            (void)memset(substitute, '\0', LEN_TOTREPLY+1);
                            if (strcmp(resource, pOpen))       /* SAM, RTi */
                              (void)strncpy(substitute, resource, (pOpen - &resource[0]));
                            if ( diff > 0)
                            {
                              (void)memset(referback, '\0', LEN_TOKEN+1);
                              (void)memset(refer_val, '\0', LEN_REPLY+1);
                              (void)strncpy(referback, pOpen + 2, diff);

                              (void)get_apps_defaults_r(referback, refer_val);

                              if (r_cou > RECUR_LIMIT+1)
                                break;

                              e_len = (int)(strlen(resource) - (int)(pClose - &resource[0]));
                              e_len = e_len - 1;
                              r_len = (int)(strlen(refer_val));
                              if ( ((int)strlen(substitute) + r_len + e_len) > LEN_TOTREPLY)
                              {
                                r_cou = RECUR_LIMIT+1;
                                break;
                              }
                              else
                              {
                                if (r_len > 0)
                                  (void)strcat(substitute, refer_val);
                              }
                            }
                            (void)strcat(substitute, pClose + 1);
                            (void)strcpy(resource, substitute);
                          }    /* end of referback expansion while-loop */
                          break;
                        }    /* end of non-comment part of resource request if-loop */
                      }    /* end of resource request characters if-loop */
                    }    /* end of token-been-found if-loop */
                  }    /* end of discard-if-delimiter-or-comment check if-loop */
                }    /* end of search for non-blank token char if-loop */
              }    /* end of check for line with a delimiter in it if-loop */
            }    /* end of non-blank line scan if-loop */
          }    /* end of t-r file read to EOL if-loop */
          if (resource[0])      /* SAM, RTi */
            break;
        }    /* end of legitimate file opening loop */
      }    /* end of loop thru files named by env vars */
    }   /* end of loop where recursion is less than limit */
    else
    {
      resource[0] = '\0';
    }
  }    /* end obtaining of resource for given token */

}
