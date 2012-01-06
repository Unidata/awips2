/*
 * function getword returns the next word (characters separated
 *  by blanks or commas) on the current line
 * idea taken from K&R pg 136
 * written by gfs, July 1991
 */
#include <stdio.h>
#include <ctype.h>

int getword(line_in, start_loc, limit, word)

char    *line_in, *word;
int     start_loc, limit;
{
 int    i, j;

 for (i = start_loc; i < limit + 1; i++)
     {                                  /* scan through line */
      if(line_in[i] != ' ')             /* skip any preceding spaces */
	{                               /* have a character  */
	 if(line_in[i] == ',')
	   {                            /* have a comma - null field */
	    *word = '\0';
	    return ++i;
	   }
	 if(line_in[i] == EOF)
	   {                            /* at end of file */
	    *word = '\0';
	    return EOF;
	   }
	 if(line_in[i] == '\n')
	   {                            /* at end of line */
	    *word = '\0';
	    return limit;
	   }
	 for(j = i; j < limit; j++)
	    {                           /* copy characters into word */
	     if(line_in[j] != ' '  &&
		line_in[j] != ','  &&
		line_in[j] != '\0' &&
		line_in[j] != EOF  &&
		line_in[j] != '\n')
	       {
		word[j-i] =  line_in[j];
	       }
	     else
	       {                        /* at end of word */
		word[j-i] = '\0';
		if(line_in[j] == EOF) return EOF;
		else                  return ++j;
	       }
	    }
	}
     }
 return limit;                          /* at end of line */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/parse_mods_by_segment/RCS/getword.c,v $";
 static char rcs_id2[] = "$Id: getword.c,v 1.1 1995/09/08 15:02:14 page Exp $";}
/*  ===================================================  */

}
