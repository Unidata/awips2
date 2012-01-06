#include <stdio.h>
/*
 *  Read a line into s, return length
 *  from K & R, page 29.
 *  entered by gfs, 7/10/91
 */
int getline(s, lim, stream)

char    s[];
int     lim;
FILE    *stream;
{
  int c, i, j,start;

  for (i = 0; i < lim-1 && (c = getc(stream)) != EOF && c != '\n'; i++)
       s[i] = c;

/* remove trailing blanks*/
  start=i;
  if (c == '\n')
     {
      start=i-1;
     }
  for(j=start; j>0 && s[j] == ' '; j--)
    ;;

/* now put in return */
  if (c == '\n')
     {
      ++j;
      s[j] = c;
      ++j;
     }
  s[j] = '\0';
  return j;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/parse_mods_by_segment/RCS/getline.c,v $";
 static char rcs_id2[] = "$Id: getline.c,v 1.2 1997/06/24 17:53:09 page Exp $";}
/*  ===================================================  */

}
