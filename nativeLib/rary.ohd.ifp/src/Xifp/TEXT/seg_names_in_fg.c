/* File: seg_names_in_fg.c
 *
 * Returns the segment id given the forecast group id.
 *
 */
#include "c_call_f/read_seg_names.h"

char ** seg_names_in_fg(fg_id, nseg)

char *  fg_id;
int  *  nseg;
{
 char **      idsegn;        /* address of segment id name pointer */
 char         loc_fg_id[8];  /* location of forecast group id */
 int          i, j;          /* counters */
 char         loc_idsegn[100][8]; /* location of segment id name */

 memset(loc_fg_id, ' ', 8);
 strncpy(loc_fg_id, fg_id, strlen(fg_id));

 for (i = 0; i < 100; i++)
      memset(loc_idsegn[i], '\0', 8);

 READ_SEG_NAMES(loc_fg_id, nseg, loc_idsegn);

 if(*nseg > 0)
   {
    idsegn = (char **)malloc(*nseg * sizeof(char *));
    for (i = 0; i < *nseg; i++)
	{
	 idsegn[i] = (char *)malloc(9 * sizeof(char));
	 idsegn[i][8] = '\0';
	 strncpy(idsegn[i], loc_idsegn[i], 8);
	 for (j = 0; j < 8; j++)
	      if(idsegn[i][j] == ' ')
		{
		 idsegn[i][j] = '\0';
		 break;
		}
	}
   }
 return(idsegn);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/seg_names_in_fg.c,v $";
 static char rcs_id2[] = "$Id: seg_names_in_fg.c,v 1.2 2002/02/11 19:10:29 dws Exp $";}
/*  ===================================================  */

}
