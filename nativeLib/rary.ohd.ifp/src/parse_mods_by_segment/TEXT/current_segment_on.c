#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

int current_segment_on(segment_name, total_segs, iseg, word)

 char   segment_name[][9];
 int    total_segs, iseg;
 char   *word;
{
 int    i, j, num_first_seg, num_last_seg;
 char   first_name[9], last_name[9];

 memset(first_name, '\0', 9);
 memset (last_name, '\0', 9);
/*
 * First see if word is a string with a hyphen in it.
 * If so, hyphen separates first and last segments in range.
 */
 i = 0;
 while(word[i] != '\0' && word[i] != '-')
   {                    /* copy first segment name */
    first_name[i] = word[i];
    i++;
   }
 if(word[i] == '-')
   {                    /* have a second segment name */
    i++;
    j = 0;
    while(word[i] != '\0')
      {
       last_name[j++] = word[i++];
      }
   }
 num_first_seg = num_last_seg = -1;
 for(i = 0; i < total_segs; i++)
    {
     if(strcmp(first_name, segment_name[i]) == 0)
	num_first_seg = i;
     if(strcmp(last_name, "\0") != 0)
       {
	if(strcmp(last_name, segment_name[i]) == 0)
	   num_last_seg = i;
       }
    }
    /* cew only print the warning once */
 if(iseg == 0 )
  {
   if(num_first_seg < 0 ||
      (strcmp(last_name, "\0") != 0 && num_last_seg < 0))
     {
      printf("Problem parsing mod card, the following segment id, range of segments, ");
      printf("or forecast group id is invalid: %s\n", word);
      return(FALSE);
     }
  }
 if(strcmp(last_name, "\0") == 0)
   {                            /* only one segment name in field */
    if(iseg == num_first_seg)
       return(TRUE);
    else
       return(FALSE);
   }
 else
   {                            /* range of segments */
    if(iseg >= num_first_seg && iseg <= num_last_seg)
       return(TRUE);
    else
       return(FALSE);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/parse_mods_by_segment/RCS/current_segment_on.c,v $";
 static char rcs_id2[] = "$Id: current_segment_on.c,v 1.3 2002/02/11 19:50:48 dws Exp $";}
/*  ===================================================  */

}
