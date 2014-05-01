#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_SEGS_IN_FG 100
#define MAX_SEGS_LINE 140
int getline();
int getword();
void get_segments();

chkdsh(int *stat, int array[20],char cur_seg[8])
{

char *ptr= (char * )&array[0];

     *stat = Check_Range(ptr,cur_seg);
     /* printf("status_ckdash = %d \n", *stat); */
}


int Check_Range(char *buf,char *curseg)
{
   FILE         *fnamefp;

   char         *cptr,
		*cptr1,
		fseg_name[10],
		lseg_name[10],
		*segment_name= "          ";
   int          n,
                found,
		range_found,
		valid_range = 0;

   memset(fseg_name,'\0',10);
   memset(lseg_name,'\0',10);

        if (cptr = strstr (buf,"-") )
        {
 		if (isdigit ( *(cptr+1)))
                {
                        valid_range = 0;
                }
                else
                {
			strncpy(fseg_name,buf,(strlen(buf) - strlen(cptr)));
			if (cptr1 = strstr(cptr," "))
			{
				strncpy(lseg_name,cptr+1,(strlen(cptr +1) - strlen(cptr1)));
			}
			valid_range = check_valid_range(fseg_name, lseg_name, curseg);
			memset(fseg_name,'\0',10);
			memset(lseg_name,'\0',10);
                }
        }

   return (valid_range);
}

int check_valid_range(char fseg[20], char lseg[20], char *sname)
{

	char	segment_names[MAX_SEGS_IN_FG][9];
	int	n,
		i, j, k,
		numsegs;

	get_segments(segment_names, &numsegs);

	i = j = -1;
	for (n = 0; n < numsegs; n++)
	{
		if ( strcmp (segment_names[n], fseg) == 0) i = n;
		if ( strcmp (segment_names[n], lseg) == 0) j = n;

	}
	/* printf(" i j = %d %d\n",i ,j); */
	for (k = i; k < (j+1); k++)
	{
	        /*printf("segnames: %s curseg: %s\n", segment_names[k], sname); */
		if ( strncmp (segment_names[k], sname,strlen(segment_names[k])) == 0 )
		    return ( 1 );
	}
        return (0);
}


void get_segments(char segment_names[MAX_SEGS_IN_FG][9], int *numsegs)
{

  FILE		*segs_input_file;
  int		i,j,NumRCSum;
  int		total_segs, start_loc, iseg;
  char  	segs_input_file_name[80];
  char  	line[MAX_SEGS_LINE];


  strcpy(segs_input_file_name, getenv("HOME"));
  strcat(segs_input_file_name, "/.ifp_files/local/seg_sort.in");
  segs_input_file = fopen(segs_input_file_name, "r");
  if(segs_input_file == NULL)
    {
     printf("No list of segments is available for forecast group %s\n");
     exit(1);
    }

/*
 * Read through the segs_input_file once to get:
 *  1.  the total number of segments in this forecast group (total_segs) and
 *  2.  an array with all segment names for this forecast group (segment_names).
 * <<<<<< read the segment names from .ifp_files/local/seg_sort.in >>>>>>
 */

  total_segs = 0;
  while (getline(line, MAX_SEGS_LINE, segs_input_file) > 0)
  {                                   /* get a segment name */
     /*printf("from segs file, %s", line);   */
     start_loc = 0;
     getword(line, start_loc, MAX_SEGS_LINE, segment_names[total_segs++]);

     for (i = 0; i < 2; i++) /*skip next 2 lines*/
       getline(line, MAX_SEGS_LINE, segs_input_file);

     fscanf (segs_input_file,"%d\n",&NumRCSum); /*get number of forecast points*/
     /*
      * skip the next 6*NumRCSum lines
      */
    for (j=0;j<NumRCSum;j++)
     for (i = 0; i < 6; i++)
        {
         getline(line, MAX_SEGS_LINE, segs_input_file);
   /*      printf("skipping %s", line);   */
        }
    } /* end while (getline(line, MAX_SEGS_LINE, ... */
    *numsegs = total_segs;
 /*printf("total_segs = %d\n", total_segs);
 for(iseg = 0; iseg < total_segs; iseg++)
 printf("  %s\n", segment_names[iseg]);*/
 fclose(segs_input_file);

}


/*
 *  Read a line into s, return length
 *  from K & R, page 29.
 *  entered by gfs, 7/10/91
 */
//int getline(s, lim, stream)
//
//char    s[];
//int     lim;
//FILE    *stream;
//{
//  int c, i, j,start;
//
//  for (i = 0; i < lim-1 && (c = getc(stream)) != EOF && c != '\n'; i++)
//       s[i] = c;
//
///* remove trailing blanks*/
//  start=i;
//  if (c == '\n')
//     {
//      start=i-1;
//     }
//  for(j=start; j>0 && s[j] == ' '; j--)
//    ;;
//
///* now put in return */
//  if (c == '\n')
//     {
//      ++j;
//      s[j] = c;
//      ++j;
//     }
//  s[j] = '\0';
//  return j;
//
//
//}

/*
 * function getword returns the next word (characters separated
 *  by blanks or commas) on the current line
 * idea taken from K&R pg 136
 * written by gfs, July 1991
 */

//int getword(line_in, start_loc, limit, word)
//
//char    *line_in, *word;
//int     start_loc, limit;
//{
// int    i, j;
//
// for (i = start_loc; i < limit + 1; i++)
//     {                                  /* scan through line */
//      if(line_in[i] != ' ')             /* skip any preceding spaces */
//	{                               /* have a character  */
//	 if(line_in[i] == ',')
//	   {                            /* have a comma - null field */
//	    *word = '\0';
//	    return ++i;
//	   }
//	 if(line_in[i] == EOF)
//	   {                            /* at end of file */
//	    *word = '\0';
//	    return EOF;
//	   }
//	 if(line_in[i] == '\n')
//	   {                            /* at end of line */
//	    *word = '\0';
//	    return limit;
//	   }
//	 for(j = i; j < limit; j++)
//	    {                           /* copy characters into word */
//	     if(line_in[j] != ' '  &&
//		line_in[j] != ','  &&
//		line_in[j] != '\0' &&
//		line_in[j] != EOF  &&
//		line_in[j] != '\n')
//	       {
//		word[j-i] =  line_in[j];
//	       }
//	     else
//	       {                        /* at end of word */
//		word[j-i] = '\0';
//		if(line_in[j] == EOF) return EOF;
//		else                  return ++j;
//	       }
//	    }
//	}
//     }
// return limit;                          /* at end of line */
//
//
//
///*  ==============  Statements containing RCS keywords:  */
//{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/chkdsh.c,v $";
// static char rcs_id2[] = "$Id: chkdsh.c,v 1.3 2002/10/10 16:31:12 dws Exp $";}
///*  ===================================================  */
//
//}


