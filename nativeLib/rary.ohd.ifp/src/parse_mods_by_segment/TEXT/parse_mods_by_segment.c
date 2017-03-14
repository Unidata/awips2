/*
 * parse_mods_by_segment - This program reads the mod card file copied from
 *  the OFS files (which has all the mods for a forecast group), and the
 *  $HOME/.ifp_files/local/seg_sort.in file (which lists all of the segments in the
 *  forecast group), and creates individual files with mod cards that apply
 *  only to each segment.
 * written by gfs, July 1991.
 * modified to allow comment lines (starting with a '$') to be
 *  used in the mod input file - modified by gfs, 950301.
 * modified so that lines longer that 72 characters can be input,
 *  but only the first 72 characters are interpreted -
 *  modified by gfs, 950303.
 * modified so that blank lines are ignored - by gfs, 950305.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "c_call_f/mcomnd.h"

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#define MAX_MODS_LINE 73
#define MAX_SEGS_LINE 140
#define MAX_SEGS_IN_FG 100
#define COMMAND_LINE_LENGTH 200

void getfgroupid();

int getline();
int getword();
int current_segment_on();
int is_mod_continuation();
char *getenv();
int rangeCheck(char *);
parse_mods_by_segment_main()

{
  FILE  *mods_input_file, *segs_input_file, *mods_output_file;
  FILE  *fgmods_output_file;
  FILE	*save_FgNamefp;
  int   len, start_loc, loc_next, keep, keep_FR, discarded_mod;

  int	range_flg;

  int   fgroup_on_command_line;
  int   istat=0, i, command_line_copied, continuation_expected,j,NumRCSum;
  int   iseg, total_segs;
  int   icmnd, isfg, ndts;
  int   str_chk;



  char  mods_fg_file_name[80];
  char  segs_input_file_name[80];
  char  mods_output_file_name[80];
  char  fgmods_output_file_name[80];

  char	FgNamef[80];
  char	fg_range_fstr[20];
  char      system_command_line[COMMAND_LINE_LENGTH];
  char  line[MAX_SEGS_LINE], word[MAX_SEGS_LINE], *word_ptr;
  char  tmp_str[18];
  char  *tmp;
  char  hold_command_line[MAX_SEGS_LINE];
  char  segment_names[MAX_SEGS_IN_FG][9], fgroup_id[9];


  char  cmdnam[9];
  char  padded_cmdnam[9]; /* mod name padded with blanks */


  memset(fgroup_id, '\0', 9);

  getfgroupid(fgroup_id, &istat);

  if(istat != 0)
    {
     exit(1);
    }

  /* store FG id for the fcfcargs.f to read */
  strcpy(FgNamef, getenv("HOME"));
  strcat(FgNamef,
	  "/.ifp_files/STORE_FG_NAME");
  strcpy(fg_range_fstr,"FG_RANGE_");
  strcat(fg_range_fstr,fgroup_id);

  save_FgNamefp = fopen(FgNamef,"w");
  if ( save_FgNamefp == NULL )
  	printf("Error in open file %s\n",FgNamef);
  else
  	fprintf(save_FgNamefp, "%s" ,fg_range_fstr);

  for(i = 0; i < 9; i++)
      if(fgroup_id[i] == ' '){
	 fgroup_id[i] = '\0';
	 break;
      }

  strcpy(mods_fg_file_name, getenv("HOME"));
  strcat(mods_fg_file_name, "/.ifp_files/mods_from_ofs/FGroups/");
  strcat(mods_fg_file_name, fgroup_id);
  mods_input_file =  fopen(mods_fg_file_name, "r");
  if(mods_input_file == NULL)
    {
     printf("No mods from the OFS are available for forecast group %s\n", fgroup_id);
     exit(0);
    }
  strcpy(segs_input_file_name, getenv("HOME"));
  strcat(segs_input_file_name, "/.ifp_files/local/seg_sort.in");
  segs_input_file = fopen(segs_input_file_name, "r");
  if(segs_input_file == NULL)
    {
     printf("No list of segments is available for forecast group %s\n", fgroup_id);
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
      * skip the next 8 lines
      */
    for (j=0;j<NumRCSum;j++)
     for (i = 0; i < 6; i++)
	{
	 getline(line, MAX_SEGS_LINE, segs_input_file);
         /*printf("skipping %s", line);   */
	}
    } /* end while (getline(line, MAX_SEGS_LINE, ... */

 /*
 printf("total_segs = %d\n", total_segs);
 for(iseg = 0; iseg < total_segs; iseg++)
 printf("  %s\n", segment_names[iseg]);
 */

/*
 * Now have total_segs in this forecast group, and
 *  the segment names in segment_names[].
 *
 * Cycle through each segment in the forecast group and see
 *  if there are any run-time Mods for that segment in the
 *  file of Mods for the forecast group (the mods_input_file).
 *
 * Create a file of Mods for each segment in the forecast group
 *  (mods_out_file).
 * Note that these files can be empty if there are no
 *  Mods in effect for a given segment.
 */



  strcpy(fgmods_output_file_name, getenv("HOME"));
  strcat(fgmods_output_file_name, "/.ifp_files/mods_from_ofs/FG_RANGE_");
  strcat(fgmods_output_file_name, fgroup_id);
  fgmods_output_file = fopen(fgmods_output_file_name, "w");

  if ( fgmods_output_file == NULL ) {
  	printf("Error in open file %s for writing.\n",fgmods_output_file_name);
        exit(0);
  }


  for(iseg = 0; iseg < total_segs; iseg++)
    {
     /* --AV added this check for linux --*/
     if(mods_input_file != NULL)
         rewind(mods_input_file);

     strcpy(mods_output_file_name, getenv("HOME"));
     strcat(mods_output_file_name, "/.ifp_files/mods_from_ofs/");
     strcat(mods_output_file_name, segment_names[iseg]);
     mods_output_file = fopen(mods_output_file_name, "w");

     str_chk = strlen(segment_names[iseg]);


     /*AV added 2/7/02 */
     if (mods_output_file == NULL ){
  	printf("Error in open file %s for writing.\n",mods_output_file_name);
        exit(0);
     }

     /*end 2/7/02 */


     keep = continuation_expected = command_line_copied = FALSE;
     keep_FR = continuation_expected = command_line_copied = FALSE;
     discarded_mod = FALSE;
/*
 * <<<<<<  For each segment, read through the entire file  >>>>>>
 * <<<<<<   of mods from the ofs for the forecast group    >>>>>>
 */
     while ((len = getline(line, MAX_SEGS_LINE, mods_input_file)) > 0)
	{
		/** Check for Range mods type. Set range flag to true ***/
		memset(tmp_str,'\0',strlen(tmp_str));
		/*kwz.r26-33.copy the whole line, so not mess up line*/
		strcpy(tmp_str,line);
		/*rnage_flag=Is the 1st token of tmp_str has '-' in it?*/
		range_flg = rangeCheck( strtok(tmp_str," "));   /*added to fix bug R23-39 av 7/31 */

		if(line[0] != '$')
		{
	/* cew only drop into this code if not a comment card */

	    /*
	     * Insert an end of line character at location MAX_MODS_LINE
	     *  in character string "line".  This will guarantee that we
	     *  do not decode any characters beyond the allowed length of
	     *  a run-time Mod image.  If a longer line is read from the
	     *  mods_input_file the characters beyond MAX_MODS_LINE will
	     *  not be decoded by function getword.
	     * Change made by G. Smith - HRL - 950303.
	     */

            if(len>MAX_MODS_LINE && iseg == 0)
	     {
	       line[MAX_MODS_LINE - 1] = '\n';

	       printf("   WARNING  \n");
	       printf("The following mod card has been truncated:\n");
	       printf("%-72.72s\n",line);
	       printf("\n");

/* cew added line to reset len after adding \n at position MAX_MODS_LINE */
	       len = MAX_MODS_LINE;
             }
	    start_loc = loc_next = 0;

	    loc_next = getword(line, start_loc, MAX_MODS_LINE, word);


/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */

	   /*
	    * This if clause added to allow blank lines
	    *  to be used in a run-time mods file.
	    * The blank lines are ignored.
	    * Change made by G. Smith - HRL - 950303
	    */
	    if(strcmp(word, "") == 0)		/* This is a blank line.   */
	      {
	       continue;
	      }

/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */

            else if(continuation_expected == TRUE)	/* This is a continuation of a */
	       						/*  line we want to keep.      */
 	      {
	       /*
		* Only copy this continuation line to the mods_output_file if
		*  the previous subsequent line was valid for the current segment.
		*/

		if(keep == TRUE && discarded_mod == FALSE)
		   fprintf(mods_output_file, "%s", line);
		if(keep_FR == TRUE && iseg == 0){
                   fprintf(fgmods_output_file, "%s", line);
                }

	       /*
	        * Whether or the current line is valid for this segment, we must check
		*  if the current line (which is continued from a previous line)
		*  is itself expecting a continuation leave continuation_expected
		*  set to TRUE.  Otherwise set continuation_expected to FALSE.
		*
		* If the current line is a comment leave continuation_expected
		*  as it is currently set.
		*/
	       if(word[0] != '$')
	       {	/* Current line is not a comment line. */
	          continuation_expected = is_mod_continuation(line, len);
	        }
	      } /* end if(continuation_expected == TRUE ... */

/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */

	    else	/* continuation_expected == FALSE */
	      {

	/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

	       if(word[0] == '$')
 		 /* This if clause added to allow comment lines
		  *  to be used in a run-time mods file.
		  * The comment lines are ignored.
		  * Change made by G. Smith - HRL - 950301
	          */
		 {               /* this is a comment line - skip */
		  continue;
		 }

	/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

	       else if(word[0] == '.')
	       {        /* see if this word is a mod command */

		 discarded_mod = FALSE;
                 if (strlen(word)-1 > 8)
                    {
                    printf(" ************  ERROR  ERROR  ERROR  ************\n");
                    printf("An invalid mod command is detected when loading the\n");
                    printf("mods files.  Please correct the error and try again.\n");
                    printf("%s\n",word);
                    printf(" ************  ERROR  ERROR  ERROR  ************\n");
                    exit(1);
                    }
		  word_ptr = word;
		  memset(cmdnam, '\0', 9);
		  strncpy(cmdnam, ++word_ptr, strlen(word)-1);

/*  pad with blanks to 8 characters so MCOMND makes proper comparison */

		  memset(padded_cmdnam, '\0', 9);
		  strcpy(padded_cmdnam, cmdnam);
		  if((i = strlen(padded_cmdnam)) < 8)
		    while(i < 8)
		      padded_cmdnam[i++] = ' ';

		  icmnd = -1;
		  MCOMND(&icmnd, padded_cmdnam, &isfg, &ndts);

		  if(icmnd > 0)     /* it is a command line */
		  {
		   /*
		    * Don't immediately copy the command line to the
		    *  mods_output_file because we don't yet know if
		    *  there will be any subsequent mod lines that apply
		    *  to the current segment.  Store command line in string
		    *  hold_command_line.
		    */
		     strcpy(hold_command_line, line);
		     command_line_copied = FALSE;
		   /*
		    * see if there is a field 'FG' or 'FGROUP' on the card
		    */
		     fgroup_on_command_line = FALSE;

		     while (loc_next < len && loc_next != EOF){

		        start_loc = loc_next;
		        loc_next = getword(line, start_loc, MAX_MODS_LINE, word);
		        if(strcmp(word, "FGROUP") == 0 ||
			   strcmp(word, "FG") == 0 ){
			     fgroup_on_command_line = TRUE;
                        }
		     } /* end while (loc_next < len ... */
		         /*
		          * Have copied command line into hold array
			  *  and determined whether or not the field FGROUP or FG
			  *  appears on the command line.
			  * Go on to the next line.
			  */
		  } /* end if(icmnd > 0) */
		  else{            /* invalid command - discard */
		     discarded_mod = TRUE;
		     if(iseg == 0){         /* only print message once */
		        printf("The following line contains an invalid mod command.  ");
		        printf("It will be discarded.\n%s", line);
		     }
		     /*
		      * Invalid mod command line -
		      *  make sure fgroup_on_command_line is set to FALSE
		      */
		     fgroup_on_command_line = FALSE;
		  } /* end else (not a mod command) */
		} /* end else if(word[0] == '.') */

	       /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

	       /*
	        * So the current line is not a
		*  1) blank,
		*  2) continuation,
		*  3) comment, or
		*  4) mod command.
		*
		* Now see if it is a subsequent line to be applied to the current segment.
	        */

		   else if ((fgroup_on_command_line == FALSE &&
			  current_segment_on(segment_names, total_segs, iseg, word) == TRUE)
			 ||
		         (fgroup_on_command_line == TRUE &&
			  current_segment_on(fgroup_id, 1, 0, word) == TRUE)
			 || range_flg == 1){	   /* it is a subsequent card for */
			 	   /*  the current segment        */
		         /*printf("1st card %s 2nd card %s \n",hold_command_line,line);*/
			 /* write to FGROUP file */

			  if((fgroup_on_command_line == TRUE || range_flg == 1) &&
                                      iseg == 0) {
			        fprintf(fgmods_output_file, "%s", hold_command_line);

                          }

			 /*end */
	 		 if(command_line_copied == FALSE)
			 {

                           /*
			    * If this is the first valid subsequent mod line for this
			    *  mod command for this segment, write the mod command
			    *  line to to the mods_output_file.
			    */
			    if(fgroup_on_command_line == FALSE && range_flg == 0 && discarded_mod == FALSE)
			    {
			      fprintf(mods_output_file, "%s", hold_command_line);
			      command_line_copied = TRUE;
			    }
			 } /* end if(command_line_copied ... */

			/*
			 * Write the subsequent mod line to the mods_output_file.
			 */
			 if(fgroup_on_command_line == FALSE && range_flg == 0 && discarded_mod == FALSE)
			 {
                             fprintf(mods_output_file, "%s", line);
			     keep = TRUE;
			 }
			 /* Write the subsequent mod line to the fgmods_output_file */
			 /* (.ifp_files/mods_from_ofs/FG_RANGE_fgroupid ).          */
			 else /* write to FGROUP/RANGE file */
			 {
                            if(iseg == 0){

                                fprintf(fgmods_output_file, "%s", line);
			        /*keep_FR = TRUE;*/
                            }
			 }

			 /*
			 * See if this subsequent line expects a continuation.
			 */
			 continuation_expected = is_mod_continuation(line, len);
                        /* printf("parse_mods_by_segment:--continuation_expected =%d\n",continuation_expected);
		         printf("parse_mods_by_segment:--line=%s\n",line);*/
                   } /* end else if ((fgroup_on_command_line ... */


	/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

	       else
		 {
		    /*
		     * Subsequent line does not apply to the current segment.
		     * Do not copy, and set keep to FALSE.
		     */
		  keep = FALSE;
		  keep_FR = FALSE;
		    /*
	 	     * See if this subsequent line expects a continuation.
		     */
		  continuation_expected = is_mod_continuation(line, len);
		 }

	/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

	    } /* end else	/# continuation_expected == FALSE #/ */

/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */

         }/* cew end of if statement right after while to skip comment cards */

	} /* end while ((len = getline( ... */


     if(mods_output_file != NULL )fclose(mods_output_file);


    } /* end for(iseg = 0; iseg < total_segs; iseg++) */

  if(fgmods_output_file != NULL) fclose(fgmods_output_file);
  if(save_FgNamefp != NULL) fclose(save_FgNamefp);
/* -- jgg added the following fcloses, which seemed to be missing  */

  if(segs_input_file != NULL) fclose(segs_input_file);
  if(mods_input_file != NULL) fclose(mods_input_file);

  exit(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/parse_mods_by_segment/RCS/parse_mods_by_segment.c,v $";
 static char rcs_id2[] = "$Id: parse_mods_by_segment.c,v 1.11 2006/04/25 14:19:32 wkwock Exp $";}
/*  ===================================================  */

}

/*
/* this function checks for a valid range mod.
/* returns 1 if it is, else
/* returns 0 -- AV added for fixing bug R23-39
*/
int rangeCheck( char *str)
{
    char tbuf[10];
    char *cptr;

    cptr = strrchr(str,'-');
    if(cptr == NULL)
        return 0;
    strcpy(tbuf, cptr+1);
    /*check to see if it is a numeric value */
    if(atof(tbuf))return 0;/* it is not a valid range mod */
    else {
       /*printf("This is a range mod ....\n"); */
       return 1;
    }

}
