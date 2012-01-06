/*
 * new line for testing sccs updates
 *
 *      seg_sort.c
 *
 *              modifies and sorts the Prime forecast group and segment
 *              data into a format usable by the FG_MAP program
 *
 *      if (this_program_works >= TRUE)
 *              #define by  Ram_Singh
 *      else
 *              #define by the_Proteus_Development_Team
 *
 *      #define date 1/17/90
 *
 *      originally written by Ram Singh, January, 1990.
 *      data structure kept, but logic rewritten by George Smith, February, 1990.
 *      modified to check up/downstream connectivity, gfs, August 1991.
 *      modified to allow multiple downstream points, gfs, December 1991.
 *      modified to reverse the order that subtrees (lowermost_segs) are
 *       listed in the seg_sort_out file, gfs, February 1995.
 */

#include        <stdio.h>
#include        <string.h>

#define TRUE            1
#define FALSE           0
#define MAX_NUM_SEG     200             /* the maximum number of segments     */
					/*   in a forecast group.             */

#define MAX_NUM_LOWERMOST_SEGS     30   /* the maximum number of lowermost    */
					/*  segments (i.e., the number of     */
					/*  separate trees) in the forecast   */
					/*  group                             */

#define MAX_NUM_UPSTREAM_SEGS       5   /* the maximum number of segments     */
					/*  that can be upstream of a segment */

#define MAX_NUM_DOWNSTREAM_SEGS     2   /* the maximum number of segments     */
					/*  that can be downstream of a       */
					/*  segment                           */

#define NO_FIELD        "EMPTY"         /* any field with no data should be   */
					/*   filled with 'EMPTY'.             */

char            *getenv();

int             check_total_segs;       /* counter for how many segments in   */
					/*  the s_segment array have been     */
					/*  checked in the check_up_down      */
					/*  function. added 12/16/91 by gfs   */
					/*  as part of multiple tree code     */

/* define a structure that can hold the names of the segments.                */

typedef char  NAME[9];

/* define a structure containing segment information that sort.c can use.     */

typedef struct
	{
	char    location[MAX_NUM_SEG]; /* tree location code                  */

	int     in_f_group;            /* Boolean value specifying if this    */
				       /*   segment is in the forecast group. */

	int     lowermost;             /* Boolean value specifying if this    */
				       /*   segment is the bottom segment,    */
				       /*   possibly not in the forecast group*/

	}       S_INFO;

/* define a structure to hold additional e19 information - gfs - 2/11/91      */

#include "extra_e19.h"

/* define a structure to hold information on each segment                     */

typedef struct
	{
	NAME    identifier;            /* name of the segment                 */

	NAME    f_group;               /* segment's forecast group            */

	NAME    c_group;               /*           carryover group           */

	char    cr_date[21];           /*           creation date             */

	char    descript[21];          /*           description               */

	NAME    upstream[MAX_NUM_UPSTREAM_SEGS];  /* names of upto            */
				       /*   MAX_NUM_UPSTREAM_SEGS segments    */
				       /*   upstream from the present segment */

	NAME    downstream[MAX_NUM_DOWNSTREAM_SEGS];  /* names of upto        */
				       /* MAX_NUM_DOWNSTREAM_SEGS segments    */
				       /* downstream from the present segment */

	S_INFO  tree_info;             /* stores the tree mapping information */
				       /*   of the segment                    */

	extra_e19 *e19;                /* pointer to additional e19 info      */

/*	char    seg_status[8]; */        /* segment flow status - "Unknown",    */
				       /*   "Normal", "Alert", and "Flood"    */
        int NumRC; /*length of e19*/
       }       SEG_INFO;

	int     num_o_seg;                      /* total number of segments   */

	int     sorted_seg;                     /* segment number that the    */
						/*   sorted segment structure */
						/*   is looking at now.       */

	SEG_INFO segment[MAX_NUM_SEG];          /* segments list (unsorted)   */

	SEG_INFO s_segment[MAX_NUM_SEG];        /* sorted segments            */

	int     connectivity_problem;   /* used to see if up/downstream       */
					/*   connectivity is OK               */

/******************************************************************************/
/*                         start of sort_segs function                        */
/*                                                                            */
sort_segs(int seg_num)

{
	int     i,j;

	if(segment[seg_num].tree_info.in_f_group == TRUE ||
	   segment[seg_num].tree_info.lowermost  == TRUE)
		{
		for(i = 0; i < MAX_NUM_UPSTREAM_SEGS; i++)
			{
			if(strcmp(segment[seg_num].upstream[i], NO_FIELD) == 0)
			   i=MAX_NUM_UPSTREAM_SEGS;
			else
			  {
			  for(j = 0; j < num_o_seg; j++)
			     {
			     if(strcmp(segment[seg_num].upstream[i],
				       segment[j].identifier) == 0)
			       {
			       sort_segs(j);
			       j = num_o_seg;
			       }                /*  end if      */
			     }                  /*  end for, j  */
			  }                     /*  end else    */
			}                       /*  end for, i  */
		}                               /*  end if      */

	sorted_seg--;

	strcpy(s_segment[sorted_seg].identifier, segment[seg_num].identifier);
	strcpy(s_segment[sorted_seg].f_group, segment[seg_num].f_group);
	strcpy(s_segment[sorted_seg].c_group, segment[seg_num].c_group);
	strcpy(s_segment[sorted_seg].cr_date, segment[seg_num].cr_date);
	strcpy(s_segment[sorted_seg].descript, segment[seg_num].descript);
	s_segment[sorted_seg].NumRC=segment[seg_num].NumRC ;

	for(i = 0; i < MAX_NUM_UPSTREAM_SEGS; i++)
	strcpy(s_segment[sorted_seg].upstream[i], segment[seg_num].upstream[i]);

	for(i = 0; i < MAX_NUM_DOWNSTREAM_SEGS; i++)
	strcpy(s_segment[sorted_seg].downstream[i], segment[seg_num].downstream[i]);

	s_segment[sorted_seg].tree_info.in_f_group =
				 segment[seg_num].tree_info.in_f_group;
	s_segment[sorted_seg].tree_info.lowermost =
				 segment[seg_num].tree_info.lowermost;

	s_segment[sorted_seg].e19 = segment[seg_num].e19;

/*	strcpy(s_segment[sorted_seg].seg_status, segment[seg_num].seg_status);
*//*comment out by kwz*/
}


/******************************************************************************/
/*                         start of check_up_down function                    */
/*                                                                            */

check_up_down(int seg_num, int down_seg_num)

{
	int     i,j;

	if(s_segment[seg_num].tree_info.in_f_group == TRUE ||
	   s_segment[seg_num].tree_info.lowermost  == TRUE)
		{
		for(i = 0; i < MAX_NUM_UPSTREAM_SEGS; i++)
			{
			if(strcmp(s_segment[seg_num].upstream[i], NO_FIELD) == 0)
			   i=MAX_NUM_UPSTREAM_SEGS;
			else
			  {
			  for(j = 0; j < num_o_seg; j++)
			     {
			     if(strcmp(s_segment[seg_num].upstream[i],
				       s_segment[j].identifier) == 0)
			       {
			       check_up_down(j, seg_num);
			       j = num_o_seg;
			       check_total_segs++;
			       }                /*  end if      */
			     }                  /*  end for, j  */
			  }                     /*  end else    */
			}                       /*  end for, i  */
		}                               /*  end if      */

	if(down_seg_num == -1) return;

	if(strcmp(s_segment[seg_num].downstream[0],
		  s_segment[down_seg_num].identifier) != 0)
	  {
	  printf("Incompatibility in upstream/downstream connectivity\n");
	  printf("  between segments %s and %s\n",
		  s_segment[seg_num].identifier,
		  s_segment[down_seg_num].identifier);
	  connectivity_problem = TRUE;
	  }
}


/******************************************************************************/
/*                         start of store_loc_code function                   */
/*                                                                            */

store_loc_code(int seg_num, char * loc_code)

{
	int     i, j;
	int     num_upstream;                   /* how many segments are      */
						/*   upstream of the current  */
						/*   segment                  */

	char    this_segs_code[MAX_NUM_SEG];

/*  first save location code for this segment                                           */

	strcpy(s_segment[seg_num].tree_info.location, loc_code);

/*  if segment upstream and outside of forecast group, just leave                       */

	if(s_segment[seg_num].tree_info.in_f_group == FALSE &&
	   s_segment[seg_num].tree_info.lowermost  == FALSE)  return;

		num_upstream = MAX_NUM_UPSTREAM_SEGS;
		for(i = 0; i < MAX_NUM_UPSTREAM_SEGS; i++)
			{
			if(strcmp(s_segment[seg_num].upstream[i], NO_FIELD) == 0)
			   {
			   num_upstream = i;
			   i = MAX_NUM_UPSTREAM_SEGS;
			   }
			}

/*  if no segments upstream of this one, just leave                                     */

		if(num_upstream < 1) return;

/*  if there are segments upstream of this one, append proper location to code          */
/*    and call store_loc_code with upstream segment as current seg_num.                 */

		for(i = 0; i < num_upstream; i++)
			{
			strcpy(this_segs_code, loc_code); /*  Reset location code to  */
							  /*  code at function entry  */
			if(num_upstream == 1)
			   {
			   if (i == 0) strcat(this_segs_code, "c");
			   }
			else if(num_upstream == 2)
			   {
			   if (i == 0) strcat(this_segs_code, "b");
			   if (i == 1) strcat(this_segs_code, "d");
			   }
			else if(num_upstream == 3)
			   {
			   if (i == 0) strcat(this_segs_code, "a");
			   if (i == 1) strcat(this_segs_code, "c");
			   if (i == 2) strcat(this_segs_code, "e");
			   }
			else if(num_upstream == 4)
			   {
			   if (i == 0) strcat(this_segs_code, "a");
			   if (i == 1) strcat(this_segs_code, "b");
			   if (i == 2) strcat(this_segs_code, "d");
			   if (i == 3) strcat(this_segs_code, "e");
			   }
			else if(num_upstream == 5)
			   {
			   if (i == 0) strcat(this_segs_code, "a");
			   if (i == 1) strcat(this_segs_code, "b");
			   if (i == 2) strcat(this_segs_code, "c");
			   if (i == 3) strcat(this_segs_code, "d");
			   if (i == 4) strcat(this_segs_code, "e");
			   }
                        else
                           {
                           printf("The number of upstream segments must ");
                           printf("be from 1 to 5.\n");
                           printf("For segment %s the number is %d.\n",
                                  s_segment[seg_num].identifier, num_upstream);
                           }
/*  find location of segment upstream to this one and call store_loc_code       */

			for(j = 0; j < num_o_seg; j++)
			   {
			   if(strcmp(s_segment[seg_num].upstream[i],
				     s_segment[j].identifier) == 0)
			     {
			     store_loc_code(j, this_segs_code);
			     j = num_o_seg;
			     check_total_segs++;
			     }                  /*  end if (strcmp...            */
			   }                    /*  end for, j                   */
			}                       /*  end for, i                   */
}


/*                                                                            */
/******************************************************************************/
/*                         START OF THE MAIN PROGRAM                          */
/*                                                                            */


seg_sort_main ()

{
	int     lowermost_seg[MAX_NUM_LOWERMOST_SEGS];
						/* segment number(s) of the   */
						/*   lowermost segments in    */
						/*   the unsorted segment     */
						/*   structure                */

	int     count1, count2, count3, count4; /* counters                   */

	int     num_lowermost;                  /* number of lowermost segs   */

	int     num_added;                      /* number of segs added to    */
						/*   unsorted seg list because*/
						/*   they are upstream segs   */
						/*   not in forecast group    */

	int     add;                            /* Boolean tests              */


	char    HOME_directory[80];             /* array to hold the name of  */
						/*   the HOME directory       */
	FILE *input_pointer, *output_pointer;   /* pointers to the input and  */
						/*   output files             */

	char    location_code[MAX_NUM_SEG];     /* array to hold location code*/

	extra_e19 *empty_e19;                   /* pointer to empty e19 info  */
	int empty_e19_length ;

        int    data_passes_all_tests;           /* flag to check connectivity */
                                                /* added by gfs - 4/7/94      */
/* make an empty_e19 structure to use for any added segments not in
 *   the forecast group
 */
	empty_e19 = (extra_e19*)malloc(sizeof(extra_e19));
	strcpy(empty_e19->river_name, NO_FIELD);
	strcpy(empty_e19->station_name, NO_FIELD);
        empty_e19->latitude = 0.0;
        empty_e19->longitude = 0.0;
	strcpy(empty_e19->forecast_point_type, NO_FIELD);
        empty_e19->total_area = 0.0;
        empty_e19->local_area = 0.0;
        empty_e19->flood_stage = 0.0;
        empty_e19->flood_flow = 0.0;
        empty_e19->secondary_stage = 0.0;
        empty_e19->warning_stage = 0.0;
        empty_e19->warning_flow = 0.0;
        empty_e19->gage_zero = 0.0;
        empty_e19->record_stage = 0.0;
        empty_e19->record_flow = 0.0;
        empty_e19->date_of_record = 0;
	strcpy(empty_e19->record_flood_comment, NO_FIELD);
        empty_e19->rating_curve_limit = 0.0;
/*	strcpy(empty_e19->f_group, NO_FIELD);
	strcpy(empty_e19->c_group, NO_FIELD);
	for(count1 = 0; count1 < 5; count1++)
            strcpy(empty_e19->upstream[count1], NO_FIELD);
	for(count1 = 0; count1 < 2; count1++)
	    strcpy(empty_e19->downstream[count1], NO_FIELD);
*//*This 4 variables are not in e19 any more---kwz*/

/* initialize counters */

	num_o_seg = 0;

/*----------------------------------------------------------------------------*/
/* set up pointers to access forecast group data files as specified by        */
/*   Kernighan and Ritchie (p 160).                                           */
/* specify the files to read/write from/to.                                   */

	strcpy(HOME_directory, getenv("HOME"));
	input_pointer =
	    fopen (strcat(HOME_directory ,
		   "/.ifp_files/local/seg_sort.in"), "r");

	strcpy(HOME_directory, getenv("HOME"));
	output_pointer =
	    fopen (strcat(HOME_directory,
		   "/.ifp_files/local/e19.data"), "w");

/*----------------------------------------------------------------------------*/
/* while (there are still entries in the input file)...                       */

while (fscanf(input_pointer, "%s %s %s %[^~]~ %[^~]~ %s %s %s %s %s %s %s %d",
		segment[num_o_seg].identifier,
		segment[num_o_seg].f_group,
		segment[num_o_seg].c_group,
		segment[num_o_seg].cr_date,
		segment[num_o_seg].descript,
		segment[num_o_seg].upstream[0],
		segment[num_o_seg].upstream[1],
		segment[num_o_seg].upstream[2],
		segment[num_o_seg].upstream[3],
		segment[num_o_seg].upstream[4],
		segment[num_o_seg].downstream[0],
		segment[num_o_seg].downstream[1],
		&segment[num_o_seg].NumRC
	    ) != EOF )

	{
/* create space for e19 info in segment arrays          */
segment[num_o_seg].e19 = (extra_e19*)malloc(sizeof(extra_e19)*segment[num_o_seg].NumRC);

  for (count1=0;count1<segment[num_o_seg].NumRC;count1++)
  { fscanf(input_pointer,"%8s %[^~]~ %[^~]~ %f %f %[^~]~ \
    %f %f %f %f %f %f %f %f %f %f %d %[^~]~ %f %s",
	segment[num_o_seg].e19[count1].RCName,
	segment[num_o_seg].e19[count1].river_name,
	segment[num_o_seg].e19[count1].station_name,
	&segment[num_o_seg].e19[count1].latitude,
	&segment[num_o_seg].e19[count1].longitude,
	segment[num_o_seg].e19[count1].forecast_point_type,
	&segment[num_o_seg].e19[count1].total_area,
	&segment[num_o_seg].e19[count1].local_area,
	&segment[num_o_seg].e19[count1].flood_stage,
	&segment[num_o_seg].e19[count1].flood_flow,
	&segment[num_o_seg].e19[count1].secondary_stage,
	&segment[num_o_seg].e19[count1].warning_stage,
	&segment[num_o_seg].e19[count1].warning_flow,
	&segment[num_o_seg].e19[count1].gage_zero,
	&segment[num_o_seg].e19[count1].record_stage,
	&segment[num_o_seg].e19[count1].record_flow,
	&segment[num_o_seg].e19[count1].date_of_record,
	segment[num_o_seg].e19[count1].record_flood_comment,
	&segment[num_o_seg].e19[count1].rating_curve_limit,
	segment[num_o_seg].e19[count1].seg_status) ;
  }/*end of for count1 loop*/

	/* check to see that there are not two segments downstream of the     */
	/* segment just specified                                             */
	/* if there is a problem, alter..er..I mean alert the user and boot   */
	/* him (or her) off                                                   */
	/* increment the counter to allow for more segments to be read in     */

	/* if string compare equals 0, the downstream[1] field = "EMPTY"      */

	if (strcmp (segment[num_o_seg].downstream[1],NO_FIELD) != 0)

		{
		printf ("Segment %s is not ",segment[num_o_seg].identifier);
		printf ("supposed to have two segments downstream from it\n");
		printf ("\nThe two segments are:\n");
		printf ("  %s\n",segment[num_o_seg].downstream[0]);
		printf ("  %s\n",segment[num_o_seg].downstream[1]);
		printf ("\nFix the data and rerun.\n");
		exit(1);

		}       /* end if       */

	segment[num_o_seg].tree_info.in_f_group = TRUE;
	segment[num_o_seg].tree_info.lowermost  = TRUE;

	++num_o_seg;

	} /* end while  */

/*----------------------------------------------------------------------------*/
/* Start a series of checks to see if segments all agree about who is
 *  upstream/downstream of each other, that the network forms trees,
 *  not loops, that entries are not duplicated, etc.
 * If the input data fails any of these checks exit with a status of 1.
 */
data_passes_all_tests = TRUE;
/*
 * Go through segment by segment and make sure that the segments agree about
 *  who is upstream and downstream from each other.
 * Loop through the segments, for each go through the list of upstream
 *  segments and if the segment is found in the list make sure that its
 *  downstream segment matches the current segment.
 * Multiple downstream segments and other problems will be checked later.
 */
for (count1 = 0; count1 < num_o_seg; count1++)
    {
    for (count2 = 0; count2 < MAX_NUM_UPSTREAM_SEGS; count2++)
	{
	 if (strcmp(segment[count1].upstream[count2],
		    NO_FIELD) == 0) break;
	 else
	    {
	     for (count3 = 0; count3 < num_o_seg; count3++)
		 {
		  if (strcmp(segment[count1].upstream[count2],
		      segment[count3].identifier) == 0)
		     {  /* have found segment listed as upstream of current   */
		      if(strcmp(segment[count3].downstream[0],
				segment[count1].identifier) != 0)
			{
			 printf("Segments %s and %s do not agree about ",
				segment[count1].identifier,
				segment[count3].identifier);
			 printf("upstream/downstream connectivity.\n");
                         data_passes_all_tests = FALSE;
			}
		      else              /* segments agree about connectivity */
			 break;
		     }
		  }    /* end for (count3) */
	    }
	}              /* end for (count2) */
    }                  /* end for (count1) */
/*
 * Now go through the segments and make sure that the downstream segment
 *  listed for each segment agrees with the upstream segment list for
 *  the named segment.  If you can follow the last sentence go into
 *  politics.
 */
for (count1 = 0; count1 < num_o_seg; count1++)
   {
    if (strcmp(segment[count1].downstream[0],
	       NO_FIELD) == 0) continue; /* if downstream EMPTY
					    go to next segment  */
    else
       {
	for (count3 = 0; count3 < num_o_seg; count3++)
	    {
	     if (strcmp(segment[count1].downstream[0],
		 segment[count3].identifier) == 0)
		{   /* have found segment listed as downstream of current */
		 connectivity_problem = TRUE;
		 for (count2 = 0; count2 < MAX_NUM_UPSTREAM_SEGS; count2++)
		     { /* go through all upstream segments of one just found */
		      if(strcmp(segment[count3].upstream[count2],
				segment[count1].identifier) == 0)
			{            /* segments agree about connectivity */
			 connectivity_problem = FALSE;
			 break;
			}
		      else if (strcmp(segment[count3].upstream[count2],
				      NO_FIELD) == 0)
			{
			 break;
			}
		     } /* end for (count2) */
		 if(connectivity_problem == TRUE)
		   {
		    printf("Segments %s and %s do not agree about ",
			   segment[count1].identifier,
			   segment[count3].identifier);
		    printf("downstream/upstream connectivity.\n");
                    data_passes_all_tests = FALSE;
		    break;
		   }
		}
	     }         /* end for (count3) */
       }
    }                  /* end for (count1) */
/*
 * Now go through segments once more to see if a segment is listed
 *  as upstream or downstream of itself.
 * Added by gfs, 12/16/91.
 */
for (count1 = 0; count1 < num_o_seg; count1++)
    {
    for (count2 = 0; count2 < MAX_NUM_UPSTREAM_SEGS; count2++)
	{
	 if (strcmp(segment[count1].upstream[count2],
		    NO_FIELD) == 0) break;
	 else
	    {
	     if (strcmp(segment[count1].upstream[count2],
			segment[count1].identifier) == 0)
		{
		 printf("Segment %s says it is upstream of itself\n",
			segment[count1].identifier);
                 data_passes_all_tests = FALSE;
		 break;
		}
	    }
	}              /* end for (count2) */
     if (strcmp(segment[count1].downstream[0],
		segment[count1].identifier) == 0)
	{
	 printf("Segment %s says it is downstream of itself\n",
		segment[count1].identifier);
         data_passes_all_tests = FALSE;
	 break;
	}
    }                  /* end for (count1) */

/*
 * Go through segments once more to see if an
 *  upstream segment is listed more than once.
 * Added by gfs, 4/7/93.
 */
for (count1 = 0; count1 < num_o_seg; count1++)
    {
    for (count2 = 0; count2 < MAX_NUM_UPSTREAM_SEGS; count2++)
	{
	 if (strcmp(segment[count1].upstream[count2],
		    NO_FIELD) == 0) break;
	 else
	    {
	     for (count3 = 0; count3 < num_o_seg; count3++)
		 {
		 for (count4 = 0; count4 < MAX_NUM_UPSTREAM_SEGS; count4++)
		     {
		     if (strcmp(segment[count2].upstream[count4],
				 NO_FIELD) == 0)
			{
			count4 = MAX_NUM_UPSTREAM_SEGS;
			}
		     else if ((count1 == count3) && (count2 == count4))
			{
			 ; /* do not compare if looking at same */
			   /*  upstream segment                 */
			}
		     else
			{
			if (strcmp(segment[count1].upstream[count2],
				   segment[count3].upstream[count4]) == 0)
			   {
			    printf("Segment %s appears in the upstream list",
				   segment[count1].upstream[count2]);
			    printf(" of segments %s and %s\n",
				   segment[count1].identifier,
				   segment[count3].identifier);
			    printf("Please fix this connectivity problem\n");
                            data_passes_all_tests = FALSE;
			    break;
			   }     /* end if (strcmp(segment[count1]... */
			}        /* end else                          */
		     }           /* end for (count4 = 0...            */
		 }               /* end for (count3 = 0...            */
	    }                    /* end else                          */
	}                        /* end for (count2)                  */
    }                            /* end for (count1)                  */
/*
 * end of 4/7/93 addition by gfs.
 */
/*
 * See if, for any segment, a segment id appears
 *  more than once in its list of upstream segments.
 * Added by gfs, 4/7/94.
 */
for (count1 = 0; count1 < num_o_seg; count1++)
    {
    for (count2 = 0; count2 < MAX_NUM_UPSTREAM_SEGS - 1; count2++)
        {
         if (strcmp(segment[count1].upstream[count2],
                    NO_FIELD) == 0) break;
         else
            {
             for (count3 = count2 + 1; count3 < MAX_NUM_UPSTREAM_SEGS; count3++)
                 {
                  if (strcmp(segment[count1].upstream[count3],
                                 NO_FIELD) == 0)
                     {
                      count3 = MAX_NUM_UPSTREAM_SEGS;
                     }
                  else
                     {
                      if (strcmp(segment[count1].upstream[count2],
                                 segment[count1].upstream[count3]) == 0)
                        {
                         printf("Segment %s appears more that once in the ",
                                 segment[count1].upstream[count2]);
                         printf("upstream segment list of %s\n",
                                 segment[count1].identifier);
                         printf("Please remove duplicate entries of %s\n",
                                 segment[count1].upstream[count2]);
                         data_passes_all_tests = FALSE;
                         break;
                        }     /* end if (strcmp(segment[count1]... */
                     }        /* end else                          */
                 }            /* end for (count3 = count2...       */
            }                 /* end else                          */
        }                     /* end for (count2 = 0...            */
    }                         /* end for (count1 = 0...            */

if(!data_passes_all_tests)
   exit(1);
/*
 * end of 4/7/94 addition by gfs.
 */
/*----------------------------------------------------------------------------*/
/* Now that all the data is read in, determine which segment is the lowermost */
/* If there is no lowermost segment, that means someone made a mistake        */
/* when entering in the data set and created a "looping forecast group"       */
/* If there are more than one "lowest segment", this, too, is a problem       */
/* This routine will detect such an occurances, provide a description of the  */
/* problem, and stubbornly refuse to continue..at least until it's been       */
/* corrected, that is.                                                        */
/* compare downstream segment field of each segment's structure to that of a  */
/*   list of  all the segments.  If all the downstream segments are either    */
/*   not a part of the current forecast group or are filled with NO_FIELD     */
/*   (i.e. being empty), it is the most downstream segment of the forecast    */
/*   group.                                                                   */

for (count1 = 0; count1 < num_o_seg; count1++)
	{
	for (count2 = 0; count2 < num_o_seg; count2++)
		{

		/* if the downstream field of a segment equals one of the     */
		/*   segment list's entry, then move onto the next segment    */

		if (strcmp(segment[count1].downstream[0],
			segment[count2].identifier) == 0)

			{
			segment[count1].tree_info.lowermost = FALSE;
			count2 = num_o_seg;
			}       /* end if       */

		 }      /* end for (count2...   */
	}               /* end for (count1...   */

	/*--------                                                    --------*/
	/* Count how many lowermost segments are in the forecast group.       */
	/* Fill lowermost_seg with the location(s) of the lowermost           */
	/*  segment(s) in the unsorted segment structure.                     */

	num_lowermost = 0;
	/*
	 * Reverse order that lowermost segment numbers are entered into
	 *  the lowermost_seg array so that the subtrees within a forecast
	 *  group are processed in the same order in the ifp as the order
	 *  the lowermost segments are specified in an OFS forecast group
	 *  definition.  Previously, the subtrees were in inverse order to
	 *  that entered in the OFS.
	 * Changed by G. Smith - HRL - 950228
	 * The original line follows:
	 *
	 *  for (count1 = 0; count1 < num_o_seg; count1++)
	 *
	 * The single new line for this change immediately follows
	 *  these comments.
	 */
	for (count1 = num_o_seg - 1; count1 >= 0; count1--)
		{
		if(segment[count1].tree_info.lowermost == TRUE)
		   {
		   if(num_lowermost < MAX_NUM_LOWERMOST_SEGS)
		     {
		      lowermost_seg[num_lowermost] = count1;
		      num_lowermost++;
		     }
		   else
		     {
		      printf("More than %d lowermost segments (i.e., ",
					MAX_NUM_LOWERMOST_SEGS);
		      printf("separate trees) in this forecast group.\n");
		      printf("Only the first %d trees will be processed\n",
					MAX_NUM_LOWERMOST_SEGS);
		      break;
		     }
		   }
		}

/*--------                                                            --------*/
/* check to make sure that a lowermost segment was found.  If one wasn't,     */
/*   there is a problem with the way the data set was specified.              */

if (num_lowermost == 0)
	{
	printf("\nNo lowermost segment was found.\n");
	printf("The data are listed in such a way that there\n");
	printf("is a loop within the forecast group,\n");
	printf("resulting in no ");
	printf("segment at the extreme downstream of the\n");
	printf("forecast group.  Please fix the data.\n");
	exit(1);
	}       /* end if       */

/*----------------------------------------------------------------------------*/
/* check to be sure that lowermost segment(s) do(es) not appear as an         */
/*  upstream segment to any other segments                                    */

for (count3 = 0; count3 < num_lowermost; count3++)
 {
  for (count1 = 0; count1 < num_o_seg; count1++)
      {
      for (count2 = 0; count2 < MAX_NUM_UPSTREAM_SEGS; count2++)
	  {
	  if (strcmp(segment[count1].upstream[count2], NO_FIELD) == 0)
	    count2 = MAX_NUM_UPSTREAM_SEGS;
	  else
	    {
	    if (strcmp(segment[count1].upstream[count2],
		       segment[lowermost_seg[count3]].identifier) == 0)
		{
		printf("Segment %s does not have any downstream segments listed\n",
			segment[lowermost_seg[count3]].identifier);
		printf("but segment %s lists it as an upstream segment.\n",
		       segment[count1].identifier);
		printf("Please fix this connectivity problem.\n");
		exit(1);
		}           /*  end if                          */
	    }               /*  end else                        */
	  }                 /*  end for(count2...               */
      }                     /*  end for(count1...               */
 }                          /*  end for(count3...               */
/*----------------------------------------------------------------------------*/
/* Now add the segment downstream of the lowermost segment(s), if any.        */
/* Mark it (them) as not in the forecast group, but will display in FG_Map.   */

num_added = 0;

for (count2 = 0; count2 < num_lowermost; count2++)
  {
   if (strcmp(segment[lowermost_seg[count2]].downstream[0], NO_FIELD) != 0)
     {

   /* There is a segment downstream of the lowermost is the forecast group. */
   /* Add it to the segment list, and reset the lowermost_seg pointer.      */

      strcpy(segment[num_o_seg + num_added].identifier,
	     segment[lowermost_seg[count2]].downstream[0]);
      strcpy(segment[num_o_seg + num_added].f_group, NO_FIELD);
      strcpy(segment[num_o_seg + num_added].c_group, NO_FIELD);
      strcpy(segment[num_o_seg + num_added].cr_date, NO_FIELD);
      strcpy(segment[num_o_seg + num_added].descript, NO_FIELD);
      strcpy(segment[num_o_seg + num_added].upstream[0],
	     segment[lowermost_seg[count2]].identifier);
      for (count1 = 1; count1 < MAX_NUM_UPSTREAM_SEGS; count1++)
	   strcpy(segment[num_o_seg + num_added].upstream[count1], NO_FIELD);
      for (count1 = 0; count1 < MAX_NUM_DOWNSTREAM_SEGS; count1++)
	   strcpy(segment[num_o_seg + num_added].downstream[count1], NO_FIELD);
      segment[num_o_seg + num_added].tree_info.in_f_group = FALSE;
      segment[num_o_seg + num_added].tree_info.lowermost  = TRUE;
      segment[num_o_seg + num_added].e19 = empty_e19;
/*      strcpy(segment[num_o_seg + num_added].seg_status, NO_FIELD);
*//*comment out ---kwz*/
      segment[lowermost_seg[count2]].tree_info.lowermost = FALSE;  /* Reset lowermost */
      lowermost_seg[count2] = num_o_seg + num_added;               /* segment pointer */
      num_added++;
     }       /* end if(strcmp...                */
  }          /* end for(count2...               */

/*----------------------------------------------------------------------------*/
/* Now add any upstream segments that are referenced in the segments read in  */
/*   but not in the forecast group (i.e., that do not match any segment names */
/*   read in).  Be sure to mark them not in the f_group.                      */

for (count1 = 0; count1 < num_o_seg; count1++)
	{
	for (count2 = 0; count2 < MAX_NUM_UPSTREAM_SEGS; count2++)
	   {
	   if(strcmp(segment[count1].upstream[count2], NO_FIELD) == 0)
		count2 = MAX_NUM_UPSTREAM_SEGS;
	   else
		{
		for (count3 = 0; count3 < num_o_seg; count3++)
			{
			add = TRUE;
			if (strcmp(segment[count1].upstream[count2],
				   segment[count3].identifier) == 0)
			    {
			    add = FALSE;
			    count3 = num_o_seg;
			    }
			 }
		 if (add == TRUE)
		    {
		    for (count3 = 0; count3 < num_lowermost; count3++)
		      {
		       if (strcmp(segment[count1].upstream[count2],
				  segment[lowermost_seg[count3]].identifier) == 0)
			   {
			   printf("Warning in segment connectivity.\n");
			   printf("Segment %s is not in the forecast group,\n",
				  segment[lowermost_seg[count3]].identifier);
			   printf("but has been added as a lowermost segment.\n");
			   printf("It is also listed as upstream of %s\n",
				  segment[count1].identifier);
			   printf("For the purpose of display on the topology schematic\n");
			   printf(" the linkage between %s and %s will be ignored\n",
				  segment[lowermost_seg[count3]].identifier,
				  segment[count1].identifier);

			   add = FALSE;

			   if (count2 < MAX_NUM_UPSTREAM_SEGS)
			      {
			      for (count4 = count2+1; count4 < MAX_NUM_UPSTREAM_SEGS; count4++)
				  {
				  strcpy(segment[count1].upstream[count4-1],
					 segment[count1].upstream[count4]);
				  }
			      }
			    strcpy(segment[count1].upstream[MAX_NUM_UPSTREAM_SEGS - 1], NO_FIELD);
			   }
/*
 *                      if (num_added > 0)
 *                         {
 *  These lines commented  for (count3 = 0; count3 < num_added; count3++)
 *  by gfs 4/7/93 when         {
 *  check to be sure that      if (strcmp(segment[count1].upstream[count2],
 *  an upstream segment                   segment[num_o_seg + count3].identifier) == 0)
 *  did not appear more           {
 *  than once was added           printf("Segment %s is listed as an upstream segment more ",
 *  to code above.                        segment[num_o_seg + count3].identifier);
 *  This check is now redundant.  printf("than once.\nFix this connectivity problem and rerun.\n");
 *                                exit(1);
 *                                }   .* end if (strcmp(segment... *.
 *                             }      .* end for (count3 = 0...    *.
 *                         }          .* end if (num_added > 0)    *.
 */
		      }               /*  end for(count3...           */

		    if (add == TRUE)
		       {
		       strcpy (segment[num_o_seg + num_added].identifier,
			       segment[count1].upstream[count2]);
		       strcpy(segment[num_o_seg + num_added].f_group, NO_FIELD);
		       strcpy(segment[num_o_seg + num_added].c_group, NO_FIELD);
		       strcpy(segment[num_o_seg + num_added].cr_date, NO_FIELD);
		       strcpy(segment[num_o_seg + num_added].descript, NO_FIELD);
		       for (count3 = 0; count3 < MAX_NUM_UPSTREAM_SEGS; count3++)
			  strcpy(segment[num_o_seg + num_added].upstream[count3], NO_FIELD);
		       strcpy(segment[num_o_seg + num_added].downstream[0],
			      segment[count1].identifier);
		       strcpy(segment[num_o_seg + num_added].downstream[1], NO_FIELD);
		       segment[num_o_seg + num_added].tree_info.in_f_group = FALSE;
		       segment[num_o_seg + num_added].tree_info.lowermost  = FALSE;
		       segment[num_o_seg + num_added].e19 = empty_e19;
/*		       strcpy(segment[num_o_seg + num_added].seg_status, NO_FIELD);
*//*comment out ---kwz*/
		       num_added++;
		       }        /*  end if(add == TRUE...       */
		    }           /*  end if(add == TRUE...       */
		}
	   }
	}
num_o_seg = num_o_seg + num_added;


/*----------------------------------------------------------------------------*/
/* Once we have found the lowermost segment, work upstream, assigning Tom's   */
/*   coding scheme to each segment, until every segment has it's tree         */
/*   position specified.                                                      */
/*                                                                            */
/* In addition, check for yet another type of data error, in which a          */
/*   downstream segment specifies an incorrect upstream segment, or           */
/*   vice-a-versa.                                                            */
/*                                                                            */
/* Lastly, any segment that is not a part of the forecast group currently     */
/*   being studied will be coded as such                                      */

/*----- fill s_segment array                                                  */
/* if more than one tree (lowermost_seg) fill each tree into s_segment        */
/*  with a separate call to sort_segs.  gfs, 12/16/91                         */

sorted_seg = num_o_seg;
for(count1 = 0; count1 < num_lowermost; count1++)
    sort_segs(lowermost_seg[count1]);

/*----- check for upstream/downstream consistency                             */

/* segments should now be sorted with downstream most segment in s_segment[0]   */
/*   and upstream segments in s_segment[num_o_seg], s_segment[num_o_seg -1] etc */

connectivity_problem = FALSE;
check_total_segs = 0;
for(count1 = 0; count1 < num_lowermost; count1++)
   {
    check_up_down(check_total_segs, -1);
    check_total_segs++;
    if(connectivity_problem == TRUE) exit(1);
   }
/*----- compute and store location code                                       */

check_total_segs = 0;
for(count1 = 0; count1 < num_lowermost; count1++)
   {
    strcpy(location_code, "c");
    store_loc_code(check_total_segs, location_code);
    check_total_segs++;
   }
	/*-----------                                                  ---------*/
	/* now we have to output the whole file to the 'sorted_data' file...    */

  for (count1 = 0; count1 < num_o_seg; count1++)
  {
    fprintf(output_pointer, "%s %d %s~ %s~\n", s_segment[count1].tree_info.location,
	    s_segment[count1].tree_info.in_f_group, s_segment[count1].identifier,
            s_segment[count1].descript);
    fprintf(output_pointer, "%s~ %s~\n   %s~ %s~ %s~ %s~ %s~\n   %s~ %s~\n",
	    s_segment[count1].f_group, s_segment[count1].c_group,
	    s_segment[count1].upstream[0], s_segment[count1].upstream[1],
	    s_segment[count1].upstream[2], s_segment[count1].upstream[3],
	    s_segment[count1].upstream[4], s_segment[count1].downstream[0],
	    s_segment[count1].downstream[1]);
    fprintf(output_pointer, "%d\n",s_segment[count1].NumRC);

    for (count2 = 0; count2 < s_segment[count1].NumRC; count2++)
    {
      fprintf(output_pointer, "   %s %s~ %s~\n   %7.2f %7.2f %s~\n   %8.1f %12.1f\n",
	s_segment[count1].e19[count2].RCName,
	s_segment[count1].e19[count2].river_name,
	s_segment[count1].e19[count2].station_name,
	s_segment[count1].e19[count2].latitude,
	s_segment[count1].e19[count2].longitude,
	s_segment[count1].e19[count2].forecast_point_type,
	s_segment[count1].e19[count2].total_area,
	s_segment[count1].e19[count2].local_area);
      fprintf(output_pointer, "   %7.1f %7.0f %7.1f %7.1f %7.0f %7.1f\n",
	s_segment[count1].e19[count2].flood_stage,
	s_segment[count1].e19[count2].flood_flow,
	s_segment[count1].e19[count2].secondary_stage,
	s_segment[count1].e19[count2].warning_stage,
 	s_segment[count1].e19[count2].warning_flow,
	s_segment[count1].e19[count2].gage_zero);
      fprintf(output_pointer, "   %7.1f %12.0f %d %s~ %7.1f\n%s\n",
	s_segment[count1].e19[count2].record_stage,
	s_segment[count1].e19[count2].record_flow,
	s_segment[count1].e19[count2].date_of_record,
	s_segment[count1].e19[count2].record_flood_comment,
	s_segment[count1].e19[count2].rating_curve_limit,
	s_segment[count1].e19[count2].seg_status);
    }/* end for (count2... */

    /*Free the memory*/

/*
    if ( strcmp(s_segment[count1].f_group, NO_FIELD) != 0 )
        {
          if ( ( (void *)s_segment[count1].e19 ) != NULL )
            free ( (void *)s_segment[count1].e19 );
        }


    if ( strcmp(segment[count1].f_group, NO_FIELD) != 0 )
        {
          if ( ( (void *)segment[count1].e19 ) != NULL )
            free ( (void *)segment[count1].e19 );
        }
*/

  }/* end for (count1... */

  if ( (extra_e19 *)empty_e19 != NULL )
    free ( (extra_e19 *)empty_e19 );

  exit(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/seg_sort/RCS/seg_sort.c,v $";
 static char rcs_id2[] = "$Id: seg_sort.c,v 1.4 2003/03/14 20:05:22 dws Exp $";}
/*  ===================================================  */

}

