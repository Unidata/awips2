
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include "extra_e19.h"

int gfn(file_names)

char    *file_names[];
{
#define MAX_NUM_SEG     100             /* the maximum number of segments     */
					/*   in a forecast group.             */

static char    segment_names[MAX_NUM_SEG][9];

/* define a structure to hold additional e19 information - gfs - 2/8/91       */


typedef struct _e19
	{
	char            id[50];
	int             in_fgroup;
	char            name[9];
	char            description[21];
	int NumRC; /*number rating curve or forecast points,or length of extra*/
	char   f_group[9]; /*this 4 variables moved from extra_e19.h*/
	char   c_group[9];
	char   upstream[5][9];
	char   downstream[2][9];
	}       e19_data;

  e19_data      e_19;
  extra_e19       extra;

  int     num_o_seg,i;
  char    HOME_directory[80];             /* array to hold the name of   */
					  /*   the HOME directory        */
  char    segment_status[8];              /* status (Unk, Norm, Al, Fld) */
  FILE *input_pointer;                    /* pointer to the input file   */

  strcpy(HOME_directory, (char *)getenv("HOME"));
  input_pointer =
      fopen (strcat(HOME_directory ,"/.ifp_files/local/e19.data"), "r");

  num_o_seg = 0;

/*----------------------------------------------------------------------------*/
/* while (there are still entries in the input file)...                       */

while (fscanf(input_pointer, "%s %d %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %[^~]~ %d",
		e_19.id,
		&e_19.in_fgroup,
		e_19.name,
		e_19.description,
		e_19.f_group,          /* added 2/13/91 - gfs */
		e_19.c_group,          /* added 2/13/91 - gfs */
		e_19.upstream[0],      /* added 2/13/91 - gfs */
		e_19.upstream[1],      /* added 2/13/91 - gfs */
		e_19.upstream[2],      /* added 2/13/91 - gfs */
		e_19.upstream[3],      /* added 2/13/91 - gfs */
		e_19.upstream[4],      /* added 2/13/91 - gfs */
		e_19.downstream[0],    /* added 2/13/91 - gfs */
		e_19.downstream[1],    /* added 2/13/91 - gfs */
		&e_19.NumRC
	    ) != EOF )

	{

for (i=0;i<e_19.NumRC;i++)
{fscanf(input_pointer,"%8s %[^~]~ %[^~]~ %f %f %[^~]~ \
 %f %f %f %f %f %f %f %f %f %f %d %[^~]~ %f %s",
	extra.RCName,
	extra.river_name,
	extra.station_name,
	&extra.latitude,
	&extra.longitude,
	extra.forecast_point_type,
	&extra.total_area,
	&extra.local_area,
	&extra.flood_stage,
	&extra.flood_flow,
	&extra.secondary_stage,
	&extra.warning_stage,
	&extra.warning_flow,
	&extra.gage_zero,
	&extra.record_stage,
	&extra.record_flow,
	&extra.date_of_record,
	extra.record_flood_comment,
	&extra.rating_curve_limit,
	extra.seg_status) ;
}/*end of for count1 loop*/	

	 if(e_19.in_fgroup != 0)        /* Only add segment to list if it's     */
	   {                            /*  in the forecast group.              */
	    if(num_o_seg < MAX_NUM_SEG)
	      {
	       strcpy(segment_names[num_o_seg], e_19.name);
	       file_names[num_o_seg] = segment_names[num_o_seg];
	       num_o_seg++;
	      }
	    else
	      {
	       return(num_o_seg);
	      }
	   }              /* end if(e_19.in_fgroup...   */
	}                 /* end while (fscanf...       */
  return(num_o_seg);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/gfn.c,v $";
 static char rcs_id2[] = "$Id: gfn.c,v 1.4 2006/04/19 20:30:15 aivo Exp $";}
/*  ===================================================  */

}
