/* File: gtd.c
 *
 * Obtains model modification data by switching on the
 * modification number to scan the parameter array for appropriate
 * operations.
 *
 *
 */

#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"
#include "mod_struct.h"
#include "Mods_ofsData_struct.h"
#include "Mods_operMods_struct.h"
#include "c_call_f/mcomnd.h"

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

get_mod_list(

             int     mod_number,        /* modification number */
             float   p_float[],         /* parameter float point data */
             char    *p_char [4],       /* parameter character data */
             float   ts_float[],        /* time series floating data */
             char    *ts_char[4],      /* time series character data */
             mod_oper_struct * mos,     /* pointer to the mod_oper_info for this mod */
             int     *number_in_list,   /* operation and parameter array index pointer */
             char    oper_names_found[][8],    /* operation name */
             char    oper_types_found[][8],    /* operation type */
             int     oper_locp[],       /* location of the beginning of operations parameter array */
             char    ts_id[][8],        /* time series id */
             char    ts_datatype[][8],  /* time series data type array */
             int     ts_delta_t[]       /* sample time interval array */
            )
{
int     mp;                  /* maximum parameter array value */
int     count;               /* counter */

      *number_in_list = 0;

for (count = 0; count < 20; count++)
     {
      oper_names_found[count][0] = (char )NULL;
      oper_types_found[count][0] = (char )NULL;
      oper_locp[count] = 0;
      ts_id[count][0] = (char )NULL;
      ts_datatype[count][0] = (char )NULL;
      ts_delta_t[count] = 0;
     }

mp = 5000;  /* Should replace this with a fortran subroutine */
            /* that returns the value of MP from ofs         */
            /* common /FP/.                                  */
            /* Proposed subroutine:                          */                 
            /*   Subroutine get_ofs_mp(mp_arg)               */
            /*   Integer mp_arg                              */
            /*   INCLUDE 'common/fp'                         */
            /*   mp_arg = MP                                 */
            /*   Return                                      */
            /*   End                                         */
            /* Function call from C program would be:        */
            /*   get_ofs_mp(&mp);                            */

                   
if(mod_number == 19)   /*  SETMSNG Mod                 */
		       /*  Find input time series that */
		       /*  allow missing data.         */
    {
     checkInputTSMsgAllow(ts_float,
			  ts_char,
			  oper_names_found,
			  oper_types_found,
			  oper_locp,
			  ts_id,
			  ts_datatype,
			  ts_delta_t,
			  number_in_list);
    }
else             /*  Scan p array for appropriate operations  */
                 /*  for all other mods.                      */
    { 
     
     checkOperInSegment(mod_number,
                        mos,
                        mp,
                        p_float,
                        p_char,
                        oper_names_found,
                        oper_types_found,
                        oper_locp,
                        ts_delta_t,
                        number_in_list);
    }
}    

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>*/
/*									*/
/*	int     mod_number	* modification number         		*/
/*	float   p_float[]	* floating point parameter data		*/
/*	char    p_char[][4]	* parameter character data		*/
/*	Display	*display	* XtDisplay used for X Window Property	*/
/*				  Change Event				*/
/*									*/
/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>*/

/* ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< */

/* ******************************************************************

	get_the_data()

	get_the_data function takes a mod name as input
	  and returns:
	     1. type and name of any operations in current
		  segment to which the mod could apply,
	     2. the number of dates needed by the mod and
		  default dates to be displayed initially
		  to the forecaster,
	     3. whether the mod can be applied to the
		  entire forecast group,
	     4. the internally used number of the mod
		  (from 1 to MAX_MODS), and
	     5. in the mod_limits_data structure posted to
		  the property bulletin board - the upper
		  and lower error and warning limits for
		  values entered for the mod.

	function originally written by George Smith, HRL, April 1990.

	Modified:	09/08/94 - Tom Adams - Changed arguments & added
				new struct type for encapsulating OFS arrays
	Completely revamped:  941119 - George Smith - now passes operMod struct
	                             to tell which operations are valid for
	                             each mod and then calls checkOperInSegment
	                             function to extract oper names and locations.			

   ****************************************************************** */

mod_data *get_the_data(char *mod_type, 
                       ofsData_struct *info_struct,
                       operMod_struct *oper_mod_struct, 
                       Display *display)
{
	mod_data        *the_data;
	int             icmnd;			/* Number of the mod command			*/
	int             isfg;			/* Flag that shows if a mod can be used by a	*/
						/* forecast group				*/
	int             ndts;			/* Number of dates required for the mod		*/
	int             i,j;			/* Counters					*/
	char            padded_mod_type[9];	/* Mod name padded with blanks */
	int		loc_mod;		/* location of current mod in oper_mod_struct */
	
#define MAX_NUMBER_OF_MODS 40	
	
	the_data = (mod_data *) malloc(sizeof(mod_data));

	/*  get mod number and fg flag from mod type  */
	/*  pad with blanks to 8 characters so MCOMND makes proper comparison */

	memset(padded_mod_type, '\0', 9);
	strcpy(padded_mod_type, mod_type);
	if((i = strlen(padded_mod_type)) < 8)
		while(i < 8) padded_mod_type[i++] = ' ';
    
	icmnd = -1;
	MCOMND(&icmnd, padded_mod_type, &isfg, &ndts);
	if(icmnd > 0) {
    		the_data->mod_number = icmnd;
		the_data->valid_for_fg = isfg;
		}
	else	{
    		the_data->mod_number = 0;
		the_data->valid_for_fg = -1;
		}
		
	/*  find which location in the oper_mod_struct matches */
	/*  the current mod number                             */
	
	loc_mod = 0;
	
	for(i = 0; i < MAX_NUMBER_OF_MODS; i++)
	  {
	   if(oper_mod_struct->mod_numbers[i] == the_data->mod_number)
	     {
	      loc_mod = i;
	      break;
	     }
	  }

	/*  get operation names and types from p array  */

	get_mod_list(the_data->mod_number,
		info_struct->p_float_array,
		(char **)info_struct->p_char_array,
		info_struct->ts_float_array,
		(char **)info_struct->ts_char_array,
		oper_mod_struct->mod_oper_info[loc_mod],
		&(the_data->number_in_list),
		the_data->operation.name,
		the_data->operation.type,
		the_data->operation.locp,
		the_data->time_series.id,
		the_data->time_series.datatype,
		the_data->time_series.delta_t);

	/*  pass other mod info back via return of the_data structure  */

	return(the_data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/gtd.c,v $";
 static char rcs_id2[] = "$Id: gtd.c,v 1.5 2006/04/19 20:30:19 aivo Exp $";}
/*  ===================================================  */

}

