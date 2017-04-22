/* File: valid_mods_for_oper.c

   This function scans the p array for a segment and returns
   the mods that could be applied.

    Function originally written by George Smith, HRL, May 1990.
    Modified to use menu_structure to pass active mods flag
	George Smith, HRL, November 1990.

    Modified to return mods valid for a all operations in a
     segment or for a single operation type.  The operation
     type is entered as a character string as the last
     argument.  If the string "ALL" or the null string is
     entered mods valid for all operations are returned.
     Note that the SETMSNG mod does not apply to a specific
     operation (it is keyed off of input time series which 
     allow missing data) and consequently it will only be
     returned as valid if "ALL" or the null string are
     entered.
    Note that function name changed to valid_mods_for_oper.
    Modified by gfs - hrl - 24 Sept 1994
    
    Modified by page - hrl - 13 Oct. 1995
    WHEN ADDING NEW MODS
    1.  increase MAX_MODS
    2.  update the nts_mask array
    3.  for each operation to which a mod can be applied
          add a line to set the valid_mods_mask
    4.  for each new operation with mods, add a new
          stanza to the case statement
	5.  updates COMAND, ISITFG, and NDATES in mcomnd.f
          
    Modified by page - hrl - 13 Feb. 1996
    Changed routine so it returns the number of valid IFP
      mods.
    Changed it to allow it to be called with menu_array set
      to NULL and size_of_menu=-1.  In this case it will skip
      certain parts of the code but still keep track of
      the number of valid IFP mods.  This will allow it to
      be called by cex25 to determine if it is necessary
      to create the mods interface.      

av added new UHGCDATE

    Date 2/7/07
    Added DPRECIP and SACST MODS
*/

#include <stdio.h>
#include <string.h>
#include "libXs.h"
#include "c_call_f/fserch.h"
#include "c_call_f/mcomnd.h"

/* AV commented the line below due to linux port error 7/16/01 */
/*char    *strstr();*/

int valid_mods_for_oper(p_float, p_char, ts_float, ts_char, menu_array,
						    size_of_menu,
                         oper_name)
float   p_float[];      /* parameter floating point data   */
float   ts_float[];     /* time series floating point data */
char    p_char[][4];    /* parameter character data   */
char    ts_char[][4];   /* time series character data */
xs_menu_struct  menu_array[];
int     size_of_menu;
char    *oper_name; /* if oper_name == "ALL" or null string   */
                    /* get mods for all operations in segment */
                    /* else                                   */
                    /* return valid mods for the specified    */
                    /*  operation                             */
                    /* NOTE: only return SETMSNG mod (not     */
                    /*       associated with any operation)   */
                    /*       if ALL or null string are entered*/
                    /* gfs - hrl - 24 Sept 1994               */
{

/* Update MAX_MODS for additional mods */
/*VA increase to 44 to add UHGCDATE mod */
/*#define MAX_MODS 43*/
#define MAX_MODS 46


int     i;        /* counter  */
int     imod;     /* for loop index */
int     ioper;    /* for loop index */
int     locp;     /* pointer to the location of the beginning of
		     parameter array. */
int     mp;         /* maximum parameter array data value */
int     valid_mods_mask[MAX_MODS];
int     icmnd;      /* counter */
int     isfg;       /* flag that shows if a mod can be used by
		       a forecast group */
int     ndts;       /* Number of dates required for the mod */
char    opname[8];  /* operation name */
char    *loc_blank; /* bland location pointer */
char    valid_mod_name[9];   /* valid modification name */
int     include_time_series; /* include time series flag */
int     locts;               /* location of the time series array */
int     loc_datatype;        /* location of the time series data type */
int     count;               /* counter */
int     num_valid_mods;      /* counter for number of valid ifp mods */
static char    msng_allowed_types[9][5] = {
					   "DQIN",
					   "DQME",
					   "PELV",
					   "QIN ",
					   "RQIN",
					   "RQME",
					   "RQOT",
					   "STG ",
					   "TWEL"
					  };

/*  See order of the mods in the mcomnd.f file (from ofs) 
    Need to update for new mods!
*/
/* aivo set position 18 from 0 to 1 to turn on SACCO mod*/
static  int     nts_mask[MAX_MODS] =
	{ 1, 1, 1, 1, 1, 0, 1, 1, 0, 1,
	  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	  0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
	  1, 0, 1, 1, 1, 0, 0, 0, 0, 1,
      0, 0, 1, 1, 1, 1};

for (imod = 0; imod < MAX_MODS; imod++)   /*  Set all mods to "off".  */
     valid_mods_mask[imod] = 0;

mp = 5000;
num_valid_mods = 0;

	 /* Special check for SETMSNG Mod       */
	 /*  See if any input time series that  */
	 /*  allow missing data.                */

             if(strcmp(oper_name, "ALL") == 0 ||
                strcmp(oper_name, "") == 0)
               {
		locts = 1;

		while (locts > 0)
		 {
		  if(ts_float[locts-1] < 0.5) break;
		  if(ts_float[locts-1] >= 1.0)
		    {                              /*  There are remaining t.s.      */
		     if(ts_float[locts-1] < 3.0)
		       {                        /*  This is an input or update    */
						/*  time series.                  */
			loc_datatype = locts-1 + 4;
			include_time_series = 0;
			for (count = 0; count < 9; count++)
			    {
			     if(strncmp(ts_char[loc_datatype],
					msng_allowed_types[count],
					4)
				== 0)
			       {
				include_time_series = 1;
				break;
			       }                       /*  end if(strncmp..  */
			    }                          /*  end for           */
			if(include_time_series == 1)
			  {
			   valid_mods_mask[19-1] = 1;
			   num_valid_mods += 1;
			   break;
			  }                            /*  end if(include_..  */
		       }                               /*  end if(ts[locts..  */
		    }                                  /*  end if(ts_float..  */
		  locts = ts_float[locts-1 + 1];
		 }                                     /*  end while          */
               }


/*  Now cycle through all the NWSRFS operations - turn "on"
    mods as appropriate.
*/
for (ioper = 1; ioper < 65; ioper++)
    {
     switch (ioper)
     {
      case 1:                                   /* SAC-SMA  */
	  if(strcmp(oper_name, "SAC-SMA") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
             locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[16-1] = 1;
		valid_mods_mask[17-1] = 1;
		valid_mods_mask[18-1] = 1;
	       }
            }
		break;

      case 2:                                   /* UNIT-HG   */
	  if(strcmp(oper_name, "UNIT-HG") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[13-1] = 1;
		valid_mods_mask[14-1] = 1;
		valid_mods_mask[25-1] = 1;
		valid_mods_mask[26-1] = 1;
		valid_mods_mask[27-1] = 1;
		valid_mods_mask[28-1] = 1;
                valid_mods_mask[44-1] = 1; /*AV added to mask out UHGCDATE */
	       }
            }
		break;

      case 14:                                  /* ADJUST-Q  */
	  if(strcmp(oper_name, "ADJUST-Q") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[10-1] = 1;
		valid_mods_mask[34-1] = 1;
		valid_mods_mask[35-1] = 1;
	       }
            }
		break;

      case 19:                                  /* SNOW-17   */
	  if(strcmp(oper_name, "SNOW-17") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[ 2-1] = 1;
		valid_mods_mask[11-1] = 1;
		valid_mods_mask[12-1] = 1;
		valid_mods_mask[29-1] = 1;
		valid_mods_mask[33-1] = 1;
		valid_mods_mask[43-1] = 1;
	       }
            }
		break;

      case 24:                                  /* API-CONT */
	  if(strcmp(oper_name, "API-CONT") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		if((int)(p_float[locp-1 + 13]) == 1)
                  {
                   valid_mods_mask[ 1-1] = 1;
                  }
		valid_mods_mask[ 3-1] = 1;
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[16-1] = 1;
		valid_mods_mask[31-1] = 1;
	       }
            }
		break;

      case 26:                                  /* RES-SNGL */
	  if(strcmp(oper_name, "RES-SNGL") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[10-1] = 1;
		valid_mods_mask[20-1] = 1;
	       }
            }
		break;

      case 29:                                  /* API-MKC   */
	  if(strcmp(oper_name, "API-MKC") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[ 3-1] = 1;
		valid_mods_mask[ 4-1] = 1;
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[16-1] = 1;
	       }
            }
		break;

      case 30:                                  /* MERGE-TS   */
	  if(strcmp(oper_name, "MERGE-TS") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[40-1] = 1;
	       }
            }
		break;

      case 31:                                  /* SNOW-43   */
          if(strcmp(oper_name, "SNOW-43") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
             locp = 1;
             FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
             if(locp > 0)
               {
                valid_mods_mask[ 2-1] = 1;
                valid_mods_mask[11-1] = 1;
                valid_mods_mask[12-1] = 1;
                valid_mods_mask[29-1] = 1;
                valid_mods_mask[33-1] = 1;
                valid_mods_mask[43-1] = 1;
                valid_mods_mask[42-1] = 1;
               }
            }
                break;

      case 33:                                  /* API-CIN   */
	  if(strcmp(oper_name, "API-CIN") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[ 3-1] = 1;
		valid_mods_mask[ 4-1] = 1;
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[16-1] = 1;
	       }
            }
		break;

      case 34:                                  /* API-SLC   */
	  if(strcmp(oper_name, "API-SLC") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[ 4-1] = 1;
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[16-1] = 1;
	       }
            }
		break;

      case 35:                                  /* API-HAR   */
	  if(strcmp(oper_name, "API-HAR") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[ 1-1] = 1;
		valid_mods_mask[ 4-1] = 1;
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[16-1] = 1;
	       }
            }
		break;

      case 36:                                  /* XIN-SMA   */
	  if(strcmp(oper_name, "XIN-SMA") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[30-1] = 1;
	       }
            }
		break;

      case 38:                                  /* BASEFLOW */
	  if(strcmp(oper_name, "BASEFLOW") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
		if(p_float[locp-1 + 8] > 0)
		  {
		   valid_mods_mask[ 5-1] = 1;
		   valid_mods_mask[ 6-1] = 1;
		   valid_mods_mask[ 7-1] = 1;
		  }
		valid_mods_mask[ 8-1] = 1;
		valid_mods_mask[ 9-1] = 1;
	       }
            }
		break;

      case 41:                                  /* API-HAR2 */
	  if(strcmp(oper_name, "API-HAR2") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
	     locp = 1;
	     FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
	     if(locp > 0)
	       {
                valid_mods_mask[ 1-1] = 1;
		valid_mods_mask[ 4-1] = 1;
		valid_mods_mask[15-1] = 1;
		valid_mods_mask[16-1] = 1;
	       }
            }
		break;
 
      case 43:                                  /* API-HFD  */
          if(strcmp(oper_name, "API-HFD") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
             locp = 1;
             FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
             if(locp > 0)
               {
                valid_mods_mask[ 4-1] = 1;
                valid_mods_mask[15-1] = 1;
                valid_mods_mask[16-1] = 1;
               }
             }
                break;
      case 64:                                  /* DHM-OP  */
          if(strcmp(oper_name, "DHM-OP") == 0 ||
             strcmp(oper_name, "ALL") == 0 ||
             strcmp(oper_name, "") == 0)
            {
             locp = 1;
             FSERCH(&ioper, opname, &locp, p_float, p_char, &mp);
             if(locp > 0)
               {
                valid_mods_mask[45-1] = 1;
                valid_mods_mask[46-1] = 1;             
               }
             }
                break;

      default:
	     break;
     }
    }
/*
 *  If the size_of_menu != -1 then
 *  Set all active flags in the menu_ptr array to FALSE.
 */

if (size_of_menu != -1)
   for (i = 0; i < size_of_menu; i++)
        menu_array[i].active = FALSE;

/*
 *  Now if nts_mask and valid_mods_mask are both on (== 1),
 *   set appropriate active flag to TRUE.  
 *  Update the number of valid mods (num_valid_mods). 
 */

for (imod = 0; imod < MAX_MODS; imod++)
    {
     if(nts_mask[imod] && valid_mods_mask[imod])
       {
        num_valid_mods += 1;
	memset(valid_mod_name, '\0', 9);
	icmnd = imod + 1;
        
	MCOMND(&icmnd, valid_mod_name, &isfg, &ndts);
	loc_blank = strstr(valid_mod_name, " ");
	if(loc_blank != NULL)
		strncpy(loc_blank, "", 1);
	if (size_of_menu != -1)	
	   for (i = 0; i < size_of_menu; i++)
	   {
	       if(strcmp(valid_mod_name, menu_array[i].name) == 0)
	       {
	          menu_array[i].active = TRUE;
	          break;
	       }     /*  end if(strcmp...        */
	   }         /*  end for (i = 0...       */
       }             /*  end if(nts_mask...      */
    }                /*  end for (imod...        */

return(num_valid_mods);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/valid_mods_for_oper.c,v $";
 static char rcs_id2[] = "$Id: valid_mods_for_oper.c,v 1.8 2004/08/05 18:01:38 wkwock Exp $";}
/*  ===================================================  */

}                    /*  end valid_mods          */
