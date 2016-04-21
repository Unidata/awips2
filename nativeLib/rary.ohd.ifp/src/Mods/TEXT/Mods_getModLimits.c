/****************************************************************/
/*								*/
/*	FILE:		Mods_getModLimits.c			*/
/*								*/
/*	Read Mod Limits file & create data structs		*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		10/24/94				*/
/*								*/
/*      NOTE:							*/
/*								*/
/****************************************************************/
#ifdef LINX /*added by kwz*/
   #define FSERCH fserch_
#else
   #define FSERCH fserch
#endif
#include <stdio.h>
#include <time.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>


#include "Mods_initStruct.h"
/*#include "c_call_f/fserch.h" *//*--Added by AV --*/

extern void FSERCH(int*, char[], int*, float[], int*);
extern char	*makeFilePath(char *, char *);
extern int	countLines(FILE *);

static void	readModLimitsFile(FILE *, Mod_limitsStruct *);
static int	countNumberOfLimitDefs(FILE *);

#define LINE_LENGTH	100




/****************************************************************/
/*								*/
/*	Mod_limitsStruct *getModValueLimits()			*/
/*								*/
/*								*/
/*	NOTE:	A Mod Limits file (fileName) is read which	*/
/*		specifies the ranges of Mod values		*/
/*      NOTE2:                                                  */
/*              In addition the ofs parameter array is searched */
/*               to see if there is an API-HAR                  */
/*               operation because limits for an AEICQN mod     */
/*               which can apply to this operation are          */
/*               set from parametric values in the p array      */
/*               for the operation.  They vary from segment to  */
/*               segment (actually operation definition to      */
/*               definition) so they cannot be set in a table.  */
/*              Just to complicate life a bit more, the         */
/*               parameter values that specify the limits vary  */
/*               from summer to winter.  That is commented on   */
/*               further where the AEICQN limits are set.       */
/*                 - gfs - HRL - 941119                         */
/*								*/
/****************************************************************/
Mod_limitsStruct *getModValueLimits(char  *SettingsPath,
                                    float *p_float_array,
                                    char  **p_char_array)
{

	int		i;
	int		num;
	char	*fileName = "ModLimits";
	char	*filePath;
	FILE	*file_ptr;
	Mod_limitsStruct	*data;
    struct tm      *time_pointer;
    long   tp;
    int    month, day, summer, locp, numop, mp, loc_limits;
    char   nameout[8];

/* printf("Inside 'getModValueLimits()'...\n"); */

	filePath = makeFilePath(SettingsPath, fileName);

	if((file_ptr = fopen(filePath,"r")) != NULL) {
		/*------------------------------------------------------*/
		/* Count the number of different kinds of Mods in the	*/
		/* Mods Limits file; make space for them; and read	*/
		/* the file (so, were actually reading the file twice)	*/
		/*------------------------------------------------------*/
		
		data        = XtNew(Mod_limitsStruct);
		data->num   = countNumberOfLimitDefs(file_ptr);
		data->array = (modLimitsDef **)XtMalloc(sizeof(modLimitsDef *)*data->num);
	
		for(i = 0; i < data->num; i++)
			data->array[i] = (modLimitsDef *)XtMalloc(sizeof(modLimitsDef));
	
		fclose(file_ptr);
		
		if((file_ptr = fopen(filePath,"r")) != NULL) {
			readModLimitsFile(file_ptr, data);
			fclose(file_ptr);
			XtFree(filePath);		/* Clean-up	*/
			}
		else return(NULL);
/*		
 *  If there is an API-HAR operation, we must get warning limits for AEICQN mod from p array.
 *  If "winter" (8/6 - 2/1) lower and upper limits are in p(16) & p(15).
 *  If "summer" (2/2 - 8/5) lower and upper limits are in p(18) & p(17).
 *  Note:  This mod applies only to the API-HAR operation (number 35).
 *         Also, note that AEICQN is mod number 1 (location 0 in LimitsDef array).
 */
 
              mp = 5000; /* Should replace this with a fortran subroutine */
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
                  
               locp = 1;
               numop = 35;

               memset(nameout, '\0', 8);
               /*this call seemed to be incorrect, there is only one version of FSERCH.f(5 args)*/
               /*AV - changed the call to pass 5 arguments instead of 6 arguments 3/06*/
               /*AV - FSERCH(&numop, nameout, &locp, p_float_array, p_char_array, &mp);*/
               FSERCH(&numop, nameout, &locp, p_float_array, &mp);
              
               if(locp > 0)
                 {                                /* Have found an API-HAR operation  */
                  time(&tp);                      /* Get current time to see if */
                  time_pointer = localtime(&tp);  /* summer or winter.          */

                  month = time_pointer->tm_mon + 1;
                  day   = time_pointer->tm_mday;

                  summer = TRUE;
                  if(month > 8)summer = FALSE;
                  if(month < 2)summer = FALSE;
                  if(month == 8 && day > 5)summer = FALSE;
                  if(month == 2 && day < 2)summer = FALSE;

                  loc_limits = locp-1 + 14;
                  if(summer == TRUE) loc_limits = loc_limits + 2;

                  /* AEICQN limits are stored in inches.                */
                  /* _warning_limits are held in mm in the structure.   */
                  /* Should update to use a general conversion routine. */
                  data->array[0]->lower_warning_limit = 
                            p_float_array[loc_limits + 1] * 25.4;
                  data->array[0]->upper_warning_limit = 
                            p_float_array[loc_limits] * 25.4;
                  
                 }

     }
	else return(NULL);	/* We return NULL to notify that an error has	*/
				/* occurred opening the ModsLimits file		*/
	
	return(data);
}





static void readModLimitsFile(FILE *fp, Mod_limitsStruct *data)
{

	int	i = 0;
	int	number;
	char	*nextline;
	char	line[LINE_LENGTH];
	
	
/* printf("Inside 'readModLimitsFile()'...\n"); */
	
	/*--------------------------------------------------------------*/
	/* Cycle through each line in the file, processing each field	*/
	/* as we go using 'sscanf()'...					*/
	/*								*/
	/* NOTE:	the 'scanf()' family of functions requires 	*/
	/*		that the format string should appear without	*/
	/*		spaces or other characters UNLESS you expect	*/
	/*		them in the input stream exactly as they appear	*/
	/*		in the format string!				*/
	/*--------------------------------------------------------------*/
	while((nextline = fgets(line, LINE_LENGTH, fp)) != NULL) 
	{
		memset(data->array[i]->name, '\0', 20);

		if((number = sscanf(line, "%s%f%d%f%d%f%d%f%d%d",	
			data->array[i]->name,
			&data->array[i]->lower_warning_limit,
			&data->array[i]->lower_warning_inclusive,
			&data->array[i]->upper_warning_limit,
			&data->array[i]->upper_warning_inclusive,
			&data->array[i]->lower_error_limit,
			&data->array[i]->lower_error_inclusive,
			&data->array[i]->upper_error_limit,
			&data->array[i]->upper_error_inclusive,
			&data->array[i]->units)) == 10) 
		   ;
		else printf("FAILED: %d values read...\n", number);

	
/* -----------------------------------------------------------
		printf("%s %8.1f %8.1f %8.1f %8.1f %d %d %d %d %d\n",	
			data->array[i]->name,
			data->array[i]->lower_warning_limit,
			data->array[i]->upper_warning_limit,
			data->array[i]->lower_error_limit,
			data->array[i]->upper_error_limit,
			data->array[i]->lower_warning_inclusive,
			data->array[i]->upper_warning_inclusive,
			data->array[i]->lower_error_inclusive,
			data->array[i]->upper_error_inclusive,
			data->array[i]->units);
   ----------------------------------------------------------- */		
		i++;	
	}
		
}



static int countNumberOfLimitDefs(FILE *fp)
{

	int	num = 0;

	num = countLines(fp);	
	/* printf("%d different Mod types recognized...\n", num); */

	return(num);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_getModLimits.c,v $";
 static char rcs_id2[] = "$Id: Mods_getModLimits.c,v 1.3 2006/04/18 13:34:06 aivo Exp $";}
/*  ===================================================  */

}

