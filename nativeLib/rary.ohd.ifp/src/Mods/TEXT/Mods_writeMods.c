/********************************************************************************/
/*										                                        */
/*	write_mods.c								                                */
/*										                                        */
/*		Coded by:	Tom Adams and Donna Page			                        */
/*				NWS/Hydrologic Research Laboratory		                        */
/*		Date:		10/??/90					                                */
/*		Modified by:	Tom Adams - 03/26/91, 03/27/91			                */
/*		Last modified:	11/23/94                                                */
/*                              05/19/95  - D. Page				                */
/*                              06/03/95  - A. Vo				                */
/*										                                        */
/*										                                        */
/*		This file contains functions, callbacks & otherwise, to		            */
/*		get data from the global Mods data structure 'data->ModArray[]'	        */
/*		& generate ASCII data files in FORTRAN card image format for	        */
/*		the coupled NWSRFS ver. 5 FORTRAN program.			                    */
/*										                                        */
/*		IMPORTANT: see "Mods_info.h" - formats A1, A2, B1, B2, or B3	        */
/*										                                        */
/********************************************************************************/

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"
#include "cex25.h"

Widget		global_toplevel;/* AV added to debug core dump 5/4/00 */
extern char *get_timeZone_code(Display *);
extern char *make_mod_A1_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_A2_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B1_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B2_string(Mods_everythingStruct *, int, Display *);
extern char *make_mod_B3_string(Mods_everythingStruct *, int, Display *);


static void write_Mod_format_A1(Mods_everythingStruct *, FILE *, int, Display *);
static void write_Mod_format_A2(Mods_everythingStruct *, FILE *, int, Display *);
static void write_Mod_format_B1(Mods_everythingStruct *, FILE *,FILE *, int, Display *);
static void write_Mod_format_B2(Mods_everythingStruct *, FILE *,FILE *, int, Display *);
static void write_Mod_format_B3(Mods_everythingStruct *, FILE *,FILE *, int, Display *);

/* Added the following which used to be in write_mods.h - dp - 15 Oct. 95 */
extern void FindMDH(int, int*, int*, int*, int*, int*, int*, int*, int*, char*);
extern int  FindDecimalPlaces(char*);

extern char     *get_fgroup_name();
extern int findrange(char *);
/********************************************************************************/
/*										*/
/*	Write_mods()								*/
/*										*/
/********************************************************************************/

void Write_mods(Mods_everythingStruct *data)
{
	FILE            *modfp,*fgmodfp;
	int             i;
	int             status;
	char            *fname;
	char            *FGfname;
	char            *FG_id;

	Display         *display;
	Window          root;




/*display = XtDisplay(data->widgetData->ifp_modsShell);*/

display = XtDisplay(global_toplevel);
root = DefaultRootWindow(display);

if(data->ModIndex < 1) return;
status = 1;
fname = (char *) malloc(80);
FGfname = (char *) malloc(80);
memset(fname, '\0', 80);
memset(FGfname, '\0', 80);
strcpy(fname, (char *)getenv("HOME"));
strcpy(FGfname, (char *)getenv("HOME"));


FG_id = get_fgroup_name();

strcat(FGfname, "/.ifp_files/mods/FG_RANGE_");

strcat(FGfname,FG_id);


strcat(fname, "/.ifp_files/mods/");



switch(data->ModArray[0]->type){
  case Mod_format_A1:
	strcat(fname, data->ModArray[0]->a1.segment_id);
	break;
  case Mod_format_A2:
	strcat(fname, data->ModArray[0]->a2.segment_id);
	break;
  case Mod_format_B1:
	strcat(fname, data->SegmentName);
	break;
  case Mod_format_B2:
	strcat(fname, data->SegmentName);
	break;

  case Mod_format_B3:
	strcat(fname, data->SegmentName);
	break;
  default:
	break;
  }


if (index(fname, ' ') != NULL) memset(index(fname, ' '), '\0', 1);
if (index(FGfname, ' ') != NULL) memset(index(FGfname, ' '), '\0', 1);

 modfp = fopen(fname, "a");
 fgmodfp = fopen(FGfname, "a");

if(modfp == NULL)
	{
	printf("Permission to open %s was denied.\n", fname);
	return;
	}

if(fgmodfp == NULL)
	{
	printf("Permission to open %s was denied.\n", FGfname);
	return;
	}
for(i = 0; i < data->ModIndex; i++)
	{
	switch (data->ModArray[i]->type)
		{
		case Mod_format_A1:
			write_Mod_format_A1(data, modfp, i, display);
			break;

		case Mod_format_A2:
			write_Mod_format_A2(data, modfp, i, display);
			break;

		case Mod_format_B1:
			write_Mod_format_B1(data, modfp,fgmodfp, i, display);
			break;

		case Mod_format_B2:
			write_Mod_format_B2(data, modfp,fgmodfp, i, display);
			break;

		case Mod_format_B3:
			write_Mod_format_B3(data, modfp,fgmodfp, i, display);
			break;
		}
	}

fclose(modfp);
fclose(fgmodfp);

for(i = 0; i < data->ModIndex; i++)
	{
	data->ModArray[i] = NULL;
	}

data->ModIndex = 0;

 XChangeProperty(
	display,
	root,
	IFPA_number_of_mods_to_write,
	IFPA_number_of_mods_to_write_type,
	8,
	PropModeReplace,
	(unsigned char *)&data->ModIndex,
	sizeof(int)
	);
}



/* ******************************************************************************************

	write_Mod_format_A1()


   ****************************************************************************************** */

void write_Mod_format_A1(Mods_everythingStruct *data, FILE *modfp, int mod_index, Display *display)
{

	char    *buffer;

 buffer = (char *) make_mod_A1_string(data, mod_index, display);

 fprintf(modfp, buffer);

}



/* ******************************************************************************************

	write_Mod_format_A2()


   ****************************************************************************************** */

void write_Mod_format_A2(Mods_everythingStruct *data, FILE *modfp, int mod_index, Display *display)
{

	int             j = 0, tot_char = 0, num_char = 0;
	int             julda;  /* Julian day */
	int             julhr;  /* Julian hour */
	int             month, day, year, hour;
	int             zondum; /* time zone value */
	int             dlsdum; /* Daylight savings time flag, 1 for
				   daylight savings time, 0 for standard */
	char            *tz_code; /* time zone code */
	char            *control_str = "      ";
	char            text_val[10];


tz_code = get_timeZone_code(display);

/* Write out mod name to file */
fprintf(modfp, ".%s ", data->ModArray[mod_index]->a2.command);

/* Get appropriate dates and write them out to the file */
if(data->ModArray[mod_index]->a2.start_date)
{
   FindMDH(data->ModArray[mod_index]->a2.start_date, &julda, &julhr,
	   &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
   fprintf(modfp, "%02d%02d%02d%02d%s ",
	   month, day, year%100, hour, tz_code);
}
if(data->ModArray[mod_index]->a2.end_date)
{
   FindMDH(data->ModArray[mod_index]->a2.end_date, &julda, &julhr,
	   &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
   fprintf(modfp, "%02d%02d%02d%02d%s ",
	   month, day, year%100, hour, tz_code);
}
if(data->ModArray[mod_index]->a2.valid_date)
{
   FindMDH(data->ModArray[mod_index]->a2.valid_date, &julda, &julhr,
	   &month, &day, &year, &hour, &zondum, &dlsdum, tz_code);
   fprintf(modfp, "%02d%02d%02d%02d%s ",
	   month, day, year%100, hour, tz_code);
}

/* Put end of line on the first line in file */
fprintf(modfp, "\n");

tot_char = fprintf(modfp, "%s %s %s %d",
		   data->ModArray[mod_index]->a2.segment_id,
		   data->ModArray[mod_index]->a2.info->ts_id,
		   data->ModArray[mod_index]->a2.info->data_type,
		   data->ModArray[mod_index]->a2.info->delta_t);

switch (FindDecimalPlaces(data->ModArray[mod_index]->a2.info->data_type))
{
   case 0:
      control_str = " %.0f";
      break;
   case 1:
      control_str = " %.1f";
      break;
   case 2:
      control_str = " %.2f";
      break;
}

/* Temporary fix so mods always written out with 2 decimal places.
   Response to need for RAIM tschng in English units to have 2 sig. figs.
   Added by dp on 10-31-91.
*/
control_str = " %.2f";

while(j < data->ModArray[mod_index]->a2.num_values)
{
   sprintf(text_val, "%.2f", data->ModArray[mod_index]->a2.values[j]);
   if(tot_char + strlen(text_val) < 68)
   {
      num_char = fprintf(modfp, control_str,
			 data->ModArray[mod_index]->a2.values[j]);
      tot_char += num_char;
      j++;
   }
   else
   {
      fprintf(modfp, " &\n");
      num_char = fprintf(modfp, control_str,
			 data->ModArray[mod_index]->a2.values[j]);
      tot_char = num_char;
      j++;
   }
}
j = 0;

/* print out proper keyword/optype/opname info */
if(strcmp(data->ModArray[mod_index]->a2.keyword, " ") != 0)
   if(tot_char + strlen(data->ModArray[mod_index]->a2.keyword) < 68)
      fprintf(modfp, " %s\n", data->ModArray[mod_index]->a2.keyword);
   else
   {
      fprintf(modfp, " &\n");
      fprintf(modfp, " %s\n" , data->ModArray[mod_index]->a2.keyword);
   }
else if(strcmp(data->ModArray[mod_index]->a2.optype, " ") != 0)
   if(strcmp(data->ModArray[mod_index]->a2.opname, " ") != 0)
      if(tot_char + strlen(data->ModArray[mod_index]->a2.optype) +
		    strlen(data->ModArray[mod_index]->a2.opname) < 68)
	 fprintf(modfp, " %s %s\n", data->ModArray[mod_index]->a2.optype,
		 data->ModArray[mod_index]->a2.opname);
      else
      {
	 fprintf(modfp, " &\n");
	 fprintf(modfp, " %s %s\n", data->ModArray[mod_index]->a2.optype,
		 data->ModArray[mod_index]->a2.opname);
      }
   else
      if(tot_char + strlen(data->ModArray[mod_index]->a2.optype) < 68)
	 fprintf(modfp, " %s\n" , data->ModArray[mod_index]->a2.optype);
      else
      {
	 fprintf(modfp, " &\n");
	 fprintf(modfp, " %s\n" , data->ModArray[mod_index]->a2.optype);
      }
else
   printf("ERROR: neither keyword or optype fields filled");
}


/* ******************************************************************************************

	write_Mod_format_B1()


   ****************************************************************************************** */

void write_Mod_format_B1(Mods_everythingStruct *data, FILE *modfp, FILE *fgmodfp, int mod_index, Display *display)
{

	char    *buffer;
	char    *tmp;
	int	range_found = 0;

 buffer = (char *) make_mod_B1_string(data, mod_index, display);
 /* Check for FGROUP string. Write to /mods/FG_RANGE_FG id file */
 tmp = strstr(buffer,"FGROUP");

 /* Check for RANGE string. */
 

 range_found = findrange(buffer);
 if(tmp != NULL || range_found )
        fprintf(fgmodfp, buffer);
 else
        fprintf(modfp, buffer);

}

/* ******************************************************************************************

	write_Mod_format_B2()


   ****************************************************************************************** */

void write_Mod_format_B2(Mods_everythingStruct *data, FILE *modfp, FILE *fgmodfp, int mod_index, Display *display)
{

	char    *buffer;
	char    *tmp;
	int	range_found = 0;


 buffer = (char *) make_mod_B2_string(data, mod_index, display);
 /* Check for FGROUP string. Write to /mods/FG_RANGE_FG id file */
 tmp = strstr(buffer,"FGROUP");
 
 /* Check for RANGE mods string*/
 range_found = findrange(buffer);
 if(tmp != NULL || range_found)
        fprintf(fgmodfp, buffer);
 else
        fprintf(modfp, buffer);
}


/* ******************************************************************************************

	write_Mod_format_B3()


   ****************************************************************************************** */

void write_Mod_format_B3(Mods_everythingStruct *data, FILE *modfp, FILE *fgmodfp, int mod_index, Display *display)
{

	char    *buffer;
	char    *tmp;
	int 	range_found = 0;


buffer = (char *) make_mod_B3_string(data, mod_index, display);

/* Check for FGROUP string. Write to /mods/FG_RANGE_FG id file */
tmp = strstr(buffer,"FGROUP");

/* check for Range Mods string */
range_found = findrange(buffer);
if(tmp != NULL || range_found )
   fprintf(fgmodfp, buffer);
else
   fprintf(modfp, buffer);



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_writeMods.c,v $";
 static char rcs_id2[] = "$Id: Mods_writeMods.c,v 1.7 2006/04/18 15:28:50 aivo Exp $";}
/*  ===================================================  */

}

