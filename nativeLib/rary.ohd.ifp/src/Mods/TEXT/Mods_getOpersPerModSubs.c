/****************************************************************/
/*								*/
/*	FILE:		Mods_readOpersPerModRecord.c		*/
/*								*/
/*	Read oper mods file & create data structs		*/
/*								*/
/*	Coded by:	George Smith				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/20/94				*/
/*								*/
/*      NOTE:		Modified by D. Page - 12 Nov. 1995 	*/
/*								*/
/****************************************************************/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include "Mods_operMods_struct.h"

#define LINE_LENGTH	120

void readOpersPerModRecord(FILE *fp,
 			   mod_oper_struct * mos)
{
 int	return_val;
 char	*nextline;
 char	line[LINE_LENGTH];
 
 switch (mos->number_of_opers)
   {
    case 1:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
           mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;
        
    case 2:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;
        
    case 3:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2])) != EOF)
	   ;
        else 
        {
   	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
          }
    }
    break;
                
    case 4:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2],
	   &(mos->oper_numbers[3]), mos->oper_types[3])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;
                
    case 5:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2],
	   &(mos->oper_numbers[3]), mos->oper_types[3],
	   &(mos->oper_numbers[4]), mos->oper_types[4])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;
                
    case 6:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2],
	   &(mos->oper_numbers[3]), mos->oper_types[3],
	   &(mos->oper_numbers[4]), mos->oper_types[4],
	   &(mos->oper_numbers[5]), mos->oper_types[5])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;
                
    case 7:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s%d%s%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2],
	   &(mos->oper_numbers[3]), mos->oper_types[3],
	   &(mos->oper_numbers[4]), mos->oper_types[4],
	   &(mos->oper_numbers[5]), mos->oper_types[5],
	   &(mos->oper_numbers[6]), mos->oper_types[6])) != EOF)
	   ;
     else 
       {
	printf("FAILED: return value is %d...\n", return_val);
	mos->number_of_opers = 0; /* set number of opers to 0 */
       }
     }
    break;
                
    case 8:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2],
	   &(mos->oper_numbers[3]), mos->oper_types[3],
	   &(mos->oper_numbers[4]), mos->oper_types[4],
	   &(mos->oper_numbers[5]), mos->oper_types[5],
	   &(mos->oper_numbers[6]), mos->oper_types[6],
	   &(mos->oper_numbers[7]), mos->oper_types[7])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;
                
    case 9:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2],
	   &(mos->oper_numbers[3]), mos->oper_types[3],
	   &(mos->oper_numbers[4]), mos->oper_types[4],
	   &(mos->oper_numbers[5]), mos->oper_types[5],
	   &(mos->oper_numbers[6]), mos->oper_types[6],
	   &(mos->oper_numbers[7]), mos->oper_types[7],
	   &(mos->oper_numbers[8]), mos->oper_types[8])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;
                
    case 10:
    
     if((nextline = fgets(line, LINE_LENGTH, fp)) != NULL)
     {
        if((return_val = sscanf(line, "%d%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s",	
	   &(mos->oper_numbers[0]), mos->oper_types[0],
	   &(mos->oper_numbers[1]), mos->oper_types[1],
	   &(mos->oper_numbers[2]), mos->oper_types[2],
	   &(mos->oper_numbers[3]), mos->oper_types[3],
	   &(mos->oper_numbers[4]), mos->oper_types[4],
	   &(mos->oper_numbers[5]), mos->oper_types[5],
	   &(mos->oper_numbers[6]), mos->oper_types[6],
	   &(mos->oper_numbers[7]), mos->oper_types[7],
	   &(mos->oper_numbers[8]), mos->oper_types[8],
	   &(mos->oper_numbers[9]), mos->oper_types[9])) != EOF)
	   ;
        else 
        {
	   printf("FAILED: return value is %d...\n", return_val);
	   mos->number_of_opers = 0; /* set number of opers to 0 */
        }
     }
    break;

    
    default:
    
    printf("\nWARNING - getOperModData can handle a maximum of 10 operations per mod.\n");
    break;
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_getOpersPerModSubs.c,v $";
 static char rcs_id2[] = "$Id: Mods_getOpersPerModSubs.c,v 1.2 1996/03/21 17:00:12 page Exp $";}
/*  ===================================================  */

}    
