
/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
/*------------------------------------------------------*/
/*   routine to read level 1 processed precip data      */
/*                                                      */
/*   if file is not found, then                         */
/*      pdata.used array initialized to 0               */ 
/*      pdata.level array initalized to 0               */ 
/*      return value set to -1                          */
/*   else return value set to 1                         */
/*------------------------------------------------------*/


/* fname: The name of the file to retrieve level 1 precipitation data from.
   example: opoint.rfcname.MM-DD-YY 
   tget: The precip retrieval time in ticks.
   i:    The day to retrieve the data for. */

int read_precip_a(const char *fname,
                  time_t tget,
                  int i,
                  struct pdata * pdata,
		  const struct station * station,
	          int max_stations )
   
{
 FILE *fr = NULL;
 int j;
 int k;
 int ier;
 int m;
 int qual;
 char kbuf[100],*p,*q='\0',*r,buf[100],hb5[10];
 char message [ GAGEQC_MESSAGE_LEN ];
 int number_found[5];
 int type=0;
 char pc,datbuf[50],parmbuf[50];
 int maxk=0,startk=0;
 struct tm *gm;

 /* Attempt to open the level 1 precipitation file.
  * Filename opoint.rfcname.MM-DD-YY */
 fr=fopen(fname,"r");
 if(fr==NULL)
{
	memset(message, '\0', GAGEQC_MESSAGE_LEN);
	sprintf(message,"Could not open file: %s\n",fname);
	logMessage(message);
}
else
{
        memset (message, '\0', GAGEQC_MESSAGE_LEN);
	sprintf(message,"Opened file: %s\n",fname);
	logMessage(message);

}

 /* Store the time in ticks of the data. */
 pdata[i].data_time=tget;

 /* Convert this time into a struct tm in GMT. */
 gm=gmtime(&tget);
 
 /* Store the GMT time as a number of format YYYYMMDD. */
 pdata[i].ztime=(gm->tm_year+1900)*10000+(gm->tm_mon+1)*100+gm->tm_mday;
 
logMessage("ztime %d\n", pdata[i].ztime);

 for(j=0;j<5;j++)
       number_found[j]=0;

 for(k=0;k<max_stations;k++) {
 
                  for(m=0;m<5;m++) {

                               pdata[i].stn[k].rain[m].data=-1;
                               pdata[i].stn[k].rain[m].qual=-1;

			     }

		}

if(fr==NULL) {

              for(k=0;k<5;k++)
                        pdata[i].used[k]=0;

              pdata[i].level=0;
            
              return(-1);

	    }

 for(k=0;k<5;k++)
       pdata[i].used[k]=1;

 pdata[i].level=1;

/* initialize structure */

 for(;;) {

         bad:
         p=fgets(kbuf,100,fr);
         if(p==NULL)
                   break;

         if(kbuf[0]==':')
                        continue;
     
	 /* Determine the type of the SHEF report the precip information
	    is stored in. E is a timeseries, A is a single value. */ 
         if(kbuf[0]=='.' && kbuf[1]=='E')
                                  type=1;

         if(kbuf[0]=='.' && kbuf[1]=='A') 
                                  type=0;

	 /* Read the handbook5 identifier, the date and the data. */
         ier=sscanf(&kbuf[2],"%s %s %s",hb5,datbuf,parmbuf);
         if(ier==0)
		         /* Error reading record. */
                         continue;

	 /* Parse the precipitation values out of the SHEF report. */
         p=strchr(parmbuf,'/');
         if(p==NULL)
                 continue;

         pc=*(p+5);

         for(j=0;j<max_stations;j++) {

                  /* Try to find a matching station id and source in the
	             stations file. */
                  if(strcmp(hb5,station[j].hb5)==0 && 
                     pc==station[j].parm[4]) {  

                             break;

			   }

		}

         if(j==max_stations) {

		      /* A matching station id and source could not be
		         found. Read the next record. */
                      continue;

		    }

         if(type==0) {
		     /* This is a type "A" SHEF Report. */
                     p=strchr(kbuf,'/');
                     if(p==NULL)
                             continue;

                     q=strchr(p,' ');
                     if(p==NULL)
                             continue;
                    
                   maxk=5;
                   startk=4;

		   }

         else if(type==1) {

         /* This is a type "E" SHEF report. */
         q=kbuf;
         for(k=0;k<3;k++) {

                   p=strchr(q,'/');
                   if(p==NULL)
                             break;

                   q=p+1;
                  
		   }

                   if(k!=3)
                          continue;  /* missing */

                   maxk=4;
                   startk=0;

            }

         for(k=startk;k<maxk;k++) {

                   pdata[i].stn[j].rain[k].qual=0;

                   if((p=strchr(q,'/'))==NULL &&
                      (p=strchr(q,'\n'))==NULL)
                                          goto bad;

                   *p=0;

                   strcpy(buf,q);

                   if((p=strchr(buf,'.'))==NULL) {

			      /* Can't find a floating point value.
			         Check if the value is report missing. */
                              if((p=strchr(buf,'m'))!=NULL ||
                                 (p=strchr(buf,'M'))!=NULL) {
                                 
				      /* The value is reported missing. */
                                      pdata[i].stn[j].rain[k].data=-1;
                                      pdata[i].stn[j].rain[k].qual=-1;

					     }

                              else             {

                                             
				               /* The value is bad. */
                                               pdata[i].stn[j].rain[k].data=-2;

					     }
 
			    }

                   else  {

                        number_found[k]++;

			/* Assign the value to the pdata structure. */
                        pdata[i].stn[j].rain[k].data=atof(buf);

			/* Process the quality flag. */
                        r=p+3;
                        qual=8;
                        while(*r != 0) {

                          if(*r != ' '){

                             if(*r=='S')
                                qual=8;

                             else if(*r=='F')
                                qual=1;

                             else if(*r=='L')
                                qual=2;

                             else if(*r=='Q')
                                qual=3;

                             else if(*r=='F')
                                qual=4;

                             else if(*r=='V')
                                qual=8;

                             else if(*r=='E')
                                qual=5;

                             else if(*r=='T')
                                qual=6;
                                    
                             pdata[i].stn[j].rain[k].qual=qual;

                             break;

			       }

                                 else 
                                        r++;

		      }


		 }

          p=strchr(q,0);
          q=p+1;      
         
		 }

       }


fclose(fr);

for(j=0;j<5;j++) {

     if(number_found[j] == 0)
              pdata[i].used[j]=0;

   }


return(1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}























