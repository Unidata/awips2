
/*******************************************************************************
* FILENAME:            read_precip_b.c
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

/*----------------------------------------------------------------*/
/*   routine to read level 2 processed precip data                */
/*                                                                */
/*   if file is not found, then value = -1 is returned            */
/*   if level 2 data is overwritten, then value = -2 is returned  */
/*   else value = 1 is returned                                   */
/*----------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

/* Parameters:
 *
 * fname:  The name of the level 2 precipitation file. opoint2.rfcname.MM-DD-YY
 * tget:   The time in ticks of the precipitation to retrieve. 
 * i:      The index of the day to retrieve precipitation for. */

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

int read_precip_b(const char *fname,
                  time_t tget,
                  int i,
                  struct pdata * pdata,
                  const struct station * station, 
                  int max_stations )
{
 FILE *fr = NULL;
 int j,k,ier,m=0,qual;
 char kbuf[100],*p,*q='\0',*r,buf[100],hb5[10];
 static char message [ GAGEQC_MESSAGE_LEN ] = {'\0'};
 int type=0;
 int maxk=0,startk=0;
 int rier;
 char pc,datbuf[50],parmbuf[50];
 int number_found[5];
 struct tm *gm;
 int reset_flag;
 
 reset_flag=0;
 
 rier=-1;

 /* Attempt to open the level 2 data file. */
 fr=fopen(fname,"r");
 if(fr==NULL)
{
	bzero(message,150);
	sprintf(message,"Could not open file: %s\n",fname);
	logMessage(message);
}
else
{
		bzero(message,150);
	sprintf(message,"Opened file: %s\n",fname);
	logMessage(message);

}
 /* Initialize the data time structure. */
 pdata[i].data_time=tget;
 
 /* Convert this time to a struct tm time representation in GMT. */
 gm=gmtime(&tget);
 
 /* Convert this time into number with format YYYYMMDD. */
 pdata[i].ztime=(gm->tm_year+1900)*10000+(gm->tm_mon+1)*100+gm->tm_mday;
 
 /* Initialize the array which keeps track of the 6 and 24 hour period
    precipitation values processed. */
 for(j=0;j<5;j++)
       number_found[j]=0;

 /* Initialize the data value and data quality elements for each record
    in the pdata array. This is done for the frain array. */
 for(k=0;k<max_stations;k++) {
 
                  for(m=0;m<5;m++) {

                               pdata[i].stn[k].frain[m].data=-1;
                               pdata[i].stn[k].frain[m].qual=-1;

			     }

			   }

 /* The file could not be opened. Maybe it does not exist. */
 if(fr==NULL) {

                  reset:
 
                  pdata[i].level=1;

                  for(k=0;k<max_stations;k++) {
 
                        for(m=0;m<5;m++) {

                                      if(pdata[i].stn[k].sflag[m] != 1) {

                                      if(pdata[i].stn[k].frain[m].qual!=2) {
                                      
                                      pdata[i].stn[k].frain[m].data=
                                      pdata[i].stn[k].rain[m].data;
                                      pdata[i].stn[k].frain[m].qual=
                                      pdata[i].stn[k].rain[m].qual;
                                      
                                      }
                                      
                                      if(pdata[i].stn[k].frain[m].data >= 0)
                                                  number_found[m]++;
                                                  
                                                  }
                                                  
                                      else {

                                      pdata[i].stn[k].frain[m].data=
                                      pdata[i].stn[k].srain[m].data;
                                      pdata[i].stn[k].frain[m].qual=8;
                                   
                                      
                                      if(pdata[i].stn[k].frain[m].data >= 0)
                                                  number_found[m]++;
                                                  
                                                  }
                                                  


				    }

		      }


             for(j=0;j<5;j++) {

               if(number_found[j] == 0)
                             pdata[i].used[j]=0;

                         }


              post_bad_values(i);

              return(rier);

		}

 /* Otherwise, the level 2 file was found. */
 for(k=0;k<5;k++)
       pdata[i].used[k]=1;


 pdata[i].level=2;

/* initialize structure */

/* Process the reports. */
 for(;;) {

         bad:
         p=fgets(kbuf,100,fr);
         if(p==NULL)
                   break;

         if(kbuf[0]==':')
                        continue;

	 /* A SHEF timeseries. */
         if(kbuf[0]=='.' && kbuf[1]=='E')
                                  type=1;

	 /* A SHEF timeseries. */
         if(kbuf[0]=='.' && kbuf[1]=='A') 
                                  type=0;

	 /* Read the handbook 5 code, the data date, and the parameter
	    string. */
         ier=sscanf(&kbuf[3],"%s %s %s",hb5,datbuf,parmbuf);
         if(ier==0)
		         /* Bad report format. */
                         continue;

         p=strchr(parmbuf,'/');
         if(p==NULL)
                 continue;

         pc=*(p+5);

	 /* Look for a station and data source match. */
         for(j=0;j<max_stations;j++) {

                  if(strcmp(hb5,station[j].hb5)==0 && 
                     pc==station[j].parm[4]) {                     

                           break;

			   }

		}

         if(j==max_stations) {

		      /* No station and data source match could be
		         found. Read the next SHEF report. */
                      continue;

		    }

         if(type==0) {

		     /* This is a type "A" SHEF report. */

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

	 /* Parse the value out of the SHEF report. */
         for(k=startk;k<maxk;k++) {

		   /* Set the quality flag to 0. */
                   pdata[i].stn[j].frain[k].qual=0;

		   /* Check if this record is completely unparsable.
		      If it is, then skip it and read the next record. */
                   if((p=strchr(q,'/'))==NULL &&
                      (p=strchr(q,'\n'))==NULL)
                                          goto bad;

                   *p=0;

                   strcpy(buf,q);

		   /* Try to parse out the value from the level 2
		      precip SHEF report. */
                   if((p=strchr(buf,'.'))==NULL) {

                              if(pdata[i].stn[j].rain[k].data >= 0)
                                            reset_flag=1;

                              else          {
				            /* Set the data and quality flags
					       to missing. */
                                            pdata[i].stn[j].frain[k].data=-1;

                                            pdata[i].stn[j].frain[k].qual=-1;


                                            }

			    }

                    

                   else  {


                        /* Found and parsed the value. */
                        number_found[k]++;

                        pdata[i].stn[j].frain[k].data=atof(buf);
                        
                        r=p+3;
                        qual=8;
                        while(*r != 0) {

                          if(*r != ' '){

                             if(*r=='S')
                                qual=0;

                             else if(*r=='F')
                                qual=1;

                             else if(*r=='W')
                                qual=2;

                             else if(*r=='Q')
                                qual=3;

                             else if(*r=='D')
                                qual=4;

                             else if(*r=='V')
                                qual=8;

                             else if(*r=='E')
                                qual=5;

                             else if(*r=='L')
                                qual=6;
                              
                             else if(*r=='A') {
                          
                                pdata[i].stn[j].sflag[k]=1;
                                qual=8;
                                
                                }
  
                             else if(*r=='B'){
                             
                                pdata[i].stn[j].sflag[k]=1;
                                qual=0;
                                
                                }

                             else if(*r=='C') {
                             
                                pdata[i].stn[j].sflag[k]=1;
                                qual=3;

                                   }   
                                    
                             pdata[i].stn[j].frain[k].qual=qual;

                             if((qual==5 || qual==4) && 
                                pdata[i].stn[j].rain[k].data >= 0) {
                         
                                 ier=is_bad(i,k,station[j].hb5,
                                            station[j].parm);

                                 if(ier==0) {

                                    logMessage("new data overwriting missing %s %s %f\n",
                                             station[j].hb5,station[j].parm,pdata[i].stn[j].rain[k].data);

                                           rier=-2;
                                           reset_flag=1;

                                             }

					 }



                                 if(pdata[i].stn[j].sflag[k] != 1) {

                                 if((qual!=5 && qual != 4 && qual != 2)
                                    && pdata[i].stn[j].rain[k].data >= 0
                                    && pdata[i].stn[j].rain[k].data !=
                                    pdata[i].stn[j].frain[k].data) {

                                    logMessage("new data overwriting old %s %s %f\n",
                                             station[j].hb5,station[j].parm,pdata[i].stn[j].rain[k].data);
                                     
                                           rier=-2;
                                           reset_flag=1;

					 }

                                 if(qual!=5 && qual != 6 && qual != 4 &&
                                    qual != 2
                                    && pdata[i].stn[j].rain[k].data < 0
                                    && pdata[i].stn[j].frain[k].data >=0) {
                                     
                                    logMessage("data set bad level 1 overwriting level 2 %s %s %f\n",
                                             station[j].hb5,station[j].parm,pdata[i].stn[j].rain[k].data);
                                       
                                           rier=-2;
                                           reset_flag=1;

					 }
					 
					 
					 }
					 
				
				else 	{

                                 if(qual!=5
                                    && pdata[i].stn[j].srain[k].data >= 0
                                    && pdata[i].stn[j].srain[k].data !=
                                    pdata[i].stn[j].frain[k].data) {

                                    logMessage("new data overwriting old %s %s %f\n",
                                             station[j].hb5,station[j].parm,pdata[i].stn[j].rain[k].data);
                                     
                                           rier=-2;
                                           reset_flag=1;

					 }

                                 if(qual!=5 && qual != 6
                                    && pdata[i].stn[j].srain[k].data < 0
                                    && pdata[i].stn[j].frain[k].data >=0) {
                                     
                                    logMessage("data set bad level 1 overwriting level 2 %s %s %f\n",
                                             station[j].hb5,station[j].parm,pdata[i].stn[j].rain[k].data);
                                       
                                           rier=-2;
                                           reset_flag=1;

					 }
					 
					 
					 } 
				
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


if(reset_flag==1)
            goto reset;
            
fclose(fr);

/* check if new data not avbl on level 2 comes in on level 1 */

for(k=0;k<5;k++) {

for(j=0;j<max_stations;j++) {
 
         if(pdata[i].stn[j].rain[k].data >= 0 &&
            pdata[i].stn[j].frain[k].data < 0)  {
            
                logMessage("new level 1 data %s %d\n",station[j].hb5,m);
                 goto reset;
                 
                 }
                 
                 }
                 
                 }


/* check for new data overwriting bad data */
   
for(m=0;m<5;m++) {

  if(number_found[m]==0)
            pdata[i].used[m]=0;

  ier=get_bad_values(i,m);
  if(ier==1) {

         logMessage("new data overwriting old bad data\n");
          goto reset;

	}

}

 return(1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
