#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "dhm.h"
/*

c   errorflag  = 1 - Error in using the same time series ID  
c              = 2 - outlet timeseries type error
c                    (not SQIN)
c              = 3 - Time interval error
c              = 5 - inflow timeseries type error
c                    (neither SQIN nor QINE)
c              = 0 - good data

*/

void pin64_parse_input(char inbuff[6][80],int *ncard,
                       char returnTsId[][NWSRFS_ID_LENGTH+1],
                       char returnTsType[][NWSRFS_DATA_TYPE_LENGTH+1],int returnTsInterval[],
                       char returnBasinId[][NWSRFS_ID_LENGTH+1], int errorflag[], char returnLabels[][8])
                
{
    int  i,j, numberOfTokens;
    char tempstr[80];
    char localLabels[6][8];
    char localTsid[6][NWSRFS_ID_LENGTH+1];
    char localTstype[6][NWSRFS_DATA_TYPE_LENGTH+1];
    char localTsinterval[6][3];
    char localBasinid[6][NWSRFS_ID_LENGTH+1];
    
    for (i = 0; i< *ncard; i++){
        errorflag[i] = 0;
        strncpy(tempstr,inbuff[i],80);
        tempstr[80]='\0';
        numberOfTokens = sscanf(tempstr,"%s %s %s %s %s",localLabels[i],localTsid[i],localTstype[i],localTsinterval[i], localBasinid[i]);
  
    }

    for(j= 0; j< *ncard; j++){
       if(strncmp(localLabels[j],"OUTLET",6) == 0){
           if(strcmp(localTstype[j],"SQIN")){
              errorflag[j] = 2;
              break;
           }
       }
       if(strncmp(localLabels[j],"INFLOW",6) == 0){
           
           if( strcmp(localTstype[j],"SQIN") != 0 && 
                    strcmp(localTstype[j],"QINE") != 0 ){
              errorflag[j] = 5;                         
              break;
           }
       }       
       if( strcmp(localTsinterval[j],"1") != 0 ){
           errorflag[j] = 3;          
           break;
       }
       for(i = 1; i< *ncard; i++){ 
           if( j != i){           
               if( !strcmp(localTsid[j],localTsid[i]) ) {                        
                   if(strcmp(localTstype[j],localTstype[i]) == 0){                                     
                       errorflag[j] = 1;
                       break;
                   }
               }
           } 	        
       }
    }
    /*data is good store them */
       
    for (i = 0; i< *ncard; i++){ 
        if( errorflag[i] == 0){  
	    strcpy(returnTsId[i],localTsid[i]);
	    for(j=0; j<NWSRFS_ID_LENGTH-strlen(localTsid[i]); j++){
	       strcat(returnTsId[i]," ");
	    }
            
	    strcpy(returnTsType[i],localTstype[i]);
	    for(j=0; j<NWSRFS_DATA_TYPE_LENGTH-strlen(localTstype[i]); j++){
	       strcat(returnTsType[i]," ");
	    }
            
	    returnTsInterval[i] = atoi(localTsinterval[i]);  
	    	     
	    strcpy(returnBasinId[i],localBasinid[i]);
	    for(j=0; j<NWSRFS_ID_LENGTH-strlen(localBasinid[i]); j++){
	       strcat(returnBasinId[i]," ");
	    }
           
	    strcpy(returnLabels[i],localLabels[i]); 
	    for(j=0; j<8-strlen(localLabels[i]); j++){
	       strcat(returnLabels[i]," ");
	    }         
        }
        else
            break;
    
    }
  

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
