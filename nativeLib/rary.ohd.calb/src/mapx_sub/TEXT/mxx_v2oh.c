/*  Subroutine to convert ASCII vector file into OH CARD format  */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

 int mxx_v2oh(char *inflx, int lin, char *outflx, int lout, char *formx, 
             int lf, char *columnx, int lcol, char *col_inpx, 
             int lcin, char *out_un, int lu, char *bsnamx, int lbsn,
             float repvalue)       
        
 {             

FILE *input;
FILE *output;        

char station[15], *name, *type, *dimens, *units, *descript;
char string[2000],tstep[2];

/*cfan  04/22/02:  change string[200] to string[2000] */ 
 
char infile[129], outfile[129], format[7], columns[3], col_inp[5], bsname[129];
char out_units[4];
char fortformat[5]="     ";
char *stemp, *outtmp="    ";
char sprn[160];
char *delim=" " ;
char *token;
int ncol=6;
int ncol_inp=1;
int ntokens=0;
int i,ii,leng;
int year=0,month=0,day=0,hour,year0=0,month0=0;
int nday,nday0=0,mon_day,nstep;
int num_days[12]={31,28,31,30,31,30,31,31,30,31,30,31};
long istart=0,istarti=0;
long irec=0;
float conval;
double value;
float prn_val;
float us2met[2]={0.0283,25.4};
               
       strcpy(infile,"");
       strcpy(outfile,"");
       strcpy(format,"");
       strcpy(fortformat,"F");
       strcpy(columns,"");
       strcpy(col_inp,"");
       strcpy(out_units,"");
       strcpy(bsname,"");

      strncat(infile,inflx,lin);
      strncat(outfile,outflx,lout);
      strncat(format,formx,lf);
      strncat(fortformat,formx+1,lf-2);                
      strncat(columns,columnx,lcol);
      strncat(col_inp,col_inpx,lcin);
      strncat(out_units,out_un,lu);
      strncat(bsname,bsnamx,lbsn);

       name = "";
       type = "";
       dimens = "";
       units = "";
       descript = "";
       strcpy(station,"");
       strcpy(tstep,"");

/*	printf("mxx_v2oh translator 10/01/96\n");
	printf("This program converts a vector time series to a N column OH DATACARD time series.\n");
		printf("Usage: mxx_v2oh infile outfile -option -option -option\n");
		printf("Options: -UUU  Converts original file's units to UUU units:\n"); 
		printf("               possible UUU are IN  MM  CMS  CFS capital or non-capital lettars\n");
		printf("         -Fn.n Prints values in the specified Fortran format.  Default is F9.3\n");
		printf("               n must be < 10\n");
		printf("         -Cn Define desired number of columns in DATACARD. Default is n=6\n");
		printf("         -Yn Define column number of input file to read (excluding date-time\n");
		printf("             definition columns. Default is n=1\n");
		printf("         -W<name>  basin ID, if it not defined in input file\n");                    
	if (argc < 2) {
		printf(" ***  ERROR ***  SHOULD BE AT LEAST TWO PARAMETERS: infile outfile\n");
		printf("Usage: mxx_v2oh infile outfile -option -option -option\n");
		exit(8);
        }
*/
	      ncol=atoi(columns);
              if (ncol == 0) ncol = 1;    /* must not allow ncol=0 for % operation later */
	      ncol_inp=atoi(col_inp);
 	      
  conval=1;
  if(NULL==(input=fopen(infile,"r"))) {
    printf("Error opening input file %s\n",infile);  
    fclose (input);
    return (8);
  }
  if (NULL==(output=fopen(outfile, "w"))) {
	printf("Error opening output file %s\n", outfile);
	fclose(output);
	exit(8);
  } 
       
 while(fgets(string,sizeof(string),input)) {
  if(strncmp(string,"$",1)==0 || strncmp(string,"#",1)==0) {
      irec++;
      strncpy(sprn,string,160);   
      sprn[159] = '\n';
      fprintf(output,"%s",sprn);
    }
    
    else  {  
       stemp=string;
        ntokens=0;
       if(istart==0) {
        
      while(token=strtok(stemp,delim)) {
           switch(ntokens) {
             case 0: descript=token; break;
             case 1: type=token; break;
             case 2: dimens=token; break;
             case 3: units=token; break;
             case 4: strcpy(tstep,token); break;
             case 5: strcpy(station,token); break;
             case 6: name=token; break; 
             default: ;
           }               
          stemp='\0';
          ntokens++;      
         } 
          nstep=24/atoi(tstep);
      if(strcmp(out_units,units)==0 || strncmp(out_units," ",1)==0 || 
             strncmp(outtmp,units,strlen(units))==0) {
            conval=1.;
            strcpy(out_units,units);
      }   
      else {
       if((strcmp(out_units,"CMS")==0 || strcmp(out_units,"cms")==0) && 
        (strcmp(units,"CFS")==0 || strcmp(units,"cfs")==0)) conval=us2met[0];
       else {
        if((strcmp(out_units,"CFS")==0 || strcmp(out_units,"cfs")==0) && 
         (strcmp(units,"CMS")==0 || strcmp(units,"cms")==0)) conval=1.0/us2met[0]; 
        else {
         if((strcmp(out_units,"MM")==0 || strcmp(out_units,"mm")==0) && 
          (strcmp(units,"IN")==0 || strcmp(units,"in")==0)) conval=us2met[1];
         else {
          if((strcmp(out_units,"IN")==0 || strcmp(out_units,"in")==0) && 
            (strcmp(units,"MM")==0 || strcmp(units,"mm")==0)) {
                conval=1.0/us2met[1];
                }
          else {
           printf(" ** ERROR ** Unit conversion wrong: File units '%s', Desired '%s'\n",units,out_units);
                  return (8);
                }  
               } 
             }
            }
         } 
         if(strcmp(bsname, " ") != 0) strcpy(station,bsname);
          fprintf(output,"%-14s%-5s%-5s%-5s%-5s%-15s%-s\n",
            descript,type,dimens,out_units,tstep,station,name);
        }  
       else {

/*  Obtain the time series and reformat it                          */
/*    ***** NOTE, the number of cases below refers to the number    */
/*                of basins allowed (i.e.  cases - 3 ... 200 here)  */

      nday=(istarti)/nstep-nday0;
          while(token=strtok(stemp,delim)) {
           switch(ntokens) {
             case 0: year=atoi(token); break;
             case 1: month=atoi(token); break;
             case 2: day=atoi(token); break;
             case 3: if(atoi(tstep) != -9) hour=atoi(token); 
                       else value=atof(token);
                       break;
             case  4: case  5: case  6: case  7: case  8: case  9: case 10: case 11: case 12: case 13:
             case 14: case 15: case 16: case 17: case 18: case 19: case 20: case 21: case 22: case 23:
             case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31: case 32: case 33:
             case 34: case 35: case 36: case 37: case 38: case 39: case 40: case 41: case 42: case 43:
             case 44: case 45: case 46: case 47: case 48: case 49: case 50: case 51: case 52: case 53:
             case 54: case 55: case 56: case 57: case 58: case 59: case 60: case 61: case 62: case 63:
             case 64: case 65: case 66: case 67: case 68: case 69: case 70: case 71: case 72: case 73:
             case 74: case 75: case 76: case 77: case 78: case 79: case 80: case 81: case 82: case 83:
             case 84: case 85: case 86: case 87: case 88: case 89: case 90: case 91: case 92: case 93:
             case 94: case 95: case 96: case 97: case 98: case 99: case 100: case 101: case 102: case 103:
                       value=atof(token); break;
             case 104: case 105: case 106: case 107: case 108: case 109: case 110: case 111: case 112: case 113:
             case 114: case 115: case 116: case 117: case 118: case 119: case 120: case 121: case 122: case 123:
             case 124: case 125: case 126: case 127: case 128: case 129: case 130: case 131: case 132: case 133:
             case 134: case 135: case 136: case 137: case 138: case 139: case 140: case 141: case 142: case 143:
             case 144: case 145: case 146: case 147: case 148: case 149: case 150: case 151: case 152: case 153:
             case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163:
             case 164: case 165: case 166: case 167: case 168: case 169: case 170: case 171: case 172: case 173:
             case 174: case 175: case 176: case 177: case 178: case 179: case 180: case 181: case 182: case 183:
             case 184: case 185: case 186: case 187: case 188: case 189: case 190: case 191: case 192: case 193:
             case 194: case 195: case 196: case 197: case 198: case 199: case 200: case 201: case 202: case 203:
                       value=atof(token); break;
             default: ;
           }               
          stemp='\0';
          if(atoi(tstep) != -9) {
            if(ntokens == ncol_inp+3) goto M1;
          }
          else {
            if(ntokens == ncol_inp+2) goto M1;
          }     
          ntokens++;
                         
         } 
M1:        if(istart == 1) {
            year0=year;
            month0=month;
            fprintf(output,"%2d  %4d %2d   %4d %2d   %-s\n",
                month0,year0,month,year,ncol,fortformat);
          }  
          mon_day=num_days[month-1];
          if((year % 4)==0 && month==2) mon_day=29;
          if(((istarti-1) % ncol)==0) {
           if(year >= 2000) 
            fprintf(output,"%-12s%02d%02d%4d",
		    station,month,year-2000,day);
           else		     
            fprintf(output,"%-12s%02d%02d%4d",
		    station,month,year-1900,day);
 	  }

  if(value == -99.) 
  prn_val=repvalue;
  else {
   if(value == -999.) prn_val=value;
    else prn_val=value*conval;
   }
    
  fprintf(output,format, prn_val);
    if((istarti-ncol) >= 0) {
     if(((istarti-ncol) % ncol)==0 || nday==mon_day) fprintf(output, "\n");
       if(nday==mon_day) istarti=0; 
      }
   }
        
       istart++;
       istarti++;
     }    
   }

   fclose (input);
   fclose (output); 
   if (NULL==(output=fopen(outfile, "r+"))) {
	printf("Error opening output file %s\n", outfile);
	fclose(output);
	exit(8);
   }    
   for(i=0; i <= irec; i++) fgets(sprn,sizeof(sprn),output); 
   fseek(output,0,1);  
   fprintf(output,"%2d  %4d %2d   %4d %2d   %-s\n",
                month0,year0,month,year,ncol,fortformat);   
   fclose (output);
   
   return (0);            

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_v2oh.c,v $";
 static char rcs_id2[] = "$Id: mxx_v2oh.c,v 1.3 2005/06/09 19:24:19 dws Exp $";}
/*  ===================================================  */

 }
