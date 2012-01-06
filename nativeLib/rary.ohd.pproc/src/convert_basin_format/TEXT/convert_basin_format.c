#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>


int convert_basin_format_main(int argc,const char ** argv)

{

FILE *fp,*fw;
int i,j,ier,num;
char *p,kbuf[100],buf1[100];
double lat[10000],lon[10000],prevlat,prevlon;
int iflag;

iflag=0;

fp=fopen(argv[1],"r");

fw=fopen(argv[2],"w");

if(fw==NULL || fp==NULL) {

            printf("could not open\n");

            exit(1);

	  }


for(;;) {

p=fgets(kbuf,100,fp);

if(p==NULL)
           break;

        ier=sscanf(kbuf,"%d %*s %s",&num,buf1);
	printf("kbuf is %s\n",kbuf);

         j=0;
         while(buf1[j]!=0) {

	             buf1[j]=toupper(buf1[j]);
		     j++;
		     }

prevlat=-1;
prevlon=-1;
i=0;
for(;;) {


        p=fgets(kbuf,100,fp);
        if(p==NULL)
                   break;

	if(strncmp(kbuf,"END",3)==0)
	                           break;


        ier=sscanf(kbuf,"%lf %lf",&lon[i],&lat[i]);


		if(ier != 2)
	         continue;


	if(fabs(lat[i]-prevlat)<.0001 && fabs(lon[i]-prevlon)<.0001)
	                              continue;




       prevlat=lat[i];
       prevlon=lon[i];
 	i++;

	}

        num=i;



	fprintf(fw,"%s XXX -1 %d\n",buf1,num);

	for(i=0;i<num;i++) {

		                  fprintf(fw,"%6.4f  %7.4f\n",lat[i],-lon[i]);



	}

}

fclose(fw);
fclose(fp);

return 0;

}


