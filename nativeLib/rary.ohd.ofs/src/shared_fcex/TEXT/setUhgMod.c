#include "mods_plot.h"/*for ifp_globals.h*/
#include "Mods_info.h"
#include "c_call_f/fserch.h"
#include "ifp_globals.h" /*use mods_general_units*/


float *UHGP;
/*global variables*/
/*int	numUhgMod=0;*//*Number of UHGCHNG and UHGCDATE and base*/
int	numUhgMod[20];
int     gopidx, total_numop=1;
char    baseopBuf[20][9], modopBuf[20][9];
union   _ModInfo UhgList[20][100],uhgchngMod;
int     search4opindex( char inBuf[9]);

int     parseOpname(char buf[162]);

/************************************************/

void    inituhglist()/*called in mods.f*/
{ 
  int i;
	/* AV numUhgMod=0;*/
        
	
	strcpy(uhgchngMod.b3.command,"       ");
        
    for (i=0;i<20;i++)
     {
        memset(UhgList[i][0].b3.command,'\0',9);
        strcpy(UhgList[i][0].b3.command,"       ");
        memset(UhgList[i][1].b3.command,'\0',9);
        strcpy(UhgList[i][1].b3.command,"       ");
            
         numUhgMod[i]=2;/* 0 = BASE, 1 is for DEFAULT */
    }
}

/******************************/
/*void setAUhgMod(char *command, char *ID, int *NumOrd, float *ordinates,
                int *NumOp, char **opname)
{	int i;

	if (numUhgMod<0 || numUhgMod>100)numUhgMod=0;

    strcpy(UhgList[numUhgMod].b3.command,command);
	strcpy(UhgList[numUhgMod].b3.id,ID);
	
	UhgList[numUhgMod].b3.num_values=*NumOrd;
	UhgList[numUhgMod].b3.values=(float*) malloc (sizeof(float) * *NumOrd);
	for (i=0;i<*NumOrd;i++)
		UhgList[numUhgMod].b3.values[i]=ordinates[i];
		
    if (*NumOp>10)
		UhgList[numUhgMod].b3.number_of_opers=0;
	else
		UhgList[numUhgMod].b3.number_of_opers=*NumOp;
	
	for (i=0;i<*NumOp;i++)
		strcpy(UhgList[numUhgMod].b3.opname[i],opname[i]);

	numUhgMod++;
}*/
/*Initialize dates for base UHG*/
void Initdates4Base(int *StartDate, int *EndDate, int *ValidDate)
{
int i;
float mult_conver_factor; 

/*set  dates for base UHG (it only has one date in uhgchng mod)*/
   
    for(i=0;i<= total_numop; i++){
	UhgList[i][0].b3.start_date=*StartDate;
	UhgList[i][0].b3.end_date=*EndDate;
        UhgList[i][0].b3.valid_date=*ValidDate;
    }
}


/******store latest uhgchng mod info in UhgList*******/
void cacheuhgchng(int *numVal, float Vals[],char opn[8])
{
	int i, cur_idx;
        float mult_conver_factor; 
        char uhg_opn_lcl[9];
        
          strncpy(uhg_opn_lcl,opn,8);
          uhg_opn_lcl[8]='\0';
          cur_idx=search4opindex(uhg_opn_lcl);
        
          strcpy(UhgList[cur_idx][1].b3.command,"DEFAULT");
	  strcpy(UhgList[cur_idx][1].b3.id,"DEFAULT");
	  UhgList[cur_idx][1].b3.num_values=*numVal;
	  UhgList[cur_idx][1].b3.values=(float*) malloc (sizeof(float) * *numVal);
              
          UhgList[cur_idx][1].b3.number_of_opers=0;    
	  if (mods_general_units) mult_conver_factor=1.0;
	  else mult_conver_factor=0.897;
	  for (i=0;i<*numVal;i++){
            UhgList[cur_idx][1].b3.values[i]=Vals[i]*mult_conver_factor;                     
          }                
 
}
/******store base uhg info to UhgList*******/
void savebuhg(float UHGP[],char opn[8],int *opidx)
{
int i;
float mult_conver_factor; 	

        if (mods_general_units) mult_conver_factor=1.0;
	else mult_conver_factor=0.897;
        gopidx = *opidx -1;
	strcpy(UhgList[gopidx][0].b3.command,"BASE");
	strcpy(UhgList[gopidx][0].b3.id,"BASE");
	UhgList[gopidx][0].b3.num_values=(int)UHGP[9];
        
	UhgList[gopidx][0].b3.values=(float*) malloc (sizeof(float) * (int)UHGP[9]);
	strncpy(UhgList[gopidx][0].b3.opname[0],opn,8);
   /*     for (i=0;i<(int)UHGP[9];i++){* av*/
        for (i=0;i<UhgList[gopidx][0].b3.num_values;i++){
        
	     UhgList[gopidx][0].b3.values[i]=UHGP[(int)UHGP[21]-1+i]*mult_conver_factor;
     
        }                     
	strcpy(uhgchngMod.b3.command,"BASE");
	strcpy(uhgchngMod.b3.id,"BASE");       
        memset(baseopBuf[gopidx],'\0',9);
        strncpy(baseopBuf[gopidx],opn,8);
        total_numop = *opidx;
        
}
/******store latest uhgchng, and uhgcdate info in UhgList*******/
void cacheuhgmods(float P[],int *MP, int *NDTUHG,int JDTUHG[20][3],
     int NVLUHG[20],float UHGVAL[100][20], char UHGOPN[20])
{
int i,j,cur_idx, l, nops;
int LOCOPN,LOCP,LORDS,two=2;
float ratio ;
float mult_conver_factor; /* Multiplication conversion factor. see mp_Create_ts_array.c*/ 
char uhg_opn_lcl[9];
int numMod;

    if (mods_general_units) 
        mult_conver_factor=1.0;
	else 
        mult_conver_factor=0.897;
    
    numMod=*NDTUHG;
        
    for(i=0;i<total_numop;i++) 
        numUhgMod[i]=2;/*offset 0 first two for BASE & DEFAULT*/
        
    /* example: UHGOPN = "ROSSE   ROSSE   ROSSEB  ROSSE   ROSSE"); */      
    nops = parseOpname(UHGOPN);
        
    if (numMod<0 ) 
        return;
    if (numMod>20) 
    {
        numMod=20;
        printf("WARNING: There are more than 20 UHGCDATE mods for this operation.\n");
        printf("WARNING:   The list has been truncated to 20.\n"); 
    } 
              
    for (i=numMod-1;i>=0;i--)
    { 
        uhg_opn_lcl[8]='\0'; 
        strncpy(uhg_opn_lcl,modopBuf[i],8);
        cur_idx=search4opindex(uhg_opn_lcl);   
	     
        /* numUhgMod is an integer array counting the number of unit hg mods for each operation */
	    /* numUhgMod[cur_idx]=2;av*/
	    strcpy(UhgList[cur_idx][numUhgMod[cur_idx]].b3.command,"UHGCDATE");
        strcpy(UhgList[cur_idx][numUhgMod[cur_idx]].b3.id,"UHGCDATE");
	
	    UhgList[cur_idx][numUhgMod[cur_idx]].b3.num_values=NVLUHG[i];
	    UhgList[cur_idx][numUhgMod[cur_idx]].b3.values=(float*) malloc (sizeof(float) * NVLUHG[i]);
	    for (j=0;j<NVLUHG[i];j++)
        {
		    UhgList[cur_idx][numUhgMod[cur_idx]].b3.values[j]=UHGVAL[j][i]*mult_conver_factor;
	    }
	    UhgList[cur_idx][numUhgMod[cur_idx]].b3.number_of_opers=1;
                
	    strncpy(UhgList[cur_idx][numUhgMod[cur_idx]].b3.opname[i],modopBuf[i],8);
                
	    UhgList[cur_idx][numUhgMod[cur_idx]].b3.start_date=JDTUHG[i][0];
	    UhgList[cur_idx][numUhgMod[cur_idx]].b3.end_date=JDTUHG[i][1];
	    UhgList[cur_idx][numUhgMod[cur_idx]].b3.valid_date=JDTUHG[i][2];
	    numUhgMod[cur_idx]++;   
	}
       

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/setUhgMod.c,v $";
 static char rcs_id2[] = "$Id: setUhgMod.c,v 1.7 2004/11/23 17:34:00 hank Exp $";}
/*  ===================================================  */
}


/*  parsing opname from a string buffer[160] (20 by 8 chars)*/
int parseOpname(char buf[162]) {

char *delim=" " ;
char *token, tbuf[162];
int   i, nspace=0, ntokens=0;
int   len = strlen(buf);


for (i=len;i>0;i--){
   if(isdigit(buf[i]) || isalpha(buf[i]))break;
   else
   nspace++;
}


strncpy(tbuf, buf, (len-nspace+1));
tbuf[len-nspace+1]='\0';


token = strtok(tbuf,delim);

while(1) {
   if( token != NULL) {
      strcpy(modopBuf[ntokens],token);
      ntokens++;
      token = strtok(NULL, " ");
      
   }
   else {
      break;
   }

}

 return ntokens ;
} 

/*  find matching operation name fron the baseopBuf array
and return the index of the array list if found.  */
/*  find matching operation name and return the index of the array list*/
int search4opindex( char *op_search)
{
  int i, j;
  int indx, op_indx;
  char mod_op_name[9],save_opn_lcl[9];
  char* terminate;

  /* copy to local varialbe */
  (void)strcpy(save_opn_lcl,"         ");
  (void)strncpy(mod_op_name,op_search,8);
  mod_op_name[8]='\0';
  
  /* Replace spaces, tabs, newlines with nulls.*/
  for (j = 0; j < 8; j ++)
  {
     if ((mod_op_name[j] == ' ') || (mod_op_name[j] == '\t') || (mod_op_name[j] == '\n'))
         mod_op_name[j] = '\0';
  }

  op_indx=0; /* if there is no mod name, then assume it is for the first operation*/
  for (i=0;i<total_numop; i++)
  {
     (void)strncpy(save_opn_lcl,baseopBuf[i],8); /* nwsrfs ids cannot be more than 8 chars*/      
      save_opn_lcl[8]='\0';
     
     /* Replace spaces, tabs, newlines with nulls before comparison.*/
     for (j = 0; j < 8; j ++)
     {
         if ((save_opn_lcl[j] == ' ') || (save_opn_lcl[j] == '\t') || (save_opn_lcl[j] == '\n'))
             save_opn_lcl[j] = '\0';
     }
     
     if(strcmp(save_opn_lcl,mod_op_name)==0) {
        op_indx=i;
        return op_indx;
     }

  }

return op_indx;
}
