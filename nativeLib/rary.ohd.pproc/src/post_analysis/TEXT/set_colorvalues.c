/*===================================================================*/
/*                         FILE NAME:   set_colorvalues.c            */
/*                                                                   */
/*  FUNCTIONS CONTAINED IN THIS FILE:   set_colorvalues              */
/*===================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include "post_siii_shared.h"
#include "postX.h"
#include "post_stage3.h"
#include "post_drawa.h"
#include "ColorValue.h"

/**********************************************************************/
/*  FUNCTION NAME:   set_colorvalues                                  */
/*       FUNCTION:   read color and value for each level in legend    */
/*                   from database or use default (hard coded) values */
/***********************************************************************

Function type:
   void

Variables
mfactor = multiplication factor for stageiii, rfcwide and post analysis precip levels
cv_use = value for color_use_name field 
       = PRECIP, HEIGHT, etc
cv_duration = value for duration field

Called by function:
   ReadParameters (stageiii and rfcwide)
   ReadParam (post analysis)

***************************************** BEGIN set_colorvalues **********/

extern char app_name[];

void set_colorvalues(data)
   draw_struct *data;

{

 char where[250],where1[100];
 int i,numlev,numcol,mfactor;

 ColorValue *cvHead,*cvPtr;

 sprintf(where1,"WHERE userid = '%s' AND application_name= '%s'",LOGNAME,app_name);
 numcol = 0;
 numlev = 0;

    /*-----------------------------------------------*/
    /*  read levels and colors from ColorValue table */
    /*-----------------------------------------------*/

    sprintf(where,
"%s AND color_use_name= '%s' AND duration= %d AND threshold_unit = 'E' ORDER BY threshold_value",
     where1,cv_use,cv_duration);

    if((cvHead = GetColorValue(where)))
    {

       cvPtr = (ColorValue*) ListFirst(&cvHead->list);
       while(cvPtr)
       {

          color_list_levels[numcol] = (char *) malloc(26*sizeof(char));
          strcpy(color_list_levels[numcol],cvPtr->color_name);
          numcol++;

          if(cvPtr->threshold_value >= 0.0)
          {
             level_value[numlev] = cvPtr->threshold_value;
             numlev++;
          }

          cvPtr = (ColorValue*) ListNext(&cvPtr->node);
       }

       FreeColorValue(cvHead);
       printf("%s levels,colors read from database\n",cv_use);

    }
    else
    {

       /*-----------------------------------------------*/
       /*  levels/colors not found in colorvalue table  */
       /*  define default levels and colors             */
       /*-----------------------------------------------*/

       if(strcmp(app_name,"stage_3") == 0 ||
          (strcmp(app_name,"rfcwide") == 0 && strcmp(cv_use,"PRECIP") == 0))
       {

          printf("default precip levels,colors used\n");
          numcol = 17;
          numlev = 15;

          color_list_levels[0] = "gray30";
          color_list_levels[1] = "black";
          color_list_levels[2] = "black";
          color_list_levels[3] = "DodgerBlue1";
          color_list_levels[4] = "cyan";
          color_list_levels[5] = "DarkGreen";
          color_list_levels[6] = "green4";
          color_list_levels[7] = "green";
          color_list_levels[8] = "yellow";
          color_list_levels[9] = "gold2";
          color_list_levels[10] = "DarkOrange1";
          color_list_levels[11] = "red";
          color_list_levels[12] = "red3";
          color_list_levels[13] = "red4";
          color_list_levels[14] = "magenta1";
          color_list_levels[15] = "DarkOrchid";
          color_list_levels[16] = "white";

          level_value[0] = 0.00;
          level_value[1] = 0.10;
          level_value[2] = 0.20;
          level_value[3] = 0.30;
          level_value[4] = 0.40;
          level_value[5] = 0.50;
          level_value[6] = 0.70;
          level_value[7] = 0.90;
          level_value[8] = 1.10;
          level_value[9] = 1.30;
          level_value[10] = 1.50;
          level_value[11] = 1.75;
          level_value[12] = 2.00;
          level_value[13] = 2.50;
          level_value[14] = 3.00;
       }

       else if(strcmp(app_name,"post_analysis") == 0)
       {

          printf("default precip levels,colors used\n");
          numcol = 17;
          numlev = 15;

          color_list_levels[0] = "gray30";
          color_list_levels[1] = "black";
          color_list_levels[2] = "black";
          color_list_levels[3] = "DodgerBlue1";
          color_list_levels[4] = "cyan";
          color_list_levels[5] = "DarkGreen";
          color_list_levels[6] = "green4";
          color_list_levels[7] = "green";
          color_list_levels[8] = "yellow";
          color_list_levels[9] = "gold2";
          color_list_levels[10] = "DarkOrange1";
          color_list_levels[11] = "red";
          color_list_levels[12] = "red3";
          color_list_levels[13] = "red4";
          color_list_levels[14] = "magenta1";
          color_list_levels[15] = "DarkOrchid";
          color_list_levels[16] = "white";

          level_value[0] = 0.00;
          level_value[1] = 0.01;
          level_value[2] = 0.10;
          level_value[3] = 0.20;
          level_value[4] = 0.30;
          level_value[5] = 0.40;
          level_value[6] = 0.50;
          level_value[7] = 0.75;
          level_value[8] = 1.00;
          level_value[9] = 1.25;
          level_value[10] = 1.50;
          level_value[11] = 1.75;
          level_value[12] = 2.00;
          level_value[13] = 2.50;
          level_value[14] = 3.00;
       }
       else
       {

          /*--------------------------------------------------------------*/
          /*  if use_name not defined for application then print message  */
          /*   and exit                                                   */
          /*--------------------------------------------------------------*/

          printf("colors/levels not defined for application = %s and use_name = %s\n",
               app_name,cv_use);
          printf("program stopping\n");
          exit(1);

       }

    }

    /*---------------------------------------------*/
    /*  define mfactor value based on application  */
    /*---------------------------------------------*/

    if(strcmp(app_name,"stage_3") == 0 ||
         (strcmp(app_name,"rfcwide") == 0 && strcmp(cv_use,"PRECIP") == 0))
       mfactor = 2540;
    else if(strcmp(app_name,"post_analysis") == 0)
       mfactor = 254;
    else
       mfactor = 1;
       
    for (i=0; i<numlev; i++)
       data->levels[i] = (int) (level_value[i]*mfactor);

    data->num_levels = numcol;




}

/********************************************* END set_colorvalues ******/
