
#include <unistd.h>
#include "ColorThreshold.h"
#include "get_colorvalues.h"
#include "color_threshold_show.h"
#include "NamedColorUseSet.h"
#include "NamedColorSetGroup.h"


// -----------------------------------------------------------------------------



int loadColorThresholdArrayFromFile(ColorThresholdArray *ctArray,
				     FILE *fp, Widget widget)
{
     
     /*
     This function reads a file and fills colorArray.
     */
     
     
     long size = 0;
     long numThresholds = 0;
     long i = 0;
     long lineLength = 80;
     char buffer[BUFSIZ];
     char namePart1[BUFSIZ];
     char namePart2[BUFSIZ];
     char namePart3[BUFSIZ];
     int done = 0;
     int rv;
     int success = 0;
     
     
     
     /*
     Init colorArray
     */
     ctArray->length = 0;
     ctArray->thresholds = NULL;
     
     
     /*
     Determine the number of thresholds to allocate
     */
     for (i=0; (fgets(buffer, 80,  fp)); i++)
     {
     }
     /*
         subtract 2 because of default color and missing color
     */
     numThresholds = i - 2 ;
     rewind(fp);
     
     
     /*
     Allocate thresholds in ctArray
     */	
     size = numThresholds * sizeof(ColorThreshold);
     ctArray->thresholds = (ColorThreshold *) malloc (size);
     ctArray->length = numThresholds;
     
     
     /*
     Read in the missing color
     */
     if (fgets(buffer, lineLength, fp))
     { 
	  strcpy(namePart1, "");
	  strcpy(namePart2, "");
	  strcpy(namePart3, "");
          rv = sscanf(buffer, "%s%s%s", namePart1, namePart2, namePart3);
	  
	  if (rv > 0)
	  {
	       strcpy(ctArray->missingColorName, namePart1);
	       if (rv > 1)
	       {
		    strcat(ctArray->missingColorName, namePart2); 
		    if (rv > 2)
	            {
		         strcat(ctArray->missingColorName, namePart3);   
	            }
	       }
	  }
	  
	  
     }
     
     
     
     /*
     Read in the default color
     */
     if (fgets(buffer, lineLength, fp))
     { 
	  strcpy(namePart1, "");
	  strcpy(namePart2, "");
	  strcpy(namePart3, "");
          rv = sscanf(buffer, "%s%s%s", namePart1, namePart2, namePart3);
	  
	  if (rv > 0)
	  {
	       strcpy(ctArray->defaultColorName, namePart1);
	       if (rv > 1)
	       {
		    strcat(ctArray->defaultColorName, namePart2);
		    if (rv > 2)
		    {
			 strcat(ctArray->defaultColorName, namePart3);   
		    }
	       }
	       
	  }
	  
     }
     
     
     /*
     	   Read in all the thresholds and color names.
     */
     for (i = 0; ( (i < numThresholds) && (!done)); i++)
     {
	  if (fgets(buffer, lineLength,  fp))
	  {
	       
	       strcpy(namePart1, "");
	       strcpy(namePart2, "");
  	       strcpy(namePart3, "");
	       
	       /*
	       
	       */
	       rv = sscanf(buffer, "%lf%s%s%s", &ctArray->thresholds[i].value,
			   namePart1,
			   namePart2,
			   namePart3);
	       
	       
	       
	       if (rv > 1)
	       {		    
		    success = 1;
		    strcpy(ctArray->thresholds[i].colorName, namePart1);
		    if (rv > 2)
		    {
		         strcat(ctArray->thresholds[i].colorName, namePart2);
			 if (rv > 3)
			 {
			      strcat(ctArray->thresholds[i].colorName, namePart3);    
			 }
		    }
	       }
	       else
	       {
	            done = 1;	    
	       }
	       
	  }
	  else
	  {
	       done = 1;
	  }
	  
     }
     
     
     fclose(fp);
     
     return success;
     
}

/**************************************************************************/

void loadDefaultColorThresholdArray(ColorThresholdArray *ctArray, Widget widget)
{ 
     long size = 0;
     long numThresholds = 0;
     long i = 0;
     
     /*
     This function set a  colorArray to  default colors and values.
     */
     LongText  missingColorName = "gray75";
     LongText  defaultColorName = "black";
     LongText  colorNames[] =
     {
          "red",
	  "orange",
	  "yellow",
	  "green",
	  "blue",
	  "violet"
     };
	
     double values[] =
     {
          0.0,
	  1.0,
	  2.0,
	  3.0,
	  4.0,
	  5.0
     };
	
     
   
     /*
     Init colorArray
     */
     ctArray->length = 0;
     ctArray->thresholds = NULL;
     
     
     numThresholds = sizeof(colorNames)/sizeof(LongText);
      
     
     /*
     Allocate thresholds in ctArray
     */	
     size = numThresholds * sizeof ( ColorThreshold ) ;

     if ( ( ctArray->thresholds = ( ColorThreshold * ) malloc ( size ) ) )
     {
          ctArray->length = numThresholds;
     }
     else
     {
          return;	
     }
     
     
     /*
	  copy missing color name and get the color value	
     */
     strcpy(ctArray->missingColorName, missingColorName);	  
     
     /*
	  copy default color name and get the color value
     */
     strcpy(ctArray->defaultColorName, defaultColorName);
     
     /*
     	  copy values and color names
     */
     for (i = 0; i < numThresholds; i++)
     {
          ctArray->thresholds[i].value = values[i];
	  strcpy ( ctArray->thresholds [ i ].colorName , colorNames [ i ] ) ;
     }
     
     return;
     
}

/**************************************************************************/

void loadDefaultColorUseArray(ColorThresholdArray *ctArray)
{ 
     long size = 0;
     long numThresholds = 0;
     long i = 0;
     
     
     /*
     This function set a  colorArray to  default colors and values.
     */
     LongText  missingColorName = "gray75";
     LongText  defaultColorName = "black";
     LongText  colorNames[] =
     {
          "red",
	  "orange",
	  "yellow",
	  "green",
	  "blue",
	  "violet"
     };
	
     double values[] =
     {
          0.0,
	  1.0,
	  2.0,
	  3.0,
	  4.0,
	  5.0
     };
	
     
   
     /*
     Init colorArray
     */
     ctArray->length = 0;
     ctArray->thresholds = NULL;
     
     
     numThresholds = sizeof(colorNames)/sizeof(LongText);
      
     
     /*
     Allocate thresholds in ctArray
     */	
     size = numThresholds * sizeof ( ColorThreshold ) ;

     if ( ( ctArray->thresholds = ( ColorThreshold * ) malloc ( size ) ) )
     {
          ctArray->length = numThresholds;
     }
     else
     {
          return;	
     }
     
     
     /*
	  copy missing color name and get the color value	
     */
     strcpy(ctArray->missingColorName, missingColorName);	  
     
     /*
	  copy default color name and get the color value
     */
     strcpy(ctArray->defaultColorName, defaultColorName);
     
     /*
     	  copy values and color names
     */
     for (i = 0; i < numThresholds; i++)
     {
          ctArray->thresholds[i].value = values[i];
	  strcpy ( ctArray->thresholds [ i ].colorName , colorNames [ i ] ) ;
     }
     
     return;
     
}


/************************************************************************/
void copyColorThresholdArray(ColorThresholdArray *dest,
			     const ColorThresholdArray *source)
{
     int i;
	
     strcpy(dest->missingColorName, source->missingColorName);
     strcpy(dest->defaultColorName, source->defaultColorName);
    
     dest->thresholds = (ColorThreshold *)
	                malloc (sizeof(ColorThreshold) * source->length );
     
     if (dest->thresholds != NULL)
     {
          dest->length = source->length; 	
     }
     else
     {
	  dest->length = 0;
     }
     
     
     for (i = 0; i < dest->length; i++)
     {
          dest->thresholds[i] = source->thresholds[i];	
     }
    
     
     return;
}
/************************************************************************/

void freeColorThresholdArray(ColorThresholdArray *ctArray)
{

     if (ctArray->thresholds)
     {
          free(ctArray->thresholds);
	  ctArray->thresholds = NULL;
	  ctArray->length = 0;
     }
   
     return;  
}

/**************************************************************************/

void printColorThresholdArray(const ColorThresholdArray *ctArray)
{
     int i;
     
     printf("missing: ");
     printf("%s\n\n",
	    ctArray->missingColorName ) ;
     
     
     printf("default: ");
     printf("%s\n\n",
	    ctArray->defaultColorName ) ;
     
     for (i = 0; i < ctArray->length; i++)
     {
	   printf("%f %s\n",
		  ctArray->thresholds[i].value,
		  ctArray->thresholds[i].colorName ) ;
     }
   
     return;
}

/**************************************************************************/

const char * determineColorByThreshold(double value, double missingValue,
			               const ColorThresholdArray *colorArray)
{
     /*
     Finds the appropriate color that matches the entered value
     in the array of points.
     */
     const char * color = NULL ;
     long i = 0;

     if (value == missingValue)
     {
          color = colorArray->missingColorName;
     }
     
     else  /* not a missing value */
     {
          color = colorArray->defaultColorName ;
	  
	  for (i = colorArray->length - 1 ; i >= 0; i--)
	  {   
	       if (value >= colorArray->thresholds[i].value)
	       {
		    color = colorArray->thresholds[i].colorName;
		    break;
	       }
	  }
     
     }
	
     return color;   
}

/****************************************************************************/

