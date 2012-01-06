#include <stdio.h>
#include <stdlib.h>
#include <string.h>  

#include "String.H"

String::String(char *initText)
{
     unsigned int size;
   
     length = strlen(initText);
     size = length + 1;
     
     text = (char *) malloc(size * sizeof(char));
     
     if (text)
     {
          strncpy(text, initText, length);   
     }
     
     
     return;
     
}
   
   
