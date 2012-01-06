
#ifndef APPLICATION_HXX
#define APPLICATION_HXX


#include <stdio.h>
#include <stdlib.h>
#include <string.h>     
#include "MotifMaster.h"

/*
extern "C"
   {

   }
*/


class Application
{
   
public:
   
      Application();
      virtual ~Application();
   
   
      static void destroy(Application *app);
   
};


#endif
