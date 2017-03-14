#ifndef HYDRO_BRIEF_HXX
#define HYDRO_BRIEF_HXX 

#include "Application.H"
#include "HydroBriefUI.H"
#include "HydroBriefULT.H"
#include "HydroBriefLogic.H"
#include "HydroBriefDLT.H"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>     
#include "MotifMaster.h"

extern "C"
   {

   }
   
   
class HydroBrief : public Application
{
   
    
public:   
     HydroBrief(Widget shell, char *databaseName,
		int argc, char *argv[]);
   
     ~HydroBrief();
     void startup();
     void createComponents();
     
     // void shutdown();
     
     static HydroBrief * create(Widget initShell, char *initDatabaseName,
			       int argc, char *argv[]);
        
     static void destroy(HydroBrief *app);
     
     
private:
    Widget topShell;
    char databaseName[80];
    
    int argc;
    char **argv;
    
    HydroBriefUI    *ui;
    HydroBriefULT   *ult;
    HydroBriefLogic *logic;
    HydroBriefDLT   *dlt;
   
};


#endif
