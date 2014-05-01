#include "HydroBrief.H"

//********************************************************************

HydroBrief* HydroBrief::create(Widget initShell,
			       char *initDatabaseName,
			       int initArgC,
			       char *initArgV[])
{
   
   //printf("inside HydroBrief::create\n");
   HydroBrief *app = new HydroBrief(initShell, initDatabaseName,
				    initArgC, initArgV);   
   
   return app;
   
}


//********************************************************************

HydroBrief::HydroBrief(Widget initShell,
		       char *initDatabaseName,
		       int initArgC,
		       char *initArgV[])
{
   
   //  printf("inside HydroBrief constructor\n");
   
   strcpy(databaseName, initDatabaseName); 
   
   /*
   if initShell is NULL, that is OK, HydroBriefUI will
   create its own XMainLoop
   */
   topShell = initShell;
   printf("in HydroBrief 4 arg constructor; topShell=%p\n", topShell);
   argc = initArgC;
   argv = initArgV;
   
   startup();
   
   
   return;   
}


//********************************************************************

HydroBrief::~HydroBrief()
{
   
   //printf("Inside HydroBrief destructor\n");
   
   delete ui;
   delete ult;
   delete logic;
   delete dlt;
   
   return;   
}


//********************************************************************
void HydroBrief::startup()
{
   
   createComponents();
   
}


//********************************************************************

void HydroBrief::createComponents()
{

   printf("creating HydroBrief DLT\n");  
   dlt   = new HydroBriefDLT(databaseName);
   
   printf("creating HydroBrief Logic\n");  
   logic = new HydroBriefLogic(dlt);
   
   printf("creating HydroBrief ULT\n");  
   ult   = new HydroBriefULT(logic);
   
   printf("creating HydroBrief UI\n");  
   ui    = new HydroBriefUI(ult, topShell, this, argc, argv); 
   
   return;   
}


//********************************************************************

void HydroBrief::destroy(HydroBrief *app)
{
   // printf("inside HydroBrief:destroy\n");
   
   
   if (app)
   {
      delete app;
   }
   
   return;
}

