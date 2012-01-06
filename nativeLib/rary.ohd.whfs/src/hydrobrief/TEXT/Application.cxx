

#include "Application.H"



//*************************************************************************

Application::Application()
{
   
     return;
}

//*************************************************************************

Application::~Application()
{
   // printf("inside Application::destructor\n");
   
     return;
}

//*************************************************************************

void Application::destroy(Application *app)
{
   //printf("inside Application:destroy\n");
   
   
     if (app)
     {
          delete app;
     }
   
     exit(0);
     return;
}

//*************************************************************************
