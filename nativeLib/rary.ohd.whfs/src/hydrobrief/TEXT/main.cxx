#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include "MotifMaster.h" 
#include "HydroBrief.H"
#include "Xtools.h"   
   
int main ( int argc, char **argv )
   {
      extern char	*optarg;
      
      char		* dbms = NULL ;
      HydroBrief        * hb = NULL ;
      int		c;
      int		db_given = 0;
      
      
      /* process command line args. */
      
      while ((c = getopt(argc, argv, "d:")) != -1)
      {
	 switch (c)
	 {
	    case 'd':
	       dbms = optarg;
	       db_given = 1;
	       break;
	       
	    default:
	       fprintf(stderr, "usage %s -d<dbName>\n", argv[0]);
	       exit(-3);	
	 }
      }
      
      if (!db_given)
      {
	 fprintf(stderr, "usage: %s -d<dbname>\n", argv[0]);
	 exit (-1);
      }

      hb  = ( HydroBrief * ) HydroBrief::create ( NULL, dbms, argc, argv ) ;
      
      return 0 ;
      
   }
