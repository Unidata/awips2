/**********************************************************************/

/* Routine to get program version information. */

void get_prog_version (char *progname, char *version, int iprint) {

int  ifound;
char prog_version[100];

/* set default version */
      strcpy (prog_version," ");
  
  if (strcmp(prog_version," ")==0) {
  /* check program name */
     ifound=0;
     if (strcmp(progname,"ffgoutput") == 0 ||
         strcmp(progname,"make6hrxmrg") == 0 ||
         strcmp(progname,"make24hrxmrg") == 0 ||
         strcmp(progname,"makeXdaysxmrg") == 0 ||
         strcmp(progname,"ofstofs") == 0 ||
         strcmp(progname,"outputbadobs") == 0 ||
         strcmp(progname,"wfoqpf") == 0 ||
         strcmp(progname,"xdat") == 0 ||
         strcmp(progname,"xnav") == 0) {
         sprintf (version,"Program %s (Version:  ob8.3 01/02/08)",progname);
        ifound=1;
        }
     if (ifound==0) {
        printf ("Warning: Default version information not available for program %s.\n",
	        progname);
        sprintf (version,"? no version information ?");
        }
     }
     else {
        strcpy (version,prog_version); 
        }  
          
  if (iprint==1) printf ("%s\n",version);
  
  return;
 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/util/src/util_gen1/RCS/get_prog_version.c,v $";
 static char rcs_id2[] = "$Id: get_prog_version.c,v 1.37 2007/03/20 17:01:14 dsa Exp $";}
/*  ===================================================  */

}
