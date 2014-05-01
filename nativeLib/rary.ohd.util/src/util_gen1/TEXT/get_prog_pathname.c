/**********************************************************************/

/* Routine to get program path name. */

void get_prog_pathname (char *argv, char *progpath, int iprint) {


/* get program pathname */
   strcpy (progpath,argv);
   
/* check if to print program pathname */
   if (iprint==1) printf ("Program pathname is %s.\n",progpath);
  
  return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_prog_pathname.c,v $";
 static char rcs_id2[] = "$Id: get_prog_pathname.c,v 1.2 2002/02/13 15:55:28 dws Exp $";}
/*  ===================================================  */

}
