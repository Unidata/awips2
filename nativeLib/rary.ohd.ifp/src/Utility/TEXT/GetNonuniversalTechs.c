#include <stdio.h>
#include "libXifp.h"
#include "ifp_atoms.h"

getnonuniversaltechs(int *NOSNOW, int *NOFRZE, int *upsc, int *upwe,
                     int *IPRSAC, int *IPRSNW, int *ICRTRO, int *IPRHY, int *IOUTYP)
{
/*
 * non_univ_techniques_struct copied from struct_defs.h
 * change it here, also, if any changes are made in struct_defs.h
 */
typedef struct _non_univ_tech
	{
	int             snow;
	int             frost;
	int             upsc;
	int             upwe;
	int             sac_snow;
	int             printsma;
	int             printsnw;
	int             prtro;
	int             tables;
	}       non_univ_techniques_struct;

  non_univ_techniques_struct   *val_returned;
  int   i, type, format, nitems, left;
  if(XGetWindowProperty
	  (
	  XtDisplay(global_toplevel),
	  DefaultRootWindow(XtDisplay(global_toplevel)),
	  IFPA_current_segment_non_univ_techs,
	  (long) 0,
	  sizeof(non_univ_techniques_struct),
	  FALSE,
	  IFPA_current_segment_non_univ_techs_type,
	  (Atom *)&type,
	  (int *)&format,
	  (unsigned long *)&nitems,
	  (unsigned long *)&left,
	  (unsigned char **)&val_returned
	  ) == Success && type == IFPA_current_segment_non_univ_techs_type
    )
     {
/*
      printf("Inside GetNonuniversalTechs, val_returned = ");
      printf("(snow, frost, upsc, upwe,sac_snow) %d, %d, %d, %d %d\n",           
              val_returned->snow, val_returned->frost,
              val_returned->upsc, val_returned->upwe,
	      val_returned->sac_snow);
 */
      if(val_returned->snow == 0) *NOSNOW = 1;
      else                        *NOSNOW = 0;
      
      if(val_returned->frost == 0) *NOFRZE = 1;
      else                         *NOFRZE = 0;
      
      *upsc = val_returned->upsc;
      
      *upwe = val_returned->upwe;
      
      if(val_returned->sac_snow == 0) *IOUTYP = 0;
      else                            *IOUTYP = 1;

      if(val_returned->printsma == 2)      *IPRSAC = 0;
      else if(val_returned->printsma == 1) *IPRSAC = 1;
      else *IPRSAC = -1;               /* val_returned->printsma == 0 */      

      if(val_returned->printsnw == 2)      *IPRSNW = 0;
      else if(val_returned->printsnw == 1) *IPRSNW = 1;
      else *IPRSNW = -1;               /* val_returned->printsnw == 0 */      

      *ICRTRO = val_returned->prtro;

      if(val_returned->tables == 2)      *IPRHY = 0;
      else if(val_returned->tables == 1) *IPRHY = 1;
      else *IPRHY = -1;               /* val_returned->tables == 0 */      

     }
   else
     {
      printf("**Warning** in GetNonuniversalTechs, no techs posted\n");
      *NOSNOW = 1;
      *NOFRZE = 1;
      *upsc   = 0;
      *upwe   = 0;
      *IPRSAC = 0;
      *IPRSNW = 0;
      *ICRTRO = 0;
      *IPRHY  = 0;
      *IOUTYP = 0;
     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/GetNonuniversalTechs.c,v $";
 static char rcs_id2[] = "$Id: GetNonuniversalTechs.c,v 1.4 2006/04/07 16:59:09 aivo Exp $";}
/*  ===================================================  */

}
