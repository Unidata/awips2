/*------------------------------------------------------------------------------
  Routines to set and check file attributes.
  ------------------------------------------------------------------------------
  notes: (1) These routines set and checks the direct and sequential
             access bits for a file.  The "unitno.h" header file
             contains the bit value macros for setting the file
             status information.
         (2) The array IFILES array is used to indicate the
             status of a file. Originally the flags for this
             variable were:
                0 = file unused
                1 = file is direct access
                2 = file is sequential
             In order to add the capability to reserve files, the
             IFILES variable is treated as a bit mask where the
             above values apply and additionally:
                4 = file is reserved
  ------------------------------------------------------------------------------
  variables:
   ifile ... file unit number (a single value from array FILES in common block 
             UFILES)
  ------------------------------------------------------------------------------
*/

#include "common/unitno.h"

/**********************************************************************/

/* setseq - set bit indicating file is a sequential file */

int setseq ( int *ifile )
{
   *ifile |= NWSRFS_FILE_SEQUENTIAL;

   return 0;
   
}


/**********************************************************************/

/* setda - set bit indicating file is a DAIO file */

int setda ( int *ifile )
{

   *ifile |= NWSRFS_FILE_DAIO;
   
    return 0;
    
}


/**********************************************************************/

/* setres - set bit indicating file is reserved for use */

int setres ( int *ifile )
{

   *ifile |= NWSRFS_FILE_RESERVED;

    return 0;

}


/**********************************************************************/
    
/* isused - check to see if a file is currently being used */

int isused ( int *ifile )
{

   if ( ((unsigned int)*ifile & NWSRFS_FILE_DAIO) ||
        ((unsigned int)*ifile & NWSRFS_FILE_SEQUENTIAL) )
      return 1;
      else
         return 0;

}


/**********************************************************************/
    
/* setnot - set file unit to not used */

int setnot ( int *ifile )
{

   *ifile |= NWSRFS_FILE_UNUSED;

    return 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/calb/src/gen/RCS/ifiles_attr.c,v $";
 static char rcs_id2[] = "$Id: ifiles_attr.c,v 1.2 2001/06/14 18:33:54 dws Exp $";}
/*  ===================================================  */

}
