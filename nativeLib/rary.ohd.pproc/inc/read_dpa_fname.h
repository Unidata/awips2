/*******************************************************************************
* FILENAME:             read_dpa_filename.h
* GENERAL INFORMATION:
* DESCRIPTION:          Contains the prototype for the read_dpa_filename 
*                       routine in the read_dpa_filename.ec file.
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        February 7, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell-Linux
* MODIFICATION HISTORY:
* DATE                      PROGRAMMER     DESCRIPTION/REASON
* February 7, 2002          Bryon Lawrence Original Coding.
* November 4, 2004          Bryon Lawrence Added const qualifiers to rad and
*                                          datetime parameters in 
*                                          read_dpa_fname prototype.
********************************************************************************
*/

#ifndef READ_DPA_FILENAME_H
#define READ_DPA_FILENAME_H

void read_dpa_fname ( const char rad [ 4 ] , const char * datetime ,
                      char fname [ 17 ] , long int * irc ) ;
void read_daa_fname ( const char rad [ 4 ] , const char * datetime ,
                      char fname [ 17 ] , long int * irc ) ;

#endif /* #ifndef READ_DPA_FILENAME_H */
