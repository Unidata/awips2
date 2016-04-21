#ifndef VERSION_INFO_H
#define VERSION_INFO_H

static char hv_name [ ] =  "HydroView" ;
static char hv_ver [ ] = "OB8.3" ; 
static char hv_ver_date [ ] ="March 25, 2008" ;

/* The routines used to retrieve the version information. */
static char * get_hv_name ( ) { return hv_name ; } ;
static char * get_hv_ver ( ) { return hv_ver ; } ;
static char * get_hv_ver_date ( ) { return hv_ver_date ; } ;

#endif /* #ifndef VERSION_INFO_H */
