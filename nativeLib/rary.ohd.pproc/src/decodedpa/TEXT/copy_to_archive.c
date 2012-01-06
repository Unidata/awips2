 #include "decodedpa.h"
 #include "GeneralUtil.h"

  /*------------------------------------------------------------------------*/
  /*  function to copy raw DPA file to archive dir                          */
  /*                                                                        */
  /*  calling functions:  decodeDPA                                         */
  /*                                                                        */
  /*  construct filename for archived file                                  */
  /*  original name of raw products are of the form DPAXXX... or SDUS...    */
  /*  products are archived with names of the form                          */
  /*    XXXyyyymmddhhmmZ  where XXX = 3 char radar id                       */
  /*------------------------------------------------------------------------*/
 
  void copy_to_archive(int iminute, short hr,
                       char cdate[9], char fname[17], char fullrawfname[128])
  {
     int len;
     char *archdir, *archfname, *command, *fn;

     fn = (char *)malloc(17*sizeof(char));
     archdir = (char *)malloc(100*sizeof(char));
     archfname = (char *)malloc(128*sizeof(char));
     command = (char *)malloc(256*sizeof(char));

     sprintf(fn,"%s%s%02d%02dZ",radid,cdate,hr,iminute);

     len = strlen("dpa_arch_dir");
     get_apps_defaults("dpa_arch_dir",&len,archdir,&len);
     sprintf(archfname,"%s/%s",archdir,fn);

     sprintf(command,"cp  %s  %s",fullrawfname,archfname);
     system(command);

   }
