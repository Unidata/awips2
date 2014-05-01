#include "geminc.h"
#include "gemprm.h"

void init_driver(int *);

void init_driver(int *iret) {

   int ier, mode = 0;

   *iret = 0;

   in_bdta(&ier);
   if ( ier != 0 ) {
      *iret = -11;
      return;
   }

   gd_init(&ier);
   if ( ier != 0 ) {
      *iret = -12;
      return;
   }

   gg_init(&mode, &ier);
   if ( ier != 0 ) {
      *iret = -13;
      return;
   }

   dg_intl(&ier);
   if ( ier != 0 ) {
      *iret = -14;
      return;
   }

   db_init(&ier);
   if ( ier != 0 ) {
      *iret = -15;
      return;
   }

}
