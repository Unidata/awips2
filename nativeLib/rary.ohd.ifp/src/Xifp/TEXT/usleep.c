#include <stdio.h>
#include <time.h>

/* ==================================== */
/*  wait for a period of time to pass   */
/*  delay is expressed in milliseconds  */
/*  rcs - 7/2/93                        */
/* ==================================== */

void usleep(unsigned int delay)
{
  clock_t begin, now;
  
  begin = clock();
  for (;;)
    {
     now = clock();
     if (now - begin > delay) break;
    }
  return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/usleep.c,v $";
 static char rcs_id2[] = "$Id: usleep.c,v 1.1 1995/09/08 15:01:41 page Exp $";}
/*  ===================================================  */

}
