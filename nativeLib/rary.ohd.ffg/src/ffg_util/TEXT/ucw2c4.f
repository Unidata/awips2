c  ================================================================
c  pgm ucw2c4 (inar,n,outar)
c
c   in: inar   .... input array
c   in: n      .... number of words in output array
c  out: outar  .... out array
c  ================================================================
      subroutine ucw2c4 (inar,n,outar)
c..................................................................
c  Routine converts character word to character*4 array of n words.
c
c..................................................................
c  Initially written by
c      Tim Sweeney, HRL                                  Feb 1997
c..................................................................
c
      character*(*) inar
      character*4 outar(n)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/ucw2c4.f,v $
     . $',                                                             '
     .$Id: ucw2c4.f,v 1.1 2001/08/16 17:42:50 dws Exp $
     . $' /
C    ===================================================================
C
c
      do 1000 i=1,n
      j = (i-1)*4 + 1
      k = j + 3
 1000 outar(i) = inar(j:k)
      return
      end
      
