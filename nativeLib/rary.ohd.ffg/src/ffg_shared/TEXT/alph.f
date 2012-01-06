c  =====================================================================
c  pgm:  alph (p,val,nval)
c
c   in: p      .... array of nval words
c  out: val    .... array of nval words
c   in: nval   .... number of words 
c  =====================================================================
      subroutine alph(p,val,nval)
c.......................................................................
c  routine copies nval words from array p to array val
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Jan 1992
c.......................................................................
c
cc      character*4 val(nval)
      dimension p(nval),val(nval)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/alph.f,v $
     . $',                                                             '
     .$Id: alph.f,v 1.1 2001/08/16 17:42:38 dws Exp $
     . $' /
C    ===================================================================
C
      do 10 i=1,nval
      val(i) = p(i)
   10 continue
      return
      end
