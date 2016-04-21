c  =====================================================================
c  pgm:  subidx (nb,ne,mcidx,ncidx,cidx)
c
c   in: nb     .... number of the first identifier to delete
c   in: ne     .... number of the last  identifier to delete
c   in: mcidx  .... maximum number of identifiers in array cidx
c   in: ncidx  .... number of identifiers in cidx
c  i/o: cidx   .... array of identifiers
c  =====================================================================
c
      subroutine subidx (nb,ne,mcidx,ncidx,cidx)
c
c.......................................................................
c
c  This routine deletes identifier and compresses the index array.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                  Apr 1992
c
c  corrected for continuous group of ids
c       Tim Sweeney, HRL                                  Jan 1997
c.......................................................................
c
      character*4 cidx(2,1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/subidx.f,v $
     . $',                                                             '
     .$Id: subidx.f,v 1.2 2003/08/20 13:15:04 scv Exp $
     . $' /
C    ===================================================================
C
c
      ioff = ne - nb + 1
c
      if (ne.lt.ncidx) then
c     move ids
        ibmov = ne + 1
        do 10 i=ibmov,ncidx
           np = i - ioff
           cidx(1,np) = cidx(1,i)
           cidx(2,np) = cidx(2,i)
10         continue
         endif
c
c  set deleted slots to blank
      idel = ncidx - ioff + 1
      do 20 i=idel,ncidx
         cidx(1,i) = ' '
         cidx(2,i) = ' '
20       continue
c
c  reset number of ids and number of words in array
      ncidx = ncidx - ioff
      mcidx = mcidx - 2*ioff
      if (mcidx.lt.2) mcidx = 2
c
      return
c
      end
