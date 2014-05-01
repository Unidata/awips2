c  =====================================================================
c  pgm:  addidx (num,rid,cidx,mcidx,ncidx,istat)
c
c   in: num    .... number assigned to identifier
c   in: rid    .... identifier
c  i/o: cidx   .... array of identifiers
c   in: mcidx  .... maximum number of identifiers in array cidx
c  i/o: ncidx  .... number of identifiers in cidx
c  out: istat  .... status code
c  =====================================================================
c
      subroutine addidx (num,rid,cidx,mcidx,ncidx,istat)
c
c.......................................................................
c
c  This routine adds an identifier to the index array.
c
c  If num = 0 append to end
c         > 0 move ids down and insert at num
c
c.......................................................................
c       Initially written by
c            Tim Sweeney, HRL                                Apr 1992
c
c       Check for full index array.
c            Tim Sweeney, HL                                 Mar 2000
c.......................................................................
c
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iuws'
c
      character*4 rid(2),cidx(2,1)
      character*4 cnone/'none'/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/addidx.f,v $
     . $',                                                             '
     .$Id: addidx.f,v 1.5 2004/01/30 17:43:42 scv Exp $
     . $' /
C    ===================================================================
C
c
      ibug=0
c
      istat=0
c
      if (ibug.eq.1) write (iud,*) 'in addidx - num=',num,
     +   ' ncidx=',ncidx
c
c  check if identifier is in index
      do 10 i=1,ncidx
         if (ibug.eq.1) write (iud,*) 'in addidx - i=',i,
     +      ' rid=',rid,' cidx(1-2,i)=',cidx(1,i),cidx(2,i)
         if (rid(1).eq.cidx(1,i).and.
     +       rid(2).eq.cidx(2,i)) go to 70
10       continue
c
c  add new identifier
      if (num.gt.0.and.num.le.ncidx) then
c     move ids down one position
         do 30 i=ncidx,num,-1
            do 20 k=1,2
20             cidx(k,i+1) = cidx(k,i)
30             continue
            j = num
         else
c        identifier not found - add at first empty slot
            do 40 j=1,ncidx
               if (cidx(1,j).eq.cnone.or.cidx(1,j).eq.' ') go to 60
40             continue
            j = ncidx + 1
            if (ncidx.gt.mcidx/2) then
               write (iutw,50) ncidx
               if (iupr.ne.iutw) write (iupr,50) ncidx
50    format (' ERROR: maximum number of identifiers (',i4,
     +   ') exceeded in index array.')
               istat = 1
               go to 70
               endif
         endif
c
c  increase size by one id
      ncidx = ncidx + 1
      mcidx = mcidx + 2
c
c  update index array
60    cidx(1,j) = rid(1)
      cidx(2,j) = rid(2)
c
70    return
c
      end
