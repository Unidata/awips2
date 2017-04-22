c  =====================================================================
c  pgm:  edvrag (kf,lin,lout,rar,mx,nperg,ngrps)
c
c   in: kf     .... function
c                     1 - add
c                     2 - change
c                     3 - list
c   in: lin    .... input device
c   in: lout   .... output device
c  i/o: rar    .... array of real words
c  i/o: mx     .... number of words in rar
c   in: nperg  .... number of words per group
c  i/o: ngrps  .... number of groups
c  =====================================================================
c
      subroutine edvrag (kf,lin,lout,rar,mx,nperg,ngrps)
c
c.......................................................................
c
c  routine used to edit array rar of length mx containing real words
c  by groups of nperg words.
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Mar 1991
c.......................................................................
c
      character*78 line
      dimension rar(mx),rin(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edvrag.f,v $
     . $',                                                             '
     .$Id: edvrag.f,v 1.2 2003/08/20 13:09:09 scv Exp $
     . $' /
C    ===================================================================
C
      mxg = mx/nperg
c
      do 110 i=1,mxg
         j = (i-1)*nperg + 1
         if (kf.eq.1) go to 40
ccc         if (rar(j+1).lt.1.1.and.i.gt.4) go to 210
c        edit and list values
            write (lout,10) i,(rar(j+k-1),k=1,nperg)
10    format (8x,'(',i2,') ',6f8.2)
            if (kf.eq.3) go to 110
            go to 40
30          write (lout,*) (' ERROR: invalid format.')
40       write (lout,50) i
50    format (8x,'(',i2,') ',$)
         read( lin,60,err=30) line
60    format (a)
         iptr = 1
         nwid = 8
         do 70 k=1,nperg
            call uffir (line,iptr,nwid,n,rin(k),nxt,istat)
            if (istat.gt.0) then
               if (rar(j).le.-1.0.and.rar(j).gt.-1.1) go to 120
               go to 100
               endif
            iptr = nxt
70          continue
c     test for entry termination
         if (nperg.gt.1.and.(rin(2).lt.-0.08.and.rin(2).gt.-1.1)) 
     +      go to 120
         if ((n.le.-1.or.(rin(1).lt.-0.08.and.rin(1).gt.-1.1)).and.
     +       i.gt.3) go to 120
c     replace values
         do 90 k=1,nperg
            rar(j+k-1) = rin(k)
90          continue
100      if (i.ge.ngrps.and.ngrps.gt.1) then
c        set next group to terminate input
            rar(j+nperg)  = -1.0
            rar(j+nperg+1) = 0.0
            endif
110      continue
c
      if (kf.eq.3) go to 140
c
c  number of sets of values
120   ngrps = (i-1)
c
c  fill remainder of array
      m = nperg*ngrps + 1
      do 130 k=m,mx
         rar(k) = 0.0
130      continue
c
140   return
c
      end
