c  =====================================================================
c  pgm:  edvra (kf,lin,lout,rar,mx)
c
c   in: kf     .... function:
c                     1 = add
c                     2 = change
c                     3 = list
c   in: lin    .... input device
c   in: lout   .... output device
c  i/o: rar    .... array of real words
c  i/o: mx     .... number of words in rar
c  =====================================================================
c
      subroutine edvra (kf,lin,lout,rar,mx)
c
c.......................................................................
c
c  routine used to edit variables in array rar of length mx containing
c  real words
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Mar 1991
c.......................................................................
c
      character*80 line
      dimension rar(mx)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edvra.f,v $
     . $',                                                             '
     .$Id: edvra.f,v 1.2 2004/01/30 17:48:01 scv Exp $
     . $' /
C    ===================================================================
C
c
      if (kf.gt.1) then
         if (rar(1).gt.10.) then
            write (lout,10) (rar(i),i=1,mx)
10    format (1x,f8.2)
            else
               write (lout,20) (rar(i),i=1,mx)
20    format (1x,f8.4)
            endif
         endif
c
      if (kf.eq.2) write (lout,30)
30    format (' Enter: ',$)
      if (kf.eq.3) go to 60
c
      read (lin,40) line
40    format (a)
      iptr = 1
      nwid = 8
      do 50 i=1,mx
         call uffir (line,iptr,nwid,n,rin,nxt,istat)
         if (istat.gt.0) go to 60
         if (rin.eq.-1.) go to 60
         rar(i) = rin
50       continue
c
60    return
c
      end
