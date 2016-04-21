c  =====================================================================
c  pgm:  edvca (kf,lin,lout,mrar,rar)
c
c   in: kf     .... function:
c                     1 = add
c                     2 = change
c                     3 = list
c   in: lin    .... input device
c   in: lout   .... output device
c  i/o: mrar   .... number of words in rar
c  i/o: rar    .... array of character words
c  =====================================================================
c
      subroutine edvca (kf,lin,lout,mrar,rar)
c
c.......................................................................
c
c  This routine is used to edit an array of character variables.
c  The array is displayed and edited as a single line.
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Mar 1991
c.......................................................................
c
      character*4 rar(mrar),rin(24)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edvca.f,v $
     . $',                                                             '
     .$Id: edvca.f,v 1.2 2004/01/30 17:47:26 scv Exp $
     . $' /
C    ===================================================================
C
c
c  function specified by variable kf:
c     1 = new (first time)
c     2 = edit
c     3 = list
c
      if (kf.gt.1) write (lout,10) (rar(i),i=1,mrar)
10    format (7x,18a4)
c
      if (kf.le.2) write (lout,20)
20    format (' Enter: ',$)
c
      if (kf.eq.3) go to 60
c
      read (lin,30) (rin(i),i=1,mrar)
30    format (18a4)
c
      if (mrar.gt.1.and.rin(1).eq.' '.and.rin(2).eq.' ') go to 60
      if (mrar.eq.1.and.rin(1).eq.' ') go to 60
c
      if (rin(1).eq.'skip'.or.rin(1).eq.'SKIP') then
         do 40 i=1,mrar
40          rar(i) = ' '
            go to 60
         endif
c
      do 50 i=1,mrar
         rar(i) = rin(i)
50       continue
c
60    return
c
      end
