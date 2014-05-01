c  =====================================================================
c  pgm:  edviag (kf,lin,lout,iar,mx,nperg,ngrps)
c
c   in: kf     .... function
c                     1 - add
c                     2 - change
c                     3 - list
c   in: lin    .... input device
c   in: lout   .... output device
c  i/o: iar    .... array of integer words by group
c  i/o: mx     .... number of words in iar
c  i/o: nperg  .... number of words per group
c  i/o: ngrps  .... number of groups
c  =====================================================================
c
      subroutine edviag (kf,lin,lout,iar,mx,nperg,ngrps)
c
c.......................................................................
c
c  This routine is used to edit an array of integer words.  The array
c  is displayed and edited by groups of nperg words.
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Mar 1991
c.......................................................................
c
      dimension iar(mx),intar(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edviag.f,v $
     . $',                                                             '
     .$Id: edviag.f,v 1.2 2003/08/20 13:08:58 scv Exp $
     . $' /
C    ===================================================================
C
      mxg = mx/nperg
c
      do 80 i=1,mxg
         j = (i-1)*nperg + 1
         if (kf.eq.1) go to 40
         if (kf.eq.3.and.iar(j).lt.1) go to 80
c        edit and list values
            write (lout,10) i,(iar(j+k-1),k=1,nperg)
10    format (8x,'(',i2,'): ',5i5)
            if (kf.eq.3) go to 80
            go to 40
30          write (lout,*) (' ERROR: invalid format.')
c     enter new or edit values
40       write (lout,50) i
50    format (8x,'(',i2,'): ',$)
         read (lin,60,err=30) (intar(k),k=1,nperg)
60    format (5i5)
c     test for entry termination value
         if (intar(1).lt.0) go to 90
c     test to keep value (no replacement)
         if (kf.eq.2) then
            if (intar(1).gt.-1.and.intar(1).lt.1) then
               go to 80
            endif
         endif
c     replace values
         do 70 k=1,nperg
            iar(j+k-1) = intar(k)
70          continue
80       continue
c
      if (kf.eq.3) go to 100
c
c  number of sets of values
90    ngrps = i-1
c
100   return
c
      end
