c  =====================================================================
c  pgm:  edwtid (kf,lin,lout,wt,aid,mx,ngrp)
c
c   in: kf     .... function:
c                     1 = add
c                     2 = change
c                     3 = list
c   in: lin    .... input device
c   in: lout   .... output device
c  i/o: wt     .... array of basin weights
c  i/o: aid    .... array of identifiers
c  i/o: mx     .... number of identifiers in aid
c  i/o: ngrp   .... number of groups
c  =====================================================================
c
      subroutine edwtid (kf,lin,lout,wt,aid,mx,ngrp)
c
c.......................................................................
c
c  Routine to edit basin weights and identifiers for headwater 
c  parameters.
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Mar 1991
c.......................................................................
c
      character*4 aid,chin(2)
c
      dimension aid(2,mx),wt(mx)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edwtid.f,v $
     . $',                                                             '
     .$Id: edwtid.f,v 1.2 2004/01/30 17:48:09 scv Exp $
     . $' /
C    ===================================================================
C
c
c  kf controls function:
c    1 = new
c    2 = edit
c    3 = list
c
      if (kf.eq.3) mx = ngrp
c
      do 70 i=1,mx
         if (kf.eq.1) go to 20
c        append 'endid' for list after last id
            if (i.gt.ngrp) then
               aid(1,i) = 'endi'
               aid(2,i) = 'd   '
               endif
c        list values
            write (lout,10) i,wt(i),(aid(j,i),j=1,2)
10    format (8x,'(',i2,') ',f5.2,2x,2a4)
            if (kf.eq.3) go to 70
            go to 20
c     prompt for new or replacement values
20       write (lout,30) i
30       format (8x,'(',i2,') ',$)
         read (lin,40,err=50) rin,chin
40    format (f5.2,2a4)
         go to 60
50       write (lout,*) ' ERROR: incorrect format.'
         go to 20
60       if (chin(1).ne.' ') then
            if (chin(1).eq.'ENDI'.and.chin(2).eq.'D   ') go to 80
            if (chin(1).eq.'endi'.and.chin(2).eq.'d   ') go to 80
            wt(i) = rin
            aid(1,i) = chin(1)
            aid(2,i) = chin(2)
            endif
70       continue
c
      if (kf.eq.3) go to 90
c
80    ngrp = i - 1
c
90    return
c
      end
