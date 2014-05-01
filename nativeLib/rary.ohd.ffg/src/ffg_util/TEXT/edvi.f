c  =====================================================================
c  pgm:  edvi (kf,lin,lout,iv)
c
c   in: kf     .... function:
c                     1 = add
c                     2 = change
c                     3 = list
c   in: lin    .... input device
c   in: lout   .... output device
c  i/o: iv     .... integer word
c  =====================================================================
c
      subroutine edvi (kf,lin,lout,iv)
c
c.......................................................................
c
c  routine used to edit an integer word
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Mar 1991
c.......................................................................
c
      character*80 line
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edvi.f,v $
     . $',                                                             '
     .$Id: edvi.f,v 1.2 2004/01/30 17:47:52 scv Exp $
     . $' /
C    ===================================================================
C
c
      if (kf.gt.1) write (lout,10) iv
10    format (1x,i8)
      if (kf.eq.2) write (lout,20)
20    format (' Enter: ',$)
c
      if (kf.eq.3) go to 60
      go to 40
c
40    read (lin,50,err=30,end=60) line
50    format (a)
      go to 55
c
30    write (lout,*) ' ERROR: incorrect format.'
      go to 40
c
55    iptr = 1
      nwid = 8
      call uffir (line,iptr,nwid,ivin,r,nxt,istat)
      if (istat.gt.0) go to 60
      iv = ivin
c
60    return
c
      end
