c  =====================================================================
c  pgm:  pinsup (id,istat)
c
c  out: id     .... water supply identifier
c  out: istat  .... completion code:
c                     0 = successful
c                     1 = end of file or error
c                     2 = invalid data type
c  =====================================================================
c
      subroutine pinsup (id,istat)
c
c.......................................................................
c
c  free format input parameters for type code wsup for water supply
c
c.......................................................................
c      Initially written by
c           Tim Sweeney, HRL                          Sept 1995
c
c   changed to free format input
c           Tim Sweeney, HRL                          Feb 1997
c.......................................................................
c
      character*4 id(2),type,wid(2)
      character*4 cend(2),lcend(2)
      character*20 work
      character*80 line
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/wsparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/pinsup.f,v $
     . $',                                                             '
     .$Id: pinsup.f,v 1.2 2004/01/30 17:51:12 scv Exp $
     . $' /
C    ===================================================================
C
      data cend / 'ENDI','D   '/
      data lcend/ 'endi','d   '/
c
c
      call prbug ('pinsup',1,1,ibug)
c
      istat = 0
c
      wstyp = 'wsup'
c
      inp = iuu
      do 10 i=1,6
         wsup(i) = -99.0
10       continue
c
c  first record
      read (inp,20,end=30,err=30) line
20    format (a)
      go to 40
c
c  end of file for input
30    istat = 1
      go to 100
c
40    write (iupr,'(1x,a)') line(1:lenstr(line))
c
c  data type
      iptr = 1
      nwid = 4
      call uffch (line,iptr,nwid,type,nxt,istat)
      if (istat.gt.0) go to 100
      if (type.ne.wstyp.and.type.ne.'WSUP') then
         istat = 2
         go to 100
         endif
c
c  water supply identifier
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 100
      call ucw2c4 (work,2,wsid)
      id(1) = wsid(1)
      id(2) = wsid(2)
c
c  description
      iptr = nxt
      nwid = 20
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 100
      call ucw2c4 (work,5,sdesc)
c
c  drainage area
      iptr = nxt
      nwid = 6
      call uffir (line,iptr,nwid,i,sarea,nxt,istat)
      if (istat.gt.0) go to 100
c
c  water supply basin id
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      call ucw2c4 (work,2,wsbid)
c
      if (ibug.eq.1) write (iud,*) ' type=',type,' wsid=',wsid,
     +   ' sdesc=',sdesc,' sarea=',sarea,' wsbid=',wsbid
c
c  component areas
      na = 0
      do 90 lr=1,4
         ib = (lr-1)*5 + 1
         ie = lr*5
         read (inp,20) line
         write (iupr,'(1x,a)') line(1:lenstr(line))
         nxt = 1
         do 60 i=ib,ie
            iptr = nxt
            nwid = 3
            call uffir (line,iptr,nwid,j,swt(i),nxt,istat)
            if (swt(i).gt.1.0) swt(i) = swt(i)/100.
            if (istat.gt.0) go to 60
            iptr = nxt
            nwid = 8
            call uffch (line,iptr,nwid,work,nxt,istat)
            if (istat.gt.0) go to 60
            call ucw2c4 (work,2,wid)
            sarid(1,i) = wid(1)
            sarid(2,i) = wid(2)
            if (sarid(1,i).eq.cend(1).or.sarid(1,i).eq.lcend(1)) then
               if (sarid(2,i).eq.cend(2).or.sarid(2,i).eq.lcend(2)) then
                  ie = i
                  na = 1
                  go to 70
                  endif
               endif
60          continue
70       if (ibug.eq.1) write (iud,80) (swt(i),
     +      (sarid(j,i),j=1,2),i=ib,ie)
80    format (6x,5(f4.2,1x,2a4))
         if (na.gt.0) go to 100
90       continue
c
100   return
c
      end
