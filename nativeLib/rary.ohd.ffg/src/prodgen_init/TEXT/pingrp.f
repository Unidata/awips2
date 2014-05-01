c  =====================================================================
c  pgm: pingrp (id,istat)
c
c  out: id     .... location identifier
c  out: istat  .... completion code:
c                     0 = successful
c                     1 = end of file or error
c                     2 = invalid data type
c  =====================================================================
c
      subroutine pingrp (id,istat)
c
c.......................................................................
c
c  Routine to read parameters data type 'grpp'.
c
c.......................................................................
c  Initially written by
c     Timothy L. Sweeney - Hydrologic Research Lab            Jan 1992
c
c  changed to free format input
c     Tim Sweeney, HRL                                        Feb 1997
c.......................................................................
c
      character*4 id(2),type,wid(2)
      character*4 cend(2),lcend(2)
      character*8 work
      character*80 line
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'prodgen_inc/grppar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_init/RCS/pingrp.f,v $
     . $',                                                             '
     .$Id: pingrp.f,v 1.2 2004/01/30 17:57:46 scv Exp $
     . $' /
C    ===================================================================
C
      data cend/ 'ENDI','D   ' /
      data lcend/ 'endi','d   ' /
c
c
      call prbug ('pingrp',1,1,ibug)
c
      istat = 0
c
      grptyp = 'grpp'
      inp = iuu
c
c  read first record
      read (inp,10,err=20,end=20) line
10    format (a)
      go to 30
c
c  end of file or i/o error encountered
20    istat = 1
      go to 90
c
30    write (iupr,'(1x,a)') line(1:lenstr(line))
c
c  data type
      iptr = 1
      nwid = 4
      call uffch (line,iptr,nwid,type,nxt,istat)
      if (istat.gt.0) go to 90
      if (type.ne.grptyp.and.type.ne.'GRPP') then
         istat = 2
         go to 90
         endif
c
c  group id
      iptr = nxt
      nwid = 8
      call uffch (line,iptr,nwid,work,nxt,istat)
      if (istat.gt.0) go to 90
      call ucw2c4 (work,2,grpid)
      id(1) = grpid(1)
      id(2) = grpid(2)
c
c  get identifiers for product contents
      k = 0
      do 80 m=1,10
         read (inp,10,end=90) line
         write (iupr,'(1x,a)') line(1:lenstr(line))
         nxt = 1
         do 50 i=1,7
            iptr = nxt
            nwid = 8
            call uffch (line,iptr,nwid,work,nxt,istat)
            if (istat.gt.0) go to 90
            call ucw2c4 (work,2,wid)
            if (wid(1).eq.' '.or.wid(1).eq.'none') go to 80
            k = k + 1
            apid(1,k) = wid(1)
            apid(2,k) = wid(2)
c        check for ENDID or endid
            if (apid(1,k).eq.cend(1).or.apid(1,k).eq.lcend(1)) then
               if (apid(2,k).eq.cend(2).or.apid(2,k).eq.lcend(2)) then
                  go to 90
                  endif
               endif
50          continue
80       continue
c
90    return
c
      end
