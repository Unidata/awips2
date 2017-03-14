C$PRAGMA C (GET_APPS_DEFAULTS)
c  =====================================================================
c  pgm:  pintbl (inp,iutw,iupr,ibug,mptnum,istat)
c
c   in: inp    .... unit number of parameter file
c   in: iupr   .... unit number of output device
c   in: ibug   .... debug control
c  out: mptnum .... parameter table number
c  out: istat  .... status code
c  =====================================================================
c
      subroutine pintbl_sub (inp,iutw,iupr,ibug,mptnum,istat)
c
c.......................................................................
c
c  Routine reads gribparm file to change default parameters.
c
c.......................................................................
c  Initially written by
c     Tim Sweeney, HL                               Apr 2000
c.......................................................................
c
      character*1 dum
      character*4 id
      character*5 abv
      character*6 tbid,wmo
      character*25 appsvar
      character*50 des
      character*80 line
      character*50 filnam
      character*128 dirnam
c
      include 'xmrg_tbl'
      include 'grib_tbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/pintbl.f,v $
     . $',                                                             '
     .$Id: pintbl.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
c
c
      call prbug ('pintbl',1,1,ibug)
c
ccc      write (*,*) ' iutw=',iutw,' iupr=',iupr
c
      istat=0
c
c  check if directory name specified
      appsvar='grib_tab_dir'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,dirnam,ldirnam)
      if (ldirnam.eq.0) then
         iprint=0
         if (iprint.eq.1) then
            write (iutw,10) appsvar(1:lappsvar)
            if (iupr.ne.iutw) then
               if (iupr.ne.iutw) write (iupr,*)
               write (iupr,10) appsvar(1:lappsvar)
10    format (' NOTE: apps_default ',a,' not specified.')
               endif
            endif
         go to 120
         endif
c
c  open file
      filnam = dirnam(1:ldirnam)//'/'//'gribtab'
      lfilnam = lenstr(filnam)
      open (inp,access='sequential',form='formatted',
     +      file=filnam,status='old',iostat=iostat)
      if (iostat.eq.0) then
         write (iutw,20) filnam(1:lfilnam)
         if (iupr.ne.iutw) write (iupr,20) filnam(1:lfilnam)
20    format (' NOTE: reading from User Table file ',a,'.')
         go to 40
         else
            write (iutw,30) iostat,filnam(1:lfilnam)
            if (iupr.ne.iutw) write (iupr,30) filnam(1:lfilnam)
30    format (' ERROR: iostat ',i3,' encountered opening file ',a,'.')
            istat = 1
            go to 120
         endif
c
c  read record
40    read (inp,'(a)',end=110,err=50) line
      go to 70
50    write (iutw,60) filnam(1:lfilnam)
60    format (' ERROR: encountered reading file ',a,'.')
      if (iupr.ne.iutw) write (iupr,60) filnam(1:lfilnam)
      istat = 1
      go to 110
c
c  determine table id
70    call tablid (iupr,ibug,line,num,icent,isubc,tbid,istat)
      if (istat.gt.0) then
         go to 40
      else if (tbid.eq.'xmrg  ') then
         num = num + 1
         call xmparm (iupr,ibug,line,num,istat)
         mproc = num
      else if (tbid.eq.'qpfwmo'.or.tbid.eq.'QPFWMO') then
         nw1 = 3
         nw2 = 6
         nw3 = 0
         call gbparm (iupr,ibug,line,nw1,nw2,nw3,num,wmo,
     +                dum,istat)
         if (num.ge.1.and.num.le.12) qpfwmo(num) = wmo
      else if (tbid.eq.'128   ') then
         nw1 = 3
         nw2 = 5
         nw3 = 50
         call gbparm (iupr,ibug,line,nw1,nw2,nw3,num,id,des,istat)
         if (num.lt.128.or.num.gt.255) then
            write (iutw,80) num
            if (iupr.ne.iutw) write (iupr,80) num
80    format (' ERROR: table 128 number ',i4,' outside range.')
            else
               abv128(num) = abv
               des128(num) = des
            endif
      else if (tbid.eq.'A     ') then
         nw1 = 3
         nw2 = 50
         nw3 = 0
         call gbparm (iupr,ibug,line,nw1,nw2,nw3,num,des,dum,istat)
         if (num.lt.0.or.num.gt.255) then
            write (iutw,90) num
            if (iupr.ne.iutw) write (iupr,90) num
90    format (' ERROR: process number' ,i4,' outside range.')
            else
               tab_a(num) = des
            endif
      else if (tbid.eq.'C     ') then
         nw2 = 4
         nw3 = 50
         call gbparm (iupr,ibug,line,nw1,nw2,nw3,num,id,des,istat)
         if (num.le.0.or.num.gt.255) then
            write (iutw,100) num
            if (iupr.ne.iutw) write (iupr,100) num
100   format (' ERROR: sub-center number ',i4,' outside range.')
         else
            scid9c(num) = id
            scnam9c(num) = des
         endif
      else
      endif
      go to 40
c
110   close (inp)
c
120   return
c
      end
