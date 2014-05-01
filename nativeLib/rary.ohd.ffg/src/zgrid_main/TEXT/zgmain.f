C$PRAGMA C (CHECK_EXIST)
C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (UDATL)
c  =====================================================================
c  pgm: zgrid
c
C  Program assigns existing threshold runoff values by areas
c  (i.e., zones, counties) to each HRAP bin in the area
c  using the boundary defined for the area.
c
c  Input and control file is the defarea file (user named).
c  Output files are gridro1, gridro3, gridro6, gridro12 and
c  gridro24 which contain the ASCII definitions of gridded
c  threshold runoff values that are input for FFGS and a new
c  area file (user named) which is the new defarea file.
c.......................................................................
c  Initially written
c       Tim Sweeney, HRL                       March 1997
c
c  Runoff parameters added
c       Tim Sweeney, HRL                       Jan 1998
c
c  Added command 'cabb' to use alternate identifier for zone boundary
c  (first sub-area identifer in list of identifiers for 'headwater')
c       Tim Sweeney, HRL                       Feb 1999
c
c  Added option to use values in runoff fields as flash flood
c  guidance.
c       Tim Sweeney, HRL                       Mar 1999
c
c  Added option 'nogr' to output grid parameters (data type gdpm)
c  to exclude ffgid area from grid computations
c       Tim Sweeney, HRL                       May 1999
c
c  Added commands 'rofo' and 'rozo' to function same as 'rofb'
c  and 'rozb' but for area records that do not include the
c  computation control field.
c       Tim Sweeney, HRL                       Jan 2000
c
c  Added command 'mul6'.  Also changed 'rohd' to NOT assign
c  runoffs to grids.
c       Tim Sweeney, HL                        Dec 2000
c
c  Changed so any command will generate new defarea records
c  and no gridded runoff.  'mult' algorithm changed do several
c  functions with factors.  Removed mul1, mul3, mul6.
c       Tim Sweeney, HL                        Apr 2001
c.......................................................................
c
c   keywords used to control program function and inserted at
c   beginning of input file (defarea):
c
c      mult - compute runoff using factors with base set as 1.0
c             3-hour:  mult  0.7  1.0  1.2   0   0
c             6-hour:  mult  0.4  0.66  1.0  1.4  1.73
c             55% of each:  mult  0.55 0.55 0.55  0   0
c
c      equa - used with a mult command above.  Compute a runoff
c  (disabled)   with factor only if runoff equals base runoff
c
c  then runoffs are computed for the grids within the specified
c  boundary.
c
c  additional function - convert area affg records to new
c                        affg record format
c
c      rofb - use ffg area ids to add runoffs to area def records
c             (Use 'rofo' when area records do not include the
c             computation control field.)
c
c      rozb - use zone boundary ids instead
c             (Use 'rozo' when area records do not include the
c             computation control field.)
c
c      rofo, rozo - same as rofb and rozb but for area records
c                   without the computation control field
c
c   additional function - convert headwater hffg records to new
c                         affg record format
c
c      cabb - use first area identifier in list of sub-basins for
c             headwater as zone boundary id
c
c      rohd - use headwater (zone) id to create area affg records
c             with runoffs included
c
c      nopc - replace any 1-hr percentages of 3-hr FFG with
c             actual 1-hr runoffs
c
c      nogr - output grid parameter records (type gdpm) set
c             to exclude the ffgid area
c.......................................................................
c
      subroutine zgrid_main
c
      character*1 resp,deb
      character*1 filfmt
      character*2 cont,bname
      character*4 aid(2),bbtyp,id(2)
      character*4 ctrl
      character*7 timezone
      character*10 filetype
      character*20 appsvar,appsval
      character*30 logfile,namgpm
      character*30 nampre,nam1,nam3,nam6,nam12,nam24
      character*100 format
      character*128 logpath,innam,innam2,anam,anam2,filnam
      character*132 strng
      character line*78,wdesc*22
c
      dimension mdy(6)
      dimension jdura(5)
      dimension fac(5)
      parameter (mro=5)
      dimension ro(mro)
c
      include 'updaio'
      COMMON /CMZGRID/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      include 'ffg_inc/arparm'
      include 'ffg_inc/hwparm'
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/gbx'
      include 'ffg_inc/linseg'
      include 'ffg_inc/paths'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/zgrid_main/RCS/zgmain.f,v $
     . $',                                                             '
     .$Id: zgmain.f,v 1.10 2004/09/13 17:08:49 scv Exp $
     . $' /
C    ===================================================================
C
      data jdura/ 1,3,6,12,24 /
C
C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
c
      nerr = 0
      nwarn = 0
      igdbug = 0
      ibug = 0
      ilogfile = 0
      ilogopen = 0
      ctrl = 'grid'
      num = 0
      npc = 0
      nbb = 0
      nogr = 0
      iaro = 0
      ndurm = 0
      mdur = 5
      nbasn = 0
      bname = ' '
c
      romsng = -999.0
      call umemst (romsng,ro,mro)
c
c  initialize input/output routine common block
      call upinio
c
c  set print unit for file input/output routines
ccc      uu = iutw
c
c  assign units
      iutr = 5
      iutw = 6
      appsvar='ffg_print_filename'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,filnam,lfilnam)
      if (lfilnam.gt.0) then
         iutw  = 98
         irecl = 0
         filfmt = 'F'
         call upopen (iutw,filnam(1:lfilnam),irecl,filfmt,ierr)
         if (ierr.ne.0) then
            write (iutw,5) ierr,filnam(1:lfilnam)
5     format (' ERROR: status code ',i3,' encountered opening file ',
     +   a,'.')
            nerr = nerr + 1
            endif
         endif
      iue  = iutw
      iul  = 9
      iud  = iutw
      iuin = 8
      iuu  = iuin
      iupr = iutw
      iuoa = 24
      iur1 = 25
      iur3 = iur1 + 1
      iur6 = iur1 + 2
      iur12 = iur1 + 3
      iur24 = iur1 + 4
      iuint = iur1 + 5
c
c  get system date and time
      call udatl (mdy)
      nmo = mdy(3)
      nda = mdy(4)
      nyr = mdy(1)
      nhr = mdy(5)/100
      nmn = mdy(5) - nhr*100
      nsc = mdy(6)/100
c
c  get daylight savings time indicator
      lcptz = 1
      ictz = 0
      call datimz (lcptz,ictz,nmo,nda,nyr,nhr,nmn,kzmo,kzda,kzyr,kzhr,
     +             kzmn,ndawk,idayl,mxday,julda)
c
c  get time zone
      appsvar='TZ'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,timezone,ltimezone)
      if (idayl.eq.1) then
         ltimezone1=5
         ltimezone2=7
         else
            ltimezone1=1
            ltimezone2=3
         endif
c
c  print program information
      idsply = 1
      call prnamv (idsply,iutw)
      write (iutw,15) nmo,nda,nyr,nhr,nmn,nsc,
     +   timezone(ltimezone1:ltimezone2)
      write (iutw,10)
10    format (/ 27x,'CONVERT ZONE/COUNTY RUNOFFS' /
     +          39x,'TO' /
     +          33x,'GRIDDED RUNOFFS')
c
c  get environmental variables
      call envfix
c
      write (iutw,20) ffglvl(1:lffgl),ofslvl(1:lofsl)
20    format (/ 20x,'ffg_level=',a,5x,'ofs_level=',a)
c
c  read control info for Preprocessor Parameteric Data Base
      call rpppco (istat)
c
      if (pgmnam.eq.' ') then
         write (iutw,'(/a)') 'ERROR: variable pgmnam is blank.'
         go to 460
         endif
c
c  set log filename
      appsvar='ffg_log_name'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      if (lappsval.gt.0) then
         logfile=appsval
         else
            call ucp2lc (pgmnam,strng,ierr)
            lstrng=lenstr(strng)
            logfile=' '
            appsvar='ffg_log_prefix'
            lappsvar=lenstr(appsvar)
            call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
            if (lappsval.gt.0) then
               call ucncat (logfile,appsval(1:lappsval),ierr)
               call ucncat (logfile,'_',ierr)
               endif
            call ucncat (logfile,strng(1:lstrng),ierr)
            call ucncat (logfile,'_log',ierr)
         endif
      llogfile=lenstr(logfile)
c
c  check logfile option
      if (iupr.eq.iul) then
         call opnlog (ilogopen,logfile,logpath,istat)
         llogpath=lenstr(logpath)
         if (istat.eq.0) then
            ilogfile = 1
            write (iutw,*) 'LOGFILE ON'
            write (iutw,50) logfile(1:llogfile),'opened'
50    format (' NOTE: file ',a,' ',a,'.')
            idsply = 1
            call prnamv (idsply,iupr)
            write (iupr,10)
            write (iupr,20) ffglvl(1:lffgl),ofslvl(1:lofsl)
            write (iupr,15) nmo,nda,nyr,nhr,nmn,nsc,
     +         timezone(ltimezone1:ltimezone2)
15    format (/ 10x,5x,3x,'RUN DATE = ',i2.2,'/',i2.2,'/',i4,' - ',
     +   i2.2,':',i2.2,':',i2.2,' ',a)
            else
               write (iutw,17) logpath(1:llogpath)
17    format (/ ' ERROR: cannot open file ',a,'.')
               iupr = iutw
               go to 450
            endif
         endif
c
c  read option
30    write (iutw,*)
      format='('' Enter d-debug'
      call ucncat (format,', l-logfile',istat)
      call ucncat (format,', q-quit',istat)
      call ucncat (format,' or <return>-continue: '',$)',istat)
      write (iutw,format)
      read (iutr,'(a)',end=450) cont
      if (cont(1:1).eq.'#') go to 30
      resp = cont(1:1)
      deb = cont(2:2)
      if (resp.eq.'q') go to 450
      if (resp.eq.'d') then
         igdbug = 1
         if (deb.eq.'1') then
            ibug = 1
            else if (deb.eq.'2') then
               ibug = 2
            else if (deb.eq.'3') then
               ibug = 3
            else if (deb.eq.'4') then
               ibug = 4
            else
               ibug = 1
            endif
         igdbug = ibug
         go to 30
         else if (resp.eq.'l') then
            if (ilogfile.eq.0) then
c           open logfile
               call opnlog (ilogopen,logfile,logpath,istat)
               if (istat.eq.0) then
                  ilogfile = 1
                  write (iutw,*) 'LOGFILE ON'
                  write (iutw,50) logfile(1:llogfile),'opened'
                  idsply = 1
                  call prnamv (idsply,iupr)
                  write (iupr,10)
                  write (iupr,20) ffglvl(1:lffgl),ofslvl(1:lofsl)
                  write (iupr,15) nmo,nda,nyr,nhr,nmn,nsc,
     +               timezone(ltimezone1:ltimezone2)
                  endif
               go to 30
               endif
            if (ilogfile.eq.1) then
               iupr = iutw
               ilogfile = 0
               write (iutw,*) 'LOGFILE OFF'
               go to 30
               endif
            go to 30
         else
         if (resp.ne.' ') then
            write (iutw,40) cont(1:lenstr(cont))
40    format (' ERROR: ',a,' is an invalid option.')
            go to 30
            endif
         endif
c
      if (igdbug.gt.0) write (iutw,*) 'DEBUG=',igdbug
c
      fac(1) = 0.70
      fac(2) = 1.00
      fac(3) = 1.5
      fac(4) = 2.0
      fac(5) = 2.5
c
c.......................................................................
c
c  get old file name
70    strng='old'
      write (iutw,80) strng(1:lenstr(strng))
80    format (' Enter pathname of ',a,' area runoff define file: ',$)
      read (iutr,'(a)',end=450) innam
      if (innam.eq.' ') go to 30
      if (innam(1:1).eq.'#') go to 70
      call uindex (innam,len(innam),'#',1,lfound)
      if (lfound.gt.0) then
         innam2=innam(1:lfound-1)
         innam=innam2
         endif
      if (innam.eq.'.') then
         innam='defarea'
         write (iutw,90) strng(1:lenstr(strng)),innam(1:lenstr(innam))
90    format (' NOTE: ',a,' file name set to ',a,'.')
         endif
c
c  check if file exists
      iprint=0
      filnam=innam(1:lenstr(innam))//char(0)
      filetype='file'//char(0)
      call check_exist (filnam,filetype,iexist,iprint)
      if (iexist.eq.0) then
         write (iutw,100) innam(1:lenstr(innam))
         if (iupr.ne.iutw) write (iupr,100) innam(1:lenstr(innam))
100   format (' ERROR: pathname ',a,' not found.')
         go to 70
         endif
c  open file
      open (iuin,access='sequential',form='formatted',
     +      file=innam,status='unknown',iostat=ios)
      if (ios.eq.0) then
         if (igdbug.gt.0) write (iud,50) innam(1:lenstr(innam))
         else
            write (iutw,60) innam(1:lenstr(innam))
            if (iupr.ne.iutw) write (iupr,60) innam(1:lenstr(innam))
60    format (' ERROR: cannot open file ',a,'.')
            nerr = nerr + 1
            go to 70
         endif
c
c  get new file name
110   strng='new'
      write (iutw,80) strng(1:lenstr(strng))
      read (iutr,'(a)',end=450) anam
      if (anam.eq.' ') go to 30
      if (anam(1:1).eq.'#') go to 110
      if (anam.eq.'.') then
         anam='defarea_new'
         write (iutw,90) strng(1:lenstr(strng)),anam(1:lenstr(anam))
         endif
      call uindex (anam,len(anam),'#',1,lfound)
      if (lfound.gt.0) then
         anam2=anam(1:lfound-1)
         anam=anam2
         endif
      if (innam.eq.anam) then
         write (iue,120)
120   format (' ERROR: input and output pathnames are the same.')
         go to 70
         endif
c  open file
      open (iuoa,access='sequential',form='formatted',
     +      file=anam,status='unknown',iostat=ios)
      if (ios.eq.0) then
         if (igdbug.gt.0) write (iud,50) anam(1:lenstr(anam))
         else
            write (iutw,60) anam(1:lenstr(anam))
            if (iupr.ne.iutw) write (iupr,60) anam(1:lenstr(anam))
            nerr = nerr + 1
            go to 110
         endif
c
c  set prefix for HRAP gridded runoff file
      appsvar='zgrid_file_prefix'
      lappsvar=lenstr(appsvar)
      appsval=' '
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      if (lappsval.gt.0) then
         nampre=appsval(1:lappsval)
         call ucncat (nampre,'_gridro',ierr)
         else
            nampre='gridro'
         endif
      lnampre=lenstr(nampre)
c
c  open HRAP gridded runoff file for 1 hour
      nam1 = nampre(1:lnampre)//'01'
      open (iur1,access='sequential',form='formatted',
     +      file=nam1,status='unknown',iostat=ios)
      if (ios.eq.0) then
         write (iutw,50) nam1(1:lenstr(nam1)),'opened'
         else
            write (iupr,60) nam1(1:lenstr(nam1))
            nerr = nerr + 1
            go to 450
         endif
c
c  open HRAP gridded runoff file for 3 hours
      nam3 = nampre(1:lnampre)//'03'
      open (iur3,access='sequential',form='formatted',
     +      file=nam3,status='unknown',iostat=ios)
      if (ios.eq.0) then
         write (iutw,50) nam3(1:lenstr(nam3)),'opened'
         else
            write (iupr,60) nam3(1:lenstr(nam3))
            nerr = nerr + 1
            go to 450
         endif
c
c  open HRAP gridded runoff file for 6 hours
      nam6 = nampre(1:lnampre)//'06'
      open (iur6,access='sequential',form='formatted',
     +      file=nam6,status='unknown',iostat=ios)
      if (ios.eq.0) then
         write (iutw,50) nam6(1:lenstr(nam6)),'opened'
         else
            write (iupr,60) nam6(1:lenstr(nam6))
            nerr = nerr + 1
            go to 450
         endif
c
c  open HRAP gridded runoff file for 12 hours
      nam12 = nampre(1:lnampre)//'12'
      open (iur12,access='sequential',form='formatted',
     +      file=nam12,status='unknown',iostat=ios)
      if (ios.eq.0) then
         write (iutw,50) nam12(1:lenstr(nam12)),'opened'
         else
            write (iupr,60) nam12(1:lenstr(nam12))
            nerr = nerr + 1
            go to 450
         endif
c
c  open HRAP gridded runoff file for 24 hours
      nam24 = nampre(1:lnampre)//'24'
      open (iur24,access='sequential',form='formatted',
     +      file=nam24,status='unknown',iostat=ios)
      if (ios.eq.0) then
         write (iutw,50) nam24(1:lenstr(nam24)),'opened'
         else
            write (iupr,60) nam24(1:lenstr(nam24))
            nerr = nerr + 1
            go to 450
         endif
c
c.......................................................................
c
130   if (ctrl.eq.'mult'.or.ctrl.eq.'grid') then
         call pinare (aid,istat)
         if (ibug.eq.1) write (iud,*) 'in zgrid - aid=',aid,
     *      ' istat=',istat
         else if (ctrl.eq.'rofb'.or.ctrl.eq.'rozb') then
c        read area definition input file without runoff values
            call pinar3 (aid,istat)
         else if (ctrl.eq.'rofo'.or.ctrl.eq.'rozo') then
c        read area definition input file without computation control
c        field and runoff values
            call pinar2 (aid,istat)
         else
            call pinhed (aid,istat)
         endif
      if (istat.eq.1) then
c     end of file
         go to 410
         else if (istat.eq.2) then
c        keyword found
            backspace (iuin)
            read (iuin,'(a)',end=410,err=390) line
            istat = 0
            ndur = 5
            iptr = 1
            nwid = 4
            call uffch (line,iptr,nwid,ctrl,nxt,istat)
            if (istat.gt.0) go to 390
            if (ctrl.eq.'MULT'.or.ctrl.eq.'mult') then
               ctrl = 'mult'
ccc            else if (ctrl.eq.'MUL1'.or.ctrl.eq.'mul1') then
ccc               ctrl = 'mul1'
ccc               ndur = 1
ccc            else if (ctrl.eq.'MUL3'.or.ctrl.eq.'mul3') then
ccc               ctrl = 'mul3'
ccc            else if (ctrl.eq.'MUL6'.or.ctrl.eq.'mul6') then
ccc               ctrl = 'mul6'
            else if (ctrl.eq.'ZEX3'.or.ctrl.eq.'zex3') then
            ctrl = 'zex3'
            else if (ctrl.eq.'ROFB'.or.ctrl.eq.'rofb')then
               ctrl = 'rofb'
               go to 130
            else if (ctrl.eq.'ROZB'.or.ctrl.eq.'rozb') then
               ctrl = 'rozb'
               go to 130
            else if (ctrl.eq.'ROFO'.or.ctrl.eq.'rofo')then
               ctrl = 'rofo'
               go to 130
            else if (ctrl.eq.'ROZO'.or.ctrl.eq.'rozo') then
               ctrl = 'rozo'
               go to 130
            else if (ctrl.eq.'ROHD'.or.ctrl.eq.'rohd') then
               ctrl = 'rohd'
               go to 130
            else if (ctrl.eq.'NOPC'.or.ctrl.eq.'nopc') then
               npc = 1
               go to 130
            else if (ctrl.eq.'CABB'.or.ctrl.eq.'cabb') then
               nbb = 1
               go to 130
            else if (ctrl.eq.'NOGR'.or.ctrl.eq.'nogr') then
               nogr = 1
               go to 130
ccc            else if (ctrl.eq.'EQUA'.or.ctrl.eq.'equa') then
ccc               ieq = 1
ccc               go to 500
            else
               go to 130
            endif
c        parse multiplier
            nwid = 8
            do 140 i=1,ndur
               iptr = nxt
               call uffir (line,iptr,nwid,k,fac(i),nxt,istat)
               if (istat.gt.0) go to 390
140            continue
            go to 130
         else if (istat.eq.3) then
c        error
            go to 130
         endif
c
c  convert area def records - add runoff to earlier version
      if (ctrl.eq.'rofb'.or.ctrl.eq.'rofo') then
         id(1) = aid(1)
         id(2) = aid(2)
         else if (ctrl.eq.'rozb'.or.ctrl.eq.'rozo') then
            id(1) = bbid(1)
            id(2) = bbid(2)
         else
            go to 180
         endif
      iropta = 0
      call findro (id,kadurf,ro,iur1,nro,istat)
      if (istat.eq.0) go to 340
      if (istat.eq.1) then
         write (iupr,150) aid,bbid
150   format (' ERROR: unexpected end of file encountered reading ',
     +   ' runoff for area ',2a4,' using bbid ',2a4,'.')
         nerr = nerr + 1
         go to 130
         endif
      if (istat.eq.2) then
         write (iupr,160) aid,bbid,nro
160   format (' ERROR: runoff not found for area ',2a4,
     +   ' using bbid ',2a4,' for duration ',i2,'.')
         nerr = nerr + 1
         go to 130
         endif
      if (istat.eq.3) then
         write (iupr,170) aid,bbid,nro
170   format (' WARNING: runoff not found for area ',2a4,
     +   ' using bbid ',2a4,'. ro(',i2,') set to 0.0')
         nwarn = nwarn + 1
         go to 340
         endif
      go to 130
c
c  convert headwater hffg def records to area def records
180   if (ctrl.eq.'rohd') then
         do 200 i=1,5
            adesc(i) = desc(i)
200         continue
         bbid(1) = aid(1)
         bbid(2) = aid(2)
c     get runoffs
         ndur = 5
         do 190 i=1,ndur
            if (fsflow.lt.10.0) then
c           runoffs
               upk(i) = upk(i)/100.0
               else
c              unitgraph peaks
                  if (upk(i).ge.1.0) then
                     upk(i) = fsflow/upk(i)
                     else
                        upk(i) = upk(i)/100.0
                     endif
               endif
            ro(i) = upk(i)
190         continue
         kadurf = ndur - 3
         iropta = 0
         go to 340
         endif
c
c   compute runoffs from affg definition
      ndur = kadurf + 3
c
c  check if to output runoff values
      do 210 i=1,mro
         if (aro(i).ne.-99.0) iaro=1
210      continue
      if (iropta.eq.3.or.iaro.eq.1) then
         do 220 i=1,mro
            ro(i) = aro(i)
220         continue
         go to 250
         endif
c
c  compute runoffs using factors from 'mult' command
      if (ctrl.eq.'mult') THEN
         do 240 i=1,ndur
            if (fac(i).eq.1.0.or.fac(i).eq.0.0) then
               ro(i) = aro(i)
               else
                  ro(i) = fac(i)*aro(i)
c              check for fac = 1
                  do 230 k=1,ndur
                     if (fac(k).eq.1.0) ro(i) = fac(i)*aro(k)
230                  continue
               endif
240         continue
         go to 340
         endif
c
ccc      if (ctrl.eq.'mult' .or. ctrl.eq.'mul1') then
ccc         if (aro(i).gt.0.0) then
ccc            ro(i) = aro(i)
ccc         else
ccc            ro(i) = fac(i)*aro(2)
ccc         endif
ccc      else if (ctrl.eq.'mul3'.or.ctrl.eq.'zex3') then
ccc         if (ieq.eq.0) then
ccc            ro(i) = fac(i)*aro(2)
ccc         else
ccc            if (ro(i).eq.aro(2)) then
ccc               ro(i) = fac(i)*aro(2)
ccc            else
ccc               ro(i) = aro(i)
ccc            endif
ccc         endif
ccc      else if (ctrl.eq.'mul6') then
ccc         if (ieq.eq.0) then
ccc            ro(i) = fac(i)*aro(3)
ccc         else
ccc            if (aro(i).eq.aro(3)) then
ccc               ro(i) = fac(i)*aro(3)
ccc            else
ccc               ro(i) = aro(i)
ccc            endif
ccc         endif
ccc      endif
ccc
ccc  recompute 1-hr runoff
ccc      if (ctrl.eq.'mul1') ro(1) = fac(1)*aro(2)
ccc
ccc  reset negative 1-hr runoff (1-hr ffg as percent of 3-hr ff)
ccc  to actual 1-hr runoff
ccc      if (npc.eq.1.and.ro(1).lt.0.0) ro(1) = fac(1)*ro(2)
c
c  get basin boundary (line segment definitions)
250   if (bbid(1).eq.'NONE'.or.bbid(1).eq.'none') then
         bbid(1) = aid(1)
         bbid(2) = aid(2)
         endif
c
c  use first area id in headwater def as zone boundary id
      if (nbb.gt.0) then
         bbid(1) = arid(1,1)
         bbid(2) = arid(2,1)
         endif
      bbtyp = 'BASN'
      call getbb (bbid,bbtyp,mbx,bx,ibx,mlseg,nlseg,nlrow,
     +   nlbeg,nlend,istat)
      if (istat.ne.0) then
         if (ibug.eq.1) write (iud,*) 'in zgrid - bbid=',bbid,
     *      ' istat=',istat
         nerr = nerr + 1
         go to 130
         endif
      if (nbasn.eq.0) write (iupr,*)
      nbasn = nbasn + 1
      write (iupr,260) bbid,aid,adesc
260   format (' NOTE: processing basin ',2a4,
     *   ' for AFFG identifier ',2a4,
     *   ' and description ',5a4,'.')
c
      do 280 i=1,ndur
         iudur = iur1 - 1 + i
         write (iudur,270) aid,adesc
270   format (4x,2a4,16x,5a4)
280      continue
c
      if (ndurm.lt.ndur) ndurm=ndur
c
      if (igdbug.gt.0) write (iud,290) aid,bbid,ro(2),adesc
290   format (' aid=',2a4,' bbid=',2a4,' ro(2)=',f5.2,
     +   ' adesc=',5a4)
C
C  process each line segment in area
      DO 330 lseg=1,nlseg
         nrow  = nlrow(lseg)
         ncbeg = nlbeg(lseg)
         ncend = nlend(lseg)
         if (igdbug.gt.2) write (iud,300) lseg,nrow,ncbeg,ncend
300   format (' lseg=',i4,' nrow=',i5,' ncbeg=',i5,' ncend=',i5)
ccc      number of columns in the line segment (row)
ccc         if (ncend.ge.ncbeg) then
ccc            nok = ncend - ncbeg + 1
ccc            else
ccc            nok = ncend - ncbeg - 1
ccc            endif
c     output gridded values for the specified durations
         do 320 n=1,ndur
            iudur = iur1 - 1 + n
            idelt = jdura(n)
            write (iudur,310) idelt,nrow,ncbeg,ncend,ro(n)
310   format (i4,' |||',i5,1x,2i4,f6.3)
320         continue
330      continue
c
c  insert delimiters in desc field if needed
340   if (adesc(1).eq.' ') adesc(1) = 'none'
      call adelim (adesc,5,ld,wdesc)
c
c  zero runoffs except 3-hr
      if (ctrl.eq.'zex3') then
         ro(1) = 0.0
         do 350 i=3,5
            ro(i) = 0.0
350         continue
         endif
c
      if (ro(1).eq.romsng) then
         write (iupr,360) aid
360      format (' ERROR: runoff values are missing for area ',2a4,'.')
         nerr = nerr + 1
         endif
c
c  output area def parameters for this zone/county
      call ucp2lc ('AFFG',strng,istat)
      if (kadurf.le.1) ro(5) = 0.0
      if (kadurf.le.0) ro(4) = 0.0
      write (iuoa,370) strng(1:lenstr(strng)),
     +   aid,wdesc(1:ld),iropta,kadurf,bbid,ro
370   format (a,' ',2a4,1x,a,2(1x,i1),1x,2a4,5f6.2)
      num = num + 1
c
c  output grid parameters by MAP
      if (iabs(nogr).eq.1) then
         if (nogr.eq.1) then
c        grid parameter files for grids
            namgpm = ofslvl(1:lofsl)//'_gridparm_new'
            open (iuint,access='sequential',form='formatted',
     +            file=namgpm,status='unknown',iostat=ios)
            if (ios.eq.0) then
               write (iutw,50) namgpm(1:lenstr(namgpm)),'opened'
               else
                  write (iutw,60) namgpm(1:lenstr(namgpm))
                  nerr = nerr + 1
                  go to 450
                endif
            nogr=-1
            endif
         iqoptg = 0
         inoptg = 9
         bank = 1.10
         write (iuint,380) aid,iqoptg,inoptg,bank
380      format ('gdpm',1x,2a4,3x,i2,4x,i2,f6.2)
         endif
c
      go to 130
c
c.......................................................................
c
390   write (iutw,400) istat
400   format (' ERROR: encountered reading input file: istat=',i4)
c
410   if (ibug.eq.1) write (iud,*) 'in zgrid - ndurm=',ndurm,
     +   ' mdur=',mdur
      do 420 i=1,ndurm
         iudur = iur1 - 1 + i
         inquire (iudur,iostat=iostat,name=filnam)
         close (iudur,iostat=iostat)
         write (iutw,50) filnam(1:lenstr(filnam)),'closed'
420      continue
c
c  delete any files not written to
      if (ndurm.lt.mdur) then
         do 430 i=ndur+1,mdur
            iudur = iur1 - 1 + i
            inquire (iudur,iostat=iostat,name=filnam)
            close (iudur,status='delete')
            write (iutw,50) filnam(1:lenstr(filnam)),'deleted'
430         continue
         endif
c
      write (iutw,440) num,
     +   innam(1:lenstr(innam)),
     +   anam(1:lenstr(anam)),
     +   nerr,nwarn
      if (iupr.ne.iutw) write (iupr,440) num,
     +   innam(1:lenstr(innam)),
     +   anam(1:lenstr(anam)),
     +   nerr,nwarn
440   format (/ 5x,'Converted runoffs to HRAP grid and ',
     +             'AFFG parameters for ',i4,' areas.' /
     +          5x,'Old area define file: ',a /
     +          5x,'New area define file: ',a //
     +          5x,'ERRORS=',i4,5x,'WARNINGS=',i4)
c
450   strng=' Program '//pgmnam(1:lenstr(pgmnam))//' completed.'
      write (iutw,'(/a)') strng(1:lenstr(strng))
      if (ilogopen.eq.1) then
         if (iupr.ne.iutw) write (iupr,'(/a)') strng(1:lenstr(strng))
         endif
c
460   if (iupr.ne.iutw) then
         call upclos (iupr,bname,ic)
         endif
c
      stop
c
      end
c
c  =====================================================================
c
      subroutine findro (bbid,kadurf,ro,iur1,n,istat)
c
c  routine searches for matching identifier in gridro files
c  and then locates runoff for each duration which are
c  appended to defarea records.
c
c   in: bbid   .... boundary identifier
c   in: kadurf .... duration flag
c  out: ro     .... runoff
c  out: iur1   .... unit number of file gridro1
c  out: istat  .... completion code
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
      character*8 bbid,id,wid
      dimension ro(5),is(5)
c
      call prbug ('findro',1,1,ibug)
c
      istat = 0
      ndur = kadurf + 3
c
c  process each duration
      do 70 n=1,ndur
         if (ibug.gt.0) write (iud,10) bbid,n
10    format (' bbid= ',a,' n=',i3)
         is(n) = 0
         iudur = iur1 - 1 + n
c     read file
20       read (iudur,30,end=60,err=80) idelt,id
30       format (i4,a,10x,f6.3)
         if (ibug.gt.0) write (iud,40) 'checking for',n,idelt,id
40    format (' ',a,' : n=',i2,' idelt=',i4,' id=',a)
         if (id.ne.bbid) go to 20
         if (ibug.gt.0) write (iud,40) 'douns',n,idelt,id
         read (iu,30) idelt,wid,ro(n)
         if (ibug.gt.0) write (iud,50) n,idelt,wid,ro(n)
50    format (' n=',i2,' idelt=',i4,' wid=',a,' ro(n)= ',f6.3)
         go to 70
60       is(n) = is(n) + 1
         if (is(n).lt.2) then
            rewind (iu)
            go to 20
            else
               if (n.eq.2) then
                  istat = 2
                  go to 90
               else if (n.le.3) then
                  ro(n) = 0.0
                  istat = 3
                  go to 70
               endif
            endif
70       continue
      go to 90
c
80    istat = 1
c
90    return
c
      end
c
c  =====================================================================
c
      subroutine uline
      return
      end
