C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (UDATL)
C$PRAGMA C (GBF_WOPN)
C$PRAGMA C (GBF_CLOS)
C$PRAGMA C (GBF_ROPN)
C$PRAGMA C (GBF_READ)
C$PRAGMA C (CHECK_EXIST)
c  Encode xmrg file(s) in GRIB.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                                Jan 2000
c
c  Modified by
c        David T. Miller                                 Nov 2007
c
c     The 1/4 HRAP grid used by the High-resolution Precipication
c     Estimator (HPE) could be many more grid points over an RFC than
c     the large common block included by gribsz could account for,
c     by factors of 16 plus extra.  Stack crashes resulted for RFC
c     areas.  Therefore, decided to use dynamic memory
c     allocation instead of the large common block.  For
c     WFOs, the initial values used before x 16 work okay.  However,
c     for large RFCs, need to reallocate memory to account for
c     larger area.  Hence the need to read the XMRG header first
c     in order to make that determination.
c
c     For this version of gribit, needed to create new but similar
c     routines so that the 1/4 HRAP grid factor could be used.
c     These all end in g.f.  For example, one of the original programs
c     was cvllgd.f but for this version of gribit, the modified version
c     of the routine is called cvllgdg.f.  This was needed so the
c     libraries and routines that called the original routines didn't
c     have to change.
c
c     Also, removed mention to a call to SUBZERO as that's has been
c     removed and another area makes the determination on the subcenter
c     number.
c
c     In addition, during OB7.2, there was a problem with the version number
c     and date of the version not printing out properly for some options.  The
c     program name, version, and version date is normally setup with by the
c     Block Data file called bdgribit.f.  However, apparently, there are some
c     options where these values weren't used.  Therefore, a write statement
c     in gbitmain.f is hardcoded with the values.  When modifying versions, one
c     should change this write format statement (10  format) as well as the values
c     in bdgribit.f.
c
c.......................................................................
c


      subroutine gribit_main

c
      include 'upvrsx'
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
c  DTM Nov 07 - grid factor to account for 1/4 HRAP
c
      real*8 gridf

c
c  DTM Nov 07 -original big common block was called gribz
c  In this version, allocating memory from the heap
c  instead because array sizes are very large with
c  1/4 HRAP grid
c

      character*1 cvgrib,fmt,sfun
      character*3 ron
      character*4 pid(3),cct,wmo(2),senof
      character*25 appsvar
      character*75 logfile
      character*128 appsval
      character*128 xmrgfile,gribfile
      character*128 ans,logpath,gribdiri,gribdiro,gribpath,xmrgpath
      character user*8,sdatim*20,proces*8,vdatim*20
      character*132 strng
c
      character(LEN=1), allocatable,save :: kbuf(:)
      logical(KIND=1), allocatable,save :: kbms(:)
      integer(KIND=2), allocatable,save :: ihfld(:)
      integer, allocatable,save :: ifld(:), ibmap(:)
      real, allocatable,save :: fld(:), wfld(:)
      integer lappsval, lappsvar

       integer, save :: mxbmap
       integer, save :: kbufsz
       integer, save :: irdxmrg

       integer, save :: mdy(6), igds(18)
       integer, parameter :: mkptr = 20
       integer, parameter :: mkpds = 25
       integer, parameter :: mkgds = 20

       integer, save :: kptr(mkptr)
       integer, save :: kpds(mkpds)
       integer, save :: kgds(mkgds)

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob83/ohd/pproc/src/gribit/RCS/gbitmain.f,v $
     . $',                                                      '
     .$Id: gbitmain.f,v 1.8 2007/11/02 15:45:36 millerd Exp $
     . $' /
C    ===================================================================
C
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
C
C  initial values
      ibug = 0
      igdbug = 0
      igtrac = 0
      igerr = 1
      cvgrib = '1'
      mihdr = 3
      inat = 1
      iread = 1
      ivrb = 0
      isearch = 0
      sfun = ' '
      iprgrib=0
      ilogfile = 0
      ilogfilez = 0
      iallocate = 0
      iaerr = 0
      senof = '    '
c
c  default values of allocatable arrays
c
      kbufsz = 4200000
      mxbmap = 3500000

      ncolxnrow = mxbmap

c  default to normal grid unless 1/4 HRAP is being used
c
      gridf = 1.
      lappsvar = 0
      lappsval = 0
      loc = 0

c
c  set unit numbers
      iutr  = 5
      iutw  = 6
      iul   = 9
      iue   = iutw
      iud   = iutw
      iupr  = iutw
      iuxmrg = 22
      iugrib = 24
      iudm  = 26
c
c  DTM Nov 07 - begin initial allocation of arrrays
c
      allocate(kbuf(kbufsz),stat=iaerr)
      if(iaerr .ne. 0) then
         iallocate=iallocate+1
         write(iutw,5) iallocate
 5       format(' ERROR Allocation section: ',i3)
         goto 250
      endif
      allocate(kbms(mxbmap), stat=iaerr)
      if(iaerr .ne. 0) then
         iallocate=iallocate+1
         write(iutw,5) iallocate
         goto 250
      endif
      allocate(ihfld(mxbmap), stat=iaerr)
      if(iaerr .ne. 0) then
         iallocate=iallocate+1
         write(iutw,5) iallocate
         goto 250
      endif
      allocate(fld(mxbmap), stat=iaerr)
      if(iaerr .ne. 0) then
         iallocate=iallocate+1
         write(iutw,5) iallocate
         goto 250
      endif
      allocate(wfld(mxbmap), stat=iaerr)
      if(iaerr .ne. 0) then
         iallocate=iallocate+1
         write(iutw,5) iallocate
         goto 250
      endif
      allocate(ifld(mxbmap), stat=iaerr)
      if(iaerr .ne. 0) then
         iallocate=iallocate+1
         write(iutw,5) iallocate
         goto 250
      endif
      allocate(ibmap(mxbmap), stat=iaerr)
      if(iaerr .ne. 0) then
         iallocate=iallocate+1
         write(iutw,5) iallocate
         goto 250
      endif

c  get system date and time (mdy(1) is 4 digit year)
      call udatl(mdy)
      nmo = mdy(3)
      nda = mdy(4)
      nyr = mdy(1)
      nhr = mdy(5)/100
      nmn = mdy(5) - nhr*100
      nsc = mdy(6)/100
c
c  check if to show menu and prompts
      appsvar='grib_verbose'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      if (appsval(1:3).eq.'yes') ivrb = 1
c
c  get grib input and output directory names
      appsvar='grib_in_dir'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,gribdiri,lgribdiri)
      appsvar='grib_out_dir'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,gribdiro,lgribdiro)
c
c  check if grib input file name specified
      appsvar='grib_in_file'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      if (lappsval.eq.0) then
         ivrb = 1
         else
            iread = 0
         endif
c
c  check if grib output file name specified
      appsvar='grib_out_file'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,gribfile,lgribfile)
c
c  check if log file name specified
      appsvar='grib_log_file'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,logfile,llogfile)
      if (llogfile.eq.0) then
         logfile='gribit_log'
         llogfile = lenstr(logfile)
         endif
c
      if (ivrb.eq.1) then
         write (iutw,10)
c
c  This should be either removed or used in the next rewrite of
c  gribit.  For the moment, not using it at all.  DTM 7/9/08
c
c         write (iutw,10) pgmnam(1:lenstr(pgmnam)),
c     +                   pgmvrn(1:lenstr(pgmvrn)),
c     +                   pgmvrd(1:lenstr(pgmvrd))
10    format (' PROGRAM Gribit (VERSION: OB8.3 - 11/14/2007)' //
     +        5x,'GRIB Encoder/Decoder Program')
c
c   DTM Nov 07 - note that this inforamtion also needs to be changed in
c   the block data file bdgribit.f.  One can tell whether the
c   info in the block data is being printed vs the format statement
c   above because the version will be in lower case.
c
c   Ideally the statement below should be used in conjunction with
c   the block data file and does seem to work.  But, due to problems
c   in the past with it not always working, it is commented out and
c   left for background information.

c10    format (' PROGRAM ',a,' (VERSION: ',a,' - ',a,')' //
c     +        5x,'GRIB Encoder/Decoder Program')
         write (iutw,20) nmo,nda,nyr,nhr,nmn,nsc
20    format (/ ' RUN DATE = ',i2.2,'/',i2.2,'/',i4.4,' - ',
     +        i2.2,':',i2.2,':',i2.2)
         write (iutw,30) gribdiri(1:lgribdiri),gribdiro(1:lgribdiro)
30    format (/ ' grib_in_dir  = ',a /
     +          ' grib_out_dir = ',a)
         write (iutw,*)
         endif
c
c  check if error option specified
      appsvar='grib_error_output'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      ron = appsval(1:3)
      if (ron.eq.'on'.or.ron.eq.'ON'.or.ron.eq.'On') igerr = 1
      if (ron.eq.'off'.or.ron.eq.'OFF'.or.ron.eq.'Off') igerr = 0
c
c  check if debug option specified
      appsvar='grib_debug_output'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      ron = appsval(1:2)
      if (ron.eq.'on'.or.ron.eq.'ON'.or.ron.eq.'On') igdbug = 1
      if (appsval(1:1).eq.'d'.and.appsval(2:2).ne.' ') then
         if (appsval(2:2).eq.'1') then
            igdbug = 1
            else if (appsval(2:2).eq.'2') then
               igdbug = 2
            else if (appsval(2:2).eq.'3') then
               igdbug = 3
            else if (appsval(2:2).eq.'4') then
               igdbug = 4
            else
               igdbug = 1
            endif
         endif
c
c  check if log option specified
      appsvar='grib_log_output'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      ron = appsval(1:2)
      if (ron.eq.'on'.or.ron.eq.'ON'.or.ron.eq.'On') then
         iupr = iul
         iue = iul
         iud = iul
         endif
c
c  check if program control option specified
      appsvar='grib_ctl'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      if (appsval(1:1).eq.'x') then
         sfun = 'x'
         else if (appsval(1:1).eq.'u') then
            sfun = 'u'
            iupr = iul
            iprgrib = 1
         endif
c
c  check if parameter table search sequence option specified
      appsvar='grib_ptbl_search'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      if (appsval(1:1).eq.'1') isearch = 1
c
c  check if apps_defaults defined
      if (lgribdiri.le.1.or.lgribdiro.le.1) then
         write (iutw,40)
40    format (' ERROR: apps_defaults grib_in_dir and/or ',
     *   'grib_out_dir not specified.')
         go to 360
         endif
c
c  load tables with default parameters
      call loadtbl (iupr,igdbug,istat)
c
c  update parameter tables if file exists
      iunit = 99
      call pintbl (iunit,iutw,iupr,igdbug,mptnum,istat)
c
      ifirst=1
c
c  check log file open
50    if (ilogfile.eq.1) go to 80

c  check if to write to log file
      if (iupr.eq.iul) then
         iuprh = iupr
         iupr  = iutw
         iud   = iutw
         iue   = iutw
c     open logfile
         logpath = gribdiro(1:lgribdiro)
     +             //'/'
     +             //logfile(1:llogfile)
     +             //char(0)
         llogpath=lenstr(logpath)
         open (iul,file=logpath,status='unknown',
     +        access='sequential',form='formatted',iostat=iostat)
         if (iostat.eq.0) then
            if (ivrb.eq.1) write (iupr,60) logpath(1:llogpath)
60    format (' NOTE: file ',a,' opened.')
            ilogfile = 1
            ilogfilez = 1
            iupr = iuprh
            iue  = iuprh
            iud  = iuprh
            else
               write (iutw,70) logpath(1:llogpath),'iostat',iostat
70    format (' ERROR: in main : unable to open file ',a,'. ',a,'=',i3)
               if (iupr.ne.iutw) write (iupr,70) logpath(1:llogpath),
     +            'iostat',iostat
            endif
         endif
c
80    if (ilogfile.eq.1.and.ifirst.eq.1) then
         ifirst=0
         write (iupr,10)
c
c  This should be either removed or used in the next rewrite of
c  gribit.  For the moment, not using it at all.  DTM 7/9/08
c
c
c         write (iupr,10) pgmnam(1:lenstr(pgmnam)),
c     +                   pgmvrn(1:lenstr(pgmvrn)),
c     +                   pgmvrd(1:lenstr(pgmvrd))
         write (iupr,20) nmo,nda,nyr,nhr,nmn,nsc
         write (iupr,30) gribdiri(1:lgribdiri),gribdiro(1:lgribdiro)
         endif
c
      if (igdbug.gt.0) write (iutw,90) igdbug
90    format (' DEBUG=',i2)
      if (ivrb.eq.1) then
         if (igerr.gt.0) write (iutw,*) 'ERROR=ON'
         if (ilogfile.eq.1) then
            write (iutw,*) 'LOGFILE=ON'
            write (iutw,100) logpath(1:llogpath)
100   format (' NOTE: log information will be written to file ',a,'.')
            endif
         endif
c
      if (ifirst.eq.1) then
         if (cvgrib.eq.' ') then
            write (iutw,110)
110   format (/ ' Select (1 - GRIB (NCEP) or 2 - GRIB2 (TDL)): ',$)
            read (iutr,'(a)') cvgrib
            endif
         endif
c
c  check control
      if (sfun.eq.'x'.or.sfun.eq.'u') go to 260
c
c  check if filename for GRIB encoded output file specified
120   if (iread.eq.1) then
         if (ilogfile.eq.0) strng='on'
         if (ilogfile.eq.1) strng='off'
         write (iutw,130) strng(1:lenstr(strng))
130   format (
     +   / ' OPTIONS:' /
     +        3x,' dn - debug level n = 1, 2 or 3' /
     +        3x,' e  - error messages on' /
     +        3x,' l  - log file ',a /
     +        3x,' v  - show prompts' /
     +     ' FUNCTIONS:' /
     +        3x,'[g] - encode XMRG to GRIB file [default]' /
     +        3x,' u  - unpack GRIB to log file' /
     +        3x,' x  - decode GRIB to XMRG file' /
     +   / ' Enter option, function, GRIB output file name ',
     +        'or <return> to quit: ',$)
         read (iutr,'(a)',end=360) ans
         if (ans.eq.' ') go to 360
         if (ans.eq.'.') then
            if (gribfile.eq.' ') then
               gribfile = 'grib_out_file'
               lgribfile = lenstr(gribfile)
               write (iutw,140) 'GRIB',gribfile(1:lgribfile)
140   format (' NOTE: ',a,' file name set to ',a,'.')
               else
                  write (iutw,145) 'GRIB',gribfile(1:lgribfile)
145   format (' NOTE: ',a,' file name is ',a,'.')
               endif
            else
               gribfile = ans
               lgribfile = lenstr(gribfile)
            endif
         iprgrib=0
         endif
c
      if (lgribfile.le.2) then
ccc         write (*,*) 'gribfile=',gribfile
c     check for option
         if (gribfile(1:6).eq.'decode') go to 260
         if (gribfile(1:2).eq.'d1') then
            igdbug = 1
            go to 50
            endif
         if (gribfile(1:2).eq.'d2') then
            igdbug = 2
            go to 50
            endif
         if (gribfile(1:2).eq.'d3') then
            igdbug = 3
            go to 50
            endif
         if (gribfile(1:2).eq.'d4') then
            igdbug = 4
            go to 50
            endif
         if (gribfile(1:1).eq.'d'.or.gribfile(1:1).eq.'D') then
            igdbug = 1
            go to 50
            endif
         if (gribfile(1:1).eq.'e'.or.gribfile(1:1).eq.'E') then
            igerr = 1
            go to 50
            endif
         if (gribfile(1:1).eq.'l'.or.gribfile(1:1).eq.'L') then
            if (ilogfile.eq.0) then
               iupr = iul
               else
                  iupr = iutw
                  ilogfile = 0
                  write (iutw,*) 'logfile=OFF'
                  go to 120
               endif
            go to 50
            endif
         if (gribfile(1:1).eq.'n') then
            inat = 0
            go to 50
            endif
         if (gribfile(1:1).eq.'u'.or.gribfile(1:1).eq.'U') then
            iprgrib = 1
            iupr = iul
            sfun = 'u'
            if (ilogfile.eq.0) go to 50
            go to 260
            endif
         if (gribfile(1:1).eq.'v') then
            ivrb = 1
            go to 50
            endif
         if (gribfile(1:1).eq.'w') then
            mihdr = 0
            go to 50
            endif
         if (gribfile(1:1).eq.'x'.or.gribfile(1:1).eq.'X') then
            sfun = 'x'
            go to 260
            endif
         write (iutw,150) gribfile(1:lgribfile)
150   format (' ERROR: ',a,' is an invalid option.')
         go to 50
         endif
c
c  open grib output file
      gribpath = gribdiro(1:lgribdiro)
     +           //'/'
     +           //gribfile(1:lgribfile)
     +           //char(0)
      lgribpath = lenstr(gribpath)
      call gbf_wopn (gribpath,lgribpath,istat)
      if (istat.eq.0) then
         if (ivrb.eq.1) write (iutw,60) gribpath(1:lgribpath)
         if (iupr.ne.iutw) then
            write (iupr,*)
            write (iupr,60) gribpath(1:lgribpath)
            endif
         ihdr = mihdr
         else
            write (iutw,70) gribpath(1:lgribpath),'istat',istat
            if (iupr.ne.iutw) write (iupr,70) gribpath(1:lgribpath),
     +         'istat',istat
            go to 50
         endif
c
c  get filename for input file (xmrg file to encode)
160   appsvar='grib_in_file'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,xmrgfile,lxmrgfile)
      if (lxmrgfile.eq.0) then
         write (iutw,170)
170      format (/ ' Enter XMRG input file name: ',$)
         read (iutr,'(a)',end=240,err=240) xmrgfile
         lxmrgfile = lenstr(xmrgfile)
         if (xmrgfile.eq.'.') then
            appsvar='operating_system'
            lappsvar=lenstr(appsvar)
            call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
            if (lappsval.eq.0) then
               xmrgfile='grib_xmrg_in_file'
               else
                  xmrgfile='grib_xmrg_in_file'//
     +                      '_'//
     +                      appsval(1:lappsval)//
     +                      char(0)
               endif
            lxmrgfile = lenstr(xmrgfile)
            write (iutw,140) 'XMRG',xmrgfile(1:lxmrgfile)
            endif
         endif
      if (lxmrgfile.eq.0) go to 240
      xmrgpath = gribdiri(1:lgribdiri)//
     +           '/'//
     +           xmrgfile(1:lxmrgfile)//
     +           char(0)
      lxmrgpath=lenstr(xmrgpath)
      iprint=0
      call check_exist (xmrgpath,'file',iexist,iprint)
      if (iexist.eq.0) then
         write (iutw,180) xmrgpath(1:lxmrgpath)
         if (iue.ne.iutw) write (iue,180) xmrgpath(1:lxmrgpath)
180   format (' ERROR: file ',a,' not found.')
         go to 250
         endif
      lxmrgfile = 0
      open (iuxmrg,file=xmrgpath,status='old',
     +      access='sequential',form='unformatted',iostat=iostat)
      if (iostat.eq.0) then
         if (ivrb.eq.1) then
            write (iutw,60) xmrgpath(1:lxmrgpath)
            if (iupr.ne.iutw) write (iupr,60) xmrgpath(1:lxmrgpath)
            endif
         else
            write (iutw,70) xmrgpath(1:lxmrgpath),'iostat',iostat
            if (iupr.ne.iutw) write (iupr,70) xmrgpath(1:lxmrgpath),
     +         'iostat',iostat
            go to 250
         endif
c
c  DTM Nov 07 - read xmrg header first.
c  Rather than use another variable, reuse istat to flag that
c  we are only reading header
c
      istat = -1
      call rdxmrgg (xmrgpath,iuxmrg,iupr,xver,mwcol,msrow,ncol,nrow,
     +             user,sdatim,proces,vdatim,mxval,ihfld,istat)
      if (istat.ne.0) then
         write (iutw,190) xmrgpath(1:lxmrgpath)
         go to 250
      endif

      rewind (iuxmrg)
c
c  DTM Nov 07 - check to see if this is on the 1/4 HRAP
c  and adjust for other conversion routines
c

       if ((INDEX(proces,'DHR').gt.0) .or.
     +     (INDEX(proces,'DSP').gt.0)) then

          appsvar='hpe_hrap_grid_factor'
          lappsvar=lenstr(appsvar)
          call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
	  if(INDEX(appsval,'4').gt.0) then
             gridf = 4.
	  endif
      endif

c	     write(iutw,185) loc, gridf
c185          format(' loc = ',i4,' empe grid factor = ',f6.0)

c
c  if ncol x nrow greater than mxbmap, then must reallocate
c
      ncolxnrow = ncol * nrow
      if(ncolxnrow.gt.mxbmap) then
         kbufsz = ncolxnrow + (ncolxnrow * 0.17)
	 mxbmap = ncolxnrow

         if(allocated(kbuf)) deallocate(kbuf)

         if(allocated(kbms)) deallocate(kbms)

         if(allocated(ihfld)) deallocate(ihfld)

         if(allocated(fld)) deallocate(fld)

         if(allocated(wfld)) deallocate(wfld)

         if(allocated(ifld)) deallocate(ifld)

         if(allocated(ibmap)) deallocate(ibmap)

         allocate(kbuf(kbufsz),stat=iaerr)
         if(iaerr .ne. 0) then
            iallocate=iallocate+1
            write(iutw,5) iallocate

            goto 250
         endif
         allocate(kbms(mxbmap), stat=iaerr)
         if(iaerr .ne. 0) then
            iallocate=iallocate+1
            write(iutw,5) iallocate
            goto 250
         endif
         allocate(ihfld(mxbmap), stat=iaerr)
         if(iaerr .ne. 0) then
            iallocate=iallocate+1
            write(iutw,5) iallocate
            goto 250
         endif
         allocate(fld(mxbmap), stat=iaerr)
         if(iaerr .ne. 0) then
            iallocate=iallocate+1
            write(iutw,5) iallocate
            goto 250
         endif
         allocate(wfld(mxbmap), stat=iaerr)
         if(iaerr .ne. 0) then
            iallocate=iallocate+1
            write(iutw,5) iallocate
            goto 250
         endif
         allocate(ifld(mxbmap), stat=iaerr)
         if(iaerr .ne. 0) then
            iallocate=iallocate+1
            write(iutw,5) iallocate
            goto 250
         endif
         allocate(ibmap(mxbmap), stat=iaerr)
         if(iaerr .ne. 0) then
            iallocate=iallocate+1
            write(iutw,5) iallocate
            goto 250
         endif
      endif
      istat = 0
c
c  read xmrg format file
c
      call rdxmrgg (xmrgpath,iuxmrg,iupr,xver,mwcol,msrow,ncol,nrow,
     +             user,sdatim,proces,vdatim,mxval,ihfld,istat)
      if (istat.ne.0) then
         write (iutw,190) xmrgpath(1:lxmrgpath)
190   format (' ERROR: reading xmrg file ',a,'.')
         go to 250
         endif
      if (igdbug.gt.0) then
         if (mxval.gt.0) then
            xminch=mxval/(100*25.4)
            else
               xminch=float(mxval)
            endif
         write (iutw,200) xver,
     +      mwcol,msrow,ncol,nrow,
     +      user(1:8),sdatim(1:20),proces(1:8),vdatim(1:20),
     +      mxval,xminch,istat
200   format (' in main - xmrg file header information:' /
     +  ' xver=',f5.2,' mwcol=',i4,' msrow=',i4,' ncol=',i4,' nrow=',i4
     +     /
     +  ' user=',a,' sdatim=',a,'proces=',a,'vdatim=',a /
     +  ' mxval=',i5,' xminch=',f5.2,' istat=',i2)
         endif
c
      m = ncol*nrow
      if (m.gt.mxbmap) then
         write (iutw,210) m,mxbmap
210   format (' wfld and ibmap arrays need ',i7,
     +        ' words but ',i6,' are allocated.')
         print 210, m,mxbmap
c
c          go to 250
      endif

c
c check for option to convert data
      iconvert = 1
      appsvar='grib_convert_data'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,appsval,lappsval)
      if (lappsval.gt.0) then
         if (appsval(1:1).eq.'n'.or.appsval(1:1).eq.'N') then
            iconvert = 0
            write (iutw,215)
            if (iue.ne.iutw) write (iue,215)
215   format (' NOTE: option set to not convert data.')
            endif
         endif
c
c  convert data from hundredths of millimeters to millimeters
      do 220 i=1,m
         if (iconvert.eq.1) then
            fld(i) = ihfld(i)/100.0
            else
               fld(i) = ihfld(i)/1.0
            endif
220      continue
c
c  date the data was processed
      nzmo = -1
      call infxdt (nzmo,nzda,nzyr,nzhr,nzmn,nzsc,sdatim)
c
c  date of data
      kmo = -1
      call infxdt (kmo,kda,kyr,khr,kmn,ivalhr,vdatim)
c
c  define initial parameters for GRIB
      call xm2gribg (iutw,iupr,iud,xver,user,proces,isearch,ivalhr,
     +         kmo,kda,kyr,khr,kmn,
     +         mwcol,msrow, nsbctr,iocent,mptnum,lptver,
     +         iresfl,rlav,rlov,iscan,iparm,modlid,ngrid,itunit,
     +         nturef,ntufc,itrang,ipkflg,inbmap,refval,ibinf,idec,
     +         iwidth,idatyp,wmo,senof,jerr)
      if (jerr.ne.0) go to 250
c
c  select grid and transform (if needed)
      nwarn = 0
      call griddefg (iutw,iupr,igdbug,ngrid,inat,
     +              mwcol,msrow,ncol,nrow,
     +              fld,wfld,igds,mxbmap,ibmap,nwarn,gridf,jerr)
      if (jerr.gt.0) go to 250
c
c  output comms header
      fmt = 'u'
      call gpcomm (iupr,iupr,igdbug,iugrib,ihdr,fmt,pid,cct,
     +             wmo,senof,nzyr,nzmo,nzda,nzhr,nzmn)
c
c  final parameter definition then call the encoder
      call engrib (iupr,iugrib,igdbug,kyr,kmo,kda,khr,kmn,
     +             mwcol,ncol,msrow,nrow,igds,
     +             mptnum,iocent,modlid,ngrid,iparm,itunit,nturef,ntufc,
     +             itrang,nsbctr,ibinf,idec,iresfl,rlov,iscan,ipkflg,
     +             idatyp,kbuf,ifld,fld,wfld,ibmap,itot,jerr)
      close (iuxmrg)
      if (jerr.eq.0) then
         if (ivrb.eq.1) write (iutw,230) gribpath(1:lgribpath),
     +                     xmrgpath(1:lxmrgpath),
     +                     wmo,senof,nzda,nzhr,nzmn,itot
230   format (' NOTE: wrote GRIB file ',a /
     +        7x,'from  XMRG file ',a /
     +        7x,'with  COMMS HEADER of ''',a4,a3,a4,1x,3i2.2,
     +           ''' and length of ',i7,' bytes.')
         if (iupr.ne.iutw) write (iupr,230) gribpath(1:lgribpath),
     +                        xmrgpath(1:lxmrgpath),
     +                        wmo,senof,nzda,nzhr,nzmn,itot
         endif
      if (iread.eq.1) go to 160

240   call gbf_clos (istat)
      if (igdbug.gt.0) go to 290
C
250   if (iread.eq.1) go to 120
      go to 360
c
c................................................................
c
c  decode or unpack GRIB file
c
260   appsvar='grib_in_file'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,gribfile,lgribfile)
270   if (lgribfile.le.1) then
         write (iutw,280)
280      format (/ ' Enter GRIB file name: ',$)
         read (iutr,'(a)',end=360,err=360) ans
         if (ans.eq.' ') go to 120
         gribfile=ans
         if (ans.eq.'.') then
            appsvar='operating_system'
            lappsvar=lenstr(appsvar)
            call get_apps_defaults (appsvar,lappsvar,appsval,
     +         lappsval)
            if (lappsval.eq.0) then
               gribfile = 'grib_out_file'
               else
                  gribfile = 'grib_out_file'
     +                       //'_'
     +                       //appsval(1:lappsval)
     +                       //char(0)
               endif
            write (iutw,140) 'GRIB',gribfile(1:lenstr(gribfile))
            else
               write (iutw,145) 'GRIB',gribfile(1:lenstr(gribfile))
            endif
         lgribfile = lenstr(gribfile)
         endif
      if (lgribfile.le.1) go to 260
      gribpath = gribdiro(1:lgribdiro)//
     +           '/'//
     +           gribfile(1:lgribfile)//
     +           char(0)
      iprint=0
      call check_exist (gribpath,'file',iexist,iprint)
      if (iexist.eq.0) then
         lgribpath=lenstr(gribpath)
         write (iutw,180) gribpath(1:lgribpath)
         if (iue.ne.iutw) write (iue,180) gribpath(1:lgribpath)
         if (iread.eq.1) then
            lgribfile = 0
            go to 270
            endif
         go to 360
         endif
290   lgribpath = lenstr(gribpath)
      call gbf_ropn (gribpath,lgribpath,istat)
      if (istat.eq.0) then
         if (ivrb.eq.1) then
            write (iutw,60) gribpath(1:lgribpath)
            write (iutw,300) gribpath(1:lgribpath)
300      format (' NOTE: decoding GRIB file ',a,'.')
            if (iupr.ne.iutw) then
               write (iupr,*)
               write (iupr,300) gribpath(1:lgribpath)
               endif
            endif
         ihdr = mihdr
         else
            write (iutw,70) gribpath(1:lgribpath),'istat',istat
            if (iupr.ne.iutw) write (iupr,70) gribpath(1:lgribpath),
     +         'istat',istat
            go to 50
         endif
c
c  read comms header
      nbytes=21
      call gbf_read (nbytes,kbuf,istat)
      if (istat.eq.0) then
         if (ivrb.eq.1) write (iupr,310) (kbuf(i),i=1,18)
310   format (/ ' COMMS HEADER = ',18a)
         jerr = -1
         else
            write (iutw,320)
320   format (' ERROR: cannot read COMMS HEADER.')
            rewind (iugrib)
         endif
c
      ngrib=0
c
c  check if to unpack GRIB file
330   if (cvgrib.eq.'1') then
         call ungribg (iupr,igdbug,mbuf,kbuf,mfld,fld,
     +                mifld,ifld,kptr,kpds,kgds,kbms,
     +                mwcol,msrow,ncol,nrow,ngrib,gridf,jerr)
         else
            call ungrib2g (iutr,iutw,iupr,iud,igdbug,ipack,fld,
     +	                  gridf,jerr)
         endif
ccc      write (*,*) 'jerr=',jerr
      if (iabs(jerr).eq.20) then
         if (jerr.eq.20) then
            if (iprgrib.eq.1) then
               if (ivrb.eq.1) write (iutw,340) logpath(1:llogpath)
340   format (' NOTE: GRIB unpack information written to file ',a,'.')
               endif
            endif
         if (iread.eq.1) go to 120
         go to 360
         endif
c
ccc      write (*,*) 'iprgrib=',iprgrib
      if (iprgrib.eq.1.or.igdbug.gt.0) then
c     print GRIB file information
         call prgrib (iupr,iud,igdbug,fld,mkptr,kptr,mkpds,kpds,mkgds,
     +                kgds,kbms,mwcol,msrow,ncol,nrow,jerr)
         endif
c
c  check if to create xmrg file
      if (sfun.eq.'x') then
c     set date the data was processed
         call infxdt (nmo,nda,nyr,nhr,nmn,nsc,sdatim)
         if (ibug.gt.0) write (iupr,*) 'in main - mwcol=',mwcol,
     +      ' msrow=',msrow,' ncol=',ncol,' nrow=',nrow
         call putxmrg (iuxmrg,iudm,igdbug,sdatim,fld,kptr,kpds,
     +                kgds,kbms,mwcol,msrow,ncol,nrow,
     +                ihfld,gribdiro,lgribdiro,xmrgpath,lxmrgpath,
     +                ivrb,jerr)
         if (jerr.eq.0) then
            if (ivrb.eq.1) write (iutw,350)
     +                        xmrgpath(1:lxmrgpath),
     +                        gribpath(1:lgribpath)
350   format (' NOTE: wrote XMRG file ',a /
     +        7x,'from  GRIB file ',a,'.')
            if (iupr.ne.iutw) write (iupr,350)
     +                           xmrgpath(1:lxmrgpath),
     +                           gribpath(1:lgribpath)
            endif
         endif
c
      if (jerr.eq.0) go to 330
      call gbf_clos (istat)
      go to 120
c
360   if (ivrb.eq.1.and.ilogfilez.eq.1) then
         write (iutw,370) logpath(1:llogpath)
370   format (' NOTE: log information written to file ',a,'.')
         endif

c
c  DTM Nov 07 - clean up, deallocate memory
c

      if(allocated(kbuf)) deallocate(kbuf)

      if(allocated(kbms)) deallocate(kbms)

      if(allocated(ihfld)) deallocate(ihfld)

      if(allocated(fld)) deallocate(fld)

      if(allocated(wfld)) deallocate(wfld)

      if(allocated(ifld)) deallocate(ifld)

      if(allocated(ibmap)) deallocate(ibmap)
      if(iallocate.gt.0) then
         write(iutw,380)
 380     format(' ERROR: allocation failure means no GRIB output ')
      endif
c
      strng=' Program '//pgmnam(1:lenstr(pgmnam))//' completed.'
      if (ivrb.eq.1) write (iutw,'(/a)') strng(1:lenstr(strng))
      if (iupr.ne.iutw) write (iupr,'(/a)') strng(1:lenstr(strng))



c
c  stop statement commented because it writes "FORTRAN STOP" on Linux
ccc      stop
c
      end

