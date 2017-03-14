C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (UDATL)
c  =====================================================================
c
c MAIN ROUTINE FOR FLASH FLOOD GUIDANCE PRODUCT GENERATION
c
c.......................................................................
c
c This program generates products containing flash flood guidance
c values and maintains the parametric values needed to  generate the
c products.
c
c.......................................................................
c
c Initially written by
c       Tim Sweeney, HRL - Jan 1991
c
c  Increased dimension of po array to store grid values in a single
c  file for each duration
c       Tim Sweeney, HRL                                   Nov 1995
c
c.......................................................................
c
      subroutine pgmain_main
c
      character*1 resp,ffm
      character*2 resp2,bname
      character*4 cnum
      character*4 lid(2)
      character*4 outtyp,dtype
      character*4 accmode
      character*7 timezone
      character*8 cid,prodidz
      character*20 outfile
      character*20 appsvar,appsval
      character*30 logfile
      character*56 dutyf,oftel
      character*128 logpath,pthnam
      character*132 strng
      integer labl(25)
     +         / 1, 1, 2,11,12,
     +           3, 3, 4,13,14,
     +           6, 5, 6,15,16,
     +          12, 7, 8,17,18,
     +          24, 9,10,19,20/
c
      dimension mdy(6)
c
      include 'udebug'
      include 'updaio'
      COMMON /CMPRODGEN/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      include 'common/fdbug'
      include 'common/ionum'
      include 'ffg_inc/paths'
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/iodev'
      include 'ffg_inc/count'
      include 'ffg_inc/gridsz'
      include 'ffg_inc/timez'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/gidx'
      include 'ffg_inc/gpo'
      include 'ffg_inc/ghfld'
      include 'prodgen_inc/pfparm'
      include 'prodgen_inc/txtpar'
      include 'prodgen_inc/propar'
      include 'prodgen_inc/grppar'
      include 'prodgen_inc/poptns'
      include 'prodgen_inc/xgpo'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_main/RCS/pgmain.f,v $
     . $',                                                             '
     .$Id: pgmain.f,v 1.13 2004/09/13 17:07:42 scv Exp $
     . $' /
C    ===================================================================
C
C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
c
      ibug=0
c
c  initialize variables
      igdbug = 0
      ihcltr=0
      ihcldb=0
      igtrac = 0
      igerr = 1
      protyp = 'prod'
      grptyp = 'grpp'
      txtyp  = 'text'
      ucon = 25.4
      dutyf = ' '
      oftel = ' '
      lcocpd = -1
      ibend  = 0
      ilogfile = 0
      ilogopen = 0
      outfile = 'ffgout'
      outtyp = 'out'
      bname = ' '
c
c  initialize file input/output routine common block
      call upinio
c
c  set print unit for file open/exist/close routines
ccc      uu = iutw
c
c  set device unit numbers
      call devnum
c
c  print program and version information
      idsply = 1
      call prnamv (idsply,iutw)
c
c  get options and directory names from environmental variables
      call envfix
c
      if (pgmnam.eq.' ') then
         write (iutw,'(/a)') 'ERROR: variable pgmnam is blank.'
         go to 500
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
ccc            logfile=ofslvl(1:lofsl)//'_'//strng(1:lstrng)//'_log'
ccc            logfile=strng(1:lstrng)//'_log'
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
c
c  check if logfile option specified as environmental variables
      if (iupr.eq.iul) then
         call opnlog (ilogopen,logfile,logpath,istat)
         llogpath=lenstr(logpath)
         if (istat.eq.0) then
            iupr = iul
            ipr = iul
            ilogfile = 1
            else
               write (iutw,5) logpath(1:llogpath)
5     format (/ ' ERROR: cannot open file ',a,'.')
               iupr = iutw
               go to 490
            endif
         endif
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
      write (iutw,10) ffglvl(1:lffgl),ofslvl(1:lofsl)
10    format (/ 20x,'ffg_level=',a,5x,'ofs_level=',a)
      write (iutw,15) nmo,nda,nyr,nhr,nmn,nsc,
     +   timezone(ltimezone1:ltimezone2)
15    format (/ 10x,5x,3x,'RUN DATE = ',i2.2,'/',i2.2,'/',i4,' - ',
     +   i2.2,':',i2.2,':',i2.2,' ',a)
      write (iutw,30) usrpa(1:lusr),
     +                arpa(1:lar),
     +                carypa(1:lcary),
     +                gffpa(1:lgff),
     +                gropa(1:lgro),
     +                hdwpa(1:lhdw),
     +                grpppa(1:lgrpp),
     +                outpa(1:lout),
     +                prodpa(1:lprod),
     +                txtpa(1:ltxt),
     +                wsuppa(1:lwsup)
30    format (/
     +        ' directory for user info      = ',a /
     +        ' directory for areas          = ',a /
     +        ' directory for carryovers     = ',a /
     +        ' directory for gridded ffg    = ',a /
     +        ' directory for gridded runoff = ',a /
     +        ' directory for headwaters     = ',a /
     +        ' directory for groups of prod = ',a /
     +        ' directory for output         = ',a /
     +        ' directory for products       = ',a /
     +        ' directory for texts          = ',a /
     +        ' directory for water supply   = ',a)
      if (iupr.ne.iutw) then
         idsply = 3
         call prnamv (idsply,iupr)
         write (iupr,10) ffglvl(1:lffgl),ofslvl(1:lofsl)
         write (iupr,15) nmo,nda,nyr,nhr,nmn,nsc,
     +      timezone(ltimezone1:ltimezone2)
         write (iupr,30) usrpa(1:lusr),
     +                   arpa(1:lar),
     +                   carypa(1:lcary),
     +                   gffpa(1:lgff),
     +                   gropa(1:lgro),
     +                   hdwpa(1:lhdw),
     +                   grpppa(1:lgrpp),
     +                   outpa(1:lout),
     +                   prodpa(1:lprod),
     +                   txtpa(1:ltxt),
     +                   wsuppa(1:lwsup)
         endif
c
c  get user info
      kod = 3
      call getinf (kod,mpo,po,iunit,istat)
c
c     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
20    write (iutw,*)
      if (igdbug.gt.0) write (iutw,60) igdbug
60    format (' DEBUG',i3)
      if (igerr.gt.0) write (iutw,*) 'ERROR ON'
      if (iupr.eq.iutw) then
         write (iutw,*) 'LOGFILE OFF'
         else
            write (iutw,*) 'LOGFILE ON'
            write (iutw,70) logpath(1:lenstr(logpath))
70    format (/ ' NOTE: log information will be written to file ',a,'.')
         endif
c
      icaller = 2
c
100   write (iutw,120)
120   format (/ 10x,'PRODUCT GENERATION MENU')
c
c  get index for groups of products
      dtype = grptyp
      call getidx (dtype,kxdv,mcidx,po,nids,cidx,istat)
      if (nids.eq.1.and.istat.eq.1) then
c     new file
         call stridx (kxdv,mcidx,po,nids,cidx,dtype)
         endif
      call upclos (kxdv,bname,ic)
      write (iutw,125)
125   format (/ 10x,5x,'Generate:')
      ipcol = 10
      call listid (nids,cidx,ipcol)
c
      write (iutw,130)
130   format ( 10x,5x,'P - Products Menu' /
     +         10x,5x,'Q - Quit program' /
     +         10x,5x,'S - Setup Menu')

135   write (iutw,140)
140   format (/ ' Select (number, P, S or <return>-exit): ',$)
c
      read (iutr,'(a)',err=135,end=490) cnum
      resp = cnum(1:1)
      if (resp.eq.' ') go to 490
      resp2 = cnum(1:2)
c
c  convert to integer
      call uc2ir (cnum,num,real,itype,istat)
      if (istat.eq.0) then
         if (num.gt.0.and.num.le.nids) then
c        group of products
            call umemov (cidx(1,num),cid,2)
            igpf = 1
            go to 250
            endif
         endif
c
c  check option
      if (resp.eq.'P'.or.resp.eq.'p') then
c     products menu
         go to 200
         else if (resp2.eq.'D '.or.resp2.eq.'d ') then
            write (iutw,160)
160     format (' Debug options:' /
     +     4x,'d,d1 - basic debug output' /
     +     4x,'  d2 - test GRIB encoding for grid product' /
     +     4x,'  d3 - not used' /
     +     4x,'  d4 - i/o accesses' /
     +     4x,'  d5 - override ffg with constant values (test)' /
     +     4x,'  d6 - override ffg with threshold runoffs')
            igdbug = 1
            go to 190
         else if (resp2.eq.'D1'.or.resp2.eq.'d1') then
c        set debug level 1
            igdbug = 1
            go to 190
         else if (resp2.eq.'D2'.or.resp2.eq.'d2') then
c        set debug level 2
            igdbug = 2
            go to 190
         else if (resp2.eq.'D3'.or.resp2.eq.'d3') then
c        set debug level 3
            igdbug = 3
            go to 190
         else if (resp2.eq.'D4'.or.resp2.eq.'d4') then
c        set debug level 4
            igdbug = 4
            go to 190
         else if (resp2.eq.'D5'.or.resp2.eq.'d5') then
c        override all ffg values to test product format
            iabug = 5
            go to 190
c        override all ffg values with threshold runoffs to test format
         else if (resp2.eq.'D6'.or.resp2.eq.'d6') then
            iabug = 6
            go to 190
         else if (resp2.eq.'DA'.or.resp2.eq.'da') then
167         write (iutw,170)
170      format (' Enter computer date and time (mmddccyyhhnn): ',$)
            read (iutr,180,err=167,end=490) nmo,nda,nyr,nhr,nmn
180      format (2i2,i4,2i2)
            go to 100
         else if (resp2.eq.'E '.or.resp2.eq.'e ') then
c        set error output indicator
            igerr = 1
            go to 190
         else if (resp2.eq.'EN'.or.resp2.eq.'en') then
            ucon = 1.0
            go to 190
         else if (resp2.eq.'L '.or.resp2.eq.'l ') then
            if (ilogfile.eq.0) then
c           open logfile
               call opnlog (ilogopen,logfile,logpath,istat)
               if (istat.eq.0) then
                  ilogfile = 1
                  iupr = iul
                  ipr = iul
                  write (iutw,*) 'LOGFILE ON'
                  write (iutw,70) logpath(1:lenstr(logpath))
                  idsply = 1
                  call prnamv (idsply,iul)
                  write (iul,10) ffglvl(1:lffgl),ofslvl(1:lofsl)
                  write (iul,15) nmo,nda,nyr,nhr,nmn,nsc,
     +               timezone(ltimezone1:ltimezone2)
                  write (iul,30) usrpa(1:lusr),
     +                           arpa(1:lar),
     +                           carypa(1:lcary),
     +                           gffpa(1:lgff),
     +                           gropa(1:lgro),
     +                           hdwpa(1:lhdw),
     +                           grpppa(1:lgrpp),
     +                           outpa(1:lout),
     +                           prodpa(1:lprod),
     +                           txtpa(1:ltxt),
     +                           wsuppa(1:lwsup)
                  endif
               else
                  iupr = iutw
                  ipr = iutw
                  ilogfile = 0
                  write (iutw,*) 'LOGFILE=OFF'
               endif
            go to 190
         else if (resp.eq.'Q'.or.resp.eq.'q') then
c        quit program
            go to 490
         else if (resp.eq.'S'.or.resp.eq.'s') then
c        setup menu
            go to 430
         else if (resp.eq.'T '.or.resp.eq.'t ') then
c        set trace option
            igtrac = 1
            go to 190
         else if (resp.eq.'U'.or.resp.eq.'u') then
c        user info menu
            call usrinf (icaller)
            go to 100
         else
            write (iutw,185) resp2(1:lenstr(resp2))
185   format (/ ' ERROR: ',a,' is an invalid option.')
            go to 135
         endif
c
      if (resp.eq.' ') go to 360
      go to 100
c
190   if (iupr.eq.iul) then
         iue = iupr
         iud = iupr
         endif
      iodbug = iud
      go to 100
c
c     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
c  products menu
c
c  get index for single products
200   call getidx (protyp,kxdv,mcidx,po,nids,cidx,istat)
      call upclos (kxdv,bname,ic)
      igpf = 0
c
210   write (iutw,220)
220   format (/ 10x,'PRODUCTS MENU')
c
      ipcol = 18
      call listid (nids,cidx,ipcol)
c
      write (iutw,230)
230   format (/ 10x,5x,'G - Groups Menu (main menu)')
      write (iutw,240)
240   format (/ ' Select (number, G or <return>-exit): ',$)
c
      read (iutr,'(a)',err=210,end=490) cnum
      resp = cnum(1:1)
c
      if (resp.eq.'G'.or.resp.eq.'g') then
c     groups menu
         go to 20
         else
            call uc2ir (cnum,num,real,itype,istat)
            if (num.gt.0.and.num.le.nids) then
               go to 250
               else
                  go to 360
               endif
         endif
c
      if (resp.eq.' ') go to 360
      if (resp.eq.'G'.or.resp.eq.'g') then
c     groups menu
         go to 20
         endif
      go to 210
c
c  process groups of products and individual product
c
250   call umemov (cidx(1,num),cid,2)

c  check user info
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.ne.0) then
         write (iutw,253)
         if (iupr.ne.iutw) write (iupr,253)
253   format (' ERROR: user parameter file not found.')
         nerr = nerr + 1
         go to 100
         endif
c
c  check maximum and minimum extremum for Gridded and Headwater FFG
c  values
      call chkext (icaller)
c
c  determine if type GRPP - nopid is number of product IDs in group
      nopid = 0
      dtype = grptyp
      if (igpf.eq.1) call procid (cid,dtype,mpo,po,istat)
c
c  process as type PROD when type GRPP not used
      if (nopid.eq.0) then
         call umemov (cid,apid(1,1),2)
         nopid = 1
         endif
c
c  check if duty forecaster specified
      appsvar='ffg_duty_fcstr'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,dutyf,ldutyf)
      dutyf(ldutyf+1:ldutyf+1)=' '
c
c  check if all SHEF products to be written to single file
      if (ising.eq.1.or.ising.eq.3) then
         accmode = 'rw'
         kod = 0
         call fixopn (outfile,outtyp,pthnam,accmode,kod,iuf,istat)
         if (istat.eq.0) rewind (iuf)
         lpthnam=lenstr(pthnam)
         endif
c
c  process each product
      idolr = 0
      do 350 i=1,nopid
         if (iupr.ne.iutw) then
            if (i.eq.1) write (iutw,*)
            write (iupr,*)
            else
               write (iutw,*)
            endif
         prodid(1) = apid(1,i)
         prodid(2) = apid(2,i)
         call umemov (prodid,prodidz,2)
         dtype = 'prod'
         call procid (prodid,dtype,mpo,po,istat)
         if (istat.gt.0) go to 350
         if (igdbug.gt.0) then
            write (iud,260) ((alidty(l,m),l=1,3),m=1,nolid)
260   format (5(1x,2a4,1x,a4))
            write (iud,*) 'nolid=',nolid
            endif
         if (ibug.eq.1) write (iud,*) ' in main - i=',i,
     +      ' nopid=',nopid,' pform=',pform
         if (pform.eq.'SHEF'.or.pform.eq.'shef') then
c        SHEF products - output in SHEF format
c        set duty forecaster
            if (igduty.ge.2.and.dutyf.eq.' ') then
               call getdut (oftel,dutyf)
               endif
            if (igduty.eq.1.and.oftel.eq.' ') then
               call getdut (oftel,dutyf)
               endif
c        output SHEF type products
            if (ising.eq.0.or.ising.eq.2) then
               accmode = 'rw'
               kod = 0
               call fixopn (prodidz,outtyp,pthnam,accmode,kod,iuf,istat)
               if (ibug.eq.1) write (iud,*) ' iuf=',iuf,
     +            ' pthnam=',pthnam
               if (istat.eq.0) rewind (iuf)
               lpthnam=lenstr(pthnam)
               endif
            if (ising.ne.0.and.ising.ne.2) then
               if (icom.eq.1.or.icom.eq.3) then
                  else
                     if (i.gt.1) write (iuf,*)
                  endif
               endif
            if (dftyp.ne.'GRIB') then
               ffm = 'f'
               else
                  ffm = 'u'
               endif
            icomh = icom
            call gpcomm (iupr,iud,igdbug,iuf,icomh,ffm,pid,cct,
     +                   wmo,senof,kzyr,kzmo,kzda,kzhr,kzmn)
c        process all pseudo-locations and types
            do 280 j=1,nolid
               lid(1) = alidty(1,j)
               lid(2) = alidty(2,j)
               dtype = alidty(3,j)
               if (dtype.eq.' ') dtype = dftyp
               call procid (lid,dtype,mpo,po,istat)
280            continue
            if (ibend.gt.0) then
c           end of SHEF dot B product
               write (iuf,290)
290   format ('.END')
               ibend = 0
               endif
            if (icom.eq.1.or.icom.eq.3) then
               else
                  write (iuf,340)
340   format ('$$')
                  idolr = 1
               endif
            if (igduty.ne.0) then
c           append office and duty forecaster phones
               write (iuf,*)
               if (oftel.ne.' '.and.igduty.ne.2) write (iuf,'(a)') oftel
               if (dutyf.ne.' '.and.igduty.ne.1) write (iuf,320) dutyf
320   format ('DUTY FORECASTER:  ',a)
               endif
            if (icom.eq.1.or.icom.eq.3) then
               write (iuf,*)
               else
                  write (iuf,330)
330   format ('NNNN')
               endif
            if (ising.eq.0.or.ising.eq.2) then
c           close file for single product per file
               if (icom.eq.1.or.icom.eq.3) write (iuf,340)
               call upclos (iuf,bname,ic)
               write (iutw,345) 'product',prodidz,pthnam(1:lpthnam)
               if (iupr.ne.iutw) write (iupr,345) 'product',prodidz,
     +            pthnam(1:lpthnam)
345   format (' NOTE: SHEF ',a,' ',a,' written to file ',a,'.')
               endif
            else
c           GRIB products - output gridded data encoded in GRIB
               if (iupr.ne.iutw) then
                  write (iutw,*)
                  endif
               ibline = 0
               call gpgffg (mpo,po,ihfld,ibline)
            endif
350      continue
c
c  check if all SHEF products written to single file
      if (ibug.eq.1) write (iud,*) 'ising=',ising,' icom=',icom
      if (ising.ne.0.and.ising.ne.2) then
c     close file for multiple products per file
         if (idolr.eq.0) write (iuf,340)
         call upclos (iuf,bname,ic)
         write (iutw,*)
         if (iupr.ne.iutw) write (iupr,*)
         write (iutw,345) 'single','product',pthnam(1:lpthnam)
         if (iupr.ne.iutw) write (iupr,345) 'single','product',
     +      pthnam(1:lpthnam)
         endif
c
c  write summary information
360   write (iutw,370)
      if (iupr.ne.iutw) write (iupr,370)
370   format (/ ' Product Generation Summary:')
      write (iutw,380) ngrid,narea,nhead
      if (iupr.ne.iutw) write (iupr,380) ngrid,narea,nhead
380   format (5x,'Processed ',i6,' grids' /
     +        5x,'Processed ',i6,' areas' /
     +        5x,'Processed ',i6,' headwaters')
      if (nvar.gt.0) then
         write  (iutw,390) nvar
         if (iupr.ne.iutw) write (iupr,390) nvar
390   format (5x,'Processed ',i6,' carryovers')
         endif
      if (nwsup.gt.0) then
         write (iutw,400) nwsup
         if (iupr.ne.iutw) write (iupr,400) nwsup
400   format (5x,'Processed ',i6,' water supply')
         endif
      write (iutw,410) nerr,nwarn
      if (iupr.ne.iutw) write (iupr,410) nerr,nwarn
410   format (/ 5x,'Errors=',i6,3x,'Warnings=',i6)
c
      np = ngrid + narea + nhead + nvar + nwsup
      if (np.gt.0) then
         write (iutw,420) outpa(1:lout)
         if (iupr.ne.iutw) write (iupr,420) outpa(1:lout)
420   format (/ ' Products written to directory ',a,'.')
         endif
c
      go to 100
c
c     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
c  setup menu
c
430   write (iutw,450)
450   format (/ 10x,'SETUP MENU')
      write (iutw,460)
460   format (/
     +          10x,5x,'G - Groups of Products' /
     +          10x,5x,'P - Products' /
     +          10x,5x,'R - Product Generation Menu (previous menu)' /
     +          10x,5x,'S - Single Products Menu' /
     +          10x,5x,'T - Text' /
     +          10x,5x,'U - User Controls'
     +          )
c
451   write (iutw,455)
455   format (/ ' Select (<return>-exit): ',$)
c
      read (iutr,'(a)',err=480,end=490) resp
c
      if (resp.eq.' ') go to 100
c
      if (resp.eq.'R'.or.resp.eq.'r') then
         go to 20
         else if (resp.eq.'S'.or.resp.eq.'s') then
            go to 200
         else if (resp.eq.'U'.or.resp.eq.'u') then
c        user info menu
            call usrinf (icaller)
            go to 430
         else if (resp.eq.'0'.or.resp.eq.' ') then
            go to 100
         else
         endif
c
c  add, change, delete, list parameters
      call mpgprm (resp,mcidx,cidx,mpo,po,istat)
      if (istat.eq.-1) go to 451
      go to 430
c
480   if (resp.eq.' ') go to 100
      go to 430
c
c     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
c
490   strng=' Program '//pgmnam(1:lenstr(pgmnam))//' completed.'
      write (iutw,'(/a)') strng(1:lenstr(strng))
      if (iupr.ne.iutw) write (iupr,'(/a)') strng(1:lenstr(strng))
c
500   if (iupr.ne.iutw) then
         if (igdbug.gt.0) iud = iutw
         call upclos (iupr,bname,ic)
         endif
c
      stop
c
      end
