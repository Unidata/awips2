C$PRAGMA C (GET_APPS_DEFAULTS)
c  =====================================================================
c  pgm:  putxmrg (iuout,iudm,ibug,sdatim,fld,kptr,kpds,
c                 kgds,kbms,mwcol,msrow,ncol,nrow,
c                 ihfld,griddir,lgriddir,xmrgpath,lxmrgpath,
c                 ivrb,istat)
c
c   in: iuout  .... unit number of xmrg output file
c   in: iudm   .... unit number of RFC HRAP domain file
c   in: ibug   .... debug control
c   in: sdatim .... saved date and time of data
c   in: fld    .... real array of data
c   in: kptr   .... array containing pointers in GRIB message
c   in: kpds   .... array containing Product Definition Section
c                   parameters
c   in: kgds   .... array containing Grid Definition Section parmeters
c   in: kbms   .... bitmap array
c   in: mwcol  .... most west HRAP column
c   in: msrow  .... most south HRAP row
c   in: ncol   .... number of HRAP columns
c   in: nrow   .... number of HRAP rows
c   in: ihfld  .... array of data in xmrg file (SW corner is ihfld(1))
c   in: griddir ... directory and path of xmrg file
c   in: lgriddir .. length of directory name of xmrg file
c  out: xmrgpath .. directory and filename of xmrg file
c  out: lxmrgpath . length of directory and filename of xmrg file
c   in: ivrb   .... verbose option
c  i/o: istat  .... completion status code
c  =====================================================================
c
      subroutine putxmrg (iuout,iudm,ibug,sdatim,fld,kptr,kpds,
     +                    kgds,kbms,mwcol,msrow,ncol,nrow,
     +                    ihfld,griddir,lgriddir,xmrgpath,lxmrgpath,
     +                    ivrb,istat)
c
c.......................................................................
c
c  This routine takes parameters from a decoded GRIB file,
c  translates them for an xmrg file and writes the xmrg file.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HL                                   Jan 2001
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'grib_tbl'
      include 'xmrg_tbl'
c
      logical*1 kbms(*)
      character*8 rfcid
      character*25 appsvar
      character*50 fidm
      character*128 dirdm,fname,griddir,xmrgpath,xmrgfile
      character user*8,sdatim*20,proces*8,vdatim*20
      character ctime*10
c
      dimension fld(*)
c
      integer*2 ihfld(*)
      dimension kptr(*),kpds(*),kgds(*)
      dimension mon(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/putxmrg.f,v $
     . $',                                                             '
     .$Id: putxmrg.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
     . $' /
C    ===================================================================
C
      data mon / 31,28,31,30,31,30,31,31,30,31,30,31 /
c
c
      if (ibug.gt.0) write (iupr,*) 'enter putxmrg'
c
      if (istat.eq.20) go to 240
c
c  initial values
      mxval = -9999
c
c  get HRAP domain for a subset (lwcol,lsrow,lncol,lnrow)
      appsvar='hrap_domain_dir'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,dirdm,ldirdm)
      if (ldirdm.eq.0) then
         appsvar='geo_st3_ascii'
         lappsvar=lenstr(appsvar)
         call get_apps_defaults (appsvar,lappsvar,dirdm,ldirdm)
         endif
      appsvar='hrap_domain_id'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,rfcid,lrfcid)
      if (lrfcid.eq.0) then
         appsvar='st3_rfc'
         lappsvar=lenstr(appsvar)
         call get_apps_defaults (appsvar,lappsvar,rfcid,lrfcid)
         endif
      appsvar='hrap_domain_file'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,fidm,lfidm)
      if (lfidm.eq.0) then
         fidm = 'coord_'//rfcid(1:lrfcid)//'.dat'
         lfidm = lenstr(fidm)
         endif
      fname = dirdm(1:ldirdm)//'/'//fidm(1:lfidm)
      lfname=lenstr(fname)
      open (iudm,access='sequential',form='formatted',
     +      file=fname,status='old',iostat=ios)
      if (ios.eq.0) then
         if (ivrb.eq.1) write (iupr,10) fname(1:lfname)
10    format (/ ' File ',a,' opened.')
         read (iudm,'(i4)',end=30,err=50) lwcol,lsrow,lncol,lnrow
         close (iudm)
         if (ibug.gt.0) write (iupr,*) ' in putxmrg - ',
     +      'rfcid=',rfcid(1:lfname),
     +      ' lwcol=',lwcol,' lsrow=',lsrow,
     +      ' lncol=',lncol,' lnrow=',lnrow
         if (lwcol.ne.mwcol) write (iupr,20) 'western','column',
     +      fname(1:lfname),lwcol,mwcol
20    format (' WARNING: ',a,' HRAP grid ',a,' in file ',a,' (',i4,')' /
     +   t11,'is not the same as in GRIB file (',i4,').')
         if (lsrow.ne.msrow) write (iupr,20) 'southern','row',
     +      fname(1:lfname),lsrow,msrow
         if (lncol.ne.ncol) write (iupr,20) 'number of','columns',
     +      fname(1:lfname),lncol,ncol
         if (lnrow.ne.nrow) write (iupr,20) 'number of','rows',
     +      fname(1:lfname),lnrow,nrow
         go to 60
30       write (iutw,40) 'end-of-file',fname(1:lfname)
40    format (' ERROR: ',a,' encountered reading file ',a,'.')
         istat = 20
         go to 240
50       write (iutw,40) 'i/o error',fname(1:lfname)
         istat = 20
         go to 240
         else
            lwcol = mwcol
            lsrow = msrow
            lncol = ncol
            lnrow = nrow
         endif
c
c  determine user, GRIB Table C
60    isubc = kpds(23)
      user = ' '
      if (isubc.gt.0.and.isubc.le.255) then
         if (kpds(1).eq.9) then
            user(1:4) = scid9c(isubc)
            else
            endif
         if (ibug.gt.0) write (iupr,70) user,isubc
70    format (' user=',a,4x,'isubc=',i4)
         else
           write (iupr,80) isubc
80    format (' ERROR: subcenter number ',i5,' out of range.')
           istat = 20
           go to 240
         endif
c
c  set date and time of data
      kyr = kpds(8) + (kpds(21)-1)*100
      kmo = kpds(9)
      kda = kpds(10)
      khr = kpds(11)
      kmn = kpds(12)
      itunit = kpds(13)
      nturef = kpds(14)
      ntufc = kpds(15)
      mptver = kpds(19)
c
c  determine vdatim
      istep = ntufc - nturef
      if (istep.eq.1) then
         call datimi (istep,kmo,kda,kyr,khr,kmn,imo,ida,iyr,ihr,imn,mon)
         isc = 0
         else if (istep.gt.0) then
            imo = kmo
            ida = kda
            iyr = kyr
            ihr = khr
            imn = -kmn
            isc = ntufc
         endif
      call infxdt (imo,ida,iyr,ihr,imn,isc,vdatim)
      if (ibug.gt.0) write (iupr,*) 'imo=',imo,' ida=',ida,' iyr=',iyr,
     +   ' ihr=',ihr,' imn=',imn,' vdatim=',vdatim

c
c  check if satellite data
      if (kpds(2).eq.190) then
c     create output xmrg file name
        ipos = 1
        iwid = 4
        num = 1
        call ffi2a (ctime,ipos,iwid,num,iyr,ier)
        ipos = 5
        iwid = 2
        call ffi2a (ctime,ipos,iwid,num,imo,ier)
        ipos = 7
        call ffi2a (ctime,ipos,iwid,num,ida,ier)
        ipos = 9
        call ffi2a (ctime,ipos,iwid,num,ihr,ier)
        do 90 i=1,10
           if (ctime(i:i).eq.' ') ctime(i:i) = '0'
90         continue
        xmrgfile = 'SATPRE'//ctime//'z'
        lxmrgfile = lenstr(xmrgfile)
        go to 120
        endif
c
c  check if xmrg output file name specified
      appsvar='grib_xmrg_out_file'
      lappsvar=lenstr(appsvar)
      call get_apps_defaults (appsvar,lappsvar,xmrgfile,lxmrgfile)
      if (lxmrgfile.eq.0) then
         appsvar='grib_out_file'
         lappsvar=lenstr(appsvar)
         call get_apps_defaults (appsvar,lappsvar,xmrgfile,lxmrgfile)
         endif
      if (lxmrgfile.eq.0) then
         write (iutw,100)
100   format (/ ' Enter XMRG output file name: ',$)
         read (iutr,'(a)',end=240,err=240) xmrgfile
         if (xmrgfile.eq.'.') then
            xmrgfile='grib_xmrg_out_file'
            write (iutw,110) 'XMRG',xmrgfile(1:lenstr(xmrgfile))
110   format (' NOTE: ',a,' file name set to ',a,'.')
            endif
         lxmrgfile = lenstr(xmrgfile)
         endif
      if (lxmrgfile.eq.0) go to 240
c
120   xmrgpath = griddir(1:lgriddir)//'/'//xmrgfile(1:lxmrgfile)
      open (iuout,access='sequential',form='unformatted',
     +      file=xmrgpath,status='unknown',iostat=ios)
      if (ios.eq.0) then
         lxmrgpath = lenstr(xmrgpath)
         if (ivrb.eq.1) write (iutw,130) xmrgpath(1:lxmrgpath)
130   format (/ ' File ',a,' opened.')
         else
            write (iutw,140) xmrgpath(1:lenstr(xmrgpath))
140   format (' ERROR: in putxmrg : unable to open file ',a,'.')
            istat = 20
            go to 240
         endif
c
c  determine process from GRIB Table A and xmrg table for xmrg process
      modlid = kpds(2)
      do 150 ixpm=1,mproc
         if (modlid.eq.xmodlid(ixpm)) go to 170
150      continue
c
c  no process defined
      write (iupr,160) istep
160   format (' ERROR: no process defined in GRIB Table A and xmrg ',
     *   ' table for process ',i4,'.')
      istat = 20
      go to 240
c
170   proces = xmproc(ixpm)
      if (proces(1:2).ne.'FF') then
c     check for appending time step
         do 190 ipt=1,8
         if (proces(ipt:ipt).eq.'*') then
            ipos = ipt
            iwidth = 2
            num = 1
            call ffi2a (proces,ipos,iwidth,num,istep,istat)
            if (istat.ne.0) then
               write (iupr,180) istep
180   format (' ERROR: in ffi2a for istep of ',i4,'.')
               istat = 20
               go to 240
               endif
            if (proces(ipt:ipt).eq.' ') proces(ipt:ipt) = '0'
            endif
190      continue
         else
            proces = abv128(kpds(5))
            proces(6:8) = '   '
         endif
      if (ibug.gt.0) write (iupr,200) proces,modlid
200   format (' proces=',a,4x,'modlid=',i4)
c
c  transfer data to sub-set array
      mecol = mwcol + ncol - 1
      mnrow = msrow + nrow - 1
      do 230 irow=1,lnrow
         krow = lsrow + irow - 1
         jrow = krow - msrow + 1
         if (krow.ge.msrow.and.krow.le.mnrow) then
            do 210 icol=1,lncol
            kcol = lwcol + icol - 1
            if (kcol.ge.mwcol.and.kcol.le.mecol) then
               jcol = kcol - mwcol + 1
               ipm = (jrow-1)*ncol + jcol
               ips = (irow-1)*lncol + icol
               if (kbms(ipm).eqv..TRUE.) then
                  ihfld(ips) = fld(ipm)*100.
                  if (fld(ipm).gt.mxval) mxval = fld(ipm)
                  else
                     ihfld(ips) = -251
                  endif
               else
                  ihfld(ips) = -251
               endif
210            continue
            else
               do 220 icol=1,lncol
               ips = (irow-1)*lncol + icol
               ihfld(ips) = -251
220            continue
            endif
230      continue
c
c  write xmrg file
      call wrxmrg (iuout,iupr,lwcol,lsrow,lncol,lnrow,
     +             user,sdatim,proces,vdatim,mxval,ihfld,istat)
      close (iuout)
c
240   return
c
      end


