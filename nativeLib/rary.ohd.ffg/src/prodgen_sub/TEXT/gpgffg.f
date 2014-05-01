C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (GBF_WOPN)
C$PRAGMA C (GBF_CLOS)
c  =====================================================================
c  pgm:  gpgffg (mpo,po,ihfld,ibline)
c
c   in: mpo    .... maximum words in array po
c   in: po     .... parametric array
c   in: ihfld  .... grid array from xmrg file
c   in: ibline .... indicator if to print blank line
c  =====================================================================
c
      subroutine gpgffg (mpo,po,ihfld,ibline)
c
c.......................................................................
c
c  Create GRIB product for each duration.
c
c.......................................................................
c
c  Initially written by
c        Tim Sweeney, HRL                              Apr 1992
c
c  Changed to single file for each duration
c        Tim Sweeney, HRL                              Dec 1995
c
c  Added selectable comms headers: AWIPS/AFOS.
C  Changed gridded guidance files to multiple records similar
c  to xmrg files.
c        Tim Sweeney, HRL                              Dec 1997
c
c  Changed to pass many GRIB parameters thru argument lists to routine
c  pgrib.
c        Tim Sweeney, HRL                              Nov 1998
c
c  Added TDL GRIB2.  Variable jdes (product definition) controls
c  single file for each duration or concatenate all durations
c  into a single GRIB bulletin.  jdes also selects NCEP GRIB1
c  (jdes = 0 or 1) or TDL GRIB2 (jdes = 2 or 3).
c        Tim Sweeney, HRL                              Jun 1999
c
c  Moved GRIB related routines into separate library.
c        Tim Sweeney, HRL                              Mar 2000
c
c.......................................................................
c
      character*1 fmt,kbuf(250000)
      character*2 bname
      character*4 gfftyp,eprod,outtyp
      character*8 goutid
      character sdatim*20, vdatim*20
      character*8 proces,aproc(5)
      character*10 user
      character*128 fname,grbopa
c
      real ffg(250000),wffg(250000)
      integer*2 ihfld(*)
      integer ifld(1),ibmap(250000),igds(18)
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/iodev'
      include 'ffg_inc/progrm'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/count'
      include 'ffg_inc/gridsz'
      include 'ffg_inc/ffgrid'
      include 'ffg_inc/timez'
      include 'prodgen_inc/poptns'
      include 'prodgen_inc/propar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpgffg.f,v $
     . $',                                                             '
     .$Id: gpgffg.f,v 1.8 2004/09/13 15:03:35 scv Exp $
     . $' /
C    ===================================================================
C
      data gfftyp/ 'grff' /
      data outtyp/ 'outu' /
      data eprod/ 'NNNN' /
      data ibmap/250000*0/
c
c
      bname = ' '
c
      call prbug ('gpgffg',1,1,ibug)
c
      isearch = 0
      call get_apps_defaults ('grib_ptbl_search',16,fname,i)
      if (fname(1:1).eq.'1') isearch = 1
c
c  load built-in xmrg and GRIB parameter tables
      call loadtbl (iupr,ibug,istat)
c
c  input gribparm file, if any, to update parameter tables
      iunit = 99
      call pintbl (iunit,iutw,iupr,ibug,mptnum,istat)
c
c  units conversion
      ucon = 25.4
      if (ibug.gt.3) ucon = 1.0
c
c  file version
      fvers = po(1)
c
c  get user info containing southwest corner of HRAP grid field
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
c
      if (kgridf.eq.0) then
c     get gridded guidance
         call getgrd (gfftyp,idurt,ndur,nrow,ncol,mxd,mxr,mxc,
     +                msrow,mwcol,xver,usrnam,sdatim,aproc,vdatim,mxval,
     +                ihfld,gg,ibline,istat)
         if (istat.ne.0) then
            write (iutw,5)
            if (iupr.ne.iutw) write (iupr,5)
5     format (' ERROR: cannot read gridded guidance file.')
            nerr = nerr + 1
            go to 180
            endif
         user=' '
         call umemov (usrnam,user,2)
         if (ibug.eq.1) write (iutw,*) 'in gpgffg - user=',user
         else
            write (iutw,10)
            if (iupr.ne.iutw) write (iupr,10)
10    format (/ 'Gridded guidance read from database earlier.')
         endif
c
c  use RFC HRAP box if local not given
      if (lsrow.eq.0) lsrow = msrow
      if (lwcol.eq.0) lwcol = mwcol
      if (lncol.eq.0) lncol = ncol
      if (lnrow.eq.0) lnrow = nrow
c
c  determine beginning row and column
      i = lsrow - msrow
      if (i.lt.0) lsrow = msrow
      krow = lsrow - msrow + 1
      i = lwcol - mwcol
      if (i.lt.0) lwcol = mwcol
      kcol = lwcol - mwcol + 1
c
c  determine ending row and column
      i = lsrow + lnrow - msrow - nrow
      if (i.gt.0) lnrow = lnrow - i
      lasrow = krow + lnrow - 1
      i = lwcol + lncol - mwcol - ncol
      if (i.gt.0) lncol = lncol - i
      lascol = kcol + lncol - 1
      nbins = lncol*lnrow

      if (ibug.gt.0) write (iud,20) lwcol,mwcol,lncol,kcol,lascol,
     +   lsrow,msrow,lnrow,krow,lasrow,nbins
20    format (' lwcol=',i3,'  mwcol=',i3,'  lncol=',i3,'  kcol=',
     +       i3,'  lascol=',i3 /
     +   ' lsrow=',i3,'  msrow=',i3,'  lnrow=',i3,'  krow=',
     +       i3,'  lasrow=',i3 /
     +   ' nbins=',i6)
c
c  set GRIB parameters:
c     jdes  GRIB  function
c     ----  ----  --------
c       0     1   separate files
c       1     1   single file bulletin
c       2     2   separate files
c       3     2   single file bulletin
c
c  process each duration
      do 170 idr=1,ndur
         idur = idurt(idr)
         if (idr.gt.1.and.(jdes.eq.1.or.jdes.eq.3) ) then
            icom = 0
            else
c        make output filename from 'prodid' and duration
c        i.e., TUA plus duration TUA1, TUA3, TUA6
               call umemov (prodid,goutid,2)
               call get_apps_defaults ('ffg_grib_out',12,grbopa,lgrbo)
               irow = 0
               if (jdes.eq.0.or.jdes.eq.2) then
                  call drname (idur,irow,goutid)
                  endif
               call mkfnam (grbopa,lgrbo,goutid,fname,istat)
               lfname = lenstr(fname)
               call gbf_wopn (fname,lfname,istat)
               if (istat.ne.0) go to 170
               icom = 1
            endif
c     transfer each HRAP row to single dimension array ffg
c     most southern row starts at ffg(1)
         do 70 irow=krow,lasrow
            k = (irow-krow)*lncol
c        transfer each column in row
            do 30 icol=kcol,lascol
               j = icol - kcol + 1
               gf = gg(idr,irow,icol)
               if (gf.ge.0.0) then
                  ngrid = ngrid + 1
                  i = int(gf*ucon)
                  ffg(k+j) = i
                  else
                     ffg(k+j) = -50.
                  endif
30             continue
            if (ibug.eq.1) then
               kpc = k + 1
               laspc = k + lncol
               write (iud,40) idur,irow,kpc,laspc
40    format (' idur=',i2,' irow=',i3,' column=',i6,
     +        '  last col=',i6,' ffg(m)=')
               if (ibug.eq.1) then
                  write (iud,50) (ffg(m),m=kpc,laspc)
50    format (25f5.0)
                  else
                     write (iud,60) (ffg(m),m=kpc,laspc)
60    format (25f4.1)
                  endif
               endif
70          continue
c     date data was processed
         nzmo = -1
         call infxdt (nzmo,nzda,nzyr,nzhr,nzmn,nzsc,sdatim)
c     get ffg valid date (computation date) from vdatim
         komo = -1
         call infxdt (komo,koda,koyr,kohr,komn,ivalhr,vdatim)
c     set up parameters for GRIB based on xmrg parameters
         proces = aproc(idr)
         call xm2grib (iutw,iupr,iud,xver,user,proces,isearch,ivalhr,
     +                 komo,koda,koyr,kohr,komn,
     +                 mwcol,msrow, nsbctr,iocent,mptnum,lptver,
     +                 iresfl,rlav,rlov,iscan,iparm,modlid,ndgrid,
     +                 itunit,nturef,ntufc,itrang,ipkflg,inbmap,refval,
     +                 ibinf,idec,iwidth,idatyp,wmo,senof,jerr)
         if (jerr.ne.0) then
            nerr = nerr + 1
            go to 170
            endif
c     select grid
         inat = 0
         call griddef (iutw,iupr,ibug,ndgrid,inat,
     +                 lwcol,lsrow,lncol,lnrow,
     +                 ffg,wffg,igds,mxbmap,ibmap,nwarn,ier)
         if (ier.gt.0) then
            write (iutw,80) fname(1:lfname)
80    format (' WARNING: file ',a,' not written.')
            if (iupr.ne.iutw) write (iupr,80) fname(1:lfname)
            call updele (0,fname,iostat)
            go to 170
            endif
c     comms header
         fmt = 'u'
         call gpcomm (iupr,iud,ibug,iuu,icom,fmt,pid,cct,
     +                wmo,senof,nzyr,nzmo,nzda,nzhr,nzmn)
         if (jdes.le.1) then
c        encode using NCEP GRIB (GRIB1)
            call engrib (iupr,iuu,ibug, koyr,komo,koda,kohr,komn,
     +         lwcol,lncol,lsrow,lnrow, igds,
     +         mptnum,iocent,modlid,ndgrid,iparm,itunit,nturef,ntufc,
     +         itrang,nsbctr,ibinf,idec,iresfl,rlov,iscan,ipkflg,
     +         idatyp,kbuf,ifld,ffg,wffg,ibmap, itot,istat)
            else
c        encode using TDL GRIB (GRIB2)
            L3264B = 64
            call engrib2 (iupr,iuu,iud,ibug,
     +         kyr,kmo,kda,khr,kmn, kzyr,kzmo,kzda,kzhr,kzmn,
     +         nsbctr,iocent,senof,mptnum,lptver,lwcol,ncol,lsrow,nrow,
     +         iresfl,rlav,rlov,iscan,iparm,modlid,nturef,itunit,
     +         ntufc,ipkflg,inbmap,refval,ibinf,idecf,iwidth,
     +         ibmap, mfld,fld, pid,cct,wmo,L3264B,istat)
            endif
         if (istat.eq.0) then
            if (jdes.eq.0.or.jdes.eq.2) call gbf_clos (ier)
            write (iutw,90) wmo,senof,nzda,nzhr,nzmn,
     +         itot,fname(1:lfname)
90    format (' NOTE: GRIB Bulletin ''',a4,a3,a4,1x,3i2.2,
     +   ''' with length ',i6,' bytes written to file ',a,'.')
            if (iupr.ne.iutw) write (iupr,90) wmo,senof,nzda,nzhr,nzmn,
     +         itot,fname(1:lfname)
            else
               nerr = nerr + 1
            endif
         if (ibug.eq.3) then
            write (iud,100) pid,idur
100   format (/ ' GRIDDED GUIDANCE - ',2a4,a1,' - ',i2,' hr ',
     +  'Duration',5x,'(north row first)')
            do 130 irow = lasrow,krow,-1
               is = (irow-krow)*lncol + 1
               ie = is + lncol - 1
c           north row on top
               write (iud,110) idur,irow,is,ie
110   format (' idur=',i2,' irow=',i3,' is=',i6,' ie=',i6,' ffg(i):')
               write (iud,120) (ffg(i),i=is,ie)
120   format (10(1x,f6.1))
130            continue
            ipbmap=0
            if (ipbmap.eq.1) then
               write (iud,*) 'inbmap=',inbmap
               if (inbmap.eq.0) then
                  write (iud,140)
140   format (' BIT MAP ARRAY (north row first)')
                  do 150 irow=lasrow,krow,-1
                     is = (irow-krow)*lncol + 1
                     ie = is + lncol - 1
                     write (iud,160) (ibmap(i),i=is,ie)
150               continue
160   format (1x,100i1)
                  endif
               endif
            endif
170      continue
c
      if (jdes.eq.1.or.jdes.eq.3) call gbf_clos (ier)
c
180   return
c
      end
