c  =====================================================================
c  pgm:  mgrid
c
c  =====================================================================
c
      subroutine mgrid
c
c..............................................................
c
c  Define parameters for gridded threshold runoff
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*1 resp,rans
      character*2 bname
      character*4 grotyp,grdid(2),gtype
      character*4 namtyp
      character*4 accmode
      character*4 meth,cdur,sbasn,grdres,terp,chng
      character*8 wfo,ident
      character*10 huc
      character*20 sdatim,vdatim
      character*128 pthnam
c
      parameter (mldurt=5)
      dimension ldurt(mldurt)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/progrm'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/gridsz'
      include 'ffg_inc/rogrid'
      include 'ffg_inc/gridpm'
      include 'ffg_inc/timez'
      include 'ffg_inc/gpo'
      include 'ffg_inc/ghfld'
      include 'ffg_inc/gbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/mgrid.f,v $
     . $',                                                             '
     .$Id: mgrid.f,v 1.7 2004/01/30 17:50:13 scv Exp $
     . $' /
C    ===================================================================
C
      data grotyp/ 'grro'/
      data namtyp/ 'anam'/
      data bname/ '  ' /
      data usrtyp/ 'user'/
      data grdid/ 'inpg','rid '/
c
c
      call prbug ('mgrid',1,1,ibug)
c
      kgridf = 0
      ichnge = -1
      gtype = ' '
c
c  get user info
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.ne.0) then
         write (iutw,5)
         if (iupr.ne.iutw) write (iupr,5)
5     format (' WARNING: cannot define grid parameters ',
     +   'because user controls are not defined.')
         go to 360
         endif
c
      if (ibug.gt.0) write (iud,*) ' mwcol=',mwcol,' ncol=',ncol,
     +   ' msrow=',msrow,' nrow=',nrow
c
c  convert computer clock to Z time
      call tzcnum (cpzone,ictz)
      lcptz = 1
      call datimz (lcptz,ictz,nmo,nda,nyr,nhr,nmn,kzmo,kzda,kzyr,kzhr,
     +             kzmn,ndawk,lsav,mxday,julda)
      call infxdt (kzmo,kzda,kzyr,kzhr,kzmn,0,sdatim)
      vdatim = sdatim
c
10    write (iutw,20)
20    format (/ 15x,'GRIDDED THRESHOLD RUNOFF PARAMETERS')
c
      if (mwcol.eq.-900.and.msrow.eq.-900) write (iutw,30)
30    format (' WARNING: southwest HRAP corner not defined.')
c
40    iascii = 0
      if (gtype.eq.'grro'.or.gtype.eq.'grff') then
         write (iutw,50)
50    format (/' Enter option (r-threshR  f-file  t-terminal',
     +        '  [m]-menu): ',$)
         else
            write (iutw,60)
60    format (/' Enter option (r-threshR  f-file  t-terminal',
     +        '  [m]-menu): ',$)
         endif
c
      read (iutr,'(a)',end=65) resp
      go to 67
65    resp = ' '
67    if (resp.eq.' ') resp = 'm'
c
c  edit grid
      imenu = 0
      if (resp.eq.'T'.or.resp.eq.'t') go to 70
      go to 80
70       kgridf = 0
         call edgrid (mxdurg,nrow,ncol,gtype,mbx,bx,ibx,ihfld,ichnge,
     +      istat)
         imenu = 1
         go to 330
c
c  ASCII file input
80    if (resp.eq.'F'.or.resp.eq.'f') go to 90
      go to 160
90       iascii = 1
100      write (iutw,110)
110   format (' Enter pathname, filename or <return>: ',$)
         read (iutr,'(a)') pthnam
         if (pthnam.eq.' ') go to 40
c     open file to input gridded threshold runoff info
         accmode = 'r'
         kod = 1
         call fixopn (ident,namtyp,pthnam,accmode,kod,iuu,istat)
         if (ibug.eq.1) write (iud,*) 'in mgrid - ',
     *      'pthnam=',pthnam(1:lenstr(pthnam)),' istat=',istat
         if (istat.ne.0) then
            write (iutw,120) pthnam(1:lenstr(pthnam))
            if (iupr.ne.iutw) write (iupr,120) pthnam(1:lenstr(pthnam))
120   format (' ERROR: file ',a,' not found.')
            go to 90
            endif
130      write (iutw,140)
140   format (' Enter duration of runoff (1, 3, 6, 12 or 24 hours): ',$)
         read (iutr,'(a,t1,i4)',err=130) cdur,idur
         if (cdur.eq.' ') go to 90
         if (cdur(1:2).eq.'1 ') then
            ib = 1
            ie = 1
            else if (cdur(1:2).eq.'3 ') then
               ib = 2
               ie = 2
            else if (cdur(1:2).eq.'6 ') then
               ib = 3
               ie = 3
            else if (cdur(1:2).eq.'12') then
               ib = 4
               ie = 4
            else if (cdur(1:2).eq.'24') then
               ib = 5
               ie = 5
            else
               write (iutw,150) cdur(1:lenstr(cdur)),'duration'
150   format (' ERROR: ',a,' is an invalid ',a,'.')
               go to 130
            endif
         go to 240
c
c  threshR file input
160   if (resp.eq.'R'.or.resp.eq.'r') go to 170
      go to 330
c     construct filename
170      iascii = 0
         call rppfil (grdid,usrtyp,kod,idev,mpo,po,npo,istat)
         if (istat.eq.0) then
            rewind (idev)
            call umemov (po(1),meth,1)
            call umemov (po(2),cdur,1)
            call umemov (po(3),sbasn,1)
            call umemov (po(4),grdres,1)
            call umemov (po(5),terp,1)
            else
               meth   = '4'
               cdur   = '0'
               sbasn  = 'a'
               grdres = '1'
               terp   = 's'
            endif
         ich = 0
c
180   write (iutw,190) meth,cdur,sbasn,grdres,terp
190   format (
     + 10x,'Method (1, 2, 3 or 4):                      (1) ',a /
     + 10x,'Duration (0-All  1-1 hr  3-3 hrs  6-6 hrs): (2) ',a /
     + 10x,'Basins (a-all  s-source):                   (3) ',a /
     + 10x,'Grid resolution in 4km units (1-1 unit):    (4) ',a /
     + 10x,'Interp (m-missing  n-not smooth  s-smooth): (5) ',a /
     + / 'Select (number or <return>-continue): ',$)
      read (iutr,'(a)') rans
      kf = 2
      nn = 1
      if (rans.eq.' ') go to 200
      if (rans.eq.'q') go to 360
      if (rans.eq.'1') then
         ich = 1
         chng = meth
         call edvca (kf,iutw,nn,chng)
         if (chng.eq.'1'.or.chng.eq.'2'.or.chng.eq.'3'.or.chng.eq.'4')
     +      meth = chng
         else if (rans.eq.'2') then
            ich = 1
            chng = cdur
            call edvca (kf,iutw,nn,chng)
            if (chng.eq.'0'.or.chng.eq.'1'.or.chng.eq.'3'.or.
     +          chng.eq.'6'.or.chng.eq.'12'.or.chng.eq.'24')
     +         cdur = chng
         else if (rans.eq.'3') then
            ich = 1
            chng = sbasn
            call edvca (kf,iutw,nn,chng)
            if (chng.eq.'a' .or. chng.eq.'s') sbasn = chng
         else if (rans.eq.'4') then
            ich = 1
            chng = grdres
            call edvca (kf,iutw,nn,chng)
            if (chng.eq.'1') grdres = chng
         else if (rans.eq.'5') then
            ich = 1
            chng = terp
            call edvca (kf,iutw,nn,chng)
            if (chng.eq.'m'.or.chng.eq.'n'.or.chng.eq.'s') terp = chng
         else
         endif
      go to 180
c
200   if (ich.eq.1) then
ccc        call umemov (meth,po(1),1)
ccc        call umemov (cdur,po(2),1)
ccc        call umemov (sbasn,po(3),1)
ccc        call umemov (grdres,po(4),1)
ccc        call umemov (terp,po(5),1)
c     write to file
         rewind (idev)
         iuse = 5
         call wppfil (idev,iuse,po,istat)
         call pstcod (istat,grdid,usrtyp,idev)
         endif
      call upclos (idev,bname,istat)
c
      if (cdur.eq.'0') then
         ib = 1
         ie = 3
         else if (cdur.eq.'1') then
            ib = 1
            ie = 1
         else if (cdur.eq.'3') then
            ib = 2
            ie = 2
         else if (cdur.eq.'6') then
            ib = 3
            ie = 3
         else if (cdur.eq.'12') then
           ib = 4
           ie = 4
         else if (cdur.eq.'24') then
           ib = 5
           ie = 5
         else
            go to 180
         endif
c
      if (terp.eq.'m') then
         terp = '    '
         else if (terp.eq.'n') then
            terp = '_ns '
         else if (terp.eq.'s') then
            terp = '_sm '
         endif
c
      write (iutw,210)
210   format (' Enter WFO: ',$)
      read (iutr,220) wfo
220   format (a)
      if (wfo.eq.' ') go to 40
      write (iutw,230)
230   format (' Enter HUC: ',$)
      read (iutr,220) huc
c
      lenw = len(wfo)
      lenh = len(huc)
      lenm = len(meth)
c
c  loop thru durations
240   call umemst (0,ldurt,mldurt)
      do 280 jdur=ib,ie
         iddur = idurt(jdur)
         call ui2c4 (iddur,cdur)
         lend = len(cdur)
         if (iascii.eq.0) then
            pthnam = wfo(1:lenw)//'/'//huc(1:lenh)//'/rval/rvalgrid_'
            li = len(pthnam)
            pthnam = pthnam(1:li)//meth(1:lenm)//'_'//cdur(1:lend)
            li = len(pthnam)
            pthnam = pthnam(1:li)//'_'//sbasn(1:1)//'_'//grdres(1:1)
            li = len(pthnam)
            pthnam = pthnam(1:li)//terp(1:3)
c        open file to input gridded threshold runoff info
            accmode = 'r'
            kod = 1
            call fixopn (ident,namtyp,pthnam,accmode,kod,iuu,istat)
            if (ibug.eq.1) write (iud,*) 'in mgrid - ',
     *         'pthnam=',pthnam(1:lenstr(pthnam)),' istat=',istat
            if (istat.ne.0) then
               write (iutw,250) pthnam(1:lenstr(pthnam))
               if (iupr.ne.iutw) then
                  write (iupr,250) pthnam(1:lenstr(pthnam))
                  endif
250   format (' ERROR: file ',a,' not found.')
               go to 40
               endif
            endif
c     initialize grid array to missing
         call inigrd (jdur,nrow,ncol,mxd,mxr,mxc,tro,istat)
         if (istat.ne.0) go to 350
c     read grid values
         gtype = grotyp
         mval = 0
         irec = 0
         narea = 0
260      call pingro (idur,narea,istat)
c     put grid values in array
         call putgro (istat)
         ichnge = 1
         if (kgridf.ne.2) go to 260
         call upclos (iuu,bname,istat)
         kgridf = 1
         ldurt(jdur)=idurt(jdur)
         if (irec.gt.0.and.narea.eq.0) narea = 1
         write (iutw,270) mval,narea,irec,
     +      pthnam(1:lenstr(pthnam))
270   format (' NOTE:',i6,' runoffs for ',i4,' areas ',
     +   'read from ',i6,' records in file ',a,'.')
         if (iupr.eq.iul) write (iupr,270) mval,narea,irec,
     +       pthnam(1:lenstr(pthnam))
280      continue
c
c  check if to store gridded values
290   if (kgridf.gt.0) then
         write (iutw,300)
300   format (' Save changes (y or n)? ',$)
         read (iutr,'(a)') rans
         if (rans.eq.'N'.or.rans.eq.'n') then
            ichnge = -1
            go to 350
            endif
         if (rans.eq.'Y'.or.rans.eq.'y') go to 310
         write (iutw,150) rans(1:lenstr(rans)),'option'
         go to 290
         endif
310   vdatim = sdatim
      write (iutw,*)
      if (iupr.ne.iutw) write (iupr,*)
      do 320 idr=1,mldurt
         idurz = ldurt(idr)
         if (idurz.gt.0) then
            call strgrd (gtype,idr,idurt,nrow,ncol,mxd,mxr,mxc,
     +                   msrow,mwcol,vers,usrnam,sdatim,vdatim,mxval,
     +                   ihfld,tro,istat)
            endif
320      continue
      kgridf = 3
      ichnge = 0
      gtype = ' '
      go to 350
c
330   if (kgridf.gt.0.and.ichnge.eq.1) then
         write (iutw,340)
340   format (' WARNING: changes not saved.')
         go to 290
         endif
c
350   if (imenu.eq.1) go to 10
      if (iascii.eq.1) go to 100
      if (resp.eq.'T'.or.resp.eq.'t') go to 70
      if (resp.eq.'F'.or.resp.eq.'f') go to 90
      if (resp.eq.'R'.or.resp.eq.'r') go to 170
      if (resp.eq.'M'.or.resp.eq.'m') go to 360
      go to 10
c
360   return
c
      end
