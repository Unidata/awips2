c  =====================================================================
c  pgm:  usrinf
c
c  variables are passed in common blocks
c
c  =====================================================================
c
      subroutine usrinf (icaller)
c
c.......................................................................
c
c  Routine to process user controls.
c
c.......................................................................
c    Initially written by
c         Tim Sweeney, HRL                                 Dec 1992
c
c   Added global control to adjust flow for grids.  Rearranged
c   flow adjust controls and intensity controls.
c         Tim Sweeney, HRL                                 Nov 1997
c
c  Added options to adjust for high flow by reducing
c  threshold runoff by storm runoff amount
c         Tim Sweeney, HRL                                 Feb 1999
c
c  Added control to extend number of columns and rows beyond
c  defined grids to reduce number of holes
c         Tim Sweeney, HRL                                 Aug 1999
c
c  Added option to check for decreasing FFG values with duration.
c  If decreasing, set a longer duration equal to previous shorter
c  duration.
c         Tim Sweeney, HRL                                 Sep 1999
c
c  Made average storm duration algorithm for API-CIN rainfall-
c  runoff model an option (irctlg and irctlh = 2 or 3)
c         Tim Sweeney, HL                                  Jan 2001
c
c  Modified values for iameth to allow value of 3 to assign
c  zone FFG computed from headwater algorithms to HRAP grid.
c         Tim Sweeney, HL                                  Mar 2001
c.......................................................................
c
      character*2 resp
      character*2 bname
      character*4 izone,parmtp,dtype
      character*4 namtyp
      character*4 accmode
      character*8 ident
      character*8 filnam,parmid
      character*80 line
      character*128 pthnam
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodev'
      include 'ffg_inc/gpo'
      include 'ffg_inc/uinfo'
      include 'hclcommon/hdflts'
c
      logical*4 opened
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/usrinf.f,v $
     . $',                                                             '
     .$Id: usrinf.f,v 1.8 2004/09/13 14:24:15 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('usrinf',1,1,ibug)
c
      bname = ' '
      mpx = mpo
      dtype='uinf'
      parmtp = 'USER'
c
c  get user information from file
      kod = 3
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.gt.1) go to 600
      isave = 0
      if (uvers.lt.0.0) isave = 1
      igetinf = 0
c
      if (icaller.eq.1) go to 10
      if (icaller.eq.2) go to 380
      write (iutw,5) 'icaller',icaller
5     format (' ERROR: in usrinf - invalid value of variable ',a,
     +   ' (',i2,').')
      go to 600
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  guidance computation controls
c
10    write (iutw,20)
20    format (/ 10x,'USER CONTROLS MENU' /)
c
c  print maximum and minimum extremum for Gridded and Headwater FFG
c  values
      iprint = 1
      icheck = 0
      call usrext (igetinf,icaller,iprint,icheck,ifound,istat)
c
      write (iutw,60) gbank,irctlg,irctlh,iqctlg,iqctlh,iameth
60    format (/
     +   10x,'Bankfull Factor: ',5x,'(21)',f5.2 /
     +   10x,'Runoff Adjust:   ',5x,'(22)',i2,16x,'(23)',i2 /
     +   10x,'High Flow Adjust:',5x,'(24)',i2,16x,'(25)',i2 /
     +   10x,'Area Method (1-Minimum [2]-Average) Grid:   (26)',i2
     +   )
c
      write (iutw,70) cpzone
70    format (
     +   10x,'Computer Time Zone (E,C,M,P,A,H,N,[Z]):     (27) ',a1
     +   )
c
      if (ibug.eq.1) write (iud,*) 'in usrinf - iofs=',iofs
      if (iofs.eq.0) then
         write (iutw,80) mwcol,ncol,msrow,nrow
80    format (/
     +   10x,'FFG HRAP grid subset from OFS:  (28)' /
     +   10x,2x,'West column:      ',i5 /
     +   10x,2x,'Number of columns:',i5 /
     +   10x,2x,'South row:        ',i5 /
     +   10x,2x,'Number of rows:   ',i5
     +   )
            write (iutw,83) parmtp
83    format (10x,'NOTE: FFG HRAP grid subset was set from ',
     +   'OFS Preprocessor Parametric Data Base ',a,' parameters.')
            write (iutw,85) loclo,nlstzo,usrnam,cpzone
85    format (10x,'NOTE: the following FFG parameters were set from ',
     +      'OFS general user parameters:' /
     +   10x,6x,'loclo=',i2,' nlstzo=',i2,' usrnam=',a,' cpzone=',a)
            if (usrnam.eq.'none') then
               write (iutw,87) usrnam
               if (iupr.ne.iutw) write (iupr,87) usrnam
87    format (/ ' WARNING in usrinf: user name is ''',a,'''.')
               nwarn = nwarn + 1
               endif
         else
            write (iutw,90) mwcol,ncol,msrow,nrow
90    format (/
     +   10x,'HRAP grid subset' /
     +   10x,2x,'West column:        (28)',i5 /
     +   10x,2x,'Number of columns:  (29)',i5 /
     +   10x,2x,'South row:          (30)',i5 /
     +   10x,2x,'Number of rows:     (31)',i5
     +   )
         endif
c
      mopt = 33
      write (iutw,100) ngfil
100   format (/
     +   10x,'Grid Fill Control:' /
     +   10x,2x,'([0]-off  1 to 5-fill col/row  6-fill grid):  (32)',i2
     +   )
      write (iutw,110) ickval
110   format (/
     +   10x,'Check Decreasing FFG:' /
     +   10x,2x,'([0]-no  1-grids,zones,hdwtrs  2-zones,hdwtrs):  (33)',
     +       i2
     +   )
c
ccc      if (nfeat.gt.0) then
         mopt = 34
         write (iutw,120) iwats
120   format (/
     +   10x,'Water Supply Runoff:' /
     +   10x,2x,'([0]-no  1-per SQMI  2-total area):  (34)',i2
     +   )
         if (iwats.gt.0) write (iutw,130) rnfl
130   format (13x,'Rainfall:',6(f4.1,1x))
ccc      endif
c
      if (ibug.gt.0) then
         mopt = 91
         write (iutw,140) nfeat
140   format (' New feature control ([0]-off  1-on):        (90)',i2)
         write (iutw,150) iofs
150   format (' iofs control ([0]-off  1-on):               (91)',i2)
         endif
c
160   write (iutw,170) mopt
170   format (/ ' Select (1-',i2,
     +   ', f-file, l-list or <return>-menu): ',$)
c
      read (iutr,'(a)',iostat=iostat) line
      if (iostat.ne.0) then
         write (iutw,180) line
180   format (' ERROR: invalid input format: ',a)
         go to 160
         endif
c
      resp=line
      if (ibug.eq.1) write (iud,*) 'in usrinf - iutr=',iutr,
     +   ' line=',line,' resp=',resp
      if (resp.eq.' ') go to 590
      if (resp.eq.'f') go to 360
      if (resp.eq.'l') go to 370
c
      ibeg=1
      lresp=lenstr(resp)
      iprerr=0
      call ufa2i (resp,ibeg,lresp,iopt,iprerr,iutw,ierr)
      if (ierr.ne.0) then
         write (iutw,180) resp
         go to 160
         endif
c
      if (iopt.lt.1) then
         write (iutw,190) iopt
190   format (' ERROR: ',i2,' is an invalid option.')
         go to 160
c
      else if (iopt.le.20) then
195      write (iutw,200)
200   format (' Enter new value: ',$)
         read (iutr,'(a)',iostat=iostat) line
         if (iostat.ne.0) then
            write (iutw,180) line
            go to 195
            endif
         if (line.ne.' ') then
            call ubegin (line,len(line),lbegin)
            istart = lbegin
            iwidth = 8
            call uffir (line,istart,iwidth,ivalue,rvalue,nxt,istat)
            if (istat.eq.1) go to 195
            ext(iopt)=rvalue
            isave = 1
            endif
         go to 10
c
      else if (iopt.eq.21) then
         write (iutw,220)
220   format (' Enter value (default=1.10): ',$)
         read (iutr,'(f5.2)') rin
         if (rin.eq.0.0) rin = 1.10
         if (rin.ne.0.0) then
            gbank = rin
            isave = 1
            endif
         go to 10
c
      else if (iopt.le.23) then
         write (iutw,240)
240   format (' Runoff Adjust Controls:' /
     +       2x,'0 = off' /
     +       2x,'1 = reference runoff options' /
     +       2x,'2 = API-CIN average storm duration algorithm' /
     +       2x,'3 = both 1 and 2')
         write (iutw,245)
245   format (' Enter Runoff Adjust Control: ',$)
         read (iutr,'(i1)') i
         if (i.lt.0.or.i.gt.3) then
            write (iutw,250) i
250   format (' ERROR: ',i2,' is an invalid value.')
            go to 160
            endif
         if (iopt.eq.22) irctlg = i
         if (iopt.eq.23) irctlh = i
         isave = 1
         go to 10
c
      else if (iopt.le.25) then
         write (iutw,260)
260   format (' Enter control (0, 1, or 2): ',$)
         read (iutr,'(i1)') i
         if (i.lt.0.or.i.gt.2) then
            write (iutw,250) i
            go to 160
            endif
         if (iopt.eq.24) iqctlg = i
         if (iopt.eq.25) iqctlh = i
         isave = 1
         go to 10
c
      else if (iopt.eq.26) then
         write (iutw,270)
270   format (' Enter control (1 or 2): ',$)
         read (iutr,'(i1)') i
         if (i.lt.0.or.i.gt.3) then
            write (iutw,250) i
            go to 160
            endif
         iameth = i
         isave = 1
         go to 10
c
      else if (iopt.eq.27) then
         write (iutw,280)
280   format (' Enter zone designator: ',$)
         read (iutr,'(a2)') izone
         if (izone.ne.' ') cpzone = izone
         isave = 1
         go to 10
c
      else if (iopt.eq.28.and.iofs.eq.0) then
c     read OFS controls
         write (iutw,*)
         call upinio
         call rpppco (istat)
         parmid=' '
         irec = 0
         mp = mpx
         call rpprec (parmid,parmtp,irec,mp,po,nfill,irecnx,istat)
         if (istat.eq.0) then
            mwcol = po(32)
            ncol  = po(33)
            msrow = po(34)
            nrow  = po(35)
            write (iutw,283) parmtp
283   format (' NOTE: FFG HRAP grid subset has been set from ',
     +   'OFS Preprocessor Parametric Data Base ',a,' parameters.')
            isave = 1
            else
               write (iutw,320) parmtp,istat
               if (iupr.ne.iutw) write (iupr,320) parmtp,istat
320   format (' ERROR: reading ',a,' parameters. rpprec istat=',i4)
            endif
c     get local and nlstz, clock time zone and user name from OFS
         call hgetpm (istat)
         if (istat.eq.0) then
            loclo = local
            nlstzo = nlstz
            call umemov (hnamrf,usrnam,2)
            call umemov (clkzon,cpzone,1)
            write (iutw,285) loclo,nlstzo,usrnam,cpzone
285   format (' NOTE: the following FFG parameters have been set from ',
     +      'OFS general user parameters:' /
     +   1x,6x,'loclo=',i2,' nlstzo=',i2,' usrnam=',a,' cpzone=',a)
            else
               write (iutw,290) istat
               if (iupr.ne.iutw) write (iupr,290) istat
290   format (' ERROR: reading OFS user parameters. hgetpm istat=',i2)
               loclo = 0
               nlstzo = -5
               endif
         if (ibug.eq.1) write (iud,300) istat,loclo,nlstzo,usrnam,
     +      cpzone
300   format (' istat=',i2,' loclo=',i4,' nlstzo=',i4,' usrnam=',a,
     +   ' cpzone=',a)
         go to 10
c
      else if (iopt.le.31) then
         write (iutw,200)
         read (iutr,'(i1)',err=10) i
         if (i.le.0) go to 10
         if (iopt.eq.28) mwcol = i
         if (iopt.eq.29) ncol  = i
         if (iopt.eq.30) msrow = i
         if (iopt.eq.31) nrow  = i
         isave = 1
         go to 10
c
      else if (iopt.eq.32) then
         write (iutw,330)
330   format (' Enter control (0 to 6): ',$)
         read (iutr,'(i1)',err=10) i
         if (i.ge.0.and.i.le.6) then
            ngfil = i
            isave = 1
            else
               write (iutw,250) i
               go to 160
            endif
         go to 10
c
      else if (iopt.eq.33) then
         write (iutw,260)
         read (iutr,'(i1)',err=10) i
         if (i.ge.0.and.i.le.2) then
            ickval = i
            isave = 1
            else
               write (iutw,250) i
               go to 160
            endif
         go to 10
c
      else if (iopt.eq.34) then
         write (iutw,260)
         read (iutr,'(i1)') i
         if (i.lt.0.or.i.gt.2) then
            write (iutw,250) i
            go to 160
            endif
         iwats = i
         if (iwats.gt.0) then
            write (iutw,130) rnfl
            write (iutw,340)
340   format (' Change rainfall (y or [n])? ',$)
            read (iutr,'(a2)') resp
            is = 2
            if (resp(1:1).eq.'Y'.or.resp(1:1).eq.'y') then
               nrnfl = 6
               nperg = 1
               call edvrag (is,iutr,iutw,rnfl,nrnfl,nperg,ngrps)
               endif
            isave = 1
            endif
         go to 10
c
      else if (iopt.eq.90) then
         write (iutw,350)
350   format (' Enter control (0 or 1): ',$)
         read (iutr,'(i1)',err=10) i
         if (i.eq.0.or.i.eq.1) then
            nfeat = i
            isave = 1
            else
               write (iutw,250) i
               go to 160
            endif
         go to 10
c
      else if (iopt.eq.91) then
         write (iutw,350)
         read (iutr,'(i1)',err=10) i
         if (i.eq.0.or.i.eq.1) then
            iofs = i
            isave = 1
            else
               write (iutw,250) i
               go to 160
            endif
         go to 10
c
      else
         write (iutw,190) iopt
         go to 160
      endif
c
c  get parameters from file
360   call mffprm_dummy (dtype,ierr)
      if (ierr.eq.0) then
         isave = 1
         go to 10
         else
            go to 160
         endif
c
c  output parameters to file
370   mcidx=1
      call duffpm_dummy (dtype,mcidx,cidx,mpo,po)
      go to 160
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  product generation controls
c
380   write (iutw,20)
c
c  print maximum and minimum extremum for Gridded and Headwater FFG
c  values
      iprint = 1
      icheck = 0
      call usrext (igetinf,icaller,iprint,icheck,ifound,istat)
c
      write (iutw,390) ising,icom,iffpe,icent,igduty
390   format (/
     +  10x,'FFG Product Files   ([0]-Many 1-Single):        (1)',i2 //
     +  10x,'Comms System   ([1]-AWIPS  2-AFOS):             (2)',i2 //
     +  10x,'FFG Physical Elements   ([1]-PF  2-PP):         (3)',i2 //
     +  10x,'Year Format   ([4]-ccyy  2-yy):                 (4)',i2 //
     +  10x,'Append Name   ([0]-No 1-Office 2-Fcstr 3-Both): (5)',i2)
c
      if (igduty.gt.0) then
c     duty forecasters
         if (mdf.eq.0) then
            write (iutw,400)
400   format (5x,2x,'No Duty Forecasters are defined.')
            else
               write (iutw,410)
410   format (5x,2x,'Duty Forecasters:')
               nperg=16
               ix1=1
               ix2=nperg
               do 430 idf=1,mdf
                  write (iutw,420) idf,(fcstr(i),i=ix1,ix2)
420   format (10x,i2,') ',16a4)
                  ix1=ix1+nperg
                  ix2=ix2+nperg
430               continue
            endif
         endif
c
      mopt = 5
c
440   write (iutw,450) mopt
450   format (/ ' Select (1-',i1,
     +   ', f-file, l-list or <return>-menu): ',$)
c
      read (iutr,'(a)',iostat=iostat) line
      if (iostat.ne.0) then
         write (iutw,180) line
         go to 160
         endif
c
      resp=line
      if (ibug.eq.1) write (iud,*) 'in usrinf - iutr=',iutr,
     +   ' line=',line,' resp=',resp
      if (resp.eq.' ') go to 590
      if (resp.eq.'f') go to 583
      if (resp.eq.'l') go to 585
c
      ibeg=1
      lresp=lenstr(resp)
      iprerr=0
      call ufa2i (resp,ibeg,lresp,iopt,iprerr,iutw,ierr)
      if (ierr.ne.0) then
         write (iutw,180) resp
         go to 160
         endif
c
      if (iopt.lt.1) then
         write (iutw,190) iopt
         go to 440
         endif
c
      if (iopt.eq.1) then
         write (iutw,470)
470   format (' Enter control (0 or 1): ',$)
         read (iutr,'(i1)') i
         if (i.ge.0.and.i.le.3) then
            ising = i
            isave = 1
            endif
         go to 380
         endif
c
      if (iopt.eq.2) then
         write (iutw,480)
480   format (' Enter control (1 or 2): ',$)
         read (iutr,'(i1)') i
         if (i.ge.1.and.i.le.2) then
            icom = i
            isave = 1
            endif
         go to 380
         endif
c
      if (iopt.eq.3) then
         write (iutw,480)
         read (iutr,'(i1)') i
         if (i.ge.1.and.i.le.2) then
            iffpe = i
            isave = 1
            endif
         go to 380
         endif
c
      if (iopt.eq.4) then
         write (iutw,490)
490   format (' Enter control (2 or 4): ',$)
         read (iutr,'(i1)') i
         if (i.eq.2.or.i.eq.4) then
            icent = i
            isave = 1
            endif
         go to 380
         endif
c
      if (iopt.eq.5) then
c     edit duty forecaster list
         write (iutw,500)
500   format (' Enter control (0, 1, 2, or 3): ',$)
         read (iutr,'(i1)') i
         if (i.ge.0.and.i.le.3) igduty = i
         if (igduty.eq.0) go to 380
         call typent (resp)
         if (resp.eq.'t') then
            is = 3
            write (iutw,520)
520   format (15x,'Edit Duty Forecasters' /)
            if (mdf.eq.0) then
               fcstr(1) = 'none'
               fcstr(2) = '    '
               mdf = 1
               endif
            write (iutw,530)
530   format (3x,'NO  ID',6x,'Name/Phone' /
     +        3x,'==  ',7('='),'-',55('=') )
            mx = nperg*mdf
            call edvcag (is,iutr,iutw,mx,fcstr,nperg,mdf)
            if (fcstr(1).eq.'none') mdf = 0
            isave = 1
            go to 380
            endif
         if (resp.eq.'f') then
c        open file
535         write (iutw,540)
540   format (/ ' Enter pathname or filename: ',$)
            read (iutr,'(a)',end=600) pthnam
            namtyp='anam'
            accmode = 'rw'
            kod = 1
            call fixopn (ident,namtyp,pthnam,accmode,kod,iuin,istat)
            if (istat.ne.0) then
               write (iutw,550) pthnam(1:lenstr(pthnam))
550   format (' ERROR: file ',a,' not found or cannot be opened.')
               go to 535
               endif
            mdf = 0
            do 570 m=1,20
               ib = (m-1)*16 + 1
               ie = ib + 15
               read (iuin,560,end=380) (fcstr(i),i=ib,ie)
560   format (16a4)
               mdf = mdf + 1
570            continue
            isave = 1
            go to 380
            endif
         endif
c
      write (iutw,580) resp(1:lenstr(resp))
580   format (' ERROR: ',a,' is an invalid option.')
      go to 440
c
c  get parameters from file
583   mcidx=1
      call mpgprm (dtype,mcidx,cidx,mpo,po,ierr)
      if (ierr.eq.0) then
         isave = 1
         go to 380
         else
            go to 440
         endif
c
c  output parameters to file
585   ncidx=1
      call dupgpm (dtype,ncidx,cidx,mpo,po)
      go to 440
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  store parameters
590   if (isave.eq.1) then
         inquire (unit=iunit,iostat=iostat,opened=opened)
         if (ibug.eq.1) write (iud,*) 'in usrinf - iunit=',iunit,
     +      ' opened=',opened
         if (.not.opened) then
            filnam='usrinfo'
            usrtyp = 'user'
            accmode = 'rw'
            kod = 1
            call fixopn (filnam,usrtyp,pthnam,accmode,kod,iunit,istat)
            endif
         call strinf (iunit,po)
         else
            call upclos (iunit,bname,ierr)
         endif
c
600   return
c
      end
