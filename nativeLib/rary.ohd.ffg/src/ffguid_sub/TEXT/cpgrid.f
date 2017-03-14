c  =====================================================================
c  pgm:  cpgrid
c
c  variables are passed in common blocks
c
c  =====================================================================
c
      subroutine cpgridx
c
c.......................................................................
c
c  compute gridded flash flood guidance values
c
c.....................................................................
c     Program initially written by
c           Tim Sweeney, HRL - Sept 1992
c
c  Added algorithm to estimate missing ro from surrounding values
c  and save the estimated value
c       Tim Sweeney, HRL                                   May 1996
c
c  Changed average storm duration adjustment algorithm for API-CIN
c  from intensity option to a permanent feature.
c  Added high flow adjust option using local stream gage.
c  Added intensity option to use runoff as ffg.
c       Tim Sweeney, HRL                                   Nov 1997
c
c  Moved algorithm that retrieves basin boundary info into a
c  separate routine.
c       Tim Sweeney, HRL                                   Feb 1998
c
c  Added option to reduce threshold runoff by amount equal to
c  storm runoff (API event models)
c       Tim Sweeney, HRL                                   Feb 1999
c
c  Added option to check basin and county area sizes (debug 5)
c       Tim Sweeney,HRL                                    Mar 1999
c
c  Added option to not use an ffg area in grid computations.
c  Control is via grid parameters and keying on ffg id.
c       Tim Sweeney, HRL                                   May 1999
c
c  Gridded values extended 3 columns before and after beginning
c  and ending columns in each row.  (was one column)
c       Tim Sweeney, HRL                                   Jul 1999
c
c  Added control for number of columns and rows to fill
c  to reduce number of holes in gridded field.
c       Tim Sweeney, HRL                                   Aug 1999
c
c  Added option to check for decreasing FFG values and,
c  if found, set equal to FFG for previous duration.
c       Tim Sweeney, HRL                                   Sep 1999
c
c  Changed API-CIN average storm duration algorithm to
c  an option (irctlg = 2 or 3).
c       Tim Sweeney, HL                                    Jan 2001
c
c  Added option to assign zone FFG computed via headwater
c  algorithm to HRAP grids within the zone boundary.
c  Set iameth = 3.
c       Tim Sweeney, HL                                    Mar 2001
c
c  Added option to fill in missing grid value from 5 of surrounding
c  8 grids.  Uses predominant value. (MR 1446)
c       Tim Sweeney, HL                                    May 2001
c
c  Added option to adjust computed ffg value using factors.
c  (iroptg = 5)
c  Added capability to specify percent impervious area.
c  Changed some logic to shorten processing time.
c  Added capability to run a single ffgid for testing.
c       Tim Sweeney, HL                                    May 2001
c.....................................................................
c
      character*2 bname/' '/
      character*4 bbtyp
      character*4 ffgtyp,gfftyp,grotyp,gpmtyp
      character*8 ffgid,bbid,rcvid,areaidz,lident
      character*8 otyprr,onamrr
      character*8 aproc(5),wrkid
      character*20 sdatim,vdatim,rdatim
      character*50 strng
      real qb(5)/5*0.0/
      integer ndays(12) /31,28,31,30,31,30,31,31,30,31,30,31/
      dimension pcfq(5),tid(2)
      dimension ff(4),numg(4)
      parameter (mfillg=6)
      dimension nfillg(mfillg)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/progrm'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/count'
      include 'ffg_inc/gridsz'
      include 'ffg_inc/rogrid'
      include 'ffg_inc/ffgrid'
      include 'ffg_inc/timez'
      include 'ffg_inc/igparm'
      include 'ffg_inc/gpo'
      include 'ffg_inc/gbx'
      include 'ffg_inc/linseg'
      include 'ffg_inc/ghfld'
      include 'ffg_inc/gidx'
      include 'ffg_inc/bsnsiz'
      include 'ffg_inc/hwparm'
      include 'common/ionum'
      include 'common/fctime'
      include 'common/fratng'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/cpgrid.f,v $
     . $',                                                             '
     .$Id: cpgrid.f,v 1.9 2004/09/13 14:23:09 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('cpgrid',1,1,ibug)
c
      ibug2 = 0
      ibug2u = iud
      if (iupr.ne.iutw) ibug2u = iupr
c
      strng='Gridded Flash Flood Guidance'
c
      negrid = nerr
      nwgrid = nwarn
c
      ndstr = 3
      conv = 3.281
      ixdim = 0
      hdwtyp = 'hffg'
      call umemst (0,nfillg,mfillg)
c
      if (ngrid.eq.-3.or.iameth.eq.3) then
c     iameth of 3 used to assign zone FFG computed from headwater
c     algorithms to HRAP grid
         ngrid = 0
         karea = 1
         else
            karea = 0
         endif
c
c  get user info
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.ne.0) then
         write (iutw,10) strng(1:lenstr(strng))
         if (iupr.ne.iutw) write (iupr,10) strng(1:lenstr(strng))
10    format (/ ' ERROR: computations for ',a,' will not be done ',
     +   'because user controls are not defined.')
         nerr = nerr + 1
         go to 880
         endif
      if (usrnam.eq.'none') then
         write (iutw,15) strng(1:lenstr(strng)),usrnam
         if (iupr.ne.iutw) write (iupr,15) strng(1:lenstr(strng)),usrnam
15    format (/ ' ERROR: computations for ',a,' will not be done ',
     +   'because the user name in the FFG user information file is ''',
     +   a,'''.')
         nerr = nerr + 1
         go to 880
         endif
c
c  check if gridded calculations are desired
      if (mxdurg.lt.0) then
         write (iutw,20) strng(1:lenstr(strng)),mxdurg
         if (iupr.ne.iutw) write (iupr,20) strng(1:lenstr(strng)),mxdurg
20    format (' ERROR: computations for ',a,' will not be done ',
     +   'because the value of the user control option is ',i2,' .')
         nerr = nerr + 1
         go to 880
         endif
c
      local = loclo
      nlstz = nlstzo
c
c  convert computer clock to Z time, if not
      call tzcnum (cpzone,ictz)
      lcptz = 1
      call datimz (lcptz,ictz,nmo,nda,nyr,nhr,nmn,kzmo,kzda,
     +             kzyr,kzhr,kzmn,ndawk,ldayl,mxday,julda)
      call infxdt (kzmo,kzda,kzyr,kzhr,kzmn,0,sdatim)
c
      if (ibug.gt.0) write (iud,*) 'mwcol=',mwcol, 'ncol=',ncol,
     +   ' msrow=',msrow,' nrow=',nrow
c
c  initialize gridded FFG array (all durations, rows, columns)
      do 40 idr=1,mxdurg
         do 40 irow=1,nrow
            do 30 icol=1,ncol
               gg(idr,irow,icol) = -9.8
30             continue
40       continue
c
      write (iutw,50) strng(1:lenstr(strng))
      if (iupr.ne.iutw) write (iupr,50) strng(1:lenstr(strng))
50    format (/ ' Computing ',a,'.')
c
      write (iutw,60) ngfil
      if (iupr.ne.iutw) write (iupr,60) ngfil
60    format (/ ' NOTE: value of the Grid Fill Control option is ',
     +   i1,'.')
      iset = 0
      if (iset.eq.1) then
         ngfil = 0
         write (iutw,65) ngfil
         if (iupr.ne.iutw) write (iupr,65) ngfil
65    format (/ ' NOTE: value of the Grid Fill Control option set to ',
     +   i1,'.')
         endif
c
c  check for single identifier for testing
      ffgid = ' '
      if (singid.ne.'none') then
         mh = 1
         if (singid.eq.'ibug2') then
            iset = 1
            if (iset.eq.1) then
               ibug2 = 1
               call umemov (singid,areaidz,2)
               write (iutw,70) ibug2,areaidz
               if (iupr.ne.iutw) write (iupr,70) ibug2,areaidz
70    format (/ ' WARNING: in cpgrid - variable ibug2 set to ',i1,
     +   ' for identifier ',a,'.')
               nwarn = nwarn + 1
               endif
            else
               ffgid=singid
               call umemov (singid,cidx(1,1),2)
               write (iutw,80) ffgid
               if (iupr.ne.iutw) write (iupr,80) ffgid
80    format (/ ' NOTE: computations will be done for identifier ',
     +   a,'.')
            endif
         endif
c
      if (karea.eq.0) go to 120
c
c  get index file to hffg locations
      call getidx (hdwtyp,idxdv,mcidx,po,mh,cidx,istat)
      call upclos (idxdv,bname,ic)
c
      write (iutw,90) mh
90    format (21x,'from ',i4,' Zones defined as headwaters' /)
      if (iupr.ne.iutw) write (iupr,80) mh
c
c  process hffg area file
      icidx = 0
100   icidx = icidx + 1
      if (icidx.gt.mh) go to 710
      if (cidx(1,icidx)(3:3).ne.'Z'.and.cidx(1,icidx)(3:3).ne.'C')
     +   go to 100
      wrkid = cidx(1,icidx)//cidx(2,icidx)
      ipos = 4
      iwidth = 3
      num = 1
      call ffa2i (wrkid,ipos,iwidth,num,nzon,istat)
      if (istat.gt.0.and.wrkid(4:6).ne.'NTY') go to 100
      hdid(1) = cidx(1,icidx)
      hdid(2) = cidx(2,icidx)
c
c  open and read hffg type parameters
      kod = 0
      call rppfil (hdid,hdwtyp,kod,ipdv,mpo,po,npo,istat)
      call upclos (ipdv,bname,ic)
      if (istat.eq.0) then
         call gethed (po)
         else
            write (iutw,110) hdid
            if (iupr.ne.iutw) write (iupr,110) hdid
110   format (' ERROR: headwater parameters not found for ',2a4,'.')
            nerr = nerr + 1
            go to 100
            endif
      call umemov (hdid,ffgid,2)
      call umemov (hdid,bbid,2)
      ndur = min(mxdurg,nhdur)
      go to 380
c
c  get gridded threshold runoffs for each duration
120   ibline = 1
      grotyp = 'grro'
      if (ibug.eq.1) write (iud,*) 'in cpgrid - usrnam=',usrnam
      call getgrd (grotyp,idurt,mxdurg,nrow,ncol,mxd,mxr,mxc,
     +             msrow,mwcol,gvers,usrnam,rdatim,aproc,vdatim,mxval,
     +             ihfld,tro,ibline,istat)
      if (ibug.eq.1) write (iud,*) 'in cpgrid - usrnam=',usrnam
c
      write (iutw,*)
      if (iupr.ne.iutw) write (iupr,*)
c
      mxval = -1
      nfilgt = 0
      irec = 0
c
c  get FFG paremeter record from OFS files - contains rainfall-runoff
c  curves
130   ffgtyp='FFG'
      call rpprec (ffgid,ffgtyp,irec,mpo,po,nfilpo,irecnx,istat)
      if (ibug.eq.1) then
ccc         iunitd = iud
         iunitd = iupr
         write (iunitd,140) ffgid,ffgtyp,irec,istat
         endif
140   format (' ffgid=',a,' ffgtyp=',a,' irec=',i5,' istat=',i2)
      if (istat.eq.6) then
         go to 710
         else if (istat.eq.3) then
            write (iutw,150) ffgtyp,nfilpo,mpo,ffgid
            if (iupr.ne.iutw) write (iupr,150) ffgtyp,nfilpo,mpo,ffgid
150   format (' ERROR: in cpgrid - number of words needed to read ',a,
     +   ' parameters (',i5,') exceeds maximum (',i5,') for ',
     +   'FFG identifier ',a,'.')
            nerr = nerr + 1
            go to 700
         else if (istat.eq.2) then
            if (ffgid.eq.' ') then
               write (iutw,160)
               if (iupr.ne.iutw) write (iupr,160)
160   format (' ERROR: no Flash Flood Guidance parameter records ',
     +   'found.')
                  nerr = nerr + 1
                  go to 700
               else
                  write (iutw,170) ffgid
                  if (iupr.ne.iutw) write (iupr,170) ffgid
170   format (' ERROR: Flash Flood Guidance Operation not defined ',
     +   'for area ',a,'.')
                  nerr = nerr + 1
                  go to 700
               endif
         else if (istat.ne.0) then
            write (iutw,180) ffgid,istat
            if (iupr.ne.iutw) write (iupr,180) ffgid,istat
180   format (' ERROR: problem encountered in routine RPPREC ',
     +   'reading FFG parameter record for area ',a,
     +       '. istat=',i3)
            nerr = nerr + 1
            go to 700
         endif
      if (ffgid.eq.'DORM5') then
         if (ibug.eq.1) write (iud,*) 'ffgid=',ffgid
         endif
c
c  duration flag:  0 = 1, 3, 6-hr durations only
c                  1 = plus 12-hr duration
c                  2 = plus 24-hr duration
      i = po(13) + 0.01
      nrrdur = i + 3
      ndur = nrrdur
c
c  reset number of durations based on extrema pmaxg
      if (ndur.gt.mxdurg) ndur = mxdurg
c
c  check for maximum number of durations to store at end of routine
      if (ndur.gt.ndstr) ndstr = ndur
c
c  check rainfall-runoff curves and get rainfall-runoff model type
      iprint = 0
      call chkrrc (ffgid,ndur,po,iud,irrbeg,nrrset,ilocrr,
     +   otyprr,onamrr,iprint)
c
c  get basin boundary identifier
      call umemov (po(9),bbid,2)
c
c  check for a rainfall-runoff curve
      if (po(18).lt.0.0) then
         write (iutw,230) ffgid
         if (iupr.ne.iutw) write (iupr,230) ffgid
230   format (' ERROR: Flash Flood Guidance Operation never run for ',
     +   'area ',a,'.')
        nerr = nerr + 1
        go to 700
        endif
c
c  check computation time
      call ckcpd (ffgid,po(17),lident,lffcpd,nwarn)
c
c  get high flow and runoff adjust parameters
      qtsidg(1) = 'none'
      qtsidg(2) = '    '
      dtcqg = 'none'
      sro = 0.0
      iqoptg = 0
      iroptg = 0
      pcimpg = 0.0
      do 240 i=1,ndur
         pcfq(i) = 0.0
240      continue
      bank = 0.0
      if (iqctlg.ne.1.and.irctlg.le.0) go to 340
      gpmtyp = 'gdpm'
      kod = 1
      call rppfil (ffgid,gpmtyp,kod,iunit,mbx,bx,nbx,istat)
      call upclos (iunit,bname,ic)
      if (istat.eq.0) then
         call getgpm (bx)
         if (ibug.gt.2) then
            write (iud,250) fvers,iffgid,gpmtyp,iqoptg,iroptg,bank
250   format (' fvers=',f5.2,' iffgid=',2a4,' gpmtyp=',a,
     +   ' iqoptg=',i2,' iroptg=',i2,' bank=',f5.2)
            if (iqoptg.gt.0) write (iud,260) qtsidg,dtcqg,taqg
260   format (' qtsidg=',2a4,' dtcqg=',a4,' taqg=',5f4.0)
            if (iroptg.eq.1.or.iroptg.eq.2.or.iroptg.eq.5)
     +        write (iud,270) rinten
270   format (' rinten=',5f5.2)
            endif
         else if (istat.ne.7) then
            write (iutw,280) ffgid
            if (iupr.ne.iutw) write (iupr,280) ffgid
280   format (' WARNING: no high flow and/or runoff adjust parameters ',
     +   'defined for area ',a,'. Defaults will be used.')
            nwarn = nwarn + 1
            iqoptg = 0
            iroptg = 0
            go to 340
         else
            go to 340
         endif
c
c  check if to skip non-grid ffg area
      if (iroptg.eq.9) then
         write (iupr,290) ffgid,bbid,iroptg
290   format (' NOTE: FFG identifier ',a,' (BASN identifier ',a,') ',
     +  'will not be processed because rainfall/FFG adjust option ',
     +  'is set to ',i1,'.' )
         go to 700
         endif
c
      if (iqctlg.ne.1.or.iqoptg.le.0) go to 350
c
c  get flow at flood stage from Rating Curve definition
      ipr = iupr
      istat = 90
      call umemov (ffgid,rcvid,2)
      call fgetrc (rcvid,istat)
      if (ibug.gt.2) write (iud,300) rcvid,istat
300   format (' rcvid=',a,' istat=',i2)
      if (istat.eq.0) then
         if (tid(1).ne.rtcvid(1).or.tid(2).ne.rtcvid(2)) then
            tid(1) = rtcvid(1)
            tid(2) = rtcvid(2)
            if (fldstg.ne.-999.0) fldstg = fldstg*conv
            if (floodq.ne.-999.0) floodq = floodq*conv**3
            endif
         if (ibug.gt.2) write (iud,310) fldstg,floodq
310   format (' fldstg=',f6.0,' floodq=',f6.0)
c     get high flow
         call gethiq (iqctlg,iqoptg,taqg,qtsidg,dtcqg,ndur,qb,istat)
         if (istat.eq.0) then
            do 320 i=1,ndur
               pcfq(i) = qb(i)/floodq
320            continue
            if (pcfq(i).gt.1.0) pcfq(i) = 1.0
            go to 350
            else
               iqoptg = 0
               go to 340
            endif
         else
            write (iutw,330) rcvid
            if (iupr.ne.iutw) write (iupr,330) rcvid
330   format (' ERROR: Rating Curve ',a,' not found.')
            nerr = nerr + 1
            istat = 4
         endif
      go to 350
c
c  adjust threshold runoff for storm total runoff instead of forecast
c  flow
340   if (iqctlg.eq.2) then
         call getsro (iqctlg,ilocrr,otyprr,po,sro,istat)
         endif
c
350   if (ibug.eq.1) then
         write (iud,360) iqctlg,iqoptg,ndur,sro
360   format (' iqctlg=',i2,' iqoptg=',i2,' ndur=',i2,' sro=',f5.2)
         else if (ibug.gt.1) then
            write (iud,370) iqctlg,iqoptg,taqg,qtsidg,dtcqg,ndur,qb,sro
370   format (' iqctlg=',i2,' iqoptg=',i2,' taqg=',5f4.0,' qtsidg=',2a4,
     +   ' dtcqg=',a4,' ndur=',i2,' qb=',5f6.0,' sro=',f5.2)
         endif
c
c  transfer state variables (carryovers) from ffg parameter array
c  to separate array for external access
      if (nvar.ge.0) call trcary (ffgid,mpo,mbx,po,bx)
c
c  get basin boundary
380   bbtyp = 'BASN'
      call getbb (bbid,bbtyp,mbx,bx,ibx,mlseg,nlseg,nlrow,
     +   nlbeg,nlend,istat)
      if (ibug.eq.1) then
ccc         iunitd = iud
         iunitd = iupr
         write (iunitd,385) bbid,bbtyp,istat
         endif
385   format (' bbid=',a,' bbtyp=',a,' istat=',i2)
      if (istat.ne.0) then
         write (iutw,387) bbtyp,bbid,ffgid
         if (iupr.ne.iutw) write (iupr,387) bbtyp,bbid,ffgid
387   format (' ERROR: ',a,' parameter record for BASN_id ',a,
     +   ' not found for FFG_id ',a,'.')
         nerr = nerr + 1
         go to 700
         endif
      if (bbid.eq.'RIZ008') then
         if (ibug.eq.1) write (iud,*) 'in cpgrid - bbid=',bbid
         endif
c
c  check area sizes
      if (ibsize.eq.1) call ckarea_file (ffgid,bbid,bx)
c
c  process each duration
      do 690 idr=1,ndur
         idur = idurt(idr)
         ncptd = 0
         noro = 0
         nfilg = 0
         puro = -9.
c     calculations for each line segment in area
         do 660 lseg=1,nlseg
            irow = nlrow(lseg)
            ncbeg = nlbeg(lseg)
            ncend = nlend(lseg)
            if (ibug.eq.2) write (iud,390) lseg,irow,ncbeg,ncend
390   format (' lseg=',i2,' irow=',i3,' ncbeg=',i3,' ncend=',i3)
c        convert national row to local row
            lrow = irow - msrow + 1
            if (lrow.lt.1) then
               write (iutw,400) lrow
400   format (' WARNING: lrow (',i5,') reset to 1.')
               nwarn = nwarn + 1
               lrow = 1
               endif
            if (lrow.gt.nrow) then
               write (iutw,410) lrow,nrow
410   format (' WARNING: lrow (',i5,') reset to ',i4,'.')
               nwarn = nwarn + 1
               lrow = nrow
               endif
c        calculations for each column in the line segment (row)
            idel = 1
            if (ncend.lt.ncbeg) idel = -1
            do 610 icol=ncbeg,ncend,idel
c           convert national column to local column
               lcol = icol - mwcol + 1
               if (lcol.lt.1) then
                  write (iutw,420) lcol
420   format (' WARNING: lcol (',i5,') reset to 1.')
                  nwarn = nwarn + 1
                  lcol = 1
                  endif
               if (lcol.gt.ncol) then
                  write (iutw,430) lcol,ncol
430   format (' WARNING: lcol (',i5,') reset to ',i4,'.')
                  nwarn = nwarn + 1
                  lcol = ncol
                  endif
               if (karea.eq.1) then
                  p = hffg(idr)
                  go to 540
                  endif
               if (ibug.eq.1) write (iud,*) 'in cpgrid -',
     +            ' idr=',idr,' lrow=',lrow,' lcol=',lcol,
     +            ' tro(idr,lrow,lcol)=',tro(idr,lrow,lcol),
     +            ' pcfq(idr)=',pcfq(idr)
               if (ibug2.eq.1) then
ccc                  if (lrow.eq.62.and.lcol.eq.151) then
ccc                  if (lrow.eq.121.and.lcol.eq.350) then
                  if (irow.eq.620.and.icol.eq.992) then
                     write (ibug2u,440) 'ffgid=',ffgid,
     +                  ' idr=',idr,
     +                  ' irow=',irow,' icol=',icol,
     +                  ' lrow=',lrow,' lcol=',lcol,
     +                  ' tro(idr,lrow,lcol)=',tro(idr,lrow,lcol)
440   format (' in cpgrid - ',a,a,
     +   a,i2,
     +   a,i4, a,i4,
     +   a,i3, a,i3,
     +   a,f6.3)
                     endif
                  endif
               troz=tro(idr,lrow,lcol)
               ichk=0
               if (ichk.eq.1.and.troz.eq.0.0) then
                  troz = -9.9
                  write (iutw,450) 'WARNING:',
     +               ffgid,idurt(idr),bbid,irow,icol,
     +               tro(idr,lrow,lcol),troz
                  if (iupr.ne.iutw) write (iupr,450) 'WARNING:',
     +               ffgid,idurt(idr),bbid,irow,icol,
     +               tro(idr,lrow,lcol),troz
450   format (' ',a,t11,'FFG_id=',a,t27,'dur=',i2,' BASN_id=',a,
     +        ' Gridded Threshold Runoff for row ',i4,' and column ',i4,
     +        ' is ',f3.1,' and will be set to ',f6.3,'.')
                  nwarn = nwarn + 1
                  endif
               uro = troz*(1.0 - pcfq(idr))
               if (ibug.eq.1) write (iud,*) 'in cpgrid -',
     +            ' idr=',idr,' uro=',uro,' puro=',puro
c           check and apply bank factor
               if (bank.lt.0.6.or.bank.gt.1.5) bank = gbank
c           check that uro is in reasonable range of values
               if (uro.ge.0.0.and.uro.le.20.0) then
                  if (uro.eq.puro) go to 550
                  puro = uro
                  if (ibug.eq.1) write (iud,*) 'in cpgrid -',
     +               ' iqctlg=',iqctlg
c              adjust runoff with storm runoff instead of forecast flow
                  if (iqctlg.eq.2) then
                     uro = tro(idr,lrow,lcol) - sro
                     if (uro.le.0.0) uro = 0.01
                     endif
                  iro = ifix((uro+0.005)*100.0)
                  ro = uro*bank
c              apply runoff adjustment to ro
                  if (irctlg.gt.0.and.irctlg.ne.2) then
                     if (iroptg.eq.1) then
                        if (rinten(idr).ge.0.05.and.
     +                      rinten(idr).lt.5.0) then
                           aro = ro
                           ro = aro*rinten(idr)
                           if (ibug.ge.2) write (iud,460) lcol,idr,
     +                                 tro(idr,lrow,lcol),sro,bank,
     +                                 uro,rinten(idr),aro,ro
460   format (' lcol=',i4,' idr=',i2,' tro=',f5.2,
     +        ' sro=',f5.2,' bank=',f4.2,' uro=',f5.2,
     +        ' ro factor=',f5.2,' aro=',f5.2,
     +        ' ro=',f5.2)
                           endif
                        else if (iroptg.eq.2) then
                           p = rinten(idr)
                           if (ibug.ge.2) write (iud,470) lcol,idr,
     +                                    tro(idr,lrow,lcol),sro,bank,
     +                                    uro,rinten(idr),p
470   format (' lcol=',i4,' idr=',i2,' tro=',f5.2,
     +        ' sro=',f5.2,' bank=',f4.2,' uro=',f5.2,
     +        ' factor is ffg=',f5.2,' p=',f5.2)
                           go to 550
                        else if (iroptg.eq.3) then
                           p = uro
                           if (ibug.ge.2) write (iud,480) lcol,idr,
     +                                    tro(idr,lrow,lcol),sro,bank,
     +                                    uro,p,rinten(idr), p
480   format (' lcol=',i4,' idr=',i2,' tro=',f5.2,
     +        ' sro=',f5.2,' bank=',f4.2,' uro=',f5.2,
     +        ' ro is ffg=',f5.2,' p=',f5.2)
                           go to 550
                        else
                           if (ibug.ge.3) write (iud,490) lcol,idr,
     +                        tro(idr,lrow,lcol),sro,bank, uro,ro
490   format (' lcol=',i4,' idr=',i2,' tro=',f5.2,' sro=',f5.2,
     +        ' bank=',f4.2,' uro=',f5.2,' ro=',f5.2)
                           if (ibug.ge.3) write (iud,500) lcol,idr,
     +                        rinten(idr),bank,uro,ro
500   format (' lcol=',i4,' idr=',i2,' rinten(idr)=',f5.2,
     +        ' bank=',f4.2,' uro=',f5.2,' ro=',f5.2)
                        endif
                  endif
c           percent impervious adjustment
               pim = ro*pcimpg
               rop = ro - pim
c           calculate ffg from rainfall-runoff curve
               call calcp (ffgid,idr,po,rop,pp,istat)
               p = pp + pim
c           average storm duration algorithm for API-CIN
c           rainfall-runoff model
               if (irctlg.ge.2.and.idr.le.2.and.
     +             (otyprr(4:8).eq.'CIN '.or.
     +              otyprr(4:8).eq.'cin ')) then
                  pg = p
                  roz = 0.0
                  call calcp (ffgid,idr,po,roz,rast,istat)
                  tloss = ro + rast
                  f = pg - tloss
                  pi = tloss + f*(idurt(idr)/9.0)**0.5
                  p = pi
                  if (ibug.ge.3) write (iud,510) idurt(idr),ro,rast,
     +               tloss,pg,f,p
510   format (' API-CIN intensity : idurt(idr)=',i2,
     +        ' ro=',f4.2,' rast=',f4.2,' tloss=',f4.2,
     +        ' pg=',f4.2,' f=',f4.2,' p=',f4.2)
                  endif
               else
c              cannot compute runoff
                  p = -9.0
                  iro = ifix((uro-0.005)*100.0)
                  noro = noro + 1
                  puro = -9.0
                  if (ibug.eq.1) write (iupr,520) bbid,lcol,lrow,
     +               icol,irow,idr,uro,iro
520   format (' no runoff defined for bbid ',a,
     +   ' at local col ',i4,' row ',i4,
     +   ' (HRAP col ',i4,' row',i4,')',
     +   ' idr=',i2,' uro=',f7.2,' iro=',i4)
                  go to 560
                  endif
c           apply factor to FFG value
               if (irctlg.ne.0.and.iroptg.eq.5) then
                  pg = p
                  p = pg*rinten(idr)
                  if (ibug.gt.2) write (iud,530) rinten(idr),pg,p
530   format (' factor',f5.2,' applied to p=',f5.2,' giving FFG=',f5.2)
                  endif
c           check max and min limits for guidance values
540            if (pmaxg(idr).lt.0.or.pmaxg(idr).gt.20.0) then
c              skip this duration
                  p = -9.1
                  go to 670
c              check if greater than max limit
                  else if (p.gt.pmaxg(idr)) then
                     p = pmaxg(idr)
c              check if less than min limit
                  else if (p.lt.pming(idr)) then
                     p = pming(idr)
                  endif
550            ncptd = ncptd + 1
               if (ibug.eq.1) write (iud,*) 'in cpgrid - idr=',idr,
     +            ' ncptd=',ncptd
560            ngrid = ngrid + 1
               gg(idr,lrow,lcol) = p
               if (p.gt.mxval) mxval = p
               if ((ixdim.lt.5).and.(mxd.gt.5.or.mxr.gt.500)) then
                  write (iutw,570) bbid,idr,lcol,lrow
570   format (' ERROR: bbid=',a,' idr=',i4,' lcol=',i4,' lrow=',i4)
                  nerr = nerr + 1
                  ixdim = ixdim + 1
                  endif
               if (iupr.ne.iutw) then
                  if (karea.eq.0) then
                     if (ibug.gt.2) write (iupr,580) ffgid,idr,
     +                  lcol,lrow,ro,p,ngrid,icol,irow
580   format (' computed ',a,i4,2i5,2f6.2,' at ngrid=',i6,' natl',2i5)
                        else
                     if (ibug.gt.2) write (iupr,590) ffgid,idr,
     +                     lcol,lrow,p,ngrid,icol,irow
590   format (' computed ',a,i4,2i5,f6.2,' at ngrid=',i6,' natl',2i5)
                     endif
                  endif
               if (ibug2.eq.1.and.
     +             lrow.ge.117.and.lrow.le.133.and.
     +             lcol.ge.339.and.lcol.le.350) then
                   write (ibug2u,600) ' ffgid=',ffgid,' bbid=',bbid,
     +                ' idur=',idur,' idr=',idr,
     +                ' lrow=',lrow,' lcol=',lcol,
     +                ' gg(idr,lrow,lcol)=',gg(idr,lrow,lcol)
600   format (' in cpgrid - ',a,a, a,a,
     +   a,i2, a,i2,
     +   a,i3, a,i3,
     +   a,f6.3)
                  endif
610            continue
c        check grid fill control option
            if (ngfil.ge.1.and.ngfil.le.5) then
c           fill missing grids in columns left and right of line segment
               ib = ncbeg - mwcol + 1
               ie = ncend - mwcol + 1
               do 630 k=1,2
                  if (k.eq.1) then
                     jb = ib - ngfil
                     je = ib - 1
                     lc = ib
                     jrb = jb
                     else
                        je = ie + ngfil
                        jb = ie + 1
                        lc = ie
                        jre = je
                     endif
                  do 620 lcol=jb,je
                     if (lcol.lt.1.or.lcol.gt.ncol) go to 620
                     if (gg(idr,lrow,lcol).lt.0.0.and.
     +                   gg(idr,lrow,lc).gt.0.0) then
                        gg(idr,lrow,lcol) = gg(idr,lrow,lc)
                        nfilg = nfilg + 1
                        if (ibug2.eq.1) then
                           irowg = msrow + lrow - 1
                           icolg = mwcol + lcol - 1
ccc                           if (irowg.eq.620.and.icolg.eq.992) then
                           if (lrow.eq.26.and.lcol.eq.118) then
                              write (ibug2u,440) 'ffgid=',ffgid,
     +                           ' idr=',idr,
     +                           ' irowg=',irowg,' icolg=',icolg,
     +                           ' lrow=',lrow,' lcol=',lcol,
     +                           ' gg(idr,lrow,lcol)=',gg(idr,lrow,lcol)
                              endif
                           endif
                        endif
620                  continue
630               continue
c           fill rows above and below current row lrow
               do 650 i=1,ngfil
                  if (lseg.eq.1) then
                     lrowi = lrow + i
                     if (lrowi.gt.nrow) go to 660
                     else if (lseg.eq.nlseg) then
                        lrowi = lrow - i
                        if (lrowi.lt.1) go to 660
                     else
                        go to 660
                     endif
                  do 640 lcol=jrb,jre
                     if (lcol.lt.1.or.lcol.gt.ncol)  go to 640
                     if (gg(idr,lrowi,lcol).lt.0.0.and.
     +                   gg(idr,lrow,lcol).gt.0.0) then
                        gg(idr,lrowi,lcol) = gg(idr,lrow,lcol)
                        nfilg = nfilg + 1
                        if (ibug2.eq.1) then
                           irowg = msrow + lrowi - 1
                           icolg = mwcol + lcol - 1
ccc                           if (irowg.eq.620.and.icolg.eq.992) then
                           if (lrowi.eq.26.and.lcol.eq.118) then
                              write (ibug2u,440) 'ffgid=',ffgid,
     +                           ' idr=',idr,
     +                           ' irowg=',irowg,' icolg=',icolg,
     +                           ' lrowi=',lrowi,' lcol=',lcol,
     +                          ' gg(idr,lrowi,lcol)=',gg(idr,irow,lcol)
                              endif
                           endif
                        endif
640                  continue
650               continue
               endif
660         continue
670      nfillg(idr) = nfillg(idr) + nfilg
         nfilgt = nfilgt + nfilg
         if (ibug.eq.1) then
            write (iud,*) 'in cpgrid - idr=',idr,
     +         ' ncptd=',ncptd,' noro=',noro,
     +         ' ngfil=',ngfil,' nfilg=',nfilg,' nfilgt=',nfilgt
            if (iupr.ne.iutw) write (iupr,*) 'in cpgrid - idr=',idr,
     +         ' ncptd=',ncptd,' noro=',noro,
     +         ' ngfil=',ngfil,' nfilg=',nfilg,' nfilgt=',nfilgt
            endif
         mtot = ncptd + noro
         if (iqctlg.eq.2) then
            if (ncptd.eq.0) then
               write (iutw,680) 'ERROR:',ffgid,idurt(idr),
     +            bbid,ncptd,noro,mtot,
     +            sro
               if (iupr.ne.iutw)
     +            write (iupr,680) 'ERROR:',ffgid,idurt(idr),
     +               bbid,ncptd,noro,mtot,
     +               sro
680   format (' ',a,t11,'FFG_id=',a,t27,'dur=',i2,' BASN_id=',a,
     +        '  grids: cptd=',i4,' noro=',i4,' total=',i4 :
     +        '  (sro=',f5.2,')')
               nerr = nerr + 1
               else if (noro.ne.0) then
                  iprint=1
                  if (iprint.eq.1) then
                     write (iutw,680) 'WARNING:',ffgid,idurt(idr),
     +                  bbid,ncptd,noro,mtot,
     +                  sro
                     if (iupr.ne.iutw)
     +                  write (iupr,680) 'WARNING:',ffgid,idurt(idr),
     +                     bbid,ncptd,noro,mtot,
     +                     sro
                     nwarn = nwarn + 1
                     endif
               else
                  write (iupr,680) ' ',ffgid,idurt(idr),
     +               bbid,ncptd,noro,mtot,
     +               sro
               endif
            else
               if (ncptd.eq.0) then
                  write (iutw,680) 'ERROR:',ffgid,idurt(idr),
     +               bbid,ncptd,noro,mtot
                  if (iupr.ne.iutw)
     +               write (iupr,680) 'ERROR:',ffgid,idurt(idr),
     +                  bbid,ncptd,noro,mtot
                  nerr = nerr + 1
                  else if (noro.ne.0) then
                     iprint=1
                     if (iprint.eq.1) then
                        write (iutw,680) 'WARNING:',ffgid,idurt(idr),
     +                     bbid,ncptd,noro,mtot
                        if (iupr.ne.iutw)
     +                     write (iupr,680) 'WARNING:',ffgid,idurt(idr),
     +                        bbid,ncptd,noro,mtot
                        nwarn = nwarn + 1
                        endif
                  else
                     write (iupr,680) ' ',ffgid,idurt(idr),
     +                  bbid,ncptd,noro,mtot
                  endif
            endif
690      continue
c
700   if (singid.ne.'none') then
         if (singid.ne.'ibug2') go to 710
         endif
c
      if (karea.eq.1) go to 100
c
c  check if last area procesed
      if (irecnx.ne.0) then
         irec = irecnx
         ffgid = ' '
         go to 130
         endif
c
710   if (ngrid.gt.0) then
         write (iutw,720)
         if (iupr.ne.iutw) write (iupr,720)
720   format (/ t5,'Grid computation values:' /
     +         t10,'dur   = duration' /
     +         t10,'cptd  = grids computed' /
     +         t10,'noro  = grids with no runoff' /
     +         t10,'total = total grids'
     +        )
         endif
c
      if (iqctlg.eq.2) then
         write (iutw,730)
         if (iupr.ne.iutw) write (iupr,730)
730   format ( t10,'sro   = storm runoff')
         endif
      if (ibug.eq.1) write (iud,*) 'in cpgrid - ngfil=',ngfil,
     +   ' nfilgt=',nfilgt
c
c  check for decreasing FFG values
      if (ickval.eq.1) then
         write (iutw,735)
         if (iupr.ne.iutw) write (iupr,735)
735   format (/ ' NOTE: checking for decreasing FFG values.')
         do 755 irow=1,nrow
            do 750 icol=1,ncol
               do 740 idr=2,ndur
                  a = gg(idr-1,irow,icol)
                  b = gg(idr,irow,icol)
                  if (a.le.0.0.or.b.le.0.0) go to 750
                  if (b.lt.a) gg(idr,irow,icol) = gg(idr-1,irow,icol)
740               continue
750            continue
755         continue
         endif
c
      if (ngfil.eq.6) then
c     fill missing grids based on at least 5 of surrounding 8 grids
         write (iutw,756) ngfil
            if (iupr.ne.iutw) write (iupr,756) ngfil
756   format (' NOTE: filling in missing grids for Grid Fill Control ',
     +   'option value of ',i1,'.')
         mecol = mwcol + ncol - 1
         mnrow = msrow + nrow - 1
         do 850 idr=1,ndur
            do 840 irow=1,nrow
               do 830 icol=1,ncol
                  if (gg(idr,irow,icol).lt.0.0) then
                     do 760 m=1,4
                        ff(m) = -1.
                        numg(m) = 0
760                     continue
                     ntot = 0
                     do 790 jj=1,3
                        j = irow + jj - 2
                        if (j.lt.msrow.or.j.gt.mnrow) go to 790
                        do 780 ii=1,3
                           i = icol + ii - 2
                           if (i.lt.mwcol.or.i.gt.mecol) go to 780
                           p = gg(idr,j,i)
                           if (p.ge.0.0) then
                              do 770 m=1,4
                                 if (ff(m).lt.0.0.or.p.eq.ff(m)) then
                                    ff(m) = p
                                    numg(m) = numg(m) + 1
                                    ntot = ntot + 1
                                    go to 780
                                    endif
770                              continue
                              endif
780                        continue
790                     continue
                     if (ibug.eq.1.and.ntot.gt.0) then
                        write (iupr,800) ntot,numg,ff
800   format (' in cpgrid - ntot=',i2,' numg=',4i3,' ff=',4(f5.2,1x))
                        endif
                     if (ntot.ge.5) then
                        mxnum = 0
                        do 810 m=1,4
                           if (numg(m).gt.mxnum) then
                              mxnum = numg(m)
                              pff = ff(m)
                              endif
810                        continue
                        if (mxnum.gt.0) then
                           gg(idr,irow,icol) = pff
                           nfilgt = nfilgt + 1
                           write (iupr,820) icol,irow,pff,mxnum
820   format (' NOTE: filled local col ',i4,' and row ',i4,
     +  ' with ',f5.1,' from ',i1,' surrounding grids')
                           endif
                        endif
                     endif
830               continue
840            continue
            nfillg(idr) = nfillg(idr) + nfilg
850         continue
         if (ibug.eq.1) write (iud,*) 'in cpgrid - ngfil=',ngfil,
     +      ' nfilgt=',nfilgt
         endif
c
      if (nfilgt.gt.0) then
         write (iutw,*)
         if (iupr.ne.iutw) write (iupr,*)
         do 865 idr=1,ndur
            idur = idurt(idr)
            write (iutw,855) nfillg(idr),idur
            if (iupr.ne.iutw) write (iupr,855) nfillg(idr),idur
855   format (' NOTE: ',i5,' missing grid positions filled for ',
     +   'duration ',i2,'.')
865         continue
         endif
c
c  check computation date
      if (lffcpd.eq.-1) go to 880
c
c  convert computation date
      jda = lffcpd/24 + 1
      jhr = mod(lffcpd,24)
      iall = 0
      if (ibug.ge.3) iall = 1
      call mdyh1 (jda,jhr,mimo,mida,miyr,mihr,0,0,tzcode)
c  convert mihr=24 to mihr=0 and increment day
      if (mimo.eq.2.and.((miyr/4)*4.eq.miyr)) ndays(2) = 29
      inchr = 0
      mimn = 0
      call datimi (inchr,mimo,mida,miyr,mihr,mimn, mmo,mda,myr,
     +             mhr,mmn,ndays)
      call infxdt (mmo,mda,myr,mhr,-1,24,vdatim)
c
c  write gridded guidance to files
      write (iutw,*)
      if (iupr.ne.iutw) write (iupr,*)
      do 870 idr=1,ndstr
         gfftyp = 'grff'
         call strgrd (gfftyp,idr,idurt,nrow,ncol,mxd,mxr,mxc,
     +                msrow,mwcol,vers,usrnam,sdatim,vdatim,mxval,
     +                ihfld,gg,istat)
         if (ibug2.eq.1) write (ibug2u,*) 'in cpgrid - idr=',idr,
     +      ' gg(idr,26,118)=',gg(idr,26,118)
870      continue
c
c  set flag so gridded guidance is not re-read from database
      if (istat.eq.0) kgridf = 1
c
880   negrid = nerr - negrid
      nwgrid = nwarn - nwgrid
c
      return
c
      end
