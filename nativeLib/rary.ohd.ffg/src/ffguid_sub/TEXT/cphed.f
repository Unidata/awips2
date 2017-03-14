c  =====================================================================
c  pgm:  cphed
c
c  variables are passed in common blocks
c  =====================================================================
c
      subroutine cphed
c
c.......................................................................
c
c  compute headwater flash flood guidance values for a location
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL                                 March 1992
c
c  Moved high flow algorithm into separate routine.
c         Tim Sweeney, HRL                                 Nov 1997
c
c  Added option to reduce threshold runoff by amount equal to
c  storm runoff (API event models)
c       Tim Sweeney, HRL                                   Feb 1999
c
c  Added option to check for decreasing FFG.  If found, set
c  longer duration FFG to next shorter duration value.
c       Tim Sweeney, HRL                                   Dec 1999
c
c  Added capability to run a single identifier for testing.
c       Tim Sweeney, HL                                    May 2001
c.......................................................................
c
      character*2 bname
      character*4 blnk,cnone,ccnone
      character*4 ffgtyp
      character*8 hdidz,ffgid,lident,onamrr,otyprr
      character*50 strng
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/hwparm'
      include 'ffg_inc/count'
      include 'ffg_inc/gidx'
      include 'ffg_inc/gpo'
      include 'ffg_inc/gfx'
      include 'common/fratng'
      include 'common/fctime'
c
      dimension qb(5),small(5),sum(5),tid(2),wffg(5)
      dimension tro(5),ratcid(2),val(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/cphed.f,v $
     . $',                                                             '
     .$Id: cphed.f,v 1.9 2005/07/07 19:48:51 xfan Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('cphed',1,1,ibug)
ccc      ibug=1   
c
      blnk = ' '
      bname = ' '
      cnone = 'none'
      ccnone = 'NONE'
      hdwtyp = 'hffg'
c
      strng='headwater guidance'
c
      nehwat = nerr
      nwhwat = nwarn
      conv   = 3.281
c
      iunitd = iud
c
c  get user info
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.ne.0) then
         write (iutw,10) strng(1:lenstr(strng))
         if (iupr.ne.iutw) write (iupr,10) strng(1:lenstr(strng))
10    format (' WARNING: computations for ',a,' will not be done ',
     +   'because user controls are not defined.')
         nwarn = nwarn + 1
         go to 640
         endif
c
c  check if headwater guidance computations desired
      if (mxdurh.lt.0) then
         write (iutw,20) strng(1:lenstr(strng)),mxdurh
         if (iupr.ne.iutw) write (iupr,20) strng(1:lenstr(strng)),mxdurh
20    format (' WARNING: computations for ',a,' will not be done ',
     +   'because the value of the user control option is ',i2,' .')
         nwarn = nwarn + 1
         go to 640
         endif
c
      local = loclo
      nlstz = nlstzo
c
c  get index (file) to hffg locations
      call getidx (hdwtyp,idxdv,mcidx,po,mh,cidx,istat)
      call upclos (idxdv,bname,ic)
      if (ibug.gt.0) write (iunitd,*) 'mh=',mh
c
c  check if to compute for a single headwater
      if (singid.ne.'none') then
         call umemov (singid,cidx(1,1),2)
         mh = 1
         endif
c
      write (iutw,30) strng(1:lenstr(strng)),mh
      if (iupr.ne.iutw) write (iupr,30) strng(1:lenstr(strng)),mh
30    format (/ ' Computing ',a,' for ',i4,' locations.')
c
      lffcpd = -1   
c
c  process each location
      do 630 lh=1,mh
         if (cidx(1,lh).ne.cnone) go to 40
         if (cidx(2,lh).eq.cnone) go to 630
40       hdid(1) = cidx(1,lh)
         hdid(2) = cidx(2,lh)
         call umemov (cidx(1,lh),hdidz,2)
         if (hdidz.eq.'THNI2'.or.hdidz.eq.'LEVI2') then
            iset = 0
            if (iset.eq.1) then
               ibug = 1
ccc               ibug = 2
               iunitd = iud
               if (ibug.gt.1) iunitd = iupr
               write (iutw,50) ibug,hdidz,iunitd
               if (iupr.ne.iutw) write (iupr,50) ibug,hdidz,iunitd
50    format (' WARNING: in cphed - variable ibug set to ',i1,
     +   ' for identifier ',a,'.',
     +   ' Debug output will be written to unit ',i2,'.')
               nwarn = nwarn + 1
               endif
            else
               ibug = 0
            endif
         if (ibug.gt.0) write (iunitd,60) hdid
60    format (' hdid=',2a4)
c     read hffg parameters
         kod = 0
         call rppfil (hdidz,hdwtyp,kod,ipdv,mpo,po,npo,istat)
         if (istat.eq.0) then
c        fill hffg parameters common block hwparm
            call gethed (po)
            else
               write (iutw,70) hdidz
70    format (' ERROR: headwater parameters not found for ',a,'.')
               nerr = nerr + 1
               go to 630
            endif
c     unitgraph peak flows or threshold runoffs
         do 80 i=1,5
            tro(i) = upk(i)
80          continue
c     reset number of durations based on extrema
         ndur = nhdur
         if (ndur.gt.mxdurh) ndur = mxdurh
         nduri = ndur
         nxdurh=0
         do 85 i=1,ndur
            if (pmaxh(i).lt.0.0) nxdurh=nxdurh+1
85          continue
         if (ibug.eq.1) write (iud,*) 'in cphed - nhdur=',nhdur,
     +      ' mxdurh=',mxdurh,' ndur=',ndur,' nxdurh=',nxdurh
         if (rcid(1).ne.blnk) then
            if (rcid(1).ne.cnone.and.rcid(1).ne.ccnone) then
c           get flow at flood stage from rating curve definition
               istat = 90
               call umemov (rcid,ratcid,2)
               call fgetrc (ratcid,istat)
               if (ibug.gt.0) write (iunitd,'(a,2a4,a,i2)')
     +            ' fgetrc called for ratcid=',ratcid,' istat=',istat
               if (istat.eq.0) then
                  if (tid(1).ne.rtcvid(1).or.tid(2).ne.rtcvid(2)) then
                     tid(1) = rtcvid(1)
                     tid(2) = rtcvid(2)
                     if (fldstg.ne.-999.0) fldstg = fldstg*conv
                     if (floodq.ne.-999.0) then
                        floodq = floodq*conv**3
                        else
                           write (iutw,90) ratcid
                           if (iupr.ne.iutw) write (iupr,90) ratcid
90    format (' ERROR: flood Q not defined for Rating Curve ',2a4,'.')
                           nerr = nerr + 1
                           istat = 4
                           do 100 i=1,ndur
                              hffg(i) = -9.8
100                           continue
                           go to 540
                           endif
                        endif
                     if (ibug.gt.0) write (iunitd,110) fldstg,floodq
110   format (' fldstg=',f7.1,' floodq=',f7.1)
                     go to 140
                  else
                     write (iutw,120) ratcid
120   format (' ERROR: Rating Curve ',2a4,' not found.')
                     nerr = nerr + 1
                     istat = 4
                     do 130 i=1,ndur
                        hffg(i) = -9.8
130                     continue
                     go to 540
                  endif
               endif
            endif
         floodq = fsflow
c     high (base) flow adjustment
140      call gethiq (iqctlh,iqopth,taq,qtsid,dtcq,ndur,qb,istat)
         if (ibug.eq.1) write (iutw,*) 'in cphed after calcp -',
     +      ' istat=',istat
c     Compute threshold runoffs from unitgraph peaks.
c     Values in tro are (1) unitgraph peaks when floodq is greater
c     than 10 or (2) threshold runoffs when floodq is less than 10.
c     qb() is base flow for each duration.
         if (floodq.gt.10.0) then

            do 160 i=1,ndur
                if (tro(i).lt.0.01) go to 160
                q = floodq - qb(i)
                if (q.lt.0.0) q = 0.0
                uro = tro(i)
                tro(i) = q / tro(i)
                if (ibug.gt.0) write (iunitd,150) i,qb(i),q,uro,tro(i)
150   format (' i=',i2,' qb(i)=',f6.0,' q=',f8.0,' uro=',f7.0,
     +   ' tro(i)=',f5.2)
160            continue
            else
c           tro values are runoff * 100
               do 200 i=1,ndur
                  if (tro(i).lt.0.01) go to 200
                  tro(i) = tro(i)/100.
                  if (ibug.gt.0) write (iunitd,170) i,tro(i)
170   format (' i=',i2,' tro(i)=',f5.2)
                  if (tro(i).gt.10.0) then
                     do 180 j=1,ndur
180                     hffg(j) = 26.5
                     write (iutw,190) hdidz
190   format (' ERROR: no flood flow and runoff greater than 10 ',
     +   'inches found for headwater area ',a,'.')
                     nerr = nerr + 1
                     go to 540
                     endif
200               continue
            endif
c     initialize local arrays and weight
         wts = 0.0
         do 210 i=1,ndur
            hffg(i) = 0.0
            wffg(i) = 0.0
            sum(i) = 0.0
            small(i) = 300.0
210         continue
         noar = 0
         if (ibug.gt.0) then
            write (iunitd,*) 'nars=',nars
            write (iunitd,220) (wt(i),(arid(j,i),j=1,2),i=1,nars)
220   format (' wt=',f4.2,' arid=',2a4 :/)
            endif
c     process weighted areas
         do 420 ia=1,nars
            if (arid(1,ia).eq.blnk) go to 420
            call umemov (arid(1,ia),ffgid,2)
c        get FFG parameter record
            ffgtyp = 'FFG '
            irec = 0
            call rpprec (ffgid,ffgtyp,irec,mfx,fx,nfilfx,irecnx,istat)
            if (ibug.gt.0) write (iunitd,'(a,a,a,i2)')
     +         ' rpprec called for ffgid=',ffgid,' istat=',istat
            if (istat.eq.2) then
               write (iutw,225) ffgtyp,ffgid
               if (iupr.ne.iutw) write (iupr,225) ffgtyp,ffgid
225   format (' ERROR: ',a,' parameter record not found for ',
     +   'FFG identifier ',a,'.')
               nerr = nerr + 1
               go to 420
               endif
            if (istat.eq.3) then
               write (iutw,230) ffgtyp,nfilfx,mfx,
     +            ffgid
               if (iupr.ne.iutw) write (iupr,230) ffgtyp,nfilfx,mfx,
     +            ffgid
230   format (' ERROR: in cphed - number of words needed to read ',a,
     +   ' parameters (',i5,') exceeds maximum (',i5,') for ',
     +   'FFG identifier ',a,'.')
               nerr = nerr + 1
               go to 420
               endif
            if (istat.eq.6) go to 420
            if (istat.ne.0) then
               write (iutw,240) ffgtyp,ffgid,istat
               if (iupr.ne.iutw) write (iupr,240) ffgtyp,ffgid,istat
240   format (' ERROR: getting  ',a,' parameter record found for ',
     +   'FFG identifier ',a,'. rpprec istat=',i2)
               nerr = nerr + 1
               go to 420
               endif
            noar = noar + 1
            wts = wts + wt(ia)
            if (iofs.ne.0) call upclos (iopdv,bname,ic)
c        set duration flag ndur not to exceed the number of 
c        rainfall-runoff curves computed in OFS
            n = fx(13) + 0.01
            nrrdur = n + 3
            if (nrrdur.lt.nduri-nxdurh) then
               ndur = nrrdur
               write (iutw,260) ffgid,nrrdur,nduri
               if (iupr.ne.iutw) write (iupr,260) ffgid,nrrdur,nduri
260   format (' ERROR: number of rainfall-runoff curves ',
     +   'for FFG identifier ',a,' (',i1,') is less than number ',
     +   'of durations to be computed (',i1,').')
               nerr = nerr + 1
               endif

            if (idurt(nrrdur).ne.idurt(nduri)) then
               ndur = nrrdur
               write (iutw,265) ffgid,idurt(nrrdur),
     +            idurt(nduri)
               if (iupr.ne.iutw) write (iupr,265) ffgid,idurt(nrrdur),
     +            idurt(nduri)
265   format (' ERROR: duration of last rainfall-runoff curve ',
     +   'for FFG identifier ',a,' (',i2,') is not the same as ',
     +   'last duration to be computed (',i2,').')
               nerr = nerr + 1
               go to 430
               endif
            if (ibug.eq.1) write (iutw,*) 'in cphed -',
     +         ' hdid=',hdid,' nars=',nars,' ndur=',ndur
            if (nars.eq.1) then
c           check rainfall-runoff curves which start in position 18
               numv = 8
               inorunt = 0
               do 247 idur=1,ndur
                  inorun = 0
                  ibeg = 17 + numv*(idur-1)
                  do 245 j=1,numv
                     ipos=ibeg+j
                     if (ibug.eq.1) write (iutw,*) 'in cphed -',
     *                  ' ipos=',ipos,'fx(ipos)=',fx(ipos)
                     if (fx(ipos).lt.0.0) then
                        if (inorun.eq.0) then
                          write (iutw,243) idurt(idur),ffgid
                          if (iupr.ne.iutw) write (iupr,243)
     +                       idurt(idur),ffgid
243   format (' ERROR: Flash Flood Guidance Operation never run for ',
     +   i2,' hour duration for FFG identifier ',a,'.')
                           nerr = nerr + 1
                           inorun = 1
                           inorunt = 1
                           istat = 7
                           endif
                        endif
245                  continue
                  if (ibug.eq.1) write (iutw,*) 'in cphed -',
     +               ' hdid=',hdid,' nars=',nars,' ia=',ia,
     +               ' ffgid=',ffgid,' noar=',noar
247               continue
               if (inorunt.eq.1) then
                  noar = noar - 1
                  go to 430
                  endif
               endif
c        check time (LSTCMPDY)
            call ckcpd (ffgid,fx(17),lident,lffcpd,nwarn)
c        check rainfall-runoff curves
            iprint = 0
            call chkrrc (ffgid,nrrdur,fx,iunitd,irrbeg,nrrset,
     +         ilocrr,otyprr,onamrr,iprint)
c        process each duration to compute ffg - variables in loop:
c          hffg   - flash flood guidance
c          fx     - rainfall runoff curves for all durations
c          p      - total precip that produces total runoff
c          pcimpv - percent impervious area (default 0.0)
c          pim    - precip over impervious area (same as runoff from
c                   impervious area)
c          pp     - precip over pervious area
c          hinten - intensity adjustment factor (default 1.0)
c          rop    - runoff from pervious area
c          rotot  - total runoff from both pervious and impervious
c                   areas
c          tro    - threshold runoff
c          wt     - weight
            do 410 idr=1,ndur

            if (pmaxh(idr) .lt. 0.0) go to 410   !cfan 05/2005  HSD r26-23

               pp = 0.0
c           adjust runoff
               if (irctlh.le.0.or.irctlh.eq.2) go to 350
               if (iropth.eq.1) then
                  if (hinten(idr).ge.0.01.and.hinten(idr).lt.5.0) then
                     rotot = tro(idr)*hinten(idr)
                     go to 360
                     endif
                  else if (iropth.eq.2) then
                     p = hinten(idr)
                     go to 380
                  else if (iropth.eq.3) then
                     p = tro(idr)
                     go to 380
                  endif
350            rotot = tro(idr)
               if (iqctlh.gt.0.and.iqopth.eq.4) then
                  call getsro (iqctlh,ilocrr,otyprr,po,sro,istat)
                  rotot = tro(idr) - sro
                  if (rotot.le.0.0) rotot = 0.01
                  endif
c           percent impervious area adjustment to total runoff (rotot)
360            pim = rotot*pcimpv
               rop = rotot*(1-pcimpv)
c           calculate ffg from rainfall-runoff curve for specific
c           duration
               call calcp (ffgid,idr,fx,rop,pp,istat)
               if (ibug.eq.1) write (iunitd,365) idr,rop,pp
365   format (' in cphed after calcp - idr=',i2,' rop=',f6.2,
     +   ' pp=',f6.3)
               p = pp + pim
               if (irctlh.ge.2.and.idr.le.2.and.
     +             (otyprr(4:8).eq.'CIN '.or.
     +              otyprr(4:8).eq.'cin ')) then
c              average storm duration algorithm for API-CIN
c              rainfall-runoff model
                  pg = pp
                  roz = 0.0
                  call calcp (ffgid,idr,fx,roz,rast,istat)
                  tloss = rop + rast
                  f = pg - tloss
                  pi = tloss + f*(idurt(idr)/9.0)**0.5
                  p = pi + pim
                  if (ibug.gt.0) write (iunitd,370) idurt(idr),rop,rast,
     +               tloss,f,pg,pi,p
370   format (' API-CIN intensity: idr=',i2,' rop=',f4.2,
     +   ' rast=',f4.2,' tloss=',f4.2,' f=',f4.2,
     +   ' pg=',f4.2,' pi=',f4.2,' p=',f4.2)
                  endif
380            wffg(idr) = wffg(idr) + p*wt(ia)
               sum(idr) = sum(idr) + p
               if (p.lt.small(idr)) small(idr) = p
               if (ibug.eq.1) write (iutw,*) 'in cphed - pp=',pp
               ppchk=20.0
               if (pp.gt.ppchk) then
                  write (iutw,390) ppchk,hdidz,
     +               ffgid,idurt(idr),pp,tro(idr)
                  if (iupr.ne.iutw) write (iupr,390) ppchk,hdidz,
     +               ffgid,idurt(idr),pp,tro(idr)
390   format (' WARNING: precip exceeds ',f4.1,' and low runoff :',
     +   ' Head_id=',a,' FFG_ID=',a,' dur=',i2,' pp=',f7.2,' tro=',f6.3)
                  iprint = 1
                  call chkrrc (hdidz,nrrdur,fx,iunitd,irrbeg,nrrset,
     +               ilocrr,otyprr,onamrr,iprint)
                  nwarn = nwarn + 1
                  endif
               if (ibug.gt.0) then
                  write (iunitd,400) idr,tro(idr),
     +               hinten(idr),rotot,pcimpv,pim
400   format (' in cphed - idr=',i2,' tro(idr)=',f6.3,
     +   ' hinten(idr)=',f5.2,' rotot=',f5.2,' pcimpv=',f4.2,
     +   ' pim=',f5.2)
                  write (iunitd,405) p,wt(ia),wts,
     +               small(idr),sum(idr),wffg(idr)
405   format (' in cphed - p=',f5.2,' wt(ia)=',f4.2,' wts=',f4.2,
     +   ' small(idr)=',f5.2,' sum(idr)=',f5.2,' wffg(idr)=',f5.2)
                  endif
410            continue
420         continue
c     check that rainfall runoff curves actually found
430      if (noar.lt.1) then
            do 440 i=1,ndur
               hffg(i) = -9.8
440            continue
c        decrement counter
            nhead = nhead - 1
            go to 540
            endif
c     sum of weights (wts) determines weighting technique used:
c        0.98 <= wts <= 1.02   use weighted p
c        wts < 0.0             use smallest p
c        wts = 0.0  and other  use average  p
         if (ibug.gt.0) then
            write (iunitd,'(1x,a,f6.3)') 'in cphed - wts=',wts
            endif
         if (wts.ge.0.98.and.wts.le.1.02) then
            if (ibug.gt.0) write (iunitd,'(1x,a,5(f6.3,1x))')
     +         'in cphed - wffg=',(wffg(i),i=1,ndur)
            do 450 i=1,ndur
                 hffg(i) = wffg(i)
450            continue
            else if (wts.lt.0.0) then
c           use lowest (minimum) value
               if (ibug.gt.0) write (iunitd,'(1x,a,5(f6.3,1x))')
     +            'in cphed - small=',(small(i),i=1,ndur)
               do 460 i=1,ndur
                  hffg(i) = small(i)
460               continue
            else
c           use average value
               if (ibug.gt.0) write (iunitd,'(1x,a,5(f6.3,1x))')
     +            'in cphed - sum=',(sum(i),i=1,ndur)
               do 470 i=1,ndur
                  hffg(i) = sum(i)/noar
470               continue
            endif
         if (ibug.gt.0) write (iunitd,'(1x,a,a,5(f6.3,1x))') 
     +      'in cphed after weighting -',
     +      ' hffg=',(hffg(i),i=1,ndur)
c     compute percent of 3 hr ffg if needed

         do 490 i=1,ndur
            if (tro(i).lt.0.0) then
               hffg(i) = -0.01*tro(i)*hffg(2)
               endif
490         continue
         if (ibug.gt.0) write (iunitd,'(1x,a,a,5(f6.3,1x))') 
     +      'in cphed after percent 3 hr ffg -',
     +      ' hffg=',(hffg(i),i=1,ndur)
c     apply max and min limits to guidance
         do 500 i=1,ndur
            if (pmaxh(i).lt.0.0) then
               go to 540
               else if (hffg(i).gt.pmaxh(i)) then
                  hffg(i) = pmaxh(i)
               endif
            if (hffg(i).lt.pminh(i)) hffg(i) = pminh(i)
500         continue
         if (ibug.gt.0) write (iunitd,'(1x,a,a,5(f6.3,1x))') 
     +      'in cphed after max/min limits applied -',
     +      ' hffg=',(hffg(i),i=1,ndur)
c     check for decreasing  FFG values
         if (ickval.gt.0) then
            do 510 idr=2,ndur
               if (hffg(idr).le.0.0.or.hffg(idr-1).le.0.0) go to 540
               if (hffg(idr).lt.hffg(idr-1)) hffg(idr) = hffg(idr-1)
510            continue
            if (ibug.gt.0) write (iunitd,'(1x,a,a,5(f6.3,1x))') 
     +         'in cphed after decreasing values check -',
     +         ' hffg=',(hffg(i),i=1,ndur)
            endif
540      if (ndur.gt.1) then
            iprint = 0
            do 530 idr=1,ndur-1
               if (pmaxh(idr).lt.0.0) go to 530
               if (pmaxh(idr+1).lt.0.0) go to 530
               val1=hffg(idr)
               val2=hffg(idr+1)
               if (val1.gt.val2) then
                  write (iutw,520) 'FFG',
     +               idurt(idr+1),val2,idurt(idr),val1,hdidz
                  if (iupr.ne.iutw) write (iupr,520) 'FFG',
     +               idurt(idr+1),val2,idurt(idr),val1,hdidz
520   format (' WARNING: ',a,' value for ',
     +   i2,' hour duration (',f6.3,
     +   ') is less than for ',
     +   i2,' hour duration (',f6.3,
     +   ') for Headwater ',a,'.')
                  nwarn = nwarn + 1
                  iprint = 1
                  endif
530            continue
            if (iprint.eq.1) then
c           print rainfall-runoff curves
               call chkrrc (hdidz,nrrdur,fx,iunitd,irrbeg,nrrset,
     +            ilocrr,otyprr,onamrr,iprint)
               endif
            endif
c     store values in po array
         nval=0                              !cfan 05/2005  HSD r26-23
         do 550 idr=1,ndur
            if (pmaxh(idr) .ge. 0.0) then    !cfan
             nval=nval+1                     !cfan 
             val(nval) = hffg(idr)           !cfan
            endif                            !cfan  
            po(lhffg+idr-1) = hffg(idr)
550         continue
c     computational time
         po(lhffg-1) = lffcpd
c     write to file
         rewind (ipdv)
         call wppfil (ipdv,iuseh,po,istat)
         call pstcod (istat,hdid,hdwtyp,ipdv)
         call upclos (ipdv,bname,ic)
         nhead = nhead + 1
         if (iqctlh.eq.0.or.iqopth.eq.0) then
cfan        write (iupr,560) hdidz,(po(lhffg+i-1),i=1,ndur)
            write (iupr,560) hdidz,(val(i),i=1,nval)
560   format (' Head_ID=',a,' FFG_val:',5(1x,f5.2))
            else if (iqopth.ge.1.and.iqopth.le.3) then
               if (ndur.eq.3) then
cfan           write (iupr,570) hdidz,(po(lhffg+i-1),i=1,ndur),
cfan +            (qb(i),i=1,ndur)
               write (iupr,570) hdidz,(val(i),i=1,nval),
     +            (qb(i),i=1,nval)
570   format (' Head_ID=',a,' FFG_val:',3(1x,f5.2),t58,'baseQ:',
     +   5(1x,f7.1))
               else if (ndur.eq.4) then
cfan           write (iupr,580) hdidz,(po(lhffg+i-1),i=1,ndur),
cfan +            (qb(i),i=1,ndur)
               write (iupr,580) hdidz,(val(i),i=1,nval),
     +            (qb(i),i=1,nval)
580   format (' Head_ID=',a,' FFG_val:',4(1x,f5.2),t58,'baseQ:',
     +   5(1x,f7.1))
               else if (ndur.eq.5) then
cfan           write (iupr,590) hdidz,(po(lhffg+i-1),i=1,ndur),
cfan +            (qb(i),i=1,ndur)
               write (iupr,590) hdidz,(val(i),i=1,nval),
     +            (qb(i),i=1,nval)
590   format (' Head_ID=',a,' FFG_val:',5(1x,f5.2),t58,'baseQ:',
     +   5(1x,f7.1))
               endif
            else if (iqopth.eq.4) then
               if (ndur.eq.3) then
cfan           write (iupr,600) hdidz,(po(lhffg+i-1),i=1,ndur),sro
               write (iupr,600) hdidz,(val(i),i=1,nval),sro
600   format (' Head_ID=',a,' FFG_val:',3(1x,f5.2),t58,'sro: ',f5.2)
               else if (ndur.eq.4) then
cfan              write (iupr,610) hdidz,(po(lhffg+i-1),i=1,ndur),sro
                  write (iupr,610) hdidz,(val(i),i=1,nval),sro
610   format (' Head_ID=',a,' FFG_val:',4(1x,f5.2),t58,'sro: ',f5.2)
                  else if (ndur.eq.5) then
cfan              write (iupr,620) hdidz,(po(lhffg+i-1),i=1,ndur),sro
                  write (iupr,620) hdidz,(val(i),i=1,nval),sro
620   format (' Head_ID=',a,' FFG_val:',5(1x,f5.2),t58,'sro: ',f5.2)
                  endif
            endif
630      continue
c
640   nehwat = nerr - nehwat
      nwhwat = nwarn - nwhwat
c
      return
c
      end
