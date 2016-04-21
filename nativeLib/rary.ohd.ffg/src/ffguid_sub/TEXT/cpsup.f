c  =====================================================================
c  pgm:  cpsup
c
c  variables are passed in common blocks
c
c  =====================================================================
c
      subroutine cpsup
c
c.......................................................................
c
c  compute water supply guidance for a location
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL                           Sept 1995
c.......................................................................
c
      character*2 bname
      character*4 blnk,cnone
      character*4 ffgtyp
      character*8 ident,ffgid,lident
      character*50 strng
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/wsparm'
      include 'ffg_inc/count'
      include 'ffg_inc/gidx'
      include 'ffg_inc/gpo'
      include 'ffg_inc/gfx'
      include 'common/fctime'
c
      dimension sum(6),wro(6)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/cpsup.f,v $
     . $',                                                             '
     .$Id: cpsup.f,v 1.7 2004/09/13 14:23:16 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('cpsup',1,1,ibug)
c
      bname = ' '
      blnk = ' '
      cnone = 'none'
c
      wstyp  = 'wsup'
c
      strng='water supply runoff'
c
      newsup = nerr
      nwwsup = nwarn
      nrn    = 6
c
c  get user info
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.ne.0) then
         write (iutw,5) strng(1:lenstr(strng))
         if (iupr.ne.iutw) write (iupr,5) strng(1:lenstr(strng))
5     format (' WARNING: computations for ',a,' will not be done ',
     +   'because user controls are not defined.')
         go to 290
         endif
c
c  check if water supply runoff computations desired
      if (iwats.lt.1) then
         write (iutw,7) strng(1:lenstr(strng)),iwats
         if (iupr.ne.iutw) write (iupr,7) strng(1:lenstr(strng)),iwats
7     format (' WARNING: computations for ',a,' will not be done ',
     +   'because the value of the user control option is ',i2,'.')
         go to 290
         endif
c
      local = loclo
      nlstz = nlstzo
c
c  get index (file) to wsup locations
      call getidx (wstyp,idxdv,mcidx,po,ms,cidx,istat)
      call upclos (idxdv,bname,ic)
c
      write (iutw,30) strng(1:lenstr(strng)),ms
30    format (/ ' Computing ',a,' for ',i4,' locations.')
      if (iupr.ne.iutw) write (iupr,30) strng(1:lenstr(strng)),ms
c
c  process each location
      do 280 ls=1,ms
         if (cidx(1,ls).ne.cnone) go to 40
         if (cidx(2,ls).eq.cnone) go to 280
40       wsid(1) = cidx(1,ls)
         wsid(2) = cidx(2,ls)
c     read wsup parameters
         call umemov (cidx(1,ls),ident,2)
         kod = 0
         call rppfil (ident,wstyp,kod,ipdv,mpo,po,npo,istat)
         if (istat.eq.0) then
c        fill wsup parameters common block wsparm
            call getsup (po)
            else
               write (iutw,50) wsid
50    format (' ERROR: water supply parameters not found for ',2a4,
     +   '.')
               nerr = nerr + 1
               go to 280
            endif
c     initialize local arrays and weight
         wtsum = 0.
         do 60 i=1,nrn
            sum(i) = 0.0
            wro(i) = 0.0
60          continue
         noar = 0
70    format (' areas=',i4,16(/' wt()=',f4.2,' sarid()=',2a4))
c     skip this water supply location if no areas defined for it
         if (nsars.lt.1) then
            write (iutw,80) wsid
            if (iupr.ne.iutw) write (iupr,80) wsid
80    format (' ERROR: no areas defined for ',2a4,'.')
            nerr = nerr + 1
            go to 280
            endif
         if (ibug.gt.0) write (iud,70) nsars,
     +      (swt(i),(sarid(j,i),j=1,2),i=1,nsars)
c     process for each area
         do 150 ia=1,nsars
            if (sarid(1,ia).eq.blnk) go to 150
            call umemov (sarid(1,ia),ffgid,2)
c        get FFG parameter record
            ffgtyp = 'FFG'
            irec = 0
            call rpprec (ffgid,ffgtyp,irec,mfx,fx,nfill,irecnx,istat)
            if (ibug.gt.0) write (iud,90) ffgid,ffgtyp,mfx,istat
90    format (' ffgid=',a,' ffgtyp=',a,' mffg=',i4,' istat=',i2)
            if (istat.eq.6) then
               go to 150
               else if (istat.eq.3) then
                  write (iutw,100) nfill,ffgtyp,mfx,ffgid
100   format (' ERROR: in cpsup - number of words needed to read ',a,
     +   ' parameters (',i5,') exceeds maximum (',i5,') for ',
     +   'FFG identifier ',a,'.')
                  nerr = nerr + 1
                  go to 150
               else if (istat.ne.0) then
                  write (iutw,110) ffgid,istat
110   format (' ERROR: no rainfall runoff curves found for area ',a,
     +   '. rpprec istat=',i2)
                  nerr = nerr + 1
                  go to 150
               endif
            noar = noar + 1
            wtsum = wtsum + swt(ia)
c        check for valid rainfall-runoff curve
c        where fx(41) is 2nd number, the runoff, of 4th pair that 
c        defines the rainfall runoff curve
            if (fx(41).le.0.0) then
               istat = 7
               write (iutw,120) ffgid
               if (iupr.ne.iutw) write (iupr,120) ffgid
120   format (' ERROR: Flash Flood Guidance Operation never run for ',
     +   'area ',a,'.')
               nerr = nerr + 1
               endif
c        check time (LSTCMPDY)
            call ckcpd (ffgid,fx(17),lident,lffcpd,nwarn)
c        use rainfall-runoff curve for 6-hr duration
            idr = 3
c        loop for rainfall amounts - variables used:
c           fx   - rainfall runoff curves for all durations
c           rnfl - total precip that produces total runoff
c           ro   - total runoff
c           swt  - weight of component area
            do 140 ir=1,nrn
c           calculate runoff from rainfall-runoff curve for specific
c           rain
               call calcro (idr,fx,nfill,ro,rnfl(ir))
               if (ro.lt.0.0) ro = 0.0
               wro(ir) = wro(ir) + ro*swt(ia)
               sum(ir) = sum(ir) + ro
               if (ibug.gt.0) write (iud,130) ir,ffgid,swt(ia),
     +            wtsum,sum(ir),wro(ir),ro
130   format (' ir=',i1,' ffgid=',a,' swt(ia)=',f4.2,' wtsum=',f4.2,
     +   ' sum(ir)=',f5.2,' wro(ir)=',f5.2,' ro=',f5.2)
c           end of loop for all rainfalls
140            continue
c        end of loop for all areas
150         continue
c     check that rainfall runoff curves actually found
         if (noar.lt.1) then
            do 160 i=1,nrn
               wsup(i) = 26.0
160            continue
c        decrement counter
            nwsup = nwsup - 1
            go to 240
         endif
c     sum of weights (wtsum) determines weighting technique used:
c        0.98 <= wtsum <= 1.02  use weighted wro
c        0.05 <= wtsum <= 0.97  use normalized weighted wro
c        wtsum = 0.0 and other  use average wro
         if (wtsum.ge.0.98.and.wtsum.le.1.02) then
c        use weighted wro
            go to 200
            else if (wtsum.ge.0.05 .and. wtsum.le.0.97) then
c           use normalized weight
               do 170 i=1,nrn
                  wro(i) = wro(i)/wtsum
170               continue
            else
c           use compute average wro
               do 180 i=1,nrn
                  wro(i) = sum(i)/noar
180               continue
            endif
         if (ibug.gt.0) write (iud,190) wtsum,wsid
190   format (' wtsum=',f4.2,' wsid=',2a4)
c     convert to million gallons per sq mi
200      do 210 i=1,nrn
            wsup(i) = wro(i)*17.374 + 0.05
210         continue
         if (iwats.eq.2) then
c        convert output runoff to total area
            if (sarea.lt.2.0) then
               write (iutw,220) wsid
220   format (' ERROR: no area size available for ',2a4,'.')
               nerr = nerr + 1
               sarea = 1.0
               endif
            do 230 i=1,nrn
               wsup(i) = wsup(i)*sarea
230            continue
            endif
         if (nrn.gt.1) then
            do 235 idr=1,nrn-1
               val1=wsup(idr)
               val2=wsup(idr+1)
               if (val1.gt.val2) then
                  write (iutw,233) 'runoff',
     +               idurt(idr+1),val2,idurt(idr),val1,wsid
                  if (iupr.ne.iutw) write (iupr,233) 'runoff',
     +               idurt(idr+1),val2,idurt(idr),val1,wsid
233   format (' WARNING: ',a,' value for ',
     +   i2,' hour duration (',f6.3,
     +   ') is less than value for ',
     +   i2,' hour duration (',f6.3,
     +   ') for Water Supply location ',2a4,'.')
                  endif
235            continue
            endif
c     store values in po array
240      do 250 i=1,nrn
            po(lro+i-1) = wsup(i)
250         continue
c     store computational time
         po(lro-1) = lffcpd
         if (iwats.lt.2) then
            write (iupr,260) wsid,(po(lro+i-1),i=1,nrn)
260   format (' Wsup_ID=',2a4,' FFG_val=',6f8.2)
            else
               write (iupr,270) wsid,(po(lro+i-1),i=1,nrn)
270   format (' Wsup_ID=',2a4,' FFG_val=',6f8.0)
            endif
c     write to file
         rewind (ipdv)
         call wppfil (ipdv,iuses,po,istat)
         call pstcod (istat,wsid,wstyp,ipdv)
         call upclos (ipdv,bname,ic)
         nwsup = nwsup + 1
280      continue
c
290   newsup = nerr - newsup
      nwwsup = nwarn - nwwsup
c
      return
c
      end
