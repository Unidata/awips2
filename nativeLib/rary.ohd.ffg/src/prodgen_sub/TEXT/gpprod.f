c  =====================================================================
c
      subroutine gpprod (mpo,po)
c
c.......................................................................
c
c  Generate product heading and get area, headwater or urban area
c  identifiers for the locations desired in this product.
c
c  Gets LSTCMPDY (ffg, carryover, water supply) and rainfall-runoff
c  model for carryovers from respective parameter files in database.
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL                                Apr 1992
c
c  Moved date and time conversion algorithms into routine
c         Tim Sweeney, HRL                                Mar 1998
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/arparm'
      include 'ffg_inc/iodev'
      include 'ffg_inc/hwparm'
      include 'ffg_inc/timez'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/wsparm'
      include 'ffg_inc/count'
      include 'prodgen_inc/pfparm'
      include 'prodgen_inc/propar'
      include 'prodgen_inc/poptns'
      include 'common/fctime'
c
      character*2 bname
      character*4 type,rrm(2),rrtyp(12)
      character*8 ident,lident
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpprod.f,v $
     . $',                                                             '
     .$Id: gpprod.f,v 1.7 2004/01/30 17:57:14 scv Exp $
     . $' /
C    ===================================================================
C
      data bname/ '  ' /
      data rrtyp/'SAC-','SMA ','API-','CONT','API-','MKC ',
     +           'API-','CIN ','API-','HAR ','API-','HFD '/
c
c
      call prbug ('gpprod',1,1,ibug)
ccc      ibug = 1
c
c  fill common block propar
      call getprd (po)
c
      if (iupr.eq.iutw) write (iutw,10) prodid
10    format (' NOTE: making product ',2a4,'.')
      if (iupr.ne.iutw) write (iupr,10) prodid
c
c  computer sys time zone designator
      call tzcnum (cpzone,lcptz)
c
c  Time zone designator for product
      tzout = tzone
      call tzcnum (tzout,ictz)
      if (ibug.eq.1) write (iud,20) cpzone,lcptz,tzout,ictz
20    format (' in gpprod - cpzone=',a4,' lcptz=',i4,' tzout=',a,
     *   '  ictz=',i4)
c
c  determine number of durations
      if (jdur.eq.12) then
         ndur = 4
         else if (jdur.eq.24) then
            ndur = 5
         else
            ndur = 3
         endif
c
      ndes = jdes
      nstr = jstr
c
c  get current time
      kmo = nmo
      kda = nda
      kyr = nyr
      khr = nhr
      kmn = nmn
      call datimz (lcptz,ictz,kmo,kda,kyr,khr,kmn,kzmo,kzda,kzyr,kzhr,
     +             kzmn,ndawk,ldayl,mxday,julda)
c
c  convert current (local) date and time to single characters
      ipos = 1
      num = 1
      iwid = 4
      call ffi2a (nowlcl(1),ipos,iwid,num,kyr)
      iwid = 2
      call ffi2a (nowlcl(5),ipos,iwid,num,kmo)
      call ffi2a (nowlcl(7),ipos,iwid,num,kda)
      call ffi2a (nowlcl(9),ipos,iwid,num,khr)
      call ffi2a (nowlcl(11),ipos,iwid,num,kmn)
      do 30 i=5,11,2
         if (nowlcl(i).eq.' ') nowlcl(i) = '0'
30       continue
c
c  convert current (z) date and time to single characters
      iwid = 4
      call ffi2a (nowz(1),ipos,iwid,num,kzyr)
      iwid = 2
      call ffi2a (nowz(5),ipos,iwid,num,kzmo)
      call ffi2a (nowz(7),ipos,iwid,num,kzda)
      call ffi2a (nowz(9),ipos,iwid,num,kzhr)
      call ffi2a (nowz(11),ipos,iwid,num,kzmn)
      do 40 i=5,11,2
         if (nowz(i).eq.' ') nowz(i) = '0'
40       continue
c
      mxel = 0
c
      lcocpd = -1
c
      if (dftyp.ne.'AFFG'.and.dftyp.ne.'affg') go to 70
      
c  get LSTCMPDY from area data
      do 60 i=1,nolid
         if (alidty(3,i).eq.'    '.or.
     +       alidty(3,i).eq.'AFFG' .or.
     +       alidty(3,i).eq.'affg') then
            call umemov (alidty(1,i),ident,2)
            type = 'affg'
            kod = 1
            call rppfil (ident,type,kod,kpdv,mpo,po,npo,istat)
            if (istat.eq.0) then
               call upclos (kpdv,bname,istat)
               call getar (po)
               lffcpd = lacpd
               if (ibug.eq.1) write (iud,50) dftyp,i,ident,lffcpd
50    format (' in gpprod - dftyp=',a,' i=',i3,' ident=',a,
     +   ' lffcpd=',i6)
               cpd = float(lffcpd)
               call ckcpd (ident,cpd,lident,lcocpd,nwarn)
               endif
            endif
60       continue
      if (ndur.gt.mxdurg) ndur = mxdurg
      go to 180
c
70    if (dftyp.ne.'CARY'.and.dftyp.ne.'cary') go to 120
c
c  get LSTCMPDY from carryover data
c  get date of latest cary id and max number of carryover values
      irrtyp = 0
      do 110 i=1,nolid
         if (alidty(3,i).eq.'    '.or.
     +       alidty(3,i).eq.'CARY' .or.
     +       alidty(3,i).eq.'cary') then
            call umemov (alidty(1,i),ident,2)
            type = 'cary'
            kod = 1
            call rppfil (ident,type,kod,kpdv,mpo,po,npo,istat)
            if (istat.eq.0) then
               call upclos (kpdv,bname,istat)
               k = ifix(po(18) )
               if (k.lt.1) go to 110
               lffcpd = k
c           check if snow carryover needed
               ls = po(13)
               ncosn = po(ls+4)
c           control output of snow carryovers using nstr
c           (stream name switch)
               if (nstr.eq.0) ncosn = 0
               if (ncosn.gt.mxel) mxel = ncosn
c           check number of rainfall-runoff carryover values
               lr = po(12)
               ncorr = po(lr+4)
               if (ncorr.gt.16) go to 110
               if (ncorr.gt.mxel) mxel = ncorr
c           determine which rainfall-runoff model & if all IDs use same
c           rainfall-runoff model
               call umemov (po(lr),rrm,2)
               do 80 k=1,6
                  m = (k-1)*2 + 1
                  if (rrm(1).ne.rrtyp(m)) go to 80
                  if (rrm(2).eq.rrtyp(m+1)) then
                     if (irrtyp.le.0) irrtyp = k
                     if (k.eq.irrtyp) go to 80
                     irrtyp = 0
                     go to 90
                     endif
80                continue
c           can't use .B for API-CONT because of IVOPT; must use .A
90             if (irrtyp.eq.2) irrtyp = 0
               if (ibug.eq.1) write (iud,100) dftyp,i,ident,lffcpd,
     +            ncosn,ncorr,mxel
100   format (' in gpprod - dftyp=',a,' i=',i3,' ident=',a,
     +   ' lffcpd=',i6,' ncosn=',i2,' ncorr=',i2,' mxel=',i2)
               cpd = float( lffcpd )
               call ckcpd (ident,cpd,lident,lcocpd,nwarn)
               endif
            endif
110      continue
      go to 180
c
120   if (dftyp.ne.'HFFG'.and.dftyp.ne.'hffg') go to 150
c
c  get latest date LSTCMPDY for headwaters
      do 140 i=1,nolid
         if (alidty(3,i).eq.'    '.or.
     +       alidty(3,i).eq.'HFFG' .or.
     +       alidty(3,i).eq.'hffg') then
            call umemov (alidty(1,i),ident,2)
            type = 'hffg'
            kod = 1
            call rppfil (ident,type,kod,kpdv,mpo,po,npo,istat)
            if (istat.eq.0) then
               call upclos (kpdv,bname,istat)
               call gethed (po)
               lffcpd = lhcpd
               if (ibug.eq.1) write (iud,130) dftyp,i,ident,lffcpd
130   format (' in gpprod - dftyp=',a,' i=',i3,' ident=',a,
     +   ' lffcpd=',i6)
               cpd = float(lffcpd)
               call ckcpd (ident,cpd,lident,lcocpd,nwarn)
               endif
            endif
140      continue
      if (ndur.gt.mxdurh) ndur = mxdurh
      go to 180
c
150   if (dftyp.ne.'WSUP'.and.dftyp.ne.'wsup') go to 210
c
c  get latest date LSTCMPDY for water supply
      do 170 i=1,nolid
         if (alidty(3,i).eq.'    '.or.
     +       alidty(3,i).eq.'WSUP' .or.
     +       alidty(3,i).eq.'wsup') then
            call umemov (alidty(1,i),ident,2)
            type = 'wsup'
            kod = 1
            call rppfil (ident,type,kod,kpdv,mpo,po,npo,istat)
            if (istat.eq.0) then
               call upclos (kpdv,bname,istat)
               call getsup (po)
               lffcpd = lwcpd
               if (ibug.eq.1) write (iud,160) dftyp,i,ident,lffcpd
160   format (' in gpprod - dftyp=',a,' i=',i3,' ident=',a,
     *   ' lffcpd=',i6)
               cpd = float(lffcpd)
               call ckcpd (ident,cpd,lident,lcocpd,nwarn)
               else
               endif
            endif
170      continue
c
c  convert OFS computation date
180   local = loclo
      nlstz = nlstzo
      if (ibug.eq.1) write (iud,*) 'in gpprod - local=',local,
     +   ' nlstz=',nlstz
c  convert date and change 24 hours to 0 hours
      call int2xt (lcocpd,inda,inhr,lczmo,lczda,lczyr,lczhr,0,0,code)
c  convert integer numbers to integer digits
      iwid = 4
      call ffi2a (lcptim(1),ipos,iwid,num,lczyr)
      iwid = 2
      call ffi2a (lcptim(5),ipos,iwid,num,lczmo)
      call ffi2a (lcptim(7),ipos,iwid,num,lczda)
      call ffi2a (lcptim(9),ipos,iwid,num,lczhr)
      lczmn = 0
      call ffi2a (lcptim(11),ipos,iwid,num,lczmn)
      do 200 i=5,11,2
         if (lcptim(i).eq.' ') lcptim(i) = '0'
200      continue
      if (ibug.eq.1) write (iutw,*) 'in gpprod - lcptim=',lcptim
c
210   return
c
      end
