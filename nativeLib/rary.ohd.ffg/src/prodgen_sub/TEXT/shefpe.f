c  =====================================================================
c  pgm:  shefpe (type,mffg,iop)
c
c   in: type   .... data type
c   in: mffg   .... switch to select new or old SHEF PE codes (ffg)
c   in: iop    .... internal operation number
c  =====================================================================
c
      subroutine shefpe (type,mffg,iop)
c
c.......................................................................
c
c  Routine constructs SHEF codes including physical elements, duration
c  code and type and source code for flash flood guidance and model
c  carryover values
c
c.......................................................................
c   Initially written by
c        Tim Sweeney, Hydrologic Research Lab              Mar 1995
c
c   Added units designation
c        Tim Sweeney, HRL                                  Apr 1998
c
c   Changed physical elements for water supply
c        Tim Sweeney, HRL                                  Apr 1999
c.......................................................................
c
      character*2 blnk2,snope(17),sacpe(7),capipe(9),wsty(7)
      character*2 mkcpe(6),cinpe(8),harpe(8),hfdpe(8)
      character*3 ffgpe(5),its(7),engl,metr,wspe
      character*4 type
      character*5 affpe(5)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'prodgen_inc/pfparm'
c
      dimension mffgw(5),mffgd(5)
      dimension mwsw(7),mwsd(7)
      dimension msnow(17),msnod(17), msacw(7),msacd(7)
      dimension mcapw(9),mcapd(9),   mmkcw(6),mmkcd(6)
      dimension mcinw(8),mcind(8),   mharw(8),mhard(8)
      dimension mhfdw(8),mhfdd(8)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/shefpe.f,v $
     . $',                                                             '
     .$Id: shefpe.f,v 1.2 2003/03/14 18:35:33 dws Exp $
     . $' /
C    ===================================================================
C
      data blnk2/ '  ' /
      data ffgpe/ 'PFH','PFT','PFQ','PFK','PFD'/
      data affpe/ 'PPHCF','PPTCF','PPQCF','PPKCF','PPDCF'/
      data wspe / 'QMX'/
      data wsty/  'C1','C2','C3','C4','C5','C6','C7'/
      data snope/ 'BA','BB','BC','BD','BE','BF','BG','BH','BI',
     +            'BJ','BK','BL','BM','BN','BO','BP','BQ'/
      data sacpe/ 'CA','CB','CC','CD','CE','CF','CM'/
      data capipe/'CG','CH','CI','CJ','CK','CL','CM','CN','CO'/
      data mkcpe/ 'CG','CP','CQ','CR','CS','CT'/
      data cinpe/ 'CG','CP','CQ','CR','CS','CT','CU','CV'/
      data harpe/ 'CG','CP','CQ','CR','CS','CW','CX','CY'/
      data hfdpe/ 'CG','CP','CQ','CR','CS','CW','CX','CY'/
      data its/ 'IMS','IMA','IMK','IMC','IMH','IMT','IMW'/
      data engl / 'DUE' /
      data metr / 'DUS' /
c
      data mffgw/ 5*5 /
      data mffgd/ 5*1 /
      data mwsw / 4*6,3*7 /
      data mwsd / 7*1 /
      data msnow/ 17*6 /
      data msnod/ 17*2 /
      data msacw/ 6,6,7,7,6,6,5 /
      data msacd/ 2,3,2,3,2,2,1 /
      data mcapw/ 5*6,4*5 /
      data mcapd/ 5*2,1,1,2,0 /
      data mmkcw/ 6*5 /
      data mmkcd/ 2,2,2,1,1,1 /
      data mcinw/ 6,5,5,4,4,4,5,5 /
      data mcind/ 2,2,2,1,1,1,1,1 /
      data mharw/ 8*5 /
      data mhard/ 8*2 /
      data mhfdw/ 8*5 /
      data mhfdd/ 8*2 /
c
c
      call prbug ('shefpe',1,1,ibug)
c
c  default English units
      peunit = engl
c
      if (type.ne.'affg'.and.type.ne.'AFFG'.and.
     +    type.ne.'hffg'.and.type.ne.'HFFG') go to 40
c
c  flash flood guidance
      nff = 5
      if (mffg.lt.2) then
         do 10 i=1,nff
            pets(i) = ffgpe(i)//blnk2
10          continue
         else
            do 20 i=1,nff
               pets(i) = affpe(i)
20             continue
         endif
      do 30 i=1,nff
         nwid(i) = mffgw(i)
         ndec(i) = mffgd(i)
30       continue
      nel = nff
      iop = 0
      go to 150
c
40    if (type.ne.'cary'.and.type.ne.'CARY') go to 130
c
c  caryyover for rainfall-runoff and snow Operations
      nel = ncorr
      if (iop.eq.1) then
c     SAC-SMA
         if (ncorr.ge.7) ncorr = 6
         nel = ncorr
         do 50 i=1,ncorr
            pets(i) = sacpe(i)//its(iop)
            nwid(i) = msacw(i)
            ndec(i) = msacd(i)
50          continue
         peunit = metr
      else if (iop.eq.2) then
c     API-CONT
         do 60 i=1,4
            pets(i) = capipe(i)//its(iop)
            nwid(i) = mcapw(i)
            ndec(i) = mcapd(i)
60          continue
         k = 0
         if (ivopt.gt.0) then
            nwid(5) = mcapw(ivopt+4)
            ndec(5) = mcapd(ivopt+4)
            pets(5) = capipe(ivopt+4)//its(iop)
            k = 1
            endif
         do 70 i=5,7
            pets(i+k) = capipe(i+2)//its(iop)
            nwid(i+k) = mcapw(i+2)
            ndec(i+k) = mcapd(i+2)
70          continue
         nel = 7 + k
      else if (iop.eq.3) then
c     API-MKC
         do 80 i=1,ncorr
            pets(i) = mkcpe(i)//its(iop)
            nwid(i) = mmkcw(i)
            ndec(i) = mmkcd(i)
80          continue
      else if (iop.eq.4) then
c     API-CIN
         do 90 i=1,ncorr
            pets(i) = cinpe(i)//its(iop)
            nwid(i) = mcinw(i)
            ndec(i) = mcind(i)
90          continue
      else if (iop.eq.5) then
c     API-HAR
         do 100 i=1,ncorr
            pets(i) = harpe(i)//its(iop)
            nwid(i) = mharw(i)
            ndec(i) = mhard(i)
100         continue
      else if (iop.eq.6) then
c     API-HFD
         do 110 i=1,ncorr
            pets(i) = hfdpe(i)//its(iop)
            nwid(i) = mhfdw(i)
            ndec(i) = mhfdd(i)
110         continue
      else if (iop.eq.7) then
c     SNOW-17
         do 120 i=1,ncosn
            pets(i) = snope(i)//its(iop)
            nwid(i) = msnow(i)
            ndec(i) = msnod(i)
120         continue
         nel = ncosn
         peunit = metr
         else
         endif
      go to 150
c
130   if (type.ne.'wsup'.and.type.ne.'WSUP') go to 170
c
c  water supply
      nel = 7
      do 140 i=1,nel
         pets(i) = wspe//wsty(i)
         nwid(i) = mwsw(i)
         ndec(i) = mwsd(i)
140      continue
      go to 150
c
150   if (ibug.gt.0) write (iud,160) iop,nel,(pets(i),i=1,nel)
160   format (' iop=',i4,' nel=',i4,' pets=',20(1x,a5))
      go to 190
c
170   write (iue,180) type
180   format (' ERROR: type ',a4,' not valid for product.')
c
190   return
c
      end
