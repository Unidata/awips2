c  =====================================================================
c  pgm:  edhed (ident,ipdv,is,mpo,po,istat)
c
c  out: ident  .... identifier
c   in: ipdv   .... device number of parameter file
c   in: is     .... edit mode
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c  out: istat  .... status code
c  =====================================================================
c
      subroutine edhed (ident,ipdv,is,mpo,po,istat)
c
c.......................................................................
c
c  editor routine for headwater definition
c
c.......................................................................
c    Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      character*2 resp
      character*4 blnk,cnone,dimn
      character*8 ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/hwparm'
c
      dimension iupk(5),itaq(5),ratcid(2)
      dimension qid(2)
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/edhed.f,v $
     . $',                                                             '
     .$Id: edhed.f,v 1.6 2006/03/16 18:56:02 xfan Exp $
     . $' /
C    ===================================================================
C
      data blnk/'    '/
      data cnone/'none'/
c
c
      call prbug ('edhed',1,1,ibug)
c
      istat = 0
c
      mx = 16
10    write (iutw,20)
20    format (' Enter headwater identifier: ',$)
      read (iutr,'(a)',err=10) ident
      if (ident.eq.' ') then
         istat = 1
         go to 280
         endif
c
c  open file
      kod = 2
      call rppfil (ident,hdwtyp,kod,ipdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (ipdv)
         call gethed (po)
         call umemov (ident,hdid,2)
      else if (istat.eq.1) then
c  set default values for new file
         istat = 0
         call umemov (ident,hdid,2)
         do 40 i=1,5
            desc(i) = cnone
            strnam(i) = cnone
40          continue
         rcid(1) = blnk
         rcid(2) = blnk
         hlat = 0
         hlon = 0.
         hlath = 0.
         hlonh = 0.
         fsflow = 0.
         lhcpd = -1
         do 50 i=1,5
            hffg(i) = -999.0
            upk(i) = -99.0
            hinten(i) = 1.0
            taq(i) = -99.0
50          continue
         pcimpv = 0.
         iqopth = 0
         iropth = 0
         qtsid(1) = cnone
         qtsid(2) = blnk
         dtcq = cnone
         intq = 6
         nars = 1
         wt(1) = 0.
         arid(1,1) = cnone
         arid(2,1) = blnk
      else
         go to 280
      endif
c
c  convert decimal degrees to integer degrees and minutes
      icv = 2
      call degcon (icv,lat,hlat)
      call degcon (icv,lon,hlon)
      lath = hlath*60.
      lonh = hlonh*60.
c
60    write (iutw,70) hdid
70    format (25x,'Edit Headwater Parameters - ',2a4 /)
      ifs = fsflow
      do 80 i=1,5
         iupk(i) = upk(i) + 0.5
80       continue
      write (iutw,90) desc,strnam,lat,lath,lon,lonh,ifs,rcid
90    format (4x,'( 1) Description:  ',5a4 /
     +        4x,'( 2) Stream:       ',5a4 /
     +        4x,'( 3) Latitude (ddmm):  ',i7,
     +       t50,'( 4) Half-width lat (mm): ',i3 /
     +        4x,'( 5) Longitude (ddmm):',i8,
     +       t50,'( 6) Half-width lon (mm): ',i3 /
     +        4x,'( 7) Flood flow (cfs):',i8,
     +       t50,'( 8) Rating curve ID: ',2a4 /)
c
      if (fsflow.gt.10.0.or. 
     +    (rcid(1).ne.'    '.and.
     +     rcid(1).ne.'none'.and.
     +     rcid(1).ne.'NONE')) then
         write (iutw,100) iupk,pcimpv,iqopth,iropth
100   format (
     +        4x,'( 9) Unitgraph peaks (cfs): ',5i10 //
     +        4x,'(10) Percent impervious (.xx):',f4.2 /
     +        4x,'(11) High flow adjust (0, 1, 2, 3, 4):',i2,
     +       t50,'(12) Runoff adjust (0, 1, 2 or 3):',i2 /)
         else
              write (iutw,110) iupk,pcimpv,iqopth,iropth
110   format (
     +        4x,'( 9) Runoff (inches x 100): ',11x,5i10 //
     +        4x,'(10) Percent impervious (.xx):',f4.2,
     +        4x,'(11) High flow adjust (0 or 4): ',i2,
     +       t50,'(12) Runoff adjust (0, 1, 2 or 3):',i2 /)
          endif
      num = 13
c
      if (iqopth.gt.0.and.iqopth.le.3) then
         do 120 i=1,5
            itaq(i) = taq(i) + 0.5
120         continue
         write (iutw,130) qtsid,dtcq,intq,itaq
130   format (4x,'(13) Forecast flow time series ID: ',2a4 /
     +        4x,'(14) Data type code: ',14x,a4 /
     +        4x,'(15) Time interval (hrs): ',9x,i2 /
     +        4x,'(16) Times to adjust flow (xx hrs):',5i5 /)
         num = 17
         endif
c
      if (iropth.gt.0) then
         write (iutw,140) num,hinten
140   format (4x,'(',i2,') Values (x.xx for 1, 3, 6, 12',
     +        ' 24 hour durations:' / 10x,5f8.2 /)
         num = num + 1
         endif
c
      n = nars
      if (n.gt.3) n = 3
      if (nars.le.3) then
         write (iutw,150) num,nars
150   format (4x,'(',i2,') Number of weighted areas: ',i2)
         else
            write (iutw,160) num,nars
160   format (4x,'(',i2,') Number of weighted areas (first 3 ',
     +   'listed): ',i2)
         endif
      write (iutw,170) (wt(i),arid(1,i),arid(2,i),i=1,n)
170   format (7x,   3( 2x, f4.2,2x,2a4) )
      write (iutw,180)
180   format (' Select (number or <return>-exit): ',$)
c
      read (iutr,'(a)',err=60) resp
c
      if (resp.eq.' ') then
         go to 280
      else if (resp.eq.'1 ') then
         call edvca (is,iutr,iutw,5,desc)
      else if (resp.eq.'2 ') then
         call edvca (is,iutr,iutw,5,strnam)
      else if (resp.eq.'3 ') then
         call edvi (is,iutr,iutw,lat)
         icv = 1
         call degcon (icv,lat,hlat)
      else if (resp.eq.'4 ') then
         call edvi (is,iutr,iutw,lath)
         hlath = lath/60.
      else if (resp.eq.'5 ') then
         call edvi (is,iutr,iutw,lon)
         icv = 1
         call degcon (icv,lon,hlon)
      else if (resp.eq.'6 ') then
         call edvi (is,iutr,iutw,lonh)
         hlonh = lonh/60.
      else if (resp.eq.'7 ') then
         call edvi (is,iutr,iutw,ifs)
         fsflow = float(ifs)
      else if (resp.eq.'8 ') then
         call edvca (is,iutr,iutw,2,rcid)
c     check rating curve
         if (rcid(1).ne.blnk.and.
     +       rcid(1).ne.'none'.and.
     +       rcid(1).ne.'NONE') then
            call umemov (rcid,ratcid,2)
            call chekrc (ratcid,istat)
            if (istat.ne.0) then
               write (iutw,200) rcid
               if (iupr.ne.iutw) write (iupr,200) rcid
200   format (' ERROR: Rating Curve ',2a4,' not defined.')
               istat = 4
               rcid(1) = '    '
               rcid(2) = '    '
            endif
         endif
      else if (resp.eq.'9 ') then
c         call edvrag (is,iutr,iutw,upk,5,1,ng)
          call edviag (is,iutr,iutw,iupk,5,1,ng)
          do 210 i=1,5
210          upk(i) = iupk(i)
      else if (resp.eq.'10') then
         call edvrag (is,iutr,iutw,pcimpv,1,1,ng)
      else if (resp.eq.'11') then
         if (iqopth.ne.4) then
           write (iutw,220)
220   format (4x,'High Flow Adjust Options:',
     +        6x,'0 - no adjustment',
     +        6x,'1 - flow at specified number of hours in future',
     +        6x,'2 - highest flow over specified number of hours',
     +        6x,'3 - highest flow in time series',
     +        6x,'4 - reduce threshold runoff by storm runoff')
         else
            write (iutw,230)
230   format (4x,'High Flow Adjust Options:',
     +        6x,'0 - no adjustment',
     +        6x,'4 - reduce threshold runoff by storm runoff')
           endif
         call edvi (is,iutr,iutw,iqopth)
      else if (resp.eq.'12') then
         write (iutw,240)
240   format (4x,'Runoff Adjust Options:',
     +        6x,'0 - no adjustment',
     +        6x,'1 - adjust runoff',
     +        6x,'2 - use values as ffg',
     +        6x,'3 - use runoff as ffg')
         call edvi (is,iutr,iutw,iropth)
      else if (resp.eq.'13'.and.iqopth.gt.0) then
         call edvca (is,iutr,iutw,2,qtsid)
c     check for forecast flow time series
         if (iqopth.gt.0.and.qtsid(1).ne.blnk) then
            ichekts = 0
            if (ichekts.eq.1) then
               call umemov (qtsid,qid,2)
               call umemov (dtcq,dq,1)
               ichkdm = 1
               miss = 0
               noval = 1
               dimn='L'
               call chekts (qid,dq,intq,ichkdm,dimn,miss,noval,istat)
               if (istat.ne.0) then
                  write (iutw,250) qtsid
                  if (iupr.ne.iutw) write (iupr,250) qtsid
250   format (' ERROR: forecast flow time series for ',2a4,
     +   ' not defined.')
                  istat = 5
                  qtsid(1) = '    '
                  qtsid(2) = '    '
               endif
            endif
         endif
      else if (resp.eq.'14'.and.iqopth.gt.0) then
         call edvca (is,iutr,iutw,1,dtcq)
      else if (resp.eq.'15') then
         call edviag (is,iutr,iutw,intq,1,1,ng)
      else if (resp.eq.'16') then
         call edviag (is,iutr,iutw,itaq,5,1,ng)
         do 260 i=1,5
            taq(i) = itaq(i)
260         continue
      else if (resp.eq.'17'.and.iqopth.gt.0.and.iropth.gt.0 .or.
     +         resp.eq.'13'.and.iqopth.eq.0.and.iropth.gt.0) then
         call edvrag (is,iutr,iutw,hinten,5,1,ng)
      else if (resp.eq.'18'.or.resp.eq.'17'.and.iropth.eq.0 .or.
     +         resp.eq.'14'.and.iqopth.eq.0.and.iropth.gt.0 .or.
     +         resp.eq.'13'.and.iqopth.eq.0.and.iropth.eq.0) then
         write (iutw,270)
270   format (' Enter (ww,id  <return>-no change  0,endid-end)')
         call edwtid (is,iutr,iutw,wt,arid,mx,nars)
      else
         go to 280
      endif
c
      go to 60
c
280   return
c
      end
