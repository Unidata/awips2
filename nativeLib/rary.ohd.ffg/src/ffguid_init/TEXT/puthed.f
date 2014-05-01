c  =====================================================================
c  pgm:  puthed (jed,iuse,po,ic)
c
c   in: jed    .... editor control:
c                     0 = editor not used
c                     1 = editor used
c  out: iuse   .... number of words to store parameters in po
c  out: po     .... array containing headwater parameters
c  i/o: ic     .... completion code
c  =====================================================================
c
      subroutine puthed (jed,iuse,po,ic)
c
c.......................................................................
c
c  put data type headwater parameters in po array
c
c.......................................................................
c      Initially written by
c           Tim Sweeney, HRL - March 1992
c.......................................................................
c
      character*4 blnk,dimn
      character*4 cend(2),lcend(2)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/hwparm'
      include 'ffg_inc/count'
      include 'common/fratng'
      include 'common/frcptr'
c
      dimension qid(2),po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/puthed.f,v $
     . $',                                                             '
     .$Id: puthed.f,v 1.4 2004/09/13 14:24:00 scv Exp $
     . $' /
C    ===================================================================
C
      data cend / 'ENDI','D   '/
      data lcend/ 'endi','d   '/
      data blnk / '    '/
      data dimn / 'L   '/
c
c
      call prbug ('puthed',1,1,ibug)
c
      if (ic.gt.0) go to 180
c
      hdwtyp = 'hffg'
c
c  file version number
      hvers = 1.0
      po(1) = hvers
c
c  transfer to parameter array po
c  identifier
      call umemov (hdid,po(2),2)
c
c  type code
      call umemov (hdwtyp,po(4),1)
c
c  location name and stream name
      call umemov (desc,po(6),5)
      call umemov (strnam,po(11),5)
c
c  check rating curve definition
      istat = 0
      if (rcid(1).ne.blnk.and.
     +    rcid(1).ne.'none'.and.
     +    rcid(1).ne.'NONE') then
         call chekrc (rcid,istat)
         if (istat.eq.0) then
            go to 20
         else
            if (iutw.ge.0) write (iutw,10) rcid,istat
            if (iupr.ne.iutw) write (iupr,10) rcid,istat
10    format (' ERROR: Rating Curve ',2a4,' not defined.',
     +   ' chekrc istat=',i3)
            istat = 4
            rcid(1) = '    '
            rcid(2) = '    '
            nerr = nerr + 1
         endif
      endif
c
c  transfer to po array
c  rating curve id
20    call umemov (rcid,po(16),2)
c
c  latitude, longitude of centroid of box
      po(18) = hlat
      po(19) = hlon
c
c  1/2 widths for box
      po(20) = hlath
      po(21) = hlonh
c
c  unused
      po(22) = -999.0
c
c  flow at flood stage
      po(23) = fsflow
c
c  unitgraph peak flow or threshold runoff
      do 30 i=1,5
30    po(i+23) = upk(i)
c
c  percent impervious area
      po(29) = pcimpv
c
c  unused
      do 40 i=1,3
40    po(i+29) = -999.0
c
c  location in po array of ffg values
      lhffg = 38
      po(33) = lhffg+0.01
c
c  computational time for hffg
      po(lhffg-1) = lhcpd
c
c  headwater flash flood guidance values
      do 50 i=1,5
50    po(lhffg+i-1) = hffg(i)
      nxt = lhffg + 5
c
c  location in po array of base flow adjust parameters
      if (iqopth.gt.0) then
         lbadj = nxt
         nxt = lbadj + 12
         else
            lbadj = 0
         endif
      po(34) = lbadj+0.01
c
c  location in po array of runoff adjust parameters
      if (iropth.gt.0) then
         lradj = nxt
         nxt = lradj + 8
         else
            lradj = 0
         endif
      po(35) = lradj + 0.01
c
c  location in po array of number of areas
      loar = nxt
      po(36) = loar
c
      if (iqopth.lt.1) go to 90
c
c  transfer base flow parameters to po array
      if (qtsid(1).ne.blnk) then
         ichekts = 0
         if (ichekts.eq.1) then
c        check time series
            call umemov (qtsid,qid,2)
            call umemov (dtcq,dq,1)
            ichkdm = 1
            dimn='L'
            miss = 0
            noval = 1
            istat = 0
            call chekts (qid,dq,intq,ichkdm,dimn,miss,noval,istat)
            if (istat.ne.0) then
               write (iutw,60) qtsid
               if (iupr.ne.iutw) write (iupr,60) qtsid
60    format (' ERROR: forecast flow time series for ',2a4,
     +        ' not defined.')
               istat = 4
               nerr = nerr + 1
               go to 180
               endif
            endif
         endif
c
c  method of adjusting base flow
      po(lbadj) = iqopth
c
c  times to adjust flow
      do 80 i=1,5
         po(i+lbadj) = taq(i)
80       continue
c
c  time series id for forecast flows
      call umemov (qtsid,po(lbadj+6),2)
c
c  time series data type code for forecast flow
      call umemov (dtcq,po(lbadj+8),1)
c
c  time interval forecast flow time series
      po(lbadj+9) = intq
c
c  unused
      po(lbadj+10) = -999.0
      po(lbadj+11) = -999.0
c
c  transfer runoff adjust parameters to po array
90    if (iropth.lt.1) go to 120
      po(lradj) = iropth
      do 100 i=1,5
         if (hinten(i).eq.0.0) hinten(i) = 1.00
         po(lradj+i) = hinten(i)
100      continue
c
c  unused
      do 110 i=1,2
         po(lradj+i+5) = -999.0
110      continue
c
c  areas
120   lpe = loar
c
c  unused
      do 130 i=1,3
         po(i+lpe) = -999.0
130      continue
c
c  area weights and identifiers
      lpe = lpe + 4
      if (jed.eq.0) then
c     check for 'endid' when parameters are entered from a file
         k = 16
         else
c        limit loop to nars when parameters are entered from editor
            k = nars
         endif
      nars = 0
      do 150 i=1,k
         if (arid(1,i).eq.blnk) go to 150
         if (jed.eq.1) go to 140
            if (arid(1,i).ne.cend(1).and.
     +          arid(1,i).ne.lcend(1)) go to 140
            if (arid(2,i).eq.cend(2).or.
     +          arid(2,i).eq.lcend(2)) go to 160
140      nars = nars + 1
         j = lpe + (nars-1)*3
c     transfer variables to po array
         po(j) = wt(i)
         call umemov (arid(1,i),po(j+1),2)
150      continue
c
c  number of weighted areas
160   po(loar) = float(nars)
c
c  compute number of words used
      iuseh = 3*nars + lpe
      po(5) = float(iuseh)
      iuse  = iuseh
      if (ibug.gt.0) write (iud,*) 'iuseh=',iuseh
c
180   return
c
      end
