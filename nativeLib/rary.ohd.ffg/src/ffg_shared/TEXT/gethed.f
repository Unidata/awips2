c  =====================================================================
c  pgm:  gethed (po)
c
c   in: po     .... headwater parameters
c  =====================================================================
c
      subroutine gethed (po)
c
c.......................................................................
c
c  Routine to fill common block hwparm with headwater parameters from
c  the po array
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Oct 1992
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/hwparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/gethed.f,v $
     . $',                                                             '
     .$Id: gethed.f,v 1.2 2004/01/30 17:49:09 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gethed',1,1,ibug)
c
c  version number
      hvers = po(1)
c
c  identifier
      call umemov (po(2),hdid,2)
c
c  type
      call umemov (po(4),hdwtyp,1)
c
c  location name and stream name
      call umemov (po(6),desc,5)
      call umemov (po(11),strnam,5)
c
c  Rating Curve identifier
      call umemov (po(16),rcid,2)
c
c  latitude and longitude of centroid
      hlat = po(18)
      hlon = po(19)
c
c  1/2 width of box that encompasses basin
      hlath = po(20)
      hlonh = po(21)
c
c  flow at flood stage
      fsflow = po(23)
c
c  unitgraph peak ordinates or threshold runoffs
      do 10 i=1,5
         upk(i) = po(i+23)
10       continue
c
c  determine number of durations nhdur based on number of
c  threshold runoffs/unitgraph peaks defined
      nhdur = 3
      if (upk(4).gt.1.) nhdur = 4
      if (nhdur.eq.4.and.upk(5).gt.1.0) nhdur = 5
c
c  percent impervious area
      pcimpv = po(29)
c
c  location of output ffg values
      lhffg = po(33) + 0.01
c
c  location of base flow adjustment parameters
      lbadj = po(34) + 0.01
c
c  location of intensity adjustment parameters
ccc      liadj = po(35) + 0.01
c
c  location of number of areas used to compute for this headwater
      loar  = po(36) + 0.01
c
c  computation date for hffg
      lhcpd = po( lhffg-1 )
c
c  ffg values
      do 20 i=1,5
         hffg(i) = po(lhffg+i-1)
20       continue
c
c  high flow adjustment parameters
      if (lbadj.eq.0) then
         iqopth = 0
         else
            iqopth = po(lbadj) + 0.01
c        times to adjust flow
            do 30 i=1,5
               taq(i) = po(i+lbadj)
30             continue
c        forecast flow time series identifier
            call umemov (po(lbadj+6),qtsid,2)
c        forecast flow time series data type code
            call umemov (po(lbadj+8),dtcq,1)
c        forecast flow time series time interval
            intq = po(lbadj+9) + 0.01
         endif
c
c  runoff adjustment parameters
      if (lradj.eq.0) then
         iropth = 0
         else
            iropth = po(lradj) + 0.01
            do 40 i=1,5
               hinten(i) = po(lradj+i)
40             continue
          endif
c
c  number of weighted areas
      nars = po(loar) + 0.01
c
c  area weights and identifiers
      lowa = loar + 4
      do 50 i=1,nars
         j = (i-1)*3 + lowa
         wt(i) = po(j)
         call umemov (po(j+1),arid(1,i),2)
50       continue
c
c  number of words
      iuseh = po(5) + 0.01
c
      call prbug2 ('gethed',1)
c
      return
c
      end
