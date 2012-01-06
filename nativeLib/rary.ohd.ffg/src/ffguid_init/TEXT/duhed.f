c  =====================================================================
c  pgm:  duhed (po,iout)
c
c   in: po     .... parameter array
c   in: iout   .... output device
c  =====================================================================
c
      subroutine duhed (po,iout)
c
c.......................................................................
c
c  This routine dumps parameters that define headwaters (data type
c  hffg) to a file in the same format that can be used to define the
c  headwaters.
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL                              May 1992
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/hwparm'
c
      character*4 blnk,cnone
      character*22 wdesc,wstrn
c
      dimension ipk(5),itaq(5),jwt(16),po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/duhed.f,v $
     . $',                                                             '
     .$Id: duhed.f,v 1.2 2003/03/14 18:13:40 dws Exp $
     . $' /
C    ===================================================================
C
      data blnk/ '    '/
      data cnone/ 'none'/
c
c
      call prbug ('duhed',1,1,ibug)
c
      call gethed (po)
c
c  convert decimal degrees to integer degrees and minutes
      icv = 2
      call degcon (icv,lat,hlat)
      call degcon (icv,lon,hlon)
ccc      lats = 0
ccc      lons = 0
ccc      lat = lat*100 + lats
ccc      lon = lon*100 + lons
c
c  insert delimiters in name fields if needed
      if (desc(1).eq.blnk) desc(1) = cnone
      call adelim (desc,5,ld,wdesc)
      if (strnam(1).eq.blnk) strnam(1) = cnone
      call adelim (strnam,5,ls,wstrn)
c
      write (iout,10) hdwtyp,hdid,wdesc(1:ld),wstrn(1:ls),lat,lon
10    format (a4,1x,2a4,1x,a,1x,a,1x,i6,1x,i7)
c
      icv = 2
      call degcon (icv,lat,hlat)
      call degcon (icv,lon,hlon)
      latd = lat/100
      latm = lat - latd*100
      lond = lon/100
      lonm = lon - lond*100
      call degcon (icv,lath,hlath)
      call degcon (icv,lonh,hlonh)
      ifq = fsflow
      do 20 i=1,5
         ipk(i) = upk(i)
20       continue
      impv = pcimpv
      if (rcid(1).eq.blnk) then
         rcid(1) = cnone
         rcid(2) = cnone
         endif
c
      if (iropth.gt.0.and.hinten(1).gt.10.) iropth = 0
c
      write (iout,30) iqopth,iropth,impv,rcid,ifq,ipk,lath,lonh
30    format (2i2,i4,1x,2a4,1x,6i7,2i4)
c
      if (iqopth.gt.0) then
c     base flow adjustment
         do 40 i=1,5
            itaq(i) = taq(i)
40          continue
         write (iout,50) itaq,qtsid,dtcq,intq
50    format (5i6,1x,2a4,1x,a4,2x,i2)
         endif
c
c  runoff adjustment
      if (iropth.gt.0) write (iout,60) hinten
60    format (5f6.2)
c
c  areas
      do 70 i=1,nars
         jwt(i) = wt(i)*100
70       continue
c  add 'endid' as last id before listing areas
      nars = nars + 1
      jwt(nars) = 0
      arid(1,nars) = 'endi'
      arid(2,nars) = 'd   '
      write (iout,80) (jwt(i),(arid(j,i),j=1,2),i=1,nars)
80    format ((4x,5(i3,1x,2a4)))
c
      return
c
      end
