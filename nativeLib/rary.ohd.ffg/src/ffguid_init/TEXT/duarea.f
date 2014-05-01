c  ====================================================================
c  pgm:  duarea (po,iout)
c
c   in: po     .... array of parameters
c   in: iout   .... output device
c  ====================================================================
c
      subroutine duarea (po,iout)
c
c......................................................................
c
c  This subroutine outputs parameters that define areas to a file
c  in the same format that can be used to define areas.
c
c.......................................................................
c      Initially written by
c           Tim Sweeney, HRL                           May 1992
c
c   Added area runoffs
c           Tim Sweeney, HRL                           Dec 1997
c
c   Added computation control
c           Tim Sweeney, HRL                           Mar 1999
c.......................................................................
c
      character*4 cnone/'none'/
      character*22 wdesc
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/arparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/duarea.f,v $
     . $',                                                             '
     .$Id: duarea.f,v 1.2 2004/01/30 17:46:43 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('duarea',1,1,ibug)
c
c  get parameters
      call getar (po)
c
c  check for blank values
      if (adesc(1).eq.' ') adesc(1) = cnone
      if (bbid(1).eq.' ') bbid(1) = cnone
c
c  add delimiters around description if needed
      call adelim (adesc,5,lw,wdesc)
c
      if (ibug.eq.1) then
         write (iud,10) lw,wdesc(1:lw)
10    format (' lw=',i4,4x,'wdesc=',a)
         write (iud,20) bbid
20    format (' bbid=',2a4)
         endif
c
      write (iout,30) artyp,areaid,wdesc(1:lw),iropta,kadurf,bbid,aro
30    format (a4,1x,2a4,1x,a,1x,i1,1x,i1,1x,2a4,5f6.2)
c
      if (bbid(1).eq.'box '.or.bbid(1).eq.'BOX ') then
c     output box that approximates boundary for area
c     convert latitude/longitude to degrees and minutes
         icv = 2
         call degcon (icv,lat,alat)
         call degcon (icv,lon,alon)
         call degcon (icv,lath,alath)
         call degcon (icv,lonh,alonh)
         latd = lat/100
         latm = lat - latd*100
         lond = lon/100
         lonm = lon - lond*100
         write (iout,40) latd,latm,lond,lonm,lath,lonh
40    format (4x, 2(1x,i2), 1x,i3, 3(1x,i2) )
         endif
c
      return
c
      end
