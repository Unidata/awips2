c  =====================================================================
c  pgm:  subroutine getar (po)
c
c   in: po     .... array containing parameters for areas
c  =====================================================================
c
      subroutine getar (po)
c
c.......................................................................
c
c  This routine fills common block 'ffg_inc/arparm' with area parameters
c  from the po array.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*4 cnone
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodev'
      include 'ffg_inc/arparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getar.f,v $
     . $',                                                             '
     .$Id: getar.f,v 1.2 2004/01/30 17:48:36 scv Exp $
     . $' /
C    ===================================================================
C
      data cnone/ 'none'/
c
c
      call prbug ('getar',1,1,ibug)
c
c  version number
      avers = po(1)
c
c  identifier
      call umemov (po(2),areaid,2)
c
c  type code
      call umemov (po(4),artyp,1)
c
c  description
      call umemov (po(6),adesc,5)
c
c  basin boundary id
      call umemov (po(11),bbid,2)
c
c  centroid lat lon
      if (bbid(1).eq.' '.or.bbid(1).eq.cnone) then
         alat = po(13)
         alon = po(14)
         alath = po(15)
         alonh = po(16)
         endif
c
c  location of ffg values in array
      laffg = po(17)
c
c  duration flag
      kadurf = po(18)
      if (kadurf.lt.0.or.kadurf.gt.2) kadurf = 0
      nadur = kadurf + 3
c
c  area runoffs
      do 10 i=1,5
         aro(i) = po(i+18)
10       continue
c
c  computation control
      if (avers.ge.1.1) then
         iropta = po(24)
         else
            iropta = 0
         endif
c
c  area ffg computation time
      lacpd = po( laffg-1 )
c
c  ffg values
      do 20 i=1,5
         affg(i) = po(laffg+i-1)
20       continue
c
c  number of words used
      iusea = po(5) + 0.01
c
      return
c
      end
