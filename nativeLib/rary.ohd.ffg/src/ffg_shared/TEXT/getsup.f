c  =====================================================================
c  pgm:  getsup (po)
c
c   in: po     .... water supply parameters
c  =====================================================================
c
      subroutine getsup (po)
c
c.......................................................................
c
c  this routine fills common/wsparm with water supply parameters from
c  the po array
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                              Sept 1995
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/wsparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getsup.f,v $
     . $',                                                             '
     .$Id: getsup.f,v 1.3 2004/09/13 14:23:47 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('getsup',1,1,ibug)
c
c  version number
      wvers = po(1)
c
c  identifier
      call umemov (po(2),wsid,2)
c
c  type
      call umemov (po(4),wstyp,1)
c
c  location name
      call umemov (po(6),sdesc,5)
c
c  area
      sarea = po(11)
c
c  water supply boundary identifier
      call umemov (po(12),wsbid,2)
c
c  location of output water supply values
      lro = po(18)
c
c  location of number of areas used to compute water supply value
      loar  = po(19)
c
c  computation date for rainfall-runoff curves
      lwcpd = po( lro-1 )
c
c  reserved for water supply values
      do 10 i=1,6
         wsup(i) = po(lro+i-1)
10       continue
c
c  number of weighted areas
      nsars = po(loar) + 0.01
c
c  area weights and identifiers
      lowa = loar + 1
      do 20 i=1,nsars
          j = (i-1)*3 + lowa
          swt(i) = po(j)
          call umemov (po(j+1),sarid(1,i),2)
20        continue
c
c  number of words
      iuses = po(5) + 0.01
c
      return
c
      end
