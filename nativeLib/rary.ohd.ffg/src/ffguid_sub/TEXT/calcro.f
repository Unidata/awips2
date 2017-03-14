c  =====================================================================
c  pgm:  calcro (jdur,arry,nfill,tro,ffg)
c
c   in: jdur   .... duration sequence (1=1 hr, 2=3 hr, 3=6 hr, 4=12 hr,
c                    5=24 hr)
c   in: arry   .... rainfall runoff curves for all durations
c   in: nfill  .... number of words in array arry
c  out: tro    .... threshold runoff
c   in: ffg    .... computer flash flood guidance
c  =====================================================================
      subroutine calcro(jdur,arry,nfill,tro,ffg)
c.......................................................................
c  rainfall-runoff curves for 1-, 3-, and 6-hr durations in positions
c  18 to 41.  12- and 24-hr duration curves in positions 42 to 57.
c  select curve for appropriate duration
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      include 'ffg_inc/iuws'
      dimension arry(nfill),rr(8)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/calcro.f,v $
     . $',                                                             '
     .$Id: calcro.f,v 1.1 2001/08/16 17:42:57 dws Exp $
     . $' /
C    ===================================================================
C
c
c  number of rainfall-runoff values for each duration
      numv = 8
c
      l = 17 + numv*(jdur-1)
      do 10 j=1,numv
   10 rr(j) = arry(j+l)
c
c  linear interpolation and extrapolation
      iconv = 0
      call linint(iconv,numv,rr,ffg,tro)
      return
      end
