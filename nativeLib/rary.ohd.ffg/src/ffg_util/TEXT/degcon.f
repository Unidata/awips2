c  =====================================================================
c  pgm:  degcon (icv,idm,ddeg)
c
c   in: icv    .... conversion control 
c  i/o: idm    .... degrees and minutes as single integer
c  o/i: ddeg   .... decimal degrees
c  =====================================================================
      subroutine degcon (icv,idm,ddeg)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/degcon.f,v $
     . $',                                                             '
     .$Id: degcon.f,v 1.1 2001/08/16 17:42:47 dws Exp $
     . $' /
C    ===================================================================
C
c.......................................................................
c  converts degrees and minutes
c
c      icv controls the function of the subroutine
c         = 1 single integer deg min to decimal degrees (idm to ddeg)
c         = 2 decimal degrees to single integer deg min (ddeg to idm)
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      if (icv.eq.1) then
c  integer deg min to decimal degrees
	 ideg = idm/100
	 decm = (idm-ideg*100)/60.
	 ddeg = ideg + decm
      else if (icv.eq.2) then
c  decimal degrees to integer deg min
	 ideg = ddeg
	 rmin = (ddeg-ideg)*60.
	 imin = rmin + 0.1
	 idm = ideg*100 + imin
      else
      end if
      return
      end
c=======================================================================
