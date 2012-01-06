
c
c
	subroutine pvpres(t_up,t_low,p_up,p_low,pvort,mni,ni,nj,
     &                    u_up,v_up,u_low,v_low,avort1,avort2,
     &                    dtdx1,dtdy1,dtdx2,dtdy2,
     &                    dx,dy,coriolis)
c
c..............................................................................
c
c	Routine to calculate the isobaric potential vorticity through a layer.
c
c	Changes:
c		J. Ramer	10-25-94	Stole from pv.for
c		J. Ramer	02-25-02	Generalized for grids of p
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	    t_up           RA         I    Theta on upper isobaric sfc (K)
c	    t_low          RA         I    Theta on this isobaric sfc (K)
c	    p_up           RA         I    Upper pressure (mb)
c	    p_low          RA         I    This (lower) pressure (mb)
c	    u_up, v_up     RA         I    U, V winds on upper surface (m/s)
c	    u_low, v_low   RA         I    U, V winds on lower surface (m/s)
c	    pvort          RA         O    Potential vorticity (K/mb/s)
c	    mni            I          I    First dimension of input array.
c	    ni, nj         I          I    Grid dimensions in i,j.
c	    avort1, avort2 RA         I    Work arrays for abs. vorticity
c	    dtdx1,dtdy1    RA         I    Work arrays for theta gradients
c	    dtdx2,dtdy2    RA         I    Work arrays for theta gradients
c	    dx, dy         RA         I    Grid interval (meters) in x,y dir.
c	    coriolis       RA         I    Coriolis parameters (/s)
c	
c	
c	User Notes:
c
c	1.  Stability is defined as -dP/d(theta).  We calculate this through
c	    the layer from the isobaric surface 'n' to the surface above it,
c	    'n+1'.
c
c	2.  Since we are dealing with a layer, we calculate a mean absolute
c	    vorticity using the winds at the upper and lower layers.
c
c	3.  The PV is then [mean abs. vort.]/[stability] + theta->pres term
c
c..............................................................................
c
	implicit none
	real p_up(mni,nj), p_low(mni,nj), bad, flag, dp, dt
        real du, dv, dtdx, dtdy, av
	integer i, j, mni, ni, nj
	real*4 t_up(mni,nj), t_low(mni,nj)
	real*4 pvort(mni,nj)
	real u_up(mni,nj), v_up(mni,nj), avort1(mni,nj)
	real u_low(mni,nj), v_low(mni,nj), avort2(mni,nj)
        real dtdx1(mni,nj), dtdy1(mni,nj)
        real dtdx2(mni,nj), dtdy2(mni,nj)
	real dx(mni,nj), dy(mni,nj), coriolis(mni,nj)
c
	parameter(flag = 1.e37)
	bad = 1.e6 - 2.
c
c.....	Calculate the absolute vorticity at each isobaric surface.
c
        call G2Gkinematics(u_up,v_up,coriolis,dx,dy,
     &                     mni,nj,ni,nj,1,avort1)
        call G2Gkinematics(u_low,v_low,coriolis,dx,dy,
     &                     mni,nj,ni,nj,1,avort2)
c
c.....	Calculate the temperature gradient on each surface
c
        call G2Gkinematics(dtdx2,dtdy2,coriolis,dx,dy,
     &                     mni,nj,ni,nj,7,t_up)
        call G2Gkinematics(dtdx1,dtdy1,coriolis,dx,dy,
     &                     mni,nj,ni,nj,7,t_low)
c
c.....	Now calculate the isobaric potential vorticity.
c
	do 1 j=1,nj
	do 1 i=1,ni
	  if (p_up(i,j).gt.bad .or. p_low(i,j).gt.bad .or.
     &        t_up(i,j).gt.bad .or. t_low(i,j).gt.bad .or.
     &        avort1(i,j).gt.bad .or. avort2(i,j).gt.bad .or.
     &        dtdx1(i,j).gt.bad .or. dtdy1(i,j).gt.bad .or.
     &        dtdx2(i,j).gt.bad .or. dtdy2(i,j).gt.bad) then
	    pvort(i,j) = flag
	  else
            dp = p_up(i,j)-p_low(i,j)
            dt = t_up(i,j)-t_low(i,j)
            du = u_up(i,j)-u_low(i,j)
            dv = v_up(i,j)-v_low(i,j)
            dtdx = dtdx1(i,j)+dtdx2(i,j)
            dtdy = dtdy1(i,j)+dtdy2(i,j)
            av = avort1(i,j)+avort2(i,j)
	    pvort(i,j) = -0.5 * ( av*dt + (du*dtdy - dv*dtdx)  ) / dp
	  endif
1	continue
c
	return
	end
