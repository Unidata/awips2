c
c
	subroutine calcpv(p_up,p_low,o_up,o_low,pvort,mni,ni,nj,
     &                    u_up,v_up,u_low,v_low,avort1,avort2,
     &                    dx,dy,coriolis)
c
c...............................................................................
c
c    Routine to calculate the isentropic potential vorticity through a layer.
c
c	Changes:
c		P.A. Stamus	01-30-90	Original 
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	    p_up           RA         I    Pressure on upper isentrope (mb)
c	    p_low          RA         I    Pressure on this isentrope (mb)
c	    th_up          R          I    Upper isentrope (K)
c	    th_low         R          I    This (lower) isentrope (K)
c	    u_up, v_up     RA         I    U, V winds on upper isentrope (m/s)
c	    u_low, v_low   RA         I    U, V winds on lower isentrope (m/s)
c	    dx, dy         RA         I    Grid interval (meters) in x,y dir.
c	    pvort          RA         O    Potential vorticity (K/mb/s)
c	    mni            I          I    First dimension of input array.
c	    ni, nj         I          I    Grid dimensions in i,j.
c	    avort1, avort2 RA         I    Work arrays for abs. vorticity (/s)
c	    coriolis       RA         I    Coriolis parameters (/s)
c	
c	
c	User Notes:
c
c	1.  Stability is defined as -dP/d(theta).  We calculate this through
c	    the layer from the isentropic surface 'n' to the surface above it,
c	    'n+1'.
c
c	2.  Since we are dealing with a layer, we calculate a mean absolute
c	    vorticity using the winds at the upper and lower layers.
c
c	3.  The PV is then [mean abs. vort.]/[stability]
c
c...............................................................................
c
	implicit none
	real o_up, o_low, bad, flag
	integer i, j, mni, ni, nj
	real*4 p_up(mni,nj), p_low(mni,nj)
	real*4 pvort(mni,nj)
	real u_up(mni,nj), v_up(mni,nj), avort1(mni,nj)
	real u_low(mni,nj), v_low(mni,nj), avort2(mni,nj)
	real dx(mni,nj), dy(mni,nj), coriolis(mni,nj)
c
	parameter(flag = 1.e37)
	bad = 1.e6 - 2.
c
c.....	Calculate the absolute vorticity at each isentropic surface.
c
        call G2Gkinematics(u_up,v_up,coriolis,dx,dy,
     &                     mni,nj,ni,nj,1,avort1)
        call G2Gkinematics(u_low,v_low,coriolis,dx,dy,
     &                     mni,nj,ni,nj,1,avort2)
c
c.....	Calculate the isentropic stability through the layer. (hold in pvort)
c
	call isen_stability(p_up,p_low,o_up,o_low,pvort,mni,ni,nj)
c
c.....	Now calculate the isentropic potential vorticity.
c
	do 1 j=1,nj
	do 1 i=1,ni
	  if (pvort(i,j).gt.bad .or.
     &        avort1(i,j).gt.bad .or. avort2(i,j).gt.bad) then
	    pvort(i,j) = flag
	  else
	    pvort(i,j) = (avort1(i,j) + avort2(i,j)) * 0.5 / pvort(i,j)
	  endif
1	continue
c
	return
	end
