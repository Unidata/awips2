c
c
	subroutine pvadv(p_up,p_low,o_up,o_low,pva,pvort,
     &                   mni,ni,nj,u_up,v_up,u_low,v_low,
     &                   dx,dy,coriolis,u,v)
c
c...............................................................................
c
c	Routine to calculate the isentropic potential vorticity advection 
c	through a layer.
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
c	    pva            RA         O    Potential vorticity advect.(K/mb/s2)
c	    pvort          RA         I    Work array for PV (K/mb/s)
c	    mni            I          I    First dimension of input array.
c	    ni, nj         I          I    Grid dimensions in i,j.
c	    coriolis       RA         I    Coriolis parameters (/s)
c	    u, v           RA         I    Work arrays for mean winds (m/s)
c	
c	
c	User Notes:
c
c	1.  Stability is defined as -dP/d(theta).  We calculate this through
c	    the layer from the isentropic surface 'n' to the surface above it,
c	    'n+1'.
c
c	2.  Since we are dealing with a layer, we calculate a mean absolute
c	    vorticity using the winds at the upper and lower layers.  The PV 
c	    is then [mean abs. vort.]/[stability].
c
c	3.  The advection is calculated using a mean wind through the layer.
c
c...............................................................................
c
	implicit none
	real o_up, o_low, bad, flag
	integer i, j, mni, ni, nj
	real*4 p_up(mni,nj), p_low(mni,nj)
	real*4 pvort(mni,nj), pva(mni,nj)
	real u_up(mni,nj), v_up(mni,nj)
	real u_low(mni,nj), v_low(mni,nj)
	real dx(mni,nj), dy(mni,nj), coriolis(mni,nj)
	real u(mni,nj), v(mni,nj)
c
	parameter(flag = 1.e37)
	bad = 1.e6 - 2.
c
c.....	Start.  First calculate the potential vorticity through the layer,
c....   use u and v as work arrays.
	call calcpv(p_up,p_low,o_up,o_low,pvort,mni,ni,nj,
     &          u_up,v_up,u_low,v_low,u,v,dx,dy,coriolis)
c
c.....	Now calculate a mean wind through the layer.
c
	do 1 j=1,nj
	do 1 i=1,ni
	  if(u_up(i,j).gt.bad .or. u_low(i,j).gt.bad) then
	    u(i,j) = flag
	  else 
	    u(i,j) = (u_up(i,j) + u_low(i,j)) * 0.5	! mean u wind
	  endif	
	  if(v_up(i,j).gt.bad .or. v_low(i,j).gt.bad) then
	    v(i,j) = flag
	  else 
	    v(i,j) = (v_up(i,j) + v_low(i,j)) * 0.5	! mean v wind
	  endif
1	continue
c
c.....	Now calculate the potential vorticity advection.
c
        call g2gkinematics(u,v,pvort,dx,dy,mni,nj,ni,nj,5,pva)
c
	return
	end
