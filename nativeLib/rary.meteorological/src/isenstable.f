c
c
	subroutine isen_stability(p_up,p_low,o_up,o_low,stab,mni,ni,nj)
c
c...............................................................................
c
c	Routine to calculate the isentropic stability through a layer.
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
c	    th_low         R          I    Lower isentrope (K)
c	    stab           RA         O    Stability through the layer (mb/K)
c	    mni            I          I    First dimension of input array.
c	    ni, nj         I          I    Grid dimensions in i,j.
c	    bad            R          -    Bad data value.
c	    flag           R          -    Bad data flag.
c	
c	
c	User Notes:
c
c	1.  Stability is defined as -dP/d(theta).  We calculate this through
c	    the layer from the isentropic surface 'n' to the surface above it,
c	    'n+1'.
c
c...............................................................................
c
	implicit none
	real o_up, o_low, dth, bad, flag
	integer i, j, mni, ni, nj
	real*4 p_up(mni,nj), p_low(mni,nj)
	real*4 stab(mni,nj)
c
	parameter(flag = 1.e37)
	bad = 1.e6 - 2.
c
c.....	Start.  Find the d(theta).
c
	dth = 1. / (o_up - o_low)		! divide here so don't later.
c
c.....	Now calculate the isentropic stability.
c
	do 2 j=1,nj
	do 2 i=1,ni
	  if(p_low(i,j).gt.bad .or. p_up(i,j).gt.bad) then
	    stab(i,j) = flag
	  else
	    stab(i,j) = amax1(p_low(i,j)-p_up(i,j),10.0)*dth
	  endif
2	continue
c
	return
	end
