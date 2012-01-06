c
c
	subroutine pres_stability(t_up,t_low,p_up,p_low,stab,mni,ni,nj)
c
c...............................................................................
c
c	Routine to calculate the isobaric stability in an isobaric layer.
c
c	Changes:
c		J. Ramer	10-25-94	Stole from isenstable.for
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	    t_up           RA         I    Theta on upper isobaric sfc (K)
c	    t_low          RA         I    Theta on this isobaric sfc (K)
c	    p_up           R          I    Upper pressure (mb)
c	    p_low          R          I    This (lower) pressure (mb)
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
c	    the layer from the isobaric surface 'n' to the surface above it,
c	    'n+1'.
c
c...............................................................................
c
	implicit none
	real p_up, p_low, bad, flag, dp
	integer i, j, mni, ni, nj
	real*4 t_up(mni,nj), t_low(mni,nj)
	real*4 stab(mni,nj)
c
	parameter(flag = 1.e37)
	bad = 1.e6 - 2.
c
c
c.....	Now calculate the isobaric stability.
c
	dp = p_up - p_low
	do 2 j=1,nj
	do 2 i=1,ni
	  if(t_low(i,j).gt.bad .or. t_up(i,j).gt.bad) then
	    stab(i,j) = flag
	  else
	    stab(i,j) = dp/(t_low(i,j)-t_up(i,j))
	  endif
2	continue
c
	return
	end
