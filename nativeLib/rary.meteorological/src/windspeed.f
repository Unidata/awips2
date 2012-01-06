c
c
	subroutine windspeed(u,v,ff,mni,ni,nj)
c
c...............................................................................
c
c	Routine to calculate the wind speed from the wind components.
c
c	Changes:
c		P.A. Stamus	01-11-89	Original (based on Baker's)
c				07-20-89	Removed conversion to kts.
c				08-21-89	Bad data check.
c				09-20-89	Add implicit none.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   u, v            RA         I    Wind components (m/sec).
c	   mni             I          I    First dimension of input array.
c	   ni, nj          I          I    Grid dimensions in x, y.
c	   ff              RA         O    Wind speed (m/sec).
c	   flag            R          -    Bad data flag.
c	   bad             R          -    Bad data value.
c
c	User Notes:
c
c	   1.  Units are not changed (output units = input units).
c
c...............................................................................
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real u(mni,nj), v(mni,nj), ff(mni,nj)
c
c.....	Compute the wind speed at each point.
c
	bad = 1e10
	do 1 j=1,nj
	do 1 i=1,ni
	  if(u(i,j).gt.bad .or. v(i,j).gt.bad) then
	    ff(i,j) = flag
	  else
	    ff(i,j) = sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
	  endif
1	continue
c
	return
	end
