c
c
	subroutine winddir(u,v,ff,mni,ni,nj)
c
c...............................................................................
c
c	Routine to calculate the wind direction from the wind components.
c
c	Changes:
c		stole framework from windspeed, J Ramer, May 2003.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   u, v            RA         I    Wind components.
c	   mni             I          I    First dimension of input array.
c	   ni, nj          I          I    Grid dimensions in x, y.
c	   ff              RA         O    Wind direction (deg).
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
	real flag, bad, DPR
	parameter(flag = 1.e37, DPR=57.29578)
	real u(mni,nj), v(mni,nj), ff(mni,nj)
c
c.....	Compute the wind speed at each point.
c
	bad = 1e10
	do 1 j=1,nj
	do 1 i=1,ni
	  if(u(i,j).gt.bad .or. v(i,j).gt.bad) then
	    ff(i,j) = flag
	  else if (u(i,j).eq.0 .and. v(i,j).eq.0) then
	    ff(i,j) = 0.0
	  else
	    ff(i,j) = DPR*atan2(-u(i,j),-v(i,j))
            if (ff(i,j).lt.0) ff(i,j)=ff(i,j)+360
	  endif
1	continue
c
	return
	end
