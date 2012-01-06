c
c
	subroutine derived_icing(t,rh,icg,mni,ni,nj)
c
c..............................................................................
c
c	Routine to calculate the wind speed from the wind components.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   t               RA         I    Temperature (K).
c	   rh              RA         I    Relative humidity (%).
c	   icg             RA         O    Icing parameter.
c	   mni             I          I    First dimension of input array.
c	   ni, nj          I          I    Grid dimensions in x, y.
c	   flag            R          -    Bad data flag.
c	   bad             R          -    Bad data value.
c
c	User Notes:
c
c	   1.  Units are not changed (output units = input units).
c
c..............................................................................
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real t(mni,nj), rh(mni,nj), icg(mni,nj)
c
c.....	Compute the derived icing value at each point.
c
	bad = 1.e6 - 2.
	do 1 j=1,nj
	do 1 i=1,ni
	  if(t(i,j).gt.bad .or. rh(i,j).gt.bad) then
	    icg(i,j) = flag
	  else if (t(i,j).gt.267.43572) Then
	    icg(i,j) = amin1( (275.15-t(i,j))/2, (rh(i,j)-35.)/15)
	  else
	    icg(i,j) = amin1( (t(i,j)-248.15)/5, (rh(i,j)-35.)/15)
	  endif
1	continue

	return
	end
