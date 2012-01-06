c
c
	subroutine exp_aray(a,b,mni,ni,nj)
c
c..... 	calculates the exponential of a field. b = exp(a)
c
	implicit none
	integer mni, ni, nj, i, j
	real flag
	parameter(flag = 1.e37)
	real a(mni,nj), b(mni,nj)
 
c
	do 1 j=1,nj
	do 1 i=1,ni
	  if(a(i,j) .gt. 85.0) then
	    b(i,j) = flag
	  else if(a(i,j) .lt. -85.0) then
	    b(i,j) = 0.0
	  else
            b(i,j) = exp(b(i,j))
	  endif
1	continue
c
	return
	end
