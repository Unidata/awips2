c
c
	subroutine natlog(a,b,mni,ni,nj)
c
c..... 	calculates the natural log of a field. b = ln(a)
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real a(mni,nj), b(mni,nj)
 
c
	bad = 1e36
	do 1 j=1,nj
	do 1 i=1,ni
	  if(a(i,j) .gt. bad .or. a(i,j).le.0.0) then
	    b(i,j) = flag
	  else
            b(i,j) = alog(a(i,j))
	  endif
1	continue
c
	return
	end
