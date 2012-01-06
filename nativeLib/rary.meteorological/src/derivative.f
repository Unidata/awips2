c
c
	subroutine derivative(a1,a2,b1,b2,result,mni,ni,nj)
c
c..... 	calculate the derivative of a with respect to b.
c
c.....	J Ramer Jun 95
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real a1(mni,nj), a2(mni,nj), b1(mni,nj), b2(mni,nj)
        real result(mni,nj)
c
	bad = 1e36
	do 1 j=1,nj
	do 1 i=1,ni
	  if(a1(i,j).gt.bad .or. b1(i,j).gt.bad .or.
     &       a2(i,j).gt.bad .or. b2(i,j).gt.bad .or.
     &       b1(i,j).eq.b2(i,j)) then
	     result(i,j) = flag
	  else
	     result(i,j) = (a2(i,j)-a1(i,j))/(b2(i,j)-b1(i,j))
	  endif
1	continue
c
	return
	end
