c
c
	subroutine mult_aray(a,b,result,mni,ni,nj)
c
c..... 	Multiply a field by another.  Each i,j in one array is
c.....	multiplied by the corresponding i,j in the other array.
c.....	result = a*b
c
c.....	Stole framework from add_aray...J Ramer Jun 95
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real a(mni,nj), b(mni,nj), result(mni,nj)
c
	bad = 1e36
	do 1 j=1,nj
	do 1 i=1,ni
	  if(a(i,j).gt.bad .or. b(i,j).gt.bad) then
	     result(i,j) = flag
	  else
	     result(i,j) = a(i,j) * b(i,j)
	  endif
1	continue
c
	return
	end
