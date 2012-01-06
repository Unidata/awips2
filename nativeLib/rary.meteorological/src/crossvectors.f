c
c
	subroutine crossVectors(aX,aY,bX,bY,result,mni,ni,nj)
c
c..... 	Cross a field of vectors by another.  Each i,j in one array of vectors
c.....	is crossed with the corresponding i,j in the other array of vectors.
c
c.....	J Ramer Jun 95
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real aX(mni,nj), aY(mni,nj), bX(mni,nj), bY(mni,nj)
	real  result(mni,nj)
c
	bad = 1e36
	do 1 j=1,nj
	do 1 i=1,ni
	if (aX(i,j).gt.bad .or. bX(i,j).gt.bad .or.
     &      aY(i,j).gt.bad .or. bY(i,j).gt.bad) then
	     result(i,j) = flag
	  else
	     result(i,j) = aX(i,j) * bY(i,j) - aY(i,j) * bX(i,j) 
	  endif
1	continue
c
	return
	end
