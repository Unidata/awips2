c
c
	subroutine add_by_cnst(a,const,result,mni,ni,nj)
c
c..... 	Routine to add an array by a real constant.  Each i,j in the 
c.....	array is added to by the constant. 
c
c.....	Original : 01-13-89		Peter A. Stamus
c		   08-21-89	Add bad data check.
c		   09-20-89	Add implicit none.
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad, const
	parameter(flag = 1.e37)
	real a(mni,nj), result(mni,nj)
c
	bad = 1e36
	do 1 j=1,nj
	do 1 i=1,ni
	  if(a(i,j) .gt. bad) then
	    result(i,j) = flag
	  else
	    result(i,j) = a(i,j) + const
	  endif
1	continue
c
	return
	end
