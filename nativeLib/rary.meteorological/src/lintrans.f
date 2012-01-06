c
c
	subroutine LinTrans(a,mult,add,result,mni,ni,nj)
c
c..... 	Routine to do a linear translation on an array. Each i,j in the 
c.....	array is multiplied by 'mult' and then added to by 'add'. 
c
c.....	Original : 01-13-89		Peter A. Stamus
c		   08-21-89	Add bad data check.
c		   09-20-89	Add implicit none.
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad, mult, add
	parameter(flag = 1.e37)
	real a(mni,nj), result(mni,nj)
c
	bad = 1e36

	do 1 j=1,nj
	do 1 i=1,ni
	  if(a(i,j) .gt. bad) then
	    result(i,j) = flag
	  else
	    result(i,j) = a(i,j) * mult + add
	  endif
1	continue
c
	return
	end
