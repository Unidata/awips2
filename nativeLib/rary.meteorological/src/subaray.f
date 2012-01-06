c
c
	subroutine sub_aray(a,b,result,mni,ni,nj)
c
c..... 	Routine to take the difference of two fields.  Each i,j of array 'b'
c.....	is subtracted from the coresponding i,j of array 'a'.
c
c.....	Original : 01-13-89		Peter A. Stamus
c		   08-11-89	Added bad data check - Adler fix.
c		   09-20-89	Add implicit none.
c
	implicit none
	integer mni, ni, nj, i, j
	real bad, flag
	parameter(flag = 1.e37)
	real a(mni,nj), b(mni,nj), result(mni,nj)
c
	bad = 1e36
	do 1 j=1,nj
	do 1 i=1,ni
	  if(a(i,j).gt.bad .or. b(i,j).gt.bad) then
	     result(i,j)=flag
	  else
	    result(i,j) = a(i,j) - b(i,j)
	  endif
1	continue
c
	return
	end
