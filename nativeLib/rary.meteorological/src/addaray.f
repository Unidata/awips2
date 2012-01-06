c
c
	subroutine add_aray(a,b,result,mni,ni,nj,mode)
c
c..... 	Routine to find the sum of two fields.  Each i,j in one array is
c.....	added to the corresponding i,j in the other array.
c
c.....	Original : 01-13-89		Peter A. Stamus
c		   08-14-89	Add bad data check - Adler fix.
c		   09-20-89	Add implicit none.
c		   11-15-01     Non-zero mode means flag treated as zero.
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real a(mni,nj), b(mni,nj), result(mni,nj)
        integer mode
c
	bad = 1.e36

        if (mode.eq.0) Then
	    do 1 j=1,nj
	    do 1 i=1,ni
	      if(a(i,j).gt.bad .or. b(i,j).gt.bad) then
	         result(i,j) = flag
	      else
	         result(i,j) = a(i,j) + b(i,j)
	      endif
1	    continue
        Else
	    do 2 j=1,nj
	    do 2 i=1,ni
	      if (a(i,j).gt.bad .and. b(i,j).gt.bad) then
	         result(i,j) = 0
              else if (a(i,j).gt.bad) then
	         result(i,j) = b(i,j)
              else if (b(i,j).gt.bad) then
	         result(i,j) = a(i,j)
	      else
	         result(i,j) = a(i,j) + b(i,j)
	      endif
2	    continue
        Endif

        Return
	end
