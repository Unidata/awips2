c
c
	subroutine max_min(a,b,result,mni,ni,nj,mode)
c
c..... 	Routine to find the max or min of two fields.  mode>0 means max,
c.....  mode<0 means min.
c
c.....	Original : 06-25-02	Borrowed from addaray
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real a(mni,nj), b(mni,nj), result(mni,nj)
        integer mode
c
	bad = 1.e36
        if (mode.gt.0) Then
	    do 1 j=1,nj
	    do 1 i=1,ni
	      if (a(i,j).gt.bad) then
	         result(i,j) = b(i,j)
	      else if (b(i,j).gt.bad) then
	         result(i,j) = a(i,j)
	      else if (a(i,j).gt.b(i,j)) then
	         result(i,j) = a(i,j)
              else
	         result(i,j) = b(i,j)
	      endif
1	    continue
        Else
	    do 2 j=1,nj
	    do 2 i=1,ni
	      if (a(i,j).lt.b(i,j)) then
	         result(i,j) = a(i,j)
              else
	         result(i,j) = b(i,j)
	      endif
2	    continue
        Endif

        Return
	end
