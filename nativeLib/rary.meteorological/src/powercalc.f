c
c
	subroutine powercalc(a,b,result,mni,ni,nj)
c
c..... 	Raise each item in the field a to the power in field b.
c.....	result = a**b
c
c.....	J Ramer Jun 95
c
	implicit none
      include 'IntrinsicFunctions.inc'
c declare formal arguments
	integer mni, ni, nj
	real a(mni,nj), b(mni,nj), result(mni,nj)
c
	integer i, j, bb
	real flag, bad
	parameter(flag = 1.e37)
c
	bad = 1e10
	do 1 j=1,nj
	do 1 i=1,ni
	  if(a(i,j).gt.bad .or. b(i,j).gt.bad) then
	     result(i,j) = flag
	  else if (a(i,j).eq.0.0) then
             if (b(i,j).lt.0.0) then
                 result(i,j) = flag
             else
	         result(i,j) = 0.0
             endif
	  else if (b(i,j).eq.0.0) then
	     result(i,j) = 1.0
	  else if(a(i,j).gt.0.0) then
	     result(i,j) = a(i,j) ** b(i,j)
	  else
             bb = jint(b(i,j))
             if (bb.ne.b(i,j)) then
                 result(i,j) = flag
             else
	         result(i,j) = a(i,j) ** bb
             endif
	  endif
1	continue
c
	return
	end
