c
c
	subroutine ver_pts(inp,count,init,mni,ni,nj)
c
c This routine adds one to each point in count for each point in
c inp that is not marked undefined.  If init is non zero, will
c start with count of zero.
c
	implicit none
	integer init,mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real inp(mni,nj), count(mni,nj)
c
	bad = 1.e36
        if (init.ne.0) then

	do 1 j=1,nj
	do 1 i=1,ni
	  if(inp(i,j).gt.bad) then
	     count(i,j) = 0
	  else
	     count(i,j) = 1
	  endif
1	continue

        else

	do 2 j=1,nj
	do 2 i=1,ni
	  if(inp(i,j).lt.bad) count(i,j) = count(i,j) + 1
2	continue

        endif
c
	return
	end
