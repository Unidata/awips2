c
c
	subroutine constant(a,const,mni,ni,nj)
c
c.....	routine to set an array to a constant.
c
	implicit none
	integer mni,ni,nj,i,j
	real a(mni,nj), const
c
	do 1 j=1,nj
	do 1 i=1,ni
	  a(i,j) = const
1	continue
c
	return
	end
