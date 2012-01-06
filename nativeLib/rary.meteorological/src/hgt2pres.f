c
c
	subroutine hgt2pres(z,p,mni,ni,nj)
c
c..... 	Routine to calculate pressure from height based on a standard
c.....  atmosphere 
c
c
	implicit none
	integer mni, ni, nj, i, j
	real flag, bad
	parameter(flag = 1.e37)
	real z(mni,nj), p(mni,nj)
 
        Real*4 T0,gamma,p0,p11,z11,c1,c2
 
        Data T0,gamma,p0/288.,.0065,1013.2/
        Data c1,c2/5.256,14600./
        Data z11,p11/11000.,226.0971/
 
c
	bad = 1.e6 - 2.
	do 1 j=1,nj
	do 1 i=1,ni
	  if(z(i,j) .gt. bad) then
	    p(i,j) = flag
	  else if (z(i,j).lt.z11) then
            p(i,j)=p0*((T0-gamma*z(i,j))/T0)**c1
          else
	    p(i,j) = p11*10.**((z11-z(i,j))/c2)
	  endif
1	continue
c
	return
	end
