c
c
	subroutine slfront(z,t,p,dx,dy,coriolis,mni,ni,nj,fgen,
     &                     slqx,slqy,w1,w2,w3,dtdx,dtdy)
c
c..............................................................................
c
c	Routine to calculate the QG frontogenesis function on a single level 
c	using just that level's data.
c
c	Changes:
c		P.A. Stamus	01-17-90	Original
c				08-24-90	Add array 
c						for smoothing in slqvect.
c               J. Ramer        08-27-90        Streamlined.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	    z              RA         I    Height field for the level (meters).
c	    t              RA         I    Temperature field for the level (K).
c	    p              R          I    Pressure of level (mb).
c	    dx, dy         RA         I    Grid interval in x, y (meters).
c	    coriolis       RA         I    Coriolis parameter (/sec).
c	    mni            I          I    First dimension of input array.
c	    ni, nj         I          I    Grid dimension in x, y.
c	    fgen           RA         O    Frontogenis function at this level.
c	    slqx...dtdy    RA         I    Other work arrays.
c	
c	User Notes:
c
c..............................................................................
c
	implicit none
	integer mni, ni, nj, i, j
	real p, flag, t2th, bad 
	real*4 z(mni,nj), t(mni,nj), coriolis(mni,nj)
	real*4 dx(mni,nj), dy(mni,nj)
	real*4 w1(mni,nj), w2(mni,nj)
	real*4 w3(mni,nj), dtdx(mni,nj), dtdy(mni,nj)
	real*4 slqx(mni,nj), slqy(mni,nj)
	real*4 fgen(mni,nj)
c
	parameter(flag = 1.e37)
	bad = 1.e6 - 2.
c
c.....	Compute the single level Q-vector components for this level.
c
	call slqvect(z,t,p,dx,dy,coriolis,mni,ni,nj,slqx,slqy,
     &               w1,w2,w3,fgen,dtdx,dtdy)

c.....	Compute the temperature to potential temperature ratio for this level.
        t2th=2*(1000.0/p)**0.286

c
c.....	Now calculate the QG frontogensis function for this level.
c.....		Fgen = 2(Qx * d(theta)/dx + Qy * d(theta)/dy)
c
	do 1 j=1,nj
	do 1 i=1,ni
	  if(slqx(i,j).gt.bad .or. slqy(i,j).gt.bad .or. 
     &       dtdx(i,j).gt.bad .or. dtdy(i,j).gt.bad) then
	    fgen(i,j) = flag
	  else
	    fgen(i,j) = (slqx(i,j)*dtdx(i,j) + slqy(i,j)*dtdy(i,j))
     &                   * t2th 
	  endif
1	continue
c
	return
	end
