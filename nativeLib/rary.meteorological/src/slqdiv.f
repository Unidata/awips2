c
c
	subroutine slqdiv(z,t,p,dx,dy,coriolis,mni,ni,nj,slqd,
     &                    slqx,slqy,w1,w2,w3,w4,w5)
c
c...............................................................................
c
c	Routine to calculate the divergence of the Q vector on a single level 
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
c	    slqd           RA         O    Divergence of Q at this level.
c	    slqx...w5      RA         I    Other work arrays.
c	
c	User Notes:
c
c...............................................................................
c
	implicit none
	integer mni, ni, nj
	real p
	real*4 z(mni,nj), t(mni,nj), coriolis(mni,nj)
	real*4 dx(mni,nj), dy(mni,nj)
	real*4 w1(mni,nj), w2(mni,nj)
	real*4 w3(mni,nj), w4(mni,nj), w5(mni,nj)
	real*4 slqx(mni,nj), slqy(mni,nj)
	real*4 slqd(mni,nj)
c
c.....	Compute the single level Q-vector components for this level.
c
	call slqvect(z,t,p,dx,dy,coriolis,mni,ni,nj,slqx,slqy,
     &               w1,w2,w3,w4,w5,slqd)

c
c.....	Compute the divergence of the Q-vector. (coriolis just place holder)
c
        call G2Gkinematics(slqx,slqy,coriolis,dx,dy,
     &                     mni,nj,ni,nj,2,slqd)

	return
	end
