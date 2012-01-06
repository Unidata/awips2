c
c
	subroutine slqvect(z,t,p,dx,dy,coriolis,mni,ni,nj,slqx,slqy,
     &                     dugdx,dugdy,dvgdx,dvgdy,dtdx,dtdy)
c
c..............................................................................
c
c	Routine to calculate the Q vector on a single level using just that
c	level's data.
c
c	Changes:
c		P.A. Stamus	01-17-90	Original
c				08-24-90	Add smoothing to geo wind comps.
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
c	    ni, nj         I          I    Grid dimension in x, y.
c	    slqx, slqy     RA         O    Q-vector components for this level.
c	    dugdx...dtdy   RA         I    Work arrays.
c	
c	User Notes:
c
c..............................................................................
c
      implicit none
c declare formal parameters
      integer  mni, ni, nj
      real*4   z(mni,nj), t(mni,nj)
      real     p
      real*4   dx(mni,nj), dy(mni,nj), coriolis(mni,nj)
      real*4   slqx(mni,nj), slqy(mni,nj)
      real*4   dugdx(mni,nj), dugdy(mni,nj)
      real*4   dvgdx(mni,nj), dvgdy(mni,nj)
      real*4   dtdx(mni,nj), dtdy(mni,nj)      
c
	integer i, j
	real    flag, bad
	parameter(flag = 1.e37)
        real*4  t2th
c
        Integer*4 passes
        Real*4    smoothness
        Common   /qsmthcmn/passes,smoothness

	Data bad/99998./
c
c.....	Compute the temperature to potential temperature ratio for this level.
        t2th=(1000.0/p)**0.286
c
c.....	Smooth heights, compute the geostrophic wind components for this level.
c....   (store components in slqx and slqy)
        Do i=1,passes/2
           call smooth(z,slqx,mni,ni,nj,smoothness)
           call smooth(slqx,z,mni,ni,nj,smoothness)
        EndDo
        if (iand(passes,1).eq.1) then
            call smooth(z,slqx,mni,ni,nj,smoothness)
            call DgeoComps(slqx,coriolis,dx,dy,mni,nj,ni,nj,
     &                     dugdx,dugdy,dvgdx,dvgdy)
        else
            call DgeoComps(z,coriolis,dx,dy,mni,nj,ni,nj,
     &                     dugdx,dugdy,dvgdx,dvgdy)
        end if

c... Compute gradients of geostrophic wind. (coriolis just place holder)
c        call G2Gkinematics(dugdx,dugdy,coriolis,dx,dy,mni,nj,ni,nj,7,slqx)
c        call G2Gkinematics(dvgdx,dvgdy,coriolis,dx,dy,mni,nj,ni,nj,7,slqy)

c... Smooth temps, then compute dT/dx and dT/dy.  (coriolis just place holder)
        Do i=1,passes/2
           call smooth(t,slqy,mni,ni,nj,smoothness)
           call smooth(slqy,t,mni,ni,nj,smoothness)
        EndDo
        if (iand(passes,1).eq.1) then
            call smooth(t,slqy,mni,ni,nj,smoothness)
            call G2Gkinematics(dtdx,dtdy,coriolis,dx,dy,
     &                         mni,nj,ni,nj,7,slqy)
        else
            call G2Gkinematics(dtdx,dtdy,coriolis,dx,dy,
     &                         mni,nj,ni,nj,7,t)
        endif
c
c.....	  Now calculate the x-component of Q.
c.....		Qx = -d(ug)/dx * d(theta)/dx - d(vg)/dx * d(theta)/dy
c.....		Qy = -d(ug)/dy * d(theta)/dx - d(vg)/dy * d(theta)/dy
c
	do 1 j=1,nj
	do 1 i=1,ni
          if (dugdx(i,j).gt.bad .or. dugdy(i,j).gt.bad .or.
     &        dvgdx(i,j).gt.bad .or. dvgdy(i,j).gt.bad .or.
     &        dtdx(i,j).gt.bad .or. dtdy(i,j).gt.bad) Then
	    slqx(i,j) = flag
	    slqy(i,j) = flag
	  else
	    slqx(i,j) = (-t2th)*
     &                  (dugdx(i,j)*dtdx(i,j)+dvgdx(i,j)*dtdy(i,j))
	    slqy(i,j) = (-t2th)*
     &                  (dugdy(i,j)*dtdx(i,j)+dvgdy(i,j)*dtdy(i,j))
	  endif
1	continue

	return
	end
