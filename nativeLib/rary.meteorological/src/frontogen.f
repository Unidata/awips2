c
c
	subroutine frontogen(zmid,ztop,zbot,ptop,pbot,mni,ni,nj,dx,dy,f,
     &                       w1,w2,w3,dtdx,dtdy,qx,qy,fgen)
c
C###############################################################################
C Statement of purpose.
C ---------------------
C This routine computes the QG frontogenesis function through a specified
C pressure layer.
C
C History.
C --------                    
C D. Baker       01 Oct 86    Original version based on equations given
C                             in Barnes (????).
C P. Stamus      30 Aug 89    Second part from Baker's original.
c		 20 Sep 89    Add implicit none.
C 
C E. Thaler      22 Aug 91    Added 2 coefficient to fgen calculation
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
c mni         Integer       First dimension of input array.
C ni, nj      Integer       Number of points in the x- and y-directions.
C dx, dy      Real Array    Grid spacing in the x- and y-directions (m).
C f           Real Array    Coriolis parameter at each grid point (/s).
C zmid        Real Array    Heights at each grid point for level at which
C                           q-vector is being computed (m asl).
C ztop        Real Array    Heights at each grid point for the top of the
C                           layer, the center of whose heights are given
C                           in zmid (m asl).
C zbot        Real Array    Same as ztop except for the bottom of the layer
C                           (m asl).
C ptop        Real          Pressure level corresponding to ztop (mb).
C pbot        Real          Pressure level corresponding to zbot (mb).
C w1-dtdy     Real Array    Work arrays.
C qx, qy      Real Array    Work arrays holding the Q-vector components.
C
C On output:
C ----------               
C fgen        Real Array    Geostrophic frontogenesis function at grid points.
C
C User notes:
C -----------
C 1. No scaling of the output is done in this routine.
C 2. Units of geostrophic frontogensis function are (K/m)**2 / s
C
C###############################################################################
c
	implicit none
c
	integer mni, ni, nj
	real flag, bad, ptop, pbot
c
c---- Declare input arguments.
c
	real dx(mni,nj), dy(mni,nj)
	real f(mni,nj), ztop(mni,nj), zbot(mni,nj), zmid(mni,nj)
c
c---- Declare output arguments.
c
	real fgen(mni,nj)
c
c---- Declare other parameters and work arrays.
c
	parameter(flag = 1.e37)
	real w1(mni,nj), w2(mni,nj), w3(mni,nj)
	real dtdx(mni,nj), dtdy(mni,nj)
	real qx(mni,nj), qy(mni,nj)
        integer i,j
c
	bad = 1.e6 - 2.
c
c---- Step 1:
c---- Compute the Q-vector components.
c
	call qvector(zmid,ztop,zbot,ptop,pbot,mni,ni,nj,dx,dy,f,
     &               w1,w2,w3,fgen,dtdx,dtdy,qx,qy)


c.....	Now calculate the QG frontogensis function for this layer.
c.....		Fgen = 2(Qx * d(t)/dx + Qy * d(t)/dy)
c
	do 1 j=1,nj
	do 1 i=1,ni
	  if(qx(i,j).gt.bad .or. qy(i,j).gt.bad .or. 
     &       dtdx(i,j).gt.bad .or. dtdy(i,j).gt.bad) then
	    fgen(i,j) = flag
	  else
	    fgen(i,j) = 2. * (qx(i,j)*dtdx(i,j) + qy(i,j)*dtdy(i,j))
	  endif
1	continue
c
	return
	end

