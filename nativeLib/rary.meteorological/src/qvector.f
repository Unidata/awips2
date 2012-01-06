c
	subroutine qvector(zmid,ztop,zbot,ptop,pbot,mni,ni,nj,dx,dy,f,
     &                     dugdx,dvgdx,dugdy,dvgdy,dtdx,dtdy,qx,qy)
c
C###############################################################################
C Statement of purpose.
C ---------------------
C This routine computes the q-vector components at a specified
C pressure level.
C
C History.
C --------                    
C D. Baker       01 Oct 86    Original version based on equations given
C                             in Barnes (????).
C P. Stamus      30 Aug 89    Made divergence a separate routine, cleaned up
C                             Baker's version, pass in work arrays, etc.
c		 20 Sep 89    Add implicit none.
C E. Thaler      22 Aug 91    Changed to use only two pressure levels.
C J. Ramer       24 Feb 92    New algorithm for derivatives of geostrophic
C                             wind components.
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
C dugdx-dtdy  Real Array    Work arrays.
C
C On output:
C ----------               
C qx, qy      Real Array    Q-vector components at grid points.
C
C User notes:
C -----------
C 1. No scaling of the output is done in this routine.
C 2. Units of Q-vector on output are K/m/s
C
C###############################################################################
c
	implicit none
c declare formal arguments
      integer mni, ni, nj
      real    zmid(mni,nj), ztop(mni,nj), zbot(mni,nj), ptop, pbot
c---- Declare input arguments.
      real    dx(mni,nj), dy(mni,nj), f(mni,nj)
c---- Declare local parameters and work arrays.
      real*4  dugdx(mni,nj), dvgdx(mni,nj), dugdy(mni,nj), dvgdy(mni,nj)
      real*4  dtdx(mni,nj), dtdy(mni,nj)
c---- Declare output arguments.
      real    qx(mni,nj), qy(mni,nj)
c
      integer i, j
      real    flag, bad, g, const, pavg
c
        Integer*4 passes
        Real*4    smoothness
        Common /qsmthcmn/passes,smoothness
	parameter(flag = 1.e37)

	data g/9.806/
        data bad/99998./
c
c---- Step 1:
c---- Compute the "mean layer" geostrophic wind components at each grid point,
c---- using the mean height between the two levels.  
c---- (store components in qx and qy)
c
	do 102 j=1,nj
    	do 102 i=1,ni
	  If (ztop(i,j).gt.bad .or. zbot(i,j).gt.bad) Then
            zmid(i,j) = flag
          Else
            zmid(i,j) = (ztop(i,j) + zbot(i,j))/2.
          Endif
 102    continue

c smooth heights and calculate components of the geostrophic wind.
        Do i=1,passes/2
           call smooth(zmid,qx,mni,ni,nj,smoothness)
           call smooth(qx,zmid,mni,ni,nj,smoothness)
        EndDo
        if (iand(passes,1).eq.1) then
            call smooth(zmid,qx,mni,ni,nj,smoothness)
            call DgeoComps(qx,f,dx,dy,mni,nj,ni,nj,
     &                     dugdx,dugdy,dvgdx,dvgdy)
        else
            call DgeoComps(zmid,f,dx,dy,mni,nj,ni,nj,
     &                     dugdx,dugdy,dvgdx,dvgdy)
        end if
c
c---- Step 3:
c---- Compute the partial with respect to pressure of the geopotential
c---- between the top and bottom layers.  The sign is reversed per the
c---- definition of q-vector equation.  'const' is used to convert from
c---- potential temperature gradient to thickness gradient. (store in qx)
c
	pavg = exp ( (alog(pbot) + alog(ptop))/2. )
	const = (g*pavg) / (287. * (ptop-pbot) * ((pavg/1000.)**.286))
	do 104 j=1,nj
	do 104 i=1,ni
	  if(ztop(i,j).gt.bad .or. zbot(i,j).gt.bad) then
	    qx(i,j) = flag
	  else
	    qx(i,j) = (-const) * (ztop(i,j) - zbot(i,j))
	  endif
104	continue
c
c---- Step 4:
c---- Smooth and compute the gradient of the quantity computed in step 3 above.
c---- (f is just a place holder)
c
        Do i=1,passes/2
           call smooth(qx,qy,mni,ni,nj,smoothness)
           call smooth(qy,qx,mni,ni,nj,smoothness)
        EndDo
        if (iand(passes,1).eq.1) then
            call smooth(qx,qy,mni,ni,nj,smoothness)
            call G2Gkinematics(dtdx,dtdy,f,dx,dy,mni,nj,ni,nj,7,qy)
        else
            call G2Gkinematics(dtdx,dtdy,f,dx,dy,mni,nj,ni,nj,7,qx)
        endif
c
c---- Step 5:
c---- Compute the x-component of the q-vector.  This is the partial with
c---- respect to x of the geostrophic wind dotted with the quantity
c---- computed in step 4.  Similiarly the y-component of the q-vector
c---- is the partial with respect to y of the geostrophic wind dotted
c---- with the quantity computed in step 4.  The equations are:
c
c	   Qx = -d(ug)/dx * d(dz/dp)/dx - d(vg)/dx * d(dz/dp)/dy
c	   Qy = -d(ug)/dy * d(dz/dp)/dx - d(vg)/dy * d(dz/dp)/dy
c
	do 108 j=1,nj
	do 108 i=1,ni
          if (dugdx(i,j).gt.bad .or. dugdy(i,j).gt.bad .or.
     &        dvgdx(i,j).gt.bad .or. dvgdy(i,j).gt.bad .or.
     &        dtdx(i,j).gt.bad .or. dtdy(i,j).gt.bad) Then
	    qx(i,j) = flag
	    qy(i,j) = flag
	 else
	    qx(i,j) = (-dugdx(i,j)) * dtdx(i,j) - dvgdx(i,j) * dtdy(i,j)
	    qy(i,j) = (-dugdy(i,j)) * dtdx(i,j) - dvgdy(i,j) * dtdy(i,j)
	 endif
108	continue
c
	return
	end
