c
c
	subroutine nadgdt(u,v,a,mni,ni,nj,dx,dy,dadxdt,dadydt)
c
C#############################################################################
C Statement of purpose.
C ---------------------
C This routine computes the non-advective local change of an arbitrary
C conservative parameter `a'.
C
C History.
C --------                    
C J. Ramer                 original version 1 Jul 03
C
C Description of input and output.
C --------------------------------
C On input:
C ---------          
c u           Real Array    U component of wind.
c v           Real Array    V component of wind.
c a           Real Array    Arbitrary conservative parameter.
c mni         Integer       First dimension of input array.
C ni, nj      Integer       Number of points in the x- and y-directions.
C dx, dy      Real Array    Grid spacing in the x- and y-directions (m).
C
C On output:
C ----------               
C dadxdt      Real Array    Local change of the x direction gradient.
C dadydt      Real Array    Local change of the y direction gradient.

C
C###############################################################################
c
	implicit none
c
	integer mni, ni, nj, i, j, im, jm, ip, jp 
	real flag, bad, dudx, dvdx, dudy, dvdy, dadx, dady
c
c---- Declare input arguments.
c
	real dx(mni,nj), dy(mni,nj)
	real u(mni,nj), v(mni,nj), a(mni,nj)
c
c---- Declare output arguments.
c
	real dadxdt(mni,nj), dadydt(mni,nj)
c
c---- Declare other parameters and work arrays.
c
	parameter(flag = 1.e37)
        bad = 1e6-2.

c  Flag out boundaries.
          Do 21 i=1,ni
          dadxdt(i,1)=Flag
21        dadydt(i,1)=Flag
           Do 22 j=2,nj-1
           dadxdt(1,j)=Flag
           dadxdt(ni,j)=Flag
           dadydt(1,j)=Flag
22         dadydt(ni,j)=Flag
            Do 23 i=1,ni
            dadxdt(i,nj)=Flag
23          dadydt(i,nj)=Flag


      jm=1
      j=2
      Do 7091 jp=3,nj
      im=1
      i=2
      Do 7090 ip=3,ni
      If (u(im,j).gt.bad .or. u(ip,j).gt.bad .or.
     -    u(i,jm).gt.bad .or. u(i,jp).gt.bad .or.
     -    v(im,j).gt.bad .or. v(ip,j).gt.bad .or.
     -    v(i,jm).gt.bad .or. v(i,jp).gt.bad .or.
     -    a(im,j).gt.bad .or. a(ip,j).gt.bad .or.
     -    a(i,jm).gt.bad .or. a(i,jp).gt.bad) Then
          dadxdt(i,j)=Flag
          dadydt(i,j)=Flag
      Else
          dudx = (u(ip,j)-u(im,j))/Dx(i,j)
          dudy = (u(i,jp)-u(i,jm))/Dy(i,j)
          dvdx = (v(ip,j)-v(im,j))/Dx(i,j)
          dvdy = (v(i,jp)-v(i,jm))/Dy(i,j)
          dadx = (a(ip,j)-a(im,j))/Dx(i,j)
          dady = (a(i,jp)-a(i,jm))/Dy(i,j)
          dadxdt(i,j)=-0.5*(dudx*dadx+dvdx*dady);
          dadydt(i,j)=-0.5*(dudy*dadx+dvdy*dady);
      End If
      im=i
      i=ip
7090  Continue
      jm=j
      j=jp
7091  Continue

	return
	end
