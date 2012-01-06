c
c
	subroutine comp_by(u,v,uu,vv,mni,ni,nj,control,comp,comp2)
c
C#############################################################################
C Statement of purpose.
C ---------------------
C This routine computes the component of the first vector in the direction
C of the second.
C
C History.
C --------                    
C J. Ramer                 original version 1 Jul 03
C
C Description of input and output.
C --------------------------------
C On input:
C ---------          
c u           Real Array    X compontent of first vector
c v           Real Array    Y compontent of first vector
c uu          Real Array    X compontent of second vector
c vv          Real Array    Y compontent of second vector
c mni         Integer       First dimension of input array.
C ni, nj      Integer       Number of points in the x- and y-directions.
C control     Real          Value which controls how calculation is done.
C                           Number of degrees to rotate second vector before
C                           it is dotted with the first vector.  If not
C                           an integer, then do not normalize by the
C                           magnitude of the second vector.  If thousands
C                           place is one, output component unsigned.
C                           If thousands place is two output the component
C                           as a vector, with comp being the x component
C                           and comp2 being the y-component.
C                           If thousands place is three output a second
C                           component in comp2, in the direction of the
C                           k-cross of second vector.
C
C On output:
C ----------               
C comp        Real Array    Component of first vector in direction of second,
C                           or x component of component as a vector.
C comp2       Real Array    Component of first vector in k-cross direction
C                           of second or y component of component as a vector.

C
C###############################################################################
c
	implicit none
c
	integer mni, ni, nj, i, j, ss, vecout, magpwr
	real flag, bad, a, b, conint, uuu, vvv, mag
        logical norml, rotyes
        real dgtord/0.01745329252/
c
c---- Declare input arguments.
c
        real control
	real u(mni,nj), v(mni,nj)
	real vv(mni,nj), uu(mni,nj)
c
c---- Declare output arguments.
c
	real comp(mni,nj), comp2(mni,nj)
c
c---- Interpret the control value
c
	parameter(flag = 1.e37)
        bad = 1e6-2.
        ss = 1
        conint = 0
        vecout = 0
        magpwr = 1
        rotyes = .false.
        norml = .true.
        if (control.gt.-3181 .and. control.lt.3181 .and.
     &      control.ne.0) Then
            if (control.lt.0) ss = -1
            conint = int(ss*control)
            if (conint.ne.ss*control) norml = .false.
            vecout = conint/1000
            conint = (conint-1000*vecout)*ss
            if (conint.lt.-180 .or. conint.gt.180 .or.
     &          conint.eq.0) Then
                conint = 0;
            else
                a = cos(dgtord*conint)
                b = sin(dgtord*conint)
                rotyes = .true.
            End If
            if (vecout.eq.2) magpwr = 2
            if (.not.norml) magpwr = magpwr -1
        End If

      mag = 1
      Do 7091 j=1,nj
      Do 7090 i=1,ni
      If (u(i,j).gt.bad .or. uu(i,j).gt.bad .or.
     -    v(i,j).gt.bad .or. vv(i,j).gt.bad) Then
          comp(i,j)=Flag
          if (vecout.ge.2) comp2(i,j)=Flag
      Else
          if (rotyes) then
              uuu = uu(i,j)*a+vv(i,j)*b
              vvv = vv(i,j)*a-uu(i,j)*b
          Else
              uuu = uu(i,j)
              vvv = vv(i,j)
          End if
          if (magpwr.eq.2) Then
              mag=uuu*uuu+vvv*vvv
          Else if (magpwr.eq.1) Then
              mag=sqrt(uuu*uuu+vvv*vvv)
          EndIf
          If (mag.eq.0) Then
              comp(i,j)=Flag
              if (vecout.ge.2) comp2(i,j)=Flag
          else If (vecout.eq.3) Then
              comp(i,j)=(uuu*u(i,j)+vvv*v(i,j))/mag
              comp2(i,j)=(vvv*u(i,j)-uuu*v(i,j))/mag
          else If (vecout.eq.2) Then
              mag=(uuu*u(i,j)+vvv*v(i,j))/mag
              comp(i,j)=uuu*mag
              comp2(i,j)=vvv*mag
          else
              comp(i,j)=(uuu*u(i,j)+vvv*v(i,j))/mag
              if (vecout.eq.1 .and. comp(i,j).lt.0)
     &            comp(i,j)=-comp(i,j)
          EndIf
      End If
7090  Continue
7091  Continue

	return
	end
