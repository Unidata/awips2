c
	subroutine calcrh2(p,t,q,mni,ni,nj,rh)
c
c..............................................................................
c
c	Routine to calculate relative from humidity temperature, and dewpoint.
c
c	Changes: 
c               J. Ramer        10-31-90     Stole framework from calcrh2.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   p               RA         I    Pressure (mb)
c	   t               RA         I    Temperature (K)
c	   q               RA         I    Specific Humidity (g/Kg)
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   rh              RA         O    Relative humidity [range: 0. - 100.]
c	
c	
c	User Notes:
c
c	1.  No quality control is performed in this routine.
c
c..............................................................................
c
	implicit none
	integer mni, ni, nj, i, j
	real p(mni,nj), t(mni,nj), q(mni,nj)
	real rh(mni,nj)
        real flg,flag,a,b,c,eee
        Data flg,flag/99998.,1e37/

c Constants a, b and c from esat.f, except a is less by ln(100) to give
c RH in percent.
        Data a,b,c/22.05565,0.0091379024,6106.3960/

c first calculate vapor pressure (epsilon is 622 instead of 0.622 because
c q is in g/Kg) then RH in percent.
        Do 10 j=1,nj
        Do 10 i=1,ni
        If (p(i,j).gt.flg .or. t(i,j).gt.flg .or. q(i,j).gt.flg) then
            rh(i,j)=flag
        Else
            eee=p(i,j)*q(i,j)/(622.0+0.378*q(i,j))
            rh(i,j)=eee/exp(a-b*t(i,j)-c/t(i,j))
        End If
10      Continue

	return
	end
