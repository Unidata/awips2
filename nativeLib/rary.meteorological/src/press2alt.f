c
c
	subroutine press2alt(p,z,mni,ni,nj,alt)
c
c..............................................................................
c
c	Routine to get altimeter setting from pressure and elevation.
c
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   p               RA         O    Pressure (X)
c	   z               RA         I    Elevation in meters.
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c          alt             RA         I    Altimeter setting (X)
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
	real p(mni,nj), z(mni,nj)
	real alt(mni,nj)
        real flg,flag,T0,gamma,g_Rgamma
        Data flg,    flag,  T0,  gamma, g_Rgamma
     &      /99998.0,1e37,288.0,0.0065,5.2532/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (p(i,j).gt.flg .or. z(i,j).gt.flg) then
            alt(i,j)=flag
        Else
            alt(i,j)=p(i,j)/((T0-gamma*z(i,j))/T0)**g_Rgamma
        End If
10      Continue

	return
	end
