c
c
	subroutine alt2press(alt,z,mni,ni,nj,p)
c
c...............................................................................
c
c	Routine to pressure from elevation and altimeter setting.
c
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c          alt             RA         I    Altimeter setting (X)
c	   z               RA         I    Elevation in meters.
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   p               RA         O    Pressure (X)
c	
c	
c	User Notes:
c
c	1.  No quality control is performed in this routine.
c
c...............................................................................
c
	implicit none
	integer mni, ni, nj, i, j
	real alt(mni,nj), z(mni,nj)
	real p(mni,nj)
        real flg,flag,T0,gamma,g_Rgamma
        Data flg,    flag,  T0,  gamma, g_Rgamma
     &      /99998.0,1e37,288.0,0.0065,5.2532/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (alt(i,j).gt.flg .or. z(i,j).gt.flg) then
            p(i,j)=flag
        Else
            p(i,j)=alt(i,j)*((T0-gamma*z(i,j))/T0)**g_Rgamma
        End If
10      Continue

	return
	end
