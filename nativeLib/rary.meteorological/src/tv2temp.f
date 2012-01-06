c
c
	subroutine tv2temp(tv,q,mni,ni,nj,t)
c
c..............................................................................
c
c	Routine to calculate temperature from the virtual temperature and
c       specific humidity.
c
c	Changes:
c               J. Ramer        11-26-95        stole framework from spechum 
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   tv              RA         I    Virtual temperature (K)
c	   q               RA         I    Specific Humidity (g/kg).
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   t               RA         O    Temperature (K)
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
	real q(mni,nj), tv(mni,nj)
	real t(mni,nj)
        real flg,flag
        Data flg,flag/99998.0,1e37/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (q(i,j).gt.flg .or. tv(i,j).gt.flg) then
            t(i,j)=flag
        Else
            t(i,j)=tv(i,j)/(1+0.000608*q(i,j))
        End If
10      Continue

	return
	end
