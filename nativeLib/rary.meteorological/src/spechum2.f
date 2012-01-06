c
c
	subroutine spechum2(p,td,mni,ni,nj,q)
c
c...............................................................................
c
c	Routine to calculate saturation specific humidity from the dewpoint
c	and pressure.
c
c	Changes:
c               J. Ramer        10-31-90        Stole from spechum.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   p               RA         I    Pressure (mb)
c	   td              RA         I    Dewpoint (C or K)
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   q               RA         O    Specific Humidity (g/kg).
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
	real p(mni,nj), td(mni,nj)
	real q(mni,nj)
        real k,eee
        real flg,flag
        Data flg,flag/99998.0,1e37/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (p(i,j).gt.flg .or. td(i,j).gt.flg) then
            q(i,j)=flag
        Else
            k=td(i,j)
            If (k.lt.80.0) k=k+273.15
            eee=exp(33.09376-0.0091379024*k-6106.396/k)
            q(i,j)=eee/(p(i,j)-0.00060771703*eee)
        End If
10      Continue

	return
	end
