c
	subroutine calctd2(p,t,q,mni,ni,nj,td)
c
c..............................................................................
c
c	Routine to calculate dewpoint from pressure, temperature, and
c       specific humidity.
c
c	Changes: 
c               J. Ramer        03-21-01     Stole framework from calcrh2.
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
c	   td              RA         O    Depoint (K)
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
	real td(mni,nj)
        real tdofesat
        real flg,flag,eee
        Data flg,flag/99998.,1e37/

c first calculate vapor pressure (epsilon is 622 instead of 0.622 because
c q is in g/Kg) then dewpoint
        Do 10 j=1,nj
        Do 10 i=1,ni
        If (p(i,j).gt.flg .or. t(i,j).gt.flg .or. q(i,j).gt.flg) then
            td(i,j)=flag
        Else
            eee=p(i,j)*q(i,j)/(622.0+0.378*q(i,j))
            td(i,j)=tdofesat(eee)
        End If
10      Continue

	return
	end
