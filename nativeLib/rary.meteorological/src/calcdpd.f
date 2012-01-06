c
	subroutine calcdpd(t,rh,mni,ni,nj,dpd)
c
c...............................................................................
c
c	Routine to calculate dewpoint depression from temperature
c       and relative humidity.
c
c	Changes:
c               J. Ramer        10-25-90     Adapted from calctd.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   t               RA         I    Temperature (K)
c	   rh              RA         I    Relative humidity [range: 0. - 100.]
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   dpd             RA         O    array of dewpoint depression (C).
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
	real t(mni,nj), rh(mni,nj)
	real dpd(mni,nj)
        real k,b,rhqc
        real flg,flag
        Data flg,flag/99998.0,1e37/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (rh(i,j).gt.flg .or. t(i,j).gt.flg) then
            dpd(i,j)=flag
        Else
            k=t(i,j)
            rhqc=amin1(100.0,amax1(1.0,rh(i,j)))
            b=0.0091379024*k+6106.396/k-alog(rhqc/100.0)
            dpd(i,j)=k-(b-sqrt(b*b-223.1986))/0.0182758048
        End If
10      Continue

	return
	end
