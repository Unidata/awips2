c
	subroutine calcrh(t,td,mni,ni,nj,rh)
c
c..............................................................................
c
c	Routine to calculate relative from humidity temperature, and dewpoint.
c
c	Changes:
c               J. Ramer        10-31-90     Stole framework from calctd.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   t               RA         I    Temperature (C or K)
c	   td              RA         I    Dewpoint (same as temp)
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
	real t(mni,nj), td(mni,nj)
	real rh(mni,nj)
        real t1,td1
        real flg,flag,b,c,k0
        Data flg,flag,b,c,k0/99998.0,1e37,0.0091379024,6106.396,273.15/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (td(i,j).gt.flg .or. t(i,j).gt.flg) then
            rh(i,j)=flag
        Else If (t(i,j).lt.80.0) Then
            t1=t(i,j)+k0
            td1=td(i,j)+k0
            rh(i,j)=100.0*exp(b*(t1-td1)+c/t1-c/td1)
        Else
            t1=t(i,j)
            td1=td(i,j)
            rh(i,j)=100.0*exp(b*(t1-td1)+c/t1-c/td1)
        End If
10      Continue

	return
	end
