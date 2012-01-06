c
c
	subroutine calctv(p,t,rh,mni,ni,nj,tv)
c
c..............................................................................
c
c	Routine to calculate virtual temperature from the pressure, 
c	temperature, and relative humidity.
c
c	Changes:
c               J. Ramer        06-18-03        Stole from spechum.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   p               RA         I    Pressure (mb)
c	   t               RA         I    Temperature (K)
c	   rh              RA         I    Relative humidity [range: 0. - 100.]
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   tv              RA         O    Virtual temperaure.
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
	real p(mni,nj), t(mni,nj), rh(mni,nj)
	real tv(mni,nj)
        real k,eee
        real flg,flag
        Data flg,flag/99998.0,1e37/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (p(i,j).gt.flg .or. t(i,j).gt.flg .or. rh(i,j).gt.flg) then
            tv(i,j)=flag
        Else
            k=t(i,j)
            eee=exp(21.0827887-0.0091379024*k-6106.396/k)
            tv(i,j)=t(i,j)*p(i,j)/ (p(i,j)-rh(i,j)*eee)
        End If
10      Continue

	return
	end
