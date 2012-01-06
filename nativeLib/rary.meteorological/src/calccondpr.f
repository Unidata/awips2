c
	subroutine calccondpr(p,t,rh,mni,ni,nj,q)
c
c..............................................................................
c
c	Routine to calculate condensation pressure from the pressure, 
c	temperature, and relative humidity.
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
c	   q               RA         O    Condensation pressure (mb).
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
	real q(mni,nj),tcp,b,tp,tdp,rhqc
        real Flag,Flg
        Data Flag,Flg/1e37,99998.0/

        do 20 j=1,nj
        do 20 i=1,ni
        if (p(i,j).gt.flg .or. t(i,j).gt.flg .or. rh(i,j).gt.flg) then
            q(i,j)=flag
        else
            tp=t(i,j)
            rhqc=amin1(100.0,amax1(1.0,rh(i,j)))
            b=0.0091379024*tp+6106.396/tp-alog(rhqc/100)
            tdp=(b-sqrt(b*b-223.1986))/0.0182758048
            tcp=tdp-(tp-tdp)*(-0.37329638+41.178204/tp+0.0015945203*tdp)
            q(i,j)=p(i,j)*(tcp/tp)**3.498257
        end if
20      continue

	return
	end
