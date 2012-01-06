c
c
	subroutine calcthetae(p,t,rh,mni,ni,nj,q)
c
c..............................................................................
c
c	Routine to calculate equivalent potential temperature from the, 
c	pressure temperature, and relative humidity.
c
c	Changes:
c		P.A. Stamus	09-05-89	Original (based on Baker's)
c				09-20-89	Add implicit none.
c               J. Ramer        05-02-90        Increased algorithm speed. 
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   p               RA         I    Pressure (mb)
c	   t               RA         I    Temperature (C or K)
c	   rh              RA         I    Relative humidity [range: 0. - 100.]
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   q               RA         O    Theta E (K).
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
	real q(mni,nj)
        real k,eee,w,tdp,tc,b,rhqc
        real flg,flag,L_cp
        Data flg,flag,L_cp/99998.0,1e37,2540/

        Do 10 j=1,nj
        Do 10 i=1,ni
          If (p(i,j).gt.flg .or. t(i,j).gt.flg .or. rh(i,j).gt.flg) then
            q(i,j)=flag
          Else
            k=t(i,j)
            If (k.lt.80.0) k=k+273.15
            rhqc = amax1(amin1(100.0,rh(i,j)),0.01)
            eee=rhqc*exp(22.05565-0.0091379024*k-6106.396/k)
            b=26.66082-alog(eee)
            tdp=(b-sqrt(b*b-223.1986))/0.0182758048
            tc=tdp-(k-tdp)*(-0.37329638+41.178204/k+0.0015945203*tdp)
            w=0.622*eee/(p(i,j)-eee)
            q(i,j)=k*exp(w*L_cp/tc)*(1000/p(i,j))**0.286
          End If
10      Continue

	return
	end
