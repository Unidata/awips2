c
	subroutine calcLI(p,t,rh,t5,p5,mni,ni,nj,LI)
c
c...............................................................................
c
c	Routine to calculate lifted index from the pressure, 
c	temperature, and relative humidity and 500mb temp.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   p               RA         I    Pressure (mb)
c	   t               RA         I    Temperature (K)
c	   rh              RA         I    Relative humidity [range: 0. - 100.]
c	   t               RA         I    Temperature at 500mb (K)
c	   p5              R          I    Upper pressure, normally 500mb
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   LI              RA         O    Lifted Index (C)
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
	real p(mni,nj), t(mni,nj), rh(mni,nj), t5(mni,nj), p5
	real LI(mni,nj),tc,b,tp,tdp,rhqc,eee,pc,w
        real Flag,Flg
        real adiabatic_te, temp_of_te
        Data Flag,Flg/1e37,99998.0/

        do 20 j=1,nj
        do 20 i=1,ni
        if (p(i,j).gt.flg .or. rh(i,j).gt.flg .or.
     &      t(i,j).gt.flg .or. t5(i,j).gt.flg .or. p(i,j).lt.p5) then
            LI(i,j)=flag
        else
            tp=t(i,j)
            rhqc=amin1(100.0,amax1(1.0,rh(i,j)))
            eee=rhqc*exp(22.05565-0.0091379024*tp-6106.396/tp)
            b=26.66082-alog(eee)
            tdp=(b-sqrt(b*b-223.1986))/0.0182758048
            tc=tdp-(tp-tdp)*(-0.37329638+41.178204/tp+0.0015945203*tdp)
            pc=p(i,j)*(tc/tp)**3.498257
            if (pc.le.p5) Then
                LI(i,j)=t5(i,j)-tp*(p5/p(i,j))**0.286
            else
                tc=adiabatic_te(tc,pc)*(p5/pc)**0.286
                LI(i,j)=t5(i,j)-temp_of_te(tc,p5)
            end if
        end if
20      continue

	return
	end
