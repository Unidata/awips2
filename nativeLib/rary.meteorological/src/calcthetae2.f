c
c
	subroutine calcthetae2(p,t,td,mni,ni,nj,q)
c
c..............................................................................
c
c	Routine to calculate equivalent potential temperature from the
c	pressure, temperature and dewpoint.
c
c	Changes:
c               J. Ramer        10-31-90        Stole from calcthetae.
c
c	Inputs/Outputs:
c
c	   Variable     Var Type     I/O   Description
c	  ----------   ----------   ----- -------------
c	   p               RA         I    Pressure (mb)
c	   t               RA         I    Temperature (C or K)
c	   td              RA         I    Dewpoint (same as temp)
c	   mni             I          I    First dimension of input array.
c	   ni,nj           I          I    Grid dimensions in i,j.
c	   q               RA         O    Theta E (K).
c	
c
c  This used a completely standard formulation for the equivalent potential
c  temperature, except for two empirical approximations.
c
c  The saturation vapor pressure in millibars as a function of temperature is
c
c   es = exp(26.660820-0.0091379024*T-6106.3960/T)
c
c  where T is temperature in degrees K, as in esat.f.
c
c  The temperature of the lifting condensation level in degrees K is
c  approximated as
c
c   Tc = Td-(T-Td)*(-0.37329638+41.178204/T+0.0015945203*Td)
c
c  where T and Td are temperature and dewpoint in degrees K.  It is
c  within 0.05 degrees over the entire range of meaningful meteorological
c  conditions, and is usually within 0.02 degrees.
c
c	User Notes:
c
c	1.  No quality control is performed in this routine.
c
c..............................................................................
c
	implicit none
	integer mni, ni, nj, i, j
	real p(mni,nj), t(mni,nj), td(mni,nj)
	real q(mni,nj)
        real eee,td1,t1,w,tc
        real flg,flag,L_cp
        Data flg,flag,L_cp/99998.0,1e37,2540/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (p(i,j).gt.flg .or. t(i,j).gt.flg .or. td(i,j).gt.flg) then
            q(i,j)=flag
        Else
            If (t(i,j).lt.80.0) Then
                t1=t(i,j)+273.15
                td1=td(i,j)+273.15
            Else
                t1=t(i,j)
                td1=td(i,j)
            End If
            eee=exp(26.66082-0.0091379024*td1-6106.396/td1)
            tc=td1-(t1-td1)*(-0.37329638+41.178204/t1+0.0015945203*td1)
            w=0.622*eee/(p(i,j)-eee)
            q(i,j)=t1*exp(w*L_cp/tc)*(1000/p(i,j))**0.286
        End If
10      Continue

	return
	end
