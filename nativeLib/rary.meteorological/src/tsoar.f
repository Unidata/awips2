      subroutine tsoar(elev,p,z,t,theta,nl,Tpmax,PTLXEC,zlnec,tlnec,
     1		       zlxec,tlxec,soarindx,Trigtemp)
C***DARE-II TSOAR Program for Denver Aviation and soaring community.
C History.
C --------                    
C	L. Rasmussen	 8 Feb 89	Original version.
C	J. Wakefield	20 Oct 89	Some tinkering.  Added PTLXEC parameter.

C Description of input and output.
C --------------------------------
C On input:
C ---------                
C Elev		Station elevation 
C p		Raob pressures (mb)
C z		Raob heights (m)
C t		Raob temperatures (degrees Celsius)
C theta		The potential temperatures of the sounding data
C nl		number of levels in sounding
C Tpmax		Forecast max temperature 

C On output:
C ----------               
C PTLXEC	Potential temperature (C) of forecast TMax.
C zlnec		The height (m) of the level of minimum effective convection.
C tlnec		The temperature (C) of the level of min effective convection.
C zlxec		The height (m) of the maximum thermal altitude
C tlxec		The temperature (C) of the LXEC (max thermal alt)
C Soarindx	The soaring index in ft/min.
C trigtemp	The trigger temperature (C)
C###############################################################################

       implicit none

C***Parameter list variables
       Integer*4	NL
       real*4		Elev,p(nl),z(nl),t(nl),theta(nl),TPMax,Zlnec
       real*4		Tlnec,Zlxec,Tlxec,Soarindx,TrigTemp

C***Local variables
       real flag
       parameter	(flag=99999.)
       real*4		Plnec,Plxec,Ptlxec,ThetaTrig,T500,Z500,P500
       integer*4	i

C Initialize the soaring parameters to flag
       Trigtemp=flag
       Zlnec=flag
       Tlnec=flag
       Zlxec=flag
       Tlxec=flag
       Soarindx=flag
       T500=flag
	
C Determine if the station elevation is above or below 2000 ft msl (609.57 m),
C then add either 4000 ft (1219.14 m) or 3000 ft (914.36 m) to compute the LNEC.
C From the LNEC compute the interpolated pressure and temperature at the LNEC,
C and finally compute the trigger temperature by following the potential
C temperature line going thru the LNEC temperature down to the surface.
       If(elev.gt.609.57) Then
          Zlnec = elev + 1219.14
       Else
          Zlnec = elev + 914.36
       Endif
C	Type *,'elev',elev,' m;',elev*3.28084,' ft'
C	Type *,'Zlnec',Zlnec,' m;',Zlnec*3.28084,' ft'

C Compute the potential temp. of the forecast max temperature, then run up the
C sounding and locate or interpolate the height and pressure of the Lxec.
C	TYPE *,'P sfc (mb) and fcst tmax (K)',P(1),Tpmax
	Ptlxec=Tpmax*((1000.0/P(1))**0.286)
C	TYPE *,'Ptlxec (K)',Ptlxec

c initialize Plnec, Plxec, &Z500
      i=1
      Plnec=P(i)+(P(i+1)-P(i))*((Zlnec-z(i))/(z(i+1)-z(i)))
      Plxec=P(i)+(P(i+1)-P(i))*((Ptlxec-theta(i))/
     +                                     (theta(i+1)-theta(i)))
      Z500=Z(i)

      Do i=1,nl-1
	 If((Zlnec.ge.z(i)).and.(Zlnec.le.z(i+1))) Then
            Tlnec=T(i)+(T(i+1)-T(i))*((Zlnec-z(i))/(z(i+1)-z(i)))  
            Plnec=P(i)+(P(i+1)-P(i))*((Zlnec-z(i))/(z(i+1)-z(i)))
	 Endif

C Save info at 500 mb, in case the max thermal alt. is above 500 mb and must be
C truncated at 500 mb/18000 ft.
	 If(P(i).le.500.0 .and. T500.eq.flag) Then
	  T500=T(i)
	  Z500=Z(i)
	  P500=P(i)
	 Endif
	 If((Ptlxec.ge.theta(i)).and.(Ptlxec.le.theta(i+1))) Then
	  If(Theta(I).eq.Theta(I+1))Then
	   ZLXEC=Z(I)
	   PLXEC=P(I)
	   TLXEC=T(I)
	  Else
	   Zlxec=Z(i)+(Z(i+1)-Z(i))*((Ptlxec-theta(i))/(theta(i+1)-
     -theta(i)))
	   Plxec=P(i)+(P(i+1)-P(i))*((Ptlxec-theta(i))/(theta(i+1)-
     -theta(i)))
	   Tlxec=T(i)+(T(i+1)-T(i))*((Ptlxec-theta(i))/(theta(i+1)-
     -theta(i)))
	  EndIf
	 Endif
	EndDo

C	Type *,'Plxec,Zlxec and Tlxec are',Plxec,'mb',Zlxec,'m',Tlxec,'K'
C	Type *,'P(1),Plnec,Zlnec,Tlnec are',P(1),Plnec,Zlnec,Tlnec

C  compute the potential temperature of the trigger temperature.
	ThetaTrig=Tlnec*((1000.0/Plnec)**0.286)
C	Type *,'thetatrig & p(1) is',Thetatrig,P(1)
	TrigTemp=((ThetaTrig)*((p(1)/1000.)**.286))-273.15
C	TYPE *,'trigtemp is(C),Tpmaxis(K)',TrigTemp,Tpmax

C  Truncate the height of the LXEC to 18000 ft, due to FAA positive controlled
C  airspace or possible soaring flight in IFR conditions by penetrating into
C  clouds. 
	If(Plxec.lt.500.) Then
C        Type *,'Truncated at 500mb.'
	 Zlxec=Z500
	 Tlxec=T500
	 Plxec=P500
	Endif
C	Type *,'LXEC Hgt (m), P (mb), T (K):',Zlxec,Plxec,Tlxec
C	Type *,'LNEC P (mb), T (K):',Plnec,Tlnec
 
C  Compute the Soaring Index if the fcst max temperature is equal to or exceeds
C  the trigger temperature.
	If(TPMax.ge.TrigTemp+273.15)Then
	 Soarindx=(3*(((Zlxec*3.281)/100.) + 10.*(Tlnec-Tlxec)))
	Else
	 Soarindx=flag
	Endif
C	TYPE *,'Soaring index (ft/min) is',Soarindx

	Return
	End
