c
	subroutine calctw(p,t,rh,mni,ni,nj,tw)
c
c..............................................................................
c
c	Routine to calculate wetbulb from temperature, and relative humidity.
c
c	Changes:
c               J. Ramer        11-12-92     Stole framework from calctd.
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
c	   tw              RA         O    wet-bulb temp (K)
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
	real t(mni,nj), rh(mni,nj), p(mni,nj)
	real tw(mni,nj), td, MyTw
        real k,b,rhqc
        real flg,flag
        Data flg,flag/99998.0,1e37/

        Do 10 j=1,nj
        Do 10 i=1,ni
        If (rh(i,j).gt.flg .or. t(i,j).gt.flg .or. p(i,j).gt.flg) then
            tw(i,j)=flag
        Else
            k=t(i,j)
            rhqc=amin1(100.0,amax1(1.0,rh(i,j)))
            b=0.0091379024*k+6106.396/k-alog(rhqc/100.0)
            td=(b-sqrt(b*b-223.1986))/0.0182758048
            tw(i,j)=MyTw(k,td,p(i,j))
        End If
10      Continue

	return
	end


      Real*4 Function MyTw(K,Kd,p)

C This function takes temperature in degrees K, dewpoint in degrees K
C and pressure in millibars and returns the isobaric wet-bulb temperature
C in degrees K using an iterative technique.  For a given guess for the
C wet bulb temp, one tries to do an energy balance, matching cp*(T-Tw) to
C (esat(Tw)-esat(Td))*eps*L/p*.
C
C c0, c1, and c2 are the same constants as from the esat.f function.
C f = cp/(L*epsilon).
C

      Implicit None

      Integer*4 l
      Real*4    f,c0,c1,c2,K,Kd,Kw,ew,p,ed,fp,s,de,Kdx
      Data      f,c0,c1,c2/0.0006355,26.66082,0.0091379024,6106.3960/

C Special cases of Td >= T or a ridiculously low T.
      If (Kd.ge.K) Then
          Kw=(K+Kd)/2
          Goto 30
      Else If (K.lt.100) Then
          Kw=K
          Goto 30
      End If

C Special case of a ridiculously high saturation vapor pressure.
      ew=c0-c1*K-c2/K
      If (ew.gt.10.0) Then
          Kw=(K+Kd)/2
          Goto 30
      End If
      ew=exp(ew)

C Kw is our current guess for wet-bulb, ed the vapor pressure corresponding
C to the depoint.  Deal with case of a ridiculously small dewpoint vapor
C pressure.
      Kdx=Kd
      ed=c0-c1*Kdx-c2/Kdx
5     If (ed.lt.-50.0) Then
          Kdx=Kdx+10
          ed=c0-c1*Kdx-c2/Kdx
          Goto 5
      End If
      ed=exp(ed)
      fp=p*f
      s=(ew-ed)/(K-Kdx)
      Kw=(K*fp+Kdx*s)/(fp+s)

C At each step of the iteration, esat(Tw)-esat(Td) is compared to
C (T-Tw)*p/(eps*L).  When that difference is less than one part in 
C 10000 of esat(Tw), or ten iterations have been done, the iteration stops.
C This is basically trying to find the value of Kw where de is 0.  The
C value s is the derivative of de with respect to Kw, a fairly standard
C numerical technique for finding the zero value of a function.
      Do 10 l=1,10
      ew=c0-c1*Kw-c2/Kw
      If (ew.lt.-50.0 .or. ew.gt.10.0) Goto 30
      ew=exp(ew)
      de=fp*(K-Kw)+ed-ew
      If (abs(de/ew).lt.1e-5) Goto 20
      s=ew*(c1-c2/(Kw*Kw))-fp
      Kw=Kw-de/s
10    Continue
20    Continue

30    MyTw=Kw
      Return
      End
