 
      Real*4 Function virttemp(T,Td,P)
 
C*  Calculates virtual temperature as a function of temperature,
C*  dewpoint, and pressure (mb).

C Author: D Perry

      Implicit None

C  Input Variables
      Real*4 T,Td,P
 
C  Internal variables
      Real*4 e,w,K,Kd

C  External Functions
      Real esat

C  Data error flags
      Real*4 Flag
      Data Flag /1e37 /
 
c  Account for both Celsius and Kelvin.
      K=T
      Kd=Td
      If (K.lt.100.) K=K+273.15
      If (Kd.lt.100.) Kd=Kd+273.15
 
c  Flag ridiculous values.
      If (K.lt.0.0 .or. K.gt.373.15) Then
          virttemp=Flag
          Return
         End If
 
c  Avoid floating underflow.
      If (Kd.lt.173.15) Then
          virttemp=K
          Return
         End If
 
c  Calculation for normal range of values.
      e= esat(Kd)
      w= (0.622*e)/(P-e)
      virttemp= K*(1+(0.6*w))
 
      Return
      End
 
