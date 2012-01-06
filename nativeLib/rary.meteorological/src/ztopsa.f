      Real*4 Function ZtoPsa(Z)
 
C*  This routine converts a height in meters into a pressure in a standard
C*  atmosphere in milibars.
 
C Author: J Ramer  written in the late 1980's

      Implicit None
 
      Real*4 T0,gamma,p0,p11,z11,c1,c2,z
 
      Data T0,gamma,p0/288.,.0065,1013.2/
      Data c1,c2/5.256,14600./
      Data z11,p11/11000.,226.0971/
 
      Real*4 Flag, Flg
c      Common /FlagFlg/ Flag, Flg
      Data Flag, Flg /1e37, 1.0E10/
 
      If (Z.gt.Flg) Then
          ZtoPsa=Flag
        Else If (Z.lt.z11) Then
          ZtoPsa=p0*((T0-gamma*Z)/T0)**c1
        Else
          ZtoPsa=p11*10.**((z11-Z)/c2)
       End If
 
      Return
      End
 
