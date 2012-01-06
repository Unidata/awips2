      Real*4 Function PtoZsa(p)
 
C*  This routine converts a pressure in milibars into a height in a
C*  standard atmosphere in meters.
 
C Author: J Ramer  written in the late 1980's

      Implicit None
 
      Real*4 T0,gamma,p0,p11,z11,c1,c2,p
 
      Data T0,gamma,p0/288.,.0065,1013.2/
      Data c1,c2/5.256,14600./
      Data z11,p11/11000.,226.0971/
 
      Real*4 Flag,Flg
      Common/FlagFlg/Flag,Flg
      Data Flag,Flg/1e37,1.0E10/
 
      If (p.gt.Flg .or. p.lt.1.0) Then
          PtoZsa=Flag
        Else If (p.gt.p11) Then
          PtoZsa=((T0-T0*(p/p0)**(1/c1))/gamma)
        Else
          PtoZsa=(c2*alog10(p11/p)+z11)
       End If
 
      Return
      End
 
