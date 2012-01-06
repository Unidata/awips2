      Real*4 function TdofEsat(Es)
 
C*  As a function of saturation vapor pressure in millibars, returns
C*  dewpoint in degrees K.
 
C Author: J Ramer  written in the late 1980's

C  Is based upon a variation of the integrated form of the Clausius-Clapeyron
C  equation.  Has an additional linear term in it and is fit to data in
C  the Smithsonian Meterological Tables.  Is accurate to one part in a
C  thousand over the range from -25C to +35C.  Its main advantage is that
C  it is invertable.
 
      Implicit None
 
      Real*4 Es,lim1,lim2,b
 
      Data lim1,lim2/3.777647E-05,980.5386/
 
      Real*4 Flag,Flg
      Common/FlagFlg/Flag,Flg
 
c  Flag ridiculous values.
      If (Es.lt.0.0 .or. Es.gt.lim2) Then
          TdofEsat=Flag
          Return
         End If
 
c  Avoid floating underflow.
      If (Es.lt.lim1) Then
          TdofEsat=173.15
          Return
         End If
 
c  Calculations for normal range of values.
      b=26.66082-alog(Es)
      TdofEsat=(b-sqrt(b*b-223.1986))/0.0182758048
 
      Return
      End
