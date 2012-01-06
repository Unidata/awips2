
      Real*4 Function esat(T)

C*  Calculates saturation vapor pressure in millibars as a function of
C*  either Kelvin of Celcius temperature.

C Author: J Ramer  written in the late 1980's

C  Is based upon a variation of the integrated form of the Clausius-Clapeyron
C  equation.  Has an additional linear term in it and is fit to data in
C  the Smithsonian Meterological Tables.  Is accurate to one part in a
C  thousand over the range from -25C to +35C.  Its main advantage is that
C  it is invertable.

      Implicit None

      Real*4 T,K

      Real*4 Flag
      Data Flag /1e37/
c unused varaible      real*4 Flg
c      Common /FlagFlg/ Flag, Flg
c      Data Flg /1.0E10/

c  Account for both Celsius and Kelvin.
      K=T
      If (K.lt.100.) K=K+273.15

c  Flag ridiculous values.
      If (K.lt.0.0 .or. K.gt.373.15) Then
          esat=Flag
          Return
         End If

c  Avoid floating underflow.
      If (K.lt.173.15) Then
          esat=3.777647E-05
          Return
         End If

c  Calculation for normal range of values.
      esat=exp(26.660820-0.0091379024*K-6106.3960/K)

      Return
      End

