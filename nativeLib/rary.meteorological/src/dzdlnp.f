
      Real*4 Function dzdlnp(p,T,Td)

C returns the rate of change of height versus the log of pressure.

C  p                Real   Pressure in mb.
C  T                Real   Temperature in C or K.
C  Td               Real   Dewpoint temp in same units as temperature.

      Real*4 p,T,Td
      Real*4 q,K,Kd

C Handle either C or K for units, missing Td
      K = T
      Kd = Td
      if (Kd.gt.K) Kd = K - 50
      if (T.lt.100.0) then
        K = K+273.15
        Kd = Kd+273.15
      endif

C Calculate vapor pressure
      q = exp(26.660820-0.0091379024*Kd-6106.396/Kd)

C Calculate mixing ratio
      q = 0.622*q/(p-0.378*q)

C return the answer using variation of hypsometric eqn
      dzdlnp = K*(1.0+0.608*q)*287.04/9.807
      Return
 
      End
 
