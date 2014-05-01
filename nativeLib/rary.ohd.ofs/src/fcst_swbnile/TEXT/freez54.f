C MEMBER FREEZ54
C
C==================================================================
C
      SUBROUTINE FREEZ54(DT,T,SDP,SDN,DBT,DMBT,DSOIL,POROS,WWP,
     *                 DZBT,DZUP,THAW,FREZ)
C   ****  SIMULATION OF ICE CONTENT OF SOIL
      REAL KICE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_swbnile/RCS/freez54.f,v $
     . $',                                                             '
     .$Id: freez54.f,v 1.1 1997/09/22 15:33:40 page Exp $
     . $' /
C    ===================================================================
C
      W=(DMBT-DBT)/DMBT
      WR=W*(POROS-WWP)
      SMAVG=WWP+WR
      IF(DZBT.EQ.0..AND.T.GT.0.) THEN
        THAW = 0.
        FREZ = 0.
        GOTO 7
      ENDIF
      SOIL=CSOIL54(DSOIL,SMAVG,T)
C   *****   KICE=2*DT*SOIL/L  ************************************
C   *****   IF DT IN DAYS, SOIL IN KAL/CM.H. C, AND L=80 KAL/G  **
C   *****   KICE=(2*100.*24./80.)DT*SOIL=60.*DT*SOIL, IN MM2    **
      KICE=60*DT*SOIL
C    IF SDP<0, SOIL SURFACE TEMPERATURE WILL BE USED INSTEAD OF
C            SNOW TEMPERATURE
      IF(SDP.LT.0.) THEN
        ASNOW=0.0
      ELSE
        ASNOW=SOIL*SDP/CSNOW54(SDN)
      ENDIF
      IF (T.GT.0.) THEN
        AA=(ASNOW+THAW)**2+KICE*ABS(T)/SMAVG
        THAW=-ASNOW+SQRT(AA)
      ELSE
        AA=(ASNOW+DZBT+DZUP)**2+KICE*ABS(T)/SMAVG
        FREZ=-ASNOW+SQRT(AA)-DZBT-DZUP
      END IF
    7 RETURN
      END
