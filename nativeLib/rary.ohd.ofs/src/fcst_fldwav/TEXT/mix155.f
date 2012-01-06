      SUBROUTINE MIX155(J,I,KFRM,KFR,KFRP,IFR,K1,K2)
      DIMENSION IFR(K2,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/mix155.f,v $
     . $',                                                             '
     .$Id: mix155.f,v 1.1 1999/04/23 18:08:43 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HSINC,4H55  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      KFRM=IFR(I-1,J)
      IF(KFRM.GE.1) KFRM=1
      IF(KFRM.LE.0) KFRM=0
      KFR=IFR(I,J)
      IF(KFR.GE.1) KFR=1
      IF(KFR.LE.0) KFR=0
      KFRP=IFR(I+1,J)
      IF(KFRP.GE.1) KFRP=1
      IF(KFRM.LE.0) KFRM=0
      RETURN
      END
