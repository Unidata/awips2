      SUBROUTINE CKHILO21(XNOS,II,NU)

C   THIS PROGRAM DETERMINES THE HI AND LO INFO
C
C  DEFINITION OF VARIABLES
C    EHH,ELH,ELL,EHL         - ELEVATION CORR. TO TIDE HI-HI,LO-HI,
C                                LO-LO,HI-LO AT CURRENTLY
C    EHHO,ELHO,ELLO,EHLO     - ELEVATION CORR. TO TIDE HI-HI,LO-HI,
C                                LO-LO,HI-LO AT PREVIOUSLY
C    BHH,BLH,BLL,BHL         - (NOS-OBS) FOR CURRENT HH,LH,LL,HL
C    BHHO,BLHO,BLLO,BHLO     - (NOS-OBS) FOR PREVIOUS HH,LH,LL,HL
C    BLMX,BLMN               - MAX AND MIN (NOS-OBS)
C    ITHH,ITLH,ITLL,ITHL     - TIME STEP CORR TO CURRENT HH,LH,LL,HL
C    ITHHO,ITLHO,ITLLO,ITHLO - TIME STEP CORR TO PREVIOUS HH,LH,LL,HL
C    II      -
C    NU      - NO. OF TIME STEPS
C    XNOS(I) - NOS TIDE T.S.
C
C THIS SUBROUTINE CALLS: NONE

      INCLUDE 'common/fdbug'
      COMMON/XHILO/EHH,ELH,ELL,EHL,EHHO,ELHO,ELLO,EHLO,
     .             BHH,BLH,BLL,BHL,BHHO,BLHO,BLLO,BHLO,BLMX,BLMN,
     .             ITHH,ITLH,ITLL,ITHL,ITHHO,ITLHO,ITLLO,ITHLO
C
      DIMENSION XNOS(*)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/ckhilo21.f,v $
     . $',                                                             '
     .$Id: ckhilo21.f,v 1.3 2000/09/27 16:11:11 page Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'CKHILO21' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)

CC              IP=>PEAK VALUES; IV=>VALLEY VALUES
      I1=0
      I2=0
      IP1=0
      IP2=0
      IV1=0
      IV2=0
      IPDON=0
      IVDON=0

      NU2=NU-2
      DO 100 K=II,NU2

C...FIND THE LOCATION OF THE TWO PEAKS IN THIS PERIOD
      K1=K+1
      K2=K+2
      IF(K1.GT.NU) K1=NU
      IF(K2.GT.NU) K2=NU
      IF(XNOS(K1).GT.XNOS(K).AND.XNOS(K1).GE.XNOS(K2)) THEN
        IF(IP1.EQ.0) THEN
          IP1=K1
        ELSEIF(IP2.EQ.0) THEN
          IP2=K1
          IPDON=1
        ENDIF

C...FIND THE LOCATION OF THE TWO VALLEYS IN THIS PERIOD
      ELSEIF(XNOS(K1).LE.XNOS(K).AND.XNOS(K1).LT.XNOS(K2)) THEN
        IF(IV1.EQ.0) THEN
          IV1=K1
        ELSEIF(IV2.EQ.0) THEN
          IV2=K1
          IVDON=1
        ENDIF
      ENDIF

C...SET HI INFO
      IF(IPDON.EQ.1) THEN
        IF(XNOS(IP1).GT.XNOS(IP2)) THEN
          EHH=XNOS(IP1)
          ELH=XNOS(IP2)
          ITHH=IP1
          ITLH=IP2
        ELSE
          EHH=XNOS(IP2)
          ELH=XNOS(IP1)
          ITHH=IP2
          ITLH=IP1
        ENDIF
        IPDON=2
      ENDIF
C...SET LO INFO
      IF(IVDON.EQ.1) THEN
        IF(XNOS(IV1).GT.XNOS(IV2)) THEN
          ELL=XNOS(IV2)
          EHL=XNOS(IV1)
          ITLL=IV2
          ITHL=IV1
        ELSE
          ELL=XNOS(IV1)
          EHL=XNOS(IV2)
          ITLL=IV1
          ITHL=IV2
        ENDIF
        IVDON=2
      ENDIF
      IF(IPDON.EQ.2.AND.IVDON.EQ.2) GO TO 200
  100 CONTINUE
 200  IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END





















