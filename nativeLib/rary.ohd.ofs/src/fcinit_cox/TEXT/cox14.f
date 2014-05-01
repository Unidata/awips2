C MEMBER COX14
C  (from old member FCCOX14)
C.......................................................................
      SUBROUTINE COX14(POLD,COLD,PONEW,CONEW)
C
C     SUBROUITNE ADJUSTS AND TRANSFERS OLD CARRYOVER VALUES,
C     WHENEVER POSSIBLE, FOR CHANGES MADE TO PARAMETER VALUES.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE    JULY 1980
C.......................................................................
      INTEGER  DTOLD,DTNEW
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1),C14(2)
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox14.f,v $
     . $',                                                             '
     .$Id: cox14.f,v 1.1 1995/09/17 18:47:11 dws Exp $
     . $' /
C    ===================================================================
C
      DATA C14/4HCOX1,4H4   /
C.......................................................................
C     CHECK TRACE LEVEL
      CALL FPRBUG(C14,1,14,IBUG)
C.......................................................................
C     CHECK PARAMETER ARRAYS FOR CHANGES IN TIME INTERVAL OF
C     SIMULATED DISCHARGE DATA.
      DTOLD=POLD(12)
      DTNEW=PONEW(12)
      IF(DTOLD.EQ.DTNEW)GO TO 75
C     ARE DTOLD AND DTNEW EVEN MULTIPLES OF EACH OTHER?
      IPASS=0
      IT1=DTOLD
      IT2=DTNEW
  5   NOMX=0
      MX=24/IT1
      DO 10 J=1,MX
      JX=J*IT1
      IF(IT2.EQ.JX)NOMX=1
 10   CONTINUE
      IF(NOMX.EQ.1) GO TO 20
      IF(IPASS.EQ.1) GO TO 15
      IPASS=1
      IT1=DTNEW
      IT2=DTOLD
      GO TO 5
C.......................................................................
C     DTOLD AND DTNEW ARE NOT EVEN MULTIPLES OF EACH OTHER.
 15   IOMO=POLD(8)
      IOMN=PONEW(8)
      IF((IOMO.NE.1).OR.(IOMN.NE.1))GO TO 100
      WRITE(IPR,900)DTOLD,DTNEW
 900  FORMAT(1H0,10X,28H**WARNING** NEITHER THE OLD(, I2,20H HOURS) NOR
     1THE NEW(,I2,39H HOURS) SIMULATED DISCHARGE TIME SERIES/,23X,
     2 87HTIME INTERVAL IS AN EVEN MULTIPLE OF THE OTHER. NO DISCHARGE V
     3ALUES CAN BE TRANSFERRED.)
      CALL WARN
      GO TO 100
 20   IF(IPASS.EQ.1)GO TO 50
C.......................................................................
C     DTNEW IS AN EVEN MULTIPLE OF DTOLD--SELECT DISCHARGE VALUES
C     FROM THE OLD CARRYOVER AT THE NEW TIME INTERVAL.
      NVALU=24/DTNEW+1
      NX=DTNEW/DTOLD
      DO 25 I=1,NVALU
      J=(NX*I)-NX+1
      CONEW(I)=COLD(J)
 25   CONTINUE
      GO TO 100
C.......................................................................
C     DTOLD IS AN EVEN MULTIPLE OF DTNEW--OBTAIN NEW CARRYOVER DISCHARGE
C     VALUES BY LINEARLY INTERPOLATING BETWEEN OLD VALUES.
 50   NVO=24/DTOLD
      NX=DTOLD/DTNEW
      DO 40 I=1,NVO
      IB=(NX*I)-NX+1
      IE=IB+NX-1
      DO 35 J=IB,IE
      IF(J.EQ.IB)GO TO 30
      DIFF=COLD(I+1)-COLD(I)
      ADJ=(DIFF/NX)*(J-IB)
      CONEW(J)=COLD(I)+ADJ
      GO TO 35
 30   CONEW(J)=COLD(I)
 35   CONTINUE
 40   CONTINUE
      CONEW(IE+1)=COLD(NVO+1)
      GO TO 100
C.......................................................................
C     DTOLD AND DTNEW ARE EQUAL. TRANSFER OLD CARRYOVER DISCHARGE
C     AS IS INTO NEW CARRYOVER ARRAY.
 75   NVALU=24/DTOLD+1
      DO 80 I=1,NVALU
      CONEW(I)=COLD(I)
 80   CONTINUE
C.......................................................................
C.......................................................................
 100  IOIOLD=POLD(7)
      IOINEW=PONEW(7)
      NO=24/DTOLD+1
      NN=24/DTNEW+1
      IF(IOINEW.EQ.0)GO TO 150
      IF(IOIOLD.EQ.0)GO TO 150
      NSOLD=POLD(17)
      NSNEW=PONEW(17)
      NBOLD=COLD(NO+2)
      LC=NBOLD*DTOLD
      LN=NSNEW*DTNEW
      IF(LC.GE.LN)GO TO 150
      CONEW(NN+1)=COLD(NO+1)
      CONEW(NN+3)=COLD(NO+3)
      IF(NBOLD.NE.0)GO TO 125
      CONEW(NN+2)=COLD(NO+2)
      GO TO 200
 125  FTO=DTOLD
      FTN=DTNEW
      RBOLD=NBOLD
      NBNEW=(RBOLD*(FTO/FTN))+0.5
      IF(NBNEW.EQ.LC)NBNEW=NBNEW-1
      CONEW(NN+2)=NBNEW+.01
      GO TO 200
 150  CONEW(NN+1)=0.
      CONEW(NN+2)=0+.01
      CONEW(NN+3)=0.
 200  CONTINUE
      RETURN
      END
