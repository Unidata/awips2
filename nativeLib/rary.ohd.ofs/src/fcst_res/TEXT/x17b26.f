C MEMBER X17B26
C  (from old member FCX17B26)
C
      SUBROUTINE X17B26(Q,DQ,NBLND,NSTEPS,IB,IE,IFB)
C.......................................................................
C     SUBROUTINE ADJUSTS SIMULATED DISCHARGES FOR THE DIFFERENCE
C     BETWEEN SIMULATED AND OBSERVED AT THE LAST OBSERVED ORDINATE
C     BY BLENDING THE DIFFERENCE IN A PRE-DEFINED NUMBER OF STEPS.
C.......................................................................
C     PROGRAMMED BY JOE OSTROWSKI - HRL - SEPT 1984
C     ADAPTED FROM ROUTINE 'FBLEND' ORIGINALLY PROGRAMMED BY KAY KROUSE
C.......................................................................
C     VARIABLES IN ARGUMENT LIST:
C             Q      -SIMULATED DISCHARGE ARRAY TO BE ADJUSTED
C             DQ     -DIFFERENCE BETWEEN SIMULATED AND OBSERVED
C                     DISCHARGE AT THE LAST OBSERVED ORDINATE
C             NBLND  -NUMBER OF STEPS IN TOTAL BLEND THAT HAVE BEEN
C                     COMPLETED IN A PREVIOUS BLEND
C             NSTEPS -TOTAL NUMBER OF BLENDING STEPS
C             IB     -FIRST ORDINATE OF Q ARRAY TO BE ADJUSTED
C             IE     -LAST AVAILABLE ORDINATE IN Q ARRAY
C             IFB    -FORWARD/BACKWARD BLEND SWITCH (0 - FWD, 1 - BACK)
C.......................................................................
C
      INCLUDE 'common/xco26'
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcary'
      DIMENSION Q(1),FWDBCK(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/x17b26.f,v $
     . $',                                                             '
     .$Id: x17b26.f,v 1.1 1995/09/17 19:06:17 dws Exp $
     . $' /
C    ===================================================================
C
      DATA FWDBCK/4HFWD ,4HBACK/
C
      IF (IBUG .GE. 1) WRITE(IODBUG,600)
  600 FORMAT('   *** ENTER X17B26 ***')
C
      LAST=IB+NSTEPS-NBLND-1-IFB
      IF(LAST.LE.IE) GO TO 5
      NB=NSTEPS-(LAST-IE)
      LAST=IE
      GO TO 10
 5    NB=0
C
 10   M=NSTEPS-NBLND
C
      IF (IBUG .GE. 2) WRITE(IODBUG,610) IB,LAST,FWDBCK(IFB+1),DQ
  610 FORMAT('     START AND END PERIODS = ',2I5,'FORWARD OR BACKWARD',
     .' BLEND? => ',A4,' DIFF. TO BLEND = ',F12.3)
C
C
      IF (IB.EQ.LAST .AND. IFB.EQ.1) GO TO 25
C
      DO 20 I=IB,LAST
      K=I-IB+1
      MFACT = M-K
      IF (IFB .EQ. 1) MFACT = NBLND + K
C
      IF (IBUG .GE. 2) WRITE(IODBUG,620) Q(I)
  620 FORMAT('  PRE-BLEND DISCHARGE = ',F12.3)
C
      Q(I)=Q(I)+(DQ/NSTEPS)*MFACT
C
      IF (IBUG .GE. 2) WRITE(IODBUG,630) Q(I)
  630 FORMAT('  POST-BLEND DISCHARGE = ',F12.3)
C
      IF(Q(I).LT.0.0)Q(I)=0.0
C
C  LOOK TO SAVE CARRYOVER FOR FORWARD BLENDS
C
       IF (IFB.EQ.1 .OR. ISAVCO.EQ.0 ) GO TO 20
C
C  LOOP THROUGH CARRY SAVE POSITIONS TO SEE IF WE'RE ON ONE
C
      DO 15 J=1,NCSTOR
      IF (ICOPOS(J) .EQ. I) GO TO 17
   15 CONTINUE
      GO TO 20
C
C  SAVE VALUES IN CB /XCO26/
C
   17 CONTINUE
      ADJCO(1,J) = DQ
      ADJCO(2,J) = K + 0.01
 20   CONTINUE
      NBLND=NB
 25   CONTINUE
C
      IF (IBUG .GE. 1) WRITE(IODBUG,699)
  699 FORMAT('    *** EXIT X17B26 ***')
C
      RETURN
      END
