C MEMBER MYZC40
C  (from old member MCEX40)
C
      SUBROUTINE MYZC40(PO,C,MC,LOCCL,NOSUB,MOWY,MONTH,KYEAR,IBUG)
C***************************************
C
C     THIS SUBROUTINE COMPUTES THE OPTIONAL MULTI-YEAR AVERAGE, HI AND
C     LO ZONE CONTENTS AND WRITES CURRENT VALUES TO THE SCRATCH FILE.
C     THE MULTI-YEAR AVERAGE IS NOT COMPUTED UNTILL THE END OF THE
C     CALIBRATION PERIOD.
C
C     THIS SUBROUTINE WAS INITIALLY WRITTEN BY:
C     ROBERT M. HARPER     HRL      APRIL, 1991
C***************************************
C
      DIMENSION PO(1),C(MC),LOCCL(1),TABLE(16,12),SNAME(2)
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FWYDS/IRWY,NXWY,NTWY,IDEFWY
      COMMON/CALBRT/IMO,IYR,LMO,LYR
      COMMON/FWYDAT/WY(372)
C
      DATA SNAME/'MYZC','40  '/
C
      EQUIVALENCE(WY,TABLE)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_waterbal/RCS/myzc40.f,v $
     . $',                                                             '
     .$Id: myzc40.f,v 1.1 1996/02/14 15:17:13 dws Exp $
     . $' /
C    ===================================================================
C
C***************************************
C
C     TRACE LEVEL=1
      IF(ITRACE .GE. 1) WRITE(IODBUG,900) SNAME
  900 FORMAT(/1X,'** ',2A4,' ENTERED')
C***************************************
C
C     SET INITIAL VALUES
      LZN=PO(17)
C
C     BEGIN MAIN SUBAREA LOOP FOR CURRENT MONTH.
      DO 10 M=1,NOSUB
        LCL=LOCCL(M)
        NREC=LZN+M-1
        READ(IRWY,REC=NREC) WY
        DO 20 J=1,16
          IF (J .EQ. 16) GO TO 30
C
C         SET OFFSET (N=) TO LOCATE UZTWC, UZFWC, LZTWC, LZFSC,LZFPC IN
C         THE CL ARRAY.
          IF((J .GE. 1) .AND. (J .LE. 3)) N=0
          IF((J .GE. 4) .AND. (J .LE. 6)) N=1
          IF((J .GE. 7) .AND. (J .LE. 9)) N=2
          IF((J .GE. 10) .AND. (J .LE. 12)) N=3
          IF((J .GE. 13) .AND. (J .LE. 15)) N=4
C
          IF((J .EQ. 1) .OR. (J .EQ. 4) .OR. (J .EQ. 7) .OR. (J .EQ. 10)
     & .OR. (J .EQ. 13)) GO TO 40
          GO TO 50
C
C         SUM MONTHLY VALUES IN THE AVG COLUMN FOR EACH OCCURANCE OF THE
C         CURRENT MONTH.
   40     TABLE(J,MOWY)=TABLE(J,MOWY)+C(LCL+N)
          GO TO 20
C
   50     IF(J .NE. ((J/3)*3)) GO TO 60
          GO TO 70
C
C         CHECK TO UPDATE THE HIGHEST VALUE OF EACH OF THE WATER
C         CONTENTS FOR EACH OCCURANCE OF THE CURRENT MONTH.
   60     IF(TABLE(J,MOWY) .GE. C(LCL+N)) GO TO 20
          TABLE(J,MOWY)=C(LCL+N)
          GO TO 20
C
C         CHECK TO UPDATE THE LOWEST VALUE OF EACH OF THE WATER
C         CONTENTS FOR EACH OCCURANCE OF THE CURRENT MONTH.
   70     IF(TABLE(J,MOWY) .LE. C(LCL+N)) GO TO 20
          TABLE(J,MOWY)=C(LCL+N)
          GO TO 20
C
C         COUNT THE NUMBER OF TIMES THE CURRENT MONTH OCCURS IN THE
C         CALIBRATION PERIOD.
   30     TABLE(J,MOWY)=TABLE(J,MOWY)+1
   20   CONTINUE
        IF((MONTH .EQ. LMO) .AND. (KYEAR .EQ. LYR)) GO TO 80
        GO TO 120
C
C       THE LAST MONTH OF THE CALIBRATION PERIOD HAS BEEN PROCESSED.
C       COMPUTE THE AVERAGE WATER CONTENTS.
   80   DO 100 J=1,13,3
          DO 110 I=1,12
            TABLE(J,I)=TABLE(J,I)/TABLE(16,I)
  110     CONTINUE
  100   CONTINUE
C***************************************
C
C     DEBUG OUTPUT-CONTENTS OF WY() CONTAINING MULTI-YEAR ZONE CONTENTS
C     INFORMATION.
  120 IF(IBUG .EQ. 0) GO TO 90
      IF(M .GT. 1) GO TO 130
      WRITE(IODBUG,800)
  800 FORMAT(/5X,'MYZC40 DEBUG OUTPUT-CONTENTS OF WY() CONTAINING MULTI-
     &YEAR ZONE CONTENTS INFORMATION FOR EACH SUBAREA.')
  130 WRITE(IODBUG,810) M
  810 FORMAT(/,5X,'CONTENTS OF THE WY() FOR SUBAREA NO.',I5)
      WRITE(IODBUG,820) (WY(I),I=1,192)
  820 FORMAT(5X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,
     &F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,F3
     &.0)
C***************************************
C
C       WRITE CURRENT VALUES BACK TO THE WATER YEAR SCRATCH FILE.
   90   WRITE(IRWY,REC=NREC) WY
   10 CONTINUE
C***************************************
C
      RETURN
      END
