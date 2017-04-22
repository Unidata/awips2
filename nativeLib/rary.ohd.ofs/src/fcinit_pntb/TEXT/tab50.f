C MEMBER TAB50
C  (from old member FCTAB50)
C
C @PROCESS LVL(77)
C                             LAST UPDATE: 04/25/95.13:32:27 BY $WC20SV
C
      SUBROUTINE TAB50( P, MP, TO, LEFT, IUSET, NXT, LPO, PO,
     1 TS, MTS, NWORK, NDD, LWORK, IDT )

C
C    THIS IS THE TAB ROUTINE FOR THE ASSIMILATOR
C    OPERATION.
C
C    ROUTINE INITIALLY WRITTEN BY
C    ERIC MARKSTROM, RIVERSIDE TECHNOLOGY, INC. DECEMBER 1994 VERSION 1
C
C  POSITION        NAME     DESCRIPTION
C     1            50       OPERATION NUMBER
C     2            LENGTH   A POINTER INDICATING WHERE IN THE T ARRAY
C                  + NXT    THE ENTRY FOR THE NEXT OPERATION TO BE
C                           EXECUTED BEGINS
C     3            LPO      STARTING POSITION OF THE PO ARRAY
C     4            0        STARTING POSITION OF THE CO ARRAY
C     5            NBASINS  NUMBER OF SUB BASINS
C     6            LOCD     ARRAY OF POINTERS TO RAINFALL RUNOFF
C                           RAIN + MELT T.S.'S
C     6+NBASINS    R_IDT    ARRAY OF TIME STEPS FOR RAINFALL RUNOFF
C                           RAIN + MELT T.S.'S
C     6+NBASINS*2           ARRAY OF POINTERS TO RAINFALL RUNOFF
C                           RAIN + MELT T.S.'S PO LOCATIONS
C     6+NBASINS*3           ARRAY OF POINTERS TO RAINFALL RUNOFF
C                           RAIN + MELT T.S.'S PO LOCATIONS
C
C     6+NBASINS*4  LOCD     POINTER TO LOCATION OF SIMULATED Q T.S.
C     7+NBASINS*4  LOCD     POINTER TO LOCATION OF OBSERVED Q T.S.
C     8+NBASINS*4  LOCD     ARRAY OF POINTERS TO KP T.S.'S
C     8+NBASINS*5  NWORK    POINTER TO STARTING LOCATION IN D
C                           ARRAY WHICH MAY BE USED FOR WORK
C
C    PASSED ARGUMENTS

      INTEGER TO(*)
      DIMENSION P(MP), PO(*), TS(*)
      INTEGER LEFT, IUSET, NXT, LPO, LCO, MTS, NWORK,
     1 NDD, LWORK, IDT

      INCLUDE 'common/fcassm'
      INCLUDE 'common/ionum'

      INCLUDE 'common/fdbug'

C    LOCAL VARIABLES
      DIMENSION SACID(2), R_ID(2), SNAME(2),ANAME(2)
      REAL R_DT
      INTEGER R_IDT, ITAB50, IBUG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab50.f,v $
     . $',                                                             '
     .$Id: tab50.f,v 1.1 1995/09/17 18:49:27 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SACID /4HSAC-,4HSMA /
      DATA RKPDT /4HKP  /
      DATA SNAME /4HTAB5,4H0   /
      DATA ANAME /4HASSI,4HM   /

CCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC  DEBUG INITIALIZATION
C
      CALL FOPCDE(ANAME, ITAB50)
      CALL FPRBUG(SNAME, 1, ITAB50, IBUG )


      NBASINS = PO(9)
      IDT = 24

      LENGTH = 8 + NBASINS*5

      CALL CHECKT( LENGTH, LEFT, IERR )
      IF (IERR.GE.1) THEN
          WRITE(IPR, 901)
  901     FORMAT( 1H0, 10X, 9H**ERROR**,
     1 42HTHERE IS INSUFFICIENT SPACE IN THE T ARRAY,
     2 19HFOR THIS OPERATION. )
          CALL ERROR
          RETURN
      END IF

      IUSET = LENGTH
      TO(1) = 50
      TO(2) = LENGTH + NXT
      TO(3) = LPO
      TO(4) = 0
      TO(5) = NBASINS
C     LOCATION OF RAINFALL RUNOFF RAIN + MELT T.S.
C     AND TIME INTERVALS FOR EACH T.S.
       DO I = 1, NBASINS
C        CHECK IF RR OPERATION IS SAC-SMA
         IF ((PO(INT(PO(24))+(I-1)*2).EQ.SACID(1)).AND.
     1 (PO(INT(PO(24))+(I-1)*2+1).EQ.SACID(2))) THEN
C          RAIN + MELT INFORMATION
           R_ID(1) = P(INT(PO(INT(PO(26))+I-1))+7)
           R_ID(2) = P(INT(PO(INT(PO(26))+I-1))+8)
           R_DT = P(INT(PO(INT(PO(26))+I-1))+9)
           R_IDT = P(INT(PO(INT(PO(26))+I-1))+1)
           CALL CKINPT(R_ID, R_DT, R_IDT, LOCD, TS, MTS, IERR )
           TO(5+I) = LOCD
           TO(5+NBASINS+ I) = R_IDT
           TO(5+NBASINS*2 + I) = PO(INT(PO(26)) + I-1)
           TO(5+NBASINS*3 + I) = P(INT(PO(INT(PO(27)) + I-1)))
           IF (IBUG.GE.1) THEN
             WRITE(IODBUG,801) R_ID, R_DT, R_IDT
  801        FORMAT(1H0,39HRAINFALL RUNOFF RAIN + MELT TIME SERIES,
     1 5X, 2HID, 2X, 2A4, 2X, 4HTYPE, 2X, 1A4, 2X, 3HIDT, I2 )
           END IF

C         KP TIME SERIES
            R_ID(1) = PO(INT(PO(28))+ (I-1)*2)
            R_ID(2) = PO(INT(PO(28))+ (I-1)*2 + 1)
            R_IDT = 24
            CALL CKINPT(R_ID, RKPDT, R_IDT, LOCD, TS, MTS, IERR )
            TO(7 + NBASINS*4 + I) = LOCD

            IF (IBUG.GE.1) THEN
              WRITE(IODBUG,802) R_ID, RKPDT, R_IDT
  802         FORMAT(1H0, 14HKP TIME SERIES,
     1 5X, 2HID, 2X, 2A4, 2X, 4HTYPE, 2X, 1A4, 2X, 3HIDT, I2 )
            END IF

         END IF
      END DO
C    SIMULATED TS
      R_ID(1) = PO(18)
      R_ID(2) = PO(19)
      R_DT = PO(20)
      R_IDT = 24
      CALL CKINPT( R_ID, R_DT, R_IDT, LOCD, TS, MTS, IERR)
      TO(6 + NBASINS * 4) = LOCD

      IF (IBUG.GE.1) THEN
        WRITE(IODBUG,803) R_ID, R_DT, R_IDT
  803   FORMAT(1H0, 21HSIMULATED TIME SERIES,
     1 5X, 2HID, 2X, 2A4, 2X, 4HTYPE, 2X, 1A4, 2X, 3HIDT, I2 )
      END IF


C    OBSERVED TS
      R_ID(1) = PO(15)
      R_ID(2) = PO(16)
      R_DT = PO(17)
      R_IDT = 24
      CALL CKINPT( R_ID, R_DT, R_IDT, LOCD, TS, MTS, IERR )
      TO(7 + NBASINS * 4) = LOCD

      IF (IBUG.GE.1) THEN
        WRITE(IODBUG,804) R_ID, R_DT, R_IDT
  804   FORMAT(1H0, 20HOBSERVED TIME SERIES,
     1 5X, 2HID, 2X, 2A4, 2X, 4HTYPE, 2X, 1A4, 2X, 3HIDT, I2 )
      END IF


C    LOCATION WHERE NWORK CAN START
      TO(8 + NBASINS * 5) = NWORK + LWORKMX

      IF (IBUG.GE.1) THEN
        WRITE(IODBUG,805) NWORK
  805   FORMAT(1H0, 5HNWORK,2X, I4)
      END IF

C    SIZE OF WORKSPACE NEEDED FOR ASSIMILATOR
      LWORK = (20 * NBASINS) + (NDD * NBASINS * 9) +
     1 (1+NDD)*NBASINS*(1+NDD)*NBASINS + 100 + LWORKMX

      IF (IBUG.GE.1) THEN
         WRITE(IODBUG,806) LWORK
  806    FORMAT(1H0, 5HLWORK, I4)
      END IF

      IF (IBUG.GE.1) THEN
         WRITE(IODBUG,807)(TO(I), I=1,IUSET)
  807    FORMAT(1H0, 14HCONTENTS OF TO, 37I6)
       END IF

      END
