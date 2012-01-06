C     MEMBER OPUGCK
C
      SUBROUTINE OPUGCK(P,MP,OA,MOA,A,MA,PARM,ILOCOA,MILOC,JB,UGH,
     *UGV,NPARM,OPNEW)
C
C.......................................
C     THIS SUBROUTINE CHECKS TO SEE WHICH ADJUSTMENT PARAMETER
C     CURRENTLY IS BEING OPTIMIZED AND IF THE OTHER PARAMETER ALSO
C     HAS BEEN INCLUDED FOR OPTIMIZATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   MAY 1981   VERSION 1
C.......................................
C
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'
C
      DIMENSION P(MP),OA(MOA),A(MA),PARM(2),OPNEW(2)
      DIMENSION ILOCOA(MILOC)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/opugck.f,v $
     . $',                                                             '
     .$Id: opugck.f,v 1.3 2006/03/16 16:40:11 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA RH/4HUGH /
      DATA RV/4HUGV /
      DATA BLNK/4H    /
      DATA IBUG/4HOPT /
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1000)
 1000 FORMAT(1H0,17H** OPUGCK ENTERED)
C
C     CHECK TO SEE WHICH PARAMETER IS BEING OPTIMIZED.
C
      JBP1=JB+1
      IF(PARM(1).EQ.RH.AND.PARM(2).EQ.BLNK) GO TO 150
C     UGV IS BEING OPTIMIZED.
      UGV=A(JB)
      IF(JB.EQ.NPARM) GO TO 121
C
C     CHECK TO SEE IF UGH ALSO IS INCLUDED.
C
C dws    The following loop is UGLY!! and causes compiler warnings.
C dws     Try a new version ... but for now ... 2006-01-23

C     DO 120 J=JBP1,NPARM
C     IOPNUM=OA(ILOCOA(J)+1)
C     IF(IOPNUM.NE.2.OR.P(OA(ILOCOA(J))-5).NE.OPNEW(1).OR.P(OA(ILOCOA(J)
C    *)-4).NE.OPNEW(2)) GO TO 120
C     IF(OA(ILOCOA(J)+2).NE.RH.OR.OA(ILOCOA(J)+3).NE.BLNK) GO TO
C    *120
C     JK=J
C     GO TO 122
C 120 CONTINUE
C 121 UGH=1.0
C     GO TO 180
C 122 UGH=A(JK)
C     GO TO 180

      DO 120 J=JBP1,NPARM
        JLOCOA = ILOCOA(J)
        KLOCOA = OA(JLOCOA)
        IOPNUM = OA(JLOCOA + 1)

        IF(      IOPNUM .NE. 2
     *      .OR. P(KLOCOA-5) .NE. OPNEW(1)
     *      .OR. P(KLOCOA-4) .NE. OPNEW(2) ) GO TO 120

        IF(      OA(JLOCOA+2) .NE. RH
     *      .OR. OA(JLOCOA+3) .NE. BLNK    ) GO TO 120

        JK=J
        GO TO 122
  120 CONTINUE

  121 UGH=1.0
      GO TO 180

  122 UGH=A(JK)
      GO TO 180
C
C     UGH IS BEING OPTIMIZED.
  150 UGH=A(JB)
      IF(JB.EQ.NPARM) GO TO 161
C     CHECK TO SEE IF UGV ALSO IS INCLUDED.
C
C dws    The following loop is UGLY!! and causes compiler warnings.
C dws     Try a new version ... but for now ... 2006-01-23

      DO 160 J=JBP1,NPARM
        JLOCOA = ILOCOA(J)
        KLOCOA = OA(JLOCOA)
        IOPNUM = OA(JLOCOA + 1)

        IF(      IOPNUM .NE. 2
     *      .OR. P(KLOCOA-5) .NE. OPNEW(1)
     *      .OR. P(KLOCOA-4) .NE. OPNEW(2) ) GO TO 160

        IF(      OA(JLOCOA+2) .NE. RV
     *      .OR. OA(JLOCOA+3) .NE. BLNK    ) GO TO 160

        JK=J
        GO TO 162
  160 CONTINUE

  161 UGV=1.0
      GO TO 180

  162 UGV=A(JK)

C     DO 160 J=JBP1,NPARM
C     IOPNUM=OA(ILOCOA(J)+1)
C     IF(IOPNUM.NE.2.OR.P(OA(ILOCOA(J))-5).NE.OPNEW(1).OR.P(OA(ILOCOA(J)
C    *)-4).NE.OPNEW(2)) GO TO 160
C     IF(OA(ILOCOA(J)+2).NE.RV.OR.OA(ILOCOA(J)+3).NE.BLNK) GO TO
C    *160
C     JK=J
C     GO TO 162
C 160 CONTINUE
C 161 UGV=1.0
C     GO TO 180
C 162 UGV=A(JK)

  180 CONTINUE
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1002)
 1002 FORMAT(1H0,14H** EXIT OPUGCK)
C
      RETURN
      END
