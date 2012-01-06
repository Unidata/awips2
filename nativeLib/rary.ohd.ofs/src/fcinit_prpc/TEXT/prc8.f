C MEMBER PRC8
C  (from old member FCPRC8)
C.......................................................................
      SUBROUTINE PRC8(PLOSS,CLOSS)
C
C     SUBROUTINE PRINTS THE CARRYOVER STORED IN THE CLOSS ARRAY
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE   APRIL 1981
C.......................................................................
      INTEGER PEDATA
      DIMENSION PLOSS(1),CLOSS(1),P8(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fconit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc8.f,v $
     . $',                                                             '
     .$Id: prc8.f,v 1.1 1995/09/17 18:49:52 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA P8/4HPRC8,4H    /
C.......................................................................
C     CHECK TRACE LEVEL
      CALL FPRBUG(P8,1,8,IBUG)
C.......................................................................
      WSAREA=PLOSS(11)
      IF(WSAREA.LE.0.)GO TO 20
      PEDATA=PLOSS(12)
      IF(PEDATA.EQ.0)GO TO 20
      ICRY=PLOSS(17)
      IF(IVALUE.EQ.0)GO TO 5
      IF(ICRY.EQ.0)GO TO 10
  5   WRITE(IPR,900)CLOSS(1)
 900  FORMAT(1H ,10X,42H**CARRYOVER** PE VALUE FOR LAST DAY(LDA) =,F5.3,
     1 /25X,72HNOTE: IF LHR LESS THAN 24, CARRYOVER VALUE IS PE OF PREVI
     2OUS DAY(LDA-1).)
      GO TO 20
C
  10  WRITE(IPR,910)
 910  FORMAT(1H0,10X,50HA DEFAULT CARRYOVER PE VALUE EQUAL TO 0.0 IS USE
     1D.)
  20  CONTINUE
      RETURN
      END
