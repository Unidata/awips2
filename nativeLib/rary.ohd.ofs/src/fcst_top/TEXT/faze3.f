C MEMBER FAZE3
C  (from old member FCFAZE3)
C
C     DESC - CLEANUP OF EXECUTION RUN - CALL FSPOOL TO SAVE CO
C            IF SAVE CO RUN AND NO ERRORS (WARNINGS ALLOWED)
C
C.................................
      SUBROUTINE FAZE3
C..................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY
C              GEORGE F SMITH - HRL - 25 MARCH 1980
C.................................................
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcary'
      INCLUDE 'common/where'
      INCLUDE 'common/killcd'
C
      DIMENSION SBNAME(2),OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/faze3.f,v $
     . $',                                                             '
     .$Id: faze3.f,v 1.1 1995/09/17 19:08:06 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HFAZE,4H3   /
C
C               WHERE
C.........................
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C........................
C
C               TRACE LEVEL = 1
C..................
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** FAZE3 ENTERED)
C.....................
C
      ICHK=IFILLC*NCSTOR
C
      IF(ICHK.GT.0.AND.KLCODE.LE.4)CALL FSPOOL
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
