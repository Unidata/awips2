C MODULE HCKDTC
C-----------------------------------------------------------------------
C    ROUTINE TO CHECK FOR A VALID TIME CODE
C
C          ROUTINE:  HCKDTC
C             VERSION:  1.0.0
C                DATE:  10-15-81
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ICODE       I    I     1     INPUT TIME CODE
C       ITZC        I    O     1     TIME ZONE CODE OFFSET VALUE
C                                        IE EDT=4,EST=5,CDT=5,CST=6
C       ISTAT       I    O     1     STATUS INDICATOR
C                                        0=NORMAL
C                                        1=BAD CODE
C***********************************************************************
      SUBROUTINE HCKDTC (ICODE,ITZC,ISTAT)

      INCLUDE 'uio'
      INCLUDE 'udebug'

      DIMENSION IARR(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_hckdat/RCS/hckdtc.f,v $
     . $',                                                             '
     .$Id: hckdtc.f,v 1.2 1998/07/02 16:31:49 page Exp $
     . $' /
C    ===================================================================
C

      DATA IARR/4HEDT ,4HEST ,4HCDT ,4HCST ,4HMDT ,4HMST ,4HPDT ,
     *          4HPST ,4HADT ,4HAST ,4HHDT ,4HHST ,4HNDT ,4HNST ,
     *          4HZ   ,4HINTL,4H9999,4H9999,4H9999,4H9999/

      ISTAT=0
      DO 10 I=1,20
         IF (IARR(I).EQ.ICODE) GO TO 20
10       CONTINUE
      ISTAT=1
      ITZC=0
      GO TO 30

C MATCH

20    ITZC=(I+2)/2+3
      IF (I.EQ.15) ITZC=0
      IF (I.GE.16) ITZC=100

30    IF (IHCLTR.GT.1) WRITE (IOGDB,40) ISTAT,ITZC
40    FORMAT (' EXIT HCKDTC - ISTAT=',I2,' ITZC=',I4)

      RETURN
      END
