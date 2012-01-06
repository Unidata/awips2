C***********************************************************************
C                                                                      *
C         MEMBER URHGTU                                                *
C                                                                      *
C***********************************************************************
       SUBROUTINE URHGTU(NAMES,NUMNAM,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  URHGTU                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  11-18-85                                       *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS ROUTINE PARSES THE USER INPUT CARD(S) FOR THE RFC USERS      *
C    WHOSE GLOBAL AND GLOBAL DEFAULT FILES WILL BE COMPRESSED IN       *
C    THE REORDER PROGRAM.                                              *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       NAMES      A     O    2X50  ARRAY CONTAINING USER NAMES        *
C       NUMNAM     I     O     1    NUMBER OF USER NAMES IN NAMES      *
C       ISTAT      I     O     1    STATUS CODE
C                                    0 = NORMAL RETURN
C                                    1 = SYSTEM ERROR
C                                    2 = USER ID"S NOT IN PAIRS
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      INTEGER NAMES(2,50),ICARD(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urhgtu.f,v $
     . $',                                                             '
     .$Id: urhgtu.f,v 1.1 1995/09/17 19:17:47 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA LUSER/4hUSER/,LEND/4hEND /
C                                                                      *
C***********************************************************************
C
C
      NUMNAM = 0
      CALL UMEMST(IBLNK,NAMES,100)
  100 CONTINUE
      CALL RWCARD(ISTAT)
      IF(ISTAT.NE.0) GO TO 900
      IF = 1
      NUM = IFSTOP(IF) - IFSTRT(IF) + 1
      IF(NUM.GT.8) NUM = 8
      CALL UPACK1(IBUF(IFSTRT(IF)),ICARD,NUM)
      IF(ICARD(1).EQ.LUSER) GO TO 125
      IF(ICARD(1).EQ.LEND) GO TO 250
      NUMNAM = NUMNAM + 1
      CALL UMEMOV(ICARD,NAMES(1,NUMNAM),2)
  125 CONTINUE
      N = NUMNAM + 1
      NUMNAM = NUMNAM + NFIELD - 1

      DO 200 I = N,NUMNAM
      IF = IF + 1
      NUM = IFSTOP(IF) - IFSTRT(IF) + 1
      IF(NUM.GT.8) NUM = 8
      CALL UPACK1(IBUF(IFSTRT(IF)),NAMES(1,I),NUM)
      IF(NAMES(1,I).EQ.LEND) GO TO 225
      IF(NOBUG.GT.0) WRITE(LPD,130) NAMES(1,I),NAMES(2,I)
  130 FORMAT(' USER ID IS ',2A4)
  200 CONTINUE
C
C   GO GET NEXT CARD
C
      GO TO 100
  225 CONTINUE
C
C  RESET COUNTER
C
      NUMNAM = NUMNAM - 1
  250 CONTINUE
      IF(NOBUG.GT.0) WRITE(LPD,260) ((NAMES(J,K),J=1,2),K=1,10)
  260 FORMAT(' NAMES = ',10(2A4))
      IF(MOD(NUMNAM,2).EQ.0) GO TO 999
      WRITE(LPE,275)
  275 FORMAT(' **ERROR** IN URHGTU. INVALID INPUT CARD. USER ID"S MUST',
     1       ' BE IN PAIRS.')
      ISTAT = 2
      GO TO 999
  900 CONTINUE
C
C  SYSTEM ERROR
C
      WRITE(LPE,910)
  910 FORMAT(' **ERROR** IN URHGTU. READING INPUT CARD.')
      ISTAT = 1
  999 CONTINUE
      IF(NOBUG.GT.0) WRITE(LPE,1000) NUMNAM
 1000 FORMAT(' NUMNAM = ',I5)
      RETURN
      END
