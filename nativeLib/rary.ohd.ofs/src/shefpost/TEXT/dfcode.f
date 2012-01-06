C MEMBER DFCODE
C  (from old member DFPOSTUT)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C
C                             LAST UPDATE: 03/21/94.16:37:26 BY $WC21BJ
C
       SUBROUTINE DFCODE (ISHBUF,ISCODE,ITRNCK)
C
C          SUBROUTINE:  DFCODE
C
C             VERSION: 1.0.1  CHANGED SUBSCRIPTS OF CODE  1-28-85
C             VERSION:  1.0.0
C
C                DATE:  12-13-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS SUBROUTINE READS THE SHEFOUT BUFFER TO OBTAIN THE ELEMENTS
C    OF THE SHEF PARAMETER CODE. THIS WILL THEN BE MATCHED TO THE
C    EXPANDED PARAMETER CODE LOCATED IN THE TRANSLATION TABLE TO
C    DETERMINE THE PPDB DATA TYPE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISHBUF     I    I      28   SHEFOUT RECORD BUFFER
C       ISCODE     A    O      2    SHEF PARAMETER CODE
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ISCODE(2),ISHBUF(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfcode.f,v $
     . $',                                                             '
     .$Id: dfcode.f,v 1.1 1995/09/17 19:26:12 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA JBLNK/4H    /,LETZ/4HZ   /
      DATA LETM/4HM   /,LETN/4HN   /
      DATA LETG/4HG   /,LETP/4HP   /
C
C***********************************************************************
C
C
C
C  DEBUG
C
      IF(NOBUG.EQ.1) WRITE(LP,100)
  100 FORMAT(' ENTER DFCODE')
C
      ITRNCK=0
      CALL UMEMST(JBLNK,ISCODE,2)
C
C  MOVE SHEF EXPANDED CODE FROM RECORD INTO ARRAY
C
      K = 1
      DO 200 I = 15,20
       CALL UMOVEX(ISHBUF(I),1,ISCODE(1),K,1)
       K = K + 1
  200 CONTINUE
C
C  MOVE IN PROBABILITY CODE AND SET SOURCE CODE TO Z
C
      IF (ISHBUF(19).EQ.LETG.OR.ISHBUF(19).EQ.LETP) ITRNCK=1
      IF (ISHBUF(19).EQ.LETM.AND.ISHBUF(20).EQ.LETN) GO TO 300
      CALL UMOVEX(LETZ,1,ISCODE(2),1,1)
300   CALL UMOVEX(LETZ,1,ISCODE(2),3,1)
C
C  GET SHEF CODE FOR ENCODED DURATION VALUE
C
      CALL DFFDUR(ISHBUF(17),K)
C
C  MOVE INTO SHEF PARAMETER ARRAY
C
      CALL UMOVEX(K,1,ISCODE(1),3,1)
C
C  FINISHED - RETURN WITH EXPANDED SHEF CODE
C
  900 CONTINUE
C
      IF(NOBUG.EQ.1) WRITE(LP,925)
  925 FORMAT(' EXIT DFCODE')
  999 CONTINUE
C
      RETURN
C
      END
