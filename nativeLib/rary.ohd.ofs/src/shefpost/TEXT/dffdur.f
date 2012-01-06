C MEMBER DFFDUR
C  (from old member DFPOSTUT)
C----------------------------------------------------------------------
C
      SUBROUTINE DFFDUR (IDUR,K)
C
C      THIS SUBROUTINE SEARCHES FOR THE SHEF EXPANDED PARAMETER CODE
C      CORRESPONDING TO THE ENCODED DURATION CODE.
C
C      NAME    TYPE   I/O     DIM      DESCRIPTION
C
C      IDUR     I      I       1        ENCODED DURATION VALUE
C        K      A1     O       1        SHEF CODE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'udsi'
C
C***********************************************************************
C
      DIMENSION IDTBL(2,18),ILETR(18)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dffdur.f,v $
     . $',                                                             '
     .$Id: dffdur.f,v 1.1 1995/09/17 19:26:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA IDTBL/0,1,1001,2,1003,3,1006,4,2001,5,5004,6,
     *           1,7,15,8,30,9,1002,10,1004,11,1008,12,
     *           1012,13,1018,14,2007,15,3001,16,4001,17,5005,18/
      DATA ILETR/4hI   ,4hH   ,4hT   ,4hQ   ,4hD   ,4hP   ,
     *           4hU   ,4hC   ,4hJ   ,4hB   ,4hF   ,4hA   ,
     *           4hK   ,4hL   ,4hW   ,4hM   ,4hY   ,4h?   /
C
C
C***********************************************************************
C
C      FIND DURATION CODE IN ARRAY
C
      DO 200 I = 1,18
         IF(IDUR.EQ.IDTBL(1,I)) GO TO 250
  200    CONTINUE
      I=18
  250 ISAV = IDTBL(2,I)
C
C  GET CODE FOR DURATION CODE
C
      K = ILETR(ISAV)
C
  999 CONTINUE
C
      RETURN
C
      END
