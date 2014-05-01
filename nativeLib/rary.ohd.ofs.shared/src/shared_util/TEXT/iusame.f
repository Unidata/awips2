C MEMBER IUSAME
C  (from old member FCIUSAME)
C
C     DESC - INTEGER FUNCTION WHICH DETERMINES IF TWO
C     DESC - DIMENSIONED VARIABLES ARE IDENTICAL.
C     DESC - USUALLY USED TO COMPARE TWO NAMES, BUT
C     DESC - COULD BE USED TO COMPARE ANY ARRAYS.
C
      INTEGER FUNCTION IUSAME(IV1,IV2,N)
C
C     ARGUMENT LIST
C
C        IV1 - FIRST ARRAY
C        IV2 - SECOND ARRAY
C        N   - NUMBER OF WORDS IN IV1 AND IV2 TO BE COMPARED
C
C     VALUE OF FUNCTION
C
C        IUSAME=0, ARRAYS NOT THE SAME
C        IUSAME=1, ARRAYS IDENTICAL
C.......................................................................
C
      DIMENSION IV1(N),IV2(N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/iusame.f,v $
     . $',                                                             '
     .$Id: iusame.f,v 1.1 1995/09/17 19:24:11 dws Exp $
     . $' /
C    ===================================================================
C
C
      IUSAME=0
      DO 10 I=1,N
      IF(IV1(I).NE.IV2(I))GO TO 20
   10 CONTINUE
      IUSAME=1
   20 RETURN
      END
