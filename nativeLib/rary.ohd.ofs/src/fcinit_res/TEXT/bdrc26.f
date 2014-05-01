C MEMBER BDRC26
C  (from old member FCBDRC26)
C
C DESC DETERMINE UPPER AND LOWER ELEVATIONS IN RULE CURVE
C---------------------------------------------------------------------
      SUBROUTINE BDRC26(WORK,LOC,ELUPR,ELOWR,IERC)
C---------------------------------------------------------------------
C  ROUTINE TO SCAN THE RULE CURVE REFERRED BY 'CODE' TO RETURN THE
C  HIGHEST AND LOWEST ELEVATIONS IN THE CURVE
C---------------------------------------------------------------------
C
      INCLUDE 'common/mult26'
C
      DIMENSION WORK(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/bdrc26.f,v $
     . $',                                                             '
     .$Id: bdrc26.f,v 1.1 1995/09/17 18:51:12 dws Exp $
     . $' /
C    ===================================================================
C
C
      IERC = 1
C
      IF (LOC.GT.0) GO TO 10
C
      CALL STRN26(65,1,MULTNM(1,1),3)
      GO TO 9999
C
   10 CONTINUE
C
C  GET WORD POSITION IN WORK OF LOCATION OF RULECURVE
C
      ELUPR = -999999.
      ELOWR = 999999.
C
      NHALF = WORK(LOC)
      DO 40 I=1,NHALF
      ELUPR = AMAX1(ELUPR,WORK(I+NHALF+LOC))
      ELOWR = AMIN1(ELOWR,WORK(I+NHALF+LOC))
   40 CONTINUE
C
      IERC = 0
C
 9999 CONTINUE
      RETURN
      END
