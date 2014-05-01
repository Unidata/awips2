C MEMBER FLWK51
C-------------------------------------------------------------------    
C
C@PROCESS LVL(77)
C
       SUBROUTINE FLWK51(STR1,IUSED,LEFT,VALUE,IERNUM)
C
C DESC STORES ONE VALUE IN AN ARRAY AND SEES IF AVAIL. SPACE EXCEEDED
C
C ARGUMENT LIST:
C   STR1   - ARRAY TO PLACE VALUE
C  IUSED   - SPACE ALREADY USED IN STR1
C  LEFT    - SPACE LEFT IN STR1
C  VALUE   - VALUE TO PLACE IN STR1
C IERNUM   - ERROR NUMBER TO CALL STER51 WITH IF SPACE EXCEEDED
C
C.....................................................................
C
C  KUANG HSU - HRL - OCTOBER 1994
C................................................................
      INCLUDE 'common/err51'
      INCLUDE 'common/comn51'
      INCLUDE 'common/fdbug'
      DIMENSION STR1(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/flwk51.f,v $
     . $',                                                             '
     .$Id: flwk51.f,v 1.1 1996/03/21 14:28:01 page Exp $
     . $' /
C    ===================================================================
C
C
C  IF ANY ERRORS OCCURRED, NO SENSE FILLING WORK ARRAY AS THE OPERATION
C  WILL NOT BE SATISFACTORILY DEFINED.
C
      IF (NUMERR.GT.0) GO TO 9999
C
      IF (LEFT.GT.1) GO TO 10
      CALL STER51(IERNUM,1)
      GO TO 9999
C
C  STORE VALUE, ENOUGH SPACE IS LEFT
C
   10 CONTINUE
      IUSED = IUSED+1
      LEFT = LEFT-1
      CALL UMEMOV(VALUE,STR1(IUSED),1)
C
 9999 CONTINUE
C     IF (NUMERR.GT.0 .AND. IBUG.GE.2) WRITE(IODBUG,1600) NUMERR
C1600 FORMAT(' ** ERRORS OCCURRED. NOTHING STORED. NUMERR = ',I3)
      RETURN
      END
