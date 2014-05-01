C MODULE FLWK26
C-----------------------------------------------------------------------
C
C  STORE A VALUE IN AN ARRAY IF AVAILABLE SPACE IS NOT EXCEEDED.
C
       SUBROUTINE FLWK26 (ARRAY,IUSED,LEFT,VALUE,IERNUM)
C
C ARGUMENT LIST:
C   ARRAY  - ARRAY TO PLACE VALUE
C   IUSED  - SPACE ALREADY USED IN ARRAY
C   LEFT   - SPACE LEFT IN ARRAY
C   VALUE  - VALUE TO PLACE IN ARRAY
C   IERNUM - ERROR NUMBER TO CALL STER26 WITH IF SPACE EXCEEDED
C
      DIMENSION ARRAY(*)
C
ccc      INCLUDE 'common/fdbug'
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      INCLUDE 'common/err26'
      INCLUDE 'common/comn26'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/flwk26.f,v $
     . $',                                                             '
     .$Id: flwk26.f,v 1.2 2001/06/13 10:09:48 mgm Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK IF ANY ERRORS HAVE BEEN ENCOUNTERED
      IF (NUMERR.GT.0) GO TO 10
C
C  CHECK IF ENOUGH SPACE LEFT
      IF (LEFT.LT.1) THEN
         CALL STER26 (IERNUM,1)
         GO TO 10
         ENDIF
C
C  STORE VALUE
      IUSED=IUSED+1
      LEFT=LEFT-1
      CALL UMEMOV (VALUE,ARRAY(IUSED),1)
C
10    RETURN
C
      END
