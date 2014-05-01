C MODULE RFIL26
C-----------------------------------------------------------------------
C
C  STORE A VALUE IN AN ARRAY.
C
      SUBROUTINE RFIL26 (ARRAY,IPOS,VALUE)
C
      DIMENSION ARRAY(*)
C
      INCLUDE 'common/err26'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/rfil26.f,v $
     . $',                                                             '
     .$Id: rfil26.f,v 1.2 2001/06/13 10:11:38 mgm Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK IF ANY ERRORS HAVE BEEN ENCOUNTERED
      IF (NUMERR.GT.0) GO TO 10
C
C  STORE VALUE
      CALL UMEMOV (VALUE,ARRAY(IPOS),1)
C
10    RETURN
C
      END
