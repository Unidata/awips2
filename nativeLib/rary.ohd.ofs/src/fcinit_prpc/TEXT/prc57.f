C MEMBER PRC57
C-----------------------------------------------------------------------
C
C                             LAST UPDATE:
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRC57 (C)

C     THIS IS THE PRINT CARRYOVER ROUTINE FOR CONSUMPTIVE USE

C     THIS ROUTINE ORIGINALLY WRITTEN BY
C        JOSEPH PICA  - NWRFC   MAY 1997

C     POSITION     CONTENTS OF C ARRAY
C      1           RETURN FLOW STORAGE

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

      DIMENSION C(*)

C     COMMON BLOCKS

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FCONIT/IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc57.f,v $
     . $',                                                             '
     .$Id: prc57.f,v 1.1 1997/12/31 17:50:52 page Exp $
     . $' /
C    ===================================================================
C
C
C        RETURN FLOW STORAGE (MM)             XXXXXX
C

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

      CALL FPRBUG ('PRC57   ',1,57,IBUG)

      WRITE(IPR,500) C(1)
 500  FORMAT(10X,'RETURN FLOW STORAGE (MM)',12X,F6.0)

      IF (ITRACE.GE.1) WRITE(IODBUG,90)
 90   FORMAT('PRC57:  EXITED:')

      RETURN
      END
