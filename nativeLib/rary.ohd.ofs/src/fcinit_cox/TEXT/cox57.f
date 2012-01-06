C MEMBER COX57
C-----------------------------------------------------------------------
C
C                             LAST UPDATE:
C
C @PROCESS LVL(77)
C
      SUBROUTINE COX57 (CO,CN)

C     THIS IS THE CARRYOVER TRANSFER ROUTINE FOR CONSUMPTIVE USE

C     THIS ROUTINE ORIGINALLY WRITTEN BY
C        JOSEPH PICA  --  NWRFC   JUNE 1997

C     POSITION     CONTENTS OF C ARRAY
C      1           RETURN FLOW STORAGE

C     THE NUMBER OF ELEMENTS REQUIRED IN THE C ARRAY IS   1

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     CONSUMPTIVE USE CARRYOVER TRANSFER RULE

C     *  THE VALUE FOR RETURN FLOW STORAGE ENTERED IN A SEGMENT
C        DEFINITION WILL OVERWRITE THE EXISTING RETURN FLOW STORAGE

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

      DIMENSION CO(*),CN(*)
      LOGICAL XWARN,XDIF

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FCONIT/IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox57.f,v $
     . $',                                                             '
     .$Id: cox57.f,v 1.1 1997/12/31 18:27:13 page Exp $
     . $' /
C    ===================================================================
C
      CALL FPRBUG ('COX57   ',1,57,IBUG)

      XWARN = .FALSE.
      XDIF = .FALSE.

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     OVERWRITE OLD WITH NEW CARRYOVER VALUE

      CO(1) = CN(1)

      IF (ITRACE.GE.1) WRITE(IODBUG,90)
 90   FORMAT('COX57:  EXITED:')

      RETURN
      END
