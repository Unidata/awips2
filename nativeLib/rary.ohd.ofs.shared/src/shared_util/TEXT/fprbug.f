C MODULE FPRBUG
C-----------------------------------------------------------------------
C
      SUBROUTINE FPRBUG (RTNNAM,LTRACE,NOP,IBUG)
C.......................................................................
C
C     THIS SUBROUTINE CHECKS THE TRACE LEVEL AND SETS THE DEBUG
C     OUTPUT SWITCH FOR SUBROUTINES ASSOCIATED WITH AN OPERATION.
C     IF THE DEBUG TRACE LEVEL IS GREATER THAN OR EQUAL TO THE
C     TRACE LEVEL OF THE CALLING SUBROUTINE, A MESSAGE IS PRINTED
C     STATING THAT THE CALLING SUBROUTINE HAS BEEN ENTERED.
C     IF DEBUG OUTPUT IS REQUESTED FOR THE OPERATION, THE DEBUG
C     OUTPUT SWITCH IS TURNED ON.
C.......................................................................
C
C  ORIGINALLY WRITTEN BY - GEORGE F. SMITH - HRL 10/1979
C.......................................................................
C
C  VARIABLES IN ARGUMENT LIST:
C
C     1. RTNNAM  - NAME OF CALLING ROUTINE
C     2. LTRACE - TRACE LEVEL OF THE CALLING ROUTINE
C     3. NOP    - OPERATION NUMBER ASSOCIATED WITH THE CALLING ROUTINE
C     4. IBUG   - DEBUG OUTPUT INDICATOR:
C                  0 = DO NOT PRINT DEBUG OUTPUT
C                  1 = PRINT DEBUG OUTPUT IN CALLING ROUTINE
C.......................................................................
C
      CHARACTER*8 RTNNAM
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fprbug.f,v $
     . $',                                                             '
     .$Id: fprbug.f,v 1.3 2001/06/13 09:48:56 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.LTRACE) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C  
      IBUG=0
C
C  CHECK IF ALL DEBUG CODES TO BE 
      IF (IDBALL.GT.0) THEN
         IBUG=1
         GO TO 20
         ENDIF
C
C  CHECK IF ANY DEBUG CODES SPECIFIED
      IF (NDEBUG.GT.0) THEN
         DO 10 I=1,NDEBUG
            IF (IDEBUG(I).EQ.NOP) THEN
               IBUG=1
               GO TO 20
               ENDIF
   10       CONTINUE
         ENDIF
C
20    RETURN
C
      END
