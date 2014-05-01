C MODULE SPURRS
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT GENERAL RRS PARAMETERS.
C
      SUBROUTINE SPURRS (IVURRS,NTYPE,TYPES,MNDAY,NMOBS,
     *   UNUSED,ISTAT)
C
C
      DIMENSION TYPES(NTYPE),MNDAY(NTYPE),NMOBS(NTYPE)
      DIMENSION UNUSED(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spurrs.f,v $
     . $',                                                             '
     .$Id: spurrs.f,v 1.2 1998/04/07 18:02:18 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1)THEN
          WRITE (IOSDBG,40)
          CALL SULINE (IOSDBG,1)
          ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('URRS')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,50)
      CALL SULINE (LP,2)
      WRITE (LP,60)
      CALL SULINE (LP,2)
      WRITE (LP,70)
      CALL SULINE (LP,2)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,80) IVURRS
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT NUMBER OF DATA TYPES
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,90) NTYPE
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT RRS DATA TYPES CODES
      WRITE (LP,100)
      CALL SULINE (LP,2)
      WRITE (LP,110)
      CALL SULINE (LP,2)
      DO 20 I=1,NTYPE
         WRITE (LP,120) I,TYPES(I),MNDAY(I),NMOBS(I)
         CALL SULINE (LP,1)
20       CONTINUE
C
C  PRINT UNUSED POSITIONS
      IF (LDEBUG.GT.0) THEN
         NUNSED=2
         WRITE (LP,130) NUNSED
         CALL SULINE (LP,1)
         ENDIF
C
      WRITE (LP,50)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,140)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SPURRS')
50    FORMAT ('0',132('-'))
60    FORMAT ('0*--> URRS PARAMETERS ',
     *   '(RRS GENERAL)')
70    FORMAT (' ')
80    FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
90    FORMAT ('0NUMBER OF RRS DATA TYPES = ',I2)
100   FORMAT ('0DATA TYPE CODES AND ATTRIBUTES')
110   FORMAT ('0',5X,'CODE',5X,'MIN DAYS',5X,'MAX OBS' /
     *   ' ',5X,4('-'),5X,8('-'),5X,7('-'))
120   FORMAT (3X,I2,1X,A4,8X,I3,10X,I3)
130   FORMAT ('0NUMBER OF UNUSED POSITIONS = ',I2)
140   FORMAT (' *** EXIT SPURRS')
C
      END
