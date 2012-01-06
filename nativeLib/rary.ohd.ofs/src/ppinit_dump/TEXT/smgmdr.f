C MEMBER SMGMDR
C-----------------------------------------------------------------------
C
C DESC ROUTINE TO DUMP MDRGRID PARAMETERS
C DESC PPINIT COMMAND :  @DUMP MDRGRID
C
      SUBROUTINE SMGMDR (LARRAY,ARRAY,IERR)
C
C
      REAL XGMDR/4HGMDR/
      INTEGER*2 NMDRGP(42,89)
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smgmdr.f,v $
     . $',                                                             '
     .$Id: smgmdr.f,v 1.1 1995/09/17 19:12:48 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,40)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(XGMDR)
C
      ISTAT=0
C
      NUMERR=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
C  PRINT HEADER LINE
      WRITE (LP,50)
      CALL SULINE (LP,2)
      WRITE (LP,60)
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF PREPROCESSOR PARAMETRIC DATA BASE ALLOCATED
C
      IDPPP=1
      CALL SUDALC (0,0,0,0,IDPPP,0,0,0,0,0,NUMERR,IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (LP,70)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 30
C
C  READ GMDR PARAMETERS
10    IPTR=0
      IPRERR=1
      INCLUDE 'scommon/callsrgmdr'
      IF (IERR.GT.0) GO TO 30
C
C  PRINT GMDR PARAMETERS
20    CONTINUE
      INCLUDE 'scommon/callspgmdr'
C
30    IF (ISTRCE.GT.0) WRITE (IOSDBG,80)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SMGMDR')
50    FORMAT (1H )
60    FORMAT ('0*--> DUMP GMDR PARAMETERS')
70    FORMAT ('0*** ERROR - PREPROCESSOR PARAMETRIC DATA BASE FILES ',
     *   'ARE NOT ALLOCATED.')
80    FORMAT (' *** EXIT SMGMDR')
C
      END
