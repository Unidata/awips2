C MEMBER SPDFLT
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/10/95.14:48:15 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO PRINT USER DEFAULTS
C
      SUBROUTINE SPDFLT (NPSMLN,IOPNWP,IOPOVP,IOPCLG,ISTAT)
C
C
      CHARACTER*4 CMDLOG,NEWPAG,OVPRNT
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spdflt.f,v $
     . $',                                                             '
     .$Id: spdflt.f,v 1.1 1995/09/17 19:14:05 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,10)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HUGNL)
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,20)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
      CMDLOG='NO'
      NEWPAG='NO'
      OVPRNT='NO'
      IF (IOPCLG.EQ.1) CMDLOG='YES'
      IF (IOPNWP.EQ.1) NEWPAG='YES'
      IF (IOPOVP.EQ.1) OVPRNT='YES'
      WRITE (LP,40) CMDLOG,NEWPAG,OVPRNT,NPSMLN
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,50)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SPDFLT')
20    FORMAT ('0- USER SPECIFIED DEFAULTS IN EFFECT -')
40    FORMAT (T5,'CMDLOG=',A4,3X,
     *   'NEWPAGE=',A4,3X,
     *   'OVERPRNT=',A4,3X,
     *   'PAGESIZE=',I2,3X)
50    FORMAT (' *** EXIT SPDFLT')
C
      END
