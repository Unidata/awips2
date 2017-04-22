C MEMBER SMDFLT
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/09/95.12:03:05 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO PRINT USER RUN DEFALUTS
C
      SUBROUTINE SMDFLT (LARRAY,ARRAY,ISTAT)
C
C
      DIMENSION ARRAY(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smdflt.f,v $
     . $',                                                             '
     .$Id: smdflt.f,v 1.1 1995/09/17 19:12:40 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,30)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HDUMP)
C
      ISTAT=0
C
C  READ USER RUN DEFAULTS
      IPRERR=1
      CALL SRDFLT (LARRAY,ARRAY,NDFLT,NPSMLN,IOPNWP,IOPOVP,IOPCLG,
     *   IPRERR,IERR)
      IF (IERR.GT.0) GO TO 20
      IF (NDFLT.GT.0) GO TO 10
         WRITE (LP,40)
         CALL SULINE (LP,2)
         GO TO 20
C
C  PRINT USER RUN DEFAULTS
10    CALL SPDFLT (NPSMLN,IOPNWP,IOPOVP,IOPCLG,IERR)
C
20    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,50)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SMDFLT')
40    FORMAT ('0*** NOTE - NO USER DEFAULTS DEFINED.')
50    FORMAT (' *** EXIT SMDFLT')
C
      END
