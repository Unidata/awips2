C MEMBER SRGBOX
C-----------------------------------------------------------------------
C
C DESC READ GRID BOX PARAMETERS
C
      SUBROUTINE SRGBOX (IVGBOX,UNUSED,
     *   GBOXID,NUGBOX,BOXLOC,IBXADJ,IBXMDR,
     *   LARRAY,ARRAY,IPRERR,IPTR,IPTRNX,
     *   ISTAT)
C
C
      REAL XGBOX/4HGBOX/
      DIMENSION ARRAY(1)
      DIMENSION UNUSED(1)
      INCLUDE 'scommon/dimgbox'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_read/RCS/srgbox.f,v $
     . $',                                                             '
     .$Id: srgbox.f,v 1.1 1995/09/17 19:14:48 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,80)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(XGBOX)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,90) LARRAY
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
      NUMERR=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,4HPPP ,IERR)
      IF (IERR.EQ.0) GO TO 10
         ISTAT=1
         GO TO 70
10    CALL RPPREC (GBOXID,XGBOX,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *     IERR)
      IF (IERR.EQ.0) GO TO 20
         ISTAT=IERR
         IF (ISTAT.EQ.6) GO TO 70
            IF (ISTAT.EQ.2.AND.IPRERR.EQ.0) GO TO 70
               CALL SRPPST (GBOXID,XGBOX,IPTR,LARRAY,NFILL,IPTRNX,IERR)
               WRITE (LP,100) IERR,IPTR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 70
C
C  SET PARAMETER ARRAY VERSION NUMBER
20    IVGBOX=ARRAY(1)
C
C  SET GRID BOX IDENTIFIER
      GBOXID(1)=ARRAY(2)
      GBOXID(2)=ARRAY(3)
C
C  SET GRID BOX NUMBER
      NUGBOX=ARRAY(4)
C
      NPOS=4
C
C  NEXT POSITIONS ARE UNUSED
      DO 30 I=1,2
         NPOS=NPOS+1
         UNUSED(I)=ARRAY(NPOS)
30       CONTINUE
C
C  SET GRID BOX BASE LATITUDE
      DO 40 I=1,2
         NPOS=NPOS+1
         BOXLOC(I)=ARRAY(NPOS)
40       CONTINUE
C
C  SET NUMBERS OF ADJACENT DEGREE BOXES
      DO 50 I=1,8
         NPOS=NPOS+1
         IBXADJ(I)=ARRAY(NPOS)
50       CONTINUE
C
C  SET MDR USAGE FLAG
      NPOS=NPOS+1
      IBXMDR=ARRAY(NPOS)
C
60    IF (LDEBUG.EQ.0) GO TO 70
         WRITE (IOSDBG,110) NPOS,NFILL,IPTRNX,IVGBOX
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP (XGBOX,4HBOTH,0,NFILL,ARRAY,ARRAY)
C
70    IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) WRITE (IOSDBG,120) GBOXID
      IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) CALL SULINE (IOSDBG,1)
      IF (LDEBUG.GT.0.AND.ISTAT.GT.0) WRITE (IOSDBG,130) GBOXID
      IF (LDEBUG.GT.0.AND.ISTAT.GT.0) CALL SULINE (IOSDBG,1)
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,140)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SRGBOX')
90    FORMAT (' LARRAY=',I5)
100   FORMAT ('0*** ERROR - IN SRGBOX - UNSUCCESSFUL CALL TO RPPREC : ',
     *   'STATUS CODE=',I2,3X,'IPTR=',I5)
110   FORMAT (' NPOS=',I3,3X,'NFILL=',I3,3X,'IPTRNX=',I3,3X,
     *   'IVGBOX=',I3)
120   FORMAT ('0*** NOTE - GBOX PARAMETERS FOR IDENTIFIER ',2A4,
     *   ' SUCCESSFULLY READ.')
130   FORMAT ('0*** NOTE - GBOX PARAMETERS FOR IDENTIFIER ',2A4,
     *   ' NOT SUCCESSFULLY READ.')
140   FORMAT (' *** EXIT SRGBOX')
C
      END
