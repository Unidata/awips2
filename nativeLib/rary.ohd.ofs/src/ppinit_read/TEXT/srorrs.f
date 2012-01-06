C MEMBER SRORRS
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/18/94.10:33:12 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC WRITE RRS STATION ALPHABETICAL ORDER
C
      SUBROUTINE SRORRS (IPRERR,IVORRS,UNUSED,IORDER,MAXSTA,IPNTRS,
     *     NUMSTA,LARRAY,ARRAY,ISTAT)
C
C
      CHARACTER*8 BLNK8/' '/
C
      INTEGER*2 IPNTRS(*)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_read/RCS/srorrs.f,v $
     . $',                                                             '
     .$Id: srorrs.f,v 1.1 1995/09/17 19:14:58 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('ORRS')
C
      ISTAT=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL RPPREC (BLNK8,'ORRS',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *     IERR)
      IF (IERR.GT.0) THEN
         ISTAT=IERR
         IF (IPRERR.GT.0) THEN
            CALL SRPPST (BLNK8,'ORRS',IPTR,LARRAY,NFILL,IPTRNX,ISTAT)
            WRITE (LP,30)
            CALL SUERRS (LP,2,-1)
            ENDIF
         GO TO 10
         ENDIF
C
C  SET PARAMETER ARRAY VERSION NUMBER
      IVORRS=ARRAY(1)
C
C  SET INDICATOR HOW LIST WAS ORDERED
      IORDER=ARRAY(2)
C
C  POSITIONS 3 AND 4 ARE UNUSED
      UNUSED(1)=ARRAY(3)
      UNUSED(2)=ARRAY(4)
C
C  SET NUMBER STATIONS IN LIST
      NUMSTA=ARRAY(5)
C
      NPOS=5
C
      IF (NUMSTA.EQ.0) GO TO 10
C
C  CHECK FOR SUFFICIENT SPACE TO STORE POINTERS
      IF (NUMSTA.GT.MAXSTA) THEN
         WRITE (LP,40) MAXSTA,NUMSTA
         CALL SUERRS (LP,2,-1)
         CALL SULINE (LP,2)
         ISTAT=1
         GO TO 10
         ENDIF
C
C  SET RECORD LOCATION OF PARAMETERS IN PARAMETRIC DATA BASE
      IPOS=NPOS*4+1
      CALL SUBSTR (ARRAY,IPOS,NUMSTA*2,IPNTRS,1)
      NPOS=NPOS+(NUMSTA+1)/2
C
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,50) NPOS,NFILL,IPTRNX,IVORRS
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('ORRS','BOTH',0,NPOS,ARRAY,ARRAY)
         IF (ISTAT.EQ.0) THEN
            WRITE (LP,60)
            CALL SULINE (LP,2)
            ELSE
                WRITE (LP,70)
                CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
10    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER SRORRS')
30    FORMAT ('0*** ERROR - IN SRORRS - UNSUCCESSFUL CALL TO RPPREC.')
40    FORMAT ('0*** ERROR - IN SRORRS - POINTER ARRAY CAN HOLD ',I4,
     *   'VALUES HOWEVER ',I4,' POINTERS ARE STORED IN PARAMETER ',
     *   'ARRAY.')
50    FORMAT (' NPOS=',I3,3X,'NFILL=',I3,3X,'IPTRNX=',I3,3X,
     *   'IVORRS=',I3)
60    FORMAT ('0*** NOTE - ORRS PARAMETERS SUCCESSFULLY READ.')
70    FORMAT ('0*** NOTE - ORRS PARAMETERS NOT SUCCESSFULLY READ.')
80    FORMAT (' *** EXIT SRORRS')
C
      END
