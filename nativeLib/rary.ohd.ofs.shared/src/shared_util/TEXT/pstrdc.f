C MODULE PSTRDC
C-----------------------------------------------------------------------
C
      SUBROUTINE PSTRDC (ISTAT,TYPE,ID,IPTR,LARRAY,NFILL)
C
C  ROUTINE TO PRINT MESSAGES FOR STATUS CODES RETURNED FROM RPPREC AND
C  CALL ROUTINE ERROR.
C
      DIMENSION ID(2)
C      
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/pstrdc.f,v $
     . $',                                                             '
     .$Id: pstrdc.f,v 1.3 1998/07/02 19:33:34 page Exp $
     . $' /
C    ===================================================================
C
C.
C
      IF (ISTAT.EQ.0) GO TO 120
C
      IF (ISTAT.EQ.1) THEN
         WRITE (IPR,10) TYPE,ID,IPTR
10    FORMAT ('0**ERROR** SYSTEM ERROR IN RPPREC - ',
     *   'TYPE=',A4,3X,'ID=',2A4,3X,'IPTR=',I7)
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (IPR,20)TYPE,ID,IPTR
20    FORMAT ('0**ERROR** PARAMETER RECORD NOT FOUND BY RPPREC - ',
     *   'TYPE=',A4,3X,'ID=',2A4,3X,'IPTR=',I7)
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (IPR,30) TYPE,ID,IPTR,NFILL,LARRAY
30    FORMAT ('0**ERROR** PARAMETER ARRAY USED FOR RPPREC IS TOO ',
     *   'SMALL - ' /
     *   11X,'TYPE=',A4,3X,
     *   'ID=',2A4,3X,'IPTR=',I7,3X,
     *   'WORDS NEEDED=',I5,3X,'WORDS AVAILABLE=',I5)
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.4) THEN
         WRITE (IPR,40) TYPE,ID,IPTR
40    FORMAT ('0**ERROR** PARAMETER TYPE NOT FOUND BY RPPREC - ',
     *   'TYPE=',A4,3X,'ID=',2A4,3X,'IPTR=',I7)
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.5) THEN
         WRITE (IPR,50) TYPE,ID,IPTR
50    FORMAT ('0**ERROR** PARAMETER RECORD NUMBER (IPTR) ',
     *   'IS OUT OF ALLOWABLE RANGE - ',
     *   'TYPE=',A4,3X,'ID=',2A4,3X,'IPTR=',I7)
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.6) THEN
         WRITE (IPR,60) TYPE
60    FORMAT ('0**ERROR** LAST RECORD IN FILE FOR TYPE ',A4,
     *   ' IS DELETED.')
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.7) THEN
         WRITE (IPR,70) ID,TYPE
70    FORMAT ('0**ERROR** RECORD IN INDEX DOES NOT MATCH ',
     *   'RECORD ON FILE FOR ID ',2A4,' AND TYPE ',A4,'.')
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.8) THEN
         WRITE (IPR,80) ID,TYPE,IPTR
80    FORMAT ('0**ERROR** PARAMETER RECORD FOR ID ',2A4,
     *    ' AND TYPE ',A4,
     *   ' NOT FOUND AT SPECIFIED RECORD NUMBER (',I7,').')
         GO TO 110
         ENDIF
C
      IF (ISTAT.EQ.9) THEN
         WRITE (IPR,90) TYPE,IPTR
90    FORMAT ('0**ERROR** DELETED RECORD WITH TYPE ',A4,
     *   ' FOUND AT RECORD NUMBER ',I7,'.')
         GO TO 110
         ENDIF
C
      WRITE (IPR,100) ISTAT,TYPE,ID,IPTR
100   FORMAT ('0**ERROR** IN PSTRDC - STATUS CODE ',I2,
     *   ' RETURNED FROM RPPREC NOT RECOGNIZED - ',
     *   'TYPE=',A4,3X,'ID=',2A4,3X,'IPTR=',I7)
C
110   CALL ERROR
C
120   RETURN

      END
