C MEMBER SPMPFO
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/10/95.14:45:29 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  ROUTINE TO PRINT FORECAST GROUP MAP COMPUTATIONAL ORDER INFORMATION.
C
      SUBROUTINE SPMPFO (IPRNT,NUMFG,IVMPFO,FGID,
     *   RMPID,NMAP,MAPSR,NMAPSR,ISTAT)
C
      DIMENSION FGID(2),RMPID(2,1),MAPSR(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/spmpfo.f,v $
     . $',                                                             '
     .$Id: spmpfo.f,v 1.2 1997/01/29 20:56:35 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MPFO')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.EQ.1) THEN
         WRITE (LP,50)
         CALL SULINE (LP,2)
         WRITE (LP,60)
         CALL SULINE (LP,2)
         WRITE (LP,*) ' '
         CALL SULINE (LP,1)
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IVMPFO=',IVMPFO,
     *      ' NMAP=',NMAP,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT FORECAST GROUP IDENTIFIER
      IF (IPRNT.EQ.1) THEN
         WRITE (LP,80) FGID
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
      WRITE (LP,90) NUMFG,FGID
      CALL SULINE (LP,2)
C
C  PRINT MAP AREA IDENTIFIERS
10    NPER=5
      NTIME=NMAP/NPER
      IF (MOD(NMAP,NPER).NE.0) NTIME=NTIME+1
      IF (NTIME.EQ.0) NTIME=1
      NUM1=1
      NUM2=NMAP
      WRITE (LP,100)
      CALL SULINE (LP,2)
      DO 20 I=1,NTIME
         WRITE (LP,110) (J,RMPID(1,J),RMPID(2,J),J=NUM1,NUM2,NTIME)
         NUM1=NUM1+1
         CALL SULINE (LP,1)
20       CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         NUM1=1
         NUM2=NMAP
         WRITE (IOSDBG,120)
         CALL SULINE (IOSDBG,2)
         DO 30 I=1,NTIME
            WRITE (IOSDBG,130) (J,MAPSR(J),J=NUM1,NUM2,NTIME)
            NUM1=NUM1+1
            CALL SULINE (IOSDBG,1)
30          CONTINUE
         ENDIF
C
      WRITE (LP,50)
      CALL SULINE (LP,2)
      CALL SULINE (LP,1)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,140)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SPMPFO')
50    FORMAT ('0',132('-'))
60    FORMAT ('0*--> MPFO PARAMETERS ',
     *   '(MAP FORECAST GROUP COMPUTATIONAL ORDER)')
80    FORMAT ('0FORECAST GROUP IDENTIFIER = ',2A4)
90    FORMAT ('0',I2,3X,'FORECAST GROUP IDENTIFIER = ',2A4)
100   FORMAT ('0MAP AREA IDENTIFIERS:')
110   FORMAT (T5,5(:I4,': ' ,2A4,5X))
120   FORMAT ('0RECORD NUMBERS FOR MAP STATION PARAMETERS:')
130   FORMAT (T5,5(:I4,': ' ,I8,5X))
140   FORMAT (' *** EXIT SPMPFO')
C
      END
