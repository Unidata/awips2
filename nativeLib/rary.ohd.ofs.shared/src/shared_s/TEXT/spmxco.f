C MODULE SPMXCO
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT XGRD PARAMETERS.
C
      SUBROUTINE SPMXCO (IPRNT,NXA,IVMXCO,XMAPID,IPXGRD,ISTAT)
C
      DIMENSION XMAPID(2,1),IPXGRD(1)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/spmxco.f,v $
     . $',                                                             '
     .$Id: spmxco.f,v 1.3 2001/06/13 14:04:16 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SPMXCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MXCO')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.GT.0) THEN
         WRITE (LP,40)
         CALL SULINE (LP,2)
         WRITE (LP,50)
         CALL SULINE (LP,1)
         WRITE (LP,*) ' '
         CALL SULINE (LP,1)
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IVMXCO=',IVMXCO,
     *      ' NXA=',NXA,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT MAPX AREA IDENTIFIERS
      NPER=5
      NTIME=NXA/NPER
      IF (MOD(NXA,NPER).NE.0) NTIME=NTIME+1
      IF (NTIME.EQ.0) NTIME=1
      NUM1=1
      NUM2=NXA
      NCHK=NTIME
      IF (NCHK.GT.NPSMLN) NCHK=10
      IF (ISLEFT(2+NCHK).GT.0) CALL SUPAGE
      WRITE (LP,70)
      CALL SULINE (LP,2)
      DO 10 I=1,NTIME
         WRITE (LP,80) (J,XMAPID(1,J),XMAPID(2,J),J=NUM1,NUM2,NTIME)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,70)
            CALL SULINE (LP,2)
            ENDIF
         NUM1=NUM1+1
10       CONTINUE
C
      IF (LDEBUG.GT.0) THEN
C     PRINT POINTER TO BASIN GRID POINT INFORMATION IN XGRD PARAMETERS
         NUM1=1
         NUM2=NXA
         NCHK=NTIME
         IF (NCHK.GT.NPSMLN) NCHK=10
         IF (ISLEFT(2+NCHK).GT.0) CALL SUPAGE
         WRITE (LP,90)
         CALL SULINE (LP,2)
         DO 20 I=1,NTIME
            WRITE (LP,100) (J,IPXGRD(J),J=NUM1,NUM2,NTIME)
            CALL SULINE (LP,1)
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,90)
               CALL SULINE (LP,2)
               ENDIF
            NUM1=NUM1+1
20          CONTINUE
         ENDIF
C
      WRITE (LP,40)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SPMXCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('0',132('-'))
50    FORMAT ('0*--> MXCO PARAMETERS ',
     *   '(MAPX CARRYOVER GROUP COMPUTATIONAL ORDER)')
70    FORMAT ('0MAPX AREA IDENTIFIERS:')
80    FORMAT (T5,5(:I4,': ',2A4,5X))
90    FORMAT ('0POINTER TO BASIN GRID POINT INFORMATION IN ',
     *   'XGRD PARAMETERS:')
100   FORMAT (T5,5(:I4,': ',I5,8X))
C
      END
