C MODULE SPMPCO
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT CARRYOVER GROUP COMPUTATIONAL ORDER INFORMATION
C
      SUBROUTINE SPMPCO (IPRNT,NUMCG,IVMPCO,CGID,NFG,
     *   FGIDS,RMPDP,NRMPID,NRMPDP,ISTAT)
C
      DIMENSION CGID(2),FGIDS(2,1),RMPDP(2,1),NRMPID(1)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/spmpco.f,v $
     . $',                                                             '
     .$Id: spmpco.f,v 1.4 2001/06/13 13:54:24 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SPMPCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT DEBUG LEVEL

      LDEBUG=ISBUG('MPCO')
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.EQ.1) THEN
         WRITE (LP,70)
         CALL SULINE (LP,2)
         WRITE (LP,80)
         CALL SULINE (LP,2)
         WRITE (LP,*) ' '
         CALL SULINE (LP,1)
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IVMPCO=',IVMPCO,
     *      ' NFG=',NFG,
     *      ' NRMPDP=',NRMPDP,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT CARRYOVER GROUP IDENTIFIER
      IF (IPRNT.EQ.1) THEN
         WRITE (LP,160) CGID
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
      WRITE (LP,170) NUMCG,CGID
      CALL SULINE (LP,2)
C
C  PRINT FORECAST GROUP IDENTIFIERS
10    NPER=5
      NTIME=NFG/NPER
      IF (MOD(NFG,NPER).NE.0) NTIME=NTIME+1
      IF (NTIME.EQ.0) NTIME=1
      NUM1=1
      NUM2=NFG
      WRITE (LP,100)
      CALL SULINE (LP,2)
      DO 20 I=1,NTIME
         WRITE (LP,110) (J,FGIDS(1,J),FGIDS(2,J),J=NUM1,NUM2,NTIME)
         NUM1=NUM1+1
         CALL SULINE (LP,1)
20       CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         NUM1=1
         NUM2=NFG
         WRITE (LP,120)
         CALL SULINE (LP,2)
         DO 30 I=1,NTIME
            WRITE (LP,130) (J,NRMPID(J),J=NUM1,NUM2,NTIME)
            NUM1=NUM1+1
            CALL SULINE (LP,1)
30          CONTINUE
         ENDIF
C
C  PRINT IDENTIFIER OF MAP AREAS IN MORE THAN ONE FORECAST GROUP
      IF (NRMPDP.EQ.0) THEN
         WRITE (LP,140)
         CALL SULINE (LP,2)
         GO TO 50
         ENDIF
C
      NPER=5
      NTIME=NRMPDP/NPER
      IF (MOD(NRMPDP,NPER).NE.0) NTIME=NTIME+1
      IF (NTIME.EQ.0) NTIME=1
      NUM1=1
      NUM2=NRMPDP
      WRITE (LP,150)
      CALL SULINE (LP,2)
      DO 40 I=1,NTIME
         WRITE (LP,110) (J,RMPDP(1,J),RMPDP(2,J),J=NUM1,NUM2,NTIME)
         NUM1=NUM1+1
         CALL SULINE (LP,1)
40       CONTINUE
C
50    WRITE (LP,70)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SPMPCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('0',132('-'))
80    FORMAT ('0*--> MPCO PARAMETERS ',
     *   '(MAP CARRYOVER GROUP COMPUTATIONAL ORDER)')
100   FORMAT ('0FORECAST GROUP IDENTIFIERS:')
110   FORMAT (T5,5(:I4,': ',2A4,5X))
120   FORMAT ('0NUMBER OF MAP AREAS IN FORECAST GROUP:')
130   FORMAT (T5,5(:I4,': ',I8,5X))
140   FORMAT ('0*** NOTE - NO MAP AREA IDENTIFIERS ARE IN MORE THAN ',
     *   'ONE FORECAST GROUP.')
150   FORMAT ('0IDENTIFIERS FOR MAP AREAS IN MORE THAN ONE ',
     *  'FORECAST GROUP:')
160   FORMAT ('0CARRYOVER GROUP IDENTIFIER = ',2A4)
170   FORMAT ('0',I2,3X,'CARRYOVER GROUP IDENTIFIER = ',2A4)
C
      END
