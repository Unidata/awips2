C MODULE SORDUP
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK FOR MAP IDENTIFIERS IN MORE THAN ONE FORECAST GROUP.
C
      SUBROUTINE SORDUP (NRIDCK,RIDCK,NFGCK,MRMPDP,NRMPDP,RMPDP,ISTAT)
C
      CHARACTER*8 RIDCK(NRIDCK),RMPDP(MRMPDP)
      DIMENSION NFGCK(NRIDCK)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sordup.f,v $
     . $',                                                             '
     .$Id: sordup.f,v 1.2 2001/06/13 13:54:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('ORDR','ORDRRDUP','SORDUP  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SORDUP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('ORDR','ORDRRDUP','SORDUP  ',LDEBUG)
C
      ISTAT=0
C
      NUMERR=0
      NRMPDP=0
C
      IF (LDEBUG.GT.1) THEN
         WRITE (IOSDBG,*) 'NRIDCK=',NRIDCK,' MRMPDP=',MRMPDP,
     *      ' NRMPDP=',NRMPDP
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,70)
         CALL SULINE (IOSDBG,1)
         DO 10 I=1,NRIDCK
            WRITE (IOSDBG,80) I,RIDCK(I)
            CALL SULINE (IOSDBG,1)
10          CONTINUE
         ENDIF
C
      NTIMES=NRIDCK-1
      IF (NTIMES.LE.1) GO TO 60
C
C  CHECK FOR DUPLICATE IDENTIFIERS
      IF (LDEBUG.GT.1) THEN
         WRITE (IOSDBG,90)
         CALL SULINE (IOSDBG,1)
         ENDIF
      DO 40 I=1,NTIMES
         ISTRT=I+1
         DO 30 J=ISTRT,NRIDCK
            IF (LDEBUG.GT.2) THEN
               WRITE (IOSDBG,130) I,J,
     *            RIDCK(I),RIDCK(J),NFGCK(I),NFGCK(J)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (RIDCK(I).EQ.RIDCK(J).AND.NFGCK(I).NE.NFGCK(J)) THEN
               IF (LDEBUG.GT.1) THEN
                  WRITE (IOSDBG,140) RIDCK(I),I,J
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (NRMPDP.GT.0) THEN
                  DO 20 N=1,NRMPDP
                     IF (RIDCK(I).EQ.RMPDP(N)) GO TO 30
20                CONTINUE
                  ENDIF
               IF (NRMPDP+1.GT.MRMPDP) THEN
                  WRITE (LP,150) MRMPDP
                  CALL SUERRS (LP,2,NUMERR)
                  ISTAT=1
                  GO TO 60
                  ENDIF
               NRMPDP=NRMPDP+1
               RMPDP(NRMPDP)=RIDCK(I)
               ENDIF
30          CONTINUE
40       CONTINUE
C
      IF (LDEBUG.GT.1) THEN
         WRITE (IOSDBG,100) NRMPDP
         CALL SULINE (IOSDBG,1)
         IF (NRMPDP.GT.0) THEN
            WRITE (IOSDBG,110)
            CALL SULINE (IOSDBG,1)
            DO 50 I=1,NRMPDP
               WRITE (IOSDBG,120) I,RMPDP(I)
               CALL SULINE (IOSDBG,1)
50             CONTINUE
            ENDIF
         ENDIF
C
60    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SORDUP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' MAP AREA IDENTIFIERS:')
80    FORMAT (' RIDCK(',I3,')=',A)
90    FORMAT (' CHECKING FOR DUPLICATE IDENTIFIERS')
100   FORMAT (' ',I5,' DUPLICATE MAP IDS FOUND')
110   FORMAT (' DUPLICATE MAP AREA IDENTIFIERS:')
120   FORMAT (' RMPDP(',I3,')=',A)
130   FORMAT (' I=',I3,3X,'J=',I3,3X,
     *   'RIDCK(I)=',A,3X,'RIDCK(J)=',A,3X,
     *   'NFGCK(I)=',I3,3X,'NFGCK(J)=',I3)
140   FORMAT (' MAP ID ',A,' FOUND AT LOCATION ',I3,
     *   ' AND AT LOCATION ',I3)
150   FORMAT ('0*** ERROR - IN SORDUP - MAXIMUM NUMBER OF DUPICATE ',
     *   'MAP IDENTIFIERS (',I5,') EXCEEDED.')
C
      END
