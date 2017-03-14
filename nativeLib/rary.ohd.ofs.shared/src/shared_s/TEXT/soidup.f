C MODULE SOIDUP
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK FOR DUPICATE IDENTIFIERS.
C
      SUBROUTINE SOIDUP (NIDENT,IDENT,NDUP,ISTAT)
C
      CHARACTER*8 IDENT(NIDENT)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/soidup.f,v $
     . $',                                                             '
     .$Id: soidup.f,v 1.2 2001/06/13 13:53:23 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('ORDR','ORDRIDUP','SOIDUP  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SOIDUP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('ORDR','ORDRIDUP','SOIDUP  ',LDEBUG)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,110) NIDENT
         CALL SULINE (IOSDBG,1)
         DO 10 I=1,NIDENT
            WRITE (IOSDBG,120) I,IDENT(I)
            CALL SULINE (IOSDBG,1)
10          CONTINUE
         ENDIF
C
      ISTAT=0
      NDUP=0
C
      NTIMES=NIDENT-1
      IF (NTIMES.LE.1) GO TO 90
C
C  CHECK FOR DUPLICATE IDENTIFIERS
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,130)
         CALL SULINE (IOSDBG,1)
         ENDIF
      DO 40 I=1,NTIMES
         IF (IDENT(I).EQ.' ') GO TO 40
         ISTRT=I+1
         DO 30 J=ISTRT,NIDENT
            IF (IDENT(J).EQ.' ') GO TO 30
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,140) I,J,IDENT(I),IDENT(J)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (IDENT(I).EQ.IDENT(J)) THEN
               IF (LDEBUG.GT.0) THEN
                  WRITE (IOSDBG,180) IDENT(I),I,J
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               NDUP=NDUP+1
               IDENT(J)=' '
               ENDIF
30          CONTINUE
40       CONTINUE
C
      IF (NDUP.EQ.0) GO TO 90
C
C  REMOVE DUPLICATE IDENTIFIES
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,160)
         CALL SULINE (IOSDBG,1)
         ENDIF
      DO 70 I=1,NIDENT
         IF (IDENT(I).EQ.' ') THEN
            IF (I.EQ.NIDENT) GO TO 70
            ISTRT=I+1
            DO 60 J=ISTRT,NIDENT
               IF (IDENT(J).NE.' ') THEN
                  IF (LDEBUG.GT.0) THEN
                     WRITE (IOSDBG,170) I,J,IDENT(I),IDENT(J)
                     CALL SULINE (IOSDBG,1)
                     ENDIF
                  IDENT(I)=IDENT(J)
                  IDENT(J)=' '
                  GO TO 70
                  ENDIF
60             CONTINUE
            ENDIF
70       CONTINUE
C
      NIDENT=NIDENT-NDUP
C
      IF (LDEBUG.GT.0) THEN
         IF (NIDENT.GT.0) THEN
            WRITE (IOSDBG,150)
            CALL SULINE (IOSDBG,1)
            DO 80 I=1,NIDENT
               WRITE (IOSDBG,120) I,IDENT(I)
               CALL SULINE (IOSDBG,1)
80             CONTINUE
            ENDIF
         ENDIF
C
90    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,200) NDUP
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,110) NIDENT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,* ) 'EXIT SOIDUP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT (' NIDENT=',I4)
120   FORMAT (' IDENT(',I4,')=',2A4)
130   FORMAT (' CHECK FOR AND REMOVE DUPLICATE IDENTIFIERS')
140   FORMAT (' I=',I4,3X,'J=',I4,3X,
     *   'IDENT(I)=',2A4,3X,'IDENT(J)=',2A4)
150   FORMAT (' DUPLICATE IDENTIFIERS':)
160   FORMAT (' COMPRESS IDENTIFIER ARRAY')
170   FORMAT (' I=',I4,3X,'J=',I4,3X,
     *   'IDENT(I)=',2A4,3X,'IDENT(J)=',2A4)
180   FORMAT (' ID ',2A4,' FOUND AT LOCATION ',I4,
     *     ' AND AT LOCATION ',I4)
200   FORMAT (' ',I5,' DUPLICATE MAP IDENTIFIES FOUND')

C
      END
