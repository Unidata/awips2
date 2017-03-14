C MODLUE SWMPFO
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE MAP COMPUTATIONAL ORDER FOR ONE FORECAST GROUP
C
      SUBROUTINE SWMPFO (IVMPFO,UNUSED,FGID,NRMPID,RMPID,LARRAY,ARRAY,
     *     LWORK,WORK,NUMERR,ISTAT)
C
      DIMENSION ARRAY(LARRAY),WORK(LWORK)
      DIMENSION FGID(2),RMPID(2,1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/swmpfo.f,v $
     . $',                                                             '
     .$Id: swmpfo.f,v 1.2 1999/07/07 11:19:24 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MPFO')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,110) IVMPFO,UNUSED,FGID,NRMPID,
     *      LARRAY,LWORK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=7+3*NRMPID-1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,120) MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,140) LARRAY,MINLEN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 90
         ENDIF
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      ARRAY(1)=IVMPFO+.01
C
C  STORE FORECAST GROUP IDENTIFIER
      ARRAY(2)=FGID(1)
      ARRAY(3)=FGID(2)
C
C  POSITIONS 4 AND 5 ARE UNUSED
      ARRAY(4)=UNUSED
      ARRAY(5)=UNUSED
C
C  STORE NUMBER OF MAP AREAS IN FORECAST GROUP
      ARRAY(6)=NRMPID+.01
C
      NPOS=6
C
C  CHECK NUMBER OF MAP AREA IDENTIFIERS SPECIFIED
      IF (NRMPID.EQ.0) THEN
         WRITE (LP,145)
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 90
         ENDIF
C
C  STORE MAP AREA IDENTIFIERS
      DO 20 I=1,NRMPID
         NPOS=NPOS+1
         ARRAY(NPOS)=RMPID(1,I)
         NPOS=NPOS+1
         ARRAY(NPOS)=RMPID(2,I)
20       CONTINUE
C
C  GET RECORD NUMBER OF MAP STATION (MAPS) PARAMETERS
      CALL SUDOPN (1,'PPP ',IERR)
      DO 60 I=1,NRMPID
         IPTR=0
         CALL RPPREC (RMPID(1,I),'MAPS',IPTR,LWORK,WORK,NFILL,IPTRNX,
     *        IERR)
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.2) THEN
               WRITE (LP,150) (RMPID(J,I),J=1,2)
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               GO TO 50
               ENDIF
            CALL SRPPST (RMPID(1,I),'MAPS',IPTR,LWORK,NFILL,IPTRNX,
     *         IERR)
            IF (IERR.GT.0) THEN
               ISTAT=1
               GO TO 60
               ENDIF
            ENDIF
50       NPOS=NPOS+1
         ARRAY(NPOS)=IPTR
60       CONTINUE
C
C  WRITE PARAMETER RECORD TO FILE
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,130) NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (FGID,'MPFO',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.EQ.0) GO TO 80
         CALL SWPPST (FGID,'MPFO',NPOS,IPTR,IERR)
         ISTAT=1
         GO TO 90
C
80    WRITE (LP,170) FGID
      CALL SULINE (LP,2)
      CALL SUDWRT (1,'PPP ',IERR)
C
      IF (LDEBUG.GT.0) CALL SUPDMP ('MPFO','BOTH',0,NPOS,ARRAY,ARRAY)
C
90    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,190)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT (' *** ENTER SWMPFO')
110   FORMAT (' IVMPFO=',I2,3X,'UNUSED=',F7.2,3X,'FGID=',2A4,3X,
     *   'NRMPID=',I3,3X,'LARRAY=',I5,3X,'LWORK=',I5)
120   FORMAT (' MINLEN=',I3)
130   FORMAT (' NPOS=',I3)
140   FORMAT ('0*** ERROR - IN SWMPFO - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I3,3X,
     *   'NUMBER OF WORDS NEEDED=',I3)
145   FORMAT ('0*** ERROR - IN SWMPFO - NO MAP AREA IDENTIFIERS ',
     *   'SPECIFIED.')
150   FORMAT ('0*** ERROR - MAP AREA ',2A4,' IS NOT DEFINED.')
170   FORMAT ('0*** NOTE - MPFO PARAMETERS SUCCESSFULLY ',
     *   'WRITTEN FOR FORECAST GROUP ',2A4,'.')
190   FORMAT (' *** EXIT SWMPFO')
C
      END
