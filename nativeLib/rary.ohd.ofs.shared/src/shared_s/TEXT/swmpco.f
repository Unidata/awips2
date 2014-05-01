C MODULE SWMPCO
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE MAP COMPUTATIONAL ORDER FOR ONE FORECAST GROUP.
C
      SUBROUTINE SWMPCO (IVMPCO,UNUSED,IFIRST,ILAST,
     *   ICG,NFG,FGID,NUMFG,NDUP,DUP,NRMPID,
     *   LARRAY,ARRAY,NPOS,NUMERR,ISTAT)
C
      DIMENSION ARRAY(LARRAY)
      CHARACTER*8 FGID,DUP(NDUP)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'common/fccgd'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/swmpco.f,v $
     . $',                                                             '
     .$Id: swmpco.f,v 1.2 2001/06/13 13:55:07 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SWMPCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MPCO')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,60) IVMPCO,UNUSED,ICG,NFG,NUMFG,NDUP,LARRAY
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*) 'IFIRST=',IFIRST,' ILAST=',ILAST
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK IF LAST FORECAST GROUP PROCESSED
      IF (ILAST.EQ.1) GO TO 15
C
      IF (IFIRST.EQ.0) GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=8+3*NFG+2*NDUP-1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,70) LARRAY,MINLEN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 40
         ENDIF
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      ARRAY(1)=IVMPCO+.01
C
C  STORE CARRYOVER GROUP IDENTIFIER
      ARRAY(2)=CGIDS(1,ICG)
      ARRAY(3)=CGIDS(2,ICG)
C
C  POSITIONS 4 AND 5 ARE UNUSED
      ARRAY(4)=UNUSED
      ARRAY(5)=UNUSED
C
C  POSITIONS 6 AND 7 FILLED LATER
      ARRAY(6)=0.0
      ARRAY(7)=0.0
C
      NPOS=7
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,80) NPOS,FGID,NRMPID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  STORE FORECAST GROUP IDENTIFIERS
      NPOS=NPOS+1
      CALL UMEMOV (FGID(1:4),ARRAY(NPOS),1)
      NPOS=NPOS+1
      CALL UMEMOV (FGID(5:8),ARRAY(NPOS),1)
C
C  STORE NUMBER OF MAP AREAS IN FORECAST GROUP
      NPOS=NPOS+1
      ARRAY(NPOS)=NRMPID+.01
C
C  CHECK IF LAST FORECAST GROUP PROCESSED
      IF (ILAST.EQ.0) GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STORE NUMBER OF FORECAST GROUPS IN CARRYOVER GROUP
15    ARRAY(6)=NUMFG+.01
C
C  STORE DUPLICATE MAP IDENTIFIERS
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NDUP=',NDUP
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=NPOS+NDUP*2
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,70) LARRAY,MINLEN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 40
         ENDIF
C
C  STORE NUMBER OF MAP AREAS IN MORE THAN ONE FORECAST GROUP
      ARRAY(7)=NDUP+.01
C
C  STORE IDENTIFIERS OF MAP AREAS IN MORE THAN ONE FORECAST GROUP
      IF (NDUP.GT.0) THEN
         DO 20 I=1,NDUP
            NPOS=NPOS+1
            CALL UMEMOV (DUP(I)(1:4),ARRAY(NPOS),1)
            NPOS=NPOS+1
            CALL UMEMOV (DUP(I)(5:8),ARRAY(NPOS),1)
20          CONTINUE
         ENDIF
C
C  WRITE PARAMETER RECORD TO FILE
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (CGIDS(1,ICG),'MPCO',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.EQ.0) GO TO 30
         CALL SWPPST (CGIDS(1,ICG),'MPCO',NPOS,IPTR,IERR)
         ISTAT=2
         GO TO 40
C
30    WRITE (LP,90) (CGIDS(I,ICG),I=1,2)
      CALL SULINE (LP,2)
      CALL SUDWRT (1,'PPP ',IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IPTR=',IPTR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (LDEBUG.GT.0) CALL SUPDMP ('MPCO','BOTH',0,NPOS,ARRAY,ARRAY)
C
40    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SWMPCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' IVMPCO=',I2,3X,'UNUSED=',F7.2,3X,'ICG=',I3,3X,
     *   'NFG=',I5,3X,'NUMFG=',I5,3X,'NDUP=',I5,3X,
     *   'LARRAY=',I5)
70    FORMAT ('0*** ERROR - IN SWMFPO - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *   'NUMBER OF WORDS NEEDED=',I5,'.')
80    FORMAT (' NPOS=',I5,3X,'FGID=',A,3X,'NRMPID=',I3)
90    FORMAT ('0*** NOTE - MPCO PARAMETERS SUCCESSFULLY ',
     *   'WRITTEN FOR CARRYOVER GROUP ',2A4,'.')
C
      END
