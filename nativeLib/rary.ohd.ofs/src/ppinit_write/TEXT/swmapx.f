C MEMBER SWMAPX
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/09/94.11:24:50 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC WRITE OR UPDATE AREA MAPX PARAMETER RECORD
C
      SUBROUTINE SWMAPX (IVMAPX,XMAPID,DESCRP,ITIME,BASNID00,FMAPID,  !cfan
     *   DISP,UNUSED,LARRAY,ARRAY,ISTAT,NUMB)                         !cfan
C
      REAL NEW/4HNEW /,OLD/4HOLD /
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION XMAPID(2),DESCRP(*),BASNID00(*),FMAPID(*)             !cfan
      DIMENSION UNUSED(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swmapx.f,v $
     . $',                                                             '
     .$Id: swmapx.f,v 1.2 2002/10/10 15:57:58 dws Exp $
     . $' /
C    ===================================================================
C
C  SET TRACE LEVEL
      CALL SBLTRC ('MAPX','        ','SWMAPX  ',LTRACE)
C
      IF (LTRACE.GT.0) WRITE (IOSDBG,180)
      IF (LTRACE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('MAPX','        ','SWMAPX  ',LDEBUG)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,190) IVMAPX,LARRAY
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
      NUMERR=0
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=16
      IF (LDEBUG.GT.0) WRITE (IOSDBG,210) MINLEN
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
      IF (MINLEN.LE.LARRAY) GO TO 10
         WRITE (LP,220) LARRAY,MINLEN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 170
C
C  STORE PARAMETER ARRAY VERSION NUMBER
10    ARRAY(1)=IVMAPX+.01

C
C  STORE MAPX AREA IDENTIFIER
      ARRAY(2)=XMAPID(1)
      ARRAY(3)=XMAPID(2)
C
      NPOS=3
C
C  STORE DESCRIPTIVE INFORMATION
      DO 20 I=1,5
         NPOS=NPOS+1
         ARRAY(NPOS)=DESCRP(I)
20       CONTINUE
C
C  STORE TIME INTERVAL (HOURS)
      NPOS=NPOS+1
      ARRAY(NPOS)=ITIME+.01
C
C  STORE NUMBER OF BASIN BOUNDARY IDENTIFIER     !cfan
         NPOS=NPOS+1                             !cfan
         ARRAY(NPOS)=NUMB                        !cfan
         NPOS=NPOS+1                             !cfan
         ARRAY(NPOS)=UNUSED(1)                   !cfan 
C  STORE IDENTIFIER OF FUTURE MAPX AREA ASSIGNED TO AREA
      DO 40 I=1,2
         NPOS=NPOS+1
         ARRAY(NPOS)=FMAPID(I)
40       CONTINUE
C
C  STORE BASIN BOUNDARY IDENTIFIER               !cfan
      DO 60 I=1,NUMB*2                           !cfan
         NPOS=NPOS+1                             !cfan
         ARRAY(NPOS)=BASNID00(I)                 !cfan
60       CONTINUE                                !cfan

C  WRITE PARAMTER RECORD TO FILE
      IF (LDEBUG.GT.0) WRITE (IOSDBG,200) NPOS
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (XMAPID,'MAPX',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.EQ.0) GO TO 150
         CALL SWPPST (XMAPID,'MAPX',NPOS,IPTR,IERR)
         WRITE (LP,240) IERR
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=2
C
150   IF (LDEBUG.GT.0) CALL SUPDMP ('MAPX','BOTH',0,NPOS,ARRAY,ARRAY)
C
      IF (DISP.EQ.NEW.AND.ISTAT.EQ.0) WRITE (LP,250) XMAPID
      IF (DISP.EQ.OLD.AND.ISTAT.EQ.0) WRITE (LP,260) XMAPID
      IF (ISTAT.EQ.0) CALL SULINE (LP,1)
      IF (ISTAT.EQ.0) CALL SUDWRT (1,'PPP ',IERR)
      IF (DISP.EQ.NEW.AND.ISTAT.GT.0) WRITE (LP,270) XMAPID
      IF (DISP.EQ.OLD.AND.ISTAT.GT.0) WRITE (LP,280) XMAPID
      IF (ISTAT.GT.0) CALL SULINE (LP,2)
C
170   IF (LTRACE.GT.0) WRITE (IOSDBG,290)
      IF (LTRACE.GT.0) CALL SULINE (IOSDBG,1)

C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' *** ENTER SWMAPX')
190   FORMAT (' IVMAPX=',I2,3X,'LARRAY=',I5)
200   FORMAT (' NPOS=',I2)
210   FORMAT (' MINLEN=',I3)
220   FORMAT ('0*** ERROR - IN SWMAPX - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *   'NUMBER OF WORDS NEEDED=',I5)
240   FORMAT ('0*** ERROR - IN SWMAPX - UNSUCCESSFUL CALL TO WPPREC : ',
     *   'STATUS CODE=',I3)
250   FORMAT ('0*** NOTE - MAPX PARAMETERS SUCCESSFULLY ',
     *   'WRITTEN FOR AREA ',2A4,'.')
260   FORMAT ('0*** NOTE - MAPX PARAMETERS SUCCESSFULLY ',
     *   'UPDATED FOR AREA ',2A4,'.')
270   FORMAT ('0*** NOTE - MAPX PARAMETERS NOT SUCCESSFULLY ',
     *   'WRITTEN FOR AREA ',2A4,'.')
280   FORMAT ('0*** NOTE - MAPX PARAMETERS NOT SUCCESSFULLY ',
     *   'UPDATED FOR AREA ',2A4,'.')
290   FORMAT (' *** EXIT SWMAPX')
C
      END
