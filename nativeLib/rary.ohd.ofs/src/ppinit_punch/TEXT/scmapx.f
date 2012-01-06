C MODULE SCMAPX
C-----------------------------------------------------------------------
C
C  ROUTINE TO PUNCH AREA MAPX PARAMETER RECORD.
C
       SUBROUTINE SCMAPX (IVMAPX,XMAPID,DESCRP,BASNID,FMAPID,ISTAT,NUMB)

C LC changed definition basnid for multiple basins
      CHARACTER *4 BASNID(*)
      CHARACTER *8 CHAR
      DIMENSION UNUSED(1)
C
C      CHARACTER*8 XMAPID,BASNID,FMAPID
      CHARACTER*8 XMAPID,FMAPID
      CHARACTER*20 DESCRP
      CHARACTER*80 CARD/' '/
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scmapx.f,v $
     . $',                                                             '
     .$Id: scmapx.f,v 1.3 2002/10/10 15:56:30 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,30)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MAPX')
C
      ISTAT=0
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) ' IVMAPX=',IVMAPX
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PUNCH 'MAPX' STARTING IN COLUMN 1
      NPOS=1
      CALL UTOCRD (ICDPUN,NPOS,'MAPX',4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 10
C
C  PUNCH IDENTIFIER
      CALL UTOCRD (ICDPUN,NPOS,XMAPID,LEN(XMAPID),1,CARD,3,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 10
C  PUNCH DESCRIPTION
      CALL UTOCRD (ICDPUN,NPOS,DESCRP,LEN(DESCRP),1,CARD,3,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 10
C
C  PUNCH BASIN BOUNDARY IDENTIFIER
C  BASIN BOUNDARY IDENTIFIER

C LC changed to handle multiple basins
C LC PUNCH NUMBER OF BASINS
C LC if mapx record not from cap number of basins=1
         MAXDEC=0
         IPRERR=1
         IF(IVMAPX.EQ.1)THEN
           VALUE=1
           NUMB=1
         ELSE
           VALUE=NUMB
         ENDIF
         CALL UFF2A (VALUE,CHAR,1,LEN(CHAR),MAXDEC,IPRERR,LP,IERR)
         IF (IERR.GT.0) GO TO 10
         CALL UTOCRD (ICDPUN,NPOS,CHAR,LEN(CHAR),0,CARD,0,0,LNUM,IERR)
	 CALL UTOCRD (ICDPUN,NPOS,' ',1,0,CARD,0,0,LNUM,IERR) !c add a space, kwz
      do 50 I=1,NUMB
      CALL UTOCRD (ICDPUN,NPOS,BASNID(2*I-1),8,1,CARD,3,0,
     *   LNUM,IERR)
50    continue
      IF (IERR.GT.0) GO TO 10
C
C  PUNCH IDENTIFIER OF FUTURE MAPX AREA ASSIGNED TO THIS AREA
      CALL UTOCRD (ICDPUN,NPOS,FMAPID,LEN(FMAPID),1,CARD,3,0,
     *   LNUM,IERR)
      IF (IERR.GT.0) GO TO 10
      CALL UPNCRD (ICDPUN,CARD)
C
      GO TO 20
C
10    ISTAT=1
C
20    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SCMAPX')
40    FORMAT (' *** EXIT SCMAPX')
C
      END
