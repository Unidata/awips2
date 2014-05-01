C MODULE SRXGRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ READ XGRD PARAMETERS.
C
      SUBROUTINE SRXGRD (IVXGRD,MXA,NXA,NSEGS,MSEGS,IY,IXB,IXE,
     *   UNUSED,LARRAY,R4ARAY,I2ARAY,IPRERR,ISTAT)
C
      CHARACTER*8 BLNK8/' '/
C
      INTEGER*2 I2ARAY(LARRAY/2)
C
      DIMENSION R4ARAY(LARRAY)
      DIMENSION NSEGS(*),IY(*),IXB(*),IXE(*)
      DIMENSION UNUSED(2)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/srxgrd.f,v $
     . $',                                                             '
     .$Id: srxgrd.f,v 1.3 2002/02/11 21:04:18 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SRXGRD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('XGRD')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'LARRAY=',LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  READ PARAMETER ARRAY
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL RPPREC (BLNK8,'XGRD',IPTR,LARRAY,R4ARAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.GT.0) THEN
         ISTAT=IERR
         IF (IPRERR.GT.0) THEN
            CALL SRPPST (BLNK8,'XGRD',IPTR,LARRAY,NFILL,IPTRNX,IERR)
            ISTAT=1
            ENDIF
         GO TO 30
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NFILL=',NFILL
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('XGRD','REAL',0,NFILL,R4ARAY,I2ARAY)
         CALL SUPDMP ('XGRD','INT2',0,NFILL,R4ARAY,I2ARAY)
         ENDIF
C
C  SET PARAMETER ARRAY VERSION NUMBER
      IVXGRD=R4ARAY(1)
C
C  SET NUMBER OF MAPX AREAS
      NXA=R4ARAY(2)
C
C  POSITIONS 3 AND 4 ARE UNUSED
      UNUSED(1)=R4ARAY(3)
      UNUSED(2)=R4ARAY(4)
C
      NPOS=4
C
C  CHECK FOR SUFFICIENT SPACE FOR MAPX IDENTIFIERS
      IF (NXA.GT.MXA) THEN
         WRITE (LP,100) NXA,MXA
         CALL SUERRS (LP,2,-1)
         ISTAT=2
         GO TO 30
         ENDIF
C
      IPOS=0
      N2POS=NPOS*2
      NTSEGS=0
C
C  PROCESS LINE SEGMENT INFORMATION FOR EACH MAPX AREA
      DO 20 I=1,NXA
         N2POS=N2POS+1
         NSEGS(I)=I2ARAY(N2POS)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,80) NXA,I,NSEGS(I)
            CALL SULINE (IOSDBG,1)
            ENDIF
         NTSEGS=NTSEGS+NSEGS(I)
C     SET BASIN LINE SEGMENT INFORMATRION
         DO 10 N=1,NSEGS(I)
            IPOS=IPOS+1
            N2POS=N2POS+1
            IF (NTSEGS.LE.MSEGS) IY(IPOS)=I2ARAY(N2POS)
            N2POS=N2POS+1
            IF (NTSEGS.LE.MSEGS) IXB(IPOS)=I2ARAY(N2POS)
            N2POS=N2POS+1
            IF (NTSEGS.LE.MSEGS) IXE(IPOS)=I2ARAY(N2POS)
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,60) IPOS,N2POS,
     *            IY(IPOS),IXB(IPOS),IXE(IPOS)
               CALL SULINE (IOSDBG,1)
               ENDIF
10          CONTINUE
20       CONTINUE
C
C  CHECK IF SUFFICIENT SPACE FOR BASIN LINE SEGMENT INFORMATION
      IF (NTSEGS.GT.MSEGS) THEN
         WRITE (LP,120) NTSEGS,MSEGS
         CALL SUERRS (LP,2,-1)
         ISTAT=2
         GO TO 30
         ENDIF
C
30    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SRXGRD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' IPOS=',I3,3X,'N2POS=',I4,3X,
     *   'IY(IPOS)=',I4,3X,'IXB(IPOS)=',I4,3X,'IXE(IPOS)=',I4)
80    FORMAT (' NXA=',I4,3X,'I=',I4,3X,'NSEGS(I)=',I4)
90    FORMAT (15X,'XGRD PARAMETER RECORD POINTER NUMBER=',I3)
100   FORMAT ('0*** ERROR - IN SRXGRD - NUMBER OF AREAS WITH MAPX ',
     *   'PARAMETERS (',I5,
     *   ') EXCEEDS MAXIMUM THAT CAN BE PROCESSED (',I5,').')
120   FORMAT ('0*** ERROR - IN SRXGRD - NUMBER OF LINE SEGMENTS ',
     *   'IN PARAMETER ARRAY (',I5,
     *   ') EXCEEDS MAXIMUM THAT CAN BE PROCESSED (',I5,').')
C
      END
