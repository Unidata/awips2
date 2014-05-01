C MODULE SWXGRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE XGRD PARAMETERS.
C
      SUBROUTINE SWXGRD (IVXGRD,IFIRST,ILAST,N2POS,NXA,
     *   NSEGS,IY,IXB,IXE,
     *   UNUSED,LARRAY,R4ARAY,I2ARAY,NUMERR,ISTAT)
C
      CHARACTER*8 BLNK8/' '/
      INTEGER*2 I2ARAY(LARRAY*2)
C
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
C
      DIMENSION R4ARAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/swxgrd.f,v $
     . $',                                                             '
     .$Id: swxgrd.f,v 1.2 2002/02/11 21:08:54 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SWXGRD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('XGRD')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IVXGRD=',IVXGRD,
     *      ' UNUSED=',UNUSED,
     *      ' LARRAY=',LARRAY,
     *      ' IFIRST=',IFIRST,
     *      ' ILAST=',IFIRST
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  COMPUTE MINIMUM ARRAY LENGTH NEEDED
      NUMADD=1+NSEGS*3
      MINLEN=5+(N2POS+NUMADD)/2
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF LAST FORECAST GROUP PROCESSED
      IF (ILAST.EQ.1) GO TO 30
C
C  CHECK IF PROCESSING FIRST FORECAST GROUP
      IF (IFIRST.EQ.0) GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      R4ARAY(1)=IVXGRD+.01
C
C  STORE NUMBER OF MAPX AREAS USED IN CARRYOVER GROUPS
      R4ARAY(2)=0.
C
C  POSITIONS 3 AND 4 ARE UNUSED
      R4ARAY(3)=UNUSED
      R4ARAY(4)=UNUSED
C
      NPOS=4
      N2POS=NPOS*2
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
10    IF (MINLEN.GT.LARRAY) THEN
         N2POS=N2POS+NUMADD
         ISTAT=-1
         GO TO 50
         ENDIF
C
C  STORE NUMBER OF LINE SEGMENTS
      N2POS=N2POS+1
      I2ARAY(N2POS)=NSEGS
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NSEGS=',NSEGS,
     *      ' N2POS=',N2POS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  STORE BASIN LINE SEGMENT INFORMATRION
      DO 20 I=1,NSEGS
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'I=',I,
     *         ' N2POS=',N2POS,
     *         ' IY(I)=',IY(I),
     *         ' IXB(I)=',IXB(I),
     *         ' IXE(I)=',IXE(I)
            CALL SULINE (IOSDBG,1)
            ENDIF
         N2POS=N2POS+1
         I2ARAY(N2POS)=IY(I)
         N2POS=N2POS+1
         I2ARAY(N2POS)=IXB(I)
         N2POS=N2POS+1
         I2ARAY(N2POS)=IXE(I)
20       CONTINUE
C
      GO TO 50
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
30    IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,70) LARRAY,MINLEN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 40
         ENDIF
C
C  UPDATE NUMBER OF MAPX AREAS USED IN CARRYOVER GROUPS
      R4ARAY(2)=NXA+.01
C
      NPOS=(N2POS+1)/2
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  WRITE PARAMETER RECORD TO FILE
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (BLNK8,'XGRD',NPOS,R4ARAY,IPTR,IERR)
      IF (IERR.GT.0) THEN
         CALL SWPPST (BLNK8,'XGRD',NPOS,IPTR,IERR)
         WRITE (LP,80)
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=2
         GO TO 40
         ENDIF
C
C  PARAMETERS SUCCESSFULLY WRITTEN
      CALL SUDWRT (1,'PPP ',IERR)
      WRITE (LP,90)
      CALL SULINE (LP,2)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IPTR=',IPTR
         CALL SULINE (IOSDBG,1)
         NWORDS=0
         CALL SUPDMP ('XGRD','REAL',NWORDS,NPOS,R4ARAY,I2ARAY)
         CALL SUPDMP ('XGRD','INT2',NWORDS,NPOS,R4ARAY,I2ARAY)
         GO TO 50
         ENDIF
      GO TO 50
C
C  PARAMETERS NOT SUCCESSFULLY WRITTEN
40    WRITE (LP,100)
      CALL SULINE (LP,2)
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SWXGRD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('0*** ERROR - IN SWXGRD - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *   'NUMBER OF WORDS NEEDED=',I5,'.')
80    FORMAT ('0*** ERROR - IN SWXGRD - UNSUCCESSFUL CALL TO WPPREC.')
90    FORMAT ('0*** NOTE - XGRD PARAMETERS SUCCESSFULLY WRITTEN.')
100   FORMAT ('0*** NOTE - XGRD PARAMETERS NOT SUCCESSFULLY WRITTEN.')
C
      END
