C MODULE SWMXCO
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE MXCO PARAMETERS.
C
      SUBROUTINE SWMXCO (IVMXCO,NXA,MAPXID,IPXGRD,
     *   IFIRST,ILAST,NPOS,
     *   UNUSED,LARRAY,ARRAY,NUMERR,ISTAT)
C
      CHARACTER*8 MAPXID
      CHARACTER*8 BLNK8/' '/
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/swmxco.f,v $
     . $',                                                             '
     .$Id: swmxco.f,v 1.3 2005/03/18 21:03:00 leecr Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SWMXCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MXCO')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IVMXCO=',IVMXCO,' UNUSED=',UNUSED,
     *      ' LARRAY=',LARRAY,' IFIRST=',IFIRST,' ILAST=',ILAST
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK IF LAST FORECAST GROUP PROCESSED
      IF (ILAST.EQ.1) GO TO 20
C
C  CHECK IF PROCESSING FIRST FORECAST GROUP
      IF (IFIRST.EQ.0) GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      ARRAY(1)=IVMXCO+.01
C
C  STORE NUMBER OF MAPX AREAS USED IN CARRYOVER GROUPS
      ARRAY(2)=0.0
C
C  POSITIONS 3 AND 4 ARE UNUSED
      ARRAY(3)=UNUSED
      ARRAY(4)=UNUSED
C
      NPOS=4
C
      IPXGRD=8
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    NXA=NXA+1
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NXA=',NXA,' NPOS=',NPOS,' MAPXID=',MAPXID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=4+NXA*3
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,80) LARRAY,MINLEN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 30
         ENDIF
C
C  STORE MAPX IDENTIFIERS AND POINTER TO XGRD
      NPOS=NPOS+1
      CALL UMEMOV (MAPXID(1:4),ARRAY(NPOS),1)
      NPOS=NPOS+1
      CALL UMEMOV (MAPXID(5:8),ARRAY(NPOS),1)
      NPOS=NPOS+1
      ARRAY(NPOS)=IPXGRD+1+.01
C
      GO TO 30
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  UPDATE NUMBER OF MAPX AREAS USED IN CARRYOVER GROUPS
20    ARRAY(2)=NXA+.01
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  WRITE PARAMETERS TO FILE
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (BLNK8,'MXCO',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.GT.0) THEN
         CALL SWPPST (BLNK8,'MXCO',NPOS,IPTR,IERR)
         ISTAT=2
         GO TO 30
         ENDIF
C
      WRITE (LP,90)
      CALL SULINE (LP,2)
      CALL SUDWRT (1,'PPP ',IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IPTR=',IPTR
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('MXCO','BOTH',0,NPOS,ARRAY,ARRAY)
         ENDIF
C
30    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SWMXCO'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT ('0*** ERROR - IN SWMXCO - NOT ENOUGH SPACE IN PARAMETER ',
     *     'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *     'NUMBER OF WORDS NEEDED=',I5,'.')
90    FORMAT ('0*** NOTE - MXCO PARAMETERS SUCCESSFULLY WRITTEN.')
C
      END
