C MEMBER SRMXCO
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/17/94.13:53:51 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC READ MXCO PARAMETERS
C
      SUBROUTINE SRMXCO (IVMXCO,UNUSED,MXA,NXA,XMAPID,IPXGRD,
     *   LARRAY,ARRAY,IPRERR,ISTAT)
C
C
      CHARACTER*8 BLNK8/' '/
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(2)
      DIMENSION XMAPID(2,1),IPXGRD(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/srmxco.f,v $
     . $',                                                             '
     .$Id: srmxco.f,v 1.1 1995/09/17 19:21:06 dws Exp $
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
      LDEBUG=ISBUG('MXCO')
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
      CALL RPPREC (BLNK8,'MXCO',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.GT.0) THEN
         ISTAT=IERR
         IF (IPRERR.GT.0) THEN
            CALL SRPPST (BLNK8,'MXCO',IPTR,LARRAY,NFILL,IPTRNX,
     *         IERR)
            ISTAT=1
            ENDIF
         GO TO 20
         ENDIF
C
C  SET PARAMETER ARRAY VERSION NUMBER
      IVMXCO=ARRAY(1)
C
C  SET NUMBER OF MAPX AREAS USED IN CARRYOVER GROUPS
      NXA=ARRAY(2)
C
C  CHECK IF MAXIMUM AREAS EXCEEDED
      IF (NXA.GT.MXA) THEN
         WRITE (LP,40) NXA,MXA
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 20
         ENDIF
C
C  POSITIONS 3 AND 4 ARE UNUSED
      UNUSED(1)=ARRAY(3)
      UNUSED(2)=ARRAY(4)
C
C   SET MAPX IDENTIFIERS AND POINTER TO BASIN INFORMATION IN XGRD
      NPOS=4
      DO 10 I=1,NXA
         NPOS=NPOS+1
         XMAPID(1,I)=ARRAY(NPOS)
         NPOS=NPOS+1
         XMAPID(2,I)=ARRAY(NPOS)
         NPOS=NPOS+1
         IPXGRD(I)=ARRAY(NPOS)
10       CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS,
     *      ' NFILL=',NFILL,
     *      ' IPTRNX=',IPTRNX
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('MXCO','BOTH',0,NPOS,ARRAY,ARRAY)
         ENDIF
C
20    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,50)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SRMXCO')
40    FORMAT ('0*** ERROR - IN SRMXCO - NUMBER OF MAPX AREAS (',I5,
     *   ') EXCEEDS MAXIMUM THAT CAN BE PROCESSED (',I5,').')
50    FORMAT (' *** EXIT SRMXCO')
C
      END
