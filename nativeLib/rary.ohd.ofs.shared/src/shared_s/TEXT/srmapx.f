C MEMBER SRMAPX
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/11/94.13:03:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  ROUTINE TO READ AREA MAPX PARAMETER RECORD.
C
      SUBROUTINE SRMAPX (IVMAPX,XMAPID,ITIME,DESCRP,BASNID00,FMAPID,   !cfan
     *   UNUSED,LARRAY,ARRAY,IPTR,IPRERR,IPTRNX,ISTAT,NUMB)            !cfan
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION XMAPID(2),DESCRP(*),BASNID00(*),FMAPID(*)              !cfan
      DIMENSION UNUSED(*)                                              !cfan
      INTEGER NUMB  !CKWZ
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/srmapx.f,v $
     . $',                                                             '
     .$Id: srmapx.f,v 1.3 2002/10/10 15:57:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('MAPX','        ','SRMAPX  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,180)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('MAPX','        ','SRMAPX  ',LDEBUG)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'LARRAY=',LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,'PPP ',IERR)
      CALL RPPREC (XMAPID,'MAPX',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.NE.0) THEN
         ISTAT=IERR
         IF (ISTAT.EQ.6) GO TO 170
         IF (IPRERR.EQ.1) THEN
            CALL SRPPST (XMAPID,'MAPX',IPTR,LARRAY,NFILL,IPTRNX,IERR)
            WRITE (LP,250)
            CALL SULINE (LP,2)
            ENDIF
         GO TO 170
         ENDIF
C
C  SET PARAMETER ARRAY VERSION NUMBER
      IVMAPX=ARRAY(1)
C
C  SET MAPX AREA IDENTIFIER
      XMAPID(1)=ARRAY(2)
      XMAPID(2)=ARRAY(3)
C
      NPOS=3
C
C  SET DESCRIPTIVE INFORMATION
      DO 20 I=1,5
         NPOS=NPOS+1
         DESCRP(I)=ARRAY(NPOS)
20       CONTINUE
C
C  SET TIME INTERVAL (HOURS)
      NPOS=NPOS+1
      ITIME=ARRAY(NPOS)
C
C  SET BASIN BOUNDARY IDENTIFIER
CC??kwz      IF (IVMAPX-1 .LT. 0.2 ) THEN      !cfan for version 1 
      IF (IVMAPX .EQ. 1 ) THEN
      DO 30 I=1,2
         NPOS=NPOS+1
         BASNID00(I)=ARRAY(NPOS)
         NUMB=1
30       CONTINUE
      ELSE                              !cfan for version 2
         NPOS=NPOS+1                    !cfan
         NUMB=ARRAY(NPOS)               !cfan
         NPOS=NPOS+1                    !cfan
         UNUSED(1)=ARRAY(NPOS)          !cfan
      ENDIF                             !cfan 
C
C  SET IDENTIFIER OF FUTURE MAPX AREA ASSIGNED TO AREA
      DO 40 I=1,2
         NPOS=NPOS+1
         FMAPID(I)=ARRAY(NPOS)
40       CONTINUE
C
C  THE NEXT TWO POSITIONS ARE UNUSED
cc??kwz      IF (IVMAPX-1 .LT. 0.2 ) THEN      !cfan for version 1
      IF (IVMAPX .EQ. 1 ) THEN
      DO 60 I=1,2
         NPOS=NPOS+1
         UNUSED(I)=ARRAY(NPOS)
60       CONTINUE
      ELSE                              !cfan for version 2
      DO 65 I=1,NUMB*2
         NPOS=NPOS+1                    !cfan
         BASNID00(I)=ARRAY(NPOS)        !cfan
65       CONTINUE                       !cfan 
      ENDIF                             !cfan
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' NPOS=',NPOS,
     *      ' NFILL=',NFILL,
     *      ' IPTRNX=',IPTRNX,
     *      ' IVMAPX=',IVMAPX,
     *      ' '
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('MAPX','BOTH',0,NPOS,ARRAY,ARRAY)
         IF (ISTAT.EQ.0) THEN
            WRITE (IOSDBG,240)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ISTAT.GT.0) THEN
            WRITE (IOSDBG,250)
            CALL SULINE (IOSDBG,1)
            ENDIF
         ENDIF
C
170   IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,260) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' *** ENTER SRMAPX')
240   FORMAT ('0*** NOTE - MAPX PARAMETERS SUCCESSFULLY ',
     *     'READ.')
250   FORMAT ('0*** NOTE - MAPX PARAMETERS NOT SUCCESSFULLY ',
     *     'READ.')
260   FORMAT (' *** EXIT SRMAPX : STATUS CODE=',I2)
C
      END
