C MODULE SWORDR
C----------------------------------------------------------------------
C
C  ROUTINE TO WRITE GENERAL ORDER PARAMETERS.
C
      SUBROUTINE SWORDR (IVORDR,UNSD,ICOMPL,
     *   NAMAP,NAFMAP,NAMAPX,ICBASN,ICFMAP,
     *   LARRAY,ARRAY,DISP,ISTAT)
C
      CHARACTER*4 DISP
      CHARACTER*8 PARMID
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/swordr.f,v $
     . $',                                                             '
     .$Id: swordr.f,v 1.3 1999/07/07 11:19:36 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('ORDR')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IVORDR=',IVORDR,
     *      ' UNSD=',UNSD,
     *      ' ICOMPL=',ICOMPL,
     *      ' LARRAY=',LARRAY,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*)
     *      ' NAMAP=',NAMAP,
     *      ' NAFMAP=',NAFMAP,
     *      ' NAMAPX=',NAMAPX,
     *      ' ICBASN=',ICBASN,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=13
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,50) LARRAY,MINLEN
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 30
         ENDIF
C
      PARMID=' '
      IRRREC=0
C
      IF (ICOMPL.EQ.0) THEN
C     READ OLD PARAMETER RECORD
         IPTR=0
         CALL SUDOPN (1,'PPP ',IERR)
         CALL RPPREC (PARMID,'ORDR',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *      IERR)
         IRRREC=IERR
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.2) GO TO 10
            CALL SRPPST (PARMID,'ORDR',IPTR,LARRAY,NFILL,IPTRNX,IRRREC)
            WRITE (LP,60) IRRREC
            CALL SUERRS (LP,2,-1)
            ISTAT=2
            GO TO 30
            ENDIF
C     SET COMPLETE FLAG TO INCOMPLETE
         ARRAY(2)=0.01
         GO TO 20
         ENDIF
C
10    NPOS=0
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      NPOS=NPOS+1
      ARRAY(NPOS)=IVORDR+.01
C
C  STORE COMPLETE FLAG
      IF (IRRREC.EQ.0) THEN
         NPOS=NPOS+1
         ARRAY(NPOS)=1.01
         ENDIF
      IF (IRRREC.EQ.2) THEN
         NPOS=NPOS+1
         ARRAY(NPOS)=0.01
         ENDIF
C
C  STORE CURRENT DATE
      CALL UDATEI (NORDMO,NORDDA,NORDYR,NORDHM,NSEC,JULDAY,IERR)
      NPOS=NPOS+1
      ARRAY(NPOS)=NORDMO+.01
      NPOS=NPOS+1
      ARRAY(NPOS)=NORDDA+.01
      NPOS=NPOS+1
      ARRAY(NPOS)=NORDYR+.01
      NPOS=NPOS+1
      ARRAY(NPOS)=NORDHM+.01
C
C  STORE NUMBER OF MAP AREAS ADDED SINCE ORDER RUN
      NPOS=NPOS+1
      ARRAY(NPOS)=NAMAP+.01
C
C  STORE NUMBER OF FMAP AREAS ADDED SINCE ORDER RUN
      NPOS=NPOS+1
      ARRAY(NPOS)=NAFMAP+.01
      IF (IVORDR.EQ.1) THEN
         NPOS=NPOS+1
         ARRAY(NPOS)=UNSD
         NPOS=NPOS+1
         ARRAY(NPOS)=UNSD
         ENDIF
C
      IF (IVORDR.GT.1) THEN
C     STORE NUMBER OF MAPX AREAS ADDED SINCE ORDER RUN
         NPOS=NPOS+1
         ARRAY(NPOS)=NAMAPX+.01
         IF (IVORDR.EQ.2) THEN
            NPOS=NPOS+1
            ARRAY(NPOS)=UNSD
            NPOS=NPOS+1
            ARRAY(NPOS)=UNSD
            ENDIF
         ENDIF
C
      IF (IVORDR.GT.2) THEN
C     STORE INDICATOR IF ANY BASINS USED BY MAPX AREAS HAVE BEEN
C     CHANGED SINCE ORDER RUN
         NPOS=NPOS+1
         ARRAY(NPOS)=ICBASN+.01
         IF (IVORDR.EQ.3) THEN
            NPOS=NPOS+1
            ARRAY(NPOS)=UNSD
            NPOS=NPOS+1
            ARRAY(NPOS)=UNSD
            ENDIF
         ENDIF
C
      IF (IVORDR.GT.3) THEN
C     STORE INDICATOR IF ANY FMAP AREAS USED BY MAP AREAS HAVE BEEN
C     CHANGED SINCE ORDER RUN
         NPOS=NPOS+1
         ARRAY(NPOS)=ICFMAP+.01
         IF (IVORDR.EQ.4) THEN
            NPOS=NPOS+1
            ARRAY(NPOS)=UNSD
            NPOS=NPOS+1
            ARRAY(NPOS)=UNSD
            ENDIF
         ENDIF
C
C  WRITE PARAMETER RECORD TO FILE
20    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPTR=0
      CALL WPPREC (PARMID,'ORDR',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.GT.0) THEN
         CALL SWPPST (PARMID,'ORDR',NPOS,IPTR,IERR)
         WRITE (LP,70) IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=3
         ENDIF
C
30    IF (ISTAT.EQ.0) THEN
         IF (DISP.EQ.'NEW') THEN
            WRITE (LP,80) 'WRITTEN'
            CALL SULINE (LP,2)
            ENDIF
         IF (DISP.EQ.'OLD') THEN
            WRITE (LP,80) 'UPDATED'
            CALL SULINE (LP,2)
            ENDIF
         CALL SUDWRT (1,'PPP ',IERR)
         IF (LDEBUG.GT.0) THEN
            CALL SUPDMP ('ORDR','REAL',0,NPOS,ARRAY,ARRAY)
            ENDIF
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,90)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SWORDR')
50    FORMAT ('0*** ERROR - IN SWORDR - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *   'NUMBER OF WORDS NEEDED=',I5)
60    FORMAT ('0*** ERROR - IN SWORDR - UNSUCCESSFUL CALL TO RPPREC : ',
     *   'STATUS CODE=',I3)
70    FORMAT ('0*** ERROR - IN SWORDR - UNSUCCESSFUL CALL TO WPPREC : ',
     *   'STATUS CODE=',I3)
80    FORMAT ('0*** NOTE - ORDR PARAMETERS SUCCESSFULLY ',A,'.')
90    FORMAT (' *** EXIT SWORDR')
C
      END
