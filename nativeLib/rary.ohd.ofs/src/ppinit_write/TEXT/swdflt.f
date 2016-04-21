C MODULE SWDFLT
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE USER DEFAULTS.
C
      SUBROUTINE SWDFLT (LARRAY,ARRAY,NPSMLN,IOPNWP,IOPOVP,IOPCLG,
     *   ISTAT)
C
      CHARACTER*4 PARMTP
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swdflt.f,v $
     . $',                                                             '
     .$Id: swdflt.f,v 1.2 2000/07/21 20:02:04 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SWDFLT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UGNL')
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
      IF (IERR.GT.0) GO TO 50
      PARMID=' '
      PARMTP='USER'
      IPTR=0
      CALL RPPREC (PARMID,PARMTP,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.NE.0) THEN
         ISTAT=IERR
         IF (ISTAT.EQ.2.OR.ISTAT.EQ.6) THEN
	    WRITE (LP,80)
            CALL SUWRNS (LP,2,-1)
	    ELSE
               CALL SRPPST (PARMID,PARMTP,IPTR,LARRAY,NFILL,IPTRNX,IERR)
               WRITE (LP,70) IERR
               CALL SUERRS (LP,2,-1)
            GO TO 40
	    ENDIF    
	ENDIF
C
C  SET STARTING POSITION OF DEFAULTS
      NPOS=26
C
C  STORE NUMBER OF DEFAULTS
      NDFLT=6
      ARRAY(NPOS)=NDFLT+.01
C
      IF (NDFLT.GT.0) THEN
C     STORE PAGESIZE OPTION
         NPOS=NPOS+1
         ARRAY(NPOS)=NPSMLN+.01
C     STORE NEWPAGE OPTION
         NPOS=NPOS+1
         ARRAY(NPOS)=IOPNWP+.01
C     STORE OVERPRNT OPTION
         NPOS=NPOS+1
         ARRAY(NPOS)=IOPOVP+.01
C     STORE CMDLOG OPTION
         NPOS=NPOS+1
         ARRAY(NPOS)=IOPCLG+.01
	 ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' NDFLT=',NDFLT,
     *      ' NPSMLN=',NPSMLN,
     *      ' IOPNWP=',IOPNWP,
     *      ' IOPOVP=',IOPOVP,
     *      ' IOPCLG=',IOPCLG,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*)
     *      ' NPOS=',NPOS,
     *      ' NFILL=',NFILL,
     *      ' IPTRNX=',IPTRNX,
     *      ' '
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP (PARMTP,'BOTH',0,NFILL,ARRAY,ARRAY)
         ENDIF
C
C  WRITE PARAMETER RECORD TO FILE
      CALL SUDOPN (1,'PPP ',IERR)
      IPTR=0
      CALL WPPREC (PARMID,PARMTP,NFILL,ARRAY,IPTR,IERR)
      IF (IERR.NE.0) THEN
         CALL SWPPST (PARMID,PARMTP,NFILL,IPTR,IERR)
         WRITE (LP,90) IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=3
         ENDIF
C
      IF (LDEBUG.GT.0) CALL SUPDMP (PARMTP,'BOTH',0,NFILL,ARRAY,ARRAY)
C
40    IF (ISTAT.EQ.0) THEN
         WRITE (LP,100)
         CALL SULINE (LP,2)
         CALL SUDWRT (1,'PPP ',IERR)
         ENDIF
      IF (ISTAT.GT.0) THEN
         WRITE (LP,110)
         CALL SULINE (LP,2)
         ENDIF
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SWDFLT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('0*** ERROR - IN SWDFLT - UNSUCCESSFUL CALL TO RPPREC : ',
     *   'STATUS CODE=',I2)
80    FORMAT ('0*** WARNING - UGNL PARAMETERS NOT DEFINED. USER ',
     *   'DEFAULTS CANNOT BE WRITTEN.')
90    FORMAT ('0*** ERROR - IN SWDFLT - UNSUCCESSFUL CALL TO WPPREC : ',
     *   'STATUS CODE=',I2)
100   FORMAT ('0*** NOTE - UGNL PARAMETERS SUCCESSFULLY ',
     *   'UPDATED WITH USER DEFAULTS.')
110   FORMAT ('0*** NOTE - UGNL PARAMETERS NOT SUCCESSFULLY ',
     *   'UPDATED WITH USER DEFAULTS.')
C
      END
