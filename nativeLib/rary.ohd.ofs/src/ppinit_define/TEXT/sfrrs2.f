C MODULE SFRRS2
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINE STATION RRS TIME SERIES.
C
      SUBROUTINE SFRRS2 (STAID,DESCRP,STALOC,
     *   RRSTYP,NRRSTP,IRTIME,NVLPOB,MNODAY,
     *   NUMOBS,RUNITS,URMISS,IRSTAT,ITSREC,NMISS,
     *   NXBUF,LIWORK,IWORK,
     *   NUMERR,NUMWRN,ISTAT)
C
      CHARACTER*4 DTYPE,DUNIT
C
C  STAN PARAMETER VARIABLES
      INCLUDE 'scommon/dimsta'
C
C  RRS PARAMETER VARIABLES
      CHARACTER*4 RUNITS(NRRSTP)
      DIMENSION IRSTAT(NRRSTP)
      INCLUDE 'scommon/dimrrs'
C
C  PROCESS DATABASE READ/WRITE VARIABLES
      CHARACTER*8 FTSID
      PARAMETER (LXBUF=1)
      DIMENSION XBUF(LXBUF)
      DIMENSION IWORK(LIWORK)
      PARAMETER (LTSDAT=1000)
      DIMENSION TSDAT(LTSDAT)
      DIMENSION IHEADO(22),IHEADN(22)
C
      INCLUDE 'uio'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfrrs2.f,v $
     . $',                                                             '
     .$Id: sfrrs2.f,v 1.5 1999/04/26 11:47:22 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,50)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('RRS ')
C
      ISTAT=0
C
      FTSID=' '
      IPRERR=1
C
C  WRITE OR UPDATE TIME SERIES HEADER
      DO 20 I=1,NRRSTP
C     CHECK IF TIME SERIES NOT TO BE CREATED
         IF (IRSTAT(I).EQ.-1) GO TO 20
C     SET DATA TYPE         
         DTYPE=URMISS(I)
         IF (DTYPE.EQ.'SAME') DTYPE=RRSTYP(I)
C     FIND TYPE IN DIRECTORY
         CALL PFDTYP (DTYPE,INDEXD)
         IF (INDEXD.EQ.0) THEN
            WRITE (LP,55) DTYPE,STAID
            CALL SUERRS (LP,2,NUMERR)
            ISTAT=1
            GO TO 20
            ENDIF
C     SET NUMBER OF VALUES PER TIME INTERVAL         
         NPERIT=DATFIL(13,INDEXD)
C     CHECK IF TIME SERIES EXISTS BUT HAS INCOMPATIBLE DATA TIME 
C     INTERVAL
         IF (IRSTAT(I).EQ.3) THEN
            INDFLD=-2
            CALL SLPRD (INDFLD,STAID,DTYPE,IERR)
            IRSTAT(I)=0
            ENDIF
C     CHECK IF TIME SERIES DOES NOT EXIST
         IF (IRSTAT(I).EQ.0) THEN
C        CREATE TIME SERIES         
            CALL WPRDH (STAID,DTYPE,IRTIME(I),RUNITS(I),NPERIT,STALOC,
     *         FTSID,DESCRP,NXBUF,XBUF,LSWORK,SWORK,IRECTS,IERR)
            IF (IERR.NE.0) THEN
               CALL SWPRST ('WPRDH   ',STAID,DTYPE,IRTIME(I),RUNITS(I),
     *            FTSID,LSWORK,NPERIT,IERR)
               WRITE (LP,60) DTYPE,STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
            WRITE (LP,100) DTYPE,'CREATED',STAID
            CALL SULINE (LP,2)
            GO TO 10
            ENDIF
C     CHECK IF TIME SERIES EXISTS AND HAS SAME TIME INTERVAL
         IF (IRSTAT(I).EQ.1) THEN
            CALL WPRDC (STAID,DTYPE,RUNITS(I),STALOC,
     *         DESCRP,FTSID,NXBUF,XBUF,LSWORK,SWORK,IRECTS,IERR)
            IF (IERR.NE.0) THEN
               CALL SWPRST ('WPRDC   ',STAID,DTYPE,IRTIME(I),RUNITS(I),
     *            FTSID,LSWORK,NPERIT,IERR)
               IF (IERR.EQ.8) GO TO 15
               WRITE (LP,70) DTYPE,STAID,IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
            WRITE (LP,100) DTYPE,'CHANGED',STAID
            CALL SULINE (LP,2)
            GO TO 10
            ENDIF
C     CHECK IF TIME SERIES EXISTS AND HAS COMPATIBLE TIME INTERVAL
         IF (IRSTAT(I).EQ.2) THEN
            IRTIMEN=IRTIME(I)
C        READ OLD TIME SERIES HEADER
            CALL RPRDH (STAID,DTYPE,LXBUF,IHEADO,NXBUF,XBUF,FTSID,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,90) 'RPRDH',DTYPE,STAID,IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
C        READ DATA FROM OLD TIME SERIES
            JHOUR=IHEADO(14)
            IRTIMEO=IHEADO(2)
            NUMVALO=IHEADO(5)
            CALL UMEMOV (IHEADO(11),DUNIT,1)
            RMISS=-999.
            IFPTR=IHEADO(7)
            IF (LDEBUG.GT.0) THEN
               CALL RPRDD (STAID,DTYPE,JHOUR,IRTIMEO,NUMVALO,DUNIT,
     *            RMISS,LTSDAT,TSDAT,IFPTR,LIWORK,IWORK,IERR)
               WRITE (IOSDBG,*) 'RPRDD CALLED WITH IRTIMEO=',IRTIMEO
               WRITE (IOSDBG,'('' TSDAT='',10F10.3)')
     *            (TSDAT(N),N=1,NUMVALO)
               NPER=10
               NLINES=(NUMVALO+NPER-1)/NPER
               CALL SULINE (IOSDBG,NLINES)
               ENDIF
C        CHECK SIZE OF WORK ARRAY TO READ OLD DATA
            CALL SUPRDW (DTYPE,LIWORK,IRTIMEO,NPERIT,NXBUF,IPRERR,
     *         LWNEED,NUMERR,IERR)
            IF (IERR.GT.0) THEN
               WRITE (LP,75) 'READ OLD',DTYPE,STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
C        ADJUST STARTING HOUR
            IDIFF=IRTIMEN-IRTIMEO
            JHOUR=JHOUR+IDIFF
C        READ DATA USING NEW DATA TIME INTERVAL
            NUMVALN=NUMVALO*IRTIMEO/IRTIMEN            
            CALL RPRDD (STAID,DTYPE,JHOUR,IRTIMEN,NUMVALN,DUNIT,
     *         RMISS,LTSDAT,TSDAT,IFPTR,LIWORK,IWORK,IERR)
            IF (IERR.NE.0) THEN
               IF (IERR.NE.2) THEN
                  WRITE (LP,90) 'RPRD',DTYPE,STAID,IERR
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 20
                  ENDIF
               ENDIF
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*) 'RPRDD CALLED WITH IRTIMEN=',IRTIMEN
               CALL SULINE (IOSDBG,1)
               WRITE (IOSDBG,'('' TSDAT='',10F10.3)')
     *            (TSDAT(N),N=1,NUMVALN)
               NPER=10
               NLINES=(NUMVALN+NPER-1)/NPER
               CALL SULINE (IOSDBG,NLINES)
               ENDIF
C        CHECK SIZE OF WORK ARRAY TO WRITE NEW DATA
            CALL SUPRDW (DTYPE,LIWORK,IRTIMEN,NPERIT,NXBUF,IPRERR,
     *         LWNEED,NUMERR,IERR)
            IF (IERR.GT.0) THEN
               WRITE (LP,75) 'WRITE OLD',DTYPE,STAID
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
C        DELETE OLD TIME SERIES
            INDFLD=-2
            CALL SLPRD (INDFLD,STAID,DTYPE,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,90) 'SLPRD',DTYPE,STAID,IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
C        CREATE NEW TIME SERIES
            CALL WPRDH (STAID,DTYPE,IRTIMEN,RUNITS(I),NPERIT,STALOC,
     *         FTSID,DESCRP,NXBUF,XBUF,LSWORK,SWORK,IRECTS,IERR)
            IF (IERR.NE.0) THEN
               CALL SWPRST ('WPRDH   ',STAID,DTYPE,IRTIMEN,RUNITS(I),
     *            FTSID,LSWORK,NPERIT,IERR)
               WRITE (LP,60) DTYPE,STAID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            WRITE (LP,100) DTYPE,'CREATED',STAID
            CALL SULINE (LP,2)
C        CHECK IF OLD TIME SERIES HAD ANY DATA VALUES
            IF (NUMVALO.EQ.0) THEN
               WRITE (LP,80) DTYPE,STAID
               CALL SULINE (LP,2)
               GO TO 10
               ENDIF
C        READ NEW TIME SERIES HEADER
            CALL RPRDH (STAID,DTYPE,LXBUF,IHEADN,NXBUF,XBUF,FTSID,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,90) 'RPRDH',DTYPE,STAID,IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
C        WRITE DATA TO NEW TIME SERIES
            NUMINT=IHEADN(4)
            NUMVAL=IHEADN(4)
            ICALL=0
            IREC=IRECTS
            CALL WPRDD (STAID,DTYPE,JHOUR,IRTIMEN,NUMINT,DUNIT,NUMVAL,
     *         LTSDAT,TSDAT,IFPTR,ICALL,LIWORK,IWORK,IREC,IERR)
            IF (IERR.NE.0) THEN
               WRITE (LP,90) 'WPRDD',DTYPE,STAID,IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 20
               ENDIF
            IF (LDEBUG.GT.0) THEN
               CALL RPRDD (STAID,DTYPE,JHOUR,IRTIMEN,NUMVAL,DUNIT,
     *            RMISS,LTSDAT,TSDAT,IFPTR,LIWORK,IWORK,IERR)
               WRITE (IOSDBG,*) 'RPRDD CALLED WITH IRTIMEN=',IRTIMEN
               CALL SULINE (IOSDBG,1)
               WRITE (IOSDBG,'('' TSDAT='',10F10.3)')
     *            (TSDAT(N),N=1,NUMVAL)
               NPER=10
               NLINES=(NUMVAL+NPER-1)/NPER
               CALL SULINE (IOSDBG,NLINES)
               ENDIF
            WRITE (LP,100) DTYPE,'COPIED',STAID
            CALL SULINE (LP,2)
            GO TO 10
            ENDIF
         WRITE (LP,105) IRSTAT(I),DTYPE,STAID
         CALL SUERRS (LP,2,-1)
         GO TO 20
10       CALL SUDWRT (1,'PRD ',IERR)
15       ITSREC(I)=IRECTS
20       CONTINUE
C
C  CHECK FOR ERRORS ENCOUNTERED WRITING TIME SERIES
      IF (NUMERR.GT.0) THEN
         WRITE (LP,110) NUMERR
         CALL SULINE (LP,2)
         ISTAT=1
         GO TO 40
         ENDIF
C
C  COUNT NUMBER OF TYPES FOR WHICH MISSING IS ALLOWED
      NMISS=0
      DO 30 I=1,NRRSTP
         IF (URMISS(I).NE.'SAME') NMISS=NMISS+1
30       CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' NRRSTP=',NRRSTP,
     *      ' NMISS=',NMISS,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
40    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,120) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER SFRRS2')
55    FORMAT ('0*** ERROR - IN SFRRS2 - DATA TYPE ',A,' ',
     *   'NOT FOUND IN PROCESSED DATA BASE DIRECTORY ',
     *   'FOR STATION ',A,'.')
60    FORMAT ('0*** ERROR - ',A,' TIME SERIES FOR STATION ',A,' ',
     *   'NOT DEFINED BECAUSE ERRORS ENCOUNTERED.')
70    FORMAT ('0*** ERROR - ',A,' TIME SERIES FOR STATION ',A,' ',
     *   'NOT REDEFINED BECAUSE ERRORS ENCOUNTERED.')
75    FORMAT ('0*** ERROR - IN SFRRS2 - ',
     *   'SIZE OF WORK ARRAY IS TOO SMALL TO ',A,' TIME SERIES ',
     *   'FOR DATA TYPE ',A,' ',
     *   'FOR STATION ',A,'.')
80    FORMAT ('0*** NOTE - ',A,' TIME SERIES ',
     *   'FOR STATION ',A,' ',
     *   'NOT COPIED BECAUSE THERE ARE NO DATA VALUES.')
90    FORMAT ('0*** ERROR - CALLING ROUTINE ',A,' ',
     *   'FOR ',A,' TIME SERIES FOR STATION ',A,'. ',
     *   'STATUS CODE = ',I2)
100   FORMAT ('0*** NOTE - ',A,' TIME SERIES SUCCESSFULLY ',A,' ',
     *   'FOR STATION ',A,'.')
105   FORMAT ('0*** ERROR - ',I2,' IS AN INVALID IRSTAT VALUE ',
     *   'FOR ',A,' TIME SERIES FOR STATION ',A,'.')
110   FORMAT ('0*** NOTE - ',I2,' ',
     *   'ERRORS ENCOUNERED WRITING TO THE PROCESSED DATA BASE.')
120   FORMAT (' *** EXIT SFRRS2 - ISTAT=',I2)
C
      END
