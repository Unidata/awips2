C MODULE SUPRDW
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK SIZE OF WORK ARRAY FOR PROCESSED DATA BASE
C  READ/WRITE ROUTINES
C
      SUBROUTINE SUPRDW (TYPE,LWKBUF,ITIME,NPERIT,NXBUF,IPRERR,LWNEED,
     *   NUMERR,ISTAT)
C
      CHARACTER*4 TYPE
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'prdcommon/pdatas'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suprdw.f,v $
     . $',                                                             '
     .$Id: suprdw.f,v 1.2 1998/07/06 12:21:42 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *   ' TYPE=',TYPE,
     *   ' LWKBUF=',LWKBUF,
     *   ' ITIME=',ITIME,
     *   ' NPERIT=',NPERIT,
     *   ' NXBUF=',NXBUF,
     *   ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  GET MAXIMUM DAYS OF DATA THAT CAN BE STORED
      IF (TYPE.NE.'FMAP') THEN
         MAXDAY=IPRDMD(TYPE)
         ELSE
            MAXDAY=IPRDMF('MAP ')
         ENDIF
      IF (MAXDAY.EQ.0) THEN
         ISTAT=1
         IF (IPRERR.EQ.1) THEN
            WRITE (LP,60) TYPE
            CALL SUERRS (LP,2,NUMERR)
            ENDIF           
         GO TO 30
         ENDIF
C
C  CHECK FOR VALID DATA TIME INTERVAL
      IF (24/ITIME*ITIME.NE.24) THEN
         ISTAT=2
         IF (IPRERR.EQ.1) THEN
            WRITE (LP,70) ITIME
            CALL SUERRS (LP,2,NUMERR)
            ENDIF          
         GO TO 30
         ENDIF
C
C  COMPUTE LENGTH OF WORK ARRAY NEEDED 
      NTSVAL=24/ITIME*NPERIT*MAXDAY
      LWNEED=(((LENHED+NXBUF+NTSVAL-1)/LRECLT)+1)*LRECLT
      IF (LWNEED.GT.LWKBUF) THEN
         ISTAT=3
         IF (IPRERR.EQ.1) THEN
            WRITE (LP,80) LWKBUF,ITIME,TYPE,LWNEED
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 30
         ENDIF
C
30    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' MAXDAY=',MAXDAY,
     *      ' LWNEED=',LWNEED,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,110)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SUPRDW')
60    FORMAT ('0*** ERROR - IN SUPRDW - DATA TYPE ',A,
     *   ' IS NOT DEFINED IN THE PROCESSED DATA BASE.')
70    FORMAT ('0*** ERROR - IN SUPRDW - ',I2,' IS AN INVALID ',
     *   'DATA TIME INTERVAL.')
80    FORMAT ('0*** ERROR - IN SUPRDW - ',
     *   'SIZE OF WORK ARRAY (',I4,') IS TOO SMALL '
     *   'FOR ',I2,' HOUR TIME SERIES ',
     *   'FOR DATA TYPE ',A,'. ',
     *   I4,' WORDS ARE NEEDED.')
110   FORMAT (' *** EXIT SUPRDW')
C
      END
