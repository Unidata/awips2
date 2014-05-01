C MEMBER SRDFLT
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/10/95.11:36:28 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO READ USER DEFAULTS
C
      SUBROUTINE SRDFLT (LARRAY,ARRAY,NDFLT,NPSMLN,IOPNWP,IOPOVP,IOPCLG,
     *   IPRERR,ISTAT)
C
C
      CHARACTER*8 BLNKID/' '/
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_read/RCS/srdflt.f,v $
     . $',                                                             '
     .$Id: srdflt.f,v 1.1 1995/09/17 19:14:47 dws Exp $
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
      IPTR=0
      CALL RPPREC (BLNKID,'USER',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.GT.0) THEN
         ISTAT=IERR
         IF (IPRERR.GT.0) THEN
            IF (ISTAT.EQ.2) THEN
               WRITE (LP,40)
               CALL SUWRNS (LP,2,-1)
               ELSE
                  CALL SRPPST (BLNKID,'USER',IPTR,LARRAY,NFILL,IPTRNX,
     *               IERR)
                  WRITE (LP,50) IERR
                  CALL SUERRS (LP,2,-1)
               ENDIF
            ENDIF
         ENDIF
C
      NPOS=26
C
C  SET NUMBER OF DEFAULTS
      NDFLT=ARRAY(NPOS)
C
      IF (NDFLT.LE.0) GO TO 10
C
C  SET PAGESIZE OPTION
      NPOS=NPOS+1
      NPSMLN=ARRAY(NPOS)
C
C  SET NEWPAGE OPTION
      NPOS=NPOS+1
      IOPNWP=ARRAY(NPOS)
C
C  SET OVERPRNT OPTION
      NPOS=NPOS+1
      IOPOVP=ARRAY(NPOS)
C
C  SET CMDLOG OPTION
      NPOS=NPOS+1
      IOPCLG=ARRAY(NPOS)
C
10    IF (LDEBUG.GT.0) THEN
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
         CALL SUPDMP ('USER','BOTH',0,NPOS,ARRAY,ARRAY)
         ENDIF
C
20    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,60) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SRDFLT')
40    FORMAT ('0*** WARNING - UGNL PARAMETERS NOT DEFINED. USER ',
     *   'DEFAULTS CANNOT BE READ.')
50    FORMAT ('0*** ERROR - IN SRDFLT - UNSUCCESSFUL CALL TO RPPREC : ',
     *   'STATUS CODE=',I2)
60    FORMAT (' *** EXIT SRDFLT - ISTAT=',I2)
C
      END
