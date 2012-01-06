C MEMBER SOFPID
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/09/95.12:04:40 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO FIND FUTURE MAP AREA ASSOCIATED WITH REGULAR AREA
C
      SUBROUTINE SOFPID (DTYPE,RMPID,MFMPID,NFMPID,FMPID,
     *   LARRAY,ARRAY,NUMERR,ISTAT)
C
C
      CHARACTER*4 DTYPE
      CHARACTER*8 FMPID(*)
      CHARACTER*8 RMPID,XFMPID
      CHARACTER*20 DESC
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION IHEAD(22),UNUSED(5)
      PARAMETER (MAXBASN=99)                                   !cfan
      DIMENSION BASNID00(MAXBASN*2)                            !cfan
      integer NUMB
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sofpid.f,v $
     . $',                                                             '
     .$Id: sofpid.f,v 1.2 2002/10/10 15:57:17 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('ORDR','ORDRFPID','SOFPID',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,20)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('ORDR','ORDRFPID','SOFPID',LDEBUG)
C
      ISTAT=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' MFMPID=',MFMPID,
     *      ' NFMPID=',NFMPID,
     *      ' DTYPE=',DTYPE,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      XFMPID=' '
C
C  GET FUTURE MAP AREA TIME SERIES IDENTIFIER
      IF (DTYPE.EQ.'MAP') THEN
         MAXX=1
         CALL RPRDH (RMPID,DTYPE,MAXX,IHEAD,NUMX,XBUF,XFMPID,IERR)
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.1) THEN
               WRITE (LP,30) 'TIME SERIES',DTYPE,RMPID
               CALL SUERRS (LP,2,NUMERR)
               ELSE
                  CALL SRPRST ('RPRDH   ',RMPID,DTYPE,MAXX,NUMERR,IERR)
               ENDIF
            ISTAT=1
            GO TO 10
            ENDIF
         ENDIF
      IF (DTYPE.EQ.'MAPX') THEN
         IPTR=0
         IPRERR=1
         ITIME=1
         NUMB=0
         CALL SRMAPX (IVMAPX,RMPID,ITIME,DESC,BASNID00,XFMPID,
     *      UNUSED,LARRAY,ARRAY,IPTR,IPRERR,IPTRNX,IERR,NUMB)
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.2) THEN
               WRITE (LP,30) 'PARAMETER RECORD',DTYPE,RMPID
               CALL SUERRS (LP,2,NUMERR)
               ENDIF
            ISTAT=1
            GO TO 10
            ENDIF
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' RMPID=',RMPID,
     *      ' XFMPID=',XFMPID,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK FUTURE MAP AREA IDENTIFIER
      IF (XFMPID.EQ.' ') THEN
         WRITE (LP,40) DTYPE,RMPID
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 10
         ENDIF
C
C  CHECK IF MAXIMUM NUMBER OF IDENTIFIERS EXCEEDED
      NFMPID=NFMPID+1
      IF (NFMPID.GT.MFMPID) THEN
         WRITE (LP,50) MFMPID
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=2
         GO TO 10
         ENDIF
C
C  STORE FUTURE MAP IDENTIFIER
      FMPID(NFMPID)=XFMPID
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' NFMPID=',NFMPID,
     *      ' FMPID(NFMPID)=',FMPID(NFMPID),
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
10    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER SOFPID')
30    FORMAT ('0*** ERROR - ',A,' NOT FOUND FOR ',A4,
     *   ' AREA ',A,'.')
40    FORMAT ('0*** ERROR - FUTURE MAP AREA IDENTIFIER IS BLANK ',
     *   'FOR ',A,' IDENTIFIER,',A,'.')
50    FORMAT ('0*** ERROR - IN SOFPID - MAXIMUM NUMBER OF FUTURE MAP ',
     *   'IDENTIFIERS THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
60    FORMAT (' *** EXIT SOFPID')
C
      END
