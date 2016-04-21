C MODULE SUIDF1
C-----------------------------------------------------------------------
C
C  ROUTINE TO STORE THE PAGE NUMBER ON WHICH THE PROCESSING OF AN
C  IDENTIFIER WAS STARTED.
C
      SUBROUTINE SUIDF1 (IDENT,NUMID,NUMEXC,NPERID,LSARAY,ISARAY,ISTAT)
C
      REAL BLNK/4H    /
C
      DIMENSION ISARAY(LSARAY)
      DIMENSION IDENT(2)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suidf1.f,v $
     . $',                                                             '
     .$Id: suidf1.f,v 1.3 2001/06/13 14:05:14 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('UTIL')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUIDF1'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,30) IDENT,NPERID,LSARAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR MAXIMUM NUMBER OF IDENTIFIERS
      MAXID=LSARAY/NPERID
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MAXID=',MAXID,
     *      ' NUMID=',NUMID,
     *      ' NUMEXC=',NUMEXC
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (NUMID+1.GT.MAXID) THEN
         ISTAT=1
         NUMEXC=NUMEXC+1
         IF (NUMEXC.GT.1) GO TO 10
            WRITE (LP,40) MAXID
            CALL SUWRNS (LP,2,-1)
            GO TO 10
         ENDIF
C
      NUMID=NUMID+1
      IPOS=NUMID*NPERID
C
C  STORE IDENTIFIER
      CALL SUBSTR (IDENT,1,8,ISARAY(IPOS-3),1)
C
C  STORE PAGE NUMBER
      ISARAY(IPOS-1)=NPSPAG
      CALL SUBSTR (BLNK,1,4,ISARAY(IPOS),1)
C
10    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUIDF1 : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' IDENT=',2A4,3X,'NPERID=',I2,3X,
     *   'LSARAY=',I5)
40    FORMAT ('0*** WARNING - MAXIMUM NUMBER OF IDENTIFIERS THAT CAN ',
     *   'BE SUMMARIZED (',I4,') EXCEEDED. ADDITIONAL IDENTIFIERS ',
     *   'WILL NOT BE PROCESSED.')
C
      END
