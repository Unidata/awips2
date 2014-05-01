C MEMBER SUGTUR
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/13/94.15:35:30 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE SUGTUR (LARRAY,ARRAY,IPRER1,ISTAT)
C
C  DESC  ROUTINE TO READ USER RRS PARAMETERS AND FILL COMMON BLOCK.
C
      CHARACTER*8 BLNKID/' '/
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/surrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtur.f,v $
     . $',                                                             '
     .$Id: sugtur.f,v 1.1 1995/09/17 19:15:39 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,10)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      ISTAT=0
C
C  READ USER RRS PARAMETERS
      IPRERR=0
      IF (IPRER1.GT.0) IPRERR=IPRER1
      INCLUDE 'scommon/callsrurrs'
      IF (IERR.GT.0) THEN
         IPTR=-999
         NFILL=-999
         IPTRNX=-999
         IF (IERR.EQ.2) THEN
C  PARAMETER RECORD NOT FOUND
            ISTAT=-IERR
            IF (IPRER1.NE.-1) CALL SRPPST (BLNKID,'URRS',IPTR,
     *         LARRY,NFILL,IPTRNX,ISTAT)
            ELSE
               ISTAT=IERR
               CALL SRPPST (BLNKID,'URRS',IPTR,LARRAY,NFILL,IPTRNX,
     *            ISTAT)
            ENDIF
         ISTAT=IERR
         ENDIF
C
      IF (ISTAT.EQ.0) THEN
         IURRSX=1
         IF (LDEBUG.GT.0) THEN
      INCLUDE 'scommon/callspurrs'
            ENDIF
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SUGTUR')
20    FORMAT (' *** EXIT SUGTUR : ISTAT=',I2)
C
      END
