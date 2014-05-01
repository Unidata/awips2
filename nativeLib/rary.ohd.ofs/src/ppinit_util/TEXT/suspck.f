C MODULE SUSPCK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK FOR NON-SUPPORTED FEATURES.
C
      SUBROUTINE SUSPCK (XCODE,XCHAR,LCHAR,XTYPE,IRUNCK,ICMERR,NCSKIP,
     *   ISTAT)
C
      CHARACTER XCODE*(*)
      CHARACTER*4 XCHAR(LCHAR)
      CHARACTER*8 XTYPE
C
      INCLUDE 'uio'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suspck.f,v $
     . $',                                                             '
     .$Id: suspck.f,v 1.2 1998/07/06 12:46:15 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      ISTAT=0
C
C  CHECK IF CODE IS FOR A NON-SUPPORTED FEATURE
      ISUPT=ISUPRT(XCODE)
      IF (ISUPT.EQ.0) THEN
         CALL ULENTH (XTYPE,LEN(XTYPE),LXTYPE)
         IRUNCK=0
         IF (IRUNCK.EQ.1) THEN
            WRITE (LP,30) XTYPE(1:LXTYPE),(XCHAR(N),N=1,2)
            CALL SUERRS (LP,2,-1)
            ENDIF
         WRITE (LP,40)
         CALL SULINE (LP,1)
         WRITE (LP,40)
         WRITE (LP,50) XTYPE(1:LXTYPE),(XCHAR(N),N=1,2),XTYPE(1:LXTYPE)
         IF (IOPOVP.EQ.1) THEN
            WRITE (LP,50) XTYPE(1:LXTYPE),(XCHAR(N),N=1,2),
     *         XTYPE(1:LXTYPE)
            WRITE (LP,50) XTYPE(1:LXTYPE),(XCHAR(N),N=1,2),
     *         XTYPE(1:LXTYPE)
            ENDIF
         CALL SUERRS (LP,1,-1)
         ICMERR=1
         NCSKIP=NRDCRD
         ISTAT=1
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,70)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER SUSPCK')
30    FORMAT ('0*** ERROR - ',A,' ',2A,' IS NOT CURRENTLY ',
     *   'ALLOWED. RUNCHECK OPTION SET. INPUT WILL BE CHECKED FOR ',
     *   'ERRORS.')
40    FORMAT (' ')
50    FORMAT ('+*** ERROR - ',A,' ',2A,' IS NOT CURRENTLY ',
     *   'ALLOWED. ',A,' WILL NOT BE PROCESSED.')
60    FORMAT ('0*** NOTE - RUNCHECK OPTION HAS BEEN SPECIFIED. INPUT ',
     *   'WILL BE CHECKED FOR ERRORS.')
70    FORMAT (' *** EXIT SUSPCK')
C
      END
