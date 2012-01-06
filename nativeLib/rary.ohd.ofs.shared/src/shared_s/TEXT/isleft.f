C MODULE ISLEFT
C-----------------------------------------------------------------------
C
      FUNCTION ISLEFT (NLINES)
C
C  FUNCTION TO INDICATE IF SPECIFIED LINES WILL FIT ON PAGE.
C
C  RETURN VALUE = 1 IF LINES WILL NOT FIT ON PAGE
C               = 0 IF LINES WILL FIT ON PAGE
C
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/isleft.f,v $
     . $',                                                             '
     .$Id: isleft.f,v 1.3 2001/06/13 10:48:15 mgm Exp $
     . $' /
C    ===================================================================
C
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      ISLEFT=0
C
      IF (NPSNLN+NLINES.GT.NPSMLN) ISLEFT=1
C
      IF (ISTRCE.GT.0.OR.LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' EXIT ISLEFT -',
     *      ' NPSNLN=',NPSNLN,
     *      ' NLINES=',NLINES,
     *      ' NPSMLN=',NPSMLN,
     *      ' ISLEFT=',ISLEFT,
     *      ' '
CCC         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
