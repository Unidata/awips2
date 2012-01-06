C MODULE ISUPRT
C-----------------------------------------------------------------------
C
      FUNCTION ISUPRT (XNAME)
C
C  FUNCTION TO CHECK IF FEATURES IS NOT SUPPORTED
C
C  RETURN VALUE = 0 IF THE FOUR-CHARACTER ARGUMENT MATCHES AN
C                   CODE IN COMMON SUPRTX
C               = 1 OTHERWISE
C
      CHARACTER XNAME*(*)
C
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suprtx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/isuprt.f,v $
     . $',                                                             '
     .$Id: isuprt.f,v 1.2 2001/06/13 13:59:32 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER ISUPRT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'XNAME=',XNAME,
     *      ' NSUPRT=',NSUPRT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISUPRT=1
C
C  CHECK IF NO CODES IN LIST
      IF (NSUPRT.LE.0) GO TO 30
C
C  CHECK IF NON-SUPPORTED FEATURE IS IN LIST
      DO 10 I=1,NSUPRT
         IF (XNAME.EQ.SUPRT(I)) GO TO 20
10       CONTINUE
         GO TO 30
C
C  FEATURE NOT SUPPORTED
20    ISUPRT=0
C
30    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT ISUPRT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
