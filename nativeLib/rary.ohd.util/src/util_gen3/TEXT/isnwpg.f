C MODULE ISNWPG
C-----------------------------------------------------------------------
C
      FUNCTION ISNWPG (NUNIT)
C
C  FUNCTION TO INDICATE IF AT TOP OF NEW PAGE.
C
C  RETURN VALUE = 1 IF TOP OF NEW PAGE
C               = 0 IF NOT TOP OF NEW PAGE
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/isnwpg.f,v $
     . $',                                                             '
     .$Id: isnwpg.f,v 1.3 2001/06/13 14:03:15 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      ISNWPG=0
C
      IF (NUNIT.EQ.LP.AND.IPSNWP.EQ.1) ISNWPG=1
C
      IF (ISTRCE.GT.1.OR.LDEBUG.GT.1) THEN
         WRITE (IOSDBG,*)
     *      ' EXIT ISNWPG -',
     *      ' NUNIT=',NUNIT,
     *      ' LP=',LP,
     *      ' IPSNWP=',IPSNWP,
     *      ' ISNWPG=',ISNWPG,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
