C MEMBER ULINE
C-----------------------------------------------------------------------
C
      SUBROUTINE ULINE (NUNIT,LINES)
C
C  ROUTINE TO COUNT LINES PRINTED TO THE SPECIFIED UNIT.
C  IF THE SPECIFIED LINES WILL NOT FIT ON THE PAGE, A NEW PAGE
C  IS STARTED.
C
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/uline.f,v $
     . $',                                                             '
     .$Id: uline.f,v 1.1 1995/09/17 19:05:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IUNIT=IABS(NUNIT)
C
C  CHECK IF PAGE HEADER HAS BEEN PRINTED
      IF (ICMDBG.GT.2)
     *   WRITE (ICMPRU,20) IUNIT,NPSPAG(IUNIT)
C
C  CHECK IF PAGE HEADER HAS BEEN PRINTED
      IF (NPSPAG(IUNIT).EQ.0) CALL UPAGE (NUNIT)
C
C  CHECK IF LINES TO BE COUNTED
      IF (IPSLIN(IUNIT).EQ.0) GO TO 10
C
C  CHECK IF SPECIFIED LINES WILL FIT ON PAGE
      IF (ICMDBG.GT.2)
     *   WRITE (ICMPRU,30) IUNIT,NPSNLN(IUNIT),LINES,NPSMLN(IUNIT)
      IF (NPSMLN(IUNIT).GT.0.AND.
     *    (NPSNLN(IUNIT)+LINES.GT.NPSMLN(IUNIT))) CALL UPAGE (NUNIT)
C
C  UPDATE LINE COUNTERS
      NPSNLN(IUNIT)=NPSNLN(IUNIT)+LINES
      NPSNLT(IUNIT)=NPSNLT(IUNIT)+LINES
C
C  RESET TOP OF PAGE INDICATOR
      IPSNWP(NUNIT)=0
C
10    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' IN ULINE - IUNIT=',I2,3X,'NPSPAG(IUNIT)=',I2)
30    FORMAT (' IN ULINE - IUNIT=',I2,3X,'NPSNLN(IUNIT)=',I2,3X,
     *   'LINES=',I2,3X,'NPSMLN(IUNIT)=',I2)
C
      END
