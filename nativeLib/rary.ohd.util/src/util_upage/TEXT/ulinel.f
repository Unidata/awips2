C MODULE ULINEL
C-----------------------------------------------------------------------
C
      SUBROUTINE ULINEL (NPUNIT,LINES,IRETRN)
C
C  ROUTINE TO DETERMINE IF SPECIFIED LINES WILL FIT ON PAGE.
C
C  INPUT ARGUMETS:
C     NPUNIT - PRINT UNIT NUMBER
C     LINES  - NUMBER OF LINES TO BE PRINTED
C
C  OUTPUT ARGUMENTS:
C     IRETRN - RETURN CODE:
C               0=LINES WILL FIT ON PAGE
C               1=LINES WILL NOT FIT ON PAGE
C
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/ulinel.f,v $
     . $',                                                             '
     .$Id: ulinel.f,v 1.2 2000/07/20 07:40:55 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER ULINEL -',
     *      ' NPUNIT=',NPUNIT,
     *      ' LINES=',LINES
         ENDIF
C
      IRETRN=0
C
      IPUNIT=IABS(NPUNIT)
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'IPUNIT=',IPUNIT,
     *      ' IPSPAG(IPUNIT)=',IPSPAG(IPUNIT),
     *      ' IPSLIN(IPUNIT)=',IPSLIN(IPUNIT),
     *      ' NPSNLN(IPUNIT)=',NPSNLN(IPUNIT),
     *      ' NPSMLN(IPUNIT)=',NPSMLN(IPUNIT)
         ENDIF
C
      IF (NPSNLN(IPUNIT)+LINES.GT.NPSMLN(IPUNIT)) IRETRN=1
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT ULINEL -',
     *      ' IRETRN=',IRETRN
         ENDIF
C
      RETURN
C
      END
