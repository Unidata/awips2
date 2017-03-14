C MODULE SULINE
C-----------------------------------------------------------------------
C
      SUBROUTINE SULINE (NUNIT,LINES)
C
C  ROUTINE TO COUNT LINES PRINTED.
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/suline.f,v $
     . $',                                                             '
     .$Id: suline.f,v 1.4 2001/06/13 10:47:08 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.5) THEN
         WRITE (IOSDBG,60) NUNIT,NPSNLN,LINES,NPSMLN,LP
         IF (IOSDBG.EQ.LP) THEN
            NPSNLN=NPSNLN+1
            NPSNLT=NPSNLT+1
            NPSNLR=NPSNLR+1
            ENDIF
         ENDIF
C
C  CHECK IF SPECIFIED UNIT IS STANDARD PRINT UNIT
      IF (NUNIT.EQ.LP) THEN
         NPSNLN=NPSNLN+LINES
         NPSNLT=NPSNLT+LINES
         NPSNLR=NPSNLR+LINES
         IPSNWP=0
         ENDIF
C
C  CHECK IF SPECIFIED NUMBER OF LINES WILL FIT ON PAGE
      IF (NPSNLN+LINES.GT.NPSMLN) CALL SUPAGE
C
      IF (ISTRCE.GT.5) THEN
         WRITE (IOSDBG,70)
         IF (IOSDBG.EQ.LP) THEN
            NPSNLN=NPSNLN+1
            NPSNLT=NPSNLT+1
            NPSNLR=NPSNLR+1
            ENDIF
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' ENTER SULINE : NUNIT=',I3,3X,'NPSNLN=',I3,3X,
     *     'LINES=',I3,3X,'NPSMLN=',I3,3X,'LP=',I3)
70    FORMAT (' EXIT SULINE')
C
      END
