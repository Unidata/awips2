C MEMBER USETPS
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C  ROUTINE TO SET PAGESIZE OPTION FOR UTILITY ROUTINES.
C
      SUBROUTINE USETPS (NPAGSZ,MINVAL,MAXVAL,ISTAT)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/usetps.f,v $
     . $',                                                             '
     .$Id: usetps.f,v 1.1 1995/09/17 19:04:08 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER USETPS -',
     *      ' NPAGSZ=',NPAGSZ,
     *      ' MINVAL=',MINVAL,
     *      ' MAXVAL=',MAXVAL
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR VALID VALUE
      IF (MINVAL.GT.0.AND.MAXVAL.GT.0) THEN
         IF (NPAGSZ.LT.MINVAL.OR.NPAGSZ.GT.MAXVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,30) NPAGSZ,MINVAL,MAXVAL
            ISTAT=1
            GO TO 10
            ENDIF
         ENDIF
C
      DO 20 I=1,99
         NPSMLN(I)=NPAGSZ
20       CONTINUE
C
10    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT  USETPS -',
     *      ' ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT ('+*** ERROR - IN USETPS - PAGESIZE ',I3,
     *   ' IS LESS THAN ',I3,' OR GREATER THAN ',I3,'.')
C
      END
