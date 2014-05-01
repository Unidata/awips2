C MODULE UREPET2
C-----------------------------------------------------------------------
C
      SUBROUTINE UREPET2 (STRNG1,STRNG2,NREPET)
C
C  ROUTINE UREPET2 FILLS AN ARRAY WITH A CHARACTER THE SPECIFIED
C  NUMBER OF TIMES.
C
C  INPUT ARGUMENTS
C     STRNG1 - CHARACTERS TO BE FILLED IN STRNG2
C     STRNG2 - CHARACTER ARRAY TO BE FILLED
C     NREPET - NUMBER OF TIMES STRNG1 TO BE REPEATED
C
      CHARACTER*(*) STRNG1,STRNG2(*)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/urepet2.f,v $
     . $',                                                             '
     .$Id: urepet2.f,v 1.2 2000/12/19 22:14:49 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UREPET2 - ',
     *      'STRNG1=',STRNG1,' NREPET=',NREPET
         ENDIF
C
      IF (NREPET.GT.0) THEN
         DO 10 I=1,NREPET
            STRNG2(I)=STRNG1
10          CONTINUE
         ENDIF
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UREPET'
         ENDIF
C
      RETURN
C
      END
