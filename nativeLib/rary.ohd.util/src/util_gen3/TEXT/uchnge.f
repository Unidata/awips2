C MEMBER UCHNGE
C-----------------------------------------------------------------------
C
      SUBROUTINE UCHNGE (STRING,CHROLD,CHRNEW,LENGTH)
C
C  ROUTINE UCHNGE CHANGES ONE CHARACTER TO ANOTHER IN A CHARACTER
C  STRING.
C
C  INPUT ARGUMENTS
C     STRING - CHARACTER STRING TO BE FILLED
C     CHROLD - OLD CHARACTER TO BE CHANGED
C     CHRNEW - NEW CHARACTER TO BE USED
C     LENGTH - NUMBER OF CHARACTERS IN CHARACTER STRING TO BE PROCESSED
C
      CHARACTER*1 STRING(1),CHROLD,CHRNEW
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/uchnge.f,v $
     . $',                                                             '
     .$Id: uchnge.f,v 1.1 1995/09/17 19:03:15 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,50) CHROLD,CHRNEW,LENGTH
         ENDIF
C
      IF (LENGTH.LE.0) GO TO 20
C
      DO 10 I=1,LENGTH
         IF (STRING(I).EQ.CHROLD) STRING(I)=CHRNEW
10       CONTINUE
C
20    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,60)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER UCHNGE : CHROLD=',A1,3X,'CHRNEW=',A1,3X,
     *   'LENGTH=',I4)
60    FORMAT (' *** EXIT UCHNGE')
C
      END
