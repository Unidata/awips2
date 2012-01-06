C MEMBER UCKCMT
C-----------------------------------------------------------------------
C
      SUBROUTINE UCKCMT (STRING,CHECK,IRETRN)
C
C  ROUTINE UCKCMT CHECKS A CARACACTER STRING TO SEE IF IT IS A COMMENT.
C
C  INPUT VARIABLES -
C     STRING - CHARACTER STRING
C     CHECK  - CHARACTERS TO BE USED TO CHECK IF STRING IS A COMMENT
C
C  OUTPUT VARIABLES -
C     IRETRN - RETURN CODE
C                0=STRING IS NOT A COMMENT
C               -1=BLANK STRING
C               >0=STRING IS A COMMENT
C                  IRETRN IS SET TO COLUMN WHERE COMMENT STARTS
C
C
      CHARACTER*(*) STRING,CHECK
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uckcmt.f,v $
     . $',                                                             '
     .$Id: uckcmt.f,v 1.1 1995/09/17 19:02:24 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,10)
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'STRING=',STRING,' CHECK=',CHECK
         ENDIF
C
      IRETRN=0
C
C  CHECK IF STRING IS ALL BLANKS
      IF (STRING.EQ.' ') THEN
         IRETRN=-1
         GO TO 40
         ENDIF
C
C  FIND FIRST NON-BLANK CHARACTER
      CALL UBEGIN (STRING,LEN(STRING),LBEGIN)
C
C  CHECK FOR COMMENT CHARACTER
      DO 30 IPOS=1,LEN(CHECK)
         IF (STRING(LBEGIN:LBEGIN).EQ.CHECK(IPOS:IPOS)) THEN
            IRETRN=LBEGIN
            GO TO 40
            ENDIF
30       CONTINUE
C
40    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,50) IRETRN
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER UCKCMT')
50    FORMAT (' *** EXIT UCKCMT - IRETRN=',I2)
C
      END
