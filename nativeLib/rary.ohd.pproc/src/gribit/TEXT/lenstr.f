C MODULE LENSTR
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION LENSTR (STRNG)
C
C  ROUTINE TO GET NUMBER OF CHARACTERS IN STRING
C
      CHARACTER*(*) STRNG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/lenstr.f,v $
     . $',                                                             '
     .$Id: lenstr.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
     . $' /
C    ===================================================================
C
C
      LENSTR=0
C
      DO 10 IPOS=LEN(STRNG),1,-1
         IF (STRNG(IPOS:IPOS).EQ.' ') GO TO 10
         IF (STRNG(IPOS:IPOS).EQ.CHAR(0)) GO TO 10
         GO TO 20
10       CONTINUE
      GO TO 30
C
20    LENSTR=IPOS
C
30    RETURN
C
      END
