C MODULE UCP2LC
C-----------------------------------------------------------------------
C 
C  ROUTINE UCP2LC CHANGES UPPER CASE CHARACTERS TO LOWER CASE
C  CHARACTERS.
C 
      SUBROUTINE UCP2LC (STROLD,STRNEW,ISTAT)
C 
      CHARACTER*(*) STROLD,STRNEW
      CHARACTER*1 XLC(26)/
     *   'a',
     *   'b',
     *   'c',
     *   'd',
     *   'e',
     *   'f',
     *   'g',
     *   'h',
     *   'i',
     *   'j',
     *   'k',
     *   'l',
     *   'm',
     *   'n',
     *   'o',
     *   'p',
     *   'q',
     *   'r',
     *   's',
     *   't',
     *   'u',
     *   'v',
     *   'w',
     *   'x',
     *   'y',
     *   'z'
     *   /
      CHARACTER*1 XCP(26)/
     *   'A',
     *   'B',
     *   'C',
     *   'D',
     *   'E',
     *   'F',
     *   'G',
     *   'H',
     *   'I',
     *   'J',
     *   'K',
     *   'L',
     *   'M',
     *   'N',
     *   'O',
     *   'P',
     *   'Q',
     *   'R',
     *   'S',
     *   'T',
     *   'U',
     *   'V',
     *   'W',
     *   'X',
     *   'Y',
     *   'Z'
     *   /
C 
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/ucp2lc.f,v $
     . $',                                                             '
     .$Id: ucp2lc.f,v 1.2 2001/06/14 18:31:23 dws Exp $
     . $' /
C    ===================================================================
C
C 
C 
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UCP2LC'
         ENDIF
C 
      ISTAT=0
C 
      STRNEW=STROLD
C 
C  CONVERT CHARACTERS
      DO 20 I=1,LEN(STROLD)
         DO 10 N=1,26
            IF (STRNEW(I:I).EQ.XCP(N)) THEN
               STRNEW(I:I)=XLC(N)
               ENDIF
10          CONTINUE
20       CONTINUE
C 
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UCP2LC'
         ENDIF
C 
      RETURN
C 
      END
