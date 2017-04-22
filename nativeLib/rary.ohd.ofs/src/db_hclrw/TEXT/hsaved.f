C MEMBER HSAVED
C  (from old member HCLCMPST)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/07/95.14:40:21 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HSAVED (ISVRAY,IAX,ISTAT)
C
C  THIS ROUTINE WRITES OUT THE ARRAY CONTAINING THE DEFAULTS FOR
C  TECHNIQUES AND ARGUMENTS THAT PERTAIN TO THIS FUNCTION.
C
C  ISVRAY=ARRAY, IAX=NUMBER OF WORDS, ISTAT=STATUS
C
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'uio'
      INCLUDE 'udebug'
      DIMENSION ISVRAY(160)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hsaved.f,v $
     . $',                                                             '
     .$Id: hsaved.f,v 1.1 1995/09/17 18:43:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      REWIND KTMPSV
C
      N=IAX*2
C
      DO 10 J=1,N,160
         JJ=J+159
         WRITE (KTMPSV) (ISVRAY(I),I=J,JJ)
10       CONTINUE
C
      REWIND KTMPSV
C
      IF (IHCLDB.GT.0) WRITE (IOGDB,20) (ISVRAY(J),J=1,N)
20    FORMAT (' IN HSAVED - ISVRAY=',20I5 / (20X,20I5))
C
      RETURN
C
      END
