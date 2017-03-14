C MODULE RDVCIN
C-----------------------------------------------------------------------
C  THIS ROUTINE READS INTEGER VALUES FROM INPUT DATA CARDS
C
      SUBROUTINE RDVCIN (NDP,ITRAN,IBUG,IER,LCD7)

      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'

      INTRINSIC    CHAR
      CHARACTER    CHAR

      PARAMETER   (NB=72)

      INTEGER      ITRAN(*)
      CHARACTER*1  BB(NB),CC(10)
      CHARACTER*1  TAB,BLANK,X,XNEG

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/rdvcin.f,v $
     . $',                                                             '
     .$Id: rdvcin.f,v 1.3 2000/12/19 15:03:03 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA   BLANK,X,XNEG / ' ','X','-' /
      DATA   CC / '1','2','3','4','5','6','7','8','9','0' /


      IF (IBUG.GT.0) WRITE (IODBUG,*) 'ENTER RDVCIN'

C  Set TAB to an ascii tab
 
      TAB = CHAR(9)

      IER=0

C  READ CARD
      READ (IN,10) BB
10    FORMAT (72A1)
      LCD7=LCD7+1

      ITEMP=0
      ISW=0
      NDP=0
      ISGN=1

C  DECODE THE CARD
30    DO 100 I=1,NB

      IF (IBUG.GT.0) WRITE (IODBUG,'('' CARD='',72A1)') BB

      IF (BB(I).EQ.BLANK .OR. BB(I).EQ.TAB) THEN

         IF (ISW .NE. 0) THEN
           NDP=NDP+1
           ITRAN(NDP)=ITEMP*ISGN
           ISGN=1
           ITEMP=0
           ISW=0
         ENDIF

      ELSE

C  Is there a continuation card?
         IF (BB(I) .EQ. X) THEN
           IF (ISW .NE. 0) THEN
             NDP=NDP+1
             ITRAN(NDP)=ITEMP*ISGN
             ISGN=1
             ITEMP=0
             ISW=0
           ENDIF

C  Read continuation card
           READ(IN,10) BB
           LCD7=LCD7+1
           GO TO 30
         ENDIF

         IF (BB(I) .EQ. XNEG) THEN
           ISGN=-1
           ISW=1
           GO TO 100
         ELSE
           DO 50 J=1,10
             IF (BB(I) .EQ. CC(J)) THEN
               JJ = J
               IF (JJ.EQ.10) JJ=0
               ITEMP=ITEMP*10+JJ
               ISW=1
               GO TO 100
             ENDIF
50         CONTINUE
           WRITE (IPR,60) BB(I),I,BB
60         FORMAT ('0**ERROR** INVALID CHARACTER (',A1,
     *      ') FOUND AT COLUMN ',I2,' ON THE FOLLOWING CARD:' /
     *      ' ',72A1)
           CALL ERROR
           IER=1
           GO TO 120
         ENDIF

      ENDIF

100   CONTINUE

      IF (ISW .NE. 0) THEN
        NDP=NDP+1
        ITRAN(NDP)=ITEMP*ISGN
      ENDIF

120   RETURN
      END
