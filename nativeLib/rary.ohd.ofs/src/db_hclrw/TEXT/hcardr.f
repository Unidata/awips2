C MODULE HCARDR
C-----------------------------------------------------------------------
C
      SUBROUTINE HCARDR (NNCARD,ISTAT)
C      
C  THIS ROUTINE READS A CARD IMAGE FROM THE HCL CARD IMAGE FILE.
C
C   ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       NNCARD     I    I/O    1    CARD NUMBER (ALSO RECORD NUMBER)
C                                   INCREMENTED IF CARD IS COMMENT
C       ISTAT      I     O     1    STATUS CODE:
C                                     0=OKAY
C                                     OTHER=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'ufstcd'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hcomnd'
      INCLUDE 'hclcommon/hprflg'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hcardr.f,v $
     . $',                                                             '
     .$Id: hcardr.f,v 1.2 1999/04/23 20:00:29 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HCARDR'
C
10    IBEG=1
      IEND=72     
C
      CALL UREADT (KHCARD,NNCARD,IBUF,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,100) NNCARD,KHCARD,ISTAT
         CALL ERROR
         GO TO 80
         ENDIF
C
C  CHECK IF TO PRINT CARD
      IF (IPRFLG.EQ.1) CALL WPCARD (IBUF)
C
C  CHECK IF COMMENT
      DO 20 I=1,IEND
         IF (IBUF(I).EQ.IDOLR) GO TO 30
20       CONTINUE
      GO TO 50
C
30    IEND=I-1
C
C  CHECK IF '$' IN COLUMN 1
      IF (IEND.NE.0) GO TO 50
      IFSTCD=1
C
40    IF (NNCARD.EQ.NCARD) GO TO 80
C
C  READ NEXT CARD
      NNCARD=NNCARD+1
      GO TO 10
C
C MAKE SURE THERE ARE NON-BLANK CHARACTERS BEFORE END OF CARD
50    DO 60 I=1,IEND
         IF (IBUF(I).NE.IBLNK) GO TO 70
60       CONTINUE
C
C  ALL BLANKS BEFORE $ OR END OF CARD - SKIP CARD
      IFSTCD=1
      GO TO 40
C
C  FIND FIELDS ON CARD
70    CALL UFREE (IBEG,IEND)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HCARDR'
C
80    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT ('0**ERROR** IN HCARDR - READING RECORD ',I5,
     *  ' FROM UNIT ',I3,'. UREADT STATUS CODE = ',I3)
C
      END
