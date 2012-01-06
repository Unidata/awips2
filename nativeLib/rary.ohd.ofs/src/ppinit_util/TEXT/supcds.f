C MODULE SUPCDS
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT INPUT CARDS.
C
      SUBROUTINE SUPCDS
C
      CHARACTER*80 CARD
C
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/supcds.f,v $
     . $',                                                             '
     .$Id: supcds.f,v 1.3 1999/01/20 14:23:38 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  REWIND TEMPORARY FILE CONTAINING INPUT CARDS
      REWIND ICD
C
C  PRINT HEADING
      IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,90)
      CALL SULINE (LP,2)
      WRITE (LP,100)
      CALL SULINE (LP,0)
      WRITE (LP,100)
      CALL SULINE (LP,0)
      WRITE (LP,100)
      CALL SULINE (LP,0)
      WRITE (LP,90)
      CALL SULINE (LP,2)
      WRITE (LP,110) (I,I=5,80,5)
      CALL SULINE (LP,1)
      WRITE (LP,120)
      CALL SULINE (LP,1)
C
C  PRINT CARD IMAGES
      NLINE=0
10    READ (ICD,70,END=30) CARD
      IF (ISNWPG(LP).EQ.1) THEN
         WRITE (LP,110) (I,I=5,80,5)
         CALL SULINE (LP,1)
         WRITE (LP,120)
         CALL SULINE (LP,1)
         ENDIF
      NLINE=NLINE+1
      IF (NLINE.EQ.NRDCRD) THEN
         WRITE (LP,130) NLINE,CARD,' '
         CALL SULINE (LP,1)
         ELSE
            WRITE (LP,130) NLINE,CARD(1:LENSTR(CARD))
            CALL SULINE (LP,1)
         ENDIF
      GO TO 10
C
C  RESET INPUT FILE TO NEXT CARD TO BE READ
30    REWIND ICD
      DO 40 I=1,NRDCRD
         READ (ICD,80,END=50)
40       CONTINUE
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,150)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SUPCDS')
70    FORMAT (A)
80    FORMAT (' ')
90    FORMAT ('0')
100   FORMAT ('+',4('***   LISTING OF INPUT CARDS   '),'***')
110   FORMAT (T17,16I5)
120   FORMAT (T17,16('----+'))
130   FORMAT (5X,'(',I6,')',3X,A :
     *   A,2X,5('<'),' CURRENT COMMAND ',5('<'))
150   FORMAT (' *** EXIT SUPCDS')
C
      END
