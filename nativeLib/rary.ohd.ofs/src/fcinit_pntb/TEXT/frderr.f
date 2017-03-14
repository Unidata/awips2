C MODULE FRDERR
C-----------------------------------------------------------------------
C
C  PRINT MESSAGE WHEN ERROR ENCOUNTERED READING CARD.
C
      SUBROUTINE FRDERR (IPR,STRING,CARD)
C
      CHARACTER*(*) STRING,CARD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/frderr.f,v $
     . $',                                                             '
     .$Id: frderr.f,v 1.2 2002/02/13 15:40:34 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (STRING.NE.' ') THEN
         IF (CARD.NE.' ') THEN
            WRITE (IPR,10) STRING,CARD
10    FORMAT ('0**ERROR** READING ',A,' VALUES FROM THE FOLLOWING CARD:'
     *   /
     *   ' ',A)
            ELSE
               WRITE (IPR,20) STRING
20    FORMAT ('0**ERROR** READING ',A,' VALUES.')
               CALL ERROR
            ENDIF
         ELSE
            WRITE (IPR,30) CARD
30    FORMAT ('0**ERROR** READING VALUES FROM THE FOLLOWING CARD:'
     *   /
     *   ' ',A)
            CALL ERROR
         ENDIF
C
      RETURN
C
      END
