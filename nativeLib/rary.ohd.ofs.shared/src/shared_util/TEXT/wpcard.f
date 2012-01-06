C MODLUE WPCARD
C-----------------------------------------------------------------------
C
      SUBROUTINE WPCARD (IBUF)
C
C  ROUTINE TO WRITE CARD IMAGE.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       ------   ----   ---   ---   -----------
C       IBUF       I     1     80   ARRAY CONTAINING CARD IMAGE
C
      INCLUDE 'uiox'
      INCLUDE 'ufstcd'
C
      INTEGER IBUF(80)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/wpcard.f,v $
     . $',                                                             '
     .$Id: wpcard.f,v 1.2 2001/06/13 13:40:39 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IFSTCD.EQ.1) THEN
         WRITE (LP,10) IBUF
10    FORMAT (' >>>>>>>> ',80A1)
         IFSTCD=0
         ELSE
            WRITE (LP,20) IBUF
20    FORMAT (10X,80A1)
         ENDIF
C
      RETURN
C
      END
