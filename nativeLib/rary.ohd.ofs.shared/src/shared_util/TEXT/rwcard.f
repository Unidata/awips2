C MODULE RWCARD
C-----------------------------------------------------------------------
C
      SUBROUTINE RWCARD (ISTAT)
C
C  THIS ROUTINE READS AND WRITES INPUT CARDS.
C
C  ARGUMENT LIST:
C
C      NAME     TYPE   I/O   DIM   DESCRIPTION
C      ------   ----   ---   ---   ------------
C      ISTAT      I     O     1    STATUS INDICATOR:
C                                    0=NORMAL RETURN
C                                    1=READ ERROR
C
      INCLUDE 'uiox'
      INCLUDE 'ufreei'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/rwcard.f,v $
     . $',                                                             '
     .$Id: rwcard.f,v 1.2 2001/06/13 13:49:50 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
C  READ CARD
      CALL RPCARD (IBUF,IERR)
      IF (IERR.NE.0) THEN
         ISTAT=1
         GO TO 10
         ENDIF
C
C  PRINT CARD
      CALL WPCARD (IBUF)
C
C  FIND FIELDS ON CARD
      IBEG=1
      IEND=72
      CALL UFREE (IBEG,IEND)
C
10    RETURN
C
      END
