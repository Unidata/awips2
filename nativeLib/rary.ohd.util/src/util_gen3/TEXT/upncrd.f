C MODULE UPNCRD
C-----------------------------------------------------------------------
C
      SUBROUTINE UPNCRD (IPUNIT,CARD)
C
C  ROUTINE TO OUTPUT THE CARD IMAGE AND REINITIALIZES THE CARD ARRAY
C  TO ALL BLANKS.
C
      CHARACTER*80 CARD
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/upncrd.f,v $
     . $',                                                             '
     .$Id: upncrd.f,v 1.3 2002/02/11 20:46:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) WRITE (ICMPRU,*) 'ENTER UPNCRD'
C
      IF (ICMDBG.GT.0) WRITE (ICMPRU,*) 'CARD=',CARD
C
C  OUTPUT CARD
      WRITE (IPUNIT,'(A)') CARD(1:LENSTR(CARD))
      CALL ULINE (IPUNIT,1)
C
C  REINITIALIZE CARD
      CARD=' '
C
      IF (ICMTRC.GT.0) WRITE (ICMPRU,*) 'EXIT UPNCRD'
C
      RETURN
C
      END
