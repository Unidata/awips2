C MEMBER UPRCR2
C-----------------------------------------------------------------------
C
C  ROUTINE UPRCR2 PRINTS AN INPUT CARD.
C
      SUBROUTINE UPRCR2 (NSPACE,IFORM,NPUNIT,CARD,NCARD)
C
C
      CHARACTER*80 CARD
      CHARACTER*133 FMT1/'('' '',10(''>''),2X,A,A,2X,20(''<''))'/
      CHARACTER*133 FMT2/'('' '',10(''>''),2X,A,I8,2X,20(''<''))'/
      CHARACTER*133 FMT3/'('' '',10(''>''),2X,A,8X,2X,20(''<''))'/
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uprcr2.f,v $
     . $',                                                             '
     .$Id: uprcr2.f,v 1.1 1995/09/17 19:02:45 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UPRCR2 -',
     *      ' NSPACE=',NSPACE,
     *      ' IFORM=',IFORM,
     *      ' NPUNIT=',NPUNIT,
     *      ' NCARD=',NCARD
          ENDIF
C
      IPUNIT=IABS(NPUNIT)
C
C  SET NUMBER OF LINES TO BE PRINTED
      NLINES=NSPACE+1
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'NLINES=',NLINES
         ENDIF
C
C  CHECK LINES LEFT ON PAGE
      CALL ULINEL (NPUNIT,NLINES,IRETRN)
      IF (IRETRN.GT.0) CALL UPAGE (NPUNIT)
C
C  CHECK SPACING OPTION
      FMT1(3:3)=' '
      FMT2(3:3)=' '
      FMT3(3:3)=' '
      IF (NSPACE.GT.0) THEN
         IF (NSPACE.EQ.1) THEN
            FMT1(3:3)='0'
            FMT2(3:3)='0'
            FMT3(3:3)='0'
            ELSE
            DO 10 I=1,NSPACE
               CALL ULINE (NPUNIT,1)
               WRITE (IPUNIT,*) ' '
10             CONTINUE
            ENDIF
         ENDIF
C
C  CHECK FOR BLANK SEQUENCE NUMBER AND PRINT CARD
      CALL ULINE (NPUNIT,NLINES)
      IF (CARD(73:80).NE.' ') THEN
          WRITE (IPUNIT,FMT1) CARD(1:72),CARD(73:80)
          ELSE
             IF (NCARD.NE.0) THEN
                WRITE (IPUNIT,FMT2) CARD(1:72),NCARD
                ELSE
                   WRITE (IPUNIT,FMT3) CARD(1:72)
                ENDIF
          ENDIF
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT  UPRCR2'
         ENDIF
C
      RETURN
C
      END
