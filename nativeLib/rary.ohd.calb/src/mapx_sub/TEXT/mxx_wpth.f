C  =====================================================================
C  pgm: MXX_WPTH .. Output the mapx or related output file pathname
C  =====================================================================
      SUBROUTINE MXX_WPTH(TYP,CT,FILNM)

      INTRINSIC       LEN
      INTEGER         LEN

      EXTERNAL        KKLAST,WLIN,KKCAPS
      INTEGER         KKLAST

      CHARACTER*(*)   TYP,CT,FILNM
      CHARACTER*200   LIN
      CHARACTER*13    MSGOUT
      CHARACTER*21    MSGFUL
      INTEGER         JE,LNTYP,LNCT,LNMSG,LNFUL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_wpth.f,v $
     . $',                                                             '
     .$Id: mxx_wpth.f,v 1.1 2001/06/13 09:29:03 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  MSGOUT / ' OUTPUT FILE:' /

        LNTYP = KKLAST(1,TYP)
        LNCT  = KKLAST(1,CT)
        LNMSG = KKLAST(1,MSGOUT)
        LNFUL = LEN(MSGFUL)

        MSGFUL = ' '
        IF (LNTYP+LNCT+LNMSG+1 .LT. LNFUL) THEN
          MSGFUL = ' ' // TYP(1:LNTYP) // CT(1:LNCT) // MSGOUT
        ELSE
          MSGFUL = ' ' // MSGOUT
        ENDIF

        LIN = ' '
        CALL KKCAPS(MSGFUL)
        WRITE(LIN,'(A,A)',IOSTAT=JE) MSGFUL,FILNM
        IF (JE .EQ. 0) THEN
          CALL WLIN('B',' ')
          CALL WLIN('M',LIN)
          CALL WLIN('B',' ')
        ENDIF

      RETURN
      END
