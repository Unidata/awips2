C MODULE MXX_RHE2
C  =====================================================================
C  pgm: MXX_RHE2 .. Output warnings for routine rdm_bnd
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXX_RHE2(FLNAM,NAREA,NUSED,MAXBSN,BASINS,ISTAT,
     $                    ISTNUM,ISTBSN)

      EXTERNAL        WLIN

      CHARACTER*200   LIN
      CHARACTER*(*)   FLNAM
      CHARACTER*8     BASINS(*)
      CHARACTER*3     FM1
      CHARACTER*5     FM2
      CHARACTER*10    FM5
      CHARACTER*49    MSG7
      CHARACTER*14    MSG7X
      CHARACTER*15    MSG7A
      CHARACTER*31    MSG7B
      CHARACTER*35    MSG7C
      INTEGER         NAREA,NUSED,MAXBSN,ISTAT,JE,II
      INTEGER         ISTNUM
      CHARACTER*8     ISTBSN(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_rhe2.f,v $
     . $',                                                             '
     .$Id: mxx_rhe2.f,v 1.3 2005/06/09 19:27:52 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  FM1  / '(A)' /
      DATA  FM2  / '(A,A)' /
      DATA  FM5  / '(I12,A,I5)' /
      DATA MSG7  / 'there were more basins defined than were found in' /
      DATA MSG7X / 'segment file: ' /
      DATA MSG7A / '  basins out of' /
      DATA MSG7B / 'these basins will be processed:' /
      DATA MSG7C / 'these basins will NOT be processed:' /

        IF (ISTAT .LT. 0) THEN

          CALL WLIN('B',' ')

          IF (ISTAT .EQ. -1) THEN
            CALL WLIN('W',MSG7)
            LIN = ' '
            WRITE(LIN,FM2,IOSTAT=JE) MSG7X,FLNAM
            IF (JE.EQ.0) CALL WLIN('S',LIN)
            LIN = ' '
            WRITE(LIN,FM5,IOSTAT=JE) NUSED,MSG7A,NAREA
            IF (JE.EQ.0) CALL WLIN('S',LIN)

            IF (NUSED.GT.0 .AND. NUSED.LE.MAXBSN) THEN
              LIN = ' '
              WRITE(LIN,FM1,IOSTAT=JE) MSG7B
              IF (JE.EQ.0) CALL WLIN('S',LIN)

              DO 20 II=1,NUSED
                LIN = ' '
                WRITE(LIN,'(''    '',A)',IOSTAT=JE) BASINS(II)
                IF (JE.EQ.0) CALL WLIN('S',LIN)
   20         CONTINUE
            ENDIF

            IF (NAREA.GT.NUSED .AND. NAREA.LE.MAXBSN .AND.
     $      ISTNUM.GT.0) THEN
              LIN = ' '
              WRITE(LIN,FM1,IOSTAT=JE) MSG7C
              IF (JE.EQ.0) CALL WLIN('S',LIN)

              DO 30 II=1,ISTNUM
                LIN = ' '
                WRITE(LIN,'(''    '',A)',IOSTAT=JE) ISTBSN(II)
                IF (JE.EQ.0) CALL WLIN('S',LIN)
   30         CONTINUE
            ENDIF
          ENDIF

          CALL WLIN('B',' ')

        ENDIF

      RETURN
      END
