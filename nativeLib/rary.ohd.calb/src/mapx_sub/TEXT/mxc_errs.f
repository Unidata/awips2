C MODULE MXC_ERRS
C  =====================================================================
C  pgm: MXC_ERRS .. Output errors from reading the input mapx file
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXC_ERRS(ISTAT)

      EXTERNAL        WLIN

      CHARACTER*200   LIN
      CHARACTER*3     FM3
      CHARACTER*46    MSG1
      CHARACTER*44    MSG2
      CHARACTER*48    MSG3
      CHARACTER*33    MSG4
      INTEGER         ISTAT,JE
      INTEGER         II1,II2,II4,II8,II16,II32
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxc_errs.f,v $
     . $',                                                             '
     .$Id: mxc_errs.f,v 1.2 2005/06/09 19:27:39 dws Exp $
     . $' /
C    ===================================================================
C

      DATA FM3  / '(A)' /
      DATA MSG1 / 'unit number for the input control file is bad!' /
      DATA MSG2 / 'read error while reading input control file!' /
      DATA MSG3 / 'encountered basin name longer than 8 characters!' /
      DATA MSG4 / 'encountered too many basin names!' /

        IF (ISTAT .NE. 0) THEN

          II1  = ISTAT
          II32 = II1/32
          II1  = II1 - 32*II32
          II16 = II1/16
          II1  = II1 - 16*II16
          II8  = II1/8
          II1  = II1 - 8*II8
          II4  = II1/4
          II1  = II1 - 4*II4
          II2  = II1/2
          II1  = II1 - 2*II2

          CALL WLIN('B',' ')

          IF (II1 .GT. 0) THEN
            LIN = ' '
            WRITE(LIN,FM3,IOSTAT=JE) MSG1
            IF(JE.EQ.0) CALL WLIN('E',LIN)
          ENDIF

          IF (II2 .GT. 0) THEN
            LIN = ' '
            WRITE(LIN,FM3,IOSTAT=JE) MSG2
            IF(JE.EQ.0) CALL WLIN('E',LIN)
          ENDIF

          IF (II4 .GT. 0) THEN
            LIN = ' '
            WRITE(LIN,FM3,IOSTAT=JE) MSG3
            IF(JE.EQ.0) CALL WLIN('W',LIN)
          ENDIF

          IF (II8 .GT. 0) THEN
            LIN = ' '
            WRITE(LIN,FM3,IOSTAT=JE) MSG4
            IF(JE.EQ.0) CALL WLIN('W',LIN)
          ENDIF

          CALL WLIN('B',' ')

        ENDIF

      RETURN
      END
