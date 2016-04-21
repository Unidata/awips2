C MODULE MXX_SDER
C  =====================================================================
C  pgm: MXX_SDER .. output error messages for routine "mxx_sdir"
C
C  use:     CALL MXX_SDER(NAM,TOKEN,ISTAT)
C
C   in: NAM ........ name of dir searched for (may be full pathname
C   in:              or just the directory name under the directory
C   in:              given by the apps-defaults input token) - CHAR*(*)
C   in: TOKEN...... if "NAM" is not the found, then use "TOKEN" to get
C   in:              a default directory and append "/<NAM>" to it to
C   in:              make the desired dir , and check for its
C   in:              existence - CHAR*(*)
C   in: ISTAT ...... output pathname status: - INT
C   in:               -2 = directory found using apps-defaults token
C   in:               -1 = directory found using only the input path
C   in:                0 = dir NOT found, apps-token not used
C   in:                1 = dir NOT found, though apps-token dir found
C   in:                2 = dir NOT found, output dir string too short
C   in:                3 = dir NOT found, apps-token dir NOT found
C   in:                4 = dir NOT found, given token itself NOT found
C   in:                5 = dir NOT found, no input name given
C   in:                6 = dir NOT found, no input token given
C
C  cmt: NOTE, if ISTAT < 0, then this routine does nothing.
C  =====================================================================
      SUBROUTINE MXX_SDER(NAM,TOKEN,ISTAT)

      EXTERNAL       KKLAST

      CHARACTER*(*)  NAM,TOKEN
      CHARACTER*200  LIN
      CHARACTER*30   MSGA
      CHARACTER*20   MSGB
      CHARACTER*20   MSG0
      CHARACTER*31   MSG1
      CHARACTER*28   MSG2
      CHARACTER*31   MSG3
      CHARACTER*34   MSG4
      CHARACTER*20   MSG5
      CHARACTER*29   MSG6
      INTEGER        ISTAT,LENNAM,LENTOK,JE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_sder.f,v $
     . $',                                                             '
     .$Id: mxx_sder.f,v 1.1 2001/06/13 09:28:08 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  MSGA / 'directory not found for name: ' /
      DATA  MSGB / 'directory not found!' /
      DATA  MSG0 / 'apps-token not used.' /
      DATA  MSG1 / 'directory was found for token: ' /
      DATA  MSG2 / 'resulting pathname too long!' /
      DATA  MSG3 / 'directory NOT found for token: ' /
      DATA  MSG4 / 'no value for apps-defaults token: ' /
      DATA  MSG5 / 'no input name given!' /
      DATA  MSG6 / 'no apps-defaults token given!' /

        IF (ISTAT .GE. 0) THEN

          LENTOK = KKLAST(1,TOKEN)

          IF (LENTOK .GT. 0) THEN
            LENNAM = KKLAST(1,NAM)
            LIN = ' '
            WRITE(LIN,'(A,A)',IOSTAT=JE) MSGA,NAM(1:LENNAM)
            IF (JE.EQ.0) CALL WLIN('E',LIN)
          ELSE
            CALL WLIN('E',MSGB)
          ENDIF

          IF (ISTAT .EQ. 0) THEN
            CALL WLIN('S',MSG0)
          ELSEIF (ISTAT .EQ. 1) THEN
            LIN = ' '
            WRITE(LIN,'(A,A)',IOSTAT=JE) MSG1,TOKEN(1:LENTOK)
            IF (JE.EQ.0) CALL WLIN('S',LIN)
          ELSEIF (ISTAT .EQ. 2) THEN
            CALL WLIN('S',MSG2)
          ELSEIF (ISTAT .EQ. 3) THEN
            LIN = ' '
            WRITE(LIN,'(A,A)',IOSTAT=JE) MSG3,TOKEN(1:LENTOK)
            IF (JE.EQ.0) CALL WLIN('S',LIN)
          ELSEIF (ISTAT .EQ. 4) THEN
            LIN = ' '
            WRITE(LIN,'(A,A)',IOSTAT=JE) MSG4,TOKEN(1:LENTOK)
            IF (JE.EQ.0) CALL WLIN('S',LIN)
          ELSEIF (ISTAT .EQ. 5) THEN
            CALL WLIN('S',MSG5)
          ELSEIF (ISTAT .EQ. 6) THEN
            CALL WLIN('S',MSG6)
          ENDIF

        ENDIF

      RETURN
      END
