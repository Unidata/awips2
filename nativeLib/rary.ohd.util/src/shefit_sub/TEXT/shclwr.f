C  =====================================================================
C  pgm: SHCLWR .. Write out message on error unit(s) about files used
C
C  use:     CALL SHCLWR(IU,LUER,LUXX,NUXX,MESSG)
C
C   in: IU ......... unit number for standard error - INT
C   in: LUER ....... unit number for error output file (may be IU) - INT
C   in: LUXX ....... unit number of file connected with message - INT
C   in: NUXX ....... fixed unit number for error output file that "LUXX"
C   in:              must be assigned instead of "-1" to be active - INT
C   in: MESSG ...... output message to procede file name - CHAR*(*)
C  out: (stderr) ... write out message and filename
C  out: (file) ..... if the error file unit is not the same as the
C  out:              standard error unit, then also write message here
C  =====================================================================
      SUBROUTINE SHCLWR(IU,LUER,LUXX,NUXX,MESSG)

      INTRINSIC      LEN

      INTEGER        IU,LUER,LUXX,NUXX,IE,IEND,LEN
      CHARACTER*(*)  MESSG
      CHARACTER*128  FILNM
      LOGICAL        EXS

        IF (LUXX .EQ. NUXX) THEN
          INQUIRE(LUXX,IOSTAT=IE,NAME=FILNM,EXIST=EXS)
          IF(IE.EQ.0 .AND. EXS) THEN

            IEND = LEN(FILNM)
  100       IF (IEND .LE. 0) GOTO 110
              IF (FILNM(IEND:IEND) .NE. ' ') THEN
                WRITE(IU,'(A,A)',IOSTAT=IE) MESSG,FILNM(1:IEND)
                IF (IU .NE. LUER) THEN
                  WRITE(LUER,'(A,A)',IOSTAT=IE) MESSG,FILNM(1:IEND)
                ENDIF
                IEND = 0
              ELSE
                IEND = IEND-1
              ENDIF
              GOTO 100
  110       CONTINUE

          ENDIF
        ENDIF

      RETURN
      END
