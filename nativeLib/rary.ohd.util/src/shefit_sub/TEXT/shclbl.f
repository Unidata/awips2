C  =====================================================================
C  pgm: SHCLBL .. Write out a blank line on error unit(s)
C
C  use:     CALL SHCLBL(IU,LUER)
C
C   in: IU ......... unit number for standard error - INT
C   in: LUER ....... unit number for error output file (may be IU) - INT
C  =====================================================================
      SUBROUTINE SHCLBL(IU,LUER)

      INTEGER        IU,LUER,IE

        WRITE(IU,'(1X)',IOSTAT=IE)
        IF (IU .NE. LUER) WRITE(LUER,'(1X)',IOSTAT=IE)

      RETURN
      END
