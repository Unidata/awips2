C MODULE UPSTAE
C  =====================================================================
C  pgm: UPSTAE .. output error messages from subroutine "upstat"
C
C  use:     CALL UPSTAE(JU,IUNIT,FILNM,STATUS,IC)
C
C   in: JU ......... unit number for output of error messages - INT
C   in: IUNIT ...... file unit number to be checked, else unused - INT
C   in: FILNM ...... file pathname to be checked, else unused - CHAR*(*)
C   in: STATUS ..... desired status of given file - CHAR*(*)
C   in: IC ......... status of file: - INT
C   in:                pos = error during INQUIRE statement
C   in:                  0 = file status good (if NEW then not found,
C   in:                                        if OLD then found file,
C   in:                                        if U then don't care,
C   in:                                        if S then not found.
C   in:                neg = error or unexpected file or no old file
C   in:                       -2 = file found for 'SCRATCH' status
C   in:                       -3 = no file found for 'OLD' status
C   in:                       -4 = file found for 'NEW' status
C   in:                       -5 = filename or unit num missing
C   in:                       -6 = bad status code given
C  out: (unit JU) .. output error messages (if any) are sent to unit JU
C  =====================================================================
      SUBROUTINE UPSTAE(JU,IUNIT,FILNM,STATUS,IC)


      INTEGER        JU,IUNIT,IC,LL1,LL2,LL3,LL4,IERR
      CHARACTER*(*)  FILNM,STATUS
      CHARACTER*128  LN
      CHARACTER*8    STA
      CHARACTER*3    KH
      CHARACTER*148  LN2
      CHARACTER*20   KH2

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upstae.f,v $
     . $',                                                             '
     .$Id: upstae.f,v 1.2 2001/06/12 15:18:52 aivo Exp $
     . $' /
C    ===================================================================
C


        IF (IC.NE.0 .AND. JU.GT.0 .AND. JU.LE.99) THEN

          IF (FILNM .NE. ' ') THEN
            KH = ' '
            LN = FILNM
          ELSE
            KH = ' '
            IF (IUNIT.GE.0.AND.IUNIT.LE.MUPRECL) WRITE(KH,'(I3)') IUNIT
            LN = ' '
            IF (IC.EQ.-2 .OR. IC.EQ.-4) THEN
              INQUIRE(UNIT=IUNIT,IOSTAT=IERR,NAME=LN)
            ENDIF
          ENDIF

          LL1 =LENSTR(LN)

          KH2 = ' '
          IF (KH .NE. ' ') KH2 = ' for unit "' // KH // '" '
          LN2 = ' '
          IF (LN .NE. ' ') LN2 = 'for filename "' // LN(1:LL1) // '"'

              LL2 = 21
   20         IF (LL2 .LE. 1) GOTO 22
                LL2 = LL2-1
                IF (KH2(LL2:LL2) .EQ. ' ') GOTO 20
   22         CONTINUE
              IF (LL2.GT.1 .AND. LL2.LT.20) LL2 = LL2+1

              LL3 = 149
   30         IF (LL3 .LE. 1) GOTO 32
                LL3 = LL3-1
                IF (LN2(LL3:LL3) .EQ. ' ') GOTO 30
   32         CONTINUE

            IF (IC .EQ. -6) THEN
              STA = STATUS
              LL4 = 9
   40         IF (LL4 .LE. 1) GOTO 42
                LL4 = LL4-1
                IF (STA(LL4:LL4) .EQ. ' ') GOTO 40
   42         CONTINUE
            ENDIF

          IF (IC .EQ. -6) WRITE(JU,107) STA(1:LL4),KH2(1:LL2),LN2(1:LL3)
          IF (IC .EQ. -5) WRITE(JU,108)
          IF (IC .EQ. -4) WRITE(JU,103) KH2(1:LL2),LN2(1:LL3)
          IF (IC .EQ. -3) WRITE(JU,104) KH2(1:LL2),LN2(1:LL3)
          IF (IC .EQ. -2) WRITE(JU,105) KH2(1:LL2),LN2(1:LL3)
          IF (IC .GT.  0) WRITE(JU,106) IC,KH2(1:LL2),LN2(1:LL3)

        ENDIF

  103   FORMAT(' **ERROR** (upstat) New file already exists',A,A)
  104   FORMAT(' **ERROR** (upstat) Cannot find existing file',A,A)
  105   FORMAT(' **ERROR** (upstat) Scratch file already exists',A,A)
  106   FORMAT(' **ERROR** (upstat) Bad file access "',I6,'"',A,A)
  107   FORMAT(' **ERROR** (upstat) Bad status "',A,'" given',A,A)
  108   FORMAT(' **ERROR** (upstat) No filename or unit number given')


      RETURN
      END
