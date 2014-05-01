C MODULE UPSTAT
C  =====================================================================
C  pgm: UPSTAT .. check status of a given filename or unit number
C
C  use:     CALL UPSTAT(IUNIT,FILNM,STATUS,IC)
C
C   in: IUNIT ...... file unit number to be checked, else unused - INT
C   in: FILNM ...... file pathname to be checked, else unused - CHAR*(*)
C   in: STATUS ..... desired status of given file - CHAR*(*)
C   in:                (blank status skips thru this routine)
C  out: IC ......... status of file: - INT
C  out:                pos = error during INQUIRE statement
C  out:                  0 = file status good (if NEW then not found,
C  out:                                        if OLD then found file,
C  out:                                        if U then don't care,
C  out:                                        if S then not found.
C  out:                neg = error or unexpected file or no old file
C  out:                       -2 = file found for 'SCRATCH' status
C  out:                       -3 = file not found for 'OLD' status
C  out:                       -4 = file found for 'NEW' status
C  out:                       -5 = filename or unit num missing
C  out:                       -6 = bad status code given
C   in: (common) ... block common UPDAIO contains unit numbers for
C   in:              i/o routine messages (see subrtn UPRIMO)
C
C  rqd: common:  UPDAIO
C  =====================================================================
      SUBROUTINE UPSTAT(IUNIT,FILNM,STATUS,IC)


      INTEGER        IUNIT,IC,IERR,LL
      LOGICAL        EXIS
      CHARACTER*(*)  FILNM,STATUS
      CHARACTER*128  LN
      CHARACTER*4    KH
      CHARACTER*1    KS

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upstat.f,v $
     . $',                                                             '
     .$Id: upstat.f,v 1.2 2001/06/12 19:39:08 dws Exp $
     . $' /
C    ===================================================================
C


        IC = 0
        KS = STATUS(1:1)

        IF (KS .EQ. ' ') THEN

          CONTINUE

        ELSEIF (KS.NE.'N' .AND. KS.NE.'O' .AND.
     $          KS.NE.'U' .AND. KS.NE.'S'       ) THEN

          IC = -6
          IF (UE.GE.0 .AND. IC.EQ.-6) WRITE(UE,107)

        ELSEIF (IUNIT.LE.0 .AND. FILNM.EQ.' ') THEN

          IF (KS .NE. 'S') IC = -5
          IF (UE.GE.0 .AND. IC.EQ.-5) WRITE(UE,101) KS

        ELSE

          IF (FILNM .NE. ' ') THEN
            LN = FILNM
            KH = ' '
            INQUIRE(FILE=LN,IOSTAT=IERR,EXIST=EXIS)
          ELSE
            LN = ' '
            WRITE(KH,'(I4)') IUNIT
            INQUIRE(UNIT=IUNIT,IOSTAT=IERR,EXIST=EXIS,NAME=LN)
            IF (LN .EQ. ' ') EXIS = .FALSE.
          ENDIF

          IF (IERR .EQ. 0) THEN
            IF (KS.EQ.'N' .AND.      EXIS) IC = -4
            IF (KS.EQ.'O' .AND. .NOT.EXIS) IC = -3
            IF (KS.EQ.'S' .AND.      EXIS) IC = -2
          ELSE
            IC = IERR
          ENDIF

          IF( UU.GE.0 .OR. UE.GE.0 ) LL = LENSTR(LN)

          IF (UU.GE.0 .AND. IC.EQ. 0) WRITE(UU,102) KS,KH,LN(1:LL)
          IF (UE.GE.0 .AND. IC.EQ.-4) WRITE(UE,103) KS,KH,LN(1:LL)
          IF (UE.GE.0 .AND. IC.EQ.-3) WRITE(UE,104) KS,KH,LN(1:LL)
          IF (UE.GE.0 .AND. IC.EQ.-2) WRITE(UE,105) KS,KH,LN(1:LL)
          IF (UE.GE.0 .AND. IC.GT. 0) WRITE(UE,106) KS,KH,IC,LN(1:LL)

        ENDIF

  101   FORMAT(' un-s-',A1,4X,'  ** ERROR unknown file')
  102   FORMAT(' un-s-',A1,A4,'     status checks     ',A)
  103   FORMAT(' un-s-',A1,A4,'  ** ERROR new exists  ',A)
  104   FORMAT(' un-s-',A1,A4,'  ** ERROR old not fnd ',A)
  105   FORMAT(' un-s-',A1,A4,'  ** ERROR scratch exs ',A)
  106   FORMAT(' un-s-',A1,A4,'  ** ERROR is',I5,'     ',A)
  107   FORMAT(' un-s- ',  4X,'  ** ERROR bad code "',A1,'"')

      RETURN

      END
