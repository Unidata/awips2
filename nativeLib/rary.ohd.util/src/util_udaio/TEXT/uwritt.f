C MODULE UWRITT
C  =====================================================================
C  pgm: UWRITT .. Write one record to a random access file for a given
C  pgm:           unit number that corresponds to an NWSRFS OFS file
C
C  use:     CALL UWRITT(IUNIT,IREC,IARR,ISTAT)
C
C   in: IUNIT .... NWSRFS OFS unit number for random access file - INT
C   in: IREC ..... record number to be written - INT
C  out: IARR ..... array of 4-byte integers for one record - INT(*)
C  out: ISTAT .... output status: - INT
C  =====================================================================
      SUBROUTINE UWRITT (IUNIT,IREC,IARR,ISTAT)

      EXTERNAL      UDOPEN

      INTEGER       IARR(*)
      INTEGER       IUNIT,JCOND,JRECL,IREC,ISTAT,IC,II,IE
      CHARACTER*128 NEWNAM
      PARAMETER   ( JCOND=1 )

      INCLUDE 'updaio'

      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'udaiox'
      INCLUDE 'ufiles'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_udaio/RCS/uwritt.f,v $
     . $',                                                             '
     .$Id: uwritt.f,v 1.4 2001/06/12 19:40:09 dws Exp $
     . $' /
C    ===================================================================
C

C         If debug flag is on, output debug statement

      IF (ICMDBG.GT.0) WRITE (ICMPRU,300,IOSTAT=IE) IUNIT,IFILES(IUNIT)

C         1 - Initialize output status to "no error" (set ISTAT =  0)
C         2 - Give error for bad input unit number       (ISTAT =  1)
C         3 - Give error if unit is for sequential file  (ISTAT =  1)
C         4 - Give error for bad input record length     (ISTAT = -1)

      ISTAT = 0
      IF (IUNIT.LT.1 .OR. IUNIT.GT.MFILES) THEN
        IF (IDAERP.GT.0) WRITE (LP,280,IOSTAT=IE) IUNIT,MFILES
        ISTAT = 1
      ELSEIF (IFILES(IUNIT) .EQ. 2) THEN
        IF (IDAERP.GT.0) WRITE (LP,350,IOSTAT=IE) IUNIT
        ISTAT = 1
      ELSEIF (IREC .LE. 0) THEN
        IF (IDAERP.GT.0) WRITE (LP,270,IOSTAT=IE) IUNIT
        ISTAT = -1
      ELSE

C         No immediate error:
C         1 - If debug flag is on, output another debug statement
C         2 - Check if unit is already open using array UPRECL,
C              JRECL from UPRECL is the record length
 
      IF (ICMDBG.GT.0) WRITE (ICMPRU,320,IOSTAT=IE) IREC,IUNIT

        JRECL = UPRECL(IUNIT)
        IF (JRECL.LE.0) THEN
 
C         File is NOT opened, call UDOPEN to open by unit number
C         Give error and log statements if unable to open (ISTAT = 2)
C         Else set IFILES flag to 1 for an opened random access file,
C          and output debug statement if debug flag is on
 
          CALL UDOPEN(IUNIT,JCOND,NEWNAM,JRECL,IC)
          IF (IC.NE.0) THEN
            IF (UE.GE.0) WRITE(UE,30,IOSTAT=IE) IUNIT,IC,IREC
            IF (IDAERP.GT.0) WRITE (LP,370,IOSTAT=IE) IUNIT
            ISTAT = 2
          ELSE
            IFILES(IUNIT) = 1
            IF (ICMDBG.GT.0) WRITE (ICMPRU,310,IOSTAT=IE) IUNIT
          ENDIF
        ENDIF

C         If no error exists, file is open and ready for writing:
C         1 - Write record IREC, for JRECL 4-byte integer words frm IARR
C         2 - Give error and log statements if write is bad (ISTAT = 2)

        IF (ISTAT.EQ.0) THEN
          WRITE(IUNIT,REC=IREC,IOSTAT=IC) (IARR(II),II=1,JRECL)
          IF (IC.GT.0) THEN
            IF (UE.GE.0) WRITE(UE,31,IOSTAT=IE) IUNIT,IC,IREC
            IF (IDAERP.GT.0) WRITE (LP,360,IOSTAT=IE) IREC,IUNIT
            ISTAT = 3
          ENDIF
        ENDIF

      ENDIF

      RETURN

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   30 FORMAT(' uwritt',I4,'  ** ERROR',I5,'  at rec',I7,'  from UDOPEN')
   31 FORMAT(' uwritt',I4,'  ** ERROR',I5,'  at rec',I7,'  bad write')

  270 FORMAT('0**ERROR** TRYING TO WRITE USING ZERO OR NEGATIVE ',
     *       'RECORD NUMBER FOR UNIT ',I3,'.')
  280 FORMAT('0**ERROR** UNIT NUMBER ',I4,' IS INVALID. VALID UNIT ',
     *       'NUMBERS ARE 1 THRU ',I3,'.')
  300 FORMAT(' IUNIT=',I3,3X,'IFILES(IUNIT)=',I2)
  310 FORMAT(' UNIT ',I3,' SUCCESSFULLY OPENED')
  320 FORMAT(' CALLING DWRITE TO WRITE RECORD NUMBER ',I6,' TO UNIT ',
     *       I3)
  350 FORMAT('0**ERROR** ATTEMPT TO USE DAIO WRITE ON SEQUENTIAL',
     *       ' FILE UNIT ',I3,'.')
  360 FORMAT('0**ERROR** DAIO ERROR WRITING RECORD ',I6,' TO UNIT ',
     *       I3,'.')
  370 FORMAT('0**ERROR** DAIO ERROR OPENING FILE FOR UNIT ',I3,'.')

      END
