C MODULE HCLDAT
C***********************************************************************
C  HCLDAT PARSES AN HCL DATE STRING AND CHECKS FOR VALID DATE AND TIME.
C
C  It was separated from HCKDAT (8/98) in order to use a character
C  string instead of an unpacked integer array.  All error messages
C  were placed in a new subroutine HCLERR.  See HCKDAT for history.
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       STRNG       C    I     *     CHAR STRING CONTAINING HCL DATE
C                                     (SHOULD LIMIT TO 24 CHARS OR LESS)
C       ITBUF       I    O     7     ARRAY TO HOLD DATE AND TIME VALUES:
C                                     ITBUF(1)=JULIAN HOUR
C                                     ITBUF(2)=MONTH
C                                     ITBUF(3)=DAY
C                                     ITBUF(4)=YEAR
C                                     ITBUF(5)=HOUR
C                                     ITBUF(6)=TIME ZONE CODE
C                                     ITBUF(7)=ZERO
C       IERR        I    O     1     STATUS CODE:
C                                     0=NORMAL RETURN
C                                     1=NOT A VALID DATE
C***********************************************************************
C  cmt: Common and its variables used by this routine:
C  cmt:     UDATAS ..... IBLNK         blank character ('    ')
C  cmt:                  IASTR         astricks char as an int ('*   ')
C  cmt:                  IPOUND        pound char as an int ('#   ')
C  cmt:                  IPRCNT        percent char as an int ('%   ')
C  cmt:     HDFLTS ..... TIME(3)       TIME(2)=hour offset
C  cmt:                                TIME(3)=time zone code
C  cmt:                  NLSTZ         time zone number (EST and EDT=-5)
C  cmt:                  LOCAL         diff between local standard and
C  cmt:                                 internal clock hours (EST=7)
C  cmt:     PDDTDR ..... IDDTDR(11,*)  date of last day of data
C  cmt:                                * comes from *=IPDCKD('PP24')
C  *********************************************************************
C  cmt: CC-1 lines are more logical initialization and default settings
C  cmt:       but do not work; most likely because following routines
C  cmt:       do this work later.
C  cmt: CC-2 lines are for debugging using unit UU and routine UPINIO.
C  cmt: CC-3 lines needed to force any hour to be two digits.
C  cmt: CC-4 more intuitive year numbers over the hour number.
C  *********************************************************************
      SUBROUTINE HCLDAT (STRNG,ITBUF,IERR)

      EXTERNAL     HCLERR,UMEMOV,DDGCH2,DDYCDL,KKCAPS,HCKDTC
      EXTERNAL     DDECML,KKTRIM,IPDCKD
      EXTERNAL     RPPDCO,MDYH2

      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pddtdr'

      INTEGER      ITBUF(7),IERR,IS,IE,ISTRT,IEND,INTEGR,INTGR,INEX
      INTEGER      ITZC,IDX,INTHR,JULDAT,JERR,KERR,II,ISPEC
      INTEGER      NMON,NDAY,NYR,NHR,ITZ,IDSAV

      CHARACTER*(*)  STRNG
      CHARACTER*24   XCHAR
      CHARACTER*1    KHAR,KHR1
      CHARACTER*80   MESSG

      CHARACTER*4    KHCONV
      INTEGER        IICONV
      EQUIVALENCE   (KHCONV,IICONV)

CC-1  CHARACTER*4    KHZULU
CC-1  INTEGER        IIZULU
CC-1  EQUIVALENCE   (KHZULU,IIZULU)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_hckdat/RCS/hcldat.f,v $
     . $',                                                             '
     .$Id: hcldat.f,v 1.7 2005/10/31 18:12:23 xfan Exp $
     . $' /
C    ===================================================================
C

CC-1  DATA  KHZULU / 'Z   ' /

C  Put input into known length working string, get first/last char

      XCHAR = STRNG

C  Initialize output

      IERR = 0
CC-1  ITBUF(1) = 0
CC-1  ITBUF(2) = 0
CC-1  ITBUF(3) = 0
      ITBUF(4) = 0
CC-1  ITBUF(5) = -1
CC-1  ITBUF(6) = IBLNK
      ITBUF(7) = 0

C  Initialize vars and flags (NOEND=0 not at end)

      ITZC  = 0
      NOEND = 0

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - If bad number of chars entered set KHR1 = ' ', else use first
C         char in a case structure to determine how to get the date
C        (note, KKTRIM will return IS=1 if string is blank)

      CALL KKTRIM(XCHAR,IS,IE)
CC-2  write(UU,'(''XCHAR('',2I5,''): '',A)',IOSTAT=IJK) IS,IE,XCHAR
      KHR1 = ' '
      IF (IE-IS .LT. 16) KHR1 = XCHAR(IS:IS)

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Set flag that will indicate if a special char starts the date
C  - - -  for later use when checking for one or two digit hour number.
C  - - - If the date starts with a special character, we do NOT want to
C  - - -  flag an one-digit hour as bad.  (dws, Sep 2004 - R25-50)

      ISPEC=0

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Bad start due to incorrect number of chars or blank input

      IF (KHR1 .EQ. ' ') THEN
        CALL HCLERR('W','HCKDAT',2,IERR)
        NOEND = 1

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Check for LASTPP24 option (set today's date to the date of the
C                                   last day of data for type PP24)

      ELSEIF (XCHAR(1:8) .EQ. 'LASTPP24') THEN

C  Read preprocessor data base controls, if no error find datatype PP24

        CALL RPPDCO (IERR)
        IF (IERR.GT.0) THEN
          CALL HCLERR('W','HCKDAT',3,IERR)
        ELSE
          IDX = IPDCKD('PP24')
          IF (IDX .EQ. 0) THEN
            CALL HCLERR('W','HCKDAT',4,IERR)

C  Get date of last day of data, convert from julian date

          ELSE
            CALL UMEMOV (IDDTDR(11,IDX),JULDAT,1)

            INTHR = 0
            CALL MDYH2 (JULDAT,INTHR,NMON,NDAY,NYR,NHR,
     $                  ITZ,IDSAV,TIME(3))

            ITBUF(1) = JULDAT
            ITBUF(2) = NMON
            ITBUF(3) = NDAY
            ITBUF(4) = NYR
            ITBUF(5) = NHR
            ITBUF(6) = TIME(3)
            WRITE(MESSG,'(''0**NOTE** TODAY''''S DATE SET TO '',
     $            I2.2,''/'',I2.2,''/'',I4.4,''-'',I2.2,A4,''.'')')
     $       ITBUF(2),ITBUF(3),ITBUF(4),ITBUF(5),ITBUF(6)
            CALL HCLERR('M',MESSG,0,KERR)
          ENDIF
        ENDIF
        NOEND = 1

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Check for * value (set date to today's date plus/minus days)

      ELSEIF (KHR1 .EQ. '*') THEN
        ISPEC=1
        ITBUF(1) = 0
        ITBUF(2) = IASTR
        ITBUF(3) = 0
        ITBUF(4) = IASTR
        ITBUF(5) = -1
        ITBUF(6) = IBLNK

        ISTRT = IS+1
        IF (ISTRT .LE. IE) THEN
          ITBUF(5) = -1
          ITBUF(6) = IBLNK
          KHAR = XCHAR(ISTRT:ISTRT)
          IF (KHAR.EQ.'+' .OR. KHAR.EQ.'-') THEN

C  Get "+" or "-" days

            II = ISTRT
            IEND = IE
   12       IF (II .GE. IEND) GOTO 14
              II = II+1
              KHAR = XCHAR(II:II)
              IF (KHAR.LT.'0' .OR. KHAR.GT.'9') IEND = II-1
              GOTO 12
   14       CONTINUE

CCC           Using 3 instead of 2 below will cause three digit numbers
CCC            to be interpreted as a day offset instead of a single
CCC            number day offset and a 2-digit hour number.  Thus to use
CCC            an hour number you must have at least 4 digits.
CCC               IF (IEND-ISTRT .GT. 2) IEND = IEND-2
CCC
CCC           Code changes allow for three digit day increment and for
CCC           optionally using a / or : in front of an hour number in
CCC           the "*" hcl date option ..... dws July 12, 2002

            ISKIP = 1
            IF (IEND .LT. IE) THEN
              KHAR = XCHAR(IEND+1:IEND+1)
              IF (KHAR.EQ.'/' .OR. KHAR.EQ.':') ISKIP = 2
            ENDIF

            IF (ISKIP.EQ.1 .AND. IEND-ISTRT.GT.3) IEND = IEND-2

            IF (IEND .LT. ISTRT) THEN
              CALL HCLERR('W','HCKDAT',17,IERR)
            ELSE IF (IEND .EQ. ISTRT) THEN
              ITBUF(3) = 0
              ISTRT = IEND + ISKIP
            ELSE
              READ(XCHAR(ISTRT:IEND),*,IOSTAT=JERR) INTEGR
              ISTRT = IEND + ISKIP
              IF (JERR .NE. 0) THEN
                CALL HCLERR('W','HCKDAT',18,IERR)
              ELSE
                ITBUF(3) = INTEGR
              ENDIF
            ENDIF

C  Skip "/" or ":" as if it starts an hour number

          ELSE IF (KHAR.EQ.'/' .OR. KHAR.EQ.':') THEN
            ISTRT = ISTRT + 1
          ENDIF
        ENDIF

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Check for # value (Initz defaults to previous default interval)

      ELSEIF (KHR1 .EQ. '#') THEN
        ISPEC=2
        ITBUF(1) = 0
        ITBUF(2) = IPOUND
        ITBUF(3) = 0
        ITBUF(4) = 0
        ITBUF(5) = 0

        ISTRT = IS+1
        IF (ISTRT .LE. IE) THEN
          KHAR = XCHAR(ISTRT:ISTRT)
          IF (KHAR.NE.'+' .AND. KHAR.NE.'-') THEN
            CALL HCLERR('W','HCKDAT',5,IERR)
             WRITE(MESSG,'(28X,''Bad position field is'',I3)') ISTRT
            CALL HCLERR('M',MESSG,0,KERR)

C  Check if "+" or "-" hours, if found get hour

          ELSE
            IF (KHAR .EQ. '+') ITBUF(3) = 1

            IF (ISTRT .LT. IE) THEN
              ISTRT = ISTRT+1
              IEND  = IE
              II    = ISTRT
   22         IF (II .GT. IEND) GOTO 24
                KHAR = XCHAR(II:II)
                IF (KHAR.LT.'0' .OR. KHAR.GT.'9') THEN
                  IEND = II-1
                  CALL HCLERR('W','HCKDAT',28,IERR)
                ENDIF
                II = II+1
                GOTO 22
   24         CONTINUE

              IF (ISTRT .LE. IEND) THEN
                READ(XCHAR(ISTRT:IEND),*,IOSTAT=JERR) INTEGR
                ISTRT = IEND+1
                IF (JERR .NE. 0) THEN
                  CALL HCLERR('W','HCKDAT',19,IERR)
                ELSE
                  IF (INTEGR.LT.1 .OR. INTEGR.GT.24) THEN
                    CALL HCLERR('W','HCKDAT',27,IERR)
                  ELSE
                    IF (24 .NE. (24/INTEGR)*INTEGR) THEN
                      CALL HCLERR('W','HCKDAT',7,IERR)
                      WRITE(MESSG,'(28X,''Given hour is'',I3.2)') INTEGR
                      CALL HCLERR('M',MESSG,0,KERR)
                    ELSE
                      ITBUF(5) = INTEGR
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        NOEND = 1

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Check for % value (Initz defaults to the previous 12Z
C                           referenced to local time)

      ELSEIF (KHR1 .EQ. '%') THEN
        ISPEC=3
        ITBUF(1) = 0
        ITBUF(2) = IPRCNT
        ITBUF(3) = 0
        ITBUF(4) = 0
        ITBUF(5) = 12-TIME(2)
        ITBUF(6) = TIME(3)

C  Check if "+" or "-" hours, if found continue later to get hour/tm-zn

        ISTRT = IS+1
        IF (ISTRT .LE. IE) THEN
          KHAR = XCHAR(ISTRT:ISTRT)
          IF (KHAR.NE.'+' .AND. KHAR.NE.'-') THEN
            CALL HCLERR('W','HCKDAT',5,IERR)
             WRITE(MESSG,'(28X,''Bad position field is'',I3)') ISTRT
            CALL HCLERR('M',MESSG,0,KERR)
          ELSE
            IF (KHAR .EQ. '+') ITBUF(3) = 1
            IF (ISTRT .EQ. IE) CALL HCLERR('W','HCKDAT',17,IERR)
            ISTRT = ISTRT+1
          ENDIF
        ENDIF

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Check for mon-day-year (else have error)

C  Process month and day

      ELSEIF (KHR1.GE.'0' .AND. KHR1.LE.'9') THEN
        ITBUF(4) = IASTR
        ITBUF(5) = -1
        ITBUF(6) = IBLNK
        ISTRT = IS
        IF (ISTRT+3 .GT. IE) THEN
          CALL HCLERR('W','HCKDAT',9,IERR)
        ELSE
          READ(XCHAR(ISTRT:ISTRT+1),'(I2)',IOSTAT=JERR) INTEGR
          ISTRT = ISTRT+2
          IF (JERR .NE. 0) THEN
            CALL HCLERR('W','HCKDAT',20,IERR)
          ELSEIF (INTEGR.LT.1 .OR. INTEGR.GT.12) THEN
            CALL HCLERR('W','HCKDAT',10,IERR)
          ELSE
            ITBUF(2) = INTEGR
          ENDIF

          READ(XCHAR(ISTRT:ISTRT+1),'(I2)',IOSTAT=JERR) INTEGR
          ISTRT = ISTRT+2
          IF (JERR .NE. 0) THEN
            CALL HCLERR('W','HCKDAT',21,IERR)
          ELSEIF (INTEGR.LT.1 .OR. INTEGR.GT.31) THEN
            CALL HCLERR('W','HCKDAT',11,IERR)
          ELSE
            ITBUF(3) = INTEGR
            IF (ISTRT .GT. IE) THEN
              ITBUF(4)=TDATES(4)
              CALL DDECML(ITBUF(4),ITBUF(2),ITBUF(3),JERR)
              IF (JERR .NE. 0) CALL HCLERR('W','HCKDAT',24,IERR)
            ENDIF
          ENDIF
        ENDIF

C  Check for year number (two digits only unless it is in slashes)

        IF (IERR.EQ.0 .AND. ISTRT.LE.IE) THEN
          KHAR = XCHAR(ISTRT:ISTRT)
          IF (KHAR .EQ. '/') THEN

C  Have slash, get ISTRT as first year-digit, IEND as last year-digit

            IEND  = ISTRT
            ISTRT = ISTRT+1
            INEX  = ISTRT
   42       IF (INEX .GE. IE) GOTO 44
              INEX = INEX+1
              IF (XCHAR(INEX:INEX) .EQ. '/') THEN
                IEND = INEX-1
                INEX = IE
              ENDIF
              GOTO 42
   44       CONTINUE

            IF (ISTRT .LE. IEND) THEN
              READ(XCHAR(ISTRT:IEND),*,IOSTAT=JERR) INTEGR
              ISTRT = IEND+1
              IF (JERR .NE. 0) THEN
                CALL HCLERR('W','HCKDAT',22,IERR)
              ELSEIF (INTEGR.LT.0 .OR. INTEGR.GT.3000) THEN
                CALL HCLERR('W','HCKDAT',14,IERR)
              ELSE
                CALL DDYCDL(INTEGR,ITBUF(2),ITBUF(3))
                ITBUF(4) = INTEGR
              ENDIF
            ELSE
              CALL HCLERR('W','HCKDAT',17,IERR)
            ENDIF

            IF (XCHAR(ISTRT:ISTRT) .EQ. '/') ISTRT = ISTRT+1

C  Do NOT have a slash so look for hh or yyhh
C  Must have more than one digit here, else skip to hr/tz parse section

          ELSEIF (ISTRT.LT.IE .AND. KHAR.GE.'0' .AND. KHAR.LE.'9') THEN

            IEND = ISTRT+1
            KHAR = XCHAR(IEND:IEND)
            IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
              READ(XCHAR(ISTRT:IEND),*,IOSTAT=JERR) INTEGR

C  If INTEGR is greater that 24, we have a two digit year number, yy

              IF (JERR .EQ. 0) THEN
                IF (INTEGR .GT. 24) THEN
CC-1              KHAR = ' '
CC-1              IF (IEND .LT. IE) KHAR = XCHAR(IEND+1:IEND+1)
CC-1              IF (KHAR.LT.'0' .OR. KHAR.GT.'9') THEN
CC-1                CALL HCLERR('W','HCKDAT',26,KERR)
CC-2                CALL HCLERR('E','HCKDAT',26,IERR)
CC-1              ENDIF
                  CALL DDYCDL(INTEGR,ITBUF(2),ITBUF(3))
                  ITBUF(4) = INTEGR
                  ISTRT = IEND+1

C  Or maybe the third and fourth digits are greater that 24, yyyy;
C  Or there is a digit after the 4-digit year, yyyyhh;
C   then must be a year ... we exclude 1920-1924 when using 2-digit yr

                ELSEIF (IEND+2 .LE. IE) THEN
                  READ(XCHAR(IEND+1:IEND+2),*,IOSTAT=JERR) INTGR
                  IF (JERR .EQ. 0) THEN
                    KHAR = ' '
                    IF (IEND+2 .LT. IE) KHAR = XCHAR(IEND+3:IEND+3)

C  Below are two choices, use first if hour always takes precedence over
C   what looks like a good 4-digit year (12032001 as 1920 at hour 1),
C   else use the second (12032001 at 2001 at no given hour)

                    IF ( (KHAR.GE.'0' .AND. KHAR.LE.'9') .OR.
     $                   (INTGR.GT.24) ) THEN
CC-4                IF ( (KHAR.GE.'0' .AND. KHAR.LE.'9') .OR.
CC-4 $                   (INTEGR.GT.19) .OR.
CC-4 $                   (INTEGR.EQ.19 .AND. INTGR.GT.24) ) THEN

                      INTEGR = 100*INTEGR + INTGR
                      CALL DDYCDL(INTEGR,ITBUF(2),ITBUF(3))
                      ITBUF(4) = INTEGR
                      ISTRT = IEND+3
CC-1                  IF (KHAR.LT.'0' .OR. KHAR.GT.'9') THEN
CC-1                    CALL HCLERR('W','HCKDAT',26,KERR)
CC-2                    CALL HCLERR('E','HCKDAT',26,IERR)
CC-1                  ENDIF
                    ELSE
                      CALL DDYCDL(INTEGR,ITBUF(2),ITBUF(3))
                      ITBUF(4) = INTEGR
                      ISTRT = IEND+1
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF

            ENDIF
          ENDIF
        ENDIF

      ELSE

C  First character (KHR1) not good for date so put out error message

        CALL HCLERR('W','HCKDAT',8,IERR)
        NOEND = 1
      ENDIF

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Check for hour and time zone

      IF (IERR.EQ.0 .AND. NOEND.EQ.0) THEN

        IF (ISTRT .LE. IE) THEN
          KHAR = XCHAR(ISTRT:ISTRT)
          IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
            IEND = ISTRT+1
            IF (IEND .GT. IE) IEND = IE
            IF (IEND .GT. ISTRT) THEN
              KHAR = XCHAR(IEND:IEND)
              IF (KHAR.LT.'0' .OR. KHAR.GT.'9') THEN
                IEND = IEND-1
CC-3            CALL HCLERR('W','HCKDAT',9,IERR)
              ENDIF
CC-3        ELSE
CC-3          CALL HCLERR('W','HCKDAT',9,IERR)
            ENDIF

C  - - - If start and end char position are the same, then we have only
C  - - -  a one character hour so if the date code does not start with
C  - - -  a special char then consider this an error (Sep 2004, R25-50).

            IF (ISTRT.EQ.IEND .AND. ISPEC.EQ.0) THEN
              CALL HCLERR('E','HCKDAT',29,IERR)
            ENDIF

            IF (ISTRT .LE. IEND) THEN
              READ(XCHAR(ISTRT:IEND),*,IOSTAT=JERR) INTEGR
              ISTRT = IEND+1
              IF (JERR .NE. 0) THEN
                CALL HCLERR('W','HCKDAT',13,IERR)
              ELSEIF (INTEGR.LT.0 .OR. INTEGR.GT.24) THEN
                CALL HCLERR('W','HCKDAT',6,IERR)
              ELSE
                ITBUF(5) = INTEGR
CC-1            ITBUF(6) = TIME(3)
CC-1            CALL HCKDTC (ITBUF(6),ITZC,JERR)
CC-1            IF (JERR .NE. 0) CALL HCLERR('X','HCKDAT',16,IERR)
              ENDIF
            ENDIF
          ENDIF
CC-1    ELSEIF (KHR1.NE.'%' .AND. ITBUF(5).EQ.-1) THEN
CC-1      ITBUF(5) = 12
CC-1      ITBUF(6) = IIZULU
        ENDIF

C  Check for time zone code

        IF (ISTRT .LE. IE) THEN
          IEND = IE
          IF (IEND-ISTRT+1 .GT. 4) THEN
            CALL HCLERR('W','HCKDAT',15,IERR)
          ELSE
            KHCONV = XCHAR(ISTRT:IEND)
            CALL KKCAPS(KHCONV)
            ITBUF(6) = IICONV
            CALL HCKDTC (ITBUF(6),ITZC,JERR)
            IF (JERR .NE. 0) CALL HCLERR('X','HCKDAT',16,IERR)
          ENDIF
        ENDIF

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Since year is not given, find year using 90/10 rule

        IF (KHR1.NE.'*' .AND. KHR1.NE.'%') THEN
          IF (ITBUF(4).EQ.0.OR.ITBUF(4).EQ.IASTR) THEN
             ITBUF(4)=TDATES(4)
             CALL DDECML(ITBUF(4),ITBUF(2),ITBUF(3),JERR)
             IF (JERR .NE. 0) CALL HCLERR('W','HCKDAT',24,IERR)
          ENDIF
        ENDIF

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - Calculate julian hour if year is specified (old rtn ujlntc)

        IF (ITBUF(6).NE.IBLNK .AND. KHR1.NE.'*' .AND. KHR1.NE.'%') THEN
          CALL DDGCH2 (JULDAT,ITBUF(4),ITBUF(2),ITBUF(3),ITBUF(5))
          IF (ITZC .LE. 20) JULDAT = JULDAT+NLSTZ-LOCAL+ITZC
          ITBUF(1) = JULDAT
        ENDIF

      ENDIF

C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  - - - End of date parsing, if error occurred, output the given string

      IF (IERR .NE. 0) THEN
        WRITE(MESSG,'(28X,''Given date string: '',A)') XCHAR(1:IE)
        CALL HCLERR('M',MESSG,0,KERR)
      ENDIF

      RETURN
      END
