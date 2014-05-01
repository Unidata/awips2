C MODULE HCKLST
C-----------------------------------------------------------------------
C  ROUTINE TO DETERMINE IF THE LAST COMPUTATIONAL DAY HAS BEEN SET
C   FURTHER INTO THE FUTURE THAN IS ALLOWED.  THE LIMIT IS SET BY A
C   TECHNIQUE NAMED LSTALLOW.  THE DEFAULT VALUE FOR LSTALLOW WAS
C   ORIGINALLY SET AT 6 FOR 6 HOURS INTO THE FUTURE.
C
C  THIS FEATURE WAS ADDED AFTER MANY USERS INADVERTENTLY SET THE LAST
C   COMPUTATIONAL DAY FURTHER IN THE FUTURE THAN THEY REALLY WANTED TO,
C   ESPECIALLY WHEN FORECASTS WERE BEING MADE AROUND THE CLOCK AND
C   DATES WERE GETTING CONFUSED.  THE RAMIFICATIONS OF THIS ACTION
C   WERE ESPECIALLY FELT IN THE RRS PREPROCESSOR.  NOW THE FCST PROGRAM
C   WILL SHUT THE FUNCTION DOWN IF THE LSTALLOW LIMIT IS VIOLATED.
C
C  WRITTEN BY JTOSTROWSKI - 3/93
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ARGUMENTS:
C     NAME         TYPE    I/O    DESCRIPTION
C     ------------ ----    ---    -----------------------------------
C     FUNNAM         R      I     FUNCTION NAME (UP TO 8 CHARS)
C     ISTAT          I      O     RETURN CODE; =0, OK
C                                              NE 0, ERROR
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      SUBROUTINE HCKLST (FUNNAM,ISTAT)

      DIMENSION     FUNNAM(2),IDATE(8),IARGS(20)
      CHARACTER*8   XTEMP8,RTNNAM

      INCLUDE 'uio'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'common/where'
      INCLUDE 'common/errdat'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hcklst.f,v $
     . $',                                                             '
     .$Id: hcklst.f,v 1.3 1998/07/06 11:30:28 page Exp $
     . $' /
C    ===================================================================
C

      DATA    RTNNAM/'HCKLST'/

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET LOCATION INFORMATION FOR ERROR MESSAGES

      CALL UMEMOV (OPNAME,XTEMP8,2)
      CALL UMEMOV (RTNNAM,OPNAME,2)

C  INITIALIZE THE WARNING AND ERROR COUNTERS FOR THIS FUNCTION

      NWARN=0
      NERRS=0

C  INITIALIZE THE RETURN CODE

      ISTAT=0

C  NOW, GET TO WORK!
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET VALUE OF LSTCMPDY TECHNIQUE FOR THIS FUNCTION

      CALL HPASTA ('LSTCMPDY',20,ILCRUN,NWORDS,IARGS,IERR)
      IF (IERR.GT.0) CALL FPHPWN (IERR,'LSTCMPDY')
      LSTCMP=IARGS(1)

C  GET VALUE OF LSTALLOW TECHNIQUE FOR THIS FUNCTION

      CALL HPAST ('LSTALLOW',LALLOW,IERR)
      IF (IERR.GT.0) CALL FPHPWN (IERR,'LSTALLOW')
      CALL FTEKCK (LALLOX,'LSTALLOW',6,LALLOW,0,72)
      LALLOW=LALLOX

C  GET CURRENT TIME AND CONVERT TO SAME UNITS AS VALUE OF LSTCMPDY
C   (I.E. - INTERNAL TIME)

      CALL HGETTM (IDATE,'INTL')
      IJUL=IDATE(1)

C  ERROR OUT IF LSTCMPDY IS SET MORE THAN LALLOW HOURS
C   BEYOND CURRENT TIME
CC  ****** NOTE ******* this was made only a warning when doing
CC                      y2k upgrading!

      IF (LSTCMP.GT.(IJUL+LALLOW)) THEN
         WRITE (LP,610) IARGS(2),IARGS(3),IARGS(4),IARGS(5),IARGS(6),
     *        LALLOW,FUNNAM
 610     FORMAT ('0**WARNING** LSTCMPDY (',I2.2,'/',I2.2,'/',I4.4,
     *      '/',I2.2,' ',A4,') EXCEEDS THE CURRENT TIME BEYOND',
     *          ' THE ALLOWED LIMIT.' /
     *      11X,'TO CHANGE THE LIMIT, SET THE LSTALLOW TECHNIQUE ',
     *          'TO A GREATER VALUE ',
     *          '(CURRENTLY ',I2,' FOR FUNCTION ',2A4,').')
         CALL WARN()
CC       CALL ERROR
CC       ISTAT=1
CC       CALL STOPFN (FUNNAM)
         ENDIF

      CALL UMEMOV (XTEMP8,OPNAME,2)

      RETURN
      END
