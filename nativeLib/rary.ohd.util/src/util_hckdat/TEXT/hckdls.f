C$PRAGMA C (UDATL)
C MODULE HCKDLS
C-----------------------------------------------------------------------
C
      SUBROUTINE HCKDLS (IUDATL,IUDATE,ICKDLS)
C
C  ROUTINE TO DETERMINE IF DAYLIGHT SAVINGS TIME IS IN EFFECT
C     Updated to account for New DST definition starting March 2007
C         (2nd Sunday in March through 1st Sunday in November)
C
C     IUDATE is Date/Time Array (I/O)
C     ICKDLS is DST Flag 1=Yes (DST), 0=No (Standard Time)
C
C-----------------------------------------------------------------------
C
      PARAMETER (NZONES=7)
      CHARACTER*4 ZONES(NZONES)
     *   /'Z','E','C','M','P','A','ANY' /
C
      DIMENSION IUDATE(6)
C
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/util/src/util_hckdat/RCS/hckdls.f,v $
     . $',                                                             '
     .$Id: hckdls.f,v 1.3 2002/02/11 15:30:37 michaelo Exp jgofus $
     . $' /
C    ===================================================================
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HCKDLS'
C
C  CHECK SYSTEM CLOCK TIME ZONE
      NCHAR=1
      DO 10 IZ=1,NZONES
         CALL UCMPAR (CLKZON,ZONES(IZ),NCHAR,IDIFF)
         IF (IDIFF.EQ.0) GO TO 20
10       CONTINUE
C
C  VALID TIME ZONE NOT FOUND - DEFAULT TO EASTERN - Why? jgg left this but
C  thinks it is suspicious and a likely source of errors in most of the US, 
C  so added a warning message.
      IZ=2
      WRITE (IOGDB,*) 'IN HCKDLS - Valid Time Zone NOT FOUND, using E'
C
C  ASSUME NOT IN DAYLIGHT SAVINGS TIME
20    ICKDLS=0
C
C  IF CLOCK IS ON Z TIME DO NOT NEED TO CHECK FOR DAYLIGHT SAVINGS TIME
      IF (IZ.EQ.1) GO TO 30
C
C  GET CURRENT CLOCK TIME
      IF (IUDATL.EQ.1) CALL UDATL (IUDATE)
C
C  GET MONTH, DAY, AND HOUR/MINUTE FROM DATE ARRAY AND SET YEAR TO 2
C  DIGITS - actually just calculates years since 1900
      IMO=IUDATE(3)
      IDA=IUDATE(4)
      IHM=IUDATE(5)
      IYR=IUDATE(1)
      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*) 'IN HCKDLS - IMO=',IMO,' IDA=',IDA,' IYR=',
     *      IYR,' IHM=',IHM
         ENDIF
      IMOD=100
      IYR=MOD(IUDATE(1),IMOD)
      IF (IYR.LT.IMOD) IYR=IYR+IMOD
C
C  Check which DST definition to use 
C     IDEF= 1 - prior to 2007 def'n
C     IDEF= 2 - 2007 and beyond
C
      IF (IYR .LE. 106 .OR. IYR .GT. 170) THEN

C  =======================================================================
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C  Using the old DST Definition years between 1970 and 2006      
C
C  IF BETWEEN APRIL AND OCTOBER THEN IN DAYLIGHT SAVINGS TIME
C
        IF (IMO.GT.4.AND.IMO.LT.10) THEN
           ICKDLS=1
           GO TO 30
           ENDIF
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  IN APRIL OR IN OCTOBER - CHECK IF IN DAYLIGHT SAVINGS TIME
C
      IF (IMO.EQ.4) THEN
C     IN APRIL - IF AFTER APRIL 7TH MUST BE IN DAYLIGHT SAVINGS TIME
         IF (IDA.GT.7) THEN
            ICKDLS=1
            ELSE
C           IN PERIOD APRIL 1-7 - CHECK WHAT IS DATE OF 1ST SUN
C           JDX IS THE NUMBER OF DAYS FROM 1/1/1900 TO 4/1/IYR
               JDX=IYR*365+IYR/4+90+1
C           JAN 1, 1900 WAS A MONDAY
               IDAY=MOD(JDX,7)
C           IF 1ST IS SUNDAY,    IDAY=0 AND 1ST SUNDAY IS APRIL 1
C           IF 1ST IS MONDAY,    IDAY=1 AND 1ST SUNDAY IS APRIL 7
C           IF 1ST IS TUEDAY,    IDAY=2 AND 1ST SUNDAY IS APRIL 6
C           IF 1ST IS WEDNESDAY, IDAY=3 AND 1ST SUNDAY IS APRIL 5
C           IF 1ST IS THURSDAY,  IDAY=4 AND 1ST SUNDAY IS APRIL 4
C           IF 1ST IS FRIDAY,    IDAY=5 AND 1ST SUNDAY IS APRIL 3
C           IF 1ST IS SATURDAY,  IDAY=6 AND 1ST SUNDAY IS APRIL 2
               IF (IDAY. GT. 0) THEN
                  I1SUND=8 - IDAY
                  ELSE
                     I1SUND=1
                  ENDIF
               IF (IDA.GT.I1SUND) THEN
                  ICKDLS=1
                  ELSE IF (IDA.EQ.I1SUND) THEN
C                 CURRENT DAY IS 1ST SUNDAY IN APRIL, CHECK HOUR
C                 IF PAST 2 AM ARE IN DST
                     IF (IHM.GE.0200) THEN
                        ICKDLS=1
                     ENDIF
                 ENDIF
              ENDIF
           ENDIF
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
        IF (IMO.EQ.10) THEN
C     IN OCTOBER - DST ENDS ON LAST SUNDAY
C                  
           IF (IDA.LT.25) THEN
              ICKDLS=1
              ELSE 
C           WHAT DAY OF THE WEEK IS November 1ST?
                 JDX=IYR*365+IYR/4+304+1
                 IDAY=MOD(JDX,7)
C           IF 1ST IS SUNDAY,    IDAY=0 AND LAST SUNDAY IS OCTOBER 25
C           IF 1ST IS MONDAY,    IDAY=1 AND LAST SUNDAY IS OCTOBER 31
C           IF 1ST IS TUEDAY,    IDAY=2 AND LAST SUNDAY IS OCTOBER 30
C           IF 1ST IS WEDNESDAY, IDAY=3 AND LAST SUNDAY IS OCTOBER 29
C           IF 1ST IS THURSDAY,  IDAY=4 AND LAST SUNDAY IS OCTOBER 28
C           IF 1ST IS FRIDAY,    IDAY=5 AND LAST SUNDAY IS OCTOBER 27
C           IF 1ST IS SATURDAY,  IDAY=6 AND LAST SUNDAY IS OCTOBER 26
                 IF (IDAY.GT.0) THEN
                    ILSTSUN=32 - IDAY
                    ELSE
                      ILSTSUN=25
                    ENDIF
                 IF (IDA.LT.ILSTSUN) THEN
                    ICKDLS=1
                    ELSE IF (IDA.EQ.ILSTSUN) THEN
C                 CURRENT DAY IS LAST SUNDAY IN OCTOBER, CHECK HOUR
C                 (USE 130AM FOR CUTOFF BECAUSE PERIOD FROM 1AM TO 2AM
C                 OCCURS TWICE)
                      IF (IHM.LT.0130) THEN
                        ICKDLS=1
                      ENDIF
                 ENDIF
              ENDIF
           ENDIF
        ELSE 
C
C =====================================================================

C  Using the new DST Definition years 2007 and beyond...      
C
C  IF BETWEEN APRIL AND OCTOBER THEN IN DAYLIGHT SAVINGS TIME
C
        IF (IMO.GT.3.AND.IMO.LT.11) THEN
           ICKDLS=1
           GO TO 30
           ENDIF
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  IN MARCH OR IN NOVEMBER - CHECK IF IN DAYLIGHT SAVINGS TIME
C  I
C
        IF (IMO.EQ.3) THEN
C     IN MARCH - IF AFTER MARCH 14TH MUST BE IN DAYLIGHT SAVINGS TIME
           IF (IDA .GE. 8) THEN
             IF (IDA .GT. 14) THEN
                ICKDLS=1
                ELSE
C           IN PERIOD MARCH 8-14 - CHECK WHAT IS DATE OF 1ST SUN
C           JDX IS THE NUMBER OF DAYS FROM 1/1/1900 TO 3/1/IYR
                   JDX=IYR*365+IYR/4+59+1
C           JAN 1, 1900 WAS A MONDAY
                   IDAY=MOD(JDX,7)
C           IF 1ST IS SUNDAY,    IDAY=0 AND 2ND SUNDAY IS MARCH 8
C           IF 1ST IS MONDAY,    IDAY=1 AND 2ND SUNDAY IS MARCH 14
C           IF 1ST IS TUEDAY,    IDAY=2 AND 2ND SUNDAY IS MARCH 13
C           IF 1ST IS WEDNESDAY, IDAY=3 AND 2ND SUNDAY IS MARCH 12
C           IF 1ST IS THURSDAY,  IDAY=4 AND 2ND SUNDAY IS MARCH 11
C           IF 1ST IS FRIDAY,    IDAY=5 AND 2ND SUNDAY IS MARCH 10
C           IF 1ST IS SATURDAY,  IDAY=6 AND 2ND SUNDAY IS MARCH 9
                   IF (IDAY. GT. 0) THEN
                      I2SUND=15 - IDAY
                      ELSE
                         I2SUND=8
                      ENDIF
                   IF (IDA .GT.I2SUND) THEN
                      ICKDLS=1
                      ELSE IF (IDA .EQ. I2SUND) THEN
C                 CURRENT DAY IS 2ND SUNDAY IN MARCH, CHECK HOUR
C                 IF PAST 2 AM ARE IN DST
                         IF (IHM. GE. 0200) THEN
                            ICKDLS=1
                         ENDIF
                   ENDIF
                 ENDIF
               ENDIF
	     ENDIF  
C
C       -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
        IF (IMO.EQ.11) THEN
C     IN NOVEMBER - IF AFTER  NOV 7 - STANDARD TIME
           IF (IDA.LE.7) THEN
C           IN PERIOD NOV 1-7 - FIND DATE OF 1ST SUN
C           WHAT DAY OF THE WEEK IS NOVEMBER 1ST?
                 JDX=IYR*365+IYR/4+304+1
                 IDAY=MOD(JDX,7)
C           IF 1ST IS SUNDAY,    IDAY=0 AND 1ST SUNDAY IS NOVEMBER 1
C           IF 1ST IS MONDAY,    IDAY 1=AND 1ST SUNDAY IS NOVEMBER 7
C           IF 1ST IS TUEDAY,    IDAY=2 AND 1ST SUNDAY IS NOVEMBER 6
C           IF 1ST IS WEDNESDAY, IDAY=3 AND 1ST SUNDAY IS NOVEMBER 5
C           IF 1ST IS THURSDAY,  IDAY=4 AND 1ST SUNDAY IS NOVEMBER 4
C           IF 1ST IS FRIDAY,    IDAY=5 AND 1ST SUNDAY IS NOVEMBER 3
C           IF 1ST IS SATURDAY,  IDAY=6 AND 1ST SUNDAY IS NOVEMBER 2
                 IF (IDAY. GT. 0) THEN
                    I1SUND=8 - IDAY
                    ELSE
                       I1SUND=1
                    ENDIF
                 IF (IDA.LT.I1SUND) THEN
                    ICKDLS=1
                    ELSE IF (IDA.EQ.I1SUND) THEN
C                 CURRENT DAY IS FIRST SUNDAY IN NOVEMBER, CHECK HOUR
C                 (USE 130AM FOR CUTOFF BECAUSE PERIOD FROM 1AM TO 2AM
C                 OCCURS TWICE)
                    IF (IHM.LT.0130) THEN
                      ICKDLS=1
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
        ENDIF
C =====================================================================

C
30    IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HCKDLS - ICKDLS=',ICKDLS
C
      RETURN
C
      END
