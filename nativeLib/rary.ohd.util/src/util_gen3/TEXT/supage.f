C$PRAGMA C (DATIM2)
C MODULE SUPAGE
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT NEW PAGE HEADING.
C
      SUBROUTINE SUPAGE
C
      CHARACTER*8 USERID
      CHARACTER*28 IDATE1
      CHARACTER*20 IDATE2
C
      INCLUDE 'uiox'
      INCLUDE 'upvrsx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/supage.f,v $
     . $',                                                             '
     .$Id: supage.f,v 1.4 2002/02/11 14:08:49 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      LDEBUG=0
      NUMERR=0
C
C  CHECK IF NO PAGES HAVE BEEN PRINTED
      IF (NPSPAG.EQ.0) GO TO 10
C
C  CHECK IF ONLY HEADING HAS BEEN PRINTED
      IF (NPSNLN.EQ.3) GO TO 40
C
10    NPSPAG=NPSPAG+1
      NPSNLN=0
C
C  PRINTOUT FOR SPECIFIED VALUES OF IOPTLE:
C     IOPTLE     PROGRAM NAME   USER NAME   RUN DATE   PAGE NUMBER
C     ------     ------------   ---------   --------   -----------
C        0
C        1                                                  X
C        2             X            X           X           X
C        3             X            X           X
C
C  CHECK IF NO TITLE LINE TO BE PRINTED
      IF (IOPTLE.EQ.0) THEN
         WRITE (LP,50)
         NPSNLN=1
         GO TO 30
         ENDIF
C
C  CHECK IF ONLY PAGE NUMBER TO BE PRINTED
      IF (IOPTLE.EQ.1) THEN
         WRITE (LP,60) NPSPAG
         GO TO 20
         ENDIF
C
C  GET AND MODIFY CURRENT DATE AND TIME
      CALL DATIM2 (IDATE1)
      CALL SUBSTR (IDATE1,2,20,IDATE2,1)
      IDATE2(14:14)='@'
      IF (IDATE2(16:16).EQ.' ') IDATE2(16:16)='0'
      IF (IDATE2(17:17).EQ.' ') IDATE2(17:17)='0'
      IDATE2(18:18)='.'
      IF (IDATE2(19:19).EQ.' ') IDATE2(19:19)='0'
      IF (IDATE2(20:20).EQ.' ') IDATE2(20:20)='0'
C
C  PRINT ENTIRE TITLE LINE
      IF (IOPTLE.EQ.2) THEN
         CALL SUPAGE2 (USERID)
         WRITE (LP,70) PGMNAM,PGMVRN,PGMVRD,USERID,IDATE2,NPSPAG
         GO TO 20
         ENDIF
C
C  PRINT ENTIRE TITLE LINE EXECPT PAGE NUMBER
      IF (IOPTLE.EQ.3) THEN
         CALL SUPAGE2 (USERID)
         WRITE (LP,80) PGMNAM,PGMVRN,PGMVRD,USERID,IDATE2
         GO TO 20
         ENDIF
C
C  INVALID TITLE OPTION
      WRITE (LP,50)
      CALL SULINE (LP,1)
      WRITE (LP,100) IOPTLE
      CALL SUERRS (LP,2,NUMERR)
      IOPTLE=0
      GO TO 30
C
20    WRITE (LP,90)
      NPSNLN=3
C
C  SET INDICATOR NEW PAGE HAS BEEN PRINTED
30    IPSNWP=1
C
40    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('1')
60    FORMAT ('1',T125,'PAGE=',I4)
70    FORMAT ('1NWSRFS FORECAST SYSTEM - PROGRAM ',A,1X,
     *     '(VERSION: ',A,' - ',A,')',3X,'USER=',A,3X,
     *     'DATE=',A,T125,'PAGE=',I4)
80    FORMAT ('1NWSRFS FORECAST SYSTEM - PROGRAM ',A,2X,
     *     '(VERSION: ',A,' - ',A,')',3X,'USER=',A,3X,
     *     'DATE=',A)
90    FORMAT ('0')
100   FORMAT ('0*** ERROR - IN SUPAGE - ',I2,' IS AN INVALID TITLE ',
     *   'OPTION VALUE. TITLE OPTION SET TO 0.')
C
      END
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      SUBROUTINE SUPAGE2 (USERID)

C  SET USER NAME - NEED TO DO IN SEPARATE ROUTINE BECAUSE VARIABLE
C  NPSPAG IS IN COMMON UPAGEX AND SUPAGX
C
      CHARACTER*8 USERID
      INCLUDE 'upagex'
C
      USERID=PUSRID
C
      RETURN
C
      END
