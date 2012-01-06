C$PRAGMA C (UDATL)
C MODULE HGETTM
C-----------------------------------------------------------------------
C  ROUTINE TO GET CURRENT TIME FOR REQUESTED TIME ZONE CODE
C
C  ARGUMENT LIST:
C
C    NAME     TYPE DIM  I/O DESCRIPTION
C    -------- ---- ---- --- -----------
C    IDATE     I    8    O  DATE ARRAY
C                             IDATE(1)=JULIAN HOUR
C                             IDATE(2)=MONTH
C                             IDATE(3)=DAY
C                             IDATE(4)=YEAR (2 DIGIT)
C                             IDATE(5)=HOUR OF DAY FOR ITZC
C                             IDATE(6)=MINUTES
C                             IDATE(7)=SECONDS
C                             IDATE(8)=TIME ZONE CODE
C    ITZC      I    1   I/O  REQUESTED TIME ZONE CODE (EG - CDT OR Z)
C                            (THE REQUEST OF ITZC='SYS ' RETURNS
C                             THE CURRENT TIME IN THE TIME ZONE THAT THE
C                             SYSTEM CLOCK IS SET FOR).
C-----------------------------------------------------------------------
      SUBROUTINE HGETTM (IDATE,ITZC)

      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      COMMON /FCTIME/ IDUM1(11),KLOCAL,IDUM2(2),KNLSTZ,IDUM3(5)

      INTEGER   ZONES(6),ICLKTZ(2,6)
      INTEGER   IDATE(8),ICDATE(6),IUDATE(6)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgettm.f,v $
     . $',                                                             '
     .$Id: hgettm.f,v 1.4 2002/02/11 15:30:23 michaelo Exp $
     . $' /
C    ===================================================================
C

      DATA LSYS   / 4hSYS  /
      DATA ZONES  / 4hZ   ,4hE   ,4hC   ,4hM   ,4hP   ,4hA    /
      DATA ICLKTZ / 4hZ   ,4hZ   ,4hEST ,4hEDT ,4hCST ,4hCDT ,
     $              4hMST ,4hMDT ,4hPST ,4hPDT ,4hAST ,4hADT   /


      NZONES=6

      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HGETTM'

C  SET LOCAL AND NLSTZ IN COMMON FCTIME SO JULDA AND MDYH2 CAN BE USED
      KLOCAL=LOCAL
      KNLSTZ=NLSTZ

C  CHECK WHAT TIME ZONE THE SYSTEM CLOCK IS SET FOR
      NCHAR=1
      DO 10 IZ=1,NZONES
         CALL UCMPAR (CLKZON,ZONES(IZ),NCHAR,IMATCH)
         IF (IMATCH.EQ.0) GO TO 20
10       CONTINUE

C  VALID TIME ZONE NOT FOUND
      IZ=2

C  GET CURRENT CLOCK TIME
20    CALL UDATL (ICDATE)

C   SET MONTH, DAY, HOUR/MINUTE AND SECONDS
      NMON=ICDATE(3)
      NDAY=ICDATE(4)
      NHRMIN=ICDATE(5)
      NHR=NHRMIN/100
      NMIN=NHRMIN-NHR*100
      NSEC=ICDATE(6)/100
      NYR=ICDATE(1)
CY2      NYR=MOD(ICDATE(1),100)

C  CHECK IF IN DAYLIGHT SAVINGS TIME
      IUDATL=1
      CALL HCKDLS (IUDATL,IUDATE,ICKDLS)
      ISW=ICKDLS+1

C  GET JULIAN DAY AND INTERNAL HOUR (HOUR IN HYDROLOGIC DAY) USING
C  SYSTEM CLOCK TIME ZONE CODE AND ACCOUNTING FOR DAYLIGHT SAVINGS
      JCLKTZ = ICLKTZ(ISW,IZ)
      CALL FCITZC (JTZ,JDSAV,JCLKTZ)
      CALL JULDA (JULDAT,INTHR,NMON,NDAY,NYR,NHR,JTZ,JDSAV,JCLKTZ)

C  CHANGE REQUESTED TZC TO THE CLOCK'S TZC IF REQUESTED TZC WAS 'SYS'
      IF (ITZC.EQ.LSYS) ITZC=JCLKTZ

C  CONVERT JULIAN DAY AND HOUR TO HOUR, MONTH, DAY AND YEAR
      CALL MDYH2 (JULDAT,INTHR,NMON,NDAY,NYR,NHR,JTZ,JDSAV,ITZC)

C  FILL IDATE ARRAY
      IDATE(1)=(JULDAT-1)*24+INTHR
      IDATE(2)=NMON
      IDATE(3)=NDAY
      IDATE(4)=NYR
CY2      IDATE(4)=MOD(NYR,100)
      IDATE(5)=NHR
      IDATE(6)=NMIN
      IDATE(7)=NSEC
      IDATE(8)=ITZC

      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*)
     *      ' IDATE(1)=',IDATE(1),
     *      ' IDATE(2)=',IDATE(2),
     *      ' IDATE(3)=',IDATE(3),
     *      ' IDATE(4)=',IDATE(4),
     *      ' IDATE(5)=',IDATE(5),
     *      ' IDATE(6)=',IDATE(6),
     *      ' IDATE(7)=',IDATE(7),
     *      ' '
         WRITE (IOGDB,'(A,A4)') ' IDATE(8)=',IDATE(8)
         ENDIF

      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HGETTM'

      RETURN
      END
