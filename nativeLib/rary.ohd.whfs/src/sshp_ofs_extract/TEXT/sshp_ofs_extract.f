C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (DATIM2)
C$PRAGMA C (UDATL)

      SUBROUTINE SSHP_OFS_EXTRACT_MAIN

C     ...............................................................
C     This program reads the NWSRFS fs5files and extracts the
C     Parameters and Carryover states for all SAC-SMA operations
C     within a given Segment.  It also extracts the Time Series
C     related to this operation.

C     Input: Argument 1 is Segment ID
C            Argument 2 is name of Output file

C     Output: file containing Runoff and Potential Evaporation Time
C             Series, SAC-SMA Parameters & Carryover states

C     Original author: Russell Erb
C     Last edited: 03/03/04
C     ...............................................................

      CHARACTER*120 OUTFILE, LOGFILE
      CHARACTER*8  OPID, OPNAM, SEGID, ROTSID, PETSID
      CHARACTER*4  ROTSDT, PETSDT
      INTEGER      FPOUT, FPLOG

      DIMENSION IDAT(6)

      PARAMETER (MC=3000)
      DIMENSION C(MC)
      PARAMETER (MP=50000)
      DIMENSION P(MP)
      PARAMETER (MT=2000)
      DIMENSION T(MT)
      PARAMETER (MTS=10000)
      DIMENSION TS(MTS)

      COMMON /IONUM/ IN,IPR,IPU
      COMMON /ERRDAT/ IOERR,NWARN,NERRS
      COMMON /HDFLTS/ DX1(3),DFLTNM(2),DX2(16),LCLHCL,LTZHCL,DX3(2)
C                                           *--> FROM COMMON.FCCGD1
      COMMON /FCCGD1/ CGIDC(2),ITDEF(5),NFG,MINDTC,CGNAME(5),ICODAY(20),
     +  ICOTIM(20),LUPDAY(20),LUPTIM(20),IPC(20)
C                                           *--> FROM COMMON.FCTIME
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     +  LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
C                                           *--> FROM CMS(UPDAIO)
C     COMMON UPDAIO CONTAINS I/O UNIT NUMBERS FOR UP... ROUTINES
      COMMON  /UPDAIO/ UR,UW,UTR,UTW,UE,UU,UPRECL,MUPRECL
      INTEGER          UR,UW,UTR,UTW,UE,UU,UPRECL(256)

c ...... Set unit numbers for OFS routines
      IPR    = 97
      IOERR  = IPR
c ...... Set unit numbers for files in this program
      FPLOG  = IPR
      FPOUT  = 98

C ...... Read command line arguments ......

      nargs = iargc()
      if (nargs .ge. 2) then
         call getarg(1, SEGID)
         call getarg(2, OUTFILE)
      else
         write (lbug,*) 'Argument list is less than 2'
         STOP
      endif

C ...... OPEN FILES for Logging and Output .......

      LOGFILE=OUTFILE(1:LENSTR(OUTFILE))//'.log'

      OPEN (UNIT=FPLOG,FILE=LOGFILE,
     + FORM='FORMATTED',IOSTAT=IOSTAT,STATUS='UNKNOWN')

      if (IOSTAT .ne. 0) then
         WRITE (97,*) 'OPEN FPLOG: UNIT=',FPLOG,' FILE=',
     +      LOGFILE(1:LENSTR(LOGFILE)),' IOSTAT=',IOSTAT
      endif

      OPEN (UNIT=FPOUT,FILE=OUTFILE,
     + FORM='FORMATTED',IOSTAT=IOSTAT,STATUS='UNKNOWN')

      if (IOSTAT .ne. 0) then
         WRITE (97,*) 'OPEN FPOUT: UNIT=',FPOUT,' FILE=',
     +      OUTFILE(1:LENSTR(OUTFILE)),' IOSTAT=',IOSTAT
      endif

C ...... CALL TIME/DATE ROUTINE ......
      CALL UDATL (IDAT)
      IMO    = IDAT(3)
      IDAY   = IDAT(4)
      IYEAR  = IDAT(1)
      IHRMIN = IDAT(5)

C ...... Read OFS timing information ......

      CALL UPINIO()
C ...... reset UE variable in commom block UPDAIO
      UE = IPR
      CALL HGETPM (ISTAT)
      if (ISTAT .ne. 0) write(FPOUT,*) 'Status from HGETPM = ',ISTAT

C ...... SET VALUES FOR LOCAL AND NLSTZ IN COMMON BLOCK FCTIME

      LOCAL=LCLHCL
      NLSTZ=LTZHCL

C ...... Read OFS Forecast Component Database Controls ......

      CALL INSTRT()

C ...... Search for Segment by using Segment ID ......

      IOPT=0
      NOPARM=0
      CALL FGETSG (SEGID,IREC,MP,P,MT,T,MTS,TS,IOPT,NOPARM,ISTAT)
      IF (ISTAT .ne. 0) then
         WRITE (FPLOG,*) 'After FGETSG call SEGID=',SEGID,' IREC=',
     1      IREC,'MP=',MP,'MT=',MT,'MTS=',MTS,'IOPT=',IOPT,'NOPARM=',
     2      NOPARM,'ISTAT=',ISTAT
      ENDIF

C ...... Get Operation Number from Operation ID ......

      OPID='SAC-SMA'
      CALL FOPCDE (OPID, NUMOP)

C ...... Search for SAC-SMA Operation(s) in P array returned from FGETSG

      WRITE (FPOUT,900) IMO, IDAY, IYEAR, IHRMIN
900   FORMAT ('Extracted on ',I2.2,'/',I2.2,'/',I4,' ',I4,'Z')

      LOCP=1
33    CALL FSERCH (NUMOP, OPNAM, LOCP, P, MP)

      IF (LOCP .LE. 0) GO TO 34

      WRITE (FPOUT,1000) 'SEGID',SEGID,'OPNAM',OPNAM
1000  FORMAT (A,'=',A,1x,A,'=',A)

c  LOCP is the location in the Segment's Big P array of where this
c  operation's (with operation number NUMOP) information begins.
c  LOCP is the location of position 1 of what is refered to as the
c  Local P (LP) array.

c  Postions 11-12 of the LP array contain the Runoff TS identifier
c  Postions 13 of the LP array contains the Runoff TS data type code

      CALL UMEMOV (P(LOCP-1 + 11), ROTSID, 2)
      CALL UMEMOV (P(LOCP-1 + 13), ROTSDT, 1)
      CALL PrintTS('RO', ROTSID, ROTSDT, FPLOG, FPOUT)


c  Postion 14 of the LP array holds the location of where the Potential
c  Evaporation (PE) time series information is located in the LP array
c  if equal to 0 (zero) means no PE data

      LPEVAP = P(LOCP-1 + 14)
      if (LPEVAP .eq. 0) then
         WRITE (FPOUT,1400)
         WRITE (FPOUT,1401)
         WRITE (FPOUT,1402)
      else
         CALL UMEMOV (P(LOCP-1 + LPEVAP), PETSID ,2)
         CALL UMEMOV (P(LOCP-1 + LPEVAP + 2), PETSDT, 1)
         CALL PrintTS('PE', PETSID, PETSDT, FPLOG, FPOUT)
      endif
1400  FORMAT ('PE Time Series Begin')
1401  FORMAT ('No PE Time Series stored')
1402  FORMAT ('PE Time Series End')


c  Postion 21 of the LP array holds the location of where the Seasonal
c  ET-curve information for this operation are located in the LP array

      LETCRV = P(LOCP-1 + 21)
      WRITE (FPOUT,2100)
      if (LPEVAP .eq. 0) then
         WRITE (FPOUT,2110)
      else
         WRITE (FPOUT,2115)
      endif
      WRITE (FPOUT,2120) (P(LOCP-1 + LETCRV-1 + ICTR),ICTR=1,12)
      WRITE (FPOUT,2130)
2100  FORMAT ('Monthly Values Begin')
2110  FORMAT ('Value Type=ET-Demand')
2115  FORMAT ('Value Type=PE Adjustment')
2120  FORMAT (12(1X,F5.1))
2130  FORMAT ('Monthly Values End')


c  Postion 20 of the LP array holds the location of where the Parameters
c  for this operation are located in the LP array

      LPARMS = P(LOCP-1 + 20)
      LocParmsInP = LOCP-1 + LPARMS
      CALL PrintParams(P, MP, LocParmsInP, FPOUT)


c  The position in the Segment's Big P array JUST BEFORE LOCP holds the
c  pointer of where the carryover for this operation begins in the Big
c  C array.  If this operation has NO carryover the pointer is Zero.

      LocParamsInC = P(LOCP-1)
      if (LocParamsInC .gt. 0) then
         CALL PrintCOParams(SEGID, C, MC, LocParamsInC, FPLOG, FPOUT)
      endif

      IF (LOCP .GT. 0) GO TO 33

34    Continue

cRAE      STOP
      CLOSE(FPOUT)
      CLOSE(FPLOG)
      END

c....................................................................
      subroutine PrintTS(TStype, PETSID, PETSDT, FPLOG, FPOUT)
c....................................................................
      INTEGER      FPLOG, FPOUT, Month, Day, Year, Hour
      INTEGER      Header, BeginJulHr, TimeInt, NumValues
      CHARACTER*8  PETSID, IFTSID
      CHARACTER*4  PETSDT, UnitsType, TimeZ
      CHARACTER*2  TStype

      DIMENSION Header(22)
      PARAMETER (MTSarray=100)
      DIMENSION TSarray(MTSarray)
      PARAMETER (LIWORK=10000)
      DIMENSION IWORK(LIWORK)

      WRITE (FPOUT,7000) TStype

      CALL RPDBCI(ISTAT)

      if (ISTAT .eq. 0) then

         CALL RPRDH (PETSID,PETSDT,1,Header,NUMX,XBUF,IFTSID,ISTAT)

         if (ISTAT .eq. 0) then

            TimeInt = Header(2)
            NumValues = Header(5)
            CALL UMEMOV (Header(11), UnitsType, 1)
            BeginJulHr = Header(14)
            NUMPD = Header(5) / Header(3)

            TimeZ = 'Z   '
            CALL DDGHC2 (BeginJulHr, Year, Month, Day, Hour)
            WRITE(FPOUT,7001) PETSID, PETSDT, TimeInt, UnitsType
            WRITE(FPOUT,7002) Month, Day, Year, Hour, TimeZ, NumValues

            CALL RPRDD (PETSID, PETSDT, BeginJulHr, TimeInt, NUMPD,
     +                 UnitsType, -999.0, MTSarray, TSarray, IFPTR,
     +                 LIWORK, IWORK, ISTAT)

            if (ISTAT .eq. 0) then
                  WRITE(FPOUT,7010) (TSarray(ICTR),ICTR=1,NumValues)
            else
               WRITE(FPOUT,*) 'Return code from RPRDD = ', ISTAT
            endif

         else if (ISTAT .ne. 1) then
           	WRITE(FPLOG,*) 'Error in RPRDH: ISTAT=', ISTAT
         endif

      else

      	WRITE(FPLOG,*) 'Error in RPDBCI: ISTAT=', ISTAT

      endif

      if (ISTAT .ne. 0) WRITE (FPOUT,7030) TStype, PETSID, PETSDT

      WRITE (FPOUT,7031) TStype

7000  FORMAT (A2,' Time Series Begin')
7001  FORMAT ('Identifier=',A,' Data Type=',A,
     +        ' Time Step=',I2, ' Data Units=',A)
7002  FORMAT ('Beginning Time=',I2.2,'/',I2.2,'/',I4,' ',I2,A,
     +        ' Value Count=',I4)
7010  FORMAT ('Values='5(F9.4,1X))
7030  FORMAT ('No ',A2,' time series '
     +        'found for Identifier=',A,' Data Type=',A)
7031  FORMAT (A2,' Time Series End')

      return
      end

c....................................................................
      subroutine PrintParams(P, MP, LocParmsInP, FPOUT)
c....................................................................
      DIMENSION    P(MP)
      INTEGER      FPOUT, IOPTET
      REAL         PXADJ, PEADJ, UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA
      REAL         ZPERC, REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE
      REAL         RSERV, SIDE, EFC

      PXADJ = P(LocParmsInP)
      PEADJ = P(LocParmsInP + 1)
      UZTWM = P(LocParmsInP + 2)
      UZFWM = P(LocParmsInP + 3)
      UZK   = P(LocParmsInP + 4)
      PCTIM = P(LocParmsInP + 5)
      ADIMP = P(LocParmsInP + 6)
      RIVA  = P(LocParmsInP + 7)
      ZPERC = P(LocParmsInP + 8)
      REXP  = P(LocParmsInP + 9)
      LZTWM = P(LocParmsInP + 10)
      LZFSM = P(LocParmsInP + 11)
      LZFPM = P(LocParmsInP + 12)
      LZSK  = P(LocParmsInP + 13)
      LZPK  = P(LocParmsInP + 14)
      PFREE = P(LocParmsInP + 15)
      RSERV = P(LocParmsInP + 16)
      SIDE  = P(LocParmsInP + 17)
      IOPTET= P(LocParmsInP + 18)
      EFC   = P(LocParmsInP + 19)

      WRITE (FPOUT,1000)
      WRITE (FPOUT,1010) 'PXADJ ',PXADJ
      WRITE (FPOUT,1010) 'PEADJ ',PEADJ
      WRITE (FPOUT,1010) 'UZTWM ',UZTWM
      WRITE (FPOUT,1010) 'UZFWM ',UZFWM
      WRITE (FPOUT,1010) 'UZK   ',UZK
      WRITE (FPOUT,1010) 'PCTIM ',PCTIM
      WRITE (FPOUT,1010) 'ADIMP ',ADIMP
      WRITE (FPOUT,1010) 'RIVA  ',RIVA
      WRITE (FPOUT,1010) 'ZPERC ',ZPERC
      WRITE (FPOUT,1010) 'REXP  ',REXP
      WRITE (FPOUT,1010) 'LZTWM ',LZTWM
      WRITE (FPOUT,1010) 'LZFSM ',LZFSM
      WRITE (FPOUT,1010) 'LZFPM ',LZFPM
      WRITE (FPOUT,1010) 'LZSK  ',lZSK
      WRITE (FPOUT,1010) 'LZPK  ',LZPK
      WRITE (FPOUT,1010) 'PFREE ',PFREE
      WRITE (FPOUT,1010) 'RSERV ',RSERV
      WRITE (FPOUT,1010) 'SIDE  ',SIDE
      WRITE (FPOUT,1011) 'IOPTET',IOPTET
      WRITE (FPOUT,1010) 'EFC   ',EFC
      WRITE (FPOUT,1020)

1000  FORMAT ('SAC Params Begin')
1010  FORMAT (A,'=',F8.3)
1011  FORMAT (A,'=',I1)
1020  FORMAT ('SAC Params End')

      return
      end

c.......................................................................
      subroutine PrintCOParams(SEGID, C, MC, LocParamsInC, FPLOG, FPOUT)
c.......................................................................
      CHARACTER*8  SEGID
      DIMENSION    C(MC)
      INTEGER      FPOUT, FPLOG, Month, Day, Year, Hour
      REAL         UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC
      CHARACTER*4  TimeZ

C                                           *--> FROM COMMON.FCCGD
      COMMON /FCCGD/ NSLOTS,NWR,NRSLOT,RFC,NWPS,ICRDAT(5),NCG,
     +  CGIDS(2,25),ICOREC(25),BUFRZZ(28)
C                                           *--> FROM COMMON.FCSEGC
      COMMON /FCSEGC/ IDC(2),ICDAYC(20),ICHRC(20)
C


      WRITE (FPOUT,1000)

C     Get all Carryover Dates for a Carryover Group given a Segment ID
      CALL FCDATE (SEGID, 0)

C     Find the most recent carryover date
      JULIAN = ICDAYC(1)
      IHRP = ICHRC(1)
      DO 20 ICTR=2,NSLOTS
         IF (ICDAYC(ICTR) .GT. JULIAN) THEN
            JULIAN = ICDAYC(ICTR)
            IHRP = ICHRC(ICTR)
         ENDIF
20    CONTINUE

      if (JULIAN .GT. 0) then
         CALL FGETCO (SEGID, JULIAN, IHRP, C, MC, 'ERROR', IERC)

         if (IREC .EQ. 0) then
            UZTWC = C(LocParamsInC)
            UZFWC = C(LocParamsInC + 1)
            LZTWC = C(LocParamsInC + 2)
            LZFSC = C(LocParamsInC + 3)
            LZFPC = C(LocParamsInC + 4)
            ADIMC = C(LocParamsInC + 5)

            TimeZ = 'Z   '
            CALL MDYH2 (JULIAN,IHRP,Month,Day,Year,Hour,ITZ,IDST,TimeZ)

            WRITE (FPOUT,1010) Month, Day, Year, Hour, TimeZ
            WRITE (FPOUT,1020) 'UZTWC',UZTWC
            WRITE (FPOUT,1020) 'UZFWC',UZFWC
            WRITE (FPOUT,1020) 'LZTWC',LZTWC
            WRITE (FPOUT,1020) 'LZFSC',LZFSC
            WRITE (FPOUT,1020) 'LZFPC',LZFPC
            WRITE (FPOUT,1020) 'ADIMC',ADIMC
         else
            WRITE (FPLOG,*) 'Return code from FGETCO=',IERC
         endif
      else
          WRITE (FPLOG,2000) SEGID
      endif

      WRITE (FPOUT,1030)

1000  FORMAT ('SAC State Begin')
1010  FORMAT ('Carryover date= ',I2.2,'/',I2.2,'/',I4,' ',I2,A)
1020  FORMAT (A,'=',F7.1)
1030  FORMAT ('SAC State End')
2000  FORMAT ('No Carryover exisit for segment ',A,'.')

      return
      end
