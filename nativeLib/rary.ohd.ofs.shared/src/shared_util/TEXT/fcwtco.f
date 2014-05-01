C MEMBER FCWTCO
C  (from old member FCFCWTCO)
C
C.......................................................................
C                             LAST UPDATE: 01/06/94.10:17:02 BY $WC21DT
C
C
C  SUBROUTINE WRITECO WRITES CARRYOVER TO THE TEMPORARY SEQUENTIAL
C   FILES.
C
C.......................................................................
C
      SUBROUTINE FCWTCO(JD,JHR,CO,NCO)
C
C.......................................................................
C
C  INPUT:
C        JD - JULIAN DAY TO SAVE CARRYYOVER
C       JHR - HOUR OF DAY TO SAVE CARRYOVER
C        CO - THAT PORTION OF THE C ARRAY TO BE WRITTEN TO THE FILE.
C       NCO - LENGTH OF ARRAY CO.
C
C.......................................................................
C
C      SUBROUTINE ORIGINALLY WRITTEN BY --
C         JOE OSTROWSKI - HRL - 791026
C
C.......................................................................
C
      INCLUDE 'common/fcio'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fc'
      INCLUDE 'common/fcoppt'
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
C
      DIMENSION CO(NCO),OLDSUB(2),IFILE(20),SUBRNM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fcwtco.f,v $
     . $',                                                             '
     .$Id: fcwtco.f,v 1.2 2001/06/13 10:36:16 mgm Exp $
     . $' /
C    ===================================================================
C
      DATA COTR,COBG/4HCOTR,4HCOBG/
      DATA SUBRNM/4HFCWT,4HCO  /
C
C   ** SET UP DEBUG INFO
C
      OLDSUB(1) = OPNAME(1)
      OLDSUB(2) = OPNAME(2)
C
      OPNAME(1)=SUBRNM(1)
      OPNAME(2)=SUBRNM(2)
      IOLDOP=IOPNUM
      IOPNUM=0
      NWST=NWARN
      NEST=NERROR
C
      IBUG=0
      IF (IFBUG(COTR).EQ.1) IBUG=1
      IF (IFBUG(COBG).EQ.1) IBUG=2
C
      IF (IBUG.GE.1) WRITE(IODBUG,600)
  600 FORMAT(22H  *** ENTER FCWTCO ***)
C
      IF (IBUG.GE.2) WRITE(IODBUG,601) IDSEGN,IDSEG
  601 FORMAT(20H  IDSEGN(/FCSEGN/)= ,2A4,16H IDSEG(/FCIO/)= ,2A4)
C
C.......................................................................
C
C    ** IF SEG. ID ON /FCIO/ IS NOT THE SAME AS THE SEG. ID ON /FCSEGN/,
C         THIS IS A NEW SEGMENT, AND A SEGMENT HEADER MUST BE WRITTEN.
C
C        OTHERWISE, GO TO ACTUAL CARRYOVER WRITE SECTION.(STATEMENT 50)
C
C.......................................................................
C
C..........................................
C
C  CHECK TO SEE IF BOTH IFILLC AND NCSTOR ARE GT ZERO, AS THEY SHOULD BE
C   IF NOT, STOP WRITING CO.
C
C..........................................
C
      IF ((NCSTOR*IFILLC).GT.0) GO TO 5
C
      WRITE(IPR,1606) NCSTOR,IFILLC
 1606 FORMAT(10X,02H**,35H ERROR - FCWTCO CALLED WITH NCSTOR=,I3,04HAND,
     .,   08H IFILLC=,I3/14X,41H  BOTH VALUES SHOULD BE GREATER THAN ZER
     .O)
C
      CALL ERROR
      GO TO 9999
C
    5 DO 10 I=1,2
      IF (IDSEG(I).NE.IDSEGN(I)) GO TO 20
   10 CONTINUE
C
      GO TO 50
C
   20 CONTINUE
      NFAIL=0
C
C......................................................................
C
C     ** CHECK TO INSURE PREVIOUS CARRYOVER RUNS RAN SUCCESSFULLY.
C       I.E. - NO.OF CO VALUES (NCVALS) = NO. OF VALUES WRITTEN(NVALSW).
C
C.......................................................................
C
      NNCV=NCVALS-1
C
      DO 30 I=1,NCSTOR
C
      IF (IBUG.GE.2) WRITE(IODBUG,604) I,NVALSW(I),NNCV
  604 FORMAT(42H   NO. OF VALUES ON TEMP FILE FOR DATE NO.,I3,
     .  02H =,I4,15H, NO. SHOULD BE,I4)
C
      IF (NVALSW(I).EQ.NNCV) GO TO 30
C
      NFAIL=NFAIL+1
       IFILE(NFAIL)=I
C
   30 CONTINUE
C
      IF (NFAIL.GT.0) GO TO 40
      GO TO 45
C
   40 CONTINUE
C
      I = IFILE(1)
      WRITE (IPR,1600) IDSEG(1),IDSEG(2),NNCV,NVALSW(I)
 1600 FORMAT(10X,13H*** ERROR ***,13H FOR SEGMENT ,2A4,14H AN INCORRECT
     *  ,57HNUMBER OF CARRYOVER VALUES WERE WRITTEN TO THE TEMPORARY ,
     *  11HSAVE FILES./
     *  24X,27HCORRECT NUMBER OF VALUES = ,I3,
     *      34HACTUAL NUMBER OF VALUES WRITTEN = ,I3,1H.)
      WRITE(IPR,1602)
C
      CALL ERROR
      GO TO 9998
C
   45 NCVALS=NC
C
      DO 42 I=1,2
   42 IDSEG(I) = IDSEGN(I)
C
      DO 48 I=1,NCSTOR
      NVALSW(I)=0
      IU=ICFNUM(I)
C
C......................................................................
C
C     ** WRITE SEGMENT HEADER TO TEMPORARY FILES.
C
C......................................................................
C
C
      IF (IBUG.GE.2) WRITE(IODBUG,605)IU,IDSEG,IWOCRY,ICDAY(I),ICHOUR(I)
     . ,NC,NCOPS
  605 FORMAT(08H   UNIT=,I3,12H SEGMENT ID=,1X,2A4,13H WORD OFFSET=,I5,
     .05H DAY=,I6,06H HOUR=,I3/,31H   NO. OF VALUES TO BE WRITTEN=,I4,
     .       31HNO. OF OPERATIONS W/ CARRYOVER=,I3)
C
      WRITE (IU) IDSEG,IWOCRY,ICDAY(I),ICHOUR(I),NC,NCOPS
   48 CONTINUE
C
   50 CONTINUE
C
      DO 60 J=1,NCSTOR
C
      IF (JD.NE.ICDAY(J).OR.JHR.NE.ICHOUR(J)) GO TO 60
      NTEMP=J
      GO TO 70
C
   60 CONTINUE
C
       WRITE (IPR,1601) JD,JHR,(ICDAY(I),ICHOUR(I),I=1,NCSTOR)
 1601 FORMAT(10X,13H*** ERROR ***,36HTHE REQUESTED SAVE DAY AND TIME, --
     *  ,2I10,46H -- COULD NOT BE FOUND ON THE TEMPORARY FILES.//
     *   49HTHE FOLLOWING TIMES EXIST ON THE TEMPORARY FILES://
     *    ,(7X,I10,17X,I10/))
      WRITE (IPR,1602)
 1602 FORMAT(/48H *** NO CARRYOVER WILL BE SAVED FOR THIS RUN ***)
      CALL ERROR
      GO TO 9998
C
   70 IF (ICPTR.GE.6) GO TO 80
C
      WRITE (IPR,1603) ICPTR
 1603 FORMAT(10X,13H*** ERROR ***,29HTHE POINTER TO THE C ARRAY --,I5,
     * 19H -- IS LESS THAN 6.)
      WRITE (IPR,1602)
      CALL ERROR
      GO TO 9998
C
   80 CONTINUE
      IF (ICPTR.LE.MC) GO TO 90
C
      WRITE (IPR,1604) ICPTR,MC
 1604 FORMAT(10X,13H*** ERROR ***,29HTHE POINTER TO THE C ARRAY --,
     *  I5,31H -- IS BEYOND THE END POINT, --,I5,01H.)
      WRITE (IPR,1602)
      CALL ERROR
      GO TO 9998
C
   90 IF ((ICPTR+NCO-1).LE.MC) GO TO 100
C
      NREM=MC-ICPTR+1
      WRITE (IPR,1605) NCO,NREM
 1605 FORMAT(10X,13H*** ERROR*** ,I6,40H RECORDS NEEDED TO STORE CARRYOV
     *ER. ONLY,I6,23H RECORDS ARE AVAILABLE.)
      WRITE (IPR,1602)
      CALL ERROR
      GO TO 9998
C
C.......................................................................
C
C  WRITE CARRYOVER RECORD.
C
C     NCPT1 - LOCATION OF START OF CARRYOVER RECORD IN C ARRAY.
C     NCPT2 - LOCATION OF END OF CARRYOVER RECORD IN C ARRAY.
C
C.......................................................................
C
  100 NCPT1=ICPTR-5
      NCPT2=ICPTR+NCO-1
      NCPTE=NCPT1+4
      IU=ICFNUM(NTEMP)
C
      IF (IBUG.GE.2) WRITE(IODBUG,606) IU,NCPT1,NCPT2
  606 FORMAT(08H   UNIT=,I4,13H FIRST WRITE=,I5,12H LAST WRITE=,I5)
C
C
      WRITE (IU) NCPT1,NCPT2,(C(I),I=NCPT1,NCPTE),(CO(J),J=1,NCO)
      NVALSW(NTEMP)=NVALSW(NTEMP)+NCO+5
      GO TO 9999
C
 9998 NCSTOR=0
C
 9999 CONTINUE
C
      IF (IBUG.GE.1) WRITE(IODBUG,620)
  620 FORMAT(22H   *** EXIT FCWTCO ***)
C
      OPNAME(1) = OLDSUB(1)
      OPNAME(2) = OLDSUB(2)
      IOPNUM = IOLDOP
C
      RETURN
      END
