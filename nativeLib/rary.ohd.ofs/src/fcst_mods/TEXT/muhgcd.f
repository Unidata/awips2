C MODULE MUHGCD
C-----------------------------------------------------------------------
C
C  THIS SUBROUTINE PERFORMS THE UHGCDATE MOD.
C    AV modified 4/15/04
C-----------------------------------------------------------------------
      SUBROUTINE MUHGCD (MP,P,NCARDS,MODCRD,IDATE,ldate,kdate,iuhgd)
C
      CHARACTER*8 RTNNAM,OLDOPN,OPNAME,STRNG,tmpopn
      CHARACTER*80 MODCRD(NCARDS)
C
      PARAMETER (MVALS=100)
      DIMENSION VALUES(MVALS)
      DIMENSION P(MP)
C
      INCLUDE 'ufreex'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/fmodft'
      INCLUDE 'common/moduhg'
ccav      COMMON /MODUHG/ MDTUHG,NDTUHG,IOPUHG(2,20),JDTUHG(3,20),
ccav     *   MVLUHG,NVLUHG(20),UHGVAL(20,100), UHGOPN(20)

ckwz. tmpLDate is for save the ldate for UHGCDATE
      real tmpIDate,tmpLDate
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/muhgcd.f,v $
     . $',                                                             '
     .$Id: muhgcd.f,v 1.5 2004/11/18 18:26:35 hank Exp $
     . $' /
C    ===================================================================
C
C
      
ckwz  IDATE,KDATE,LDATE should keep as origin
cew    idate is start of mod
cew    kdate is valid of mod
cew    ldate is end of mod

      tmpIDate=IDATE
      tmpLDate=LDATE

      RTNNAM='MUHGCD'
      IOPNUM=0
      tmpopn = ' '
     
      CALL FSTWHR (RTNNAM,IOPNUM,OLDOPN,IOLDOP)
C
      IBUG=IFBUG('MODS')+IFBUG('UHGCD')
C
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      ISTRUN=(IDARUN-1)*24+IHRRUN
      IENDRN=(LDARUN-1)*24+LHRRUN
C
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'IDATE=',IDATE,' ISTRUN=',ISTRUN,
     *   ' LDATE=',LDATE,' IENDRN=',IENDRN
C
C  CHECK IF THE DATE IS THE SAME AS LSTCMPDY VALUE
      ICOMP=((LDACPD-1)*24)+LHRCPD
C
C  GET DATES OF BEGINNING AND END OF RUN PERIOD
      CALL MDYH2 (IDARUN,IHRRUN,IM1,ID1,IY1,IH1,DUM1,DUM2,MODTZC)
      CALL MDYH2 (LDARUN,LHRRUN,IM2,ID2,IY2,IH2,DUM1,DUM2,MODTZC)
C
C  GET DATES OF MOD
      IXDA=IDATE/24+1
      IXHR=IDATE-(IXDA-1)*24
      IF (IXHR.EQ.0) IXDA=IXDA-1
      IF (IXHR.EQ.0) IXHR=24
      CALL MDYH2 (IXDA,IXHR,IM3,ID3,IY3,IH3,DUM1,DUM2,MODTZC)
      LXDA=LDATE/24+1
      LXHR=LDATE-(LXDA-1)*24
      IF (LXHR.EQ.0) LXDA=LXDA-1
      IF (LXHR.EQ.0) LXHR=24
      CALL MDYH2 (LXDA,LXHR,IM4,ID4,IY4,IH4,DUM1,DUM2,MODTZC)
C
C  CHECK IF ALL OF CURRENT OBSERVED DATA PERIOD IS WITHIN PERIOD ENTERED
cav  EW direction: do not check for all valid dates. Only check if the valid date is 
cac starting before the mod run
cew    idate is tart of mod
cew    kdate is end date of mod
cew    ldate is valid date of mod
      IF (LDATE.LT.ISTRUN) GO TO 10   ! is end date less than start run
      IF (IDATE.GT.IENDRN) GO TO 10   ! is mod start after end run
      IF (IDATE.GT.LDATE) GO TO 10    ! is start of mod after end of mod
      IF (IDATE.LT.ISTRUN) then       ! is start of mod before start run
cav do not print out the warning as ew's opinion     
cav        WRITE (IPR,4)IM3,ID3,IY3,IH3,IM1,ID1,IY1,IH1
cav 4      FORMAT ('0**WARNING** IN UHGCDATE MOD, START DATE CHANGED FROM ',
cav     1           I2.2,I2.2,I4,I2.2,'Z TO ',I2.2,I2.2,I4,I2.2,'Z.')
        IDATE=ISTRUN
      endif
      IF (LDATE.GT.IENDRN) then     ! is end mod is after end of run
cav       
cav        WRITE (IPR,5)IM4,ID4,IY4,IH4,IM2,ID2,IY2,IH2
cav 5      FORMAT ('0**WARNING** IN UHGCDATE MOD, END DATE CHANGED FROM ',
cav     1           I2.2,I2.2,I4,I2.2,'Z TO ',I2.2,I2.2,I4,I2.2,'Z.')
	    LDATE=IENDRN
      endif
      
cew    is valid date <  lstcmpdy and validdate <= endmod
      IF (KDATE.LT.ICOMP.AND.KDATE.LE.LDATE) LDATE=KDATE 
       
C
C  ALL OF OBSERVED DATA PERIOD IS WITHIN PERIOD BEING CHANGED
cew      is validdate < startrun
        IF(KDATE.LT.ISTRUN) go to 25
        GO TO 30
C
10      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,20) IM3,ID3,IY3,IH3,MODTZC,
     *     IM4,ID4,IY4,IH4,MODTZC,
     *     IM1,ID1,IY1,IH1,MODTZC,
     *     IM2,ID2,IY2,IH2,MODTZC,
     *     MODCRD(NRDCRD)
20    FORMAT ('0**WARNING** THE DATES FOR CHANGES IN THE ',
     * 'UHGCDATE MOD (',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,' TO ',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,')' /
     * 13X,'DO NOT FALL WITHIN (',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,' TO ',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,'). ' /
     * 13X,'THE FOLLOWING MOD CARD WILL BE IGNORED:' /
     * 13X,A)
       END IF 
      go to 390
25     IF (MODWRN.EQ.1) THEN
         WRITE (IPR,28) MODCRD(NRDCRD)
28    FORMAT ('0**WARNING** IN UHGCDATE MOD - VALID DATE IS BEFORE ',
     * 'START RUN THIS MOD CARD WILL BE IGNORED:' /
     * 13X,A )
        CALL WARN
      ENDIF
      GO TO 390
C
30    IFIRST=1
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'NCARDS=',NCARDS
      IF (NRDCRD.EQ.NCARDS) then 
         IF (MODWRN.EQ.1) THEN
            WRITE (IPR,60) MODCRD(NRDCRD)
            CALL WARN
         endif
         GO TO 390
      ENDIF
C
40    IF (NRDCRD.EQ.NCARDS) GO TO 390
      NOPERS=0
C
      OPNAME=' '
C
      IF (IBUG.GT.0) WRITE (IODBUG,'(1X,A,I3,2A)') 'NRDCRD=',NRDCRD,
     *   ' MODCRD(NRDCRD)=',MODCRD(NRDCRD)
C
C  READ NEXT CARD AND CHECK FOR COMMAND
      ISCMD=MISCMD(NCARDS,MODCRD)
      IF (ISCMD.NE.0) THEN
         IF (IFIRST.EQ.1) THEN
C        HAVE FOUND COMMAND AS FIRST SUBSEQUENT CARD
50          IF (MODWRN.EQ.1) THEN
            WRITE (IPR,60) MODCRD(NRDCRD)
60    FORMAT ('0**WARNING** NO SUBSEQUENT CARDS FOUND FOR THE ',
     *   'FOLLOWING MOD:' /
     * 13X,A)
             CALL WARN
             ENDIF
          ENDIF
          GO TO 390
      ENDIF
C
      IFIRST=0
C
C  READ VALUES
      NFLD=1
      NRDCRD=NRDCRD+1
      CALL MRDVAL (NCARDS,MODCRD,NFLD,MVALS,NVALS,VALUES,ISTAT)
      IF (IBUG.GT.0) THEN
         WRITE (IODBUG,*) 'ISTAT=',ISTAT,' IFIRST=',IFIRST,
     *      ' NVALS=',NVALS
         IF (NVALS.GT.0.AND.ISTAT.NE.-1) WRITE (IODBUG,80)
     *     (VALUES(I),I=1,NVALS)
80    FORMAT (' VALUES=',(10G10.2,1X))
         ENDIF
C
C  ISTAT RETURNED FROM MRDVAL MEANS:
C     0 = VALUES READ OK, NO ADDITIONAL FIELDS ON CARD
C     2 = VALUES READ OK, ADDITIONAL FIELDS ON CARD
C    -1 = NO VALUES ENTERED
C  ELSE = TOO MANY VALUES ENTERED
      IF (ISTAT.EQ.0) GO TO 170
      IF (ISTAT.EQ.2) GO TO 120
      IF (ISTAT.EQ.-1) GO TO 100
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,90) MVALS,MVALS,MODCRD(NRDCRD)
90    FORMAT ('0**WARNING** IN UHGCDATE MOD - MORE THAN ',I3,
     *     ' VALUES ENTERED.' /
     * 13X,'THE FIRST ',I3,' ORDINATES ENTERED WILL BE USED, ',
     *     'THE REMAINING ORDINATES WILL BE SET TO ZERO.' /
     * 13X,'CURRENT MOD CARD IS:' /
     * 13X,A)
         CALL WARN
         ENDIF
      GO TO 170
C
100   IF (MODWRN.EQ.1) THEN
         WRITE (IPR,110) MODCRD(NRDCRD)
110   FORMAT ('0**WARNING** ',
     *     'NO VALUES ENTERED ON A SUBSEQUENT CARD FOR UHGCDATE MOD' /
     * 13X,'THE FOLLOWING MOD CARD WILL BE IGNORED:' /
     * 13X,A)
         CALL WARN
         ENDIF
      GO TO 40
C
C  HAVE ADDITIONAL FIELDS - REPROCESS CURRENT FIELD TO SEE IF A SLASH
120   IONEOP=1
      ISTRT=-1
      NCHAR=-LEN(STRNG)
      ICKDAT=0
      CALL UFIEL2 (NCARDS,MODCRD,NFLD,ISTRT,LENGTH,ITYPE,NREP,
     *   INTGER,REAL,NCHAR,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,
     *   ISTAT)
      IF (STRNG.NE.'/') THEN
         IF (MODWRN.EQ.1) THEN
            WRITE (IPR,130) MODCRD(NRDCRD)
130   FORMAT ('0**WARNING** IN UHGCDATE MOD - A SLASH ',
     *     'WAS NOT FOUND BEFORE THE OPERATION NAME.' /
     * 13X,'THE FOLLOWING MOD CARD WILL BE IGNORED:' /
     * 13X,A)
            CALL WARN
            ENDIF
         GO TO 40
         ENDIF
C
C  GET OPERATION NAME
      ISTRT=-3
      NCHAR=-LEN(OPNAME)
      ICKDAT=0
      CALL UFIEL2 (NCARDS,MODCRD,NFLD,ISTRT,LENGTH,ITYPE,NREP,
     *   INTGER,REAL,NCHAR,OPNAME,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,
     *   ISTAT)
      IF (ISTRT.EQ.-2) THEN
         IF (MODWRN.EQ.1) THEN
            WRITE (IPR,140) MODCRD(NRDCRD)
140   FORMAT ('0**WARNING** NO OPERATION NAME ENTERED ',
     *     'AFTER THE SLASH.' /
     * 13X,'THE FOLLOWING MOD CARD WILL BE IGNORED:' /
     * 13X,A)
            CALL WARN
            GO TO 40
            ENDIF
         ENDIF
C
C  CHECK THAT OPERATION NAME IS IN THE SEGMENT
      LOCOPN=0
      NUMOP=2
      CALL FSERCH (NUMOP,OPNAME,LOCOPN,P,MP)
      
      IF (IBUG.GT.0) WRITE (IODBUG,*) ' OPNAME=',OPNAME,
     *   ' IONEOP=',IONEOP,' LOCOPN=',LOCOPN
      IF (LOCOPN.EQ.0) THEN
         IF (MODWRN.EQ.1) THEN
            WRITE (IPR,160) OPNAME
160   FORMAT ('0**WARNING** IN UHGCDATE MOD - ',
     * 'UNIT-HG OPERATION FOR OPERATION NAME ',A,
     *    ' NOT FOUND IN THIS SEGMENT.')
            CALL WARN
            GO TO 40
            ENDIF
         ENDIF
         tmpopn=OPNAME 
      GO TO 200
C
C  FIND START OF SECOND PORTION OF P ARRAY FOR THE OPERATION
170   LOCOPN=1
      IONEOP=0
180   NUMOP=2
      CALL FSERCH (NUMOP,OPNAME,LOCOPN,P,MP)
      IF (IBUG.GT.0) WRITE (IODBUG,*) ' OPNAME=',OPNAME,
     *   ' IONEOP=',IONEOP,' LOCOPN=',LOCOPN,' NOPERS=',NOPERS
      IF (LOCOPN.EQ.0) THEN
C     NO MORE UNIT-HYDROGRAPH OPERATIONS IN THIS SEGMENT
         IF (NOPERS.EQ.0) THEN
            IF (MODWRN.EQ.1) THEN
               WRITE (IPR,190)
190   FORMAT ('0**WARNING** IN UHGCDATE MOD - NO ',
     *   'UNIT-HYDROGRAPH OPERATIONS FOUND IN THIS SEGMENT.')
               CALL WARN
               ENDIF
            ENDIF
         GO TO 40
	 ENDIF
        
C
C  CHECK FOR NEGATIVE UNIT-HYDROGRAPH ORDINATES
200   INEG=0
      DO 220 I=1,NVALS
         IF (VALUES(I).GE.0.0) GO TO 220
         IF (MODWRN.EQ.1) THEN
            WRITE (IPR,210) I
210   FORMAT ('0**WARNING** IN UHGCDATE MOD - ',
     *   'A NEGATIVE NUMBER WAS ENTERED AS ORDINATE NUMBER ',I3,'. ',
     *   'UNIT-HYDROGRAPH WILL NOT BE CHANGED.')
            CALL WARN
            INEG=1
            ENDIF
220      CONTINUE
      IF (INEG.EQ.1) THEN
         WRITE (IPR,230) MODCRD(NRDCRD)
230   FORMAT (
     * 13X,'THE FOLLOWING MOD CARD WILL BE IGNORED:' /
     * 13X,A)
         GO TO 40
         ENDIF
C
C  NUMBER OF UNIT-HG ORDINATES IS IN POSITION 10 OF P ARRAY
C  ORDINATES START IN POSITION 27
      LOCP=LOCOPN-1
      NOPERS=NOPERS+1
      NORDS=P(LOCP+10)
      LORDS=LOCP+27-1

C
      IF (IBUG.GT.0) THEN
         WRITE (IODBUG,'(1X,A)') 'OPNAME=',OPNAME
         WRITE (IODBUG,*) 'IDATE=',IDATE,' LDATE=',LDATE,
     *      ' NVALS=',NVALS,' NORDS=',NORDS
         IF (NVALS.GT.0) WRITE (IODBUG,240) (VALUES(I),I=1,NVALS)
240   FORMAT (' VALUES=',(1X,10G9.2))
         IF (NORDS.GT.0) WRITE (IODBUG,250) (P(LORDS+I),I=1,NORDS)
250   FORMAT (' CURRENT UNIT HYDROGRAPH ORDINATES=',(1X,10G9.2))
         ENDIF
C
      
      IF (NVALS.LT.MVALS) THEN
C     SET END OF VALUES ARRAY TO ZERO
         NSET=MVALS-NVALS
         DO 260 I=1,NSET
            VALUES(NVALS+I)=0.0
260         CONTINUE
      ENDIF
cc calculate area for base uhg      
      SUM=0.0
      
      DO 270 I=1,NORDS      
270   SUM=SUM+P(LORDS+I)
C
C     SUM CURRENT UNIT-HG ORDINATES
C         
C
C  CHECK IF NEW UNIT-HG WILL FIT IN P ARRAY
      IF (NVALS.GT.NORDS) THEN
         IF (MODWRN.EQ.1) THEN
            WRITE (IPR,280) NVALS,NORDS,NORDS
280   FORMAT ('0**WARNING** IN UHGCDATE MOD - ',I3,
     *     ' UNIT-HG ORDINATES WERE ENTERED. THERE IS ROOM IN ',
     *     'THE PARAMETER ARRAY FOR ONLY ',I3,'.' /
     * 13X,'THE FIRST ',I3,' UNIT-HG ORDINATES ENTERED WILL BE USED.')
            CALL WARN
            ENDIF
         NVALS=NORDS

      else
C
C     SUM ENTERED ORDINATES
C
        SUMNEW=0.0
        DO 290 I=1,NVALS
290       SUMNEW=SUMNEW+VALUES(I)
      endif 
C
C     RESCALE NEW VALUES IF NEEDED
C
cav      IF(SUM.EQ.SUMNEW)GO TO 440
C
      RATIO=0.
      IF(SUMNEW.NE.0.0)RATIO=SUM/SUMNEW

      IF(IBUG.GT.0)WRITE(IODBUG,292)RATIO,SUM,SUMNEW
292   FORMAT(11X,'RESCALING ORDINATES - RATIO= ',G10.3,
     1 ', SUM= ',G10.2,', SUMNEW= ',G10.2)

      DO 295 I=1,NVALS     
295      VALUES(I)=VALUES(I)*RATIO  
  
       
C
C
C  CHECK IF ANY ENTRIES IN COMMON BLOCK      
ckwz      IF (NDTUHG.EQ.0) THEN
ckwz         NDTUHG=1
ckwz         CALL UMEMOV (OPNAME,IOPUHG(1,1),2)
         
         
ckwz         JDTUHG(1,1)=IDATE
ckwz         JDTUHG(2,1)=KDATE
ckwz		 JDTUHG(3,1)=tmpLDate
ckwz         NVLUHG(1)=NVALS
ckwz         DO 300 I=1,NVALS
ckwz            UHGVAL(1,I)=VALUES(I)
ckwz300         CONTINUE
ckwz         UHGOPN(NDTUHG)=tmpopn
ckwz         GO TO 380
ckwz         ENDIF
C
C  CHECK IF NAME OR DATE ALREAY IN COMMON BLOCK
ckwz      DO 340 I=1,NDTUHG
         IF (IUSAME(OPNAME,IOPUHG(1,I),2).EQ.0) GO TO 340
         IF (JDTUHG(1,I).EQ.IDATE.AND.JDTUHG(2,I).EQ.LDATE) THEN
C        REPLACE PREVIOUS VALUES
            IF (MODWRN.EQ.1) THEN
            WRITE (IPR,310) OPNAME,
     *         IM3,ID3,IY3,IH3,MODTZC,
     *         IM4,ID4,IY4,IH4,MODTZC
310   FORMAT ('0**WARNING** IN UHGCDATE MOD - ',
     *   'OPERATION NAME ',A,' AND DATES ',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,' TO ',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,
     *  ' ALREADY SPECIFIED.')
            WRITE (IPR,230) MODCRD(NRDCRD)
            CALL WARN
            ENDIF
            DO 320 J=1,MVALS
               UHGVAL(NDTUHG,J)=0.0
              
320            CONTINUE
            DO 330 J=1,NVALS
               UHGVAL(NDTUHG,J)=VALUES(J)
            
330            CONTINUE
            
ckwz            GO TO 380
            ENDIF
340      CONTINUE
C
C  NO MATCH FOUND FOR NAME AND DATE
      
      IF (NDTUHG+1.GT.MDTUHG) THEN
         IF (MODWRN.EQ.1) THEN
         WRITE (IPR,350) MDTUHG,MODCRD(NRDCRD)
350   FORMAT ('0**WARNING** IN UHGCDATE MOD - ',
     *   'MAXIMUM NUMBER OF ENTRIES IN COMMON BLOCK MODUHG (',I2,
     *   ') EXCEEDED.' /
     * 13X,'THE FOLLOWING MOD CARD WILL BE IGNORED:' /
     * 13X,A)
            CALL WARN
            ENDIF
ckwz         GO TO 380
         ENDIF
       
      IF (NVALS.GT.MVLUHG) THEN
         IF (MODWRN.EQ.1) THEN
         WRITE (IPR,360) MVALS,MVLUHG,MODCRD(NRDCRD)
360   FORMAT ('0**WARNING** IN UHGCDATE MOD - ',
     *    'NUMBER OF VALUES (',I3,') EXCEEDS MAXIMUM (',I3,
     *    ') FOR THE FOLLOWING MOD:' /
     * 13X,'THE FOLLOWING MOD CARD WILL BE IGNORED:' /
     * 13X,A)
            CALL WARN
            ENDIF
ckwz         GO TO 380
         ENDIF
c
C  ADD NEW ENTRY TO COMMON BLOCK MODUHG
      NDTUHG=NDTUHG+1  
      CALL UMEMOV (OPNAME,IOPUHG(1,NDTUHG),2)
      UHGOPN(NDTUHG)=tmpopn
      JDTUHG(1,NDTUHG)=tmpIDATE  !start date
      JDTUHG(2,NDTUHG)=tmpLDate  !end date
      JDTUHG(3,NDTUHG)=KDATE     !valid date
      NVLUHG(NDTUHG)=NVALS
      DO 370 J=1,NVALS
         UHGVAL(NDTUHG,J)=VALUES(J)
370      CONTINUE
C
C  IF A SPECIFIC OPERATION NAME WAS ENTERED READ NEXT CARD,
C  IF NO OPERATION NAME WAS ENTERED, SEARCH P ARRAY FOR ANOTHER
C  UNIT-HYDROGRAPH OPERATION
ckwz commentted to avoid duplicated uhgcdate mod in other mod window.
ckwz       IF (IONEOP.EQ.1) GO TO 40
ckwz       GO TO 180
C
390   CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
       
C

      iuhgd = 1
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'EXIT ',RTNNAM
      RETURN
C
      END
