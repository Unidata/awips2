C$PRAGMA C (CHECK_EXIST)
C MODULE FCPPTS
C-----------------------------------------------------------------------
C
      SUBROUTINE FCPPTS (TS,MTS,IPRINT,IPUNCH)
C
C  THIS ROUTINE PRINTS OR PUNCHES THE TIME SERIES INFORMATION IN THE TS
C  ARRAY.
C
      CHARACTER*4 DTYPE,EDTYPE,EDTYPEN
      CHARACTER*4 FILETP,TFILETP,EFILETP,EFILETPN,EFILETPT
      CHARACTER*8 TSID,PTSID,ETSID,FTSID,ETSIDF,ETSIDN
      CHARACTER*8 TSTYPE,ETSTYPE,ZTSTYPE
      CHARACTER*8 EPROCTP
      CHARACTER*8 OLDOPN,SEGID
      CHARACTER*10 STRNG
      CHARACTER*12 STAID
      CHARACTER*20 DESCRP
      CHARACTER*32 TSNAME
      CHARACTER*80 DIRNAM
      CHARACTER*112 PATHNAME
C
      DIMENSION TS(MTS)
      DIMENSION EXTLOC(16),IHEAD(22),PLOCAT(2)
      PARAMETER (MPESP=1000)
      DIMENSION PESP(MPESP)
      PARAMETER (MSPESP=1)
      DIMENSION SPESP(MSPESP)
      PARAMETER (MTSESP=10000)
      DIMENSION TSESP(MTSESP)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fcsegn'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fcppts.f,v $
     . $',                                                             '
     .$Id: fcppts.f,v 1.5 2003/03/14 18:51:04 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FCPPTS'
C
      IOPNUM=0
      CALL FSTWHR ('FCPPTS  ',IOPNUM,OLDOPN,IOLDOP)
C
      IBUG=IFBUG('SGPR')
C
      IF (IBUG.EQ.1) THEN
         CALL FDMPA ('TS  ',TS,MTS)
         ENDIF
C
C  PRINT HEADING
      IF (IPRINT.EQ.1) THEN
         CALL TSPRT_HEADER
         ENDIF
      IF (IPUNCH.EQ.1) THEN
         WRITE (IPU,'(A)') 'DEF-TS'
         ENDIF
C
C  CHECK IF DEFINED IN ESP PARAMETER FILE
      IERECT=IEREC
      IF (IEREC.GT.0) THEN
C     READ ESP PARAMETER FILE CONTROL RECORD
         CALL EREAD1 (IERR)
         IF (IERR.NE.0) THEN
            IERECT=0
            ELSE
C           GET ESP SEGMENT DEFINITION
               ICODE=1
               ICHKID=1
               CALL ESPRDF (ICODE,ICHKID,IEREC,TSESP,MTSESP,PESP,MPESP,
     *            SPESP,MSPESP,IERR)
               IF (IERR.NE.0) THEN
                  WRITE (IPR,10) IDSEGN,IEREC
10    FORMAT ('0**ERROR** ENCOUNTERED READING SEGMENT ',2A4,
     *   ' FROM RECORD ',I5,' OF THE ESP PARAMETER FILE.')
                  CALL ERROR
                 IERECT=0
                 ENDIF
            ENDIF
         ENDIF
C
      NUMTS=0
      LTS=1
      LD=0
      IETSERR=0
C
C  GET TYPE OF TIME SERIES
20    ITSTYP=TS(LTS)
C
C  CHECK FOR END OF TS ARRAY.
      IF (ITSTYP.EQ.0) GO TO 140
C
      IF (ITSTYP.GE.1.AND.ITSTYP.LE.4) THEN
         IF (ITSTYP.EQ.1) TSTYPE='INPUT'
         IF (ITSTYP.EQ.2) TSTYPE='UPDATE'
         IF (ITSTYP.EQ.3) TSTYPE='OUTPUT'
         IF (ITSTYP.EQ.4) TSTYPE='INTERNAL'
         ELSE
            IBEG=1
            NCHAR=LEN(STRNG)
            IPRERR=1
            CALL UFI2A (ITSTYP,STRNG,IBEG,NCHAR,IPRERR,IPR,IERR)
            WRITE (IPR,30) 'TIME SERIES',STRNG(1:LENSTR(STRNG))
30    FORMAT ('0**ERROR** ',A,' TYPE ',A,' CANNOT BE PROCESSED.')
            CALL ERROR
            GO TO 40
         ENDIF
C
C  GET INFORMATION FOR THE TIME SERIES
      CALL UMEMOV (TS(LTS+2),TSID,2)
      CALL UMEMOV (TS(LTS+4),DTYPE,1)
      ITIME=TS(LTS+5)
      NPDT=TS(LTS+6)
      LD=LD+(24/ITIME)*NPDT*NDD
      IF (ITSTYP.EQ.4) THEN
         FILETP=' '
         ELSE
            CALL UMEMOV (TS(LTS+9),FILETP,1)
         ENDIF
C
      IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,*) ' TSID=',TSID,' DTYPE=',DTYPE,
     *      ' ITSTYP=',ITSTYP
         ENDIF
      IF (TSID.EQ.'TAHC1'.AND.DTYPE.EQ.'QINE'.AND.ITSTYP.EQ.3) THEN
         IF (IBUG.EQ.1) WRITE (IODBUG,*) ' FOUND: TSID=',TSID,
     *      ' DTYPE=',DTYPE,' ITSTYP=',ITSTYP
         ENDIF
C
      EFILETP=' '
      ETSTYPE=' '
C
C  CHECK IF ESP SEGMENT INFORMATION DEFINED
40    IF (IERECT.GT.0) THEN
         LTSESP=0
50       IETSTYP=TSESP(LTSESP+1)
         IF (IETSTYP.GT.0) THEN
            IF (IETSTYP.GE.1.AND.IETSTYP.LE.4) THEN
               ELSE
                  IF (IETSERR.EQ.0) THEN
                     IBEG=1
                     NCHAR=LEN(STRNG)
                     IPRERR=1
                     CALL UFI2A (IETSTYP,STRNG,IBEG,NCHAR,IPRERR,IPR,
     *                  IERR)
                     WRITE (IPR,30) 'ESP TIME SERIES',
     *                  STRNG(1:LENSTR(STRNG))
                     CALL ERROR
                     IETSERR=1
                     ENDIF
                  GO TO 60
               ENDIF
            CALL UMEMOV (TSESP(LTSESP+3),ETSID,2)
            CALL UMEMOV (TSESP(LTSESP+5),EDTYPE,1)
            IETIME=TSESP(LTSESP+6)
            IF (IBUG.EQ.1) THEN
               WRITE (IODBUG,*) ' ETSID=',ETSID,' EDTYPE=',EDTYPE,
     *            ' IETSTYP=',IETSTYP
               ENDIF
            ETSTYPE=' '
            CALL UMEMOV (TSESP(LTSESP+10),EFILETP,1)
            IFOUND=0
            IF (IETSTYP.LT.4) IFOUND=1
            IF (IETSTYP.EQ.4.AND.EFILETP.EQ.'CARD') IFOUND=1
            IF (IFOUND.EQ.1) THEN
               IF (ETSID.EQ.TSID.AND.
     *             EDTYPE.EQ.DTYPE.AND.
     *             ITIME.EQ.IETIME) THEN
                  IF (IETSTYP.LT.4.AND.EFILETP.EQ.'CARD') THEN
                     CALL UMEMOV (TSESP(LTSESP+21),DIRNAM,LEN(DIRNAM)/4)
                     CALL UMEMOV (TSESP(LTSESP+41),TSNAME,LEN(TSNAME)/4)
                     ENDIF
                  ICSAME=0
                  ISAME=0
                  IF (ICSAME.EQ.1.AND.IETSTYP.EQ.ITSTYP) ISAME=1
                  IF (ISAME.EQ.0) THEN
                     IF (IETSTYP.EQ.1) ETSTYPE='INPUT'
                     IF (IETSTYP.EQ.2) ETSTYPE='UPDATE'
                     IF (IETSTYP.EQ.3) ETSTYPE='OUTPUT'
                     IF (IETSTYP.EQ.4) ETSTYPE='INTERNAL'
                     ENDIF
                  IF (IBUG.EQ.1) THEN
                     WRITE (IODBUG,*) ' EFILETP=',EFILETP,
     *                  ' ETSTYPE=',ETSTYPE
                     ENDIF
                  GO TO 60
                  ENDIF
               ENDIF
            EFILETP=' '
            LTSESP=TSESP(LTSESP+2)-1
            IF (LTSESP.LT.MTSESP) GO TO 50
            ENDIF
         ENDIF
C
C  OUTPUT GENERAL INFORMATION FOR TIME SERIES
60    EFILETPT=EFILETP
CCC      IF (EFILETPT.EQ.' '.AND.ETSTYPE.EQ.'OUTPUT') EFILETPT='NONE'
      IF (IPRINT.EQ.1) THEN
         CALL TSPRT_GENERAL (NUMTS,TSID,DTYPE,ITIME,TSTYPE,FILETP,
     *      EFILETPT,ETSTYPE)
         ENDIF
      IF (IPUNCH.EQ.1) THEN
         WRITE (IPU,70) TSID,DTYPE,ITIME,TSTYPE,FILETP,
     *      EFILETPT,ETSTYPE
70    FORMAT (A,3X,A,3X,I2,12X,A,6X,A,5X,A,1X,A)
         ENDIF
C
C  CHECK IF INTERNAL TIME SERIES
      IF (ITSTYP.EQ.4) THEN
         IF (IERECT.GT.0.AND.(IETSTYP.GE.1.AND.IETSTYP.LE.3)) THEN
            ELSE
               GO TO 130
            ENDIF
         ENDIF
C
C  GET EXTERNAL LOCATION INFORMATION
      NUMEXT=TS(LTS+11)
      IF (ITSTYP.EQ.2.AND.NUMEXT.LE.4) THEN
         LXBUF=1
         CALL RPRDH (TSID,DTYPE,LXBUF,IHEAD,NXBUF,XBUF,FTSID,IERR)
         ENDIF
      ICKVAL=9999
      IF (NUMEXT.GT.ICKVAL) THEN
         WRITE (IPR,80) 'NUMEXT',NUMEXT,ICKVAL
80    FORMAT ('0**ERROR** IN FCPPTS - VALUE OF VARIABLE ',A,' (',I10,
     *   ') IS GREATER THAN ',I6,'.')
         CALL ERROR
         ELSE
	   IF (NUMEXT.GT.16) THEN
             DO 89 I=1,16
               EXTLOC(I)=TS(LTS+11+I)
89           CONTINUE	   
	   ELSE
             DO 90 I=1,NUMEXT
               EXTLOC(I)=TS(LTS+11+I)
90           CONTINUE
           ENDIF
         ENDIF
C
C  WRITE EXTERNAL LOCATION INFORMATION FOR SPECIFIED FILE TYPE
C
      IFILETP=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (FILETP.EQ.'FPDB') THEN
C     PROCESSED DATA BASE FILE TYPE
         CALL UMEMOV (EXTLOC(1),PTSID,2)
         CALL UMEMOV (EXTLOC(3),FDTYPE,1)
         IFOUND=0
         IF (ITSTYP.EQ.2.AND.NUMEXT.LE.4) THEN
            CALL UMEMOV (IHEAD(12),PLOCAT,2)
            CALL UMEMOV (IHEAD(18),DESCRP,5)
            IF (IPRINT.EQ.1) THEN
               CALL TSPRT_FPDB_OUTPUT (FILETP,PTSID,FDTYPE,PLOCAT,
     *            DESCRP)
               ENDIF
            IF (IPUNCH.EQ.1) THEN
               WRITE (IPU,100) PTSID,FDTYPE,PLOCAT,DESCRP
100   FORMAT (A,3X,A,2F10.2,5X,A)
               ENDIF
            IFOUND=1
            ENDIF
         IF (NUMEXT.GT.3) THEN
            CALL UMEMOV (EXTLOC(5),PLOCAT,2)
            CALL UMEMOV (EXTLOC(7),DESCRP,5)
            IF (IPRINT.EQ.1) THEN
               CALL TSPRT_FPDB_OUTPUT (FILETP,PTSID,FDTYPE,PLOCAT,
     *            DESCRP)
               ENDIF
            IF (IPUNCH.EQ.1) THEN
               WRITE (IPU,100) PTSID,FDTYPE,PLOCAT,DESCRP
               ENDIF
            IFOUND=1
            ENDIF
         IF (IFOUND.EQ.0) THEN
            IF (IPRINT.EQ.1) THEN
               CALL TSPRT_FPDB_INPUT (FILETP,PTSID,FDTYPE)
               ENDIF
            IF (IPUNCH.EQ.1) THEN
               WRITE (IPU,100) PTSID,FDTYPE
               ENDIF
            ENDIF
         IFILETP=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (FILETP.EQ.'CALB') THEN
C     CALIBRATION DATA FILE TYPE
         IF (MAINUM.GT.1) GO TO 130
         IF (ITSTYP.EQ.2) GO TO 130
         IF (ITSTYP.EQ.1) THEN
C        INPUT TIME SERIES
            CALL TSPRT_CALB_INPUT (EXTLOC)
            ENDIF
         IF (ITSTYP.EQ.3) THEN
C        OUTPUT TIMES SERIES
            CALL TSPRT_CALB_OUTPUT (EXTLOC)
            ENDIF
         IFILETP=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (EFILETP.EQ.'CARD') THEN
C     DATACARD FILE TYPE
         CALL FCPPTS_CARD (DIRNAM,TSNAME,PATHNAME,DTYPE,
     *      STAID,DESCRP,ITMO1,ITYR1,ITMO2,ITYR2)
         IF (IPRINT.EQ.1) THEN
            CALL TSPRT_CARD (EFILETP,STAID,DESCRP,
     *         ITMO1,ITYR1,ITMO2,ITYR2,PATHNAME)
            ENDIF
         IF (IPUNCH.EQ.1) THEN
            WRITE (IPU,'(A)') TSNAME(1:LENSTR(TSNAME))
            IF (ETSTYPE.EQ.'UPDATE') WRITE (IPU,'(A)') 'CARD'
            ENDIF
         IFILETP=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (EFILETP.EQ.'ESP') THEN
C     ESP FILE TYPE
         IF (IBUG.EQ.1) THEN
            WRITE (IPR,*) 'IN FCPPTS - LTSESP=',LTSESP
            ENDIF
         ZTSTYPE=ETSTYPE
         IF (ZTSTYPE.EQ.' ') ZTSTYPE=TSTYPE
         IF (ZTSTYPE.EQ.'INPUT') THEN
            CALL UMEMOV (TSESP(LTSESP+13),ETSIDF,2)
            CALL UMEMOV (TSESP(LTSESP+15),EDTYPE,1)
            ITIME=TSESP(LTSESP+16)
            CALL UMEMOV (TSESP(LTSESP+17),STAID,3)
            IDELTE=TSESP(LTSESP+20)
            IF (IPRINT.EQ.1) THEN
               CALL TSPRT_ESP_INPUT (EFILETP,STAID,ETSIDF,EDTYPE,ITIME,
     *            IDELTE)
               ENDIF
            IF (IPUNCH.EQ.1) THEN
               WRITE (IPU,'(A,3X,A,2X,A,1X,I5,I5)')
     *            STAID,ETSIDF,EDTYPE,ITIME,IDELTE
               ENDIF
            IF (ETSIDF.NE.TSID) THEN
               WRITE (IPR,105) EFILETP(1:LENSTR(EFILETP)),ETSIDF,TSID
105   FORMAT ('0**WARNING** ',A,' TIME SERIES IDENTIFIER (',A,
     *   ') IS NOT THE SAME AS THE TIME SERIES IDENTIFIER (',A,
     *   ').')
               CALL WARN
               ENDIF
            ENDIF
         IF (ZTSTYPE.EQ.'OUTPUT') THEN
            CALL UMEMOV (TSESP(LTSESP+17),STAID,3)
            IWRITE=TSESP(LTSESP+20)
            IF (IPRINT.EQ.1) THEN
               CALL TSPRT_ESP_OUTPUT (EFILETP,STAID,IWRITE)
               ENDIF
            IF (IPUNCH.EQ.1) THEN
               WRITE (IPU,'(A,2X,I5)') STAID,IWRITE
               ENDIF
            CALL UMEMOV (IDSEGN,SEGID,2)
            IF (STAID(1:8).EQ.SEGID.AND.STAID(9:12).EQ.' ') THEN
               ELSE
                  WRITE (IPR,110) ETSID,IDSEGN
110   FORMAT ('0**WARNING** STATION IDENTIFIER (',A,
     *   ') IS NOT THE SAME AS THE SEGMENT IDENTIFIER (',2A4,
     *   ').' /
     * 13X,'THE STATION IDENTIFIER MUST BE SAME AS THE ',
     *   'SEGMENT IDENTIFIER FOR PROGRAM ESPADP TO FIND THE FILES.')
                  CALL WARN
               ENDIF
            ENDIF
         IFILETP=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (EFILETP.EQ.'GENR') THEN
C     GENR FILE TYPE
         IPOS=13
         CALL UMEMOV (TSESP(LTSESP+IPOS),EPROCTP,2)
         CALL UMEMOV (TSESP(LTSESP+IPOS+21),DIRNAM,LEN(DIRNAM)/4)
         CALL UMEMOV (TSESP(LTSESP+IPOS+41),TSNAME,LEN(TSNAME)/4)
         IF (IPRINT.EQ.1) THEN
            IF (EPROCTP.EQ.'BLEND-TS') THEN
               JPOS=IPOS+2
               CALL TSPRT_BLEND_TS (EFILETP,EPROCTP,TSESP(LTSESP+JPOS))
               CALL FCPPTS_EXIST (DIRNAM,TSNAME,PATHNAME,IFOUND)
               CALL TSPRT_PATHNAME (PATHNAME)
               ENDIF
            IF (EPROCTP.EQ.'CREAT-PE') THEN
               JPOS=IPOS+2
               CALL TSPRT_CREAT_PE (EFILETP,EPROCTP,TSESP(LTSESP+JPOS))
               ENDIF
            ENDIF
         IF (IPUNCH.EQ.1) THEN
            WRITE (IPU,'(A)') EPROCTP
            IF (EPROCTP.EQ.'BLEND-TS') THEN
               JPOS=IPOS+1
               ITEMP1=TSESP(LTSESP+JPOS+3)
               ITEMP2=TSESP(LTSESP+JPOS+4)
               WRITE (IPU,'(2F5.2,2I5)') (TSESP(LTSESP+JPOS+I),I=1,2),
     *            ITEMP1,ITEMP2
               WRITE (IPU,'(A)') TSNAME(1:LENSTR(TSNAME))
               ENDIF
            IF (EPROCTP.EQ.'CREAT-PE') THEN
               JPOS=IPOS+1
               WRITE (IPU,'(12F5.2)') (TSESP(LTSESP+JPOS+I),I=1,12)
               ENDIF
            ENDIF
         IFILETP=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (EFILETP.EQ.'MSNG') THEN
C     MSNG FILE TYPE - NO OUTPUT
         IFILETP=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (EFILETP.EQ.'REPL') THEN
C     REPLACE FILE TYPE
         ZTSTYPE=ETSTYPE
         IF (ZTSTYPE.EQ.' ') ZTSTYPE=TSTYPE
         IF (ZTSTYPE.EQ.'INPUT') THEN
            CALL UMEMOV (TSESP(LTSESP+57),EFILETPN,1)
            CALL UMEMOV (TSESP(LTSESP+58),ETSIDN,2)
            CALL UMEMOV (TSESP(LTSESP+60),EDTYPEN,1)
            ITIMEN=TSESP(LTSESP+61)
            IF (EFILETPN.EQ.'CARD') THEN
               CALL UMEMOV (TSESP(LTSESP+21),DIRNAM,LEN(DIRNAM)/4)
               CALL UMEMOV (TSESP(LTSESP+41),TSNAME,LEN(TSNAME)/4)
               CALL FCPPTS_CARD (DIRNAM,TSNAME,PATHNAME,DTYPE,
     *            STAID,DESCRP,ITMO1,ITYR1,ITMO2,ITYR2)
               ENDIF
            IF (IPRINT.EQ.1) THEN
               CALL TSPRT_REPL_INPUT (EFILETP,ETSIDN,EDTYPEN,ITIMEN,
     *            EFILETPN)
               IF (EFILETPN.EQ.'CARD') THEN
                  CALL TSPRT_CARD (EFILETP,STAID,DESCRP,
     *               ITMO1,ITYR1,ITMO2,ITYR2,PATHNAME)
                  ENDIF
               ENDIF
            IF (IPUNCH.EQ.1) THEN
               WRITE (IPU,'(A,3X,A,3X,I2,26X,A)')
     *            ETSIDN,EDTYPEN,ITIMEN,EFILETPN
               IF (EFILETPN.EQ.'CARD') THEN
                  WRITE (IPU,'(A)') TSNAME(1:LENSTR(TSNAME))
                  ENDIF
               ENDIF
            ENDIF
         IF (ZTSTYPE.EQ.'OUTPUT') THEN
            CALL UMEMOV (TSESP(LTSESP+17),STAID,3)
            IWRITE=TSESP(LTSESP+20)
            IF (IPRINT.EQ.1) THEN
               CALL TSPRT_REPL_OUTPUT (EFILETP,ETSIDF,IWRITE)
               ENDIF
            IF (IPUNCH.EQ.1) THEN
               WRITE (IPU,'(A,2X,I5)') STAID,IWRITE
               ENDIF
            ENDIF
         IFILETP=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (IFILETP.EQ.0) THEN
         TFILETP=FILETP
         IF (TFILETP.EQ.' ') TFILETP=EFILETP
         WRITE (IPR,120) TFILETP
120   FORMAT ('0**ERROR** FILE TYPE ',A,' CANNOT BE PROCESSED.')
         CALL ERROR
         ENDIF
      GO TO 130
C
C  INCREMENT TO NEXT TIME SERIES
130   LTS=TS(LTS+1)
      IF (LTS.LE.MTS) GO TO 20
      LTS=MTS
C
140   IF (IPRINT.EQ.1) THEN
C     PRINT ARRAY SPACE USAGE
         CALL TSPRT_ARRAY_SPACE (LTS,MTS,LD)
         ENDIF
C
      IF (IPUNCH.EQ.1) THEN
         WRITE (IPU,'(A)') 'END'
         ENDIF
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C$PRAGMA C (CHECK_EXIST)
      SUBROUTINE FCPPTS_EXIST (DIRNAM,TSNAME,PATHNAME,IFOUND)
C
C  CHECK IF FILE EXISTS
C
      CHARACTER*10 FILETYPE
      CHARACTER*32 TSNAME
      CHARACTER*80 DIRNAM
      CHARACTER*112 PATHNAME,PATHNAME2
C
      INCLUDE 'common/ionum'
C
C     IFOUND=0
C
      PATHNAME=DIRNAM(1:LENSTR(DIRNAM))//'/'//TSNAME
      PATHNAME2=PATHNAME(1:LENSTR(PATHNAME))//CHAR(0)
      FILETYPE='file'//CHAR(0)
      IPRINT=0
      CALL CHECK_EXIST (PATHNAME2,FILETYPE,IEXIST,IPRINT)
      IF (IEXIST.EQ.0) THEN
         WRITE (IPR,10) PATHNAME(1:LENSTR(PATHNAME))
10    FORMAT ('0**WARNING** FILE ',A,' NOT FOUND.')
         CALL WARN
         IFOUND=0
         ENDIF
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE FCPPTS_CARD (DIRNAM,TSNAME,PATHNAME,RDTYPE,
     *   STAID,DESCRP,ITMO1,ITYR1,ITMO2,ITYR2)
C
C  GET INFORMATION FOR CARD FILE TYPE
C
      CHARACTER*4 RUNITS,DUNITS,RDTYPE
      CHARACTER*12 STAID,OFORMAT
      CHARACTER*20 DESCRP
      CHARACTER*32 TSNAME
      CHARACTER*80 DIRNAM
      CHARACTER*112 PATHNAME
C
      INCLUDE 'common/ionum'
C
C
      STAID='?'
      DESCRP='?'
      ITMO1=0
      ITYR1=0
      ITMO2=0
      ITYR2=0
C
C  CHECK IF FILE EXISTS
      CALL FCPPTS_EXIST (DIRNAM,TSNAME,PATHNAME,IFOUND)
      IF (IFOUND.EQ.1) THEN
         IRMO1=0
         IRYR1=0
         IRMO2=0
         IRYR2=0
         RUNITS='SAME'
         DUNITS='?'
         STAID='?'
         DESCRP='?'
         CALL CARDLO (IRMO1,IRYR1,IRMO2,IRYR2,
     *                ITMO1,ITYR1,ITMO2,ITYR2,
     *                RUNITS,DUNITS,PATHNAME,RDTYPE,ITIME,
     *                STAID,DESCRP,IUNIT,OFORMAT,
     *                CONVF1,CONVF2,
     *                IJMO1,IJMO2,IERR)
         ENDIF
C
      RETURN
C
      END
