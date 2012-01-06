C MODULE SSPPD2
C-----------------------------------------------------------------------
C
C   ROUTINE TO PRINT LEVEL 2 STATUS FOR PREPROCESSOR DATA BASE.
C
      SUBROUTINE SSPPD2 (LARRAY,IARRAY,STAIDX,LSIBUF,ISIBUF,ISTAT)
C
      CHARACTER*4 PP24X1,PP24X2,TM24X1,TM24X2
      CHARACTER*4 CNUM1,CNUM2
      CHARACTER*4 PARMTP
      PARAMETER (MOTHR=20)
      CHARACTER*4 OTHR1(MOTHR),OTHR2(MOTHR)
      CHARACTER*4 XPP24/'PP24'/,XTM24/'TM24'/
      CHARACTER*6 STANB1,STANB2
      CHARACTER*8 STAIDX,STAID
      INTEGER*2 I2BLNK/2H  /
      INTEGER*2 IBUF1(5),IBUF2(5)
      INTEGER*2 ISIBUF(LSIBUF)
C
      DIMENSION IARRAY(3,1)
      PARAMETER (MTYPES=50)
      DIMENSION ITYPES(2,MTYPES)
      PARAMETER (LARAY=500)
      DIMENSION ARAY(LARAY)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssppd2.f,v $
     . $',                                                             '
     .$Id: ssppd2.f,v 1.4 2002/02/11 21:04:47 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSPPD2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
      NUMERR=0
      NUMWRN=0
      MAXID=LARRAY/3
      NTYPES=0

C  OPEN DATA BASES
      CALL SUDOPN (1,'PPD ',IERR)
      CALL SUDOPN (1,'PPP ',IERR)
C
C  CHECK IF ANY STATIONS DEFINED
      IF (NPDSTA.EQ.0) THEN
         WRITE (LP,140)
         CALL SULINE (LP,2)
         GO TO 120
         ENDIF
C
      IF (STAIDX.NE.' ') THEN
         CALL PDFNDR (STAIDX,LSIBUF,IFIND,ISIREC,ISIBUF,IFREE,IERR)
         IF (IERR.EQ.0.AND.IFIND.GT.0) THEN
            ELSE
               IF (IFIND.EQ.0) THEN
                  WRITE (LP,220) STAIDX
                  CALL SULINE (LP,2)
                  GO TO 120
                  ENDIF
               IF (IERR.GT.0) THEN
                  WRITE (LP,230) STAIDX,IERR
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 120
                  ENDIF
            ENDIF
         NUMID=1
         GO TO 20
         ENDIF
C
C  GET LIST OF PPDB IDENTIFIERS AND RECORD LOCATIONS
      NUMID=0
      IREC=INFREC+1
10    CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,145) 'PDRSIF',IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 110
         ENDIF
      CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
      IF (STAID.NE.'DELETED') THEN
         IF (NUMID+1.GT.MAXID) THEN
            WRITE (LP,150) MAXID
            CALL SUERRS (LP,2,NUMERR)
            GO TO 110
            ENDIF
         NUMID=NUMID+1
         CALL SUBSTR (STAID,1,8,IARRAY(1,NUMID),1)
         IARRAY(3,NUMID)=IREC
         ENDIF
      IF (NXREC.LE.LSTSIF) THEN
         IREC=NXREC
         GO TO 10
         ENDIF
C
C  SORT IDENTIFIERS
      NWORDS=3
      ISPTR=0
      CALL SUSORT (NWORDS,NUMID,IARRAY,IARRAY,ISPTR,IERR)
C
C  INITIALIZE DATA TYPE ARRAY
20    CALL SMPPDT ('INIT',MTYPES,ITYPES,NTYPES,IERR)
C
C  WRITE HEADER FOR REPORT
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      IF (NUMID.EQ.1) THEN
         IF (STAIDX.EQ.' ') THEN
            WRITE (LP,160) 'STATION'
            CALL SULINE (LP,2)
            ENDIF
         WRITE (LP,170)
         CALL SULINE (LP,2)
         WRITE (LP,180)
         CALL SULINE (LP,1)
         ELSE
            WRITE (LP,160) 'STATIONS'
            CALL SULINE (LP,2)
            WRITE (LP,170) ' '
            CALL SULINE (LP,2)
            WRITE (LP,180) ' '
            CALL SULINE (LP,1)
         ENDIF
C
C  PRINT STATION INFORMATION
      MTIME=NUMID/2
      IEVEN=1
      IF (MOD(NUMID,2).NE.0) IEVEN=0
      IF (IEVEN.EQ.0) MTIME=MTIME+1
      NPER=5
      JSIBUF=11
      DO 100 NTIME=1,MTIME
         DO 30 J=1,5
            IF (J.NE.5) THEN
               IBUF1(J)=I2BLNK
               IBUF2(J)=I2BLNK
               ENDIF
            IF (J.EQ.5) THEN
               IBUF1(J)=0
               IBUF2(J)=0
               ENDIF
30          CONTINUE
         PP24X1=' '
         TM24X1=' '
         PP24X2=' '
         TM24X2=' '
         DO 40 J=1,MOTHR
            OTHR1(J)=' '
            OTHR2(J)=' '
40          CONTINUE
         IF (STAIDX.EQ.' ') THEN
            IREC=IARRAY(3,NTIME)
            CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,IERR)
            IF (IERR.GT.0) THEN
               WRITE (LP,145) 'PDRSIF',IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 110
               ENDIF
            ENDIF
         CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
         IF (STAIDX.NE.' '.AND.STAIDX.NE.STAID) GO TO 100
         CALL SUBSTR (ISIBUF,3,10,IBUF1,1)
         IF (ISIBUF(8).GT.0) THEN
            PP24X1=XPP24
            CALL SMPPDT (XPP24,MTYPES,ITYPES,NTYPES,IERR)
            IREC=0
            PARMTP='PCPN'
            CALL RPPREC (STAID,PARMTP,IREC,LARAY,ARAY,NFILL,IRECNX,IERR)
            IF (IERR.NE.0) THEN
               IERRN=-IERR
               CALL SRPPST (STAID,PARMTP,IREC,LARAY,NFILL,IRECNX,IERRN)
               ENDIF
            ENDIF
         IF (ISIBUF(9).GT.0) THEN
            TM24X1=XTM24
            CALL SMPPDT (XTM24,MTYPES,ITYPES,NTYPES,IERR)
            IREC=0
            PARMTP='TEMP'
            CALL RPPREC (STAID,PARMTP,IREC,LARAY,ARAY,NFILL,IRECNX,IERR)
            IF (IERR.NE.0) THEN
               IERRN=-IERR
               CALL SRPPST (STAID,PARMTP,IREC,LARAY,NFILL,IRECNX,IERRN)
               ENDIF
            ENDIF
         NOTHR1=ISIBUF(10)
         IF (NOTHR1.GT.0) THEN
            IF (NOTHR1.GT.MOTHR) THEN
               CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
               WRITE (LP,190) STAID,NOTHR1,MOTHR,MOTHR
               CALL SUWRNS (LP,2,NUMWRN)
               NOTHR1=MOTHR
               ENDIF
            IRRS=0
            DO 50 J=1,NOTHR1
               IPOS=JSIBUF+(J-1)*3
               CALL SUBSTR (ISIBUF(IPOS),1,4,OTHR1(J),1)
               CALL SMPPDT (OTHR1(J),MTYPES,ITYPES,NTYPES,IERR)
               IRRSTP=IPDCKR(OTHR1(J))
               IF (IRRSTP.GT.0) IRRS=1
50             CONTINUE
            IF (IRRS.EQ.1) THEN
               IREC=0
               PARMTP='RRS'
               CALL RPPREC (STAID,PARMTP,IREC,LARAY,ARAY,NFILL,IRECNX,
     *            IERR)
               IF (IERR.NE.0) THEN
                  IERRN=-IERR
                  CALL SRPPST (STAID,PARMTP,IREC,LARAY,NFILL,IRECNX,
     *              IERRN)
                  ENDIF
               ENDIF
            ENDIF
         NOTHR2=0
         IF (NTIME.EQ.MTIME.AND.IEVEN.EQ.0) GO TO 70
            IREC=IARRAY(3,NTIME+MTIME)
            CALL PDRSIF (IREC,NXREC,LSIBUF,ISIBUF,IERR)
            IF (IERR.GT.0) THEN
               WRITE (LP,145) 'PDRSIF',IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 110
               ENDIF
            CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
            CALL SUBSTR (ISIBUF,3,10,IBUF2,1)
            IF (ISIBUF(8).GT.0) THEN
               PP24X2=XPP24
               CALL SMPPDT (XPP24,MTYPES,ITYPES,NTYPES,IERR)
               IREC=0
               PARMTP='PCPN'
               CALL RPPREC (STAID,PARMTP,IREC,LARAY,ARAY,NFILL,IRECNX,
     *            IERR)
               IF (IERR.NE.0) THEN
                  IERRN=-IERR
                  CALL SRPPST (STAID,PARMTP,IREC,LARAY,NFILL,IRECNX,
     *               IERRN)
                  ENDIF
               ENDIF
            IF (ISIBUF(9).GT.0) THEN
               TM24X2=XTM24
               CALL SMPPDT (XTM24,MTYPES,ITYPES,NTYPES,IERR)
               IREC=0
               PARMTP='TEMP'
               CALL RPPREC (STAID,PARMTP,IREC,LARAY,ARAY,NFILL,IRECNX,
     *            IERR)
               IF (IERR.NE.0) THEN
                  IERRN=-IERR
                  CALL SRPPST (STAID,PARMTP,IREC,LARAY,NFILL,IRECNX,
     *               IERRN)
                  ENDIF
               ENDIF
            NOTHR2=ISIBUF(10)
            IF (NOTHR2.GT.0) THEN
               IF (NOTHR2.GT.MOTHR) THEN
                  CALL SUBSTR (ISIBUF(2),1,8,STAID,1)
                  WRITE (LP,190) STAID,NOTHR2,MOTHR,MOTHR
                  CALL SUWRNS (LP,2,NUMWRN)
                  NOTHR2=MOTHR
                  ENDIF
               IRRS=0
               DO 60 J=1,NOTHR2
                  IPOS=JSIBUF+(J-1)*3
                  CALL SUBSTR (ISIBUF(IPOS),1,4,OTHR2(J),1)
                  CALL SMPPDT (OTHR2(J),MTYPES,ITYPES,NTYPES,IERR)
                  IRRSTP=IPDCKR(OTHR2(J))
                  IF (IRRSTP.GT.0) IRRS=1
60                CONTINUE
               IF (IRRS.EQ.1) THEN
                  IREC=0
                  PARMTP='RRS'
                  CALL RPPREC (STAID,PARMTP,IREC,LARAY,ARAY,NFILL,
     *               IRECNX,IERR)
                     IF (IERR.NE.0) THEN
                     IERRN=-IERR
                     CALL SRPPST (STAID,PARMTP,IREC,LARAY,NFILL,IRECNX,
     *                  IERRN)
                     ENDIF
                  ENDIF
               ENDIF
70       CALL UINTCH (NTIME,4,CNUM1,LFILL,IERR)
         NUMSTA=IBUF1(5)
         CALL UINTCH (NUMSTA,LEN(STANB1),STANB1,LFILL,IERR)
         CALL UINTCH (NTIME+MTIME,4,CNUM2,LFILL,IERR)
         NUMSTA=IBUF2(5)
         CALL UINTCH (NUMSTA,LEN(STANB2),STANB2,LFILL,IERR)
         IF (NTIME.EQ.MTIME.AND.IEVEN.EQ.0) CNUM2=' '
         IF (NTIME.EQ.MTIME.AND.IEVEN.EQ.0) STANB2=' '
         WRITE (LP,200)
     *      CNUM1,(IBUF1(J),J=1,4),STANB1,PP24X1,TM24X1,
     *         (OTHR1(J),J=1,NPER),
     *      CNUM2,(IBUF2(J),J=1,4),STANB2,PP24X2,TM24X2,
     *         (OTHR2(J),J=1,NPER)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,170) ' '
            CALL SULINE (LP,2)
            WRITE (LP,180) ' '
            CALL SULINE (LP,1)
            ENDIF
         IF (NOTHR1.LE.NPER.AND.NOTHR2.LE.NPER) GO TO 90
            NLEFT1=NOTHR1-NPER
            IF (NLEFT1.LT.0) NLEFT1=0
            NUM1=NLEFT1/NPER
            IF (MOD(NLEFT1,NPER).GT.0) NUM1=NUM1+1
            NLEFT2=NOTHR2-NPER
            IF (NLEFT1.LT.0) NLEFT1=0
            NUM2=NLEFT2/NPER
            IF (MOD(NLEFT2,NPER).GT.0) NUM2=NUM2+1
            NUM=NUM1
            IF (NUM2.GT.NUM1) NUM=NUM2
            DO 80 J=1,NUM
               NUM1=J*NPER+1
               NUM2=(J+1)*NPER
               WRITE (LP,210) (OTHR1(N),N=NUM1,NUM2),
     *                        (OTHR2(N),N=NUM1,NUM2)
               CALL SULINE (LP,1)
               IF (ISNWPG(LP).EQ.1) THEN
                  WRITE (LP,170) ' '
                  CALL SULINE (LP,2)
                  WRITE (LP,180) ' '
                  CALL SULINE (LP,1)
                  ENDIF
80             CONTINUE
90       IF (STAIDX.NE.' ') GO TO 120
100      CONTINUE
C
      IF (STAIDX.NE.' ') THEN
         WRITE (LP,220) STAIDX
         CALL ULINE (LP,2)
         GO TO 120
         ENDIF
C
      WRITE (LP,240) NUMID,MAXID
      CALL SULINE (LP,2)
C
C  PRINT DATA TYPES FOUND
      CALL SMPPDT ('PRNT',MTYPES,ITYPES,NTYPES,IERR)
C
110   IF (NUMERR.GT.0) ISTAT=1
C
120   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSPPD2 : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT ('0*** NOTE - NO DAILY STATIONS ARE DEFINED.')
145   FORMAT ('0*** ERROR - IN SSPPD2 - CALLING ROUTINE ',A,'. ',
     *   'STATUS CODE=',I4)
150   FORMAT ('0*** ERROR - IN SSPPD2 - MAXIMUM NUMBER OF IDENTIFIERS ',
     *   'THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
160   FORMAT ('0- ATTRIBUTES OF ',A,' IN PREPROCESSOR DATA BASE -')
170   FORMAT ('0',2(5X,'STA ID  ',3X,'NUMBER',3X,'DATA TYPES',29X : A))
180   FORMAT (' ',2(5X,8('-'),3X,6('-'),3X,36('-'),3X : A))
190   FORMAT ('0*** WARNING - NUMBER OF ADDITIONAL DATA TYPES TO ',
     *   'BE PROCESSED FOR STATION ',A,' (',I3,') EXCEEDS MAXIMUM ',
     *   'ALLOWED (',I3,'). THE FIRST ',I3,' WILL BE PROCESSED.')
200   FORMAT (' ',2(A,1X,4A2,3X,A6,3X,2(A,1X),2X,5(A,1X),3X))
210   FORMAT (' ',T39,5(A,1X),T104,5(A,1X))
220   FORMAT ('0*** NOTE - STATION ',A,' NOT FOUND IN ',
     *   'PREPROCESSOR DATA BASE.')
230   FORMAT ('0*** ERROR - ACCESSING STATION ',A,'. ',
     *   'PDFNDR STATUS CODE = ',I2)
240   FORMAT ('0*** NOTE - ',I5,' STATIONS PROCESSED. A MAXIMUM OF ',
     *   I5,' STATIONS CAN BE PROCESSED.')
C
      END
