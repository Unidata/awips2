C MODULE SURIDS
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ IDENTIFIERS FROM THE PARAMETRIC DATA BASE.
C
      SUBROUTINE SURIDS (TYPE,ISORT,MAXID,ISTATE,NWORDS,ISRTBY,
     *   IPNTRS,NUMID,LARRAY,ARRAY,IPRMSG,IPRERR,ISTAT)
C
      CHARACTER*4 TYPE,XSORT,RDISP,UNITS
      CHARACTER*4 BYID/'ID'/
      CHARACTER*4 BYDESC/'DESC'/
      CHARACTER*4 BYNUM/'NUM'/
      CHARACTER*8 XIDENT
C
      DIMENSION ARRAY(LARRAY)
      CHARACTER*2 CSTATE1,CSTATE2
      INTEGER*2 ISTATE(MAXID)
      CHARACTER*4 ISRTBY
      DIMENSION ISRTBY(NWORDS,MAXID)
      DIMENSION IPNTRS(MAXID)
C
      DIMENSION UNUSED(5)
      INCLUDE 'scommon/dimstan'
      INCLUDE 'scommon/dimgbox'
      INCLUDE 'scommon/dimrfro'
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/surids.f,v $
     . $',                                                             '
     .$Id: surids.f,v 1.5 2002/02/11 20:55:56 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SURIDS'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' TYPE=',TYPE,
     *      ' ISORT=',ISORT,
     *      ' MAXID=',MAXID,
     *      ' NWORDS=',NWORDS,
     *      ' IPNTRS(1)=',IPNTRS(1),
     *      ' LARRAY=',LARRAY,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NUMID=0
C
      ISSTA=0
      ISPTR=0
      IF (IPNTRS(1).EQ.1) ISPTR=1
C
C  CHECK SORT VALUE - VALID VALUES ARE:
C     0=NOT SORTED
C     1=BY IDENTIFIER
C     2=BY DESCRIPTION
C     3=BY NUMBER
C  NEGATIVE INDICATES ONLY STATIONS WITH A STATUS OF COMPLETE SHOULD
C  BE PROCESSED.
      ICOMPL=0
      IF (ISORT.LT.0) ICOMPL=1
      ISORTA=IABS(ISORT)
      CALL UREPET ('?',XSORT,LEN(XSORT))
      IF (ISORTA.EQ.0) XSORT='NONE'
      IF (ISORTA.EQ.1) XSORT=BYID
      IF (ISORTA.EQ.2) XSORT=BYDESC
      IF (ISORTA.EQ.3) XSORT=BYNUM
      IF (ISORTA.GE.0.OR.ISORTA.LE.3) THEN
         ELSE
            WRITE (LP,440) ISORT
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 410
         ENDIF
C
C  CHECK IF READING STATION PARAMETERS
      IF (TYPE.EQ.'STAN'.OR.
     *    TYPE.EQ.'PCPN'.OR.
     *    TYPE.EQ.'TEMP'.OR.
     *    TYPE.EQ.'PE'.OR.
     *    TYPE.EQ.'RRS') GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF READING GRID BOX PARAMETERS
      IF (TYPE.EQ.'GBOX') GO TO 110
C
C  CHECK IF READING RAINFALL-RUNOFF RELATIONSHIP PARAMTERS
      IF (TYPE.EQ.'RFRO') GO TO 150
C
C  CHECK FOR INVALID TYPE
      IF (TYPE.EQ.'FMAP') THEN
         WRITE (LP,450) TYPE
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 410
         ENDIF
C
C  SORT BY IDENTIFIER AND DESCRIPTION VALID FOR NON-STATION TYPES
      IF (ISORTA.EQ.0.OR.ISORTA.EQ.1) THEN
         ELSE
            WRITE (LP,460) XSORT,TYPE,BYID
            CALL SUWRNS (LP,2,-1)
            ISORTA=1
         ENDIF
C
C  SORT BY DESCRIPTION VALID ONLY FOR CERTAIN TYPES
      IF (ISORTA.EQ.2) THEN
         IF (TYPE.EQ.'BASN'.OR.
     *       TYPE.EQ.'MAP'.OR.
     *       TYPE.EQ.'MAT'.OR.
     *       TYPE.EQ.'MAPE') THEN
            ELSE
               WRITE (LP,460) XSORT,TYPE,BYID
               CALL SUWRNS (LP,2,-1)
               ISORTA=1
            ENDIF
         ENDIF
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.GT.0) GO TO 190
C
      NTYPE=0
      IPTRNX=0
C
C  READ PARAMETER RECORD
10    CALL UREPET (' ',XIDENT,LEN(XIDENT))
      IPTR=IPTRNX
      CALL RPPREC (XIDENT,TYPE,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.6) GO TO 30
         IF (IERR.EQ.2.OR.IERR.EQ.4) THEN
            ISTAT=2
            GO TO 30
            ENDIF
         CALL SRPPST (XIDENT,TYPE,IPTR,LARRAY,NFILL,IPTRNX,IERR)
         ISTAT=1
         GO TO 410
         ENDIF
      IF (NUMID+1.GT.MAXID) THEN
         WRITE (LP,490) MAXID
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 30
         ENDIF
      NUMID=NUMID+1
      IF (ISPTR.EQ.1) IPNTRS(NUMID)=IPTR
      IF (ISORTA.EQ.0.OR.ISORTA.EQ.1) THEN
         CALL SUBSTR (XIDENT,1,8,ISRTBY(1,NUMID),1)
         GO TO 20
         ENDIF
      CALL SUBSTR (ARRAY,13,20,ISRTBY(1,NUMID),1)
20    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,470) (ISRTBY(I,NUMID),I=1,2),
     *      IPNTRS(NUMID)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NTYPE=NTYPE+1
C
      IF (IPTRNX.GT.0) GO TO 10
C
30    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' TYPE=',TYPE,
     *      ' NTYPE=',NTYPE,
     *      ' '
         CALL ULINE (IOSDBG,2)
         ENDIF
C
      GO TO 190
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ STATION GENERAL PARAMETERS
C
40    NSTAN=0
      RDISP='OLD'
      IPRNT=1
      UNITS='ENGL'
      IPTRNX=0
      IF (ISTATE(1).EQ.1) ISSTA=1
C
50    CALL UREPET (' ',STAID,8)
      IPTR=IPTRNX
      INCLUDE 'scommon/callsrstan'
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.6) GO TO 100
         IF (IERR.EQ.2) THEN
            ISTAT=2
            GO TO 100
            ENDIF
         WRITE (LP,500) 'STAN',IPTR,'STAN',IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 410
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,510) STAID,DESCRP
         CALL SULINE (IOSDBG,1)
      INCLUDE 'scommon/callspstan'
         ENDIF
C
      IF (ICOMPL.EQ.0) GO TO 60
         IF (ICSTAN.EQ.0) GO TO 60
            WRITE (LP,480) STAID
            CALL SULINE (LP,2)
            GO TO 90
C
C  CHECK TYPE BEING PROCESSED
60    IGPS=0
      IF (TYPE.EQ.'STAN') GO TO 80
         DO 70 IGPS=1,NGPS
            IF (GPS(IGPS).EQ.TYPE) GO TO 80
70          CONTINUE
         GO TO 90
80    IF (NUMID+1.GT.MAXID) THEN
         WRITE (LP,490) MAXID
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 100
         ENDIF
      NUMID=NUMID+1
CCC      CALL SUBSTR (STATE,1,2,NSTATE,3)
CCC      IF (ISSTA.EQ.1) ISTATE(NUMID)=NSTATE
      IF (ISSTA.EQ.1) THEN
         CALL SUBSTR (STATE,1,2,ISTATE(NUMID),1)
         ENDIF
      IF (ISORTA.EQ.0) CALL SUBSTR (STAID,1,8,ISRTBY(1,NUMID),1)
      IF (ISORTA.EQ.1) CALL SUBSTR (STAID,1,8,ISRTBY(1,NUMID),1)
      IF (ISORTA.EQ.2) CALL SUBSTR (DESCRP,1,20,ISRTBY(1,NUMID),1)
      IF (ISORTA.EQ.3) CALL SUBSTR (STAID,1,8,ISRTBY(1,NUMID),5)
      IF (ISORTA.EQ.3) CALL SUBSTR (NBRSTA,1,4,ISRTBY(1,NUMID),1)
      IF (ISPTR.EQ.1) THEN
         IF (TYPE.EQ.'STAN') IPNTRS(NUMID)=IPTR
         IF (TYPE.NE.'STAN') IPNTRS(NUMID)=IPARM(IGPS)
         ENDIF
      IF (LDEBUG.GT.0) THEN
         IF (ISSTA.EQ.0.AND.ISPTR.EQ.0) THEN
            WRITE (IOSDBG,550) NUMID,
     *         (ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ISSTA.EQ.1.AND.ISPTR.EQ.0) THEN
            WRITE (IOSDBG,560) NUMID,
     *         ISTATE(NUMID),(ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ISSTA.EQ.0.AND.ISPTR.EQ.1) THEN
            WRITE (IOSDBG,570) NUMID,
     *         IPNTRS(NUMID),(ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ISSTA.EQ.1.AND.ISPTR.EQ.1) THEN
            WRITE (IOSDBG,580) NUMID,
     *         IPNTRS(NUMID),ISTATE(NUMID),(ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         ENDIF
C
90    NSTAN=NSTAN+1
C
      IF (IPTRNX.GT.0) GO TO 50
C
100   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' TYPE=',TYPE,
     *      ' NSTAN=',NSTAN,
     *      ' '
         CALL ULINE (IOSDBG,2)
         ENDIF
C
      GO TO 190
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ GRID BOX PARAMETERS
C
110   NGBOX=0
      IPTRNX=0
C
120   CALL UREPET (' ',GBOXID,8)
      IPTR=IPTRNX
      INCLUDE 'scommon/callsrgbox'
      IF (IERR.EQ.0) GO TO 130
         IF (IERR.EQ.6) GO TO 140
         IF (IERR.EQ.2) THEN
            ISTAT=2
            GO TO 140
            ENDIF
         WRITE (LP,500) 'GBOX',IPTR,'GBOX',IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 410
C
130   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,520) GBOXID,NUGBOX
         CALL SULINE (IOSDBG,1)
      INCLUDE 'scommon/callspgbox'
         ENDIF
C
      IF (NUMID+1.GT.MAXID) THEN
         WRITE (LP,490) MAXID
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 140
         ENDIF
      NUMID=NUMID+1
      IF (ISORTA.EQ.0) CALL SUBSTR (GBOXID,1,8,ISRTBY(1,NUMID),1)
      IF (ISORTA.EQ.1) CALL SUBSTR (GBOXID,1,8,ISRTBY(1,NUMID),1)
      IF (ISORTA.EQ.3) CALL SUBSTR (GBOXID,1,8,ISRTBY(1,NUMID),5)
      IF (ISORTA.EQ.3) CALL SUBSTR (NUGBOX,1,4,ISRTBY(1,NUMID),1)
      IF (ISPTR.EQ.1) IPNTRS(NUMID)=IPTR
      IF (LDEBUG.GT.0) THEN
         IF (ISPTR.EQ.0) THEN
            WRITE (IOSDBG,550) NUMID,
     *         (ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ISPTR.EQ.1) THEN
            WRITE (IOSDBG,570) NUMID,
     *         IPNTRS(NUMID),(ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         ENDIF
C
      NGBOX=NGBOX+1
C
      IF (IPTRNX.GT.0) GO TO 120
C
140   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' TYPE=',TYPE,
     *      ' NGBOX=',NGBOX,
     *      ' '
         CALL ULINE (IOSDBG,2)
         ENDIF
C
      GO TO 190
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ RAINFALL-RUNOFF RELATIONSHIP PARAMETERS
C
150   NRFRO=0
      IPTRNX=0
C
160   CALL UREPET (' ',RFROID,8)
      IPTR=IPTRNX
      INCLUDE 'scommon/callsrrfro'
      IF (IERR.EQ.0) GO TO 170
         IF (IERR.EQ.6) GO TO 180
         IF (IERR.EQ.2) THEN
            ISTAT=2
            GO TO 180
            ENDIF
         WRITE (LP,500) 'RFRO',IPTR,'RFRO',IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 410
C
170   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,520) RFROID,NURFRO
         CALL SULINE (IOSDBG,1)
      INCLUDE 'scommon/callsprfro'
        ENDIF
C
      IF (NUMID+1.GT.MAXID) THEN
         WRITE (LP,490) MAXID
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 180
         ENDIF
      NUMID=NUMID+1
      IF (ISORTA.EQ.0) CALL SUBSTR (RFROID,1,8,ISRTBY(1,NUMID),1)
      IF (ISORTA.EQ.1) CALL SUBSTR (RFROID,1,8,ISRTBY(1,NUMID),1)
      IF (ISORTA.EQ.3) CALL SUBSTR (RFROID,1,8,ISRTBY(1,NUMID),5)
      IF (ISORTA.EQ.3) CALL SUBSTR (NURFRO,1,4,ISRTBY(1,NUMID),1)
      IF (ISPTR.EQ.1) IPNTRS(NUMID)=IPTR
      IF (LDEBUG.GT.0) THEN
         IF (ISPTR.EQ.0) THEN
            WRITE (IOSDBG,550) NUMID,
     *         (ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (ISPTR.EQ.1) THEN
            WRITE (IOSDBG,570) NUMID,
     *         IPNTRS(NUMID),(ISRTBY(N,NUMID),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
            ENDIF
         ENDIF
C
      NRFRO=NRFRO+1
C
      IF (IPTRNX.GT.0) GO TO 160
C
C
180   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' TYPE=',TYPE,
     *      ' NRFRO=',NRFRO,
     *      ' '
         CALL ULINE (IOSDBG,2)
         ENDIF
C
      GO TO 190
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
190   NUM=NUMID-1
      IF (NUM.GT.0) GO TO 200
         IF (NUMID.EQ.1) GO TO 410
            ISTAT=2
            GO TO 410
C
200   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'BEFORE SORTING'
         CALL SULINE (IOSDBG,2)
         DO 210 I=1,NUMID
            IF (ISSTA.EQ.0) THEN
               WRITE (IOSDBG,570) I,IPNTRS(I),
     *            (ISRTBY(N,I),N=1,NWORDS)
               CALL SULINE (IOSDBG,1)
               ELSE
                  WRITE (IOSDBG,580) I,IPNTRS(I),ISTATE(I),
     *               (ISRTBY(N,I),N=1,NWORDS)
                  CALL SULINE (IOSDBG,1)
               ENDIF
210         CONTINUE
         ENDIF
C
      IF (ISORTA.EQ.0) GO TO 410
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (ISSTA.EQ.0) GO TO 260
C
C  SORT BY STATE
      DO 230 N=1,NUM
         IND=0
         DO 220 I=1,NUM
            CALL SUBSTR (ISTATE(I),1,2,CSTATE1,1)
            CALL SUBSTR (ISTATE(I+1),1,2,CSTATE2,1)
            IF (CSTATE1.LE.CSTATE2) GO TO 220
               CALL SURID2 (NWORDS,I,ISSTA,ISTATE,ISRTBY,ISPTR,IPNTRS)
               IND=1
220         CONTINUE
            IF (IND.EQ.0) GO TO 240
230      CONTINUE
C
240   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'SORTED BY STATE'
         CALL SULINE (IOSDBG,2)
         DO 250 I=1,NUMID
            WRITE (IOSDBG,580) I,IPNTRS(I),ISTATE(I),
     *         (ISRTBY(N,I),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
250         CONTINUE
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SORT BY FIRST WORD
260   DO 280 N=1,NUM
         IND=0
         DO 270 I=1,NUM
            IF (ISSTA.GT.0) THEN
               CALL SUBSTR (ISTATE(I),1,2,CSTATE1,1)
               CALL SUBSTR (ISTATE(I+1),1,2,CSTATE2,1)
               IF (CSTATE1.NE.CSTATE2) GO TO 270
               ENDIF
            IF (ISRTBY(1,I).LE.ISRTBY(1,I+1)) GO TO 270
               CALL SURID2 (NWORDS,I,ISSTA,ISTATE,ISRTBY,ISPTR,IPNTRS)
               IND=1
270         CONTINUE
            IF (IND.EQ.0) GO TO 290
280      CONTINUE
C
290   IF (NWORDS.EQ.1) GO TO 410
C
C  SORT BY SECOND WORD
      DO 310 N=1,NUM
         IND=0
         DO 300 I=1,NUM
            IF (ISSTA.GT.0) THEN
               CALL SUBSTR (ISTATE(I),1,2,CSTATE1,1)
               CALL SUBSTR (ISTATE(I+1),1,2,CSTATE2,1)
               IF (CSTATE1.NE.CSTATE2) GO TO 300
               ENDIF
            IF (ISRTBY(1,I).NE.ISRTBY(1,I+1)) GO TO 300
            IF (ISRTBY(2,I).LE.ISRTBY(2,I+1)) GO TO 300
               CALL SURID2 (NWORDS,I,ISSTA,ISTATE,ISRTBY,ISPTR,IPNTRS)
               IND=1
300         CONTINUE
            IF (IND.EQ.0) GO TO 320
310      CONTINUE
C
320   IF (NWORDS.EQ.2) GO TO 410
C
C  SORT BY THIRD WORD
      DO 340 N=1,NUM
         IND=0
         DO 330 I=1,NUM
            IF (ISSTA.GT.0) THEN
               CALL SUBSTR (ISTATE(I),1,2,CSTATE1,1)
               CALL SUBSTR (ISTATE(I+1),1,2,CSTATE2,1)
               IF (CSTATE1.NE.CSTATE2) GO TO 330
               ENDIF
            IF (ISRTBY(1,I).NE.ISRTBY(1,I+1)) GO TO 330
            IF (ISRTBY(2,I).NE.ISRTBY(2,I+1)) GO TO 330
            IF (ISRTBY(3,I).LE.ISRTBY(3,I+1)) GO TO 330
               CALL SURID2 (NWORDS,I,ISSTA,ISTATE,ISRTBY,ISPTR,IPNTRS)
               IND=1
330         CONTINUE
            IF (IND.EQ.0) GO TO 350
340      CONTINUE
C
350   IF (NWORDS.EQ.3) GO TO 410
C
C  SORT BY FOURTH WORD
      DO 370 N=1,NUM
         IND=0
         DO 360 I=1,NUM
            IF (ISSTA.GT.0) THEN
               CALL SUBSTR (ISTATE(I),1,2,CSTATE1,1)
               CALL SUBSTR (ISTATE(I+1),1,2,CSTATE2,1)
               IF (CSTATE1.NE.CSTATE2) GO TO 360
               ENDIF
            IF (ISRTBY(1,I).NE.ISRTBY(1,I+1)) GO TO 360
            IF (ISRTBY(2,I).NE.ISRTBY(2,I+1)) GO TO 360
            IF (ISRTBY(3,I).NE.ISRTBY(3,I+1)) GO TO 360
            IF (ISRTBY(4,I).LE.ISRTBY(4,I+1)) GO TO 360
               CALL SURID2 (NWORDS,I,ISSTA,ISTATE,ISRTBY,ISPTR,IPNTRS)
               IND=1
360         CONTINUE
            IF (IND.EQ.0) GO TO 380
370      CONTINUE
C
380   IF (NWORDS.EQ.4) GO TO 410
C
C  SORT BY FIFTH WORD
      DO 400 N=1,NUM
         IND=0
         DO 390 I=1,NUM
            IF (ISSTA.GT.0) THEN
               CALL SUBSTR (ISTATE(I),1,2,CSTATE1,1)
               CALL SUBSTR (ISTATE(I+1),1,2,CSTATE2,1)
               IF (CSTATE1.NE.CSTATE2) GO TO 390
               ENDIF
            IF (ISRTBY(1,I).NE.ISRTBY(1,I+1)) GO TO 390
            IF (ISRTBY(2,I).NE.ISRTBY(2,I+1)) GO TO 390
            IF (ISRTBY(3,I).NE.ISRTBY(3,I+1)) GO TO 390
            IF (ISRTBY(4,I).NE.ISRTBY(4,I+1)) GO TO 390
            IF (ISRTBY(5,I).LE.ISRTBY(5,I+1)) GO TO 390
               CALL SURID2 (NWORDS,I,ISSTA,ISTATE,ISRTBY,ISPTR,IPNTRS)
               IND=1
390         CONTINUE
            IF (IND.EQ.0) GO TO 410
400      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
410   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NUMID=',NUMID
         CALL SULINE (IOSDBG,1)
         IF (NUMID.GT.0) THEN
            WRITE (IOSDBG,*) 'AFTER SORTING'
            CALL SULINE (IOSDBG,1)
            DO 420 I=1,NUMID
               IF (ISSTA.EQ.0.AND.ISPTR.EQ.0) THEN
                  WRITE (IOSDBG,550) I,
     *               (ISRTBY(N,I),N=1,NWORDS)
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (ISSTA.EQ.1.AND.ISPTR.EQ.0) THEN
                  WRITE (IOSDBG,560) I,
     *               ISTATE(I),(ISRTBY(N,I),N=1,NWORDS)
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (ISSTA.EQ.0.AND.ISPTR.EQ.1) THEN
                  WRITE (IOSDBG,570) I,
     *               IPNTRS(I),(ISRTBY(N,I),N=1,NWORDS)
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               IF (ISSTA.EQ.1.AND.ISPTR.EQ.1) THEN
                  WRITE (IOSDBG,580) I,
     *               IPNTRS(I),ISTATE(I),(ISRTBY(N,I),N=1,NWORDS)
                  CALL SULINE (IOSDBG,1)
                  ENDIF
420            CONTINUE
            ENDIF
         ENDIF
C
      IF (IPRMSG.EQ.1) THEN
         WRITE (LP,600) NUMID,TYPE,MAXID
         CALL SULINE (LP,2)
         IF (ISORTA.EQ.0) THEN
            WRITE (LP,610) TYPE
            CALL SULINE (LP,2)
            ELSE
               CALL UREPET ('?',XSORT,LEN(XSORT))
               IF (ISORTA.EQ.1) XSORT=BYID
               IF (ISORTA.EQ.2) XSORT=BYDESC
               IF (ISORTA.EQ.3) XSORT=BYNUM
               IF (ISSTA.EQ.0) THEN
                  WRITE (LP,620) TYPE,XSORT
                  CALL SULINE (LP,2)
                  ENDIF
               IF (ISSTA.EQ.1) THEN
                  WRITE (LP,630) TYPE,XSORT
                  CALL SULINE (LP,2)
                  ENDIF
            ENDIF
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SURIDS : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
440   FORMAT ('0*** ERROR - IN SURIDS - INVALID SORT VALUE : ',I3)
450   FORMAT ('0*** ERROR - IN SURIDS - INVALID PARAMETER TYPE : ',A4)
460   FORMAT ('0*** WARNING - IN SURIDS - SORT BY ',A,' IS NOT ',
     *   'ALLOWED FOR TYPE ',A,'. SORT OPTION SET TO BY ',A,'.')
470   FORMAT (' ISRTBY=',2A4,3X,'IPNTRS=',I5)
480   FORMAT ('0*** NOTE - STATION DEFINITION STATUS FOR STATION ',
     *   2A4,' IS INCOMPLETE.')
490   FORMAT ('0*** ERROR - IN SURIDS - MAXIMUM IDENTIFIERS THAT CAN ',
     *   'BE PROCESSED (',I5,') EXCEEDED.')
500   FORMAT ('0*** ERROR - IN SURIDS - ERROR READING ',A,
     *   ' PARAMETERS FOR RECORD NUMBER ',I5,'. STATUS CODE ',
     *   'FROM SR',A,' IS ',I3,'.')
510   FORMAT (' STAID=',2A4,3X,'DESCRP=',5A4)
520   FORMAT (' GBOXID=',2A4,3X,'NUGBOX=',I2)
550   FORMAT (' ',I5,3X,'ISRTBY=',5A4)
560   FORMAT (' ',I5,3X,'ISTATE=',A2,3X,'ISRTBY=',5A4)
570   FORMAT (' ',I5,3X,'IPNTRS=',I5,3X,'ISRTBY=',5A4)
580   FORMAT (' ',I5,3X,'IPNTRS=',I5,3X,'ISTATE=',A2,3X,'ISRTBY=',5A4)
600   FORMAT ('0*** NOTE - ',I5,' ',A,' IDENTIFIERS PROCESSED ',
     *   'FROM PARAMETRIC DATA BASE. ',
     *   'A MAXIMUM OF ',I5,' CAN BE PROCESSED.')
610   FORMAT ('0*** NOTE - ',A,' IDENTIFIERS WERE NOT SORTED.')
620   FORMAT ('0*** NOTE - ',A,' IDENTIFIERS WERE SORTED BY ',A,'.')
630   FORMAT ('0*** NOTE - ',A,' IDENTIFIERS WERE SORTED FIRST BY ',
     *   'STATE AND THEN BY ',A,'.')
C
      END
