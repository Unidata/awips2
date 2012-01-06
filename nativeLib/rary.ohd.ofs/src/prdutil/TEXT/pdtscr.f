C MEMBER PDTSCR
C  (from old member PRDDEFTS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/17/95.11:58:32 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDTSCR (ICMD,LWKBUF,IWKBUF)
C
C          ROUTINE:  PDTSCR
C
C             VERSION:  1.0.0
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL READ CARDS CONTAINING TIME SERIES
C    DESCRIPTIVE INFORMATION AND PARSE THE INFO INTO THE HEADER
C    ITEMS.  IT WILL DO A DEFTS OR CHNGTS TO CHANGE A TIME SERIES
C    CHECKING IS FOR SYNTAX ONLY.  THE CREATE ROUTINE (WPRDH(F))
C    CHECKS THE VALIDITY OF THE HEADER INFORMATION
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION LSTCMD(1),ITSID(2),RLOCT(2),INFO(5)
      DIMENSION ITSIDF(2)
      DIMENSION XBUF(20)
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'prdcommon/pdatas'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/pdtscr.f,v $
     . $',                                                             '
     .$Id: pdtscr.f,v 1.1 1995/09/17 19:16:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LSTCMD /4HEND /,NCMD/1/
      DATA LETX/4HX   /
C
C***********************************************************************
C
C
      IF (ICMD.LT.1.OR.ICMD.GT.4) GO TO 290
C
      CALL RCOMND (LSTCMD,NCMD,INDXC)
      IF (INDXC) 310,10,220
C
C  READ CARD
10    IERR=0
      CALL WPCARD (IBUF)
      CALL UMEMST (IBLNK,ITSID,2)
      CALL UMEMST (IBLNK,ITSIDF,2)
      ITYPE=IBLNK
C
C MUST BE AT LEAST 6 FIELDS
      IF (NFIELD.GE.6) GO TO 30
      WRITE (LP,20)
      IERR=1
20    FORMAT ('0**ERROR** MUST BE AT LEAST 6 FIELDS ON FIRST CARD')
      GO TO 150
C
C  PACK TIME SERIES ID
30    J=IFSTRT(1)
      N=IFSTOP(1)-J+1
      IF (N.LE.8) GO TO 50
      WRITE (LP,40)
40    FORMAT ('0**ERROR** TIME SERIES ID LIMITED TO 8 CHARACTERS.')
      IERR=1
      N=8
50    CALL UPACK1 (IBUF(J),ITSID,N)
C
C  GET DATA TYPE
      J=IFSTRT(2)
      N=IFSTOP(2)-J+1
      IF (N.LE.4) GO TO 70
      WRITE (LP,60)
60    FORMAT ('0**ERROR** DATA TYPE LIMITED TO 4 CHARACTERS.')
      IERR=1
      N=4
70    CALL UPACK1 (IBUF(J),ITYPE,N)
C
C  GET DATA TIME INTERVAL
      I=3
      CALL UINTFX (ITSTEP,IFSTRT(I),IFSTOP(I),IERR)
C
C  GET DATA UNITS
      J=IFSTRT(4)
      N=IFSTOP(4)-J+1
      IF (N.LE.4) GO TO 90
      WRITE (LP,80)
80    FORMAT ('0**ERROR** UNITS LIMITED TO 4 CHARACTERS.')
      IERR=1
      N=4
90    CALL UPACK1 (IBUF(J),IUNIT,N)
C
C  GET LOCATION (LAT/LONG)
      I=5
      ID=0
      DO 100 J=1,2
         CALL URELFX (RLOCT(J),IFSTRT(I),IFSTOP(I),IERR,ID)
         I=I+1
100      CONTINUE
      CALL UMEMST (IBLNK,ITSIDF,2)
C
C  SET NVAL TO 0
      NVAL=0
C
C  CHECK IF HAVE MORE FIELDS TO PROCESS
      IF (NFIELD.LE.6) GO TO 150
C
C IF NOT INTEGER, MUST BE FUTURE ID
      IF (IFTYPE(7).EQ.1) GO TO 140
C
C  IF FUTURE TYPE, SHOULD NOT HAVE THIS FIELD
      IF (ICMD.EQ.1.OR.ICMD.EQ.3) GO TO 120
      WRITE (LP,110)
110   FORMAT ('0**ERROR** FUTURE TIME SERIES CANNOT HAVE A FUTURE ID.')
      IERR=1
C
C  GET FUTURE ID
120   J=IFSTRT(7)
      N=IFSTOP(7)-J+1
      IF (N.LE.8) GO TO 130
         WRITE (LP,40)
         IERR=1
         GO TO 150
130   CALL UPACK1 (IBUF(J),ITSIDF,N)
      IF (NFIELD.LE.7) GO TO 150
C
140   CALL UINTFX (NVAL,IFSTRT(NFIELD),IFSTOP(NFIELD),IERR)
C
C  NOW READ DESCRIPTIVE INFORMATION ON ANOTHER CARD
150   CALL RCOMND (LSTCMD,NCMD,INDXC)
      CALL WPCARD (IBUF)
      IF (INDXC) 270,160,270
C
C  GET DESCRIPTION
160   J=IFSTRT(1)
      CALL UPACK1 (IBUF(J),INFO,20)
C
C  CHECK IF XBUF CARD
      NX=0
      IF (ICMD.GT.2) NX=-1
      CALL RCOMND (LSTCMD,NCMD,INDXC)
      IF (INDXC) 310,170,180
170   IF (IBUF(1).NE.LETX) GO TO 180
      CALL WPCARD (IBUF)
      NX=IFSTOP(2)-IFSTRT(2)+1
      CALL UPACK1 (IBUF(IFSTRT(2)),XBUF,NX)
      NX=(NX+3)/4
C
C READ NEXT CARD
      CALL RCOMND (LSTCMD,NCMD,INDXC)
C
C  CHECK IF ERRORS ENCOUNTERED
180   IF (IERR.NE.0) GO TO 250
C
      IF (ICMD.GT.2) GO TO 230
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CREATE TIME SERIES
C
      IF (IPRDB.GT.0) WRITE (IOGDB,190) ITSID,ITYPE,ITSTEP,IUNIT,RLOCT,
     *   NVAL,ITSIDF,INFO,NX
190   FORMAT ('** CALLING WPRDH WITH ID: ',2A4,' TYPE: ',A4,' DT: ',I4,
     * ' UNITS: ',A4,' LAT/LONG: ',2F8.2,' NVAL ',I2,' FID:',2A4,/
     *  '  DESCRP: ',5A4,' NX=',I6)
C
      IF (ICMD.EQ.1) CALL WPRDH (ITSID,ITYPE,ITSTEP,IUNIT,NVAL,RLOCT,
     *   ITSIDF,INFO,NX,XBUF,LWKBUF,IWKBUF,IRECNO,ISTAT)
C
      IF (ICMD.EQ.2) CALL WPRDFH (ITSID,ITYPE,ITSTEP,IUNIT,NVAL,RLOCT,
     *   INFO,NX,XBUF,LWKBUF,IWKBUF,IRECNO,ISTAT)
C
      IF (ISTAT.NE.0) GO TO 250
C
      IF (ICMD.EQ.1) WRITE (LP,200) ITSID,ITYPE
200   FORMAT (' **NOTE** TIME SERIES FOR ID ',2A4,' AND TYPE ',A4,
     *   ' SUCCESSFULLY CREATED.')
C
      IF (ICMD.EQ.2) WRITE (LP,210) ITSID,ITYPE
210   FORMAT (' **NOTE** FUTURE TIME SERIES FOR ID ',2A4,
     *   ' AND TYPE ',A4,
     *   ' SUCCESSFULLY CREATED.')
C
      IF (INDXC) 310,10,220
C
220   CALL WPCARD (IBUF)
      GO TO 330
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C   CHANGE TIME SERIES
C
230   IF (IPRDB.GT.0) WRITE (IOGDB,240) ITSID,ITYPE,ITSTEP,IUNIT,RLOCT,
     *   NVAL,ITSIDF,INFO,NX
240   FORMAT ('** CALLING PRDCTS WITH ID: ',2A4,' TYPE: ',A4,' DT: ',I4,
     * ' UNITS: ',A4,' LAT/LONG: ',2F8.2,' NVAL ',I2,' FID:',2A4,/
     *  '  DESCRP: ',5A4,' NX=',I6)
C
      IF (ICMD.EQ.3) CALL PRDCTS (ITSID,ITYPE,ITSTEP,IUNIT,NVAL,RLOCT,
     *   ITSIDF,INFO,NX,XBUF,LWKBUF,IWKBUF,ISTAT)
C
      IF (ICMD.EQ.4) CALL PRDCTF (ITSID,ITYPE,ITSTEP,IUNIT,NVAL,RLOCT,
     *   INFO,NX,XBUF,LWKBUF,IWKBUF,ISTAT)
C
      IF (INDXC) 310,10,220
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
250   WRITE (LP,260) ITSID,ITYPE
260   FORMAT ('0**ERROR** TIME SERIES FOR ID ',2A4,' AND TYPE ',A4,
     *   ' NOT SUCCESSFULLY CREATED.')
      IF (INDXC) 310,10,220
C
270   WRITE (LP,280)
280   FORMAT ('0**ERROR** 2 CARDS ARE NECESSARY TO CREATE A TIME ',
     *   'SERIES.')
      WRITE (LP,260) ITSID,ITYPE
      GO TO 330
C
290   WRITE (LP,300)
300   FORMAT ('0**ERROR** NO COMMAND FOUND.')
      GO TO 330
C
310   WRITE (LP,320)
320   FORMAT ('0**ERROR** MISSING END CARD')
C
330   RETURN
C
      END
