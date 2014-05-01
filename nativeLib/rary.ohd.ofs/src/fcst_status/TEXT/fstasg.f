C MODULE FSTASG
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT SEGMENT STATUS INFORMATION
C
      SUBROUTINE FSTASG (NOOBSO,ISEGTB)
C
C  THE SEGMENT INFORMATION IS PRINTED IN 3 TABLES:
C    - SEGMENT CONNECTIVITY TABLE
C    - SEGMENT DESCRIPTION TABLE PART 1
C    - SEGMENT DESCRIPTION TABLE PART 2
C
C  ROUTINE ORIGINALLY WRITTEN BY ED JOHNSON - HRL - 12/1979
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
C
      CHARACTER*8 RTNNAM,OPNOLD,CHKID,SEGID
      CHARACTER*50 DPART
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_status/RCS/fstasg.f,v $
     * $',                                                             '
     .$Id: fstasg.f,v 1.5 2002/02/11 20:21:51 dws Exp $
     * $' /
C    ===================================================================
C
C
      RTNNAM='FSTASG'
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      IBUG=0
C
C  PRINT PAGE HEADER
      IFIRST=0
      CALL FSTAHD (IFIRST)
      WRITE (IPR,10)
10    FORMAT ('0- SEGMENT STATUS -')
C
C  FILL COMMON BLOCK FCSEGP FROM SEGMENT POINTER FILE
      IREC=1
      CALL UREADT (KFSGPT,IREC,NS,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      IREC=2
      CALL UREADT (KFSGPT,IREC,NRP,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
C
      IF (NRSTS.EQ.0) THEN
         WRITE (IPR,20)
20       FORMAT ('0**NOTE** NO SEGMENTS ARE DEFINED.')
         GO TO 240
         ENDIF
C
C  SET MAXIMUM NUMBER OF SEGMENTS PER PAGE
      NMAX=75
C
      ICKPRT=0
      NOBSOL=0
C
C  PROCESS EACH OUTPUT PART
      DO 200 IPART=1,3
C     CHECK IF THE PART BEING PRINTED IS WANTED
         IF (IPART.EQ.1.AND.MOD(ISEGTB,2).NE.0) GO TO 200
         IF (IPART.EQ.2.AND.MOD(ISEGTB,3).NE.0) GO TO 200
         IF (IPART.EQ.3.AND.MOD(ISEGTB,5).NE.0) GO TO 200
         IPAGE=1
         NSGPG=1
         INEWPG=1
         NSEGSP=0
         NSEGST=0
         DO 190 IRSEG=1,NRSTS
            CALL UREADT (KFSGST,IRSEG,IDSEGN,ISTAT)
            IF (ISTAT.NE.0) GO TO 240
            NSEGST=NSEGST+1
            IF (INEWPG.EQ.1) THEN
C           PRINT HEADING FOR NEW PAGE
               IF (NSEGST.GT.1) THEN
                  CALL FSTAHD (IFIRST)
                  WRITE (IPR,75)
75    FORMAT ('0- SEGMENT STATUS (CONTINUED) -')
                  ENDIF
               INEWPG=0
               IF (IPART.EQ.1) THEN
                  DPART='SEGMENT CONNECTIVITY TABLE'
                  ENDIF
               IF (IPART.EQ.2) THEN
                  DPART='SEGMENT DESCRIPTION TABLE : PART 1'
                  ENDIF
               IF (IPART.EQ.3) THEN
                  DPART='SEGMENT DESCRIPTION TABLE : PART 2'
                  ENDIF
               WRITE (IPR,80) DPART(1:LENSTR(DPART))
80    FORMAT ('0',A,A)
               IF (IPART.EQ.1) WRITE (IPR,90)
               IF (IPART.EQ.2) WRITE (IPR,100)
               IF (IPART.EQ.3) WRITE (IPR,110)
               ENDIF
C        PRINT IF NOT OBSOLETE SEGMENT
            CALL UMEMOV (IDSEGN,CHKID,LEN(CHKID)/4)
            IF (CHKID.EQ.'OBSOLETE') THEN
               IF (ICKPRT.EQ.0) ICKPRT=IPART
C           ONLY COUNT OBSOLETE SEGMENT IN FIRST PART GONE THROUGH
C           OTHERWISE WOULD DOUBLE OR TRIPLE COUNT OBSOLETE SEGMENTS
               IF (ICKPRT.EQ.IPART) NOBSOL=NOBSOL+1
               IF (NOOBSO.EQ.1) GO TO 190
               ENDIF
            NSEGSP=NSEGSP+1
            IF (NSEGSP.EQ.478) THEN
CCC               WRITE (IPR,*) 'NSEGSP=',NSEGSP
               ENDIF
            IF (IPART.EQ.1) THEN
               WRITE (IPR,140) NSEGSP,IDSEGN,IUPSEG,IDNSEG
               DO 183 I=1,5
                  CALL UMEMOV (IUPSEG(1,I),SEGID,LEN(SEGID)/4)
                  IF (SEGID.NE.' ') THEN
                     CALL UBEGIN (SEGID,LEN(SEGID),LBEGIN)
                     IF (LBEGIN.GT.1) THEN
                        WRITE (IPR,145) LBEGIN,'UPSTREAM',SEGID
                        CALL WARN
                        ENDIF
                     ENDIF
183               CONTINUE
               DO 185 I=1,2
                  CALL UMEMOV (IDNSEG(1,I),SEGID,LEN(SEGID)/4)
                  IF (SEGID.NE.' ') THEN
                     CALL UBEGIN (SEGID,LEN(SEGID),LBEGIN)
                     IF (LBEGIN.GT.1) THEN
                        WRITE (IPR,145) LBEGIN,'DOWNSTREAM',SEGID
                        CALL WARN
                        ENDIF
                     ENDIF
185               CONTINUE
               ENDIF
            IYEAR=MOD(ICRDTE(3),100)
            IF (IPART.EQ.2) THEN
               IF (IBUG.EQ.0) WRITE (IPR,150) NSEGSP,IDSEGN,IFGID,ICGID,
     *            ICRDTE(1),ICRDTE(2),IYEAR,ICRDTE(4),ICRDTE(5),SGDSCR
               IF (IBUG.EQ.1) WRITE (IPR,150) NSEGSP,IDSEGN,IFGID,ICGID,
     *            ICRDTE(1),ICRDTE(2),IYEAR,ICRDTE(4),ICRDTE(5),SGDSCR,
     *            NC,ND,NT,NTS,NP
               IF (IDEFSG.NE.0) THEN
                  WRITE (IPR,155) IDSEGN,IDEFSG
155   FORMAT ('0**WARNING** COMPLETE INDICATOR FOR SEGMENT ',2A4,
     *   ' IS ',I2,'.')
                  CALL WARN
                  ENDIF
               IF (IEREC.GT.0) THEN
C              CHECK IF ESP SEGMENT DEFINED
                  IFILLA=0
                  ICHKID=1
                  MTSESP=1
                  MPESP=1
                  MSPESP=1
                  CALL ESPRDF (IFILLA,ICHKID,IEREC,TSESP,MTSESP,
     *               PESP,MPESP,SPESP,MSPESP,IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (IPR,157) IEREC
157   FORMAT ('0**ERROR** ENCOUNTERED IN ROUTINE ESPRDF READING ',
     *   'RECORD ',I5,' FROM FILE ESPPARM.')
                     CALL ERROR
                     ENDIF
                  ENDIF
               ENDIF
            IF (IPART.EQ.3) THEN
               IF (ABS(XLAT).LE.90.0) THEN
                  WRITE (IPR,160) NSEGSP,IDSEGN,IPREC,IWOCRY,MINDT,
     *               XLAT,XLONG,
     *               NC,ND,NT,NTS,NP,NCOPS,IWKLOC
                  ENDIF
               IF (ABS(XLAT).GT.90.0) THEN
                  WRITE (IPR,170) NSEGSP,IDSEGN,IPREC,IWOCRY,MINDT,
     *               NC,ND,NT,NTS,NP,NCOPS,IWKLOC
                  ENDIF
               ENDIF
            NSGPG=NSGPG+1
            IF (NSGPG.GT.NMAX) THEN
               INEWPG=1
               NSGPG=1
               IPAGE=IPAGE+1
               ENDIF
190         CONTINUE
200      CONTINUE
C
      WRITE (IPR,205) NOBSOL
C
C  DO NOT PRINT GLOSSARY IF HAVE NOT PRINTED DESCRIPTION TABLE PART 2
      NLINES=0
      IF (MOD(ISEGTB,5).EQ.0) THEN
         NLINES=15
         IREM=NMAX-NSGPG
         IF (IREM.LT.NLINES) THEN
            IFIRST=0
            CALL FSTAHD (IFIRST)
            ENDIF
         WRITE (IPR,210)
         ENDIF
C
240   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IPR,*) 'EXIT ',RTNNAM
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (
     *  '0',5X,'SEGMENT ',2X,'UPSTREAM SEGMENTS',27X,2X,
     *         'DOWNSTREAM SEGMENTS' /
     *  ' ',5X,8('-'),2X,44('-'),2X,19('-'))
100   FORMAT ('0',5X,10X,'FORECAST',2X,'CARRYOVER' /
     *   ' ',5X,'SEGMENT ',2X,'GROUP',3X,2X,'GROUP',4X,2X,
     *       'DEFINITION DATE',3X,2X,'DESCRIPTION' /
     *   ' ',5X,8('-'),2X,8('-'),2X,9('-'),2X,18('-'),2X,20('-'))
110   FORMAT (
     *   ' ',5X,'SEGMENT ',1X,'IPREC',1X,'IWOCRY',1X,'MINDT',1X,
     *       'XLAT   ',1X,'XLONG  ',1X,
     *       'NC   ',1X,'ND   ',1X,'NT   ',1X,'NTS  ',1X,'NP   ',1X,
     *       'NCOPS',1X,'IWKLOC' /
     *  ' ',5X,'--------',1X,'-----',1X,'------',1X,'-----',1X,
     *       '-------',1X,'-------',1X,
     *       '-----',1X,'-----',1X,'-----',1X,'-----',1X,'-----',1X,
     *       '-----',1X,'------')
140   FORMAT (' ',I4,1X,2A4,2X,5(2A4,1X),1X,2(2A4,1X))
145   FORMAT ('0**WARNING** LOCATION IF FIRST NON-BLANK CHARACTER (',I1,
     *   ') IS NOT 1 FOR ',A,' SEGMENT ',A,'.')
150   FORMAT (' ',I4,1X,2A4,2X,2A4,2X,2A4,1X,2X,
     *  I2.2,'/',I2.2,'/',I2.2,'-',I4.4,'.',I4.4,2X,5A4 :
     *  ' NC=',I5,' ND=',I5,' NT=',I5,' NTS=',I5,' NP=',I5)
160   FORMAT (' ',I4,1X,2A4,1X,I5,1X,I6,1X,I5,
     *  2(1X,F7.2),
     *  6(1X,I5),1X,I6)
170   FORMAT (' ',I4,1X,2A4,1X,I5,1X,I6,1X,I5,
     *  1X,'--NOT DEFINED--',
     *  6(1X,I5),1X,I6)
205   FORMAT ('0THERE ARE ',I3,' OBSOLETE SEGMENTS DEFINED')
210   FORMAT ('0SEGMENT DESCRIPTION GLOSSARY:' //
     *  '  IPREC  - RECORD NUMBER OF FIRST PARAMETER RECORD ON ',
     *             'PARAMETER FILE FOR THIS SEGMENT' /
     *  '  IWOCRY - WORD OFFSET FROM BEGINNING OF SLOT ON ',
     *             'CARRYOVER FILE FOR THIS SEGMENT' /
     *  '  MINDT  - MINIMUM TIME STEP THIS SEGMENT CAN BE RUN (HOURS)' /
     *  '  XLAT   - LATITUDE OF SEGMENT IN DEGREES ',
     *             '(POSITIVE FOR NORTH)' /
     *  '  XLONG  - LONGITUDE OF SEGMENT IN DEGREES ',
     *             '(POSITIVE FOR WEST)' /
     *  '  NC     - LENGTH OF CARRYOVER (C) ARRAY' /
     *  '  ND     - LENGTH OF TIME SERIES DATA AND ',
     *             'WORK SPACE (D) ARRAY' /
     *  '  NT     - LENGTH OF OPERATIONS TABLE POINTER ',
     *             '(T) ARRAY' /
     *  '  NTS    - LENGTH OF TIME SERIES LOCATION ',
     *             '(TS) ARRAY' /
     *  '  NP     - LENGTH OF PARAMETER (P) ARRAY' /
     *  '  NCOPS  - NUMBER OF OPERATIONS THAT STORE CARRYOVER' /
     *  '  IWKLOC - NEXT OPEN POSITION IN D ARRAY (USED FOR WORK SPACE'
     *  )
C
      END
