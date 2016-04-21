C MODULE URRDCN
C-----------------------------------------------------------------------
C
      SUBROUTINE URRDCN
C
C   THIS ROUTINE COPIES THE REORDER COMMONS TO THE NON-REORDER COMMONS
C   SINCE THE ROUTINES THAT PRINT THE DATA BASE STATUS AFTER REORDERING
C   ONLY USE THE NON-REORDER COMMONS.
C
      DIMENSION IBUF(60)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'urcommon/urpddt'
      INCLUDE 'urcommon/urpddd'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urxctl'
      INCLUDE 'urcommon/urrrsc'
      INCLUDE 'urcommon/urhshi'
      INCLUDE 'urcommon/urhshc'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urppmc'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urmaxm'
      INCLUDE 'urcommon/urtscl'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urrdcn.f,v $
     . $',                                                             '
     .$Id: urrdcn.f,v 1.2 2003/08/21 07:48:15 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0.OR.IPPTR.GT.0.OR.IPRTR.GT.0) WRITE (IOGDB,*)
     *   'ENTER URRDCN'
C
C  COPY USERPARM COMMON
      CALL UREADT (KURPRM,1,IBUF,ISTAT)
      CALL UMEMOV (IBUF(1),TIME(1),25)
C
C  COPY PPD COMMONS
      CALL UMEMOV (MAXRSF,MXRRSF,16)
      CALL UMEMOV (NWCTL,NWDCTL,16)
      CALL UMEMO2 (JDDTDR,IDDTDR,720)
      CALL UMEMOV (JPDDFC,IPDDFC,20)
      CALL UMEMO2 (IURHSI,IPDHSI,MURHSI)
      CALL UMEMO2 (IURHSC,IPDHSC,MURHSC)
      KPDSIF=KURSIF
      KPDRRS=KURRRS
      CALL UMEMOV (KURDDF,KPDDDF,5)
C
C  COPY PPP COMMONS
      CALL UMEMOV (JPMCTL,IPMCTL,72)
      CALL UMEMOV (MAXPXR,MXPXRC,8)
      CALL UMEMOV (JPDTDR,IPDTDR,400)
      KPPIDX=KURIDX
      CALL UMEMOV (KUPRMI,KPPRM1,9)
C
C  COPY PRD COMMONS
      DO 30 I=1,NMPRDF
         CALL UMEMOV (ITSCNT(1,I),TSCNTR(1,I),16)
30       CONTINUE
      DO 20 I=1,NUMDTP
         CALL UMEMOV (IDATFL(1,I),DATFIL(1,I),18)
         DATFIL(2,I)=DATFIL(2,I)-KUPRDO
20       CONTINUE
      CALL UMEMOV (KUMAPT,KMAPTS,7)
      CALL UMEMOV (INAMRF,USERPR,60)
C
      IF (IPDDB.GT.0) WRITE (IOGDB,40) MAXRSF,LRRSXR,
     *   LFREE1,LFREEN,LFREEL,IUFREE,MAXFRC,MXSIZE,NUMSTA,INUSFL,NWCTL,
     *   LRLURD,LRLURS,LRLURI,MPDTYP,NPDTYP,IPTDTP,IHASHR,NH8CHR,NHINRC,
     *   ISIFRC,MAXSIF,LTSIFR,MAXDOD,MXDDF,NMDDF,MAXPXR,MAXPTP,NUMPTP,
     *   NUMPFL,IXPRC1,NAMRFC(1),NAMRFC(2)
40    FORMAT (' RRS CNTL= ',10I4, /
     *   ' SIF CNTL= ',16I4 /
     *   ' PPP CNTL=',5I4,1X,2A4)
      IF (IPDDB.GT.0)
     *   WRITE (IOGDB,50) ((JDDTDR(I,J),I=2,12),J=1,NPDTYP)
50    FORMAT (' PPP DIR= ',2A2,9I6)
      IF (IPRDB.GT.0) WRITE (IOGDB,60) (ITSCNT(3,J),ITSCNT(4,J),J=1,5)
60    FORMAT (' PRD CNTL: NXT AVL REC=',I5,' # OF DATA TYPES=',I4)
      IF (IPRDB.GT.0) WRITE (IOGDB,70) INAMRF,MXTYPE,
     *   MXTIME,MNDAYS,MAXDWD,NMTIMS,NMTYPE
70    FORMAT (' INAMRF=',2A4,' MXTYPE=',I4,' MXTIME=',I4,
     *   ' NMDAYS=',I4,' MAXDWD=',I4,' NMTIMS=',I4,' NMTYPE=',I4)
C
      IF (IPDTR.GT.0.OR.IPPTR.GT.0.OR.IPRTR.GT.0) WRITE (IOGDB,*)
     *   'EXIT URRDCN'
C
      RETURN
C
      END
