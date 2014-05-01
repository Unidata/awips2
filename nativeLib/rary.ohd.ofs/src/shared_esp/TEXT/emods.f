C MODULE EMODS
C-----------------------------------------------------------------------
C
C     THIS IS THE MAIN ROUTINE FOR DECODING RUNTIME MODS.
C
      SUBROUTINE EMODS (NCARDS,MODCRD,MP,P,MC,C,MTS,TS,
     1                MT,T,MD,D,NXTOPN,NXTNAM,IHZERO,
     2                ijdlst,idloop, mecards, ierr )
C
      LOGICAL PERM,CHANGE
      INTEGER T
      CHARACTER*8 NXTNAM,MODNAM
C
      INCLUDE 'common/fcassm'
C
      INCLUDE 'common/fmodft'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/fctime'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/modctl'
      INCLUDE 'ufreex'
      COMMON/HDFLTS/DX1,INPTZN,INPHCL,DX2(18),LCLHCL,LTZHCL,DX3(2)

      INCLUDE 'common/fprog'

C
      character MODCRD(80,ncards), MECARDS(80,NCARDS)
      DIMENSION P(MP),C(MC),TS(MTS),T(MT),D(MD)
      DIMENSION OLDOPN(2)
C
C  IASSIM - TEST IF ASSIMILATOR OPERATION IS IN SEGMENT
      INTEGER IASSIM, IOPNUM
      DIMENSION ASSIM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/emods.f,v $
     . $',                                                             '
     .$Id: emods.f,v 1.5 2003/03/14 18:53:34 dws Exp $
     . $' /
C    ===================================================================
C
      DATA ASSIM/4HASSI,4HM   /
C
      CHARACTER*4 IDOTS/4H..../
C
      CALL FSTWHR('EMODS   ',0,OLDOPN,IOLDOP)
C
      IBUG=IFBUG('MODS')
C
C  SET VALUE IN UFREEX COMMON BLOCK
C
      ierr=0
      IPRBLN=0
      IPASS=0
      
      INPNCH=INPTZN
      INPTCH=INPTZC
      INPHCH=INPHCL
      CALL HCKDTC(MODTZC,INPTZN,ISTCK)
      IF(ISTCK.EQ.0)GO TO 432
      WRITE(IPR,953)MODTZC,INPTZC
  953 FORMAT(1H0,10X,'**WARNING** AN INVALID TIME ZONE CODE HAS BEEN ',
     1'ENTERED FOR MOD INPUT (',A4,')'/11X,'DEFAULT MOD TIME ZONE ',
     2'CODE WILL BE SET TO ',A4,'.')
      CALL WARN
      MODTZC=INPTZC
      CALL HCKDTC(MODTZC,INPTZN,ISTCK)
  432 INPHCL=MODTZC
      INPTZC=MODTZC

C
      DO 321 I=1,100
  321 ICDBUF(I:I)=IDOTS
C
      IPASS=IPASS+1
      NRDCRD=0
C
C
  5   ILINE=NRDCRD
      IF(NRDCRD.GE.NCARDS)GO TO 9999
C
C     CHECK FOR COMMAND TO SEE IF PERMANENT OR NONPERMANENT
C
      ICMND=MISCMD(NCARDS,MECARDS)
C
C     IF NOT A COMMAND - READ NEXT CARD, NO MORE CARDS - RETURN
C
      IF(ICMND.EQ.-1)GO TO 9999
      IF(ICMND.EQ.41)GO TO 8
    7 IF(IBUG.GT.0)WRITE(IODBUG,905)ICMND
  905 FORMAT(' ICMND=',I3)
C
      NRDCRD=NRDCRD+1
      GO TO 5
C
C
C
 8    CALL EMFCMND(NCARDS,MECARDS,ICMND,IDATE,LDATE,KDATE,istrt)
C
C     ICMND LT ZERO IF DATES ON MOD CARD ARE INVALID
C     SKIP MOD IF THIS IS THE CASE
C
      IF(ICMND.LT.0)GO TO 5
C
C     GO TO PROPER ROUTINE TO DECODE THE SUBSEQUENT CARDS
C
C
C     BRANCH TO LABEL 500 IF MOD NOT YET TESTED
C     ELSE GO TO APPROPRIATE ROUTINE TO PROCESS MOD
C
C     GO TO (AEIC,AESC,AIAD,APICQ,BASE,BFRA,BFRC,CBAS,CBFR,IGNO,
C    1       MFC ,RAIN,ROCH,ROMU,RRIC,RRIM,SACB,SACC,SETM,SETQ,
C    2       TSAD,TSCH,TSMU,TSRE,UCBA,UCBF,UHGA,UHGC,WECH,XINC,
C    3       APICB,APICC,WEADD,ZERODIF,CHGBLEND,MATCHNG,
C    4       QCSHIFT,QPSHIFT,BUBLSHFT,SWITCHTS,SSARREG),ICMND
C
      GOTO (888,888,888,888,888,888,888,888,888,888,
     1      888,888,888,888,888,888,888,888,888,888,
     2      888,888,888,888,888,888,888,888,888,888,
     3      888,888,888,888,888,888,888,888,888,888,
     4      1410),ICMND
C     CODE HERE TO WRITE MESSAGE FOR MODS NOT YET TESTED
C
      CALL MCMDNA(ICMND,MODNAM)
      WRITE(IPR,501)MODNAM
  501 FORMAT(1H0,10X,'**WARNING** THE ',A8,' MOD IS NOT ',
     1 'CURRENTLY AVAILABLE - PROCESSING OF OTHER MODS CONTINUES.')
      CALL WARN
      GO TO 5
C
C     CALLS TO ROUTINES TO PROCESS MODS
C     LISTED IN ALPHABETICAL ORDER
C
C

cew idate is effective date for ssarr mod in internal time
cew compute difference between effective
cew date and carryover date (ijdlst), then compute new
cew effective date by adding difference to idloop

1410    continue

        JUL=iabs(idate)
        JHDAY = JUL/24 + 1
        JHR = MOD(JUL,24)
        IF(JHR .LT. 0) JHR = 0
        IF(JUL .LT. 0) JHDAY = 0

        idiff = jhday - ijdlst
        idate_new = idloop + idiff
cxf
cxf  bug r22-6: inconsistent time shift problem
cxf
cxf  change ihr to jhr
cxf

        call mdyh2(ijdlst,jhr,ilmonth,ilday,ilyear,ilhour,itz,
     +   idsav,modtzc)

        call mdyh2(jhday,jhr,ijmonth,ijday,ijyear,ijhour,itz,
     +   idsav,modtzc)

        call mdyh2(idate_new,jhr,idmonth,idday,idyear,idhour,itz,
     +   idsav,modtzc)

cxf (end)

cew write new date onto mod card in modcrd array

      call ufi2a(idmonth,modcrd(istrt,nrdcrd),1,-2,1,ipr,istatuf)
      call ufi2a(idday,modcrd(istrt+2,nrdcrd),1,-2,1,ipr,istatuf)
      idyear=mod(idyear,100)
      call ufi2a(idyear,modcrd(istrt+4,nrdcrd),1,-2,1,ipr,istatuf)
      call ufi2a(idhour,modcrd(istrt+6,nrdcrd),1,-2,1,ipr,istatuf)
      call umemov(modtzc,modcrd(istrt+8,nrdcrd),1)

      CALL ESSARR(MP,P,NCARDS,MODCRD,IDATE,NXTOPN,NXTNAM,
     1                           IHZERO,ijdlst,idloop,mecards,ierr)
888   IBTWEN=1
      GOTO 5
C
 9999 continue
       INPTZN=INPNCH
       INPTZC=INPTCH
       INPHCL=INPHCH

      CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)
      RETURN
      END
