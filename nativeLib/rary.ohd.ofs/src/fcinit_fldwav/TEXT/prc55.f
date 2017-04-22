C    THIS SUBROUTINE PRINT OUT CO ARRAY FOR NWSRFS

      SUBROUTINE PRC55(PO,IPO,CO,ICO)

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU

      DIMENSION PO(*),IPO(*),CO(*),ICO(*)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/prc55.f,v $
     . $',                                                             '
     .$Id: prc55.f,v 1.5 2004/02/02 20:36:43 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'PRC55   '/

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      NTQL=PO(313)
      NLOCK=PO(321)
      JN=PO(17)
      LNB=PO(126)-1
      LNQL=PO(69)-1
      LNLAD=PO(130)-1
      LITWT=PO(364)-1


C----------------------   STATE VARIABLES  Q  AND  H   ------------------------
      LXYDI=PO(316)-1
      LXQDI=PO(317)-1
      K2=PO(102)
      DO 500 J=1,JN
      WRITE(IPR,450) J
  450 FORMAT(/10X,'INITIAL WATER SURFACE ELEVATIONS FOR RIVER J=',I4)
      N=IPO(LNB+J)
      ICKVAL=999999
      IF (N.GT.ICKVAL) THEN
         WRITE (IPR,998) 'N',N,ICKVAL
998   FORMAT ('0**ERROR** IN PRC55 - VALUE OF VARIABLE ',A,' (',I10,
     *   ') IS GREATER THAN ',I6,'.')
         CALL ERROR
         GO TO 500
         ENDIF
      WRITE(IPR,'(10F10.2)') (CO(LXYDI+KK),KK=1,N)
      LXYDI=LXYDI+K2
      WRITE(IPR,455) J
  455 FORMAT(/10X,'INITIAL DISCHARGES FOR RIVER J=',I4)
      WRITE(IPR,'(10F10.0)') (CO(LXQDI+KK),KK=1,N)
      LXQDI=LXQDI+K2
  500 CONTINUE

C----------------------   LATERAL FLOWS  ---------------------------------------
      IF (NTQL.LE.0) GOTO 601
      LXQLI=PO(318)-1
cc      K10=PO(110)
      DO 600 J=1,JN
      NQL=IPO(LNQL+J)
      IF(NQL.LE.0) GOTO 600
      WRITE(IPR,550) J
  550 FORMAT(/10X,'INITIAL LATERAL FLOWS FOR RIVER J=',I4)
      ICKVAL=999999
      IF (NQL.GT.ICKVAL) THEN
         WRITE (IPR,998) 'NQL',NQL,ICKVAL
         CALL ERROR
         GO TO 600
         ENDIF
      WRITE(IPR,'(10F10.1)') (CO(LXQLI+KK),KK=1,NQL)
      LXQLI=LXQLI+NQL
cc      LXQLI=LXQLI+K10
  600 CONTINUE

C----------   DAM/LOCK  POOL ELVATION    ---------------------------------------
  601 IF(NLOCK.LE.0) GOTO 999
      LXPLTI=PO(319)-1
      LONLAD=PO(130)-1
      LOLAD=PO(151)-1
      LORCHT=PO(129)-1
      K16=PO(116)
      K13=PO(113)
      IP1=1
      DO 700 J=1,JN
      NUMLAD=IPO(LONLAD+J)
      IF(NUMLAD.EQ.0) GO TO 700
      NPL=0
      I1=K16*(J-1)
      KR=K13*(J-1)
      DO 72 I=1,NUMLAD
        LD=IPO(LOLAD+I+I1)
        KRCH=IPO(LORCHT+LD+KR)
        IF(KRCH.EQ.28) NPL=NPL+1
 72   CONTINUE
      IF(NPL.EQ.0) GO TO 700
      IP2=IP1+NPL-1
      WRITE(IPR,650) J
  650 FORMAT(/10X,'INITIAL POOL ELEVATIONS FOR RIVER J=',I4)
      WRITE(IPR,'(10F10.2)') (CO(LXPLTI+KK),KK=IP1,IP2)
      LXPLTI=LXPLTI+NPL
CMGM      IP1=IP2+1
      IP1=1
  700 CONTINUE
C----------   DAM/LOCK  GATE CONTROL SWITCH   ----------------------------------
      LXIWTI=PO(364)-1
      IP1=1
      DO 800 J=1,JN
      NUMLAD=IPO(LONLAD+J)
      IF(NUMLAD.EQ.0) GO TO 800
      NPL=0
      I1=K16*(J-1)
      KR=K13*(J-1)
      DO 74 I=1,NUMLAD
        LD=IPO(LOLAD+I+I1)
        KRCH=IPO(LORCHT+LD+KR)
        IF(KRCH.EQ.28) NPL=NPL+1
 74   CONTINUE
      IF(NPL.EQ.0) GO TO 800
      IP2=IP1+NPL-1
      WRITE(IPR,750) J
  750 FORMAT(/10X,'INITIAL GATE CONTROL SWITCHES FOR RIVER J=',I4)
      WRITE(IPR,'(10I10)') (ICO(LXIWTI+KK),KK=IP1,IP2)
      LXIWTI=LXIWTI+NPL
c      IP1=IP2+1
      IP1=1
  800 CONTINUE


  999 RETURN
      END






