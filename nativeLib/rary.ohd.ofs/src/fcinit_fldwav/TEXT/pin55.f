      SUBROUTINE PIN55(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC,W,IUSEW,IDOS1)
C
C    THIS SUBROUTINE INITIALIZES PARAMETERS FOR DYNAMIC WAVE ROUTING
C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C
      COMMON/DIMS55/K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14,K15,
     *            K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,
     *            K29,K30
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M3055/EPSY,EPSQ,EPSQJ,THETA,XFACT
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/M3455/KXP,ICD,ITMAX,KWARM
      COMMON/SS55/ NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV
      COMMON/FLP55/KFLP
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IDOS55/IDOS,IFCST
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/ofs55'

      DIMENSION Z(1),PO(1),CO(1),W(1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/pin55.f,v $
     . $',                                                             '
     .$Id: pin55.f,v 1.9 2004/09/27 20:40:24 jgofus Exp $
     . $' /
C    ===================================================================

cjms 01/03 added fldview capability

C
      DATA SNAME/ 'PIN55   '   /

      CALL FPRBUG(SNAME, 1, 55, IBUG)

CHDH  This piece is now being done in fsgdef.f.  The entire P array is
CHDH  initialized.
CMGM  A PRECAUTIONARY INITIALIZATION OF ALL FLDWAV PARAMETERS.
C      DO 500 I=1,500
C        PO(I)=0.0
C 500  CONTINUE

      IDOS=IDOS1
      PO(1)=1.01
      PO(9)=IDOS

      IUSEP=500
CCC      IUSEW=1
CCC      IUSET=1

      LOMSG=501
      IUSEP=500+20*20
      PO(10)=LOMSG+0.01
      CALL FIRST55
      CALL READN55(PO,PO,LEFTP,IUSEP,W,IUSEW,Z,IUSEZ,LEFTZ,CO,IUSEC,
     .  LEFTC,PO(LOMSG),IERR)

cc      print*, "in readn at bottom | dtmap=",po(1029)
cc      print*, "after readn k8, loyqcm= ",k8,loyqcm,"     yqcm: "
cc      print*, (po(loyqcm+k),k=1,k8)
cc      print*, "after readn55  jn=",jn,po(17)

      IF(IERR.EQ.1) go to 999
      PO(2)=IUSEP+0.01
C......................................................................

      K1 =PO(101)
      K2 =PO(102)
      K9 =PO(109)
      JNK=PO(33)
      NCS=PO(31)
      KPRES=PO(30)
C
C ======= Check cross section properties & generate areas
      CALL CHAN55(PO(LONBT),PO(LOAS),PO(LOBS),PO(LOHS),NCS,PO(LOASS),
     . PO(LOBSS),PO(LOBSL),PO(LOBSR),PO(LOASL),PO(LOASR),PO(LOSNM),
     . KPRES,KFLP,JN,JNK,K1,K2,K9)
C......................................................................

C ======= Convert parametes from metric to English units
      METRIC=PO(16)
      IF(METRIC.EQ.1) CALL METRC55(PO,PO(LOKU),PO(LOKD),PO(LONQCM),
     .   PO(LOEPQJ),KFLP,KCG,NCG,NLEV,NPOND,NP,NCS,IOBS,K1)
C......................................................................

C ======= Put Manning n reaches at each cross section reach
cc      print*, "before initcm  k8, loyqcm= ",k8,loyqcm,"     yqcm: "
cc      print*, (po(loyqcm+k),k=1,k8)
cc      print*, "before inicm  jn=",jn,po(17)
      IF(KFLP.LE.1) CALL INITCM55(PO,JN,NU,XFACT,PO(LOXT),
     * PO(LOCM),PO(LOCML),PO(LOCMR),PO(LOHS),PO(LOSLFI),PO(LONQCM),
     * PO(LOYQCM),PO(LONCM1),PO(LONCM),PO(LONBT),
     * K1,K2,K7,K8,K9,K23)
C......................................................................
C ======= Interpolate extra cross sections and renumbering everything
cc      print*, "before intpxs  jn=",jn,po(17)
      CALL INTPXS55(PO,JNK,JN,PO(LONB),NP,PO(LOBS),PO(LOHS),PO(LOAS),
     * PO(LOBSL),PO(LOBSR),PO(LOASL),PO(LOASR),PO(LOASS),PO(LOFKEC),
     1 PO(LOX),PO(LCNN),PO(LONBT),PO(LOSLFI),PO(LOXT),PO(LOKRCH),
     2 PO(LODXM),PO(LOBSS),NCS,PO(LOSNC),PO(LOSNM),PO(LOCM),
     3 PO(LOCML),PO(LOCMR),PO(LOYQCM),PO(LONQCM),PO(LOIFXC),PO(LONGAG),
     4 PO(LONGS),PO(LONJFM),PO(LONJTO),PO(LONIFM),PO(LONITO),PO(LONJFT),
     5 PO(LONJTT),PO(LONIFT),PO(LONITT),PO(LORCHT),PO(LCPR),PO(LOMIXF),
     6 PO(LOMUD),PO(LCIRGM),PO(LCDXR),PO(LCHCAV),PO(LCIFCV),PO(LOXLAT),
     7 PO(LOXLON),PO(LOFLST),K1,K2,K4,K5,K7,K8,K9,K10,K13,K14,K16,K18,
     8 K22,K23,K30)
      PO(310)=IEXT+0.01
C......................................................................
C ======= Interpolate levees
cc      print*, "before lvin  jn=",jn,po(17) 
C jgg modified argument list for HSD bug r23-48
C jgg      IF(NLEV.GT.0) CALL LVIN55(PO(LCNN),PO(LOX),PO(LONJFM),PO(LONJTO),
C jgg     * PO(LONXLV),PO(LONIFM),PO(LONITO),PO(LCIFLV),PO(LCTFL0),PO(LOHWLV)
C jgg     *,PO(LOWCLV),PO(LOTFLV),PO(LOBLMX),PO(LOHFLV),PO(LOHLMN),PO(LOSLV),
C jgg     * PO(LCHLV),PO(LCBLV),PO(LOHPLV),PO(LODPLV),K2,K23)

      IF(NLEV.GT.0) CALL LVIN55(PO(LCNN),PO(LOX),PO(LONJFM),PO(LONJTO),
     * PO(LONXLV),PO(LONIFM),PO(LONITO),PO(LCIFLV),PO(LCTFL0),PO(LOHWLV)
     *,PO(LOWCLV),PO(LOTFLV),PO(LOBLMX),PO(LOHFLV),PO(LOHLMN),PO(LOSLV),
     * PO(LCHLV),PO(LCBLV),PO(LOHPLV),PO(LODPLV),K1,K2,K18,K22,K23)
C jgg end changes
      PO(59)=NLEV+0.01
cc      print*, "after lvin  jn=",jn,po(17) 
C......................................................................
C ======= Sinuosity coe., convayancy
      IF(KFLP.GE.1) CALL COMPK55(PO,JNK,JN,PO(LONB),PO(LOHS),PO(LOHKC),
     1 PO(LOQKC),PO(LCBEV),PO(LCNKC),PO(LOSNM),PO(LONBT),PO(LCNN),
     2 K1,K2,K7,K8,K9,K23)
C......................................................................

      IF(KFLP.EQ.1) CALL PREFLP55(JNK,JN,NCS,PO(LONB),PO(LOSNC),
     1 PO(LOBS),PO(LOAS),PO(LOBSL),PO(LOBSR),PO(LOASL),
     2 PO(LOASR),K2,K9)
C......................................................................
C ======= Put read-in ydi, qdi into new-numbered cross-sections
cc      print*, "before icin  jn=",jn,po(17)
      ICOND=PO(25)
      CALL ICIN55(JN,PO(LONB),PO(LONBT),CO(LXYDI),CO(LXQDI),PO(LCNN),
     1 ICOND,K1,K2,K23)
C......................................................................

      IF(IBUG.EQ.2) CALL ARRY21(PO,PO,IUSEP)

CC      IF(NP.LE.-1) CALL PRECAL55(Z,Z(LONBT),Z(LONGAG),Z(LONGS),Z(LOXT),
CC     *   W,Z(LONJUN),Z(LOGZ),Z(LOKD),W,W(LOGZN),Z(LOMRV),
CC     *   Z(LCORDR),IUSEW,LTS(LTSTN),LTS(LTSTT),K1,K3,K4,K23)
CC        IPRT=1
CC        ISW=1
CC

CC      CALL IDPREP55(Z(LOX),Z(LONB),Z(LCDDX),Z(LOAS),Z(LOBS),Z(LOGZ),
CC     * Z(LOHS),Z(LOKD),Z(LOKU),Z(LCWF),Z(LOASS),Z(LOATF),Z(LOBSS),
CC     * W,W,W,Z(LONGAG),W(LOGZ1),W(LOGZN),W(LOYQD),W(LOQYQD),Z(LOCOFW),
CC     * Z(LOVWND),Z(LOWAGL),W(LOT1),Z(LCHCAV),Z(LCIFCV),Z(LOLTST),
CC     * LTS(LTSTN),LTS(LTST1),LTS(LTSTT),K1,K2,K3,K4,K6,K9)
CC

C-------------------------------------------------------------------------------
C    CARRY OVER ARRAY CO DEFINITION (YDI, QDI, QLI  etc.) FOR NWSRFS (IDOS=3)
cc      IF (IDOS.GE.3) CALL CARRY55(Z,CO,LEFTC,IUSEC,Z(LONB),Z(LXYDI),
cc     # Z(LOXQDI),Z(LOTS1),Z(LOTS2),K1,K2,0)
C-------------------------------------------------------------------------------
cc    PO(2)=IUSEP+0.01
cc      print*, "dtmp=",po(1029)
cc      print*, "in pin55 at bottom:  lospth=",lospth," syspth="
cc      print ('6a4'),(po(lospth+l-1),l=1,6)
 999  RETURN
      END

