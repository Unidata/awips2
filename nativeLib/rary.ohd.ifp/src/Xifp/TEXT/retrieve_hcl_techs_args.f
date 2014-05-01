C$PRAGMA C (UDATL)    
      Subroutine retrieve_hcl_techs_args
     1  (a_in, a_ipr, a_ipu, a_ioerr, a_iodbug,
     2   a_metric, a_iumgen, a_iumsac, a_iumapi,
     3   a_nhopdb, a_nhocal, a_local, a_nlstz,
     4   a_inptzc, a_modtzc,
     5   a_noutz, a_noutds, a_modwrn,
     6   a_nosnow, a_nofrze, a_iupwe, a_isac_snow, a_iupsc,
     7   a_iprsac, a_iprsnw, a_icrtro, a_iprhy, a_ifpr,
     8   a_idarun, a_ihrrun, a_ldacpd, a_lhrcpd, a_ldarun, a_lhrrun,
     9   a_now)

      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/fcary'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fcfutp'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/where'
      INCLUDE 'common/fmodft'
      INCLUDE 'common/fplotc'
      INCLUDE 'common/fdisps'
      INCLUDE 'hclcommon/hunits'
C
      COMMON/HDFLTS/XHCL(2),LINPTZ,YHCL(18),LCLHCL,LTZHCL,NPDHCL,NCAHCL
C
      
      DIMENSION IARGS(84)
      Dimension IDATE(7), a_now(5)
      
c
      Integer
     1 a_in, a_ipr, a_ipu, a_ioerr, a_iodbug,
     2 a_metric, a_iumgen, a_iumsac, a_iumapi,
     3 a_nhopdb, a_nhocal, a_local, a_nlstz,
     4 a_inptzc, a_modtzc,
     5 a_noutz, a_noutds, a_modwrn,
     6 a_nosnow, a_nofrze, a_iupwe, a_isac_snow, a_iupsc,
     7 a_iprsac, a_iprsnw, a_icrtro, a_iprhy, a_ifpr,
     8 a_idarun, a_ihrrun, a_ldacpd, a_lhrcpd, a_ldarun, a_lhrrun,
     9 a_now
     
     
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/retrieve_hcl_techs_args.f,v $
     . $',                                                             '
     .$Id: retrieve_hcl_techs_args.f,v 1.5 2002/02/20 10:19:51 michaelo Exp $
     . $' /
C  =====================================================================
C
C
c  load HDFLTS common block and commons for FCEXEC function
c
      Call hldcbs("FCEXEC  ", istat)
c      write(*,610)istat
c 610  format("after hldcbs, istat = ", i2)
C
      Call hsysda(julian_day)
c      write(*,612)julian_day
c 612  format("after hsysda, julian_day = ", i9)
c
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>
c  Universal techniques
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>
c
      CALL HPAST(8HPRINT   ,IPR,ISTAT)
C
      CALL HPAST(8HPUNCH   ,IPU,ISTAT)
C
      CALL HPAST(8HDEBUGPR ,IODBUG,ISTAT)
C
      CALL HPAST(8HERRORPR ,IOERR,ISTAT)
C
C  TECHNIQUE METRIC - NO ARGUMENTS
C
      CALL HPAST(8HMETRIC  ,METRIC,ISTAT)
C
C  TECHNIQUE MODUNITS - NO ARGUMENTS
C
      CALL HPAST(8HMODUNITS,IUMGEN,ISTAT)
C
C  TECHNIQUE MODSACUN - NO ARGUMENTS
C
      CALL HPAST(8HMODSACUN,IUMSAC,ISTAT)
C
C  TECHNIQUE MODAPIUN - NO ARGUMENTS
C
      CALL HPAST(8HMODAPIUN,IUMAPI,ISTAT)
C
C  IF ANY OF MODUNITS, MODSACUN, OR MODAPIUN EQUALS 2
C  SET ITS VALUES TO THAT OF METRIC
C
      IF(IUMGEN.EQ.2)IUMGEN=METRIC
      IF(IUMSAC.EQ.2)IUMSAC=METRIC
      IF(IUMAPI.EQ.2)IUMAPI=METRIC
C
C  SET NHOPDB AND NHOCAL FROM HCL RFC DEFAULT CB /HDFLTS/
C
      NHOPDB=NPDHCL
      NHOCAL=NCAHCL
      DO 100 I=1,8
  100 NHROFF(I)=0
C
C  SET INPTZC, LOCAL AND NLSTZ FROM HCL RFC DEFAULT CB /HDFLTS/
C
      INPTZC=LINPTZ
      LOCAL=LCLHCL
      NLSTZ=LTZHCL
C
C  SET DEFAULT MOD TIME ZONE CODE FROM ARGUMENT OF TECH MODTZC
C  IF MODTZC (MTZTEK) EQ ZERO OR ARG (MODTMP) EQ BLANKS USE INPTZC
C
      CALL HPASTA(8HMODTZC  ,100,MTZTEK,NWORDS,IARGS,ISTAT)
C
      MODTZC=INPTZC
      IF(MTZTEK.EQ.0)GO TO 74
      CALL HGTSTR(1,IARGS,MODTMP,IFL,ISTAT)
C
      IF(ISTAT.EQ.0)GO TO 72
      WRITE(IPR,602)ISTAT
  602 FORMAT(1H0,10X,'**ERROR** STATUS OF ',I3,' RETURNED FROM ',
     1 'HGTSTR')
      WRITE(IPR,607)
  607 FORMAT(11X,'WHILE DECODING TIME ZONE CODE FOR TECHNIQUE MODTZC'/
     1 11X,'TIME ZONE CODE FROM FILE USERPARM IS ASSUMED.')
      CALL ERROR
      GO TO 74
C
   72 IF(MODTMP.NE.IBLNK)MODTZC=MODTMP
C
C  SET OUTPUT TIME ZONE NUMBER AND DAYLIGHT SAVINGS SWITCH FROM
C  TECHS NOUTZ AND NOUTDS
C
   74 CALL HPAST(8HNOUTZ   ,NOUTZ,ISTAT)
C
      CALL HPAST(8HNOUTDS  ,NOUTDS,ISTAT)
C
C  TECHNIQUE FUTPRECP - NO ARGUMENTS
C
      CALL HPAST(8HFUTPRECP,IFPR,ISTAT)
C
C  TECHNIQUE MODWARN - NO ARGUMENTS
C
      CALL HPAST(8HMODWARN ,MODWRN,ISTAT)
c
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>
c  Non-universal techniques
c     Added code (from fun001.f) for PRTRO, PRINTSMA, and TABLES 
c     techniques - dp - 14 Mar. 1995
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>
C
C  TECHNIQUE PRTRO - NO ARGUMENTS
C
      CALL HPAST(8HPRTRO   ,ICRTRO,ISTAT)
C
C  SET VALUES IN /FPLTAB/
C  VALUE OF 0 FOR TECHNIQUE GIVES VALUE OF -1 IN /FPLTAB/
C  VALUE OF 1 FOR TECHNIQUE GIVES VALUE OF  1 IN /FPLTAB/
C  VALUE OF 2 FOR TECHNIQUE GIVES VALUE OF  0 IN /FPLTAB/
C
C  TECHNIQUE TABLES - NO ARGUMENTS
C
      CALL HPAST(8HTABLES  ,ITABLE,ISTAT)
C
      IPRHY=0
      IF(ITABLE.EQ.0)IPRHY=-1
      IF(ITABLE.EQ.1)IPRHY=1
C
C  TECHNIQUE SNOW - NO ARGUMENTS
C
      CALL HPAST(8HSNOW    ,ISNOW,ISTAT)
      NOSNOW=0
      IF(ISNOW.EQ.0)NOSNOW=1
C
C  SET VALUE IN /FSNW/
C  VALUE OF 0 FOR TECHNIQUE GIVES VALUE OF -1 IN /FSNW/
C  VALUE OF 1 FOR TECHNIQUE GIVES VALUE OF  1 IN /FSNW/
C  VALUE OF 2 FOR TECHNIQUE GIVES VALUE OF  0 IN /FSNW/
C
C  TECHNIQUE PRINTSNW - NO ARGUMENTS
C
      CALL HPAST(8HPRINTSNW,IPRTSN,ISTAT)
C
      IPRSNW=0
      IF(IPRTSN.EQ.0)IPRSNW=-1
      IF(IPRTSN.EQ.1)IPRSNW=1
C
C  SET VALUE IN /FSACPR/
C  VALUE OF 0 FOR TECHNIQUE GIVES VALUE OF -1 IN /FSACPR/
C  VALUE OF 1 FOR TECHNIQUE GIVES VALUE OF  1 IN /FSACPR/
C  VALUE OF 2 FOR TECHNIQUE GIVES VALUE OF  0 IN /FSACPR/
C
C  TECHNIQUE PRINTSMA - NO ARGUMENTS
C
      CALL HPAST(8HPRINTSMA,IPRTSM,ISTAT)
C
      IPRSAC=0
      IF(IPRTSM.EQ.0)IPRSAC=-1
      IF(IPRTSM.EQ.1)IPRSAC=1
C
C  TECHNIQUE FROST - NO ARGUMENTS
C
C  VALUE OF 0 FOR TECHNIQUE GIVES VALUE OF  1 IN /FSACPR/
C  VALUE OF 1 FOR TECHNIQUE GIVES VALUE OF  0 IN /FSACPR/
C
      CALL HPAST(8HFROST   ,IFROST,ISTAT)
C
      NOFRZE=0
      IF(IFROST.EQ.0)NOFRZE=1
C
C  TECHNIQUE UPWE - NO ARGUMENTS
C
      CALL HPAST(8HUPWE    ,IUPWE,ISTAT)
      
C
C  TECHNIQUE SACSNOW - NO ARGUMENTS
C
      CALL HPAST(8HSACSNOW ,ISACSNOW,ISTAT)
C
C  TECHNIQUE UPSC - NO ARGUMENTS
C
      CALL HPAST(8HUPSC    ,IUPSC,ISTAT) 
           
c
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>
c  run date techniques
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>
C
C  SET DATES FOR START OF RUN, END OF RUN, AND LAST COMPUTATIONAL
C  DAY FROM ARGUMENTS OF TECHS STARTRUN, ENDRUN AND LSTCMPDY
C
      CALL HPASTA(8HSTARTRUN,100,ISTRUN,NWORDS,IARGS,ISTAT)
c
c  Replace star (*) date in iargs with real date based on current date
c
      Call HSETDY(IARGS)
c
      IDARUN=IARGS(1)/24 + 1
      IHRRUN=IARGS(1)-IDARUN*24 + 24
C
      CALL HPASTA(8HENDRUN  ,100,IENRUN,NWORDS,IARGS,ISTAT)
c
c  Replace star (*) date in iargs with real date based on current date
c
      Call HSETDY(IARGS)
c
      LDARUN=IARGS(1)/24 + 1
      LHRRUN=IARGS(1)-LDARUN*24 + 24
C
      CALL HPASTA(8HLSTCMPDY,100,ILCRUN,NWORDS,IARGS,ISTAT)
c
c  Replace star (*) date in iargs with real date based on current date
c
      Call HSETDY(IARGS)
c
      LDACPD=IARGS(1)/24 + 1
      LHRCPD=IARGS(1)-LDACPD*24 + 24
C
      IF(IHRRUN.NE.24)GO TO 55
      IDARUN=IDARUN+1
      IHRRUN=0
   55 IF(LHRRUN.NE.0)GO TO 60
      LDARUN=LDARUN-1
      LHRRUN=24
   60 IF(LHRCPD.NE.0)GO TO 70
      LDACPD=LDACPD-1
      LHRCPD=24
C
 70   CALL UDATL(IDATE)
      NOW(1)=IDATE(3)
      NOW(2)=IDATE(4)
      NOW(3)=IDATE(1)
      NOW(4)=IDATE(5)
      NOW(5)=IDATE(6)
C
c      write(*,650)in, ipr, ipu, ioerr, iodbug
c 650  format(6i10)
c      write(*,650)metric, iumgen, iumsac, iumapi
c      write(*,650)nhopdb, nhocal, local, nlstz
c      write(*,651)inptzc, modtzc
c 651  format(1x,a4,1x,a4)
c      write(*,650)noutz, noutds, modwrn
c      write(*,650)nosnow, nofrze, iupwe, iupsc
c      write(*,650)iprsac, iprsnw, icrtro, iprhy, ifpr
c      write(*,650)idarun, ihrrun, ldacpd, lhrcpd, ldarun, lhrrun
c      write(*,650)now
c
      a_in     = in
      a_ipr    = ipr
      a_ipu    = ipu
      a_ioerr  = ioerr
      a_iodbug = iodbug
c
      a_metric = metric
      a_iumgen = iumgen
      a_iumsac = iumsac
      a_iumapi = iumapi
      a_nhopdb = nhopdb
      a_nhocal = nhocal
      a_local  = local
      a_nlstz  = nlstz
c
      a_inptzc = inptzc
      a_modtzc = modtzc
      a_noutz  = noutz
      a_noutds = noutds
      a_modwrn = modwrn
      a_nosnow = nosnow
      a_nofrze = nofrze
      a_iupwe  = iupwe
      a_iupsc  = iupsc
      
      a_isac_snow = isacsnow

cc      write(*,554)a_isac_snow
cc 554  format('in retreive_hcl nofreeze = ', i2) 
c
      a_iprsac = iprsac
      a_iprsnw = iprsnw
      a_icrtro = icrtro
      a_iprhy  = iprhy
      a_ifpr   = ifpr
c
      a_idarun = idarun
      a_ihrrun = ihrrun
      a_ldacpd = ldacpd
      a_lhrcpd = lhrcpd
      a_ldarun = ldarun
      a_lhrrun = lhrrun
c
      do 555 i = 1, 5
        a_now(i) = now(i)
 555  continue
c
      call uclosl
c
      Return
      End
