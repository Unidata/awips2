C MEMBER GMROPO
C  (from old member PPGMROPO)
C
      SUBROUTINE GMROPO(IRIV, NAMER, NMAROS, IMARO, ISTCOD)
C
C.....SUBROUTINE TO LIST OUT MARO AREAS IN FORECAST GROUP ORDER.
C
C.....FORECAST GROUP IS THE SAME AS RIVER SYSTEM.
C
C.....THIS SUBROUTINE IS A TEMPORARY SUBROUTINE PENDING CREATION OF A
C.....PARAMETRIC ARRAY TO ACCOMPLISH THIS IN THE NWSRFS VERSION 5.0
C.....MARO FUNCTION. AFTER THE PARAMETRIC ARRAY IS CREATED...THIS
C.....SUBROUTINE WILL NO LONGER BE NEEDED.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN      WGRFC FT. WORTH      SEPTEMBER 30, 1987
C
C.....CHANGE MADE 10/14/92 BY JERRY M. NUNN AND BOBBY ARMSTRONG.
C
C.....FIVE NEW MARO AREAS ADDED TO SAN JACINTO RIVER SYSTEM, AND ONE
C.....MARO AREA DELETED.
C
C.....LIST OF ARGUMENTS IN THE SUBROUTINE:
C
C.....IRIV   - THE 4 CHARACTER ID OF THE RIVER WHOSE MARO AREA IDS
C.....         ARE TO BE SUPPLIED. (INPUT BY USER).
C.....NAMER  - THE NAME OF THE RIVER SYSTEM (UP TO 20 CHARACTERS).
C.....         (RETURNED FROM SUBROUTINE).
C.....NMAROS - THE NUMBER OF MARO AREAS IN THIS RIVER SYSTEM.
C.....         (RETURNED FROM SUBROUTINE).
C.....IMARO  - THE ARRAY OF 8 CHARACTER MARO AREA IDS (UP TO 100).
C.....         (RETURNED FROM SUBROUTINE).
C.....ISTCOD - A STATUS RETURN CODE.
C.....          = 0   NORMAL RETURN.
C.....          = 1   RIVER ID GIVEN NOT FOUND IN LIST.
C
      DIMENSION IDBRAZ(200), IDCOLO(200), IDRIOG(200), IDTRIN(200)
      DIMENSION IDNECH(200), IDNUEC(200), IDSABN(200), IDSANJ(200)
      DIMENSION IDGUAD(200), IDBRZ1(90), IDBRZ2(110), IRVMRO(1800)
      DIMENSION IDCOL1(98), IDCOL2(102), IDGRN1(90), IDGRN2(110)
      DIMENSION IRVID(9), IRVNAM(45), NUMARO(9), NAMER(1), IMARO(1)
C
      EQUIVALENCE (IRVMRO(1),IDBRAZ(1)),(IRVMRO(201),IDCOLO(1))
      EQUIVALENCE (IRVMRO(401),IDRIOG(1)),(IRVMRO(601),IDTRIN(1))
      EQUIVALENCE (IRVMRO(801),IDNECH(1)),(IRVMRO(1001),IDNUEC(1))
      EQUIVALENCE (IRVMRO(1201),IDSABN(1)),(IRVMRO(1401),IDSANJ(1))
      EQUIVALENCE (IRVMRO(1601),IDGUAD(1))
C
      EQUIVALENCE (IDBRZ1(1),IDBRAZ(1)),(IDBRZ2(1),IDBRAZ(91))
      EQUIVALENCE (IDCOL1(1),IDCOLO(1)),(IDCOL2(1),IDCOLO(99))
      EQUIVALENCE (IDGRN1(1),IDRIOG(1)),(IDGRN2(1),IDRIOG(91))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gmropo.f,v $
     . $',                                                             '
     .$Id: gmropo.f,v 1.1 1995/09/17 19:02:09 dws Exp $
     . $' /
C    ===================================================================
C
C
C.....BRAZOS RIVER MARO AREAS.
C
      DATA IDBRZ1 /4hJUST, 4hCBRG, 4hASPE, 4hRUS , 4hASPE, 4hRDMF,
     *             4hPEAC, 4hOCUS, 4hGIRA, 4hRD  , 4hPEAC, 4hOCK ,
     *             4hJAYT, 4hON  , 4hASPE, 4hRSFK, 4hKNOX, 4hCITY,
     *             4hASPE, 4hRSTK, 4hSEYM, 4hORUS, 4hSEYM, 4hOUR ,
     *             4hMILL, 4hERES, 4hSOUT, 4hHBUS, 4hSOUT, 4hHBND,
     *             4hROBY, 4h    , 4hLKSW, 4hEET , 4hHAWL, 4hEY  ,
     *             4hPHAN, 4hTOM , 4hNUGE, 4hNT  , 4hSTAM, 4hFORD,
     *             4hLKST, 4hAM  , 4hGRIF, 4hFIN , 4hALBA, 4hNY  ,
     *             4hBREC, 4hKBIG, 4hHUBB, 4hRES , 4hELIS, 4hVILE,
     *             4hGRAH, 4hAM  , 4hPK  , 4h    , 4hPALO, 4hPNTO,
     *             4hDENN, 4hISUS, 4hLKPA, 4hLOP , 4hSANT, 4hO   ,
     *             4hDENN, 4hIS  , 4hGRAN, 4hBURY, 4hRAIN, 4hBOW ,
     *             4hGLEN, 4hBRA , 4hLCLE, 4hBURN, 4hBLUM, 4h    ,
     *             4hLKWH, 4hIT  , 4hLKAQ, 4hUIL , 4hAQUI, 4hLLA ,
     *             4hHICO, 4h    , 4hCLIF, 4hTON , 4hVALL, 4hYMLS/
      DATA IDBRZ2 /4hCRAW, 4hFORD, 4hMCGR, 4hEGOR, 4hLKWA, 4hCO  ,
     *             4hWACO, 4hBRA , 4hLKLE, 4hON  , 4hDLEO, 4hNSAB,
     *             4hDLEO, 4hNLEO, 4hPROC, 4hTOR , 4hHAMI, 4hLTON,
     *             4hGATE, 4hSVIL, 4hPIDC, 4hOKE , 4hBELT, 4hON  ,
     *             4hKEMP, 4hNER , 4hYUNG, 4hSPRT, 4hLKGE, 4hORGE,
     *             4hGEOR, 4hGSFK, 4hGRAN, 4hGER , 4hROCK, 4hBRUS,
     *             4hROCK, 4hBRSH, 4hROCK, 4hSGR , 4hLITR, 4hIV  ,
     *             4hCAMR, 4hONUS, 4hCAME, 4hRON , 4hHIGH, 4hBANK,
     *             4hVALL, 4hEY  , 4hBRYA, 4hNBRA, 4hYEGU, 4hAMID,
     *             4hYEGU, 4hAEAS, 4hSOME, 4hRRES, 4hLYON, 4hS   ,
     *             4hGROE, 4hSBEK, 4hLIME, 4hSTON, 4hEAST, 4hERLY,
     *             4hBRYA, 4hNNAV, 4hCLLS, 4hTA  , 4hGIBB, 4hONS ,
     *             4hWASH, 4hNTON, 4hHEMP, 4hUS  , 4hHEMP, 4hSTED,
     *             4hMILL, 4hCR  , 4hRICH, 4hUS  , 4hRICH, 4hMOND,
     *             4hJULI, 4hFF  , 4hWEST, 4hCOL ,      22*4h    /
C
C.....COLORADO RIVER MARO AREAS.
C
      DATA IDCOL1 /4hLTHO, 4hMAS , 4hIRA , 4h    , 4hDUNN, 4h    ,
     *             4hCOLC, 4hITY , 4hLCOL, 4hCITY, 4hCHAM, 4hPION,
     *             4hBGSP, 4hRING, 4hWEST, 4hBROK, 4hSILV, 4hER  ,
     *             4hLSPE, 4hNCE , 4hOAKC, 4hRRES, 4hBALL, 4hINUS,
     *             4hBALL, 4hVALL, 4hBALL, 4hINGR, 4hBALL, 4hELM ,
     *             4hSTER, 4hLING, 4hCARL, 4hSBAD, 4hFISH, 4hER  ,
     *             4hCHRS, 4hTOVL, 4hTANK, 4hCHO , 4hTANK, 4hSPG ,
     *             4hKNIC, 4hKER , 4hBUTT, 4hES  , 4hSANA, 4hNPEC,
     *             4hSANA, 4hNCHO, 4hPAIN, 4hTLIP, 4hPAIN, 4hTKIK,
     *             4hPAIN, 4hTCHO, 4hOHIV, 4hIERU, 4hOHIV, 4hIER ,
     *             4hSTAC, 4hY   , 4hWINC, 4hHHOM, 4hWINC, 4hHELL,
     *             4hCROS, 4hSCUT, 4hLCOL, 4hEMAN, 4hBROW, 4hNUS ,
     *             4hLBRO, 4hWNWD, 4hBROW, 4hNWOD, 4hMULL, 4hIN  ,
     *             4hMENA, 4hRDUS, 4hMENA, 4hRD  , 4hBRAD, 4hY   ,
     *             4hLBRA, 4hDY  , 4hSSAB, 4hSABU, 4hSSAB, 4hSSAB,
     *             4hSSAB, 4hCOLU, 4hSSAB, 4hCOL , 4hLBUC, 4hHUS ,
     *             4hLBUC, 4hHAN /
      DATA IDCOL2 /4hJUNC, 4hNLUS, 4hJUNC, 4hTNLL, 4hTELE, 4hGRPH,
     *             4hJUNC, 4hTION, 4hJUNC, 4hJOHN, 4hMASO, 4hNJAM,
     *             4hMASO, 4hNUS , 4hMASO, 4hN   , 4hMASO, 4hNBEA,
     *             4hLLAN, 4hOUS , 4hLLAN, 4hO   , 4hKING, 4hSCOL,
     *             4hKING, 4hSSAN, 4hJOHN, 4hSUS , 4hJOHN, 4hSCTY,
     *             4hPEDF, 4hALLS, 4hLTRA, 4hVUS , 4hLTRA, 4hVIS ,
     *             4hBART, 4hON  , 4hAUST, 4hIN  , 4hONIO, 4hN   ,
     *             4hBAST, 4hRPUS, 4hBAST, 4hROP , 4hCEDA, 4hR   ,
     *             4hSMIT, 4hHVLE, 4hGRAN, 4hGEUS, 4hBUCK, 4hNER ,
     *             4hLAGR, 4hANGE, 4hCUMM, 4hINS , 4hCOLU, 4hMBUS,
     *             4hWHAR, 4hTUS , 4hWHAR, 4hTON , 4hBAYC, 4hTYUS,
     *             4hBAYC, 4hITY , 4hHALL, 4hETT , 4hYOAK, 4hUM  ,
     *             4hEDNA, 4h    , 4hSUBL, 4hIME , 4hSAND, 4hYUS ,
     *             4hSAND, 4hYCR , 4hMUST, 4hANG , 4hTEXA, 4hNA  ,
     *             4hBOLI, 4hNGUS, 4hBOLI, 4hNG  , 4hSWEE, 4hNY  ,
     *          12*4h    /
C
C.....RIO GRANDE RIVER MARO AREAS.
C
      DATA IDGRN1 /4hPRSI, 4hDARC, 4hCUCH, 4hILLP, 4hOJIN, 4hAGA ,
     *             4hPRSI, 4hDOBR, 4hALMI, 4hTOCU, 4hPRES, 4hIDOW,
     *             4hLAJI, 4hTAS , 4hTERL, 4hNGUS, 4hTERL, 4hNGUA,
     *             4hJOHN, 4hSONR, 4hBOQR, 4hSUS , 4hTORN, 4hILLC,
     *             4hBOQU, 4hILRS, 4hHEAT, 4hHCAN, 4hMARV, 4hLSCU,
     *             4hMARV, 4hLSCM, 4hMARV, 4hLSCD, 4hSFCS, 4hCOCU,
     *             4hSFCS, 4hCOCD, 4hMAXO, 4hNCK , 4hISIN, 4hGLSC,
     *             4hSAND, 4hRSNC, 4hDRYD, 4hEN  , 4hBIGC, 4hANUS,
     *             4hBIGC, 4hANDS, 4hMEYE, 4hRSCU, 4hMEYE, 4hRSCD,
     *             4hFOST, 4hERRH, 4hTUNI, 4hSCRK, 4hSHEF, 4hFELD,
     *             4hCHAN, 4hDLRU, 4hCHAN, 4hDLER, 4hHOWR, 4hDSDU,
     *             4hHOWA, 4hRDSD, 4hPAND, 4hLCRU, 4hPAND, 4hALEC,
     *             4hLANG, 4hTRY , 4hDEVI, 4hLSRU, 4hDEVI, 4hLSRM,
     *             4hEFDV, 4hLSRU, 4hEFDV, 4hLSRD, 4hBUCK, 4hLEYC,
     *             4hJOHN, 4hSRUN, 4hBAKE, 4hRSCR, 4hCOMS, 4hTOCK/
      DATA IDGRN2 /4hPAFF, 4hRDCU, 4hPAFF, 4hORDC, 4hAMIS, 4hTDRU,
     *             4hAMIS, 4hTDRM, 4hGOOD, 4hENSP, 4hAMIS, 4hTADR,
     *             4hDELR, 4hIO  , 4hARRO, 4hYOLV, 4hSYCA, 4hMORC,
     *             4hPINT, 4hOCK , 4hEAGL, 4hEPSU, 4hRSND, 4hIEGO,
     *             4hRSRO, 4hDRGO, 4hEAGL, 4hEPAS, 4hRESC, 4hDIDO,
     *             4hSANT, 4hONCR, 4hPALA, 4hFOX , 4hPALA, 4hFOXM,
     *             4hLARE, 4hDO  , 4hLARE, 4hDOM , 4hRSAB, 4hINAS,
     *             4hCARR, 4hANZR, 4hRODR, 4hIQUZ, 4hLTOR, 4hTLRS,
     *             4hLTOR, 4hTILL, 4hZAPA, 4hTAAM, 4hZAPA, 4hTAMX,
     *             4hFALC, 4hONRM, 4hFALC, 4hONR , 4hCDMI, 4hER  ,
     *             4hICAM, 4hOLE , 4hLHER, 4hRERS, 4hELCU, 4hCHIL,
     *             4hMTGO, 4hMEZR, 4hCAMA, 4hRGO , 4hRGRA, 4hNDEC,
     *             4hANZA, 4hLDSU, 4hANZA, 4hLDSD,      34*4h    /
C
C.....TRINITY RIVER MARO AREAS.
C
      DATA IDTRIN /4hJAKS, 4hBORO, 4hBDGP, 4hTRES, 4hBRID, 4hGEPT,
     *             4hBOYD, 4h    , 4hEGMT, 4hNRES, 4hLKWO, 4hRTH ,
     *             4hALED, 4hO   , 4hBENB, 4hRKLK, 4hFTWO, 4hRTH ,
     *             4hFTWO, 4hRTHW, 4hLKAR, 4hLING, 4hGPRA, 4hIRIE,
     *             4hJUST, 4hIN  , 4hGRAP, 4hEVLK, 4hGAIN, 4hESVL,
     *             4hRAYR, 4hOBLK, 4hSANG, 4hER  , 4hLEWI, 4hSVHC,
     *             4hLEWI, 4hSVLK, 4hCARR, 4hOLTN, 4hJOEP, 4hOOLL,
     *             4hMTNC, 4hRKLK, 4hDALL, 4hAS  , 4hDALL, 4hASS ,
     *             4hBLUE, 4hRIDG, 4hMCKI, 4hNNEY, 4hLAVO, 4hNLWC,
     *             4hLAVO, 4hNLK , 4hLKRA, 4hYHUB, 4hCRAN, 4hDALL,
     *             4hROSS, 4hER  , 4hKING, 4hSCKM, 4hCEDA, 4hRCKP,
     *             4hCEDA, 4hRCKR, 4hTRIN, 4hIDAD, 4hBARD, 4hWLK ,
     *             4hRICE, 4hUS  , 4hRICE, 4h    , 4hNAVM, 4hILLK,
     *             4hRICH, 4hCRUS, 4hRICH, 4hLCKR, 4hLONG, 4hLKUS,
     *             4hLONG, 4hLKTC, 4hLONG, 4hLKCC, 4hLONG, 4hLAKE,
     *             4hCROC, 4hKETE, 4hCROC, 4hKETT, 4hMADI, 4hSNVL,
     *             4hLKLI, 4hVUS , 4hLKLI, 4hVMID, 4hLKLI, 4hVWRC,
     *             4hLAKE, 4hLIV , 4hGOOD, 4hRICH, 4hROMA, 4hYOR ,
     *             4hLIBE, 4hRTY , 4hMOSS, 4hBLF ,      88*4h    /
C
C.....NECHES RIVER MARO AREAS.
C
      DATA IDNECH /4hLKPA, 4hLSTN, 4hNECH, 4hES  , 4hALTO, 4hNEC ,
     *             4hDIBO, 4hLL  , 4hROCK, 4hLAND, 4hJAKS, 4hONVL,
     *             4hSTRI, 4hKCKR, 4hCUSH, 4hING , 4hSHAW, 4hNEEC,
     *             4hALTO, 4hANG , 4hLUFK, 4hIN  , 4hCHIR, 4hENO ,
     *             4hSAUG, 4hUSTN, 4hSRAY, 4hBRUS, 4hSAMR, 4hAYBR,
     *             4hBAST, 4hHGLK, 4hEVAD, 4hALE , 4hKOUN, 4hTZE ,
     *             4hSOUR, 4hLAKE, 4hLAWS, 4hNSCR,     160*4h    /
C
C.....NUECES RIVER MARO AREAS.
C
      DATA IDNUEC /4hLAGU, 4hNA  , 4hBRAC, 4hKETV, 4hUVAL, 4hDENU,
     *             4hELMC, 4hK   , 4hTURK, 4hEYCK, 4hCOMA, 4hNCHC,
     *             4hASHE, 4hRTNU, 4hASHE, 4hRTON, 4hSROG, 4hUECK,
     *             4hCOTU, 4hLLA , 4hTILD, 4hENUN, 4hLOLM, 4hOSCK,
     *             4hSCAS, 4hMROC, 4hBLAC, 4hKCK , 4hTILD, 4hENMN,
     *             4hTILD, 4hENNU, 4hUVAL, 4hDEFR, 4hSABI, 4hNAL ,
     *             4hDERB, 4hYUS , 4hHOND, 4hOCUS, 4hHOND, 4hOCDS,
     *             4hLEON, 4hARVR, 4hDERB, 4hY   , 4hTILD, 4hENUF,
     *             4hTILD, 4hENFR, 4hTILD, 4hNSMU, 4hTILD, 4hENSM,
     *             4hCHOK, 4hECR , 4hWHIT, 4hSETU, 4hWHIT, 4hSETT,
     *             4hTRIV, 4hERSN, 4hTRIV, 4hERS , 4hLCOR, 4hPCHU,
     *             4hLCOR, 4hPCHR, 4hCALA, 4hLLEN, 4hREFU, 4hGIO ,
     *             4hFALF, 4hURRS, 4hALIC, 4hESDC, 4hALIC, 4hESFC,
     *             4hSKID, 4hMORE, 4hSINT, 4hON  ,     118*4h    /
C
C.....SABINE RIVER MARO AREAS.
C
      DATA IDSABN /4hGREE, 4hNVL , 4hQUIN, 4hLAN , 4hCADD, 4hOCK ,
     *             4hLTAW, 4hAKUS, 4hLKTA, 4hWAKN, 4hEMOR, 4hY   ,
     *             4hMINE, 4hOLUS, 4hMINE, 4hOLA , 4hLFOR, 4hKRUS,
     *             4hLKFO, 4hRKR , 4hQUIT, 4hMAN , 4hBIGS, 4hANDY,
     *             4hGLDW, 4hTRUS, 4hGLAD, 4hEWTR, 4hKILG, 4hORE ,
     *             4hLONG, 4hVIEW, 4hLCHE, 4hROKE, 4hBECK, 4hVL  ,
     *             4hMART, 4hINLK, 4hLGNS, 4hPTUS, 4hMURV, 4hAULL,
     *             4hLGNS, 4hPTDS, 4hBYOU, 4hCSTR, 4hTENA, 4hHACK,
     *             4hBYOU, 4hSPAT, 4hBYOU, 4hSMIG, 4hPGBA, 4hYOU ,
     *             4hTOLE, 4hDOBR, 4hBURK, 4hEVL , 4hROSE, 4hPINE,
     *             4hBONW, 4hIER , 4hDEWY, 4hVLUS, 4hDEWE, 4hYVL ,
     *             4hORAN, 4hGE  ,     132*4h    /
C
C.....SAN JACINTO RIVER MARO AREAS.
C
      DATA IDSANJ /4hLCON, 4hROEU, 4hLKCO, 4hNROE, 4hCONR, 4hOEUS,
     *             4hCONR, 4hOE  , 4hPORT, 4hER  , 4hSPRI, 4hNGUS,
     *             4hSPRI, 4hNG  , 4hCYPR, 4hESSC, 4hWSTF, 4hIELD,
     *             4hHUMB, 4hLE  , 4hSPLE, 4hNDCC, 4hSPLE, 4hNDPC,
     *             4hCLEV, 4hELND, 4hNEWC, 4hANEY, 4hHUFF, 4hMAN ,
     *             4hLKHO, 4hUUS , 4hLAKE, 4hHOU , 4hADDI, 4hCKSR,
     *             4hBARK, 4hERR , 4hHOUS, 4hTON ,      160*4h    /
C
C.....GUADALUPE RIVER MARO AREAS.
C
      DATA IDGUAD /4hHUNT, 4h    , 4hCOMF, 4hORT , 4hSPRI, 4hNGBR,
     *             4hCANY, 4hONLK, 4hNBRN, 4hFLAC, 4hNBRN, 4hFLBC,
     *             4hWIMB, 4hLYUS, 4hWIMB, 4hERLY, 4hKYLE, 4h    ,
     *             4hLULN, 4hGSMU, 4hLULI, 4hNGSM, 4hLULN, 4hGPCU,
     *             4hLULI, 4hNGPC, 4hGONZ, 4hLSUS, 4hGONZ, 4hLSMD,
     *             4hGONZ, 4hALES, 4hWHOF, 4hFSCU, 4hWHOF, 4hFSCK,
     *             4hWEST, 4hHOFF, 4hDILW, 4hORTH, 4hCUER, 4hOUS ,
     *             4hCUER, 4hO   , 4hVCTO, 4hRIAU, 4hVICT, 4hORIA,
     *             4hDUPO, 4hNT  , 4hBAND, 4hERA , 4hMEDI, 4hNALK,
     *             4hLACO, 4hSTE , 4hSOME, 4hRSET, 4hSANT, 4hONMR,
     *             4hSANT, 4hONSC, 4hELME, 4hNDRF, 4hFLSC, 4hTYSR,
     *             4hSELM, 4hA   , 4hFCIT, 4hYCCU, 4hFLSC, 4hTYCC,
     *             4hRUNG, 4hE   , 4hGOLI, 4hADUS, 4hGOLI, 4hAD  ,
     *             4hSCHR, 4hOEDR, 4hCOLE, 4hTOCR,     118*4h    /
C
      DATA IRVID  /4hBRAZ, 4hCOLO, 4hRIOG, 4hTRIN, 4hNECH, 4hNUEC,
     *             4hSABI, 4hSANJ, 4hGUAD/
C
      DATA IRVNAM /4hBRAZ, 4hOS R, 4hIVER, 4h    , 4h    , 4hCOLO,
     *             4hRADO, 4h RIV, 4hER  , 4h    , 4hRIO , 4hGRAN,
     *             4hDE R, 4hIVER, 4h    , 4hTRIN, 4hITY , 4hRIVE,
     *             4hR   , 4h    , 4hNECH, 4hES R, 4hIVER, 4h    ,
     *             4h    , 4hNUEC, 4hES R, 4hIVER, 4h    , 4h    ,
     *             4hSABI, 4hNE R, 4hIVER, 4h    , 4h    , 4hSAN ,
     *             4hJACI, 4hNTO , 4hRIVE, 4hR   , 4hGUAD, 4hALUP,
     *             4hE RI, 4hVER , 4h    /
C
      DATA NUMARO /89, 94, 83, 56, 20, 41, 34, 20, 41/
C
      ISTCOD = 0
C
C.....GET THE NUMBER OF THE RIVER SYSTEM TO BE PROCESSED.
C
      DO 100 MP = 1, 9
      IF(IRIV .NE. IRVID(MP)) GOTO 100
      NP = MP
      GOTO 200
  100 CONTINUE
C
C.....RIVER ID NOT FOUND.
C
      ISTCOD = 1
      GOTO 999
C
C.....GET THE STARTING ADDRESS IN THE INFORMATION ARRAYS WHERE THE
C.....INFORMATION FOR THIS RIVER SYSTEM IS STORED.
C
C.....STORE THE MARO IDS.
C
  200 JP = 200*(NP-1) + 1
      KP = 200*NP
      IP = 1
C
      DO 300 LP = JP, KP
      IMARO(IP) = IRVMRO(LP)
      IP = IP + 1
  300 CONTINUE
C
C.....STORE THE RIVER NAME.
C
      JP = 5*(NP-1) + 1
      KP = 5*NP
      IP = 1
C
      DO 400 LP = JP, KP
      NAMER(IP) = IRVNAM(LP)
      IP = IP + 1
  400 CONTINUE
C
C.....STORE THE NUMBER OF MARO AREAS IN THE RIVER SYSTEM.
C
      NMAROS = NUMARO(NP)
C
  999 RETURN
      END

