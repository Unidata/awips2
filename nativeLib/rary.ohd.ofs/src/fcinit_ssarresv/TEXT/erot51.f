C MODULE EROT51
C---------------------------------------------------------------------
C
C  ROUTINE TO PRINT ERROR MESSAGES OCCURRING IN PIN51
C
C  ERRORS THAT HAVE OCCURRED ARE HELD IN /ERR51/. INFO HELD THERE
C  INCLUDES THE NUMBER OF THE ERROR, THE LINE NUMBER IT OCCURRED ON',
C  THE FIELD NO. WITHIN THE LINE IT OCCURRED ON, AND THE CHARACTER
C  WITHIN THE STRING WHERE THE ERROR STARTED.
C
      SUBROUTINE EROT51
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/err51'
C
C  EMSG(15,NALL): WHERE NALL IS THE NUMBER OF STATEMENTS IN ALL',
C          NALL = NMSG(1) + NMSG(2) + NMSG(3)
C  THIS SHOULD BE MODIFIED IF THERE IS ANY NEW MSG TO COME.
C
      CHARACTER*8    EMSG(15,138),FMT(15)
C
C  EACH MGXXXX(N) ALLOCATES  STORAGE FOR N/15 STATEMENTS
C
      CHARACTER*8
     *          MG   1(75),MG   6(75),MG  11(75),MG  16(75),
     *          MG  21(75),MG  26(75),MG  31(75),MG  36(75),
     *          MG  41(75),MG  46(75),MG  51(75),MG  56(75),
     *          MG  61(75),MG  66(75),MG  71(75),MG  76(75),
     *          MG  81(75),MG  86(75),MG  91(75),MG  96(75),
     *          MG 101(75),MG 106(75),MG 111(75),MG 116(75),
     *          MG 121(75),MG 126(75),
     *          MG 500(90),
     *          MG1000(30)
C
C  THE ENTRIES IN (EMSG(1, E1),MG 500) AND (EMSG(1, E2),MG1000)
C  ARE  E1 = NMSG(1) + 1 AND
C       E2 = NMSG(1) + NMSG(2) + 1 , RESPECTIVELY.
C  THESE SHOULD BE MODIFIED IF THERE IS ANY NEW MSG TO COME.
C
      EQUIVALENCE    (EMSG(1, 1),MG 1(1)),(EMSG(1, 6),MG 6(1)),
     *               (EMSG(1,11),MG11(1)),(EMSG(1,16),MG16(1)),
     *               (EMSG(1,21),MG21(1)),(EMSG(1,26),MG26(1)),
     *               (EMSG(1,31),MG31(1)),(EMSG(1,36),MG36(1)),
     *               (EMSG(1,41),MG41(1)),(EMSG(1,46),MG46(1)),
     *               (EMSG(1,51),MG51(1)),(EMSG(1,56),MG56(1)),
     *               (EMSG(1,61),MG61(1)),(EMSG(1,66),MG66(1)),
     *               (EMSG(1,71),MG71(1)),(EMSG(1,76),MG76(1)),
     *               (EMSG(1,81),MG81(1)),(EMSG(1,86),MG86(1)),
     *               (EMSG(1,91),MG91(1)),(EMSG(1,96),MG96(1)),
     *               (EMSG(1,101),MG101(1)),(EMSG(1,106),MG106(1)),
     *               (EMSG(1,111),MG111(1)),(EMSG(1,116),MG116(1)),
     *               (EMSG(1,121),MG121(1)),(EMSG(1,126),MG126(1)),
     *               (EMSG(1,131),MG500(1)),
     *               (EMSG(1,137),MG1000(1))
C
      DIMENSION ISTRT(3),IEND(3),NMSG(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/erot51.f,v $
     . $',                                                             '
     .$Id: erot51.f,v 1.4 2001/06/13 09:59:01 mgm Exp $
     . $' /
C    ===================================================================
C
C  THE MSG STATEMENTS RANGE FROM 'ISTRT(I) TO IEND(I), I=1,2,3',
C  FOR THE 1ST ,2ND AND 3RD GROUP CORRESPONDINGLY.  IF THERE IS ANY
C  REARRANGE FOR THEM, THEN A SUITABLE CHANGE SHOULD BE MADE TO THE
C  ENTRIES OF ISTRT AND IEND.
C
      DATA ISTRT/ 1,500,1000/
      DATA IEND /130,505,1001/
C
      DATA IBLNK/4H    /
C
      DATA MG   1/
     *  '(''   NOT',' A VALID',' KEYWORD',', EITHER',' NOT REC',
     *  'OGNIZABL','E OR NOT',' ALLOWED',' IN CURR','ENT SITU',
     *  'ATION.'')','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   REA','L VALUE ','EXPECTED',', NOT FO','UND.'')  ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   INT','EGER VAL','UE EXPEC','TED, NOT',' FOUND.''',
     *  ')       ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  6/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG 11/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  16/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   INV','ALID UNI','TS SPEC ','KEYWORD ','USED, TH',
     *  'EY ARE ','''''ENGLISH',''''' OR ''''','METRIC''''',' OR THEI',
     *  'IR FIRST',' LETTER.',''')      ','        ','        ',
C
     *  '(''   NUL','L FIELD ','NOT ALLO','WED.'')  ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   FIE','LD LONGE','R THAN A','LLOWED.''',')       ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  21/
     *  '(''   NO ','END CARD',' FOUND.',''')       ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   AT ','LEAST TW','O DEPEND','ENT FLOW',' MUST BE',
     *  ' USED TO',' DEFINE ','A BACKWA','TER CURV','E IN THE',
     *  ' BACKWAT','ER TABLE','.'')     ','        ','        ',
C
     *  '(''   ELE','VATION/S','TORAGE/D','ISCHARGE',' CURVE S',
     *  'HOULD BE',' ENTERED',' BEFORE ','THIS CAR','D TO CHE',
     *  'CK VALUE',' IS WITH','IN BOUND','S.'')    ','        ',
C
     *  '(''   FLO','W  IS PO','SSIBLE O','NLY IF E','LEVATION',
     *  ' AT SITE',' IS GREA','TER THAN',' CONTROL',' ELEVATI',
     *  'ON.'')   ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  26/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  31/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   PAR','AMETER K','EYWORD N','EEDED.'')','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  36/
     *  '(''   TIM','E-SERIES',' KEYWORD',' NEEDED.',''')      ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   CAR','RYOVER K','EYWORD N','EEDED.'')','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   NOT',' A VALID',' UNITS S','PECIFICA','TION.'') ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   MUL','TIPLE EN','TRIES OF',' KEYWORD',' NOT ALL',
     *  'OWED.'') ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   NUM','BER OF V','ALUES EN','TERED CA','N NOT BE',
     *  ' DIVIDED',' ''/''   I','NTO A SE','T OF PAI','RS.'')   ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  41/
     *  '(''   VAL','UES ARE ','NOT IN A','SCENDING',' ORDER.''',
     *  ')       ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   INV','ALID BAC','KWATER C','ONTROL K','EYWORD U',
     *  'SED, THE',' CORRECT','KEYWORD ','SHOULD B','E  ELEV ',
     *  ' OR  FLO','W. '')   ','        ','        ','        ',
C
     *  '(''   INC','OMPLETE ','INPUT FO','R INFLOW','  SECTIO',
     *  'N.'')    ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   TIM','E-SERIES',' ALREADY',' SPECIFI','ED IN TH',
     *  'IS SECTI','ON.'')   ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   TIM','E-SERIES',' DOES NO','T EXIST,',' OR MISS',
     *  'ING DATA',' NOT ALL','OWED.'') ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  46/
     *  '(''   TIM','E-SERIES',' DATATYP','E HAS WR','ONG DIME',
     *  'NSIONS.''',')       ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   TIM','E INTERV','AL FOR A','LL THE T','IME-SERI',
     *  'ES MUST ','BE BETWE','EN 1-24 ','HOURS, I','NCLUSIVE',
     *  '.'')     ','        ','        ','        ','        ',
C
     *  '(''   ALL','  TIME-S','ERIES MU','ST HAVE ','SAME TIM',
     *  'E INTERV','AL AND M','UST BE T','HE SAME ','AS TIME-',
     *  'SERIES ','''''INSTQO2','''''.'')   ','        ','        ',
C
     *  '(''   NO ','CARRYOVE','R FOR IN','FLOW HAS',' BEEN EN',
     *  'TERED.'')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   NO ','CARRYOVE','R FOR DI','SCHARGE ','HAS BEEN',
     *  ' ENTERED','.'')     ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  51/
C
     *  '(''   NO ','CARRYOVE','R HAS BE','EN ENTER','ED FOR P',
     *  'OOL OR S','TORAGE.',''')       ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  56/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   FIR','ST STORA','GE IN EL','EV. VS S','TORAGE C',
     *  'URVE MUS','T BE 0.0',', FIRST ','ELEVATIO','N IS THE',
     *  ' BOTTOM ','OF THE R','ESERVOIR','.'')     ','        ',
C
     *  '(''   A _','________','___ KEYW','ORD REQU','IRED BUT',
     *  ' NOT FOU','ND.'',T6,','3A4)    ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   A _','________','___ KEYW','ORD FOUN','D, NOT N',
     *  'EEDED.'',','T6,3A4) ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  61/
     *  '(''   ONL','Y POSITI','VE VALUE','S ALLOWE','D.'')    ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   ELE','VATION N','OT WITHI','N BOUNDS',' OF ELEV',
     *  'ATION VS',' STORAGE',' CURVE.',''')       ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   NO ','OF DISCH','ARGE VAL','UES IS N','OT EQUAL',
     *  ' TO     ','''/''   NO','. OF ELE','VATION V','ALUES IN',
     *  ' ELVSSTO','R CURVE ',''')      ','        ','        ',
C
     *  '(''   NUM','BER OF V','ALUES EN','TERED IS',' NOT AN ',
     *  'EVEN    ',' ''/''   M','ULTIPLE ','OF THREE',''')      ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  66/
C
     *  '(''   NUM','BER OF V','ALUES EN','TERED IS',' NOT AN ',
     *  'EVEN    ',' ''/''   M','ULTIPLE ','OF SEVEN',''')      ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  71/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  76/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  81/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  86/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  91/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   INC','ORRECT U','NITS, __','__, FOR ','THIS TIM',
     *  'E-SERIES','.'',T21,A','4)      ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  96/
     *  '(''   FIE','LD LONGE','R THAN A','LLOWED.',''')       ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG 101/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG 106/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG  111/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG 116/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   NUM','BR OF VA','LUES IN ','LIST EXC','EEDS ARR',
     *  'AY DIMEN','SION    ',''')      ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG 121/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG 126/
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG 500/
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   MAX',' WORDS O','F WORK A','RRAY USE','D - LEFT',
     *  'W PLUS I','USEW.'') ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''    '')','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C
      DATA MG1000/
     *  '(''   MOR','E THAN 5','0 ERRORS',' OCCURRE','D.'')    ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        ',
C
     *  '(''   MOR','E THAN 5','0 WARNIN','GS OCCUR','RED.'')  ',
     *  '        ','        ','        ','        ','        ',
     *  '        ','        ','        ','        ','        '/
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      LDEBUG=0
      IF (LDEBUG.EQ.1) THEN
         NUMERR=0
C     MAXIMUM NUMBER OF ERRORS IN COMMON ERR51 IS 50
10       NUMERR=NUMERR+50
         DO 30 I=1,NUMERR
            IERTYP(I)=I
            DO 20 J=1,3
               IERNME(J,I)=IBLNK
20             CONTINUE
30          CONTINUE
         ENDIF
C
      IF (NUMERR.EQ.0) RETURN
C
      DO 40 I=1,3
         NMSG(I)=IEND(I)-ISTRT(I)+1
40       CONTINUE
C
      DO 170 I=1,NUMERR
         N=IERTYP(I)
         IF (IERLN(I).EQ.0) THEN
            WRITE (IPR,50) N
50    FORMAT ('0**ERROR** ERROR NUMBER ',I4,
     *        ' ENCOUNTERED:')
            ELSE
               WRITE (IPR,60) N,IERLN(I),IERFLD(I),IERPOS(I)
60    FORMAT ('0**ERROR** ERROR NUMBER ',I4,
     *        ' ENCOUNTERED ON LINE ',I3,
     *        ', FIELD NUMBER ',I3,
     *        ', CHARACTER NUMBER',I3,
     *        ':')
            ENDIF
C     LOCATION FOR N=1,2,3,...IEND(1)
         IF (N.GT.IEND(1)) GO TO 70
            LOC=N
            GO TO 110
C     LOC IS THE LOCATION IN EMSG FOR N = ISTRT(2),...,IEND(2)
70       M=ISTRT(2)
         IF (.NOT.(N.GE.M.AND.N.LE.IEND(2))) GO TO 80
            LOC = N - (N/M)*M + NMSG(1) + 1
            GO TO 110
C     FOR N=ISTRT(3),...,IEND(3)
80       M=ISTRT(3)
         IF (.NOT.(N.GE.M.AND.N.LE.IEND(3))) GO TO 90
            LOC=N-(N/M)*M+NMSG(1)+NMSG(2)+1
            GO TO 110
C     ERROR NUMBER NOT VALID
90       WRITE (IPR,100) N
100   FORMAT ('0**WARNING** NO INFORMATION AVAILABLE FOR ERROR NUMBER ',
     *   I4,'.')
         CALL WARN
         GO TO 170
C     START PRINTING ERROR MSG IN VARIABLE FORMAT BASED ON IERNME
110      DO 120 K=1,15
            FMT(K)=EMSG(K,LOC)
120         CONTINUE
        IF (LDEBUG.GT.1) WRITE (IPR,130) FMT
130   FORMAT (' FMT=',15A)
        IF (IERNME(1,I).EQ.IBLNK) GO TO 140
           NWORDS=3
           IF (N.EQ.93) NWORDS=1
           WRITE (IPR,FMT,ERR=150) (IERNME(K,I),K=1,NWORDS)
           CALL ERROR
           GO TO 170
140     WRITE (IPR,FMT,ERR=150)
        CALL ERROR
        GO TO 170
150     IF (FMT(1).NE.'(''    '')') then
           WRITE (IPR,160) FMT
160   FORMAT ('0**ERROR** EROT51 - THE FOLLOWING IS AN INVALID ',
     *   'FORMAT: ' /
     *   11X,15A)
           CALL ERROR
           ENDIF
170     CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         IF (NUMERR.LT.150) THEN
            NUMERR=NUMERR+50
            GO TO 10
            ENDIF
         ENDIF
C
      IF (IERR.GT.0) THEN
         WRITE (IPR,180) IERR,VALUE1,VALUE2,IPT
180   FORMAT ('0**ERROR** ERROR NUMBER ',I4,' : ',
     *   'VALUE1=',F11.2,5X,'VALUE2=',F11.2,5X,'AT POINT=',I4)
         CALL ERROR
         ENDIF
C
      RETURN
C
      END
