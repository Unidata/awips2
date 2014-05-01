C MODULE PRP47
C
      SUBROUTINE PRP47(PO)
C.......................................
C     THIS ROUTINE PRINTS THE CONTENTS OF THE PO ARRAY FOR
C     THE PEAKFLOW OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY. . .
C        BRYCE FINNERTY - HRL   DECEMBER 1994
cbf  modified by Bryce Finnerty December 1997 to accomodate changes related
cbf to new USGS peakflow data, and porting of operation to the workstations.
cbf
C.......................................
      DIMENSION PO(*), SNAME(2)
C
C     COMMON BLOCKS.
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_peakflow/RCS/prp47.f,v $
     . $',                                                             '
     .$Id: prp47.f,v 1.3 1998/04/09 10:20:36 page Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME /4HPRP4,4H7   /
C.......................................
C     TRACE LEVEL FOR ROUTINE=1, DEBUG SWITCH=IBUG
c      ITRACE=1
C
      CALL FPRBUG(SNAME,1,47,IBUG)
C.......................................
      WRITE(IPR,500) (PO(I),I=2,6)
      WRITE(IPR,510) PO(11),PO(12)
      WRITE(IPR,530) PO(7),PO(8)
      WRITE(IPR,540) PO(10)
      WRITE(IPR,550) PO(9)
      WRITE(IPR,565) PO(13)
      WRITE(IPR,575) PO(14)
      WRITE(IPR,580) PO(15)
      WRITE(IPR,590) PO(16)
      write(ipr,595) (po(j),j=17,24)
  500 FORMAT(10X,'PEAKFLOW OPERATION FOR :                       ',
     &'      ',5A4)
  510 FORMAT(10X,'OBSERVED INSTANTANEOUS PEAK FLOW U.S.G.S.',
     &' STATION NUMBER :        ',2A4)
  530 FORMAT(10X,'SIMULATED DISCHARGE T.S. IDENTIFIER :              ',
     &'                  ',2A4)
  540 FORMAT(10X,'SIMULATED DISCHARGE T.S. TIME INTERVAL :           ',
     &'                  ',F5.0)
  550 FORMAT(10X,'SIMULATED DISCHARGE T.S. DATA TYPE :               ',
     &'                  ',A4)
  565 FORMAT(10X,'WINDOW SIZE FOR SEARCHING :                        ',
     &'                  ',F5.0)
  575 FORMAT(10X,'TOTAL NUMBER OF OBSERVED PEAKS TO BE PROCESSED :   ',
     &'                 ',F6.0)
  580 FORMAT(10X,'DISPLAY OPTION SWITCH, 0=TABLE, 1=TABLE AND GRAPH :',
     &'                  ',F5.0)
  590 FORMAT(10X,'DISPLAY PEAKS 0=CHRONOLOGICALLY, 1=BY MAGNITUDE :  ',
     &'                  ',F5.0)
  595 format(10x,'INPUT FILE NAME FOR U.S.G.S. PEAKFLOW DATA :       ',
     &'                  ',8A4)
C
C     ITRACE = 1
      IF (ITRACE.GE.1) WRITE(IODBUG,910)
  910 FORMAT(10X,'** PRP47 EXITED **')
      RETURN
      END

