C resjffcwtco.f.f
C
C Fortran write carryover routine for RES-J
C
C History:
C 5/15/98      Eric Markstrom, RTi Created Routine
C 9/20/01      DWS, changed name from ResJ_ffcwtco.f to resjffcwtco.f
C
C kda   I       int: current day
C khr   I       int: current hour 
C ctemp I       array of floats: carryover array containing the array CO 
C                                vars for KDA, KHR
C num   I       int: number of values in array CTEMP 
C            
C           
C

      subroutine resjffcwtco( kda, khr, ctemp, num )
       integer kda
       integer khr
       real ctemp(1)
       integer num
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/resjffcwtco.f,v $
     . $',                                                             '
     .$Id: resjffcwtco.f,v 1.2 2001/09/20 13:27:15 dws Exp $
     . $' /
C    ===================================================================
C
     
C CALL RFS ROUTINE
       call FCWTCO( kda, khr, ctemp, num )

      end       
