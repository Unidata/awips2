
C      set upvrsx with the given information
       subroutine set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

       include "upvrsx_types"

       COMMON /UPVRSX/ UPVRSXa,UPVRSXb,UPVRSXc,mUPVRSX,UPVRSXe,UPVRSXf
       CHARACTER*4 UPVRSXe,UPVRSXf
       CHARACTER*8 UPVRSXc,UPVRSXb
       CHARACTER*10 UPVRSXa

       UPVRSXa = PGMVRN
       UPVRSXb = PGMVRD
       UPVRSXc = PGMNAM
       mUPVRSX = MPGMRG
       UPVRSXe = PGMCMP
       UPVRSXf = PGMSYS
       end
