      SUBROUTINE ATOMDIS2(ATOMA,ATOMB,RVECTOR,RSQUARE,CCSET2)
C
C     Purpose: Calculation of atomic distance vector and square 
C              distance using two timesteps along BOMD trajectory.
C
C     History: - Creation (20.07.15, LGMP)
C
C     ******************************************************************
C
C     List of local variables:
C
C     ATOM{A/B}: Local atoms.
C     RVECTOR  : Atomic distance vector.
C     RSQUARE  : Interatomic square distance.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'molecule.h'
C
      INTEGER ATOMA,ATOMB,ICOOR,CCSET2
      REAL DISTANCE2,RSQUARE
C
      REAL RVECTOR(3)
C
C     ------------------------------------------------------------------
C
C     *** Calculate interatomic square distance ***
C
      RSQUARE = DISTANCE2(ATOMA,ATOMB,CCSET2)**2
C
C     *** Calculate interatomic vectors ***
C
      ENTRY ATOMVEC2(ATOMA,ATOMB,RVECTOR)
C
      DO 10 ICOOR=1,3
        RVECTOR(ICOOR) = COORD(ICOOR,ATOMA,CCSET2) -
     $                   COORD(ICOOR,ATOMB,CCSET)
   10 CONTINUE
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE ATOMDIS2 ***
C
      END
