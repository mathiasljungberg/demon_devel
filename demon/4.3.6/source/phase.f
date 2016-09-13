      SUBROUTINE PHASE(C,DSTO,NORB)
C
C     Purpose: Apply phase convention to molecular orbital coefficients.
C
C     History: - Creation (21.03.07, AMK)
C
C     ******************************************************************
C
C     List of local dimensions:
C
c     DSTO: Dimension of Slater type orbital matrix.
C
C     List of local variables:
C
C     C   : Molecular orbital coefficients.
C     NORB: Number of molecular orbitals.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
C add dynamics flags
      INCLUDE 'dynamics.h'
      INCLUDE 'flags.h'
      INCLUDE 'plot.h'
C
      INTEGER DSTO
      REAL C(DSTO,DSTO)
C
      LOGICAL DEFINED
      INTEGER IAO,IMO,ISTO,IORB,NORB
      REAL CAO
C
      REAL CC(MAXSTO)
C
C     ------------------------------------------------------------------
C
C     *** Apply phase convention to SCF solution ***
C
      IF ((STATUS.EQ.'SCF').OR.(STATUS.EQ.'GRID') .or. 
     $    (PTRJ.EQ.'XES')) THEN
C
C     *** Loop over all orbitals with explicit phase specification ***
C
        DO IMO=1,MOPHASE(0)
          IORB = MOPHASE(IMO)
          CALL READPHASE(IMO,CC,NORB)
          CAO = DOT_PRODUCT(CC(1:NORB),C(1:NORB,IORB))
          IF (CAO.LT.0.0) THEN
            C(1:NORB,IORB) = -C(1:NORB,IORB)
          END IF
        END DO
C
C     *** Loop over all orbitals with implicit phase specification ***
C
        DO IORB=1,NORB
          DEFINED = .FALSE.
          DO IMO=1,MOPHASE(0)
            IF (IORB.EQ.MOPHASE(IMO)) DEFINED = .TRUE.
          END DO
          IAO = 0
          CAO = -1.0
          IF (.NOT.DEFINED) THEN
            CC(1:NORB) = ABS(C(1:NORB,IORB))
            DO ISTO=1,NORB
              IF (CC(ISTO).GT.CAO) THEN
                IAO = ISTO
                CAO = CC(ISTO)
              END IF
            END DO
            IF ((IAO.GT.0).AND.(C(IAO,IORB).LT.0.0)) THEN
              C(1:NORB,IORB) = -C(1:NORB,IORB)
            END IF
          END IF
        END DO
C
      END IF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE PHASE ***
C
      END
