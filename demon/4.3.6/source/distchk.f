      SUBROUTINE DISTCHK(OPTION)
C
C     Purpose: Distance check.
C
C     History: - Creation (16.08.96, BZ)
C                         (25.03.10, GG)
C
C     ******************************************************************
C
C     List of global dimensions:
C
C     NATOM_ALL: Number of atoms.
C     NDUMMY   : Number of dummy atoms.
C
C     List of global variables:
C
C     COORD : Coordinate field.
C     OPTION: a) OPTION = SORTED, check sorted coordinates.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'flags.h'
      INCLUDE 'molecule.h'
C
      CHARACTER*(*) OPTION
      CHARACTER*80 STRCOMP,STRING
      INTEGER IATOM,JATOM,NATOMS
      REAL DISTANCE,DISTIJ
C
C     ------------------------------------------------------------------
C
C     *** Number of atoms and dummy atoms ***
C

C MLJ disable this check
      RETURN

      NATOMS = NATOM_ALL + NDUMMY
C
C     *** Check atomic distance ***
C
      DO IATOM=1,NATOMS
        DO JATOM=IATOM+1,NATOMS
          DISTIJ = DISTANCE(IATOM,JATOM)
          IF (DISTIJ.LT.0.05) THEN
            IF (OPTION.EQ.'SORTED') THEN
              WRITE (STRING,5000) IATOM,TRIM(ATOMLAB(ATOMMAP(IATOM,1))),
     $                            JATOM,TRIM(ATOMLAB(ATOMMAP(JATOM,1))),
     $                            DISTIJ
            ELSE
              WRITE (STRING,5000) IATOM,TRIM(ATOMLAB(IATOM)),JATOM,
     $                            TRIM(ATOMLAB(JATOM)),DISTIJ
            END IF
 5000       FORMAT ('DISTANCE BETWEEN ','ATOM ',I5,' (',A,') AND ATOM ',
     $              I5,' (',A,') IS ONLY ',F6.3,' A.U.')
            IF (ATOMNO(IATOM)*ATOMNO(JATOM).EQ.0) THEN
              CALL ERRMSG('DISTCHK',STRCOMP(STRING),0)
            ELSE
              CALL ERRMSG('DISTCHK',STRCOMP(STRING),1)
            END IF
            IF (STATUS.EQ.'READ IN') THEN
              CALL PRICAR('CARTESIAN COORDINATES',CCSET)
              IF (INPTYP.NE.'CARTESIAN') THEN
                CALL PRIZMAT('GENERATED Z-MATRIX',3)
              END IF
            END IF
          ELSE IF (DISTIJ.LT.0.15) THEN
            IF (OPTION.EQ.'SORTED') THEN
              WRITE (STRING,5000) IATOM,TRIM(ATOMLAB(ATOMMAP(IATOM,1))),
     $                            JATOM,TRIM(ATOMLAB(ATOMMAP(JATOM,1))),
     $                            DISTIJ
            ELSE
              WRITE (STRING,5000) IATOM,TRIM(ATOMLAB(IATOM)),JATOM,
     $                             TRIM(ATOMLAB(JATOM)),DISTIJ
            END IF
            CALL ERRMSG('DISTCHK',STRCOMP(STRING),0)
          END IF
        END DO
      END DO
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE DISTCHK ***
C
      END
