      SUBROUTINE XRAYOUT(EIGVAL,TDM,NOMO,IHOLE)
C
C     Purpose: Output routine for x-ray absorption and emission 
C              spectroscopy (XAS/XES).
C
C     History: - Creation (13.12.09, AMK)
C                         (09.12.11, AMK, GG, LGM)
C
C     ******************************************************************
C
C     List of local variables:
C
C     EIGVAL: Molecular orbital eigenvalues.
C     TDM   : X-ray transition dipole moments.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'fileio.h'
      INCLUDE 'flags.h'
      INCLUDE 'molecule.h'
      INCLUDE 'mp_demon.h'
      INCLUDE 'physic.h'
      INCLUDE 'dynamics.h'
C
      REAL EIGVAL(*),TDM(3,*)
C
      CHARACTER*80 PRTSTR,STRCOMP
      INTEGER IHOLE,IOMO,IORB,IUMO,IXYZ,NOMO,NSTATE,NXES,I,J
      REAL DELTA,DELTAEV,STRENGTH
C
C     ------------------------------------------------------------------
C
C     *** Write output header for XES ***
C
      IF (XES) THEN
        WRITE (PRTSTR,5000) IHOLE
 5000   FORMAT ('XES CALCULATION FOR MO #: ',I5)
        CALL MESSAGE(0,STRCOMP(PRTSTR),1,0)
        WRITE (PRTSTR,5010) -EIGVAL(IHOLE)*EVOLT
 5010   FORMAT ('IONIZATION POTENTIAL = ',F12.2,' EV')
        CALL MESSAGE(0,STRCOMP(PRTSTR),1,0)
      END IF
C
C     *** Write transition moment table header ***
C
      WRITE (OUT,5020)
 5020 FORMAT (/,T2,3X,'TRANSITION',11X,'TRANSITION MOMENTS',//,
     $          T2,'  NO.',4X,'E[eV]',3X,'STRENGTH',7X,'X',10X,
     $          'Y',10X,'Z',/)
C
C     *** Write transition moment table to deMon.out & deMon.xry ***
C
      IF (MPMASTER) THEN
C
C     *** Write XAS output ***
C
        IF (XAS) THEN
C
          NSTATE = NXAS - NOMO
          WRITE(XRY,'(A5,I5)') 'XAS  ',NSTATE
C
C     *** Loop over virtual molecular orbitals ***
C
          DO IUMO=1,NSTATE
            IORB = NOMO + IUMO
            DELTA = EIGVAL(IORB) - EIGVAL(IHOLE)
            STRENGTH = DOT_PRODUCT(TDM(1:3,IORB),TDM(1:3,IORB))
            STRENGTH = 2.0/3.0*DELTA*STRENGTH
            DELTAEV  = DELTA*EVOLT
            WRITE (OUT,5030) IUMO,DELTAEV,STRENGTH,
     $                       (TDM(IXYZ,IORB),IXYZ=1,3)
 5030       FORMAT (T2,I5,2X,F8.2,1X,4(F8.4,3X))
            WRITE (XRY,5040) DELTA,STRENGTH,STRENGTH,
     $                       (TDM(IXYZ,IORB),IXYZ=1,3)
 5040       FORMAT (F22.10,2F22.16,3D20.12)
          END DO
C
C     *** Write XES output ***
C
        ELSE IF (XES) THEN
C
          NXES = 0
          NSTATE = NOMO
C
          DO IUMO=1,NSTATE
            DELTA = EIGVAL(IUMO) - EIGVAL(IHOLE)
            IF (DELTA.GT.0.0) NXES = NXES + 1
          END DO
            IF(PTRJ.EQ.'XES')THEN
              WRITE(XRY,'(F10.3)') -EIGVAL(IHOLE)*EVOLT
            ENDIF
C
          WRITE(XRY,'(A5,I5)') 'XES  ',NXES
C
C     *** Loop over occupied molecular orbitals ***
C
          DO IOMO=1,NSTATE
            DELTA = EIGVAL(IOMO) - EIGVAL(IHOLE)
            IF (DELTA.GT.0.0) THEN
              STRENGTH = DOT_PRODUCT(TDM(1:3,IOMO),TDM(1:3,IOMO))
              STRENGTH = (DELTA**3)/3.0*STRENGTH
              DELTAEV = DELTA*EVOLT
              WRITE (OUT,5030) IOMO,DELTAEV,STRENGTH,
     $                         (TDM(IXYZ,IOMO),IXYZ=1,3)
              WRITE (XRY,5040) DELTA,STRENGTH,STRENGTH,
     $                         (TDM(IXYZ,IOMO),IXYZ=1,3)
            END IF
          END DO
C    
c         write(XRY,*) 'Coordinates for particular step'    
c         do i=1,natom
c          write(XRY,'(3F20.6)') (COORD(j,i,2), j=1,3)
c        enddo
C         write(XRY,'(A5)') '     '
        END IF
C
      END IF
C
      WRITE (OUT,'(/)')
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE XRAYOUT ***
C
      END
