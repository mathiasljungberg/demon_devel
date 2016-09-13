      SUBROUTINE TRJPRO
C
C     Purpose: Molecular dynamics TRaJectory PROperty calculation.
C
C     History: - Creation (05.07.06, AMK)
C                         (24.07.15, LGMP)
C
C     ******************************************************************
C
C     List of local variables:
C
C     MDCYC : Molecular dynamics cycle of analysis.
C     SIMINT: Step interval for trajectory simulation.
C     TRJMAX: Maximum trajectory entry marked.
C     TRJMIN: Minimum trajectory entry marked.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'dynamics.h'
      INCLUDE 'fileio.h'
      INCLUDE 'flags.h'
      INCLUDE 'molecule.h'
      INCLUDE 'mp_demon.h'
C
      LOGICAL MDFLUSH
      INTEGER CYCLE,CCSET2
ctest
      integer i,j
ctest
      INTEGER ALLOCATION
      REAL,ALLOCATABLE :: CMO1(:,:,:),CMO2(:,:,:),W(:,:,:),
     $     OCC(:,:),WOCC(:)
C
C     ------------------------------------------------------------------
C
C     *** Quick return, if no trajectory simulation is requested ***
C
      IF (TRJTYP.NE.'PROPERTY') RETURN
C
C     *** Write trajectory header ***
C
      IF (PTRJ.NE.'XES') THEN
         CALL TRJOUT('HEADER')
      ELSE 
        IF (CCSET.EQ.1) THEN
           CCSET2 = 2
        ELSE
           CCSET2 = 1
        ENDIF
        ALLOCATE(CMO1(NSTO,NSTO,2),CMO2(NSTO,NSTO,2),W(NSTO,NSTO,2),
     $        OCC(NSTO,2),WOCC(NSTO),STAT=ALLOCATION)
        CALL CHKERRSTAT(ALLOCATION,'TRJANA','ALLOCATION FAILED')
      ENDIF
C
C     *** Loop over requested trajectory part ***
C
      CYCLE = 0
C
      CALL MP_SEND_ALL(SIMINT,1,MASTER,MP_INT)
      CALL MP_SEND_ALL(TRJMIN,1,MASTER,MP_INT)
      CALL MP_SEND_ALL(TRJMAX,1,MASTER,MP_INT)
      CALL MP_SEND_ALL(TRJINT,1,MASTER,MP_INT)

C
      DO MDCYC=TRJMIN,TRJMAX,SIMINT
C
C     *** Read trajectory step from trajectory tape ***
C
        CYCLE = CYCLE + 1
        RECNO(IOTRJ) = MDCYC
        CALL MDTRJ('READ')
ctest
        write(28,*) 'MD step :',MDCYC,'**************************'
        write(6,*) ' TRJPRO: COORDINATES AFTER READ - CYCLE: ',MDCYC
        do i = 1,natom
           write(6,'(3f9.5)') (coord(j,i,ccset),j=1,3)
        enddo
C
        CALL MP_SEND_ALL(COORD(1,1,CCSET),3*MAXATOM,MASTER,MP_REAL)
        IF(PTRJ.EQ.'XES') THEN
          WRITE(XRY,'(F20.2)') MDTIME*0.02418884326505 
          WRITE(XRY,'(F20.9)') EPOT
        ENDIF
C
C     *** Use standard orientation for rotational properties ***
C
        IF (NSRC.OR.RGTEN) THEN
          COORD(1:3,1:NATOM,1) = COORD(1:3,1:NATOM,CCSET)
          CALL SYMDRV
        END IF
C
C     *** Single point SCF calculation along trajectory ***
C
        restart=.true.
        CALL SCFDRV
C
C     *** Compute wave function overlaps for surface hopping
C
        IF (PTRJ.EQ.'XES' ) THEN
            status='XRAY'

C MLJ
C            call xraydrv
C end MLJ
            status='BOMD'
           IF (MDCYC.EQ.TRJMIN) THEN
              COORD(:,:,CCSET2) = COORD(:,:,CCSET)
              write(*,*) 'trjmin ',trjmin
              if (MPMASTER ) then
               CALL WFSTORE(W,NSTO,WOCC,'READ','REWIND')
              endif
              CMO1(:,:,1) = W(:,:,1)
              OCC(:,1)    = WOCC(:)
              if (MPMASTER ) then
               CALL WFSTORE(W,NSTO,WOCC,'READ','NOREWIND')
              endif 
              CMO1(:,:,2) = W(:,:,1)
              OCC(:,2)    = WOCC(:)
C MLJ
C              CALL OVLT1T2(CMO1,CMO2,W,NSTO,CCSET2)
C              if (MPMASTER) then
C                 CALL WFOVLAP_NOHOLE(W,NSTO,OCC,XSPIN)
C              endif
C end MLJ
           ELSE
               if (MPMASTER ) then
                CALL WFSTORE(W,NSTO,WOCC,'READ','REWIND')
              endif
              write(*,*) 'mdcycle ',mdcyc
              CMO2(:,:,1) = W(:,:,1)
              OCC(:,1)    = WOCC(:)
               if (MPMASTER) then
                CALL WFSTORE(W,NSTO,WOCC,'READ','NOREWIND')
               endif             
              CMO2(:,:,2) = W(:,:,1)
              OCC(:,2)    = WOCC(:)
              CALL OVLT1T2(CMO1,CMO2,W,NSTO,CCSET2)
              if (MPMASTER) then
C MLJ
C              CALL WFOVLAP(W,NSTO,OCC,XSPIN)
              CALL WFOVLAP_NOHOLE(W,NSTO,OCC,XSPIN)
C end MLJ
               endif
              CMO1(:,:,:) = CMO2(:,:,:)
              COORD(:,:,CCSET2) = COORD(:,:,CCSET)
           ENDIF
        ENDIF
C
C     *** Perform electrostatic analysis ***
C
        IF (PEM) THEN
          CALL ELAN
          CALL MDPRO('DIPOLE MOMENT')
        END IF
C
C     *** Calculate polarizabilities ***
C
        IF (POLARIS) THEN
          CALL BLDROMO
          CALL ANAPOL
          CALL MDPRO('POLARIZABILITY')
        END IF
C
C     *** Calculate NMR shieldings ***
C
        IF (NMR) THEN
          CALL SHIELDING
          IF (TRJNMR) CALL MDPRO('NMR SHIELDINGS')
          IF (TRJNSR) CALL MDPRO('NSR CONSTANTS')
        END IF
C
C     *** Calculate magnetizability ***
C
        IF (MXI) THEN
          CALL MAGNETIZE
          IF (TRJMAG) CALL MDPRO('MAGNETIZABILITY')
          IF (TRJROG) CALL MDPRO('ROTATIONAL GTENSOR')
        END IF
C
C     *** Update trajectory statistics ***
C
        CALL MDSTAT(CYCLE)
C
C     *** Store modified trajectory step on trajectory tape ***
C
        RECNO(IOTRJ) = MDCYC
        CALL MDTRJ('MODIFY')
C
C     *** Write trajectory output ***
C
        CALL TRJOUT('STEP')
C
C     *** Rewrite trajectory file ***
C
        IF (MDFLUSH(CYCLE,TRJINT)) CALL MDTRJ('REWRITE')
C
      END DO

      IF (PTRJ.EQ.'XES') THEN
         DEALLOCATE(CMO1,CMO2,W,OCC,WOCC,STAT=ALLOCATION)
         CALL CHKERRSTAT(ALLOCATION,'TRJANA','DEALLOCATION FAILED')
      ELSE
C
C     *** Rewrite trajectory file ***
C
         CALL MDTRJ('REWRITE')
C
C     *** Write trajectory statistics ***
C
         CALL TRJOUT('FINAL')
      ENDIF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE TRJPRO ***
C
      END
