      SUBROUTINE CLOUT(KSA,PA,DSTO,DSTO_ME)
C
C     Purpose: Closed shell SCF output.
C
C     History: - Creation (09.09.99, AMK)
C                         (27.06.08, AG)
C                         (05.02.10, GG)
C
C     ******************************************************************
C
C     List of local dimensions:
C
C     DSTO   : Dimension of Slater typ orbital matrix.
C     DSTO_ME: Dimension of Slater typ orbital matrix on node ME.
C
C     List of local variables:
C
C     KSA   : MO coefficients.
C     NUCREP: Nuclear repulsion.
C     PA    : Density matrix.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'fileio.h'
      INCLUDE 'flags.h'
      INCLUDE 'iteration.h'
      INCLUDE 'molecule.h'
      INCLUDE 'mp_demon.h'
      INCLUDE 'physic.h'
      INCLUDE 'scf.h'
      INCLUDE 'scfvec.h'
      INCLUDE 'xc.h'
      INCLUDE 'dynamics.h'
C
      INTEGER DSTO,DSTO_ME
      REAL KSA(DSTO,DSTO),PA(DSTO_ME,DSTO_ME)
C
      LOGICAL STATED
      CHARACTER*80 PRTSTR,STRCOMP
      INTEGER IMO,STREXT
      REAL NUCREP
C
      REAL RDUMMY(1)
C
C     ------------------------------------------------------------------
C
C     *** Save restart information ***
C
      IF (((STATUS.EQ.'SCF').OR.(STATUS.EQ.'SADSCF').OR.
     $     (STATUS.EQ.'GRID').OR.(STATUS.EQ.'BOMD').OR.
     $     (STATUS.EQ.'OPT').OR.(STATUS.EQ.'SAD').OR.
     $     (STATUS.EQ.'IRC').OR. STATED('SCAN')).AND.
     $     (.NOT.FREQRST)) THEN
        CALL RSTIO(KSA,EIGVALA,PA,DSTO,DSTO_ME,OCCNA,AOCCNA,ELCFGA,
     $             MOEXCA,NMOEXCA,LFOMOA,HFOMOA,'WRITE','ALPHA')
      END IF
C
      IF (MPSLAVE) GO TO 100
      IF (STOREWF) THEN
         IF (REWWF) THEN
            CALL WFSTORE(KSA,DSTO,OCCNA,'WRITE','REWIND')
            CALL WFSTORE(KSA,DSTO,OCCNA,'WRITE','NOREWIND')
         ELSE
            CALL WFSTORE(KSA,DSTO,OCCNA,'WRITE','NOREWIND')
            CALL WFSTORE(KSA,DSTO,OCCNA,'WRITE','NOREWIND')
         ENDIF
      ENDIF
C
C     *** Write energy and basis into MOLDEN, MOLEKEL or WFN files ***
C
      IF ((STATUS.EQ.'SCF').AND.(.NOT.OPT).OR.
     $    (STATUS.EQ.'GRID').AND.(.NOT.OPT)) THEN
        IF (MOLDEN.EQ.'FULL') THEN
          CALL MOLDRV(RDUMMY,RDUMMY,RDUMMY,RDUMMY,1,1,'BAS')
          CALL MOLDRV(EIGVALA,KSA,RDUMMY,RDUMMY,DSTO,DSTO,'ALPHA')
        ELSE IF (MOLEKEL.EQ.'FULL') THEN
          CALL MKLDRV(RDUMMY,RDUMMY,1,'BAS','UNDEFINED')
          CALL MKLDRV(EIGVALA,KSA,DSTO,'MOS','ALPHA')
          CALL MKLDRV(EIGVALB,KSA,DSTO,'MOS','BETA')
        ELSE IF (WFNFILE.EQ.'FULL') THEN
          CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'REWIND')
          CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'HEADER')
          CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'BASIS')
          CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'ALPHA')
          CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'END')
        END IF
      ELSE IF (((STATUS.EQ.'OPT').AND.OPTIMIZED).OR.
     $         ((STATUS.EQ.'BOMD').AND.OPTIMIZED)) THEN
        CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'REWIND')
        CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'HEADER')
        CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'BASIS')
        CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'ALPHA')
        CALL WFNDRV(EIGVALA,OCCNA,KSA,NSTO,'END')
      END IF
C
C     *** Calculate converged energy ***
C
      ESCF = EHARTREE + EXC + EVDW + NUCREP()
C
C     *** Write SCF output ***
C
      IF ((STATUS.EQ.'SCF').OR.(STATUS.EQ.'GRID')) THEN
        IF (STATUS.EQ.'GRID') THEN
          IF (.NOT.BREAK) CALL RMSCFOUT
          WRITE (PRTSTR,5000) SCFCYC
 5000     FORMAT ('SCF GRID GENERATED IN ',I5,' CYCLES')
          IF (RANGRD) PRTSTR = 'RANDOMIZED '//PRTSTR(1:STREXT(PRTSTR))
          CALL MESSAGE(0,STRCOMP(PRTSTR),1,1)
        END IF
        WRITE (OUT,5010) ENUCLEUS,ECOULOMB,EHARTREE
 5010   FORMAT (/,T2,'ELECTRONIC CORE ENERGY      = ',F20.9,
     $          /,T2,'ELECTRONIC COULOMB ENERGY   = ',F20.9,
     $          /,T2,'ELECTRONIC HARTREE ENERGY   = ',F20.9)
        WRITE (OUT,5020) EXSUM,ECSUM,EXC
 5020   FORMAT (/,T2,'EXCHANGE ENERGY             = ',F20.9,
     $          /,T2,'CORRELATION ENERGY          = ',F20.9,
     $          /,T2,'EXCHANGE-CORRELATION ENERGY = ',F20.9)
        IF (QMONLY.AND.(EVDW.EQ.0.0)) THEN
          WRITE (OUT,5030) EHARTREE+EXC,NUCREP(),ESCF
 5030     FORMAT (/,T2,'ELECTRONIC SCF ENERGY       = ',F20.9,
     $            /,T2,'NUCLEAR-REPULSION ENERGY    = ',F20.9,
     $            /,T2,'TOTAL ENERGY                = ',F20.9,/)
        ELSE IF (QMONLY) THEN
          WRITE (OUT,5040) EHARTREE+EXC,EVDW,NUCREP(),ESCF
 5040     FORMAT (/,T2,'ELECTRONIC SCF ENERGY       = ',F20.9,
     $            /,T2,'EMPIRICAL DISPERSION ENERGY = ',F20.9,
     $            /,T2,'NUCLEAR-REPULSION ENERGY    = ',F20.9,
     $            /,T2,'TOTAL ENERGY                = ',F20.9,/)
        ELSE IF (EVDW.EQ.0.0) THEN
          WRITE (OUT,5050) EHARTREE+EXC,NUCREP(),ESCF
 5050     FORMAT (/,T2,'ELECTRONIC SCF ENERGY       = ',F20.9,
     $            /,T2,'NUCLEAR-REPULSION ENERGY    = ',F20.9,
     $            /,T2,'TOTAL SCF ENERGY            = ',F20.9,/)
        ELSE
          WRITE (OUT,5060) EHARTREE+EXC,EVDW,NUCREP(),ESCF
 5060     FORMAT (/,T2,'ELECTRONIC SCF ENERGY       = ',F20.9,
     $            /,T2,'EMPIRICAL DISPERSION ENERGY = ',F20.9,
     $            /,T2,'NUCLEAR-REPULSION ENERGY    = ',F20.9,
     $            /,T2,'TOTAL SCF ENERGY            = ',F20.9,/)
        END IF
      ELSE IF (((STATUS.EQ.'OPT').AND.OPTIMIZED).OR.
     $         ((STATUS.EQ.'BOMD').AND.OPTIMIZED)) THEN
        IF (.NOT.(BREAK.OR.PRTMOE.OR.PRTMOS.OR.PRTOPT)) CALL RMSCFOUT
        WRITE (OUT,5010) ENUCLEUS,ECOULOMB,EHARTREE
        WRITE (OUT,5020) EXSUM,ECSUM,EXC
        IF (QMONLY.AND.(EVDW.EQ.0.0)) THEN
          WRITE (OUT,5030) EHARTREE+EXC,NUCREP(),ESCF
        ELSE IF (QMONLY) THEN
          WRITE (OUT,5040) EHARTREE+EXC,EVDW,NUCREP(),ESCF
        ELSE IF (EVDW.EQ.0.0) THEN
          WRITE (OUT,5050) EHARTREE+EXC,NUCREP(),ESCF
        ELSE
          WRITE (OUT,5060) EHARTREE+EXC,EVDW,NUCREP(),ESCF
        END IF
      ELSE 
        IF (.NOT.BREAK) CALL RMSCFOUT
      END IF

c add ground state energy to surface hopping input file
      IF(PTRJ.EQ.'XES') THEN
         write(XRY,'(F20.9)') ESCF

      ENDIF
         write(*,*) 'status',status
C
C     *** Write additional output for failed SCF ***
C
      IF (BREAK) THEN
        IF (STATUS.EQ.'SCF') CALL NEWDRV('NEW INPUT FILE')
        CALL MESSAGE(0,'MO OCCUPATIONS AND ENERGIES [eV]',1,1)
        WRITE (OUT,'(T2,"   MO"," OCCUPATION "," ENERGY [eV] ")')
        DO IMO=MAX(1,NOMOA-5),MIN(NORB,NOMOA+5)
          WRITE (OUT,5070) IMO,OCCNA(IMO),EIGVALA(IMO)*EVOLT
 5070     FORMAT (T2,I5,2X,F7.4,3X,F10.3)
        END DO
      END IF
C
  100 CONTINUE
C
C     *** Flush buffer ***
C
      CALL DEFLUSH(OUT)
C
C     *** Stop program, if SCF is not converged ***
C
      IF (BREAK) CALL ERRMSG('CLOUT','PROGRAM STOP',1)
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE CLOUT ***
C
      END
