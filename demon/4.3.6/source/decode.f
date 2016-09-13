      SUBROUTINE DECODE(KEYWORD,OPTIONS,NOPTION)
C
C     Purpose: Decode input keys.
C
C     History: - Creation (16.01.96, AMK)
C                         (05.03.10, GG)
C                         (19.02.15, AMK)
C
C     ******************************************************************
C
C     List of local variables:
C
C     DEFINED: If true, a special option form is defined.
C     KEYWORD: Keyword to be found.
C     OPTIONS: Options of the keyword.
C
C     ******************************************************************
C
C     List of numerical flags:
C
C     CDFTOL : Tolerance for charge density fitting convergence.
C     CDPTOL : Tolerance for charge density perturbation convergence.
C     CDVAL  : User defined charge density mixing value.
C     ERITOL : Tolerance for ERI thresholds.
C     FFSTEP : Finite field step.
C     FIXCYC : Start SCF cycle for fixing of MOs.
C     FIXTOL : Tolerance for fixing of MOs.
C     GRDTOL : Tolerance for automatic grid generation.
C     IRCMAX : Maximum number of intrinsic reaction coordinate steps.
C     IRCTOL : Tolerance for IRC path convergence.
C     ISOTOL : Tolerance for isosurface reduction.
C     LSMIN  : Dynamic level shift minimum value.
C     LSVAL  : Level shift value.
C     MDMAX  : Maximum number of molecular dynamics step.
C     MDMIN  : Minimum number of molecular dynamics step.
C     OPTMAX : Maximum number of optimization iterations.
C     OPTTOL : Tolerance for geometry optimization convergence.
C     PCGMAX : Maximum number of PCG iterations.
C     SADMAX : Maximum number of saddle interpolation steps.
C     SADTOL : Tolerance for saddle convergence.
C     SCFMAX : Maximum number of SCF iterations.
C     SCFOUT : Number of SCF iterations with print output.
C     SCFTOL : Tolerance for SCF convergence.
C     SMRVAL : SMEAR value.
C     SVDTOL : Tolerance for singular value decomposition.
C     TDELTA : Step value for temperature.
C     TLOWER : Lower temperature boundary.
C     TUPPER : Upper temperature boundary.
C     VIBSTEP: Vibrational displacement step.
C     WPOLAR : Dynamical (hyper)-polarizability frequencies.
C     XASTOL : Tolerance for XAS orthogonalization.
C
C     List of global control flags:
C
C     CDFTYP : Flag for charge density fitting type.
C     CTYP   : Flag for correlation functional.
C     C6READ : Flag for C6 coefficient read-in format.
C     FITTING: Flag for fitting procedure.
C     GGFUN  : Flag for grid generating function.
C     GRDTYP : Flag for grid type.
C     HREAD  : Flag for Hessian read-in format.
C     INPTYP : Flag for input type.
C     LCTYP  : Flag for local correlation functional in GGA.
C     MATDIA : Flag for matrix diagonalization technique.
C     MOLDEN : Flag for MOLDEN output.
C     MOLEKEL: Flag for MOLEKEL output.
C     PLOT   : Flag for plot output.
C     PLOTTYP: Flag for plot type.
C     PRODIA : Flag for projector diagonalization technique.
C     RADQUA : Flag for radial quadrature type.
C     TDDIA  : Flag for TDDFT matrix diagonalization technique.
C     UNIT   : Flag for BOHR or ANGSTROM unit system.
C              Entry 1: Length unit.
C              Entry 2: Velocity unit.
C              Entry 3: Acceleration unit.
C              Entry 4: Dynamic frequency unit.
C     WEIGHTS: Flag for atomic weight function.
C     WFNFILE: Flag for WFN file output.
C     XTYP   : Flag for exchange functional.
C
C     List of global logical flags:
C
C     BATH     : If true, heat bath for the MD is requested.
C     BWDER    : If true, include Becke's weight derivatives.
C     CDXC     : If true, fitted exchange-correlation is requested.
C     CHARMM   : If true, QM/MM forces for CHARMM are calculated.
C     CUKS     : If true, a constrained UKS calculation is requested.
C     DIRGRD   : If true, a direct grid integration is requested.
C     ERIRAM   : If true, distributed ERI storage in RAM is requested.
C     ESA      : If true, a electronic structure analysis is requested.
C     EXX      : If true, exact exchange is requested.
C     FREQ     : If true, frequency analysis is requested.
C     FREQRST  : If true, restart an old frequency analysis.
C     GGA      : If true, a GGA functional is requested.
C     HYBRID   : If true, a hybrid functional is requested.
C     IRC      : If true, a IRC optimization is requested.
C     LAP      : If true, a meta-GGA functional is requested.
C     MD       : If true, a molecular dynamic simulation is requested.
C     MDRUN    : If true, a molecular dynamics run is requested.
C     MULTIPOLE: If true, a multipole expansion of ERIs is requested.
C     OKS      : If true, open-shell Kohn-Sham is requested.
C     OMA      : If true, the optimal mixing approach is requested.
C     OPT      : If true, geometry optimization is requested.
C     PORT     : If true, interface ports are activated.
C     PROP     : If true, property calculation is requested.
C     RANGRD   : If true, randomize SCF generated grid.
C     ROKS     : If true, restricted open-shell Kohn-Sham is requested.
C     ROTGRD   : If true, rotate local atomic coordinate systems.
C     SADDLE   : If true, perform a saddle interpolation.
C     SADTS    : If true, a TS is searched after a saddle interpolation.
C     SCANPES  : If true, scan potential energy surface.
C     SCF      : If true, a single point start SCF is requested.
C     SPHORB   : If true, spherical orbitals will be used.
C     SSFG98   : If true, calculate G98 spin scaling funcion.
C     STOREWF  : If true, store wave function after each MD step.
C     SYMGEO   : If true, symmetry is enforced in geometry optimization.
C     SYMMETRY : If true, use molecular symmetry.
C     TS       : If true, a transition state search is requested.
C     USELMO   : If true, use localized molecular orbitals.
C     USELPI   : If true, use localized molecular pi orbitals.
C     VIBONLY  : If true, only vibrational sum of states is calculated.
C
C     List of SCF flags:
C
C     CFGATOM: If true, an atomic configuration is requested.
C     DIIS   : If true, DIIS for SCF is requested.
C     FERMI  : If true, a Fermi start density is requested.
C     FIXCFG : If true, the electronic configuration will be fixed.
C     FIXLAST: If true, FIXCFG acts on the last SCF cycle.
C     HUECKEL: If true, a Hueckel start density is requested.
C     LSHIFT : If true, level shifting is performed.
C     MOCC   : If true, MO occupation will be modified.
C     MOEX   : If true, MOs will be exchanged.
C     PROJCT : If true, a projected start density is requested.
C     SCFADJ : If true, SCF adjustment in optimization is requested.
C     SETAOCC: If true, fixed atomic occupation number will be used.
C     SMEAR  : If true, fractional orbital occupation is enabled.
C     SMRUNI : If true, uniform fractional occupation is enabled.
C     USEDIA : If true, use diagonal SVD decomposition.
C     USELUD : If true, use LU (Cholesky) decomposition.
C     USEONE : If true, use identity start matrix for PCG solver.
C     USESVD : If true, use singular value decomposition.
C     VDW    : If true, add empirical van der Waals energy.
C
C     List of optimization and PES scan flags:
C
C     HESSIAN : Start Hessian for geometry optimization.
C     MAXDR   : Maximum step size for optimization.
C     RELAXING: If true, relax geometry during PES scan.
C     STEPTYP : Step type for geometry optimization.
C     UPHESS  : Update type for geometry optimization.
C
C     List of molecular dynamics flags:
C
C     MDBATH  : Requested molecular dynamics bath.
C     MDFLP   : True for LP force conservation.
C     MDINT   : Step interval for MD printing.
C     MDSTEP  : Requested molecular dynamics time step.
C     MDTEMP  : Requested molecular dynamics temperature.
C     MDVLP   : True for LP velocity conservation.
C     MDZERO  : Zero external moments in molecular dynamics.
C     RDFMAX  : Maximum extension in radial distribution function.
C     RDFWIDTH: Width of bins in radial distribution function.
C
C     List of property flags:
C
C     BPA    : If true, perform Bader population analysis.
C     DEFORM : If true, use deformation density in population analysis.
C     DOS    : If true, an AO resolved DOS is calculated.
C     EFIELD : If true, electric field embedding is requested.
C     EFISH  : If true, an EFISH calculation is requested.
C     EMBED  : If true, point charge embedding is requested.
C     FBLMO  : If true, a Foster-Boys MO localization is requested.
C     FPA    : If true, perform fuzzy (Becke) population analysis.
C     FUKUI  : If true, perform a Fukui reactivity analysis.
C     FULLPA : If true, an orbital resolved population analysis is done.
C     HPA    : If true, perform Hirshfeld population analysis.
C     ISOABS : If true, an absolute isosurface value is given.
C     ITHFO  : If true, perform ITHPA with frac. occupation numbers.
C     ITHPA  : If true, perform iterative Hirshfeld analysis.
C     LPA    : If true, perform Loewdin population analysis.
C     MAGOUT : If true, write magnetic properties output.
C     MODMAP : If true, align with best preserved connectivity.
C     MPA    : If true, perform Mulliken population analysis.
C     MXI    : IF true, calculate molecular magnetizability xi.
C     NICS   : If true, calculate nuclear independent chemical shifts.
C     NMR    : If true, calculate nuclear magnetic resonance.
C     NONCOL : If true, calculate noncollinear exications in TDDFT.
C     NPA    : If true, generate file for natural bond order analysis.
C     NQR    : If true, calculate nuclear quadrupole resonance.
C     NSRC   : If true, calculate spin-rotation constant.
C     PEM    : If true, calculate permanent electrostatic moments.
C     PMLMO  : If true, a Pipek-Mezey MO localization is requested.
C     POLARIS: If true, calculate polarizabilities.
C     RAMAN  : If true, calculate Raman intensities.
C     RGTEN  : If true, calculate rotational g-tensor.
C     SIGPI  : If true, calculate sigma-pi energy separation.
C     SPHPA  : If true, perform spherical atom Hirshfeld analysis.
C     TDA    : If true, use Tamm-Dancoff approximationin TDDFT.
C     TDDFT  : If true, calculate excited states via TDDFT.
C     THERMOS: If true, calculate thermodynamical data.
C     VPA    : If true, perform Voronoi population analysis.
C     XAS    : If true, x-ray absorption spectroscopy is requested.
C     XES    : If true, x-ray emission spectroscopy is requested.
C
C     List of plot and isosurface flags:
C
C     ISOINT : Isosurface interpolation.
C     ISOVAL : Isosurface function value.
C     PLOTMOA: Plot function orbital specification (alpha).
C     PLOTMOB: Plot function orbital specification (beta).
C
C     List of print flags:
C
C     PRTAMAT : If true, print auxiliary function matrix.
C     PRTAUX  : If true, print AUX table.
C     PRTBAS  : If true, print contraction for basis set optimization.
C     PRTC6   : If true, print C6 coefficient table.
C     PRTCHI  : If true, print magnetic suceptibility information.
C     PRTCIS  : If true, print CIS ERI batching output.
C     PRTCON  : If true, print molecular dyanmics constraints.
C     PRTCPS  : If true, print critical points.
C     PRTDBG  : If true, debug output is requested.
C     PRTDE2  : If ture, print hessian during geoemetry optimization.
C     PRTECP  : If true, print ECP information.
C     PRTEMBED: If true, print coordinates of embedding cube.
C     PRTERIS : If true, print electron repulsion integrals.
C     PRTFIT  : If true, print iterative fitting cycles.
C     PRTFUK  : If true, print Fukui matrix.
C     PRTGEO  : If true, print system/molecular geometry.
C     PRTGRD  : If true, print grid data.
C     PRTGTO  : If true, print GTO table.
C     PRTKS   : If true, print Kohn-Sham matrix.
C     PRTLMAT : If true, print Loewdin transformation matrix.
C     PRTMAG  : If true, print magnetizability tensors.
C     PRTMCP  : If true, print MCP information.
C     PRTMD   : If true, print molecular dynamics information.
C     PRTMOE  : If true, print MO energies (short output).
C     PRTMOS  : If true, print MO coefficients (full output).
C     PRTNIA  : If true, print NIA iterations.
C     PRTNMR  : If true, print NMR information.
C     PRTNQR  : If true, print NQR information.
C     PRTOPT  : If true, print optimization information.
C     PRTPOP  : If true, print population density matrix.
C     PRTPMAT : If true, print P matrix.
C     PRTPRI  : If true, print primitive coordinate information.
C     PRTRAM  : If true, print RAM allocation.
C     PRTRMAT : If true, print response matrix.
C     PRTSAD  : If true, print saddle optimization.
C     PRTSCF  : If true, print always SCF cycles.
C     PRTSMAT : If true, print overlap integrals.
C     PRTSTO  : If true, print STO table.
C     PRTSYM  : If true, print symmetry information.
C     PRTTB   : If true, print tight-binding information.
C     PRTTD   : If true, print TDDFT information.
C     PRTTMAT : If true, print kinetic energy integrals.
C     PRTXCE  : If true, print exchange-correlation energy.
C     PRTXCV  : If true, print exchange-correlation potential.
C     VERBOSE : If true, print all warnings.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'align.h'
      INCLUDE 'cfp.h'
      INCLUDE 'custom.h'
      INCLUDE 'dynamics.h'
      INCLUDE 'embed.h'
      INCLUDE 'flags.h'
      INCLUDE 'freq.h'
      INCLUDE 'grid.h'
      INCLUDE 'iteration.h'
      INCLUDE 'mm.h'
      INCLUDE 'molden.h'
      INCLUDE 'molecule.h'
      INCLUDE 'mp_demon.h'
      INCLUDE 'opt.h'
      INCLUDE 'physic.h'
      INCLUDE 'plot.h'
      INCLUDE 'pointer.h'
      INCLUDE 'polarizability.h'
      INCLUDE 'property.h'
      INCLUDE 'scf.h'
      INCLUDE 'scfvec.h'
      INCLUDE 'symmetry.h'
      INCLUDE 'tddft.h'
      INCLUDE 'thermo.h'
      INCLUDE 'threshold.h'
      INCLUDE 'tolerance.h'
      INCLUDE 'xc.h'
C
      LOGICAL DEFINED,NOTIN,VARSCAN
      CHARACTER*32 KEYSTR
      CHARACTER*(*) KEYWORD
      CHARACTER*160 ERRSTR,STRCOMP
      CHARACTER*80 NUMBER,PRTSTR,UTL,VCTYPE,VXTYPE
      INTEGER IOPTION,JOPTION,KEYLEN,NOPTION,STREXT,SYMBOL
      REAL DIGIT
C
      CHARACTER*80 OPTIONS(MAXOPT)
C
C     ------------------------------------------------------------------
C
C     *** Definition of keyword string ***
C
      KEYLEN = STREXT(KEYWORD)
      IF (KEYLEN.GT.5) THEN
        KEYSTR = KEYWORD(1:5)//UTL(KEYWORD(6:KEYLEN))
      ELSE
        KEYSTR = KEYWORD(1:)
      END IF
C
C     ------------------------------------------------------------------
C
C     *** Select input keyword ***
C
      SELECT CASE (KEYWORD)
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 1 - MOLECULAR KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Molecular charge ***
C
      CASE ('CHARGE')
C
        CHARGE = INT(DIGIT('CHARGE','VALUE','ALL',OPTIONS(1)))
C
C     ------------------------------------------------------------------
C
C     *** Symmetry options ***
C
      CASE ('SYMMETRY')
C
        IF (NOPTION.EQ.0) THEN
          SYMGEO = .TRUE.
          SYMMETRY = .TRUE.
        END IF
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(1)(1:2).EQ.'ON') THEN
            SYMGEO = .TRUE.
            SYMMETRY = .TRUE.
          ELSE IF (OPTIONS(1)(1:3).EQ.'OFF') THEN
            SYMGEO = .FALSE.
            SYMMETRY = .FALSE.
          ELSE IF (OPTIONS(1)(1:4).EQ.'NONE') THEN
            SYMGEO = .FALSE.
            SYMMETRY = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Structure alignment options ***
C
      CASE ('ALIGNMENT')
C
        ALIGNMENT = .TRUE.
        IF (.NOT.SADDLE) SCF = .FALSE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:3).EQ.'OFF') THEN
            SADALI = .FALSE.
            ALIGNMENT = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            POPTOL = INT(DIGIT('ALIGN','TOL=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'CUBE=') THEN
            NUMBER = OPTIONS(IOPTION)(6:)
            CUBLEV = INT(DIGIT('ALIGN','CUBE=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'CONNE') THEN
            MODMAP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ENANT') THEN
            ENANTIO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'EXCLU') THEN
            CALL READBLACK
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'UNIFO') THEN
            UNIFORM = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Geometry pattern options ***
C
      CASE ('PATTERN')
C
        CALL READPAT
C
C     ------------------------------------------------------------------
C
C     *** Geometry options ***
C
      CASE ('PRODUCT')
      CASE ('REACTANT')
      CASE ('GEOMETRY')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION).EQ.'BOHR') THEN
            UNIT(1) = 'BOHR'
            UNIT(2) = 'A.U.'
            UNIT(3) = 'A.U.'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ANGST') THEN
            UNIT(1) = 'ANGSTROM'
            UNIT(2) = 'ANGSTROM/FSEC'
            UNIT(3) = 'ANGSTROM/FSEC^2'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'CARTE') THEN
            INPTYP = 'CARTESIAN'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MIXED') THEN
            INPTYP = 'MIXED'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ZMATR') THEN
            INPTYP = 'INTERNAL'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'Z-MAT') THEN
            INPTYP = 'INTERNAL'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read geometry body and select run type ***
C
        CALL READGEO
        CALL RUNTYPE
        CALL LINKGEO
C
      CASE ('CONSTANTS')
      CASE ('VARIABLES')
C
C     ------------------------------------------------------------------
C
C     *** Scan options ***
C
      CASE ('SCAN')
C
        SCANPES = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'END=') THEN
            SCANEND = OPTIONS(IOPTION)(5:)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ADIAB') THEN
            RELAXING = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'VERTI') THEN
            RELAXING = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'STEP=') THEN
            NUMBER = OPTIONS(IOPTION)(6:)
            SCANVAR(1,0) = ABS(DIGIT('SCAN','STEP=','ALL',NUMBER))
          ELSE IF (VARSCAN(OPTIONS(IOPTION))) THEN
            SCANLAB = OPTIONS(IOPTION)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
        CALL READSCAN
C
C     ------------------------------------------------------------------
C
C     *** Electric field embedding options ***
C
      CASE ('EFIELD')
C
        EFIELD = .TRUE.
C
        IF (NOPTION.NE.3) THEN
          CALL MESSAGE(1,'3 ELECTRIC FIELD COMPONENTS ARE NEEDED')
          CALL ERRMSG('DECODE','ERROR IN EFIELD KEYWORD',1)
        END IF
C
        DO IOPTION=1,NOPTION
          NUMBER = OPTIONS(IOPTION)(1:)
          ELFIELD(IOPTION) = DIGIT('EFIELD','VALUE','ALL',NUMBER)
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Embedding options ***
C
      CASE ('EMBED')
C
        EMBED = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'NOPOL') THEN
            MONOPOLE = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'-9999') THEN
            CALL GETCUBE
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FILE') THEN
            CALL GETCUBE
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'READ') THEN
            CALL READCUBE
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 2 - LCAO KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Forzen auxiliary function fit options ***
C
      CASE ('FREEZE')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'NONE') THEN
            FITTYP = 'ALL'
            ZETFIX(0) = FIXNONE
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            FITTYP = 'VALENCE'
            NUMBER = OPTIONS(IOPTION)(5:)
            ZETFIX(0) = DIGIT('FREEZE','TOL=','>=0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CUSP') THEN
            FITTYP = 'VALENCE'
            ZETFIX(0) = FIXCUSP
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CORE') THEN
            FITTYP = 'VALENCE'
            ZETFIX(0) = FIXCORE
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'VALEN') THEN
            FITTYP = 'VALENCE'
            ZETFIX(0) = FIXVALENCE
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
        CALL SETFIX
C
C     ------------------------------------------------------------------
C
C     *** Auxiliary function options ***
C
      CASE ('AUXIS')
C
        IF (OPTIONS(1).NE.'-99999') THEN
          ATOMAUX(0) = OPTIONS(1)
        END IF
C
        CALL FINDAUXIS(ATOMAUX(0),.FALSE.)
        CALL AUXIS
C
C     ------------------------------------------------------------------
C
C     *** Basis set augmentation options ***
C
      CASE ('AUGMENT')
C
        AUGMENTED = .TRUE.
C
        IF (OPTIONS(1).NE.'-99999') THEN
          ATOMBAS(0,3) = OPTIONS(1)
        END IF
C
        IF (STATUS.EQ.'READ IN') THEN
          CALL FINDAUG(ATOMBAS(0,3))
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Basis set options ***
C
      CASE ('BASIS')
C
        IF (OPTIONS(1).NE.'-99999') THEN
          ATOMBAS(0,1) = OPTIONS(1)
        END IF
C
        CALL FINDBASIS(ATOMBAS(0,1),'BASIS')
        CALL ORBITAL
C
C     ------------------------------------------------------------------
C
C     *** ECP options ***
C
      CASE ('ECPS')
C
        IF (OPTIONS(1).NE.'-99999') THEN
          ATOMECP(0) = OPTIONS(1)
        END IF
C
        CALL FINDECP(ATOMECP(0))
        CALL POTENTIAL('ECPS',.TRUE.)
        CALL POTENTIAL('CHECK',.FALSE.)
C
C     ------------------------------------------------------------------
C
C     *** MCP options ***
C
      CASE ('MCPS')
C
        IF (OPTIONS(1).NE.'-99999') THEN
          ATOMMCP(0) = OPTIONS(1)
        END IF
C
        CALL FINDMCP(ATOMMCP(0))
        CALL POTENTIAL('MCPS',.TRUE.)
        CALL POTENTIAL('CHECK',.FALSE.)
C
C     ------------------------------------------------------------------
C
C     *** Orbital options ***
C
      CASE ('ORBITALS')
C
        IF (OPTIONS(1)(1:5).EQ.'CARTE') THEN
          SPHORB = .FALSE.
        ELSE IF (OPTIONS(1)(1:5).EQ.'SPHER') THEN
          SPHORB = .TRUE.
        ELSE
          WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
          CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Molecular orbital localization options ***
C
      CASE ('LOCALIZATION')
C
        USELPI = .TRUE.
C
        CALL READPIS
C
C     ------------------------------------------------------------------
C
C     *** ERI options ***
C
      CASE ('ERIS')
C
        DEFINED = .FALSE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            DEFINED = .TRUE.
            NUMBER = OPTIONS(IOPTION)(5:)
            ERITOL = DIGIT('ERIS','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'CONVE') THEN
            ERITYP = 'NOT DIRECT'
            ERIRAM = .FALSE.
            MULTIPOLE = .FALSE.
            IF (.NOT.DEFINED) ERITOL = 1.0E-14
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DIREC') THEN
            ERITYP = 'DIRECT'
            ERIRAM = .FALSE.
            MULTIPOLE = .FALSE.
            IF (.NOT.DEFINED) ERITOL = 1.0E-10
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MIXED') THEN
            ERITYP = 'DIRECT'
            ERIRAM = .TRUE.
            MULTIPOLE = .TRUE.
            IF (.NOT.DEFINED) ERITOL = 1.0E-10
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MULTI') THEN
            ERITYP = 'DIRECT'
            ERIRAM = .FALSE.
            MULTIPOLE = .TRUE.
            IF (.NOT.DEFINED) ERITOL = 1.0E-10
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 3 - SCF KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** SCF options ***
C
      CASE ('SCFTYPE')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            IF (NUMBER.EQ.'0') THEN
              SCFMAX = 0
            ELSE
              SCFMAX = INT(DIGIT('SCFTYPE','MAX=','>=0',NUMBER))
            END IF
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CDF=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            CDFTOL = DIGIT('SCFTYPE','TOL=','> 0',NUMBER)
            CDPTOL = DIGIT('SCFTYPE','TOL=','> 0',NUMBER)
            CDFTHRESH = DIGIT('SCFTYPE','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            SCFTOL = DIGIT('SCFTYPE','TOL=','> 0',NUMBER)
            SCFTHRESH = DIGIT('SCFTYPE','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NOTIG') THEN
            SCFADJ = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'RKS') THEN
            OKS = .FALSE.
            CUKS = .FALSE.
            ROKS = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'UKS') THEN
            OKS = .TRUE.
            CUKS = .FALSE.
            ROKS = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CUKS') THEN
            OKS = .TRUE.
            CUKS = .TRUE.
            ROKS = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'ROKS') THEN
            OKS = .TRUE.
            CUKS = .FALSE.
            ROKS = .TRUE.
            CALL READROKS(OPTIONS,NOPTION)
          ELSE IF (NOTIN('SCFTYPE',OPTIONS(IOPTION))) THEN
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Start density options ***
C
      CASE ('GUESS')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:2).EQ.'TB') THEN
            FERMI = .FALSE.
            HUECKEL = .TRUE.
            PROJCT = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CORE') THEN
            FERMI = .FALSE.
            HUECKEL = .FALSE.
            PROJCT = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'ONLY') THEN
            SCF = .TRUE.
            RESTART = .FALSE.
            GUESSONLY = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FERMI') THEN
            FERMI = .TRUE.
            HUECKEL = .FALSE.
            PROJCT = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'HBOMD') THEN
            HBOMD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PROJE') THEN
            FERMI = .FALSE.
            HUECKEL = .FALSE.
            PROJCT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'RESTA') THEN
            FERMI = .FALSE.
            HUECKEL = .FALSE.
            PROJCT = .FALSE.
            RESTART = .TRUE.
            IF (OPT.AND.(INPTYP.EQ.'CARTESIAN').AND.
     $                  (OPTTYP.NE.'CARTESIAN')) THEN
              NEWINP = .TRUE.
            END IF
          ELSE IF (OPTIONS(IOPTION)(1:1).EQ.'(') THEN
            ATOMBAS(0,2) = OPTIONS(IOPTION)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
        IF (PROJCT) THEN
          CALL FINDBASIS(ATOMBAS(0,2),'GUESS')
          CALL ORBITAL_PRJ
        END IF
C
        IF ((.NOT.SCF).AND.(.NOT.GUESSONLY)) THEN
          FERMI = .FALSE.
          HUECKEL = .FALSE.
          RESTART = .TRUE.
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Matrix diagonalization options ***
C
      CASE ('MATDIA')
C
        IF (OPTIONS(1)(1:2).EQ.'RS') THEN
          MATDIA = 'RS'
        ELSE IF (OPTIONS(1)(1:3).EQ.'D&C') THEN
          IF (.NOT.MPP) MATDIA = 'DSYEVD'
        ELSE IF (OPTIONS(1)(1:5).EQ.'DSYEV') THEN
          PRODIA = 'DSYEV'
          IF (.NOT.MPP) MATDIA = 'DSYEV'
        ELSE IF (OPTIONS(1)(1:5).EQ.'JACOB') THEN
          PRODIA = 'JACOBI'
          IF ((.NOT.MPP).OR.(NATOM.EQ.1)) MATDIA = 'JACOBI'
        ELSE
          WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
          CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Matrix inversion option ***
C
      CASE ('MATINV')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'ANALY') THEN
            FITTING = 'ANALYTICAL'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NUMER') THEN
            FITTING = 'NUMERICAL'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            SVDTOL = DIGIT('MATINV','TOL=','>=0',NUMBER)
            CDFTOL = MIN(1.0E-4,MAX(10.0*SVDTOL,1.0E-6))
            CDPTOL = MIN(1.0E-4,MAX(10.0*SVDTOL,1.0E-6))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            PCGMAX = INT(DIGIT('MATINV','MAX=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'DIA') THEN
            USEDIA = .TRUE.
            USELUD = .FALSE.
            USEONE = .FALSE.
            USESVD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'LUD') THEN
            USEDIA = .FALSE.
            USELUD = .TRUE.
            USEONE = .FALSE.
            USESVD = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'ONE') THEN
            USEDIA = .FALSE.
            USELUD = .FALSE.
            USEONE = .TRUE.
            USESVD = .FALSE.
            FITTING = 'NUMERICAL'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'SVD') THEN
            USEDIA = .FALSE.
            USELUD = .FALSE.
            USEONE = .FALSE.
            USESVD = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** DIIS options ***
C
      CASE ('DIIS')
C
        IF (NOPTION.EQ.0) THEN
          DIIS = .TRUE.
        END IF
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            DIIS = .TRUE.
            NUMBER = OPTIONS(IOPTION)(5:)
            DIISTOL = DIGIT('DIIS','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'OFF') THEN
            DIIS = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'ON') THEN
            DIIS = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Charge density mixing options ***
C
      CASE ('MIXING')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:3).EQ.'OMA') THEN
            OMA = .TRUE.
          ELSE
            CDVAL = DIGIT('MIXING','VALUE','ALL',OPTIONS(IOPTION))
          END IF
        END DO
C
        IF ((CDVAL.GT.1.0).OR.(CDVAL.LT.-1.0)) THEN
          CALL ERRMSG('DECODE','MIXING VALUE MUST BE -1 < CDMIX < 1',1)
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Level-shift options ***
C
      CASE ('SHIFT')
C
      DO IOPTION=1,NOPTION
        IF (OPTIONS(IOPTION)(1:3).EQ.'OFF') THEN
          LSVAL = 0.0
          LSHIFT = .FALSE.
        ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MIN=') THEN
          NUMBER = OPTIONS(IOPTION)(5:)
          LSMIN = DIGIT('SHIFT','MIN=','> 0',NUMBER)
        ELSE
          LSHIFT = .TRUE.
          LSVAL = DIGIT('SHIFT','VALUE','ALL',OPTIONS(1))
        END IF
      END DO
C
C     ------------------------------------------------------------------
C
C     *** Smear options ***
C
      CASE ('SMEAR')
C
        SMEAR = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'UNIFO') THEN
            SMRUNI = .TRUE.
          ELSE
            SMRVAL = DIGIT('SMEAR','VALUE','>=0',OPTIONS(1))
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Multiplicity options ***
C
      CASE ('MULTIPLICITY')
C
        MULTIP = INT(DIGIT('MULTIPLICITY','VALUE','> 0',OPTIONS(1)))
C
C     ------------------------------------------------------------------
C
C     *** MO modify options ***
C
      CASE ('MOMODIFY')
C
        MOCC = .TRUE.
C
        IF (NOPTION.EQ.0) THEN
          CALL ERRMSG('DECODE','NUMBER OF MO-MODIFICATIONS MISSING',1)
        END IF
C
        NMOMODA = INT(DIGIT('MOMODIFY','ALPHA','>=0',OPTIONS(1)))
        IF (NOPTION.EQ.2) THEN
          NMOMODB = INT(DIGIT('MOMODIFY','BETA','>=0',OPTIONS(2)))
        END IF
C
C     *** Read MO modify body ***
C
        CALL READMOCC
C
C     ------------------------------------------------------------------
C
C     *** MO exchange options ***
C
      CASE ('MOEXCHANGE')
C
        MOEX = .TRUE.
C
        IF (NOPTION.EQ.0) THEN
          CALL ERRMSG('DECODE','NUMBER OF MO-EXCHANGES MISSING',1)
        END IF
C
        NMOEXCA = INT(DIGIT('MOEXCHANGE','ALPHA','>=0',OPTIONS(1)))
        IF (NOPTION.EQ.2) THEN
          NMOEXCB = INT(DIGIT('MOEXCHANGE','BETA','>=0',OPTIONS(2)))
        END IF
C
C     *** Read MO exchange body ***
C
        CALL READMOEX
C
C     ------------------------------------------------------------------
C
C     *** Options to fix MOs ***
C
      CASE ('FIXMOS')
C
        FIXCYC = 1
        FIXCFG = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'SCF=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            FIXCYC = INT(DIGIT('FIXMOS','SCF=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FIXED') THEN
            FIXLAST = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ITERA') THEN
            FIXLAST = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            FIXCYC = 100000
            FIXLAST = .TRUE.
            NUMBER = OPTIONS(IOPTION)(5:)
            FIXTOL = DIGIT('FIXMOS','TOL=','> 0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Atomic configuration options ***
C
      CASE ('CONFIGURE')
C
        CFGCYC = SCFMAX
        CFGATOM = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            CFGCYC = INT(DIGIT('CONFIGURE','MAX=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'OCCUP') THEN
            SPHORB = .TRUE.
            SETAOCC = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read configuration body ***
C
        CALL READCONF

C MLJ
        CASE('STOREWF')
           STOREWF = .TRUE.
           write(6,*) "decode: STOREWF", STOREWF
C End MLJ

C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 4 - NUMERICAL INTEGRATION KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Grid options ***
C
      CASE ('GRID')
C
        IF ((XTYP.EQ.'NOFUN').AND.(CTYP.EQ.'NOFUN')) RETURN
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'FIXED') THEN
            GGFUN = 'NONE'
            GRDTYP = 'FIXED'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ADAPT') THEN
            GRDTYP = 'ADAPTIVE'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DIREC') THEN
            DIRGRD = .TRUE.
          END IF
        END DO
C
C     *** Fixed grid options ***
C
        IF (GRDTYP.EQ.'FIXED') THEN
C
          DO IOPTION=1,NOPTION
            IF (OPTIONS(IOPTION)(1:5).EQ.'COARS') THEN
              GGFUN = 'NONE'
              G98TYP = '(50,194)p'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MEDIU') THEN
              GGFUN = 'NONE'
              G98TYP = '(75,302)p'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'REFER') THEN
              GGFUN = 'NONE'
              GRDTYP = 'FIXED'
              G98TYP = '(200,1202)'
            ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FINE') THEN
              GGFUN = 'NONE'
              G98TYP = '(99,590)p'
            ELSE IF ((OPTIONS(IOPTION)(1:5).NE.'FIXED').AND.
     $               (OPTIONS(IOPTION)(1:5).NE.'DIREC')) THEN
              WRITE (ERRSTR,5100) OPTIONS(IOPTION),'FIXED '//KEYSTR
              CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
            END IF
          END DO
C
C     *** Adaptive grid options ***
C
        ELSE IF (GRDTYP.EQ.'ADAPTIVE') THEN
C
          DO IOPTION=1,NOPTION
            IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
              NUMBER = OPTIONS(IOPTION)(5:)
              GRDTOL = DIGIT('GRID','TOL=','> 0',NUMBER)
              GRDTOL = 10.0*GRDTOL
              DO JOPTION=1,NOPTION
                IF (OPTIONS(JOPTION)(1:5).EQ.'GUESS') THEN
                  GRDTOL = GRDTOL/10.0
                END IF
              END DO
            ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'RAD=') THEN
              NUMBER = OPTIONS(IOPTION)(5:)
              NRADGRD = INT(DIGIT('GRID','RAD=','> 0',NUMBER))
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'COARS') THEN
              GRDTOL = 1.0E-3
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MEDIU') THEN
              GRDTOL = 1.0E-4
            ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FINE') THEN
              GRDTOL = 1.0E-5
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'GUESS') THEN
              GGFUN = 'GUESS'
              DEFINED = .FALSE.
              DO JOPTION=1,NOPTION
                IF (OPTIONS(JOPTION)(1:5).EQ.'COARS') THEN
                  GRDTOL = 1.0E-4
                  DEFINED = .TRUE.
                ELSE IF (OPTIONS(JOPTION)(1:5).EQ.'MEDIU') THEN
                  GRDTOL = 1.0E-5
                  DEFINED = .TRUE.
                ELSE IF (OPTIONS(JOPTION)(1:4).EQ.'FINE') THEN
                  GRDTOL = 1.0E-6
                  DEFINED = .TRUE.
                ELSE IF (OPTIONS(JOPTION)(1:4).EQ.'TOL=') THEN
                  DEFINED = .TRUE.
                END IF
              END DO
              IF (.NOT.DEFINED) GRDTOL = 1.0E-5
            ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'SCF') THEN
              GGFUN = 'SCF'
            ELSE IF ((OPTIONS(IOPTION)(1:5).NE.'ADAPT').AND.
     $               (OPTIONS(IOPTION)(1:5).NE.'DIREC')) THEN
              WRITE (ERRSTR,5100) OPTIONS(IOPTION),'ADAPTive '//KEYSTR
              CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
            END IF
          END DO
C
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Weighting options ***
C
      CASE ('WEIGHTING')
C
        IF (OPTIONS(1)(1:5).EQ.'BECKE') THEN
          WEIGHTS = 'BECKE'
        ELSE IF (OPTIONS(1)(1:5).EQ.'SCREE') THEN
          WEIGHTS = 'BECKE SCREENED'
        ELSE
          WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
          CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Quadrature options ***
C
      CASE ('QUADRATURE')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'REFER') THEN
            BWDER = .TRUE.
            REFGRD = .TRUE.
            WEIGHTS = 'BECKE'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NORAN') THEN
            RANGRD = .FALSE.
            ROTGRD = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'RANDO') THEN
            RANGRD = .TRUE.
            ROTGRD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'EULER') THEN
            RADQUA = 'EULER-MACLAURIN'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'GAUSS') THEN
            RADQUA = 'GAUSS-CHEBYSHEV'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MURA') THEN
            RADQUA = 'MURA-KNOWLES'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Semi-numerical Central Force Potential integration options ***
C
      CASE ('CFPINTEGRATION')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'COARS') THEN
            CFPTOL = 1.E-7
            NGP_CFP = 67
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MEDIU') THEN
            CFPTOL = 1.E-8
            NGP_CFP = 99
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FINE') THEN
            CFPTOL = 1.E-9
            NGP_CFP = 131
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'REFER') THEN
            CFPTOL = 1.E-10
            NGP_CFP = 195
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Exchange-correlation potential options ***
C
      CASE ('VXCTYPE')
C
        DEFINED = .FALSE.
C
        DO IOPTION=1,NOPTION
          IF (INDEX(OPTIONS(IOPTION),'-').NE.0) THEN
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            VXTYPE = OPTIONS(IOPTION)(:SYMBOL-1)
            VCTYPE = OPTIONS(IOPTION)(SYMBOL+1:)
            DEFINED = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'AUXIS') THEN
            CDXC = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BASIS') THEN
            CDXC = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'NONE') THEN
            XTYP = 'NOFUN'
            CTYP = 'NOFUN'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FOCK') THEN
            EXX = .TRUE.
            XTYP = 'NOFUN'
            CTYP = 'NOFUN'
            HYBRID = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'XALPH') THEN
            XTYP = 'XALPHA'
            CTYP = 'NOFUN'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'VWN') THEN
            XTYP = 'LSD'
            CTYP = 'PVWN'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PZ81') THEN
            XTYP = 'LSD'
            CTYP = 'PZ81'
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PW92') THEN
            XTYP = 'LSD'
            CTYP = 'PW92'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PW86') THEN
            XTYP = 'PW86'
            LCTYP = 'PVWN'
            CTYP = 'P86'
            GGA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'BLYP') THEN
            XTYP = 'B88'
            CTYP = 'LYP'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'B3LYP') THEN
            XTYP = 'B3LYP'
            CTYP = 'B3LYP'
            EXX = .TRUE.
            GGA = .TRUE.
            HYBRID = .TRUE.
            PB88 = 0.72
            PEXX = 0.20
            PLYP = 0.81
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'BH&H') THEN
            XTYP = 'B3LYP'
            CTYP = 'B3LYP'
            EXX = .TRUE.
            GGA = .TRUE.
            HYBRID = .TRUE.
            PB88 = 0.5
            PEXX = 0.5
            PLYP = 1.0
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'OLYP') THEN
            XTYP = 'OPTX'
            CTYP = 'LYP'
            GGA  = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PW91S') THEN
            XTYP = 'PW91'
            CTYP = 'PW91SSF'
            GGA = .TRUE.
            SSFG98 = .FALSE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PW91') THEN
            XTYP = 'PW91'
            CTYP = 'PW91'
            GGA = .TRUE.
            SSFG98 = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PBESO') THEN
            XTYP = 'PBESOL'
            CTYP = 'PBESOL'
            GGA = .TRUE.
            SSFG98 = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PBESS') THEN
            XTYP = 'PBE96'
            CTYP = 'PBESSF'
            GGA = .TRUE.
            SSFG98 = .FALSE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PBE0') THEN
            XTYP = 'PBE0'
            CTYP = 'PBE'
            EXX = .TRUE.
            GGA = .TRUE.
            HYBRID = .TRUE.
            SSFG98 = .TRUE.
            PEXX = 0.25
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'PBE') THEN
            XTYP = 'PBE96'
            CTYP = 'PBE'
            GGA = .TRUE.
            SSFG98 = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'KT1') THEN
            XTYP = 'KT1X'
            CTYP = 'RVWN'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'KT2') THEN
            XTYP = 'KT2X'
            CTYP = 'KT2C'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'KT3') THEN
            XTYP = 'KT3X'
            CTYP = 'KT3C'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'SO11') THEN
            XTYP = 'SO11'
            CTYP = 'SO11'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'N12') THEN
            XTYP = 'N12'
            CTYP = 'N12'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'CAP') THEN
            XTYP = 'CAP'
            CTYP = 'PBE'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'GAM') THEN
            XTYP = 'GAM'
            CTYP = 'GAM'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'VS98') THEN
            XTYP = 'VS98'
            CTYP = 'VS98'
            LAP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PKZB') THEN
            XTYP = 'PKZB'
            CTYP = 'PKZB'
            LAP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TPSS') THEN
            XTYP = 'TPSS'
            CTYP = 'TPSS'
            LAP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'M062X') THEN
            XTYP = 'M062X'
            CTYP = 'M062X'
            EXX = .TRUE.
            LAP = .TRUE.
            HYBRID = .TRUE.
            PEXX = 0.54
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'M06HF') THEN
            XTYP = 'M06HF'
            CTYP = 'M06HF'
            EXX = .TRUE.
            LAP = .TRUE.
            HYBRID = .TRUE.
            PEXX = 1.0
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'M06L') THEN
            XTYP = 'M06L'
            CTYP = 'M06L'
            LAP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'M06') THEN
            XTYP = 'M06'
            CTYP = 'M06'
            EXX = .TRUE.
            LAP = .TRUE.
            HYBRID = .TRUE.
            PEXX = 0.27
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'M11L') THEN
            XTYP = 'M11'
            CTYP = 'M11'
            LAP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MN12') THEN
            XTYP = 'MN12'
            CTYP = 'MN12'
            LAP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'X=') THEN
            XTYP = 'XALPHA'
            CTYP = 'NOFUN'
            NUMBER = OPTIONS(IOPTION)(3:)
            XALPHA = DIGIT('VXCTYPE','XALPHA','> 0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** User defined exchange and correlation potential ***
C
        IF (DEFINED) THEN
C
C     *** Exchange potentials ***
C
          IF (VXTYPE(1:4).EQ.'NONE') THEN
            XTYP = 'NOFUN'
          ELSE IF (VXTYPE(1:5).EQ.'DIRAC') THEN
            XTYP = 'LSD'
          ELSE IF (VXTYPE(1:4).EQ.'PW86') THEN
            XTYP = 'PW86'
            GGA = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'B88') THEN
            XTYP = 'B88'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'PW91') THEN
            XTYP = 'PW91'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'ECMV') THEN
            XTYP = 'ECMV'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'EV93') THEN
            XTYP = 'EV93'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'LB94') THEN
            XTYP = 'LB94'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:5).EQ.'PBE96') THEN
            XTYP = 'PBE96'
            GGA = .TRUE.
          ELSE IF (VXTYPE(1:5).EQ.'PBE98') THEN
            XTYP = 'PBE98'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:5).EQ.'PBE99') THEN
            XTYP = 'PBE99'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:5).EQ.'PBESO') THEN
            XTYP = 'PBESOL'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PBE0') THEN
            XTYP = 'PBE0'
            EXX = .TRUE.
            GGA = .TRUE.
            HYBRID = .TRUE.
            SSFG98 = .TRUE.
            PEXX = 0.25
          ELSE IF (VXTYPE(1:3).EQ.'PBE') THEN
            XTYP = 'PBE96'
            GGA = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'KT1') THEN
            XTYP = 'KT1X'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'KT2') THEN
            XTYP = 'KT2X'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'KT3') THEN
            XTYP = 'KT3X'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'OPTX') THEN
            XTYP = 'OPTX'
            GGA  = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:5).EQ.'VMTSO') THEN
            XTYP = 'VMTSOL'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'VMT') THEN
            XTYP = 'VMT09'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'SO11') THEN
            XTYP = 'SO11'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'N12') THEN
            XTYP = 'N12'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'CAP') THEN
            XTYP = 'CAP'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:3).EQ.'GAM') THEN
            XTYP = 'GAM'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'VS98') THEN
            XTYP = 'VS98'
            LAP = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'PKZB') THEN
            XTYP = 'PKZB'
            LAP = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'TPSS') THEN
            XTYP = 'TPSS'
            LAP = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'M06L') THEN
            XTYP = 'M06L'
            LAP = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'M11L') THEN
            XTYP = 'M11L'
            LAP = .TRUE.
          ELSE IF (VXTYPE(1:4).EQ.'MN12') THEN
            XTYP = 'MN12'
            LAP = .TRUE.
          ELSE
            WRITE (ERRSTR,'("UNKNOWN EXCHANGE POTENTIAL ",A)') VXTYPE
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
C
C     *** Correlation potentials ***
C
          IF (VCTYPE(1:4).EQ.'NONE') THEN
            CTYP = 'NOFUN'
          ELSE IF (VCTYPE(1:3).EQ.'VWN') THEN
            CTYP = 'PVWN'
          ELSE IF (VCTYPE(1:4).EQ.'PVWN') THEN
            CTYP = 'PVWN'
          ELSE IF (VCTYPE(1:4).EQ.'RVWN') THEN
            CTYP = 'RVWN'
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'PZ81') THEN
            CTYP = 'PZ81'
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'PW92') THEN
            CTYP = 'PW92'
          ELSE IF (VCTYPE(1:3).EQ.'KT2') THEN
            CTYP = 'KT2C'
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:3).EQ.'P86') THEN
            LCTYP = 'PVWN'
            CTYP = 'P86'
            GGA = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'PZ86') THEN
            LCTYP = 'PZ81'
            CTYP = 'P86'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:3).EQ.'LYP') THEN
            CTYP = 'LYP'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:5).EQ.'PW91S') THEN
            CTYP = 'PW91SSF'
            GGA = .TRUE.
            SSFG98 = .FALSE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'PW91') THEN
            CTYP = 'PW91'
            GGA = .TRUE.
            SSFG98 = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:5).EQ.'PBESS') THEN
            CTYP = 'PBESSF'
            GGA = .TRUE.
            SSFG98 = .FALSE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:5).EQ.'PBESO') THEN
            CTYP = 'PBESOL'
            GGA = .TRUE.
            SSFG98 = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:3).EQ.'PBE') THEN
            CTYP = 'PBE'
            GGA = .TRUE.
            SSFG98 = .TRUE.
          ELSE IF (VCTYPE(1:3).EQ.'KT3') THEN
            CTYP = 'KT3C'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'SO11') THEN
            CTYP = 'SO11'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:3).EQ.'N12') THEN
            CTYP = 'N12'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:3).EQ.'GAM') THEN
            CTYP = 'GAM'
            GGA = .TRUE.
            FDKERNEL = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'VS98') THEN
            CTYP = 'VS98'
            LAP = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'PKZB') THEN
            CTYP = 'PKZB'
            LAP = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'TPSS') THEN
            CTYP = 'TPSS'
            LAP = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'M06L') THEN
            CTYP = 'TPSS'
            LAP = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'M11L') THEN
            CTYP = 'M11L'
            LAP = .TRUE.
          ELSE IF (VCTYPE(1:4).EQ.'MN12') THEN
            CTYP = 'MN12'
            LAP = .TRUE.
          ELSE
            WRITE (ERRSTR,'("UNKNOWN CORRELATION POTENTIAL ",A)') VCTYPE
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
C
        END IF
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 5 - QM/MM KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Empirical van der Waals energy options ***
C
      CASE ('DISPERSION')
C
        VDW = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:3).EQ.'OFF') THEN
            VDW = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'S6=') THEN
            NUMBER = OPTIONS(IOPTION)(4:)
            SSIX = DIGIT('DISPERSION','S6=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'READ') THEN
            C6READ = 'FROM INPUT'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read and assign C6 coefficients ***
C
        CALL READCSIX
C
C     ------------------------------------------------------------------
C
C     *** QM/MM interface options ***
C
      CASE ('QM/MM')
C
        DEFINED = .FALSE.
        SYMMETRY = .FALSE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'CHARM') THEN
            PORT = .TRUE.
            CHARMM = .TRUE.
            DEFINED = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NOPOL') THEN
            MONOPOLE = .FALSE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read internal QM/MM input ***
C
        IF (.NOT.DEFINED) THEN
          CALL READQMMM
          CALL READQMPARAM
          CALL TYPINGQM
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Force field options ***
C
      CASE ('FORCEFIELD')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:3).EQ.'FF=') THEN
            FFIELD = OPTIONS(IOPTION)(4:)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read force field parameters ***
C
        CALL FFIELDINI
        CALL CONNECMM
        CALL READPARAM
        CALL TYPING
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 6 - OPTIMIZATION KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Intrinsic reaction coordinate options ***
C
      CASE ('IRC')
C
        IRC = .TRUE.
        OPTTYP = 'CARTESIAN'
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'EVIB=') THEN
            NUMBER = OPTIONS(IOPTION)(6:)
            DEVIB = DIGIT('IRC','EVIB=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'READ=') THEN
            IRCTYP = 'READ STEP'
            TRJTYP = 'READ STEP'
            NUMBER = OPTIONS(IOPTION)(6:)
            IRCSTEP = INT(DIGIT('IRC','READ=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'STEP=') THEN
            NUMBER = OPTIONS(IOPTION)(6:)
            MAXIRC = DIGIT('IRC','STEP=','> 0',NUMBER)
            IRCDR = MAXIRC
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            IRCMAX = INT(DIGIT('IRC','MAX=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            IRCTOL = DIGIT('IRC','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'EXTEN') THEN
            IRCTYP = 'EXTEND'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NOMAS') THEN
            USEMW = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'RESTA') THEN
            IRCTYP = 'RESTART'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MASS') THEN
            USEMW = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'FOR') THEN
            IRCSIGN = 1.0
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'REV') THEN
            IRCSIGN = -1.0
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Set IRC related keywords ***
C
        CALL SETIRC
C
C     ------------------------------------------------------------------
C
C     *** Saddle interpolation options ***
C
      CASE ('SADDLE')
C
        SADDLE = .TRUE.
        OPTTYP = 'CARTESIAN'
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'INT=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            SADINT = DIGIT('SADDLE','INT=','> 0',NUMBER)/100.0
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            SADMAX = INT(DIGIT('SADDLE','MAX=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            SADTOL = DIGIT('SADDLE','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'TS') THEN
            SADTS = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read end points of saddle interpolation ***
C
        IF (STATUS.EQ.'READ IN') CALL READENDS
C
C     ------------------------------------------------------------------
C
C     *** Optimization options ***
C
      CASE ('OPTIMIZATION')
C
        OPT = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:2).EQ.'TS') THEN
            TS = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            OPTMAX = INT(DIGIT('OPTIMIZATION','MAX=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MOD=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            HMODE = INT(DIGIT('OPTIMIZATION','MOD=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            OPTTOL = DIGIT('OPTIMIZATION','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'CARTE') THEN
            OPTTYP = 'CARTESIAN'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'INTER') THEN
            OPTTYP = 'INTERNAL'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'REDUN') THEN
            OPTTYP = 'REDUNDANT'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'STEP=') THEN
            NUMBER = OPTIONS(IOPTION)(6:)
            MAXDR = DIGIT('OPTIMIZATION','STEP=','> 0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Hessian options ***
C
      CASE ('HESSIAN')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'UNITY') THEN
            HESSIAN = 'UNITMATRIX'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'INTER') THEN
            HESSIAN = 'INTERNAL'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BAKER') THEN
            HESSIAN = 'BAKER'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FISCH') THEN
            HESSIAN = 'FISCHER'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'LINDH') THEN
            HESSIAN = 'LINDH'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'CALCU') THEN
            HESSIAN = 'CALCULATED'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DEMON') THEN
            HREAD = 'DEMON-FORMAT'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PLAIN') THEN
            HREAD = 'PLAIN-FORMAT'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'READ') THEN
            HESSIAN = 'READ'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'VIB=') THEN
            NUMBER = OPTIONS(IOPTION)(5:) 
            VIBSTEP = DIGIT('FREQUENCY','VIB=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'RESTA') THEN
            HESSIAN = 'RESTART'
            FERMI = .FALSE.
            HUECKEL = .FALSE.
            RESTART = .TRUE.
            IF (IRC) THEN
              FREQRST = .TRUE.
              NEWINP = .FALSE.
            ELSE
              IF ((OPTTYP.NE.'CARTESIAN').AND.
     $            (INPTYP.EQ.'CARTESIAN')) THEN
                NEWINP = .TRUE.
              END IF
            END IF
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Update options ***
C
      CASE ('UPDATE')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'BERNY') THEN
            UPHESS = 'BERNY'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'EXACT') THEN
            UPHESS = 'EXACT'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'POWEL') THEN
            UPHESS = 'POWELL'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'BFGS') THEN
            UPHESS = 'BFGS'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'DFP') THEN
            UPHESS = 'DFP'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'MSP') THEN
            UPHESS = 'MSP'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'SR1') THEN
            UPHESS = 'SR1'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'VIB=') THEN
            NUMBER = OPTIONS(IOPTION)(5:) 
            VIBSTEP = DIGIT('FREQUENCY','VIB=','> 0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Step type options ***
C
      CASE ('STEPTYPE')
C
        IF ((OPTIONS(1)(1:5).EQ.'P-RFO').OR.
     $      (OPTIONS(1)(1:4).EQ.'PRFO').OR.
     $      (OPTIONS(1)(1:3).EQ.'RFO')) THEN
          STEPTYP = 'P-RFO'
        ELSE IF (OPTIONS(1)(1:4).EQ.'WALK') THEN
          STEPTYP = 'PES-WALK'
        ELSE IF (OPTIONS(1)(1:5).EQ.'DESCE') THEN
          STEPTYP = 'DESCENT'
        ELSE IF (OPTIONS(1)(1:5).EQ.'LEVEN') THEN
          STEPTYP = 'LEVENBERG'
        ELSE
          WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
          CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
        END IF
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 7 - MOLECULAR DYNAMICS KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Molecular dynamics options ***
C
      CASE ('DYNAMICS')
C
        MD = .TRUE.
        MDRUN = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'STEP=') THEN
            NUMBER = OPTIONS(IOPTION)(6:)
            MDSTEP = DIGIT('DYNAMICS','STEP=','>=0',NUMBER)
            MDSTEP = MDSTEP*FSEC
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            MDMAX = INT(DIGIT('DYNAMICS','MAX=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'INT=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            MDINT = INT(DIGIT('DYNAMICS','INT=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'R=') THEN
            SYMGEO = .TRUE.
            NUMBER = OPTIONS(IOPTION)(3:)
            MDRADIUS = DIGIT('DYNAMICS','R=','>=0',NUMBER)
            MDRADIUS = MDRADIUS/ANGBOR(1)
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'STOR') THEN
            STOREWF = .TRUE.
            REWWF   = .FALSE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Trajectory options ***
C
      CASE ('TRAJECTORY')
C
        MD = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'FORCE') THEN
            TRJACC = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'RESTA') THEN
            TRJTYP = 'RESTART'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PART=') THEN
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(6:SYMBOL-1)
            TRJMIN = INT(DIGIT('TRAJECTORY','PART=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            TRJMAX = INT(DIGIT('TRAJECTORY','PART=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PLOT=') THEN
            SCF = .FALSE.
            TRJTYP = 'PLOT'
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(6:SYMBOL-1)
            TRJMIN = INT(DIGIT('TRAJECTORY','PLOT=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            TRJMAX = INT(DIGIT('TRAJECTORY','PLOT=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'READ=') THEN
            TRJTYP = 'READ STEP'
            NUMBER = OPTIONS(IOPTION)(6:)
            TRJMIN = INT(DIGIT('TRAJECTORY','READ=','>=0',NUMBER))
            TRJMAX = INT(DIGIT('TRAJECTORY','READ=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CUT>') THEN
            SCF = .FALSE.
            TRJTYP = 'CUT>'
            NUMBER = OPTIONS(IOPTION)(5:)
            TRJMIN = 1
            TRJMAX = INT(DIGIT('TRAJECTORY','CUT>','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CUT<') THEN
            SCF = .FALSE.
            TRJTYP = 'CUT<'
            NUMBER = OPTIONS(IOPTION)(5:)
            TRJMIN = INT(DIGIT('TRAJECTORY','CUT<','>=0',NUMBER))
            TRJMAX = 0
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'INT=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            TRJINT = INT(DIGIT('TRAJECTORY','INT=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MOS=') THEN
            TRJMOS = .TRUE.
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(5:SYMBOL-1)
            LLTMOS = INT(DIGIT('TRAJECTORY','MOS=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            ULTMOS = INT(DIGIT('TRAJECTORY','MOS=','> 0',NUMBER))
            IF (MOD(ULTMOS-LLTMOS+1,3).GT.0) THEN
              ULTMOS = ULTMOS + 3 - MOD(ULTMOS-LLTMOS+1,3)
            END IF
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'PLOT') THEN
            SCF = .FALSE.
            TRJTYP = 'PLOT'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Simulation options ***
C
      CASE ('SIMULATION')
C
        MD = .TRUE.
C
        DO IOPTION=1,NOPTION
C
C     *** Global simulation options ***
C
          IF (OPTIONS(IOPTION)(1:5).EQ.'ANALY') THEN
            MDMAX = 0
            SCF = .FALSE.
            OPT = .FALSE.
            ESA = .FALSE.
            FREQ = .FALSE.
            PROP = .FALSE.
            TDDFT = .FALSE.
            TRJTYP = 'ANALYSIS'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'CALCU') THEN
            MDMAX = 0
            SCF = .TRUE.
            OPT = .FALSE.
            ESA = .FALSE.
            FREQ = .FALSE.
            PROP = .FALSE.
            TDDFT = .FALSE.
            TRJTYP = 'PROPERTY'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'INT=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            SIMINT = INT(DIGIT('SIMULATION','INT=','>0',NUMBER))
C
C     *** Calculation and analysis options ***
C
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DIPOL') THEN
            PEM = .TRUE.
            ETRJ = 'UNKNOWN'
            PTRJ = 'DIPOLE'
            TRJDIP = .TRUE.
            RANGRD = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'POLAR') THEN
            POLARIS = .TRUE.
            ETRJ = 'UNKNOWN'
            PTRJ = 'POLARIS'
            TRJPOL = .TRUE.
            RANGRD = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'SIMIL') THEN
            ALIGNMENT = .TRUE.
            ETRJ = 'UNKNOWN'
            PTRJ = 'SIMILARITY'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'NMR=') THEN
            NMR = .TRUE.
            ETRJ = 'UNKNOWN'
            PTRJ = 'NMR'
            TRJNMR = .TRUE.
            IF (NSRC) TRJNSR = .TRUE.
            NUMBER = OPTIONS(IOPTION)(5:)
            TRJATOM = INT(DIGIT('SIMULATION','NMR=','>0',NUMBER))
            IF (.NOT.NMRSIG(0)) NMRSIG(:) = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MAGNE') THEN
            MXI = .TRUE.
            ETRJ = 'UNKNOWN'
            PTRJ = 'MAGNET'
            TRJMAG = .TRUE.
            IF (RGTEN) TRJROG = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'XES') THEN
            PTRJ   = 'XES'
            XSPIN  = 'ALFA'
            SCF    = .TRUE.
            STOREWF= .TRUE.
c set up the xes options
            XES=.TRUE.
            LLXES=1
            ULXES=1
            MODALP=.true.
c set up the xes options

            IF (TRJTYP.EQ.'PROPERTY') THEN
               REWWF = .TRUE.
            ELSE IF (TRJTYP.EQ.'ANALYSIS') THEN
               REWWF = .FALSE.
               SCFMAX = 0
            ENDIF
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'BETA') THEN
             XSPIN = 'BETA'
            MODBET=.true.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'ALFA') THEN
             XSPIN = 'ALFA'
            MODALP=.true.
C
C     *** Analysis only options ***
C
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ANGLE') THEN
            CALL READSET
            ETRJ = 'UNKNOWN'
            PTRJ = 'ANGLE'
            TRJGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DIHED') THEN
            CALL READSET
            ETRJ = 'UNKNOWN'
            PTRJ = 'DIHEDRAL'
            TRJGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'LENGT') THEN
            CALL READSET
            ETRJ = 'UNKNOWN'
            PTRJ = 'LENGTH'
            TRJGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'LINDE') THEN
            NPAT = 1
            ETRJ = 'STANDARD'
            PTRJ = 'LINDEMANN'
            TRJGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MEAND') THEN
            NPAT = 1
            ETRJ = 'UNKNOWN'
            PTRJ = 'MEANDIS'
            TRJGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'RDF=') THEN
            ETRJ = 'STANDARD'
            PTRJ = 'RADDISTR'
            TRJRDF = .TRUE.
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            RDFTYP(1) = OPTIONS(IOPTION)(5:SYMBOL-1)
            RDFTYP(2) = OPTIONS(IOPTION)(SYMBOL+1:)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MOMEN') THEN
            ETRJ = 'UNKNOWN'
            PTRJ = 'LPMOMENTA'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PHASE') THEN
            ETRJ = 'UNKNOWN'
            PTRJ = 'PHASESPACE'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PROLA') THEN
            NPAT = 1
            ETRJ = 'UNKNOWN'
            PTRJ = 'PROLATE'
            TRJGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=KIN') THEN
            ETRJ = 'KINETIC'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=POT') THEN
            ETRJ = 'POTENTIAL'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=STA') THEN
            ETRJ = 'STANDARD'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=SYS') THEN
            ETRJ = 'SYSTEM'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=TOT') THEN
            ETRJ = 'TOTAL'
C
C     *** Error handling ***
C
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
C
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Heat bath options ***
C
      CASE ('BATH')
C
        BATH = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'SCALI') THEN
            MDTAU = 1.0
            MDBATH = 'SCALING'
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:4).EQ.'INT=') THEN
                NUMBER = OPTIONS(JOPTION)(5:)
                MDTAU = DIGIT('BATH','INT=','>=0',NUMBER)
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BEREN') THEN
            MDTAU = 500*FSEC
            MDBATH = 'BERENDSEN'
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:4).EQ.'TAU=') THEN
                NUMBER = OPTIONS(JOPTION)(5:)
                MDTAU = DIGIT('BATH','TAU=','>=0',NUMBER)
                MDTAU = MDTAU*1000.0*FSEC
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'HOOVE') THEN
            MDTAU = 1.0
            MDBATH = 'HOOVER'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'NOSE') THEN
            NNHC = 3
            TRJNHC = .TRUE.
            MDBATH = 'NOSE'
            MDTAU = 1500.0/WAVENUM
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:5).EQ.'FREQ=') THEN
                NUMBER = OPTIONS(JOPTION)(6:)
                MDTAU = DIGIT('BATH','FREQ=','>=0',NUMBER)
                MDTAU = MDTAU/WAVENUM
              ELSE IF (OPTIONS(JOPTION)(1:4).EQ.'NHC=') THEN
                NUMBER = OPTIONS(JOPTION)(5:)
                NNHC = INT(DIGIT('BATH','NHC=','>=0',NUMBER))
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'NONE') THEN
            BATH = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'T=') THEN
            NUMBER = OPTIONS(IOPTION)(3:)
            MDTEMP = DIGIT('BATH','T=','>=0',NUMBER)
          ELSE IF (INDEX(OPTIONS(IOPTION),'=').EQ.0) THEN
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
        CALL MAXCHK('BATH')
C
C     ------------------------------------------------------------------
C
C     *** Nuclear velocity options ***
C
      CASE ('VELOCITIES')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'RANDO') THEN
            MDVELO = 'RANDOM'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'READ') THEN
            MDVELO = 'READ'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'ZERO') THEN
            MDVELO = 'ZERO'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'LP=0') THEN
            MDZERO = 'LP'
            SYMGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'P=0') THEN
            MDZERO = 'P'
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'T=') THEN
            NUMBER = OPTIONS(IOPTION)(3:)
            MDTEMP0 = DIGIT('VELOCITIES','T=','>=0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
        IF (TRJTYP.NE.'RESTART') CALL READVELO(MDVELO)
C
C     ------------------------------------------------------------------
C
C     *** LP conservation options ***
C
      CASE ('LPCONSERVE')
C
        IF (NOPTION.EQ.0) THEN
          MDFLP = .TRUE.
          SYMGEO = .TRUE.
        END IF
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'FORCE') THEN
            MDFLP = .TRUE.
            SYMGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'VELO') THEN
            MDVLP = .TRUE.
            SYMGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            MDFLP = .TRUE.
            MDVLP = .TRUE.
            SYMGEO = .TRUE.
            NUMBER = OPTIONS(IOPTION)(5:)
            LPTOL = DIGIT('LPCONSERVE','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'OFF') THEN
            MDFLP = .FALSE.
            MDVLP = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'ON') THEN
            MDFLP = .TRUE.
            MDVLP = .TRUE.
            SYMGEO = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Radial distribution function options ***
C
      CASE ('RDF')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            RDFMAX = DIGIT('RDF','MAX=','>=0',NUMBER)
            RDFMAX = RDFMAX/ANGBOR(1)
          ELSE IF (OPTIONS(IOPTION)(1:6).EQ.'WIDTH=') THEN
            NUMBER = OPTIONS(IOPTION)(7:)
            RDFWIDTH = DIGIT('RDF','WIDTH=','> 0',NUMBER)
            RDFWIDTH = RDFWIDTH/ANGBOR(1)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 8 - PERTURBATION THEORY KEYWORDS *** 
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Frequency analysis options ***
C
      CASE ('FREQUENCY')
C
        FREQ = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'VIB=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            VIBSTEP = DIGIT('FREQUENCY','VIB=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FIXED') THEN
            FREQFIX = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'RAMAN') THEN
            RAMAN = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'RESTA') THEN
            MD = .FALSE.
            OPT = .FALSE.
            OPTMAX = 0
            FREQRST = .TRUE.
            RESTART = .TRUE.
            HUECKEL = .FALSE.
            HESSIAN = 'ANALYSIS'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read frequency body ***
C
        CALL READFIX
C
C     ------------------------------------------------------------------
C
C     *** Thermodynamic data options ***
C
      CASE ('THERMO')
C
        IF (FREQ) THERMOS = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'VIBON') THEN
            VIBONLY = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MIN=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            TLOWER = DIGIT('THERMO','MIN=','>=0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            TUPPER = DIGIT('THERMO','MAX=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'INT=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            TDELTA = DIGIT('THERMO','INT=','> 0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Polarizability options ***
C
      CASE ('POLARIZABILITY')
C
        POLARIS = .TRUE.
        RANGRD = .FALSE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'ALPHA') THEN
            POLORD = 1
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'BETA') THEN
            POLORD = 2
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'GAMMA') THEN
            POLORD = 3
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ANALY') THEN
            POLTYP = 'ANALYTIC'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NIACP') THEN
            POLTYP = 'NIACPKS'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NUMER') THEN
            POLKEY = 'DD'
            POLTYP = 'NUMERIC'
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:4).EQ.'BETA') THEN
                EFISH = .TRUE.
                PEM = .TRUE.
              ELSE IF (OPTIONS(JOPTION)(1:5).EQ.'GAMMA') THEN
                EFISH = .TRUE.
                PEM = .TRUE.
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'EFISH') THEN
            EFISH = .TRUE.
            PEM = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FDKER') THEN
            FDKERNEL = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FALDA') THEN
            FALDA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'GALDA') THEN
            GALDA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FFS=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            FFSTEP = DIGIT('POLARIZABILITY','FFS=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            GXCTHRESH = DIGIT('POLARIZABILITY','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'W1=') THEN
            POLTYP = 'ANALYTIC'
            NUMBER = OPTIONS(IOPTION)(4:)
            CALL READOMEGA(NUMBER,OPTIONS,NOPTION,'USER')
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'W=') THEN
            POLTYP = 'ANALYTIC'
            NUMBER = OPTIONS(IOPTION)(3:)
            CALL READOMEGA(NUMBER,OPTIONS,NOPTION,'AUTO')
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'DD') THEN
            POLKEY = 'DD'
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'DQ') THEN
            POLKEY = 'DQ'
          ELSE IF (NOTIN('POLARIZABILITY',OPTIONS(IOPTION))) THEN
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Excitation options ***
C
      CASE ('EXCITATION')
C
        TDDFT = .TRUE.
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:2).EQ.'RS') THEN
            TDDIA = 'RS'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'D&C') THEN
            IF (.NOT.MPP) TDDIA = 'DSYEVD'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'TDA') THEN
            TDA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DAVID') THEN
            TDDIA = 'DAVIDSON'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DSYEV') THEN
            IF (.NOT.MPP) TDDIA = 'DSYEV'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'JACOB') THEN
            IF (.NOT.MPP) TDDIA = 'JACOBI'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
      CASE('NONCOLLINEAR')
C
        OKS = .TRUE.
        TDA = .TRUE.
        NONCOL = .TRUE.
C
C     ------------------------------------------------------------------
C
C     *** Davidson options ***
C
      CASE ('DAVIDSON')
C
        TDDIA = 'DAVIDSON'
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'BAS=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            KRYBAS = INT(DIGIT('DAVIDSON','BAS=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'EIG=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            NEIGEN = INT(DIGIT('DAVIDSON','NEIGEN','> 0',NUMBER))
            IF ((.NOT.OKS).AND.(MOD(NEIGEN,2).NE.0)) THEN
              NEIGEN = NEIGEN + 1
            END IF
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            KRYMAX = INT(DIGIT('DAVIDSON','MAX=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            KRYTOL = DIGIT('DAVIDSON','TOL=','> 0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Nuclear magnetic resonance options ***
C
      CASE ('NMR')
C
        NMR = .TRUE.
        NICS = .FALSE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'SPINR') THEN
            NSRC = .TRUE.
            SYMGEO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'READ') THEN
            NMRSIG(0) = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'NICS') THEN
            NICS = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read nuclear magnetic resonance body ***
C
        CALL READNMR
C
C     ------------------------------------------------------------------
C
C     *** Nuclear quadrupole resonance options ***
C
      CASE ('NQR')
C
        NQR = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'READ') THEN
            NQRSIG(0) = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'BARN') THEN
            BARNS = BOHR**2
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FM^2') THEN
            BARNS = BOHR**2/100.0
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read nuclear quadrupole resonance body ***
C
        CALL READNQR
C
C     ------------------------------------------------------------------
C
C     *** Molecular magnetic susceptibility options ***
C
      CASE ('MAGNETIZABILITY')
C
        MXI = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'GTENS') THEN
            RGTEN = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'G-TEN') THEN
            RGTEN = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** X-ray spectroscopy options ***
C
      CASE ('XRAY')
C
        LLXES = 1
        ULXES = 1
        MODALP  = .TRUE.
C
        XAS = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:3).EQ.'XAS') THEN
            XAS = .TRUE.
            XES = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'XES') THEN
            XAS = .FALSE.
            XES = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            XASTOL = DIGIT('XRAY','TOL=','> 0',NUMBER)
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BETA=') THEN
            MODBET  = .TRUE.
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(6:SYMBOL-1)
            LLXES = INT(DIGIT('XRAY','BETA=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            ULXES = INT(DIGIT('XRAY','BETA=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:6).EQ.'ALPHA=') THEN
            MODALP  = .TRUE.
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(7:SYMBOL-1)
            LLXES = INT(DIGIT('XRAY','ALPHA=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            ULXES = INT(DIGIT('XRAY','ALPHA=','> 0',NUMBER))
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Chemical reactivity analysis (Fukui) options ***
C
      CASE ('FUKUI')
C
        FUKUI = .TRUE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 9 - ELECTRONIC STRUCTURE ANALYSIS KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Electrostatic moment options ***
C
      CASE ('DIPOLE')
C
        ESA = .TRUE.
        PEM = .TRUE.
        RANGRD = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Population analysis options ***
C
      CASE ('POPULATION')
C
        ESA = .TRUE.
        MPA = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'MULLI') THEN
            MPA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'LOEWD') THEN
            LPA = .TRUE.
            MPA = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BECKE') THEN
            FPA = .TRUE.
            MPA = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'HIRSH') THEN
            HPA = .TRUE.
            MPA = .FALSE.
            SPHPA = .TRUE.
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:5).EQ.'FRACT') THEN
                ITHFO = .TRUE.
                ITHPA = .TRUE.
              ELSE IF (OPTIONS(JOPTION)(1:5).EQ.'ITERA') THEN
                ITHPA = .TRUE.
              ELSE IF (OPTIONS(JOPTION)(1:3).EQ.'SCF') THEN
                SPHPA = .FALSE.
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'VORON') THEN
            MPA = .FALSE.
            VPA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BADER') THEN
            BPA = .TRUE.
            MPA = .FALSE.
            G98TOP = '(75,302)p'
            REFTOP = .FALSE.
            PLOTDER = 0
            PLOTFNC = 'RHO'
            PLOT = 'BINARY'
            PLOTMOA = 'DEFAULT'
            PLOTMOB = 'DEFAULT'
            PLOTTYP = 'CPSEARCH'
            LATTYP = 'POINTS'
            LATINP = 'POLYGON'
            CALL POLYPOINT
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DEFOR') THEN
            DEFORM = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FULL') THEN
            FULLPA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'NBO') THEN
            NPA = .TRUE.
            MPA = .FALSE.
          ELSE IF (NOTIN('POPULATION',OPTIONS(IOPTION))) THEN
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Topological analysis option ***
C
      CASE ('TOPOLOGY')
C
        IF (.NOT.(BPA.OR.FPA.OR.HPA.OR.VPA.OR.USEDD)) THEN
          BPA = .TRUE.
          ESA = .TRUE.
        END IF
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'AUXIS') THEN
            TOPCD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BASIS') THEN
            TOPCD = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'COARS') THEN
            G98TOP = '(50,194)p'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MEDIU') THEN
            G98TOP = '(75,302)p'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'REFER') THEN
            REFTOP = .TRUE.
            G98TOP = '(200,1202)'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DIREC') THEN
            DIRTOP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FINE') THEN
            G98TOP = '(99,590)p'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            TOPMAX = INT(DIGIT('TOPOLOGY','MAX=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            TOPTOL = DIGIT('TOPOLOGY','TOL=','>0',NUMBER)
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Density of state (DOS) options ***
C
      CASE ('DOS')
C
        DOS = .TRUE.
        ESA = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'MOS=') THEN
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(5:SYMBOL-1)
            LLDOS = INT(DIGIT('DOS','MOS=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            ULDOS = INT(DIGIT('DOS','MOS=','> 0',NUMBER))
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Sigma-pi energy separation options ***
C
      CASE ('SIGPI')
C
        ESA = .TRUE.
        SIGPI = .TRUE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 10 - MAGOUT KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Magnetic output options ***
C
      CASE ('MAGOUT')
C
        PROP = .TRUE.
        MAGOUT = .TRUE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 11 - PLOT KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Visualization options ***
C
      CASE ('VISUALIZATION')
C
        PORT = .TRUE.
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:3).EQ.'OFF') THEN
            MOLDEN = 'NONE'
            MOLEKEL = 'NONE'
            WFNFILE = 'NONE'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MOLDE') THEN
            MOLDEN = 'FULL'
            MOLEKEL = 'NONE'
            WFNFILE = 'NONE'
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:3).EQ.'XYZ') THEN
                MOLDEN = 'XYZ'
              ELSE IF (OPTIONS(JOPTION)(1:3).EQ.'OPT') THEN
                MOLDEN = 'OPT'
              ELSE IF (OPTIONS(JOPTION)(1:2).EQ.'MD') THEN
                MOLDEN = 'MD'
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MOLEK') THEN
            MOLDEN = 'NONE'
            MOLEKEL = 'FULL'
            WFNFILE = 'NONE'
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:3).EQ.'XYZ') THEN
                MOLDEN = 'XYZ'
                MOLEKEL = 'NONE'
              ELSE IF (OPTIONS(JOPTION)(1:3).EQ.'OPT') THEN
                MOLDEN = 'OPT'
                MOLEKEL = 'NONE'
              ELSE IF (OPTIONS(JOPTION)(1:2).EQ.'MD') THEN
                MOLDEN = 'MD'
                MOLEKEL = 'NONE'
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=KIN') THEN
            ETRJMOL = 'KINETIC'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=POT') THEN
            ETRJMOL = 'POTENTIAL'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=SYS') THEN
            ETRJMOL = 'SYSTEM'
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'E=TOT') THEN
            ETRJMOL = 'TOTAL'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FULL') THEN
            MOLDEN = 'FULL'
            MOLEKEL = 'NONE'
            WFNFILE = 'NONE'
            DO JOPTION=1,NOPTION
              IF (OPTIONS(JOPTION)(1:5).EQ.'MOLEK') THEN
                MOLDEN = 'NONE'
                MOLEKEL = 'FULL'
              ELSE IF (OPTIONS(JOPTION)(1:5).EQ.'MD') THEN
                MOLDEN = 'MD'
              END IF
            END DO
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'OPT') THEN
            MOLDEN = 'OPT'
            MOLEKEL = 'NONE'
            WFNFILE = 'NONE'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'WFN') THEN
            MOLDEN = 'NONE'
            MOLEKEL = 'NONE'
            WFNFILE = 'FULL'
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'XYZ') THEN
            MOLDEN = 'XYZ'
            MOLEKEL = 'NONE'
            WFNFILE = 'NONE'
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'MD') THEN
            MOLDEN = 'MD'
            MOLEKEL = 'NONE'
            WFNFILE = 'NONE'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
        IF (FREQ) THEN
          IF ((MOLEKEL.EQ.'NONE').AND.(MOLDEN.NE.'FULL')) THEN
            MOLDEN = 'FULL'
            IF (STATUS.EQ.'READ IN') THEN
              PRTSTR = 'VISUAlization SET TO FULL FOR FREQUENCIES'
              CALL ERRMSG('DECODE',PRTSTR,0)
            END IF
          ELSE IF ((MOLDEN.EQ.'NONE').AND.(MOLEKEL.NE.'FULL')) THEN
            MOLEKEL = 'FULL'
            IF (STATUS.EQ.'READ IN') THEN
              PRTSTR = 'VISUAlization SET TO FULL FOR FREQUENCIES'
              CALL ERRMSG('DECODE',PRTSTR,0)
            END IF
          END IF
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Plot options ***
C
      CASE ('PLOT')
C
        PLOT = 'BINARY'
        PLOTMOA = 'DEFAULT'
        PLOTMOB = 'DEFAULT'
        PLOTTYP = 'FUNCTION'
        LATINP = 'VECTORS'
        LATTYP = 'STRUCTURED'
C
        DO IOPTION=1,NOPTION
          DEFINED = .FALSE.
          CALL READFUN(OPTIONS(IOPTION),DEFINED)
          IF (.NOT.DEFINED) THEN
            IF (OPTIONS(IOPTION)(1:5).EQ.'ASCII') THEN
              PLOT = 'ASCII'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'AUXIS') THEN
              PLOTCD = .TRUE.
              CALL CHKPLOT('PLOTCD')
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BASIS') THEN
              PLOTCD = .FALSE.
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BINAR') THEN
              PLOT = 'BINARY'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'TABLE') THEN
              PLOT = 'TABLE'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FBLMO') THEN
              USELMO = .TRUE.
              FBLMO = .TRUE.
              PMLMO = .FALSE.
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'PMLMO') THEN
              USELMO = .TRUE.
              FBLMO = .FALSE.
              PMLMO = .TRUE.
            ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'LMO') THEN
              USELMO = .TRUE.
              FBLMO = .FALSE.
              PMLMO = .FALSE.
            ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'DD') THEN
              USEDD = .TRUE.
            ELSE
              WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
              CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
            END IF
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Isosurface options ***
C
      CASE ('ISOSURFACE')
C
        PLOT = 'BINARY'
        PLOTMOA = 'DEFAULT'
        PLOTMOB = 'DEFAULT'
        PLOTTYP = 'ISOSURFACE'
        LATINP = 'VECTORS'
        LATTYP = 'STRUCTURED'
C
        DO IOPTION=1,NOPTION
          DEFINED = .FALSE.
          CALL READFUN(OPTIONS(IOPTION),DEFINED)
          IF (.NOT.DEFINED) THEN
            IF (OPTIONS(IOPTION)(1:5).EQ.'ASCII') THEN
              PLOT = 'ASCII'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'AUXIS') THEN
              PLOTCD = .TRUE.
              CALL CHKPLOT('PLOTCD')
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BASIS') THEN
              PLOTCD = .FALSE.
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BINAR') THEN
              PLOT = 'BINARY'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'LINEA') THEN
              ISOINT = 'LINEAR'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'LOGAR') THEN
              ISOINT = 'LOGARITHM'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'TABLE') THEN
              PLOT = 'TABLE'
            ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'ISO=') THEN
              IF (INDEX(OPTIONS(IOPTION),'/').NE.0) THEN
                ISOABS = .TRUE.
                SYMBOL = INDEX(OPTIONS(IOPTION),'/')
                NUMBER = OPTIONS(IOPTION)(5:SYMBOL-1)
                ISOVAL = DIGIT('ISOSURFACE','ISO=','ALL',NUMBER)   
                NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
                ISOTWO = DIGIT('ISOSURFACE','ISO=','ALL',NUMBER)
              ELSE IF (OPTIONS(IOPTION)(5:5).EQ.'|') THEN
                ISOABS = .TRUE.
                NUMBER = OPTIONS(IOPTION)(6:)
                NUMBER = NUMBER(1:STREXT(NUMBER)-1)
                ISOVAL = ABS(DIGIT('ISOSURFACE','ISO=','ALL',NUMBER))
              ELSE
                ISOABS = .FALSE.
                NUMBER = OPTIONS(IOPTION)(5:)
                ISOVAL = DIGIT('ISOSURFACE','ISO=','ALL',NUMBER)
              END IF
            ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
              NUMBER = OPTIONS(IOPTION)(5:)
              ISOTOL = DIGIT('ISOSURFACE','TOL=','> 0',NUMBER)
              IF (ISOTOL.GT.0.5) THEN
                CALL ERRMSG('DECODE','ISOTOL MUST BE < 0.5',1)
              END IF
              ISOTOL = MIN(MAX(ISOTOL,TINIEST),0.5)
            ELSE
              WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
              CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
            END IF
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Geometrical surface options ***
C
      CASE ('GEOSURFACE')
C
        PLOT = 'BINARY'
        PLOTTYP = 'GEOSURFACE'
        LATINP = 'VECTORS'
        LATTYP = 'STRUCTURED'
C
        DO IOPTION=1,NOPTION
          DEFINED = .FALSE.
          CALL READFUN(OPTIONS(IOPTION),DEFINED)
          IF (.NOT.DEFINED) THEN
            IF (OPTIONS(IOPTION)(1:5).EQ.'ASCII') THEN
              PLOT = 'ASCII'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BINAR') THEN
              PLOT = 'BINARY'
            ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'TABLE') THEN
              PLOT = 'TABLE'
            ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'TOL=') THEN
              NUMBER = OPTIONS(IOPTION)(5:)
              ISOTOL = DIGIT('ISOSURFACE','TOL=','> 0',NUMBER)
              IF (ISOTOL.GT.0.5) THEN
                CALL ERRMSG('DECODE','ISOTOL MUST BE < 0.5',1)
              END IF
              ISOTOL = MIN(MAX(ISOTOL,TINIEST),0.5)
            ELSE
              WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
              CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
            END IF
          END IF
        END DO  
C
C     *** Read geosurface body ***
C
        CALL READBODY
C
C     ------------------------------------------------------------------
C
C     *** Critical point search options ***
C     
      CASE ('CPSEARCH')
C     
        PLOT = 'BINARY'
        PLOTMOA = 'DEFAULT'
        PLOTMOB = 'DEFAULT'
        PLOTTYP = 'CPSEARCH'
        PRTCPS = .TRUE.
        LATINP = 'VECTORS'
        LATTYP = 'STRUCTURED'
C       
        DO IOPTION=1,NOPTION
          DEFINED = .FALSE.
          CALL READFUN(OPTIONS(IOPTION),DEFINED)
          IF (.NOT.DEFINED) THEN
            IF (PLOTFNC.EQ.'ESP') THEN
              IF (OPTIONS(IOPTION)(1:5).EQ.'AUXIS') THEN
                PLOTCD = .TRUE.
                CALL CHKPLOT('PLOTCD')
              ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BASIS') THEN
                PLOTCD = .FALSE.
              END IF
            ELSE IF (PLOTFNC.EQ.'RHO') THEN
              IF (OPTIONS(IOPTION)(1:5).EQ.'BASIS') THEN
                PLOTCD = .FALSE.
              ELSE
                WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
                CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
              END IF
            ELSE
              WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
              CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
            END IF
          END IF
        END DO
C
C     ------------------------------------------------------------------
C
C     *** Plot box options ***
C
      CASE ('BOX')
C
        DEFINED = .FALSE.
        LATINP = 'VECTORS'
        LATTYP = 'STRUCTURED'
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'COARS') THEN
            BOXMESH = 0.9
            IF (PLOTTYP.EQ.'CPSEARCH') BOXMESH = 1.0
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'LARGE') THEN
            BOXSIZE = 4.0
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'MEDIU') THEN
            BOXMESH = 0.3
            IF (PLOTTYP.EQ.'CPSEARCH') BOXMESH = 0.5
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'SMALL') THEN
            BOXSIZE = 1.2
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'STAND') THEN
            BOXSIZE = 2.0
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'FINE') THEN
            BOXMESH = 0.1
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'READ') THEN
            DEFINED = .TRUE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     *** Read plot box body ***
C
        IF (DEFINED) THEN
          CALL READBOX
        ELSE
          CALL PLOTBOX
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Plot point options ***
C
      CASE ('POINTS')
C
        LATTYP = 'POINTS'
C
        IF (NOPTION.EQ.0) THEN
          LATINP = 'EXTERNAL BINARY'
        ELSE IF (OPTIONS(1)(1:5).EQ.'ASCII') THEN
          LATINP = 'EXTERNAL ASCII'
        ELSE IF (OPTIONS(1)(1:5).EQ.'BINAR') THEN
          LATINP = 'EXTERNAL BINARY'
        ELSE IF (OPTIONS(1)(1:5).EQ.'POLYG') THEN
          LATINP = 'POLYGON'
        ELSE IF (OPTIONS(1)(1:4).EQ.'READ') THEN
          LATINP = 'INTERNAL'
        ELSE
          WRITE (ERRSTR,5100) OPTIONS(1),KEYSTR
          CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
        END IF
C
C     *** Read points body ***
C
        IF (LATINP.EQ.'POLYGON') THEN
          CALL POLYPOINT
        ELSE
          CALL READPOINT
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Orbital phase options ***
C
      CASE ('PHASE')
C
        DO IOPTION=1,NOPTION
          MOPHASE(0) = NOPTION
          NUMBER = OPTIONS(IOPTION)
          MOPHASE(IOPTION) = INT(DIGIT('PHASE','VALUE','> 0',NUMBER))
        END DO
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 12 - DEBUG PRINTING ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Print options ***
C
      CASE ('PRINT')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:5).EQ.'AUXIS') THEN
            PRTAUX = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'BASIS') THEN
            PRTBAS = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'COORD') THEN
            IF (OPTTYP.EQ.'REDUNDANT') PRTPRI = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'ORTHO') THEN
            PRTLMAT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'EMBED') THEN
            PRTEMBED = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'FUKUI') THEN
            PRTFUK = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'NOGEO') THEN
            PRTGEO = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'POPAN') THEN
            PRTPOP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'SADDL') THEN
            PRTSAD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'SYMME') THEN
            PRTSYM = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'VERBO') THEN
            VERBOSE = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'CGTO') THEN
            PRTSTO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'ERIS') THEN
            PRTERIS = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'GRID') THEN
            PRTGRD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'XRAY') THEN
            PRTXRAY = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MAX=') THEN
            NUMBER = OPTIONS(IOPTION)(5:)
            SCFOUT = INT(DIGIT('PRINT','MAX=','>=0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MOE=') THEN
            PRTMOE = .TRUE.
            PRTMOS = .FALSE.
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(5:SYMBOL-1)
            LLMOS = INT(DIGIT('PRINT','MOE=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            ULMOS = INT(DIGIT('PRINT','MOE=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'MOS=') THEN
            IF (.NOT.PRTMOE) PRTMOS = .TRUE.
            SYMBOL = INDEX(OPTIONS(IOPTION),'-')
            NUMBER = OPTIONS(IOPTION)(5:SYMBOL-1)
            LLMOS = INT(DIGIT('PRINT','MOS=','> 0',NUMBER))
            NUMBER = OPTIONS(IOPTION)(SYMBOL+1:)
            ULMOS = INT(DIGIT('PRINT','MOS=','> 0',NUMBER))
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'CHI') THEN
            PRTCHI = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'CIS') THEN
            PRTCIS = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'CON') THEN
            PRTCON = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'DE2') THEN
            PRTDE2 = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'ECP') THEN
            PRTECP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'FIT') THEN
            PRTFIT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'GTO') THEN
            PRTGTO = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'IRC') THEN
            PRTSAD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'MAG') THEN
            PRTMAG = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'MCP') THEN
            PRTMCP = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'MOE') THEN
            PRTMOE = .TRUE.
            PRTMOS = .FALSE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'MOS') THEN
            PRTMOS = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'NIA') THEN
            PRTNIA = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'NMR') THEN
            PRTNMR = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'NQR') THEN
            PRTNQR = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'OPT') THEN
            PRTOPT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'RAM') THEN
            PRTRAM = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'SCF') THEN
            PRTSCF = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'XCE') THEN
            PRTXCE = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:3).EQ.'XCV') THEN
            IF (.NOT.CDXC) PRTXCV = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'C6') THEN
            PRTC6 = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'KS') THEN
            PRTKS = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'MD') THEN
            PRTMD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'TB') THEN
            PRTTB = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'TD') THEN
            PRTTD = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:1).EQ.'G') THEN
            PRTAMAT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:1).EQ.'P') THEN
            PRTPMAT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:1).EQ.'R') THEN
            PRTRMAT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:1).EQ.'S') THEN
            PRTSMAT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:1).EQ.'T') THEN
            PRTTMAT = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:2).EQ.'CP') THEN
            PRTCPS = .TRUE.
          ELSE IF (OPTIONS(IOPTION)(1:5).EQ.'DEBUG') THEN
            PRTAMAT = .TRUE.
            PRTAUX = .TRUE.
            PRTBAS = .TRUE.
            PRTCHI = .TRUE.
            PRTCIS = .TRUE.
            PRTCON = .TRUE.
            PRTCPS = .TRUE.
            PRTDBG = .TRUE.
            PRTDE2 = .TRUE.
            PRTECP = .TRUE.
            PRTEMBED = .TRUE.
            PRTERIS = .TRUE.
            PRTFIT = .TRUE.
            PRTFUK = .TRUE.
            PRTGEO = .TRUE.
            PRTGRD = .TRUE.
            PRTGTO = .TRUE.
            PRTKS = .TRUE.
            PRTLMAT = .TRUE.
            PRTMCP = .TRUE.
            PRTMD = .TRUE.
            PRTMOS = .TRUE.
            PRTNIA = .TRUE.
            PRTNMR = .TRUE.
            PRTNQR = .TRUE.
            PRTOPT = .TRUE.
            PRTPMAT = .TRUE.
            PRTPOP = .TRUE.
            PRTPRI = .TRUE.
            PRTRAM = .TRUE.
            PRTRMAT = .TRUE.
            PRTSAD = .TRUE.
            PRTSCF = .TRUE.
            PRTSMAT = .TRUE.
            PRTSTO = .TRUE.
            PRTSYM = .TRUE.
            PRTTB = .TRUE.
            PRTTD = .TRUE.
            PRTTMAT = .TRUE.
            PRTXCE = .TRUE.
            PRTXRAY = .TRUE.
            VERBOSE = .TRUE.
            IF (.NOT.CDXC) PRTXCV = .TRUE.
            IF (OPTTYP.EQ.'INTERNAL') PRTPRI = .FALSE.
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 13 - RESOURCE USAGE ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Parallel processing ***
C
      CASE ('PARALLEL')
C
        DO IOPTION=1,NOPTION
          IF (OPTIONS(IOPTION)(1:4).EQ.'GRAM') THEN
            MPFRQMEM = 'RAM'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'HSCF') THEN
            MPFRQTYP = 'SCF'
          ELSE IF (OPTIONS(IOPTION)(1:4).EQ.'HXYZ') THEN
            MPFRQTYP = 'XYZ'
          ELSE
            WRITE (ERRSTR,5100) OPTIONS(IOPTION),KEYSTR
            CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
          END IF
        END DO
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 14 - ERROR HANDLING ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Unknown keyword ***
C
      CASE DEFAULT
C
        WRITE (ERRSTR,5000) KEYWORD
 5000   FORMAT ('KEYWORD ',A,' IS UNKNOWN')
        CALL ERRMSG('DECODE',STRCOMP(ERRSTR),1)
C
      END SELECT
C
C     ------------------------------------------------------------------
C
C     *** Error string format ***
C
 5100 FORMAT ('< ',A,' > IS AN UNKNOWN < ',A,' > OPTION')
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE DECODE ***
C
      END
