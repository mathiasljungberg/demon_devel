      SUBROUTINE DEFAULT(KEYWORD,SUCCESS)
C
C     Purpose: Default keyword setting.
C
C     History: - Creation (18.02.02, AMK)
C                         (05.03.10, GG)
C                         (19.02.15, AMK)
C
C     ******************************************************************
C
C     List of local variables:
C
C     KEYWORD: Input keyword.
C     SUCCESS: If true, keyword will be decoded.
C
C     ******************************************************************
C
C     List of numerical flags:
C
C     CDFTOL: Tolerance for charge density fitting convergence.
C     CDPTOL: Tolerance for charge density perturbation convergence.
C     CDVAL : User defined charge density mixing value.
C     ERITOL: Tolerance for ERI thresholds.
C     GRDTOL: Tolerance for automatic grid generation.
C     IRCMAX: Maximum number of intrinsic reaction coordinate steps.
C     IRCTOL: Tolerance for IRC path convergence.
C     LSMIN : Dynamic level shift minimum value.
C     LSVAL : Level shift value.
C     OPTMAX: Maximum number of optimization iterations.
C     OPTTOL: Tolerance for geometry optimization convergence.
C     PCGMAX: Maximum number of PCG iterations.
C     SADMAX: Maximum number of saddle interpolation steps.
C     SADTOL: Tolerance for saddle convergence.
C     SCFMAX: Maximum number of SCF iterations.
C     SCFOUT: Number of SCF iterations with print output.
C     SCFTOL: Tolerance for SCF convergence.
C     SMRVAL: SMEAR value.
C     SSIX  : Dispersion (vdW) energy scaling factor.
C     SVDTOL: Tolerance for singular value decomposition.
C     WPOLAR: Dynamical (hyper)-polarizability frequencies.
C     XASTOL: Tolerance for XAS orthogonalization.
C
C     List of global control flags:
C
C     CDFTYP : Flag for charge density fitting type.
C     CTYP   : Flag for correlation functional.
C     C6READ : Flag for C6 coefficient read-in format.
C     FITTING: Flag for fitting procedure.
C     FITTYP : Flag for auxiliary function fit.
C     GGFUN  : Flag for grid generating function.
C     GRDTYP : Flag for grid type.
C     HREAD  : Flag for Hessian read-in format.
C     INPTYP : Flag for input type.
C     MATDIA : Flag for matrix diagonalization technique.
C     MOLDEN : Flag for MOLDEN output.
C     MOLEKEL: Flag for MOLEKEL output.
C     OPTTYP : Flag for optimization type.
C     PLOT   : Flag for plot output.
C     PLOTTYP: Flag for plot type.
C     PRODIA : Flag for projector diagonalization technique.
C     RADQUA : Flag for radial quadrature type.
C     TDDIA  : Flag for TDDFT matrix diagonalization technique.
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
C     EXX      : If true, exact exchange is requested.
C     FREQ     : If true, frequency analysis is requested.
C     FREQRST  : If true, restart an old frequency analysis.
C     GGA      : If true, a GGA functional is requested.
C     HYBRID   : If true, a hybrid functional is requested.
C     IRC      : If true, a IRC optimization is requested.
C     LAP      : If true, a meta-GGA functionals is requested.
C     MD       : If true, a molecular dynamic simulation is requested.
C     MDRUN    : If true, a molecular dynamics run is requested.
C     MULTIPOLE: If true, a multipole expansion of ERIs is requested.
C     OKS      : If true, open-shell Kohn-Sham is requested.
C     OPT      : If true, geometry optimization is requested.
C     OPTRST   : If true, restart geometry optimization.
C     PORT     : If true, interface ports are activated.
C     RANGRD   : If true, randomize SCF generated grid.
C     ROKS     : If true, restricted open-shell Kohn-Sham is requested.
C     ROMO     : If true, calculate block-diagonal ROKS MOs.
C     ROTGRD   : If true, rotate local atomic coordinate systems.
C     SADDLE   : If true, perform a saddle interpolation.
C     SADTS    : If true, a TS is searched after a saddle interpolation.
C     SCANPES  : If true, scan potential energy surface.
C     SPHORB   : If true, spherical orbitals will be used.
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
C     SCFADJ : If true, SCF adjustment in optimization is requested.
C     SETAOCC: If true, fixed atomic occupation number will be used.
C     SMEAR  : If true, fractional orbital occupation is enabled.
C     SMRUNI : If true, uniform fractional occupation is enabled.
C     USEDIA : If true, use diagonal SVD decomposition.
C     USELUD : If true, use LU (Cholesky) decomposition.
C     USEONE : If true, use identity start matrix for PCG solver.
C     USESVD : If true, use singular value decomposition.
C
C     List of optimization, IRC and PES scan flags:
C
C     HESSIAN : Start Hessian for geometry optimization.
C     MAXDR   : Maximum step size for optimization.
C     MAXIRC  : Maximum step size for intrinsic reaction coordinate.
C     MINIRC  : Minimum step size for intrinsic reaction coordinate.
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
C     MDTIME  : Elapsed molecular dynamics trajectory time.
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
C     FPA    : If true, perform fuzzy (Becke) population analysis.
C     FUKUI  : If true, perform a Fukui reactivity analysis.
C     FULLPA : If true, an orbital resolved population analysis is done.
C     HPA    : If true, perform Hirshfeld population analysis.
C     ITHFO  : If true, perform ITHPA with frac. occupation numbers.
C     ITHPA  : If true, perform iterative Hirshfeld analysis.
C     LPA    : If true, perform Loewdin population analysis.
C     MAGOUT : If true, write magnetic properties output.
C     MODMAP : If true, align with best preserved connectivity.
C     MPA    : If true, perform Mulliken population analysis.
C     MXI    : IF true, calculate molecular magnetizability xi.
C     NICS   : If true, calculate nuclear independent chemical shifts.
C     NMR    : If true, calculate nuclear magnetic resonance.
C     NONCOL : If true, noncollinear exications in TDDFT.
C     NPA    : If true, generate file for natural bond order analysis.
C     NQR    : If true, calculate nuclear quadrupole resonance.
C     NSRC   : If true, calculate spin-rotation constant.
C     PEM    : If true, calculate permanent electrostatic moments.
C     POLARIS: If true, calculate polarizabilities.
C     RAMAN  : If true, calculate Raman intensities.
C     RGTEN  : If true, calculate rotational g-tensor.
C     SIGPI  : If true, calculate sigma-pi energy separation.
C     SPHPA  : If true, perform spherical atom Hirshfeld analysis.
C     TDA    : If true, use Tamm-Dancoff approximation in TDDFT.
C     TDDFT  : If true, calculate excited states via TDDFT.
C     THERMOS: If true, calculate thermodynamical data.
C     VPA    : If true, perform Voronoi population analysis.
C     XAS    : If true, x-ray absorption spectroscopy is requested.
C     XES    : If true, x-ray emission spectroscopy is requested.
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
C     PRTXRAY : If true, print x-ray information.
C     VERBOSE : If true, print all warnings.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'align.h'
      INCLUDE 'auxis.h'
      INCLUDE 'cfp.h'
      INCLUDE 'custom.h'
      INCLUDE 'dynamics.h'
      INCLUDE 'embed.h'
      INCLUDE 'flags.h'
      INCLUDE 'freq.h'
      INCLUDE 'grid.h'
      INCLUDE 'iteration.h'
      INCLUDE 'marching.h'
      INCLUDE 'mm.h'
      INCLUDE 'molden.h'
      INCLUDE 'molecule.h'
      INCLUDE 'mp_demon.h'
      INCLUDE 'opt.h'
      INCLUDE 'pcg.h'
      INCLUDE 'physic.h'
      INCLUDE 'plot.h'
      INCLUDE 'polarizability.h'
      INCLUDE 'property.h'
      INCLUDE 'qmmm.h'
      INCLUDE 'scf.h'
      INCLUDE 'scfvec.h'
      INCLUDE 'symmetry.h'
      INCLUDE 'tddft.h'
      INCLUDE 'thermo.h'
      INCLUDE 'threshold.h'
      INCLUDE 'tolerance.h'
      INCLUDE 'xc.h'
C
      LOGICAL SUCCESS
C
      CHARACTER*32 KEYSTR
      CHARACTER*(*) KEYWORD
      CHARACTER*80 PRTSTR,STRCOMP,UTL
C
      INTEGER KEYLEN,SEGMENT,STREXT
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
C     *** Default charge setting ***
C
      CASE ('CHARGE')
C
        CHARGE = 0
C
C     ------------------------------------------------------------------
C
C     *** Default symmetry setting ***
C
      CASE ('SYMMETRY')
C
        SYMGEO = .FALSE.
        SYMMETRY = .TRUE.
C
C     ------------------------------------------------------------------
C
C     *** Structure alignment setting ***
C
      CASE ('ALIGNMENT')
C
        ALIGNMENT = .FALSE.
C
        MODMAP = .FALSE.
        ENANTIO = .FALSE.
        UNIFORM = .FALSE.
C
        CUBLEV = 6
        NBLACK = 0
        POPTOL = 50
C
C     ------------------------------------------------------------------
C
C     *** Default geometry setting ***
C
      CASE ('GEOMETRY')
C
        INPTYP = 'CARTESIAN'
C
      CASE ('PATTERN')
      CASE ('PRODUCT')
      CASE ('REACTANT')
      CASE ('CONSTANTS')
      CASE ('VARIABLES')
C
C     ------------------------------------------------------------------
C
C     *** Default scan setting ***
C
      CASE ('SCAN')
C
        SCANPES = .FALSE.
        RELAXING = .TRUE.
C
        SCANEND = 'UNKNOWN'
        SCANLAB = 'UNKNOWN'
C
C     ------------------------------------------------------------------
C
C     *** Default electric field embedding setting ***
C
      CASE ('EFIELD')
C
        EFIELD = .FALSE.
C
        ELFIELD(1) = 0.0
        ELFIELD(2) = 0.0
        ELFIELD(3) = 0.0
C
C     ------------------------------------------------------------------
C
C     *** Default embedding setting ***
C
      CASE ('EMBED')
C
        EMBED = .FALSE.
        MONOPOLE = .TRUE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 2 - LCAO KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default auxiliary function fitting ***
C
      CASE ('FREEZE')
C
        FIXNONE = 1.E20
        FIXCUSP = 1.0E3
        FIXCORE = 1.0E2
        FIXVALENCE = 1.0
C
        FITTYP = 'ALL'
        ZETFIX(0) = FIXNONE
        FIXAUXIS(:) = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default auxiliary function setting ***
C
      CASE ('AUXIS')
C
        SUCCESS = .TRUE.
        ATOMAUX(0) = '(GEN-A2)'
C
C     ------------------------------------------------------------------
C
C     *** Default basis set augmentation setting ***
C
      CASE ('AUGMENT')
C
        SUCCESS = .TRUE.
        AUGMENTED = .FALSE.
        ATOMBAS(0,3) = '(NONE)'
C
C     ------------------------------------------------------------------
C
C     *** Default basis set setting ***
C
      CASE ('BASIS')
C
        SUCCESS = .TRUE.
        ATOMBAS(0,1) = '(DZVP)'
C
C     ------------------------------------------------------------------
C
C     *** Default ECP setting ***
C
      CASE ('ECPS')
C
        SUCCESS = .TRUE.
        ATOMECP(0) = 'NONE'
C
C     ------------------------------------------------------------------
C
C     *** Default MCP setting ***
C
      CASE ('MCPS')
C
        SUCCESS = .TRUE.
        ATOMMCP(0) = 'NONE'
C
C     ------------------------------------------------------------------
C
C     *** Default orbital setting ***
C
      CASE ('ORBITALS')
C
        SPHORB = .TRUE.
C
C     ------------------------------------------------------------------
C
C     *** Default orbital localization setting ***
C
      CASE ('LOCALIZATION')
C
        USELPI = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default ERI setting ***
C
      CASE ('ERIS')
C
        ERIRAM = .FALSE.
        MULTIPOLE = .TRUE.
C
        ERITOL = 1.0E-10
        ERITYP = 'DIRECT'
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 3 - SCF KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default SCF setting ***
C
      CASE ('SCFTYPE')
C
        SCFADJ = .TRUE.
        SCFMAX = 100
        SCFTOL = 1.0E-5
        SCFTHRESH = 1.0E-5
C
        CUKS = .FALSE.
        ROKS = .FALSE.
        ROMO = .FALSE.
        IF (MULTIP.EQ.1) THEN
          OKS = .FALSE.
        ELSE
          OKS = .TRUE.
        END IF
C
        ROKSA(1:3) = 0.5
        ROKSB(1:3) = 0.5
C
C     ------------------------------------------------------------------
C
C     *** Default start density setting ***
C
      CASE ('GUESS')
C
        FERMI = .FALSE.
        HUECKEL = .TRUE.
        PROJCT = .FALSE.
        ATOMBAS(0,2) = '(STO-3G)'
C
        IF (SCFMAX.EQ.0) THEN
          FERMI = .FALSE.
          HUECKEL = .FALSE.
          PROJCT = .FALSE.
          RESTART = .TRUE.
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default matrix diagonalization setting ***
C
      CASE ('MATDIA')
C
        PRODIA = 'RS'
C
        IF (MPP) THEN
          MATDIA = 'RS'
        ELSE IF (NATOM.EQ.1) THEN
          MATDIA = 'JACOBI'
        ELSE
          MATDIA = 'DSYEV'
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default matrix inversion setting ***
C
      CASE ('MATINV')
C
        USEDIA = .TRUE.
        USELUD = .FALSE.
        USEONE = .FALSE.
        USESVD = .TRUE.
C
        PCGMAX = 5000
        CANTOL = 1.0E-8
        CDFTOL = 5.0E-4
        CDPTOL = 1.0E-4
        SVDTOL = 1.0E-6
        CDFTHRESH = 5.0E-4
C
        FITIO = .TRUE.
        FITTING = 'NUMERICAL'
C
C     ------------------------------------------------------------------
C
C     *** Default DIIS setting ***
C
      CASE ('DIIS')
C
        DIIS = .TRUE.
        DIISTOL = 0.01
C
C     ------------------------------------------------------------------
C
C     *** Default charge density mixing ***
C
      CASE ('MIXING')
C
        CDVAL = -0.3
        OMA = .FALSE.
        CDFTYP = 'MIXING'
C
        IF (EXX.AND.(.NOT.HYBRID)) THEN
          CDVAL = -1.0
        ELSE
          CDVAL = -0.3
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default level-shift setting ***
C
      CASE ('SHIFT')
C
        LSHIFT = .FALSE.
        LSMIN = 0.0
        LSVAL = 0.0
C
C     ------------------------------------------------------------------
C
C     *** Default smear setting ***
C
      CASE ('SMEAR')
C
        SMEAR = .FALSE.
C
        SMRVAL = 0.0
        SMRUNI = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default multiplicity setting ***
C
      CASE ('MULTIPLICITY')
C
        IF (MOD(NELEC,2).EQ.0) THEN
          MULTIP = 1
        ELSE
          MULTIP = 2
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default setting for MO modify ***
C
      CASE ('MOMODIFY')
C
        MOCC = .FALSE.
        MODALP = .FALSE.
        MODBET = .FALSE.
C
        NMOMODA = 0
        NMOMODB = 0
        MOMODA(:,:) = 0.0
        MOMODB(:,:) = 0.0
C
C     ------------------------------------------------------------------
C
C     *** Default setting for MO exchange ***
C
      CASE ('MOEXCHANGE')
C
        MOEX = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default setting for fixing MOs ***
C
      CASE ('FIXMOS')
C
        FIXCFG = .FALSE.
        FIXLAST = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default setting for atomic configuration ***
C
      CASE ('CONFIGURE')
C
        CFGATOM = .FALSE.
        SETAOCC = .FALSE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 4 - NUMERICAL INTEGRATION KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default grid settings ***
C
      CASE ('GRID')
C
        IF (MPP) THEN
          DIRGRD = .TRUE.
        ELSE
          DIRGRD = .FALSE.
        END IF
C
        IF ((XTYP.EQ.'NOFUN').AND.(CTYP.EQ.'NOFUN')) THEN
          GRDTOL = TINY(1.0)
          GGFUN = 'NONE'
          GRDTYP = 'NONE'
          G98TYP = 'NONE'
        ELSE
          GRDTOL = 1.0E-4
          GGFUN = 'SCF'
          GRDTYP = 'ADAPTIVE'
          G98TYP = '(75,302)p'
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default weight function setting ***
C
      CASE ('WEIGHTING')
C
        WEIGHTS = 'BECKE SCREENED'
C
C     ------------------------------------------------------------------
C
C     *** Default quadrature setting ***
C
      CASE ('QUADRATURE')
C
        BWDER = .FALSE.
        REFGRD = .FALSE.
        ROTGRD = .FALSE.
C
        IF (NATOM.EQ.1) THEN
          RANGRD = .FALSE.
        ELSE IF (PEM.OR.POLARIS.OR.TRJDIP.OR.TRJPOL) THEN
          RANGRD = .FALSE.
        ELSE IF ((GGFUN.EQ.'NONE').OR.(GGFUN.EQ.'GUESS')) THEN
          RANGRD = .FALSE.
        ELSE
          RANGRD = .TRUE.
        END IF
C
        IF (GRDTYP.EQ.'ADAPTIVE') THEN
          RADQUA = 'GAUSS-CHEBYSHEV'
        ELSE IF (GRDTYP.EQ.'FIXED') THEN
          RADQUA = 'EULER-MACLAURIN'
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default semi-numerical Central Force integration setting ***
C
      CASE ('CFPINTEGRATION')
C
        CFPTOL = 1.0E-8
        NGP_CFP = 99
C
C     ------------------------------------------------------------------
C
C     *** Default exchange-correlation potential setting ***
C
      CASE ('VXCTYPE')
C
        XALPHA = 0.75
        XTYP = 'LSD'
        CTYP = 'PVWN'
C
        CDXC = .TRUE.
        EXX = .FALSE.
        GGA = .FALSE.
        LAP = .FALSE.
C
        PEXX = 1.0
        HYBRID = .FALSE.
        FDKERNEL = .FALSE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 5 - QM/MM KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default setting for empirical van der Waals energy ***
C
      CASE ('DISPERSION')
C
        SSIX = 1.0
        VDW = .FALSE.
        C6READ = 'FROM LIST'
C
C     ------------------------------------------------------------------
C
C     *** Default QM/MM interface setting ***
C
      CASE ('QM/MM')
C
        NTYPQM = 0
        CHARMM = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default force field setting ***
C
      CASE ('FORCEFIELD')
C
        FFIELD = 'UNKNOWN'
        MMFILE = .FALSE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 6 - OPTIMIZATION KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default intrinsic reaction coordinate setting ***
C
      CASE ('IRC')
C
        IRC = .FALSE.
        USEMW = .TRUE.
C
        DEVIB = 0.001
        IRCDR = 0.1
        IRCMAX = 50
        IRCSIGN = 1.0
        IRCTOL = 3.0E-4
        MAXIRC = 0.5
        MINIRC = 0.005
C
        IF (STATUS.EQ.'READ IN') IRCTYP = 'NEW'
C
C     ------------------------------------------------------------------
C
C     *** Default saddle interpolation setting ***
C
      CASE ('SADDLE')
C
        SADALI = .TRUE.
        SADTS = .FALSE.
        SADDLE = .FALSE.
C
        SADMAX = 100
        SADTOL = 0.1
        SADINT = 0.05
C
C     ------------------------------------------------------------------
C
C     *** Default optimization setting ***
C
      CASE ('OPTIMIZATION')
C
        TS = .FALSE.
        OPT = .FALSE.
        OPTRST = .FALSE.
C
        MAXDR = 0.3
        OPTMAX = 50
        OPTTOL = 3.0E-4
        OPTTYP = 'REDUNDANT'
C
C     ------------------------------------------------------------------
C
C     *** Default Hessian setting ***
C
      CASE ('HESSIAN')
C
        IF (OPTTYP.EQ.'REDUNDANT') THEN
          HESSIAN = 'FISCHER'
        ELSE IF (OPTTYP.EQ.'INTERNAL') THEN
          HESSIAN = 'BAKER'
        ELSE IF (OPTTYP.EQ.'CARTESIAN') THEN
          HESSIAN = 'UNITMATRIX'
        END IF
C
        HREAD = 'PLAIN-FORMAT'
C
        IF ((STATUS.EQ.'READ IN').AND.IRC) HESSIAN = 'CALCULATED'
C
C     ------------------------------------------------------------------
C
C     *** Default update setting ***
C
      CASE ('UPDATE')
C
        IF (TS) THEN
          UPHESS = 'POWELL'
        ELSE
          UPHESS = 'BFGS'
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default step type setting ***
C
      CASE ('STEPTYPE')
C
        STEPTYP = 'LEVENBERG'
C
C     ------------------------------------------------------------------
C
C     *** Default store wave function setting ***
C
      CASE('STOREWF')
        STOREWF= .FALSE.
        write(6,*) "Settign default value STOREWF", STOREWF
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 7 - MOLECULAR DYNAMICS KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default molecular dynamics setting ***
C
      CASE ('DYNAMICS')
C
        MD = . FALSE.
        MDRUN = .FALSE.
        STOREWF = .FALSE.
C
        MDINT = 10
        MDSTEP = 1.0*FSEC
        MDRADIUS = 0.0
C
C     ------------------------------------------------------------------
C
C     *** Default trajectory setting ***
C
      CASE ('TRAJECTORY')
C
        IF (STATUS.EQ.'READ IN') THEN
          TRJBIT = 1
          TRJMAX = -1
          TRJMIN = -1
          TRJINT = 1
          TRJTYP = 'NEW'
          TRJACC = .FALSE.
          TRJDIP = .FALSE.
          TRJEXT = .FALSE.
          TRJGEO = .FALSE.
          TRJMOS = .FALSE.
          TRJNHC = .FALSE.
          TRJNMR = .FALSE.
          TRJNSR = .FALSE.
          TRJMAG = .FALSE.
          TRJPOL = .FALSE.
          TRJRDF = .FALSE.
          TRJROG = .FALSE.
          TRJOVL = .FALSE.
          TRJLEN = SEGMENT('ACCELERATIONS')
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default simulation settings ***
C
      CASE ('SIMULATION')
C
        SIMINT = 1
        ETRJ = 'STANDARD'
        PTRJ = 'UNDEFINED'
C
C     ------------------------------------------------------------------
C
C     *** Default heat bath setting ***
C
      CASE ('BATH')
C
        BATH = .FALSE.
        MDBATH = 'NONE'
C
        IF (STATUS.EQ.'READ IN') THEN
          NNHC = 0
          MDTEMP = 300.0
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default nuclear velocity setting ***
C
      CASE ('VELOCITIES')
C
        SUCCESS = .TRUE.
C
        MDVELO = 'RANDOM'
        MDZERO = 'NONE'
        MDTEMP0 = MDTEMP
        VELOTYP = 'RANDOM'
C
C     ------------------------------------------------------------------
C
C     *** Default LP conservation setting ***
C
      CASE ('LPCONSERVE')
C
        MDFLP = .FALSE.
        MDVLP = .FALSE.
        LPTOL = 1.0E-10
C
C     ------------------------------------------------------------------
C
C     *** Default radial distribution function setting ***
C
      CASE ('RDF')
C
        RDFMAX = 10.0*BOHR
        RDFWIDTH = 0.01*BOHR
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 8 - PERTURBATION THEORY KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default frequency analysis setting ***
C
      CASE ('FREQUENCY')
C
        FREQ = .FALSE.
        FREQFIX = .FALSE.
        FREQRST = .FALSE.
        RAMAN = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default thermodynamic data setting ***
C
      CASE ('THERMO')
C
        THERMOS = .FALSE.
        VIBONLY = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default polarizability setting ***
C
      CASE ('POLARIZABILITY')
C
        NPOLAR = 1
        POLORD = 1
        GXCTHRESH = 0.0
        WPOLAR(:) = 0.0
C
        EOPE = .FALSE.
        EORE = .FALSE.
        OSHG = .FALSE.
        EFISH = .FALSE.
        FALDA = .FALSE.
        GALDA = .FALSE.
        POLARIS = .FALSE.
C
        POLKEY = 'DD'
        IF (LAP.OR.EXX) THEN
          POLTYP = 'NIACPKS'
        ELSE
          POLTYP = 'ANALYTIC'
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default excitation setting ***
C
      CASE ('EXCITATION')
C
        TDA = .FALSE.
        TDDFT = .FALSE.
        NONCOL = .FALSE.
C
        TDDIA = 'DAVIDSON'
C
C     ------------------------------------------------------------------
C
C     *** Default noncollinear excitation setting ***
C
      CASE('NONCOLLINEAR')
C
        NONCOL = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default Davidson setting ***
C
      CASE ('DAVIDSON')
C
        NEIGEN = 10
        KRYBAS = 30
        KRYMAX = 500
        KRYTOL = 1.0E-5
C
C     ------------------------------------------------------------------
C
C     *** Default nuclear magnetic resonance setting ***
C
      CASE ('NMR')
C
        NMR = .FALSE.
        NICS = .FALSE.
        NSRC = .FALSE.
        NMRSIG(:) = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default nuclear quadrupole resonance setting ***
C
      CASE ('NQR')
C
        NQR = .FALSE.
        NQRSIG(:) = .FALSE.
C
        NUCSPIN(:) = 0
        NUCQUAD(:) = 0.0
C
        BARNS = BOHR**2
C
C     ------------------------------------------------------------------
C
C     *** Default molecular magnetic susceptibility setting ***
C
      CASE ('MAGNETIZABILITY')
C
        MXI = .FALSE.
        RGTEN = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default x-ray spectroscopy setting ***
C
      CASE ('XRAY')
C
        XAS = .FALSE.
        XES = .FALSE.
        XASTOL = 1.0E-6
C
C     ------------------------------------------------------------------
C
C     *** Default chemical reactivity analysis (Fukui) setting ***
C
      CASE ('FUKUI')
C
        FUKUI = .FALSE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 9 - ELECTRONIC STRUCTURE ANALYSIS KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default electrostatic moment setting ***
C
      CASE ('DIPOLE')
C
        PEM = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default population analysis setting ***
C
      CASE ('POPULATION')
C
        BPA = .FALSE.
        FPA = .FALSE.
        HPA = .FALSE.
        LPA = .FALSE.
        MPA = .FALSE.
        NPA = .FALSE.
        VPA = .FALSE.
        ITHFO = .FALSE.
        ITHPA = .FALSE.
        SPHPA = .FALSE.
        DEFORM = .FALSE.
        FULLPA = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default topological analysis setting ***
C
      CASE ('TOPOLOGY')
C
        BPA = ((.FALSE.).OR.BPA)
C
        IF (MPP) THEN
          DIRTOP = .TRUE.
        ELSE
          DIRTOP = .FALSE.
        END IF
C
        TOPMAX = 999
        TOPTOL = 5.0E-4
        G98TOP = '(75,302)p'
C
C     ------------------------------------------------------------------
C
C     *** Default density of state (DOS) setting ***
C
      CASE ('DOS')
C
        DOS = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default sigma-pi energy separation setting ***
C
      CASE ('SIGPI')
C
        SIGPI = .FALSE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 10 - MAGOUT KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default magnetic output setting ***
C
      CASE ('MAGOUT')
C
        MAGOUT = .FALSE.
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 11 - PLOT KEYWORDS ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default visualization setting ***
C
      CASE ('VISUALIZATION')
C
        PORT = .TRUE.
C
        ETRJMOL = 'POTENTIAL'
        MOLEKEL = 'NONE'
        WFNFILE = 'NONE'
C
        IF (FREQ) THEN
          MOLDEN = 'FULL'
        ELSE
          MOLDEN = 'XYZ'
        END IF
C
C     ------------------------------------------------------------------
C
C     *** Default plot setting ***
C
      CASE ('PLOT')
C
        HFIELD(1) = 0.0
        HFIELD(2) = 0.0
        HFIELD(3) = 1.0
C
        PLOT = 'OFF'
        PLOTTYP = 'OFF'
        USELMO = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default isosurface setting ***
C
      CASE ('ISOSURFACE')
C
        ISOINT = 'LINEAR'
        ISOTOL = 0.001
        ISOTWO = -99999
C
C     ------------------------------------------------------------------
C
C     *** Default geometrical surface setting ***
C
      CASE ('GEOSURFACE')
C
        GEOAXES = 0.0
        GEORIGIN = 0.0
C
C     ------------------------------------------------------------------
C
C     *** Default critical point search setting ***
C
      CASE ('CPSEARCH')
C
C     ------------------------------------------------------------------
C
C     *** Default plot box setting ***
C
      CASE ('BOX')
C
        BOXATOM = .TRUE.
C
        IF (PLOTTYP.EQ.'CPSEARCH') THEN
          BOXMESH = 0.5
          BOXSIZE = 4.0
        ELSE
          BOXMESH = 0.3
          BOXSIZE = 2.0
        END IF
C
        IF (.NOT.MMONLY) CALL PLOTBOX
C
C     ------------------------------------------------------------------
C
C     *** Default plot point setting ***
C
      CASE ('POINTS')
C
        JOINED = .FALSE.
C
C     ------------------------------------------------------------------
C
C     *** Default orbital phase setting ***
C
      CASE ('PHASE')
C
        MOPHASE(0:MAXPLOT) = 0
C
C     ------------------------------------------------------------------
C     ******************************************************************
C     *** SECTION - 12 - DEBUG PRINTING ***
C     ******************************************************************
C     ------------------------------------------------------------------
C
C     *** Default print setting ***
C
      CASE ('PRINT')
C
        SCFOUT = 0
C
        PRTAMAT = .FALSE.
        PRTAUX = .FALSE.
        PRTBAS = .FALSE.
        PRTC6 = .FALSE.
        PRTCHI = .FALSE.
        PRTCIS = .FALSE.
        PRTCON = .FALSE.
        PRTCPS = .FALSE.
        PRTDE2 = .FALSE.
        PRTDBG = .FALSE.
        PRTECP = .FALSE.
        PRTEMBED = .FALSE.
        PRTERIS = .FALSE.
        PRTFIT = .FALSE.
        PRTFUK = .FALSE.
        PRTGEO = .TRUE.
        PRTGRD = .FALSE.
        PRTGTO = .FALSE.
        PRTKS = .FALSE.
        PRTLMAT = .FALSE.
        PRTMAG = .FALSE.
        PRTMCP = .FALSE.
        PRTMD = .FALSE.
        PRTMOE = .FALSE.
        PRTMOS = .FALSE.
        PRTNIA = .FALSE.
        PRTNMR = .FALSE.
        PRTNQR = .FALSE.
        PRTOPT = .FALSE.
        PRTPOP = .FALSE.
        PRTPMAT = .FALSE.
        PRTPRI = .FALSE.
        PRTRAM = .FALSE.
        PRTRMAT = .FALSE.
        PRTSAD = .FALSE.
        PRTSCF = .FALSE.
        PRTSMAT = .FALSE.
        PRTSTO = .FALSE.
        PRTSYM = .FALSE.
        PRTTB = .FALSE.
        PRTTD = .FALSE.
        PRTTMAT = .FALSE.
        PRTXCE = .FALSE.
        PRTXCV = .FALSE.
        PRTXRAY = .FALSE.
        VERBOSE = .FALSE.
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
        MPFRQMEM = 'I/O'
        MPFRQTYP = 'XYZ'
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
        WRITE (PRTSTR,5000) KEYSTR
 5000   FORMAT ('KEYWORD ',A,' IS UNKNOWN')
        CALL ERRMSG('DEFAULT',STRCOMP(PRTSTR),1)
C
      END SELECT
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE DEFAULT ***
C
      END
