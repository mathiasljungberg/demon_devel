C     FLAGS COMMON
C
C     Purpose: Common definition for program flags.
C
C     History: - Creation (16.01.96, AMK)
C                         (08.09.08, GG)
C                         (10.12.12, AAI)
C
C     ******************************************************************
C
C     List of global control flags:
C
C     CDFTYP : Flag for charge density fitting type.
C     CTYP   : Flag for correlation functional.
C     C6READ : Flag for C6 coefficient read-in format.
C     ERITYP : Flag for electron repulsion integral type calculation.
C     FITTING: Flag for fitting procedure.
C     FITTYP : Flag for auxiliary function fit.
C     G98TOP : Flag for G98 topological analysis fixed grid type.
C     G98TYP : Flag for G98 fixed grid type.
C     GGFUN  : Flag for grid generating function.
C     GRDTYP : Flag for grid type.
C     HREAD  : Flag for Hessian read-in format.
C     INPTYP : Flag for input type.
C     LATTYP : Flag for plot lattice type.
C     LATINP : Flag for plot lattice input.
C     LCTYP  : Flag for local correlation functional in GGA.
C     MATDIA : Flag for general diagonalization technique.
C     MOLDEN : Flag for Molden output.
C     MOLEKEL: Flag for Molekel output.
C     PLOT   : Flag for plot output.
C     PLOTTYP: Flag for plot type.
C     PRODIA : Flag for projector diagonalization technique.
C     RADQUA : Flag for radial quadrature type.
C     STATUS : Flag for program status.
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
C     List of global print flags:
C
C     PRTAMAT : If true, print auxiliary function matrix.
C     PRTAUX  : If true, print AUX table.
C     PRTBAS  : If true, print contraction for basis set optimization.
C     PRTC6   : If true, print C6 coefficient table.
C     PRTCHI  : If true, print magnetic suceptibility information.
C     PRTCIS  : If true, print CIS ERI batching output.
C     PRTCON  : If true, print molecular dyanmics constraints.
C     PRTCPS  : If true, print critical points.
C     PRTDBG  : If true, print all (debug output).
C     PRTDE2  : If true, print hessian during geoemetry optimization.
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
C     List of global logical flags:
C
C     AUGMENTED: If true, basis set must be augmented.
C     BATH     : If true, heat bath for the MD is requested.
C     BWDER    : If true, include Becke weight derivatives.
C     CDXC     : If true, fitted exchange-correlation is requested.
C     CHARMM   : If true, QM/MM forces for CHARMM are calculated.
C     CUKS     : If true, a constrained UKS calculation is requested.
C     DIRGRD   : If true, a direct grid integration is requested.
C     DIRTOP   : If true, a direct topological integration is requested.
C     ECPS     : If true, effective core potentials are declared.
C     ERIRAM   . If true, distributed ERI storage in RAM is requested.
C     ESA      : If true, a electronic structure analysis is requested.
C     EXX      : If true, exact exchange is requested.
C     FALDA    : If true, LDA fxc kernel is used for all functionals.
C     FREQ     : If true, frequency analysis is requested.
C     FREQRST  : If true, restart an old frequency analysis.
C     GALDA    : If true, LDA gxc kernel is used for all functionals.
C     GGA      : If true, a GGA functional is requested.
C     GUESSONLY: If true, only the start guess is calculated.
C     HYBRID   : If true, an hybrid functional is requested.
C     INITDIIS : If true, DIIS data are initialized.
C     INITGRD  : If true, grid data are initialized.
C     INITLEB  : If true, Lebedev grids are initialized.
C     INITOCT  : If true, octree data are initalized.
C     INITXC   : If true, xc functional constants are initialized.
C     LAP      : If true, a meta-GGA functionals is requested.
C     MCPS     : If true, model core potentials are declared.
C     MD       : If true, a molecular dynamic simulation is requested.
C     MDRUN    : If true, a molecular dynamics run is requested.
C     MMONLY   : If true, a pure MM calculation is performed.
C     MONOPOLE : If true, a multipole expansion of NAIs is requested.
C     MULTIPOLE: If true, a multipole expansion of ERIs is requested.
C     NEWINP   : If true, a new input was generated.
C     NODIIS   : If true, no DIIS step was made.
C     OKS      : If true, a open Kohn-Sham calculation is requested.
C     OMA      : If true, the optimal mixing approach is requested.
C     OPT      : If true, geometry optimization is requested.
C     OPTRST   : If true, restart geometry optimization.
C     PLOTCD   : If true, plot charge density based quantities.
C     PORT     : If true, interface ports are activated.
C     PROP     : If true, property calculation is requested.
C     QMMM     : If true, a mixed QM/MM calculation is performed.
C     QMONLY   : If true, a pure QM calculation is performed.
C     RANGRD   : If true, randomize SCF generated grid.
C     REJECT   : If true, reject current optimization step.
C     RESTART  : If true, the restart file will be read.
C     RETGRD   : If true, retrieve old grid for the next SCF run.
C     REWWF    : If true, rewind wave function output file before read/write
C     ROKS     : If true, restricted open Kohn-Sham is requested.
C     ROMO     : If true, calculate block-diagonal ROKS MOs.
C     ROTGRD   : If true, rotate local atomic coordinate systems.
C     SCANPES  : If true, scan potential energy surface.
C     SCF      : If true, a single point start SCF is requested.
C     SPHORB   : If true, spherical orbitals will be used.
C     STOREWF  : If true, save wave function after each MD step.
C     SYMMETRY : If true, use molecular symmetry.
C     TOPCD    : If true, use charge density for topological analysis.
C     USEDD    : If true, use deformation density for plotting.
C     USEDIA   : If true, use diagonal SVD decomposition.
C     USELMO   : If true, use localized molecular orbitals.
C     USELPI   : If true, use localized molecular pi orbitals.
C     USELUD   : If true, use LU (Cholesky) decomposition.
C     USEONE   : If true, use identity start matrix for PCG solver.
C     USESVD   : If true, use singular value decomposition.
C     ZBUILD   : If true, Z-Matrix builder succeeded.
C     ZCONST   : If true, Cartesian constraints applied to Z-Matrix.
C     ZFIXED   : If true, internal constraints applied to Z-Matrix.
C
C     List of SCF flags:
C
C     CFGATOM: If true, an atomic configuration is requested.
C     DIIS   : If true, DIIS for SCF is requested.
C     FERMI  : If true, a Fermi start density is requested.
C     FIXCFG : If true, the electronic configuration will be fixed.
C     FIXLAST: If true, the electronic configuration of the last SCF
C              cycle will be fixed.
C     HBOMD  : If true, a core start density is used in each BOMD step.
C     HUECKEL: If true, a Hueckel start density is requested.
C     LSHIFT : If true, level shifting is performed.
C     MOCC   : If true, MO occupation will be modified.
C     MOEX   : If true, MOs will be exchanged.
C     PRESCF : If true, a pre-SCF solution is searched.
C     PROJCT : If true, a projected start density is requested.
C     SCFADJ : If true, SCF adjustment in optimization is requested.
C     SETAOCC: If true, set fixed atomic occupation numbers.
C     SMEAR  : If true, fractional orbital occupation enabled.
C     SMRUNI : If true, uniform fractional occupation is enabled.
C     VDW    : If true, add empirical van der Waals energy.
C
C     List of optimization flags:
C
C     HESSIAN: Start Hessian for geometry optimization.
C     OPTTYP : Geometry optimization coordinate type.
C     STEPTYP: Step type for geometry optimization (DEFAULT).
C     UPHESS : Update type for geometry optimization.
C
C     List of property flags:
C
C     BPA    : If true, perform Bader population analysis.
C     DEFORM : If true, use deformation density in population analysis.
C     DOS    : If true, an AO resolved DOS is calculated.
C     EFIELD : If true, electric field embedding is requested.
C     EMBED  : If true, embedding in a cube is requested.
C     FBLMO  : If true, a Foster-Boys MO localization is requested.
C     FPA    : If true, perform fuzzy (Becke) population analysis.
C     FUKUI  : If true, perform a Fukui reactivity analysis.
C     FULLESP: If true, a full MESP calculation is requested.
C     FULLPA : If true, an orbital resolved population analysis is done.
C     HPA    : If true, perform Hirshfeld population analysis.
C     ISOABS : If true, an absolute isosurface value is given.
C     ITELF  : If true, an IT based ELF calcualtion is requested.
C     ITHFO  : If true, perform ITHPA with frac. occupation numbers.
C     ITHPA  : If true, perform iterative Hirshfeld analysis.
C     LPA    : If true, perform Loewdin population analysis.
C     MAGOUT : If true, write magnetic properties output.
C     MPA    : If true, perform Mulliken population analysis.
C     MXI    : IF true, calculate molecular magnetizability xi.
C     NICS   : If true, calculate nuclear independent chemical shifts.
C     NMR    : If true, calculate nuclear magnetic resonance.
C     NONCOL : If true, noncollinear formulation is used in TDDFT.
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
C     TDA    : If true, use Tamm-Dancoff approximation in TDDFT.
C     TDDFT  : If true, calculate excited states via TDDFT.
C     THERMOS: If true, calculate thermodynamical data.
C     VPA    : If true, perform Voronoi population analysis.
C     XAS    : If true, x-ray absorption spectroscopy is requested.
C     XES    : If true, x-ray emission spectroscopy is requested.
C
C     ------------------------------------------------------------------
C
C     *** Common block for control flags ***
C
      CHARACTER*15 C6READ,HREAD,LATINP,RADQUA,WEIGHTS
      CHARACTER*10 CDFTYP,CTYP,ERITYP,FITTING,FITTYP,G98TOP,G98TYP,
     $             GGFUN,GRDTYP,HESSIAN,INPTYP,LATTYP,LCTYP,MATDIA,
     $             MOLDEN,MOLEKEL,OPTTYP,PLOT,PLOTTYP,PRODIA,STATUS,
     $             STEPTYP,TDDIA,UPHESS,WFNFILE,XTYP
C
      CHARACTER*15 UNIT(4)
C
      COMMON /CFLAGS/ CDFTYP,CTYP,C6READ,ERITYP,FITTING,FITTYP,G98TOP,
     $                G98TYP,GGFUN,GRDTYP,HESSIAN,HREAD,INPTYP,LATINP,
     $                LATTYP,LCTYP,MATDIA,MOLDEN,MOLEKEL,OPTTYP,PLOT,
     $                PLOTTYP,PRODIA,RADQUA,STATUS,STEPTYP,TDDIA,UNIT,
     $                UPHESS,WEIGHTS,WFNFILE,XTYP
C
C     *** Common block for global logical flags ***
C
      LOGICAL AUGMENTED,BATH,BWDER,CDXC,CHARMM,CUKS,DIRGRD,DIRTOP,ECPS,
     $        ERIRAM,ESA,EXX,FALDA,FREQ,FREQRST,GALDA,GGA,GUESSONLY,
     $        HYBRID,INITDIIS,INITGRD,INITLEB,INITOCT,INITXC,LAP,MCPS,
     $        MD,MDRUN,MMONLY,MONOPOLE,MULTIPOLE,NEWINP,NODIIS,OKS,OMA,
     $        OPT,OPTRST,PLOTCD,PORT,PROP,QMMM,QMONLY,RANGRD,REJECT,
     $        RESTART,RETGRD,ROKS,ROMO,ROTGRD,SCANPES,SCF,SPHORB,
     $        SYMMETRY,TOPCD,USEDD,USEDIA,USELMO,USELPI,USELUD,
     $        USEONE,USESVD,ZBUILD,ZCONST,ZFIXED,STOREWF,REWWF
C
      COMMON /LFLAGS/ AUGMENTED,BATH,BWDER,CDXC,CHARMM,CUKS,DIRGRD,
     $                DIRTOP,ECPS,ERIRAM,ESA,EXX,FALDA,FREQ,FREQRST,
     $                GALDA,GGA,GUESSONLY,HYBRID,INITDIIS,INITGRD,
     $                INITLEB,INITOCT,INITXC,LAP,MCPS,MD,MDRUN,MMONLY,
     $                MONOPOLE,MULTIPOLE,NEWINP,NODIIS,OKS,OMA,OPT,
     $                OPTRST,PLOTCD,PORT,PROP,QMMM,QMONLY,RANGRD,
     $                REJECT,RESTART,RETGRD,ROKS,ROMO,ROTGRD,SCANPES,
     $                SCF,SPHORB,SYMMETRY,TOPCD,USEDD,USEDIA,USELMO,
     $                USELPI,USELUD,USEONE,USESVD,ZBUILD,ZCONST,ZFIXED,
     $                STOREWF,REWWF 
C
C     *** Common block for SCF flags ***
C
      LOGICAL CFGATOM,DIIS,FERMI,FIXCFG,FIXLAST,HBOMD,HUECKEL,LSHIFT,
     $        MOCC,MOEX,PRESCF,PROJCT,SCFADJ,SETAOCC,SMEAR,SMRUNI,VDW
C
      COMMON /SCFFLAGS/ CFGATOM,DIIS,FERMI,FIXCFG,FIXLAST,HBOMD,HUECKEL,
     $                  LSHIFT,MOCC,MOEX,PRESCF,PROJCT,SCFADJ,SETAOCC,
     $                  SMEAR,SMRUNI,VDW
C
C     *** Common block for property flags ***
C
      LOGICAL BPA,DEFORM,DOS,EFIELD,EMBED,FBLMO,FPA,FUKUI,FULLESP,
     $        FULLPA,HPA,ISOABS,ITELF,ITHFO,ITHPA,LPA,MAGOUT,MPA,MXI,
     $        NICS,NMR,NONCOL,NPA,NQR,NSRC,PEM,PMLMO,POLARIS,RAMAN,
     $        RGTEN,SIGPI,SPHPA,TDA,TDDFT,THERMOS,VPA,XAS,XES
C
      COMMON /PROPFLAGS/ BPA,DOS,DEFORM,EFIELD,EMBED,FBLMO,FPA,FUKUI,
     $                   FULLESP,FULLPA,HPA,ISOABS,ITELF,ITHFO,ITHPA,
     $                   LPA,MAGOUT,MPA,MXI,NICS,NMR,NONCOL,NPA,NQR,
     $                   NSRC,PEM,PMLMO,POLARIS,RAMAN,RGTEN,SIGPI,
     $                   SPHPA,TDA,TDDFT,THERMOS,VPA,XAS,XES
C
C     *** Common block for logical print flags ***
C
      LOGICAL PRTAMAT,PRTAUX,PRTBAS,PRTC6,PRTCHI,PRTCIS,PRTCON,
     $        PRTCPS,PRTDBG,PRTDE2,PRTECP,PRTEMBED,PRTERIS,PRTFIT,
     $        PRTFUK,PRTGEO,PRTGRD,PRTGTO,PRTKS,PRTLMAT,PRTMAG,
     $        PRTMCP,PRTMD,PRTMOE,PRTMOS,PRTNIA,PRTNMR,PRTNQR,
     $        PRTOPT,PRTPMAT,PRTPOP,PRTPRI,PRTRAM,PRTRMAT,PRTSAD,
     $        PRTSCF,PRTSMAT,PRTSTO,PRTSYM,PRTTB,PRTTD,PRTTMAT,
     $        PRTXCE,PRTXCV,PRTXRAY,VERBOSE
C
      COMMON /PFLAGS/ PRTAMAT,PRTAUX,PRTBAS,PRTC6,PRTCHI,PRTCIS,
     $                PRTCON,PRTCPS,PRTDBG,PRTDE2,PRTECP,PRTERIS,
     $                PRTEMBED,PRTFIT,PRTFUK,PRTGEO,PRTGRD,PRTGTO,
     $                PRTKS,PRTLMAT,PRTMAG,PRTMCP,PRTMD,PRTMOE,
     $                PRTMOS,PRTNIA,PRTNMR,PRTNQR,PRTOPT,PRTSAD,
     $                PRTSCF,PRTPOP,PRTPMAT,PRTPRI,PRTRAM,PRTRMAT,
     $                PRTSMAT,PRTSTO,PRTSYM,PRTTB,PRTTD,PRTTMAT,
     $                PRTXCE,PRTXCV,PRTXRAY,VERBOSE
C
C     ------------------------------------------------------------------
