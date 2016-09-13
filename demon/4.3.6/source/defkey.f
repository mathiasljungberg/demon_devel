      SUBROUTINE DEFKEY
C
C     Purpose: Define input keywords and their properties.
C
C     History: - Creation (19.02.02, AMK)
C                         (19.02.10, GG)
C                         (19.02.15, AMK)
C
C     ******************************************************************
C
C     List of local variables:
C
C     KEYWORDS: deMon input keywords.
C
C     List of keywords:
C
C     TITLE          : Job title.
C     CHARGe         : Molecular charge.
C     SYMMEtry       : Molecular symmetry use.
C     OPTIMization   : Geometry optimization.
C     SADDLe         : Saddle TS interpolation.
C     IRC            : Intrinsic reaction coordinate.
C     DYNAMics       : Molecular dynamics simulation.
C     EFIELd         : Electric field embedding.
C     EMBED          : Point charge embedding.
C     ALIGNment      : Structure alignment.
C     PATTErn        : Geometry pattern.
C     GEOMEtry       : Input geometry.
C     REACTant       : Reactant geometry.
C     PRODUct        : Product geometry.
C     FORCEfield     : Force field definition.
C     QM/MM          : QM/MM interface definition.
C     SCAN           : Scan potential energy surface.
C     PRINT          : Print debug information.
C     DISPErsion     : Dispersion energy function.
C     ORBITals       : Orbital selection.
C     AUGMEnt        : Augment basis set.
C     BASIS          : Basis set.
C     ECPS           : Effective core potential set.
C     MCPS           : Model core potential set.
C     AUXIS          : Auxiliary function set.
C     MULTIplicity   : Molecular multiplicity.
C     MATINv         : Matrix inversion algorithm.
C     SCFTYpe        : SCF specification.
C     VXCTYpe        : Exchange-correlation potential.
C     CONFIgure      : Atomic configuration.
C     FREEZe         : Freezing of auxiliary funcions.
C     VARIAbles      : Z-Matrix variables.
C     TRAJEctory     : Molecular dynamics trajectory.
C     BATH           : Heat bath for molecular dynamics simulation.
C     VELOCities     : Nuclear velocities.
C     LPCONserve     : Conserve LP in molecular dynamics simulation.
C     CONSTants      : Z-Matrix and Cartesian constants.
C     ERIS           : Electron repulsion integral calculation.
C     GUESS          : SCF start density.
C     MATDIa         : Matrix diagonalization algorithm.
C     DIIS           : DIIS convergence acceleration.
C     MIXINg         : Charge density mixing.
C     SHIFT          : Level shift.
C     SMEAR          : Fractional occupation smearing.
C     MOMODify       : Molecular orbital modification.
C     MOEXChange     : Molecular orbital exchange.
C     FIXMOs         : Fixing of molecular orbitals.
C     LOCALization   : Localization of MOs during the SCF.
C     GRID           : Grid selection.
C     WEIGHting      : Atomic weight function.
C     CFPINtegration : Central Force Potential integration.
C     HESSIan        : Start Hessian for geometry optimization.
C     UPDATe         : Hessian update.
C     STEPType       : Optimization step type.
C     FREQUency      : Frequency analysis.
C     THERMo         : Thermodynamic data.
C     EXCITation     : Excited electronic states.
C     NONCOllinear   : Calculate noncollinear TDDFT excitations.
C     DIPOLe         : Dipole and higher electrostatic moments.
C     POLARisability : Polarizability and hyperpolarizability.
C     MAGOUt         : Magnetic property output.
C     NMR            : Nuclear magnetic resonance.
C     NQR            : Nuclear quadrupole resonance. 
C     MAGNEtizability: Molecular magnetizability.
C     XRAY           : X-ray absorption/emission spectroscopy.
C     PLOT           : Plot output.
C     POPULation     : Population analysis.
C     TOPOLogy       : Topological analysis.
C     DOS            : Density of states (MOs) distribution.
C     SIGPI          : Sigma-pi energy separation.
C     FUKUI          : Fukui reactivity analysis.
C     VISUAlization  : Visualization output.
C     ISOSUrface     : Isosurface output.
C     GEOSUrface     : Gemetrical surface output.
C     CPSEArch       : Critical point search.
C     BOX            : Plot box.
C     POINTs         : Plot points.
C     PHASE          : Phase convention for molecular orbital plot.
C     RDF            : Radial distribution function.
C     SIMULation     : Molecular dynamics simulation post processing.
C     QUADRature     : Quadrature specification.
C     DAVIDson       : Davidson diagonalization specification. 
C     PARALlel       : Define parallel processing algorithm.
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'parameter.h'
C
      INCLUDE 'keywords.h'
C
C     ------------------------------------------------------------------
C
C     *** Define deMon input keywords ***
C
      KEYWORDS(1)  = DEKEY('TITLE','ALL',1,.FALSE.,.FALSE.)
      KEYWORDS(2)  = DEKEY('CHARGE','ALL',1,.FALSE.,.FALSE.)
      KEYWORDS(3)  = DEKEY('SYMMETRY','ALL',1,.FALSE.,.FALSE.)
      KEYWORDS(4)  = DEKEY('OPTIMIZATION','ALL',6,.FALSE.,.FALSE.)
      KEYWORDS(5)  = DEKEY('SADDLE','QM',5,.FALSE.,.FALSE.)
      KEYWORDS(6)  = DEKEY('IRC','QM',9,.FALSE.,.FALSE.)
      KEYWORDS(7)  = DEKEY('DYNAMICS','ALL',5,.FALSE.,.FALSE.)
      KEYWORDS(8)  = DEKEY('EFIELD','ALL',3,.FALSE.,.FALSE.)
      KEYWORDS(9)  = DEKEY('EMBED','ALL',2,.FALSE.,.FALSE.)
      KEYWORDS(10) = DEKEY('ALIGNMENT','ALL',6,.FALSE.,.FALSE.)
      KEYWORDS(11) = DEKEY('PATTERN','ALL',2,.TRUE.,.FALSE.)
      KEYWORDS(12) = DEKEY('GEOMETRY','ALL',2,.FALSE.,.FALSE.)
      KEYWORDS(13) = DEKEY('REACTANT','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(14) = DEKEY('PRODUCT','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(15) = DEKEY('FORCEFIELD','MM',3,.FALSE.,.FALSE.)
      KEYWORDS(16) = DEKEY('QM/MM','ALL',2,.FALSE.,.FALSE.)
      KEYWORDS(17) = DEKEY('SCAN','ALL',4,.FALSE.,.FALSE.)
      KEYWORDS(18) = DEKEY('PRINT','ALL',48,.FALSE.,.FALSE.)
      KEYWORDS(19) = DEKEY('DISPERSION','ALL',1,.FALSE.,.FALSE.)
      KEYWORDS(20) = DEKEY('ORBITALS','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(21) = DEKEY('AUGMENT','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(22) = DEKEY('BASIS','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(23) = DEKEY('ECPS','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(24) = DEKEY('MCPS','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(25) = DEKEY('AUXIS','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(26) = DEKEY('MULTIPLICITY','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(27) = DEKEY('MATINV','ALL',3,.FALSE.,.FALSE.)
      KEYWORDS(28) = DEKEY('SCFTYPE','QM',6,.FALSE.,.FALSE.)
      KEYWORDS(29) = DEKEY('VXCTYPE','QM',4,.FALSE.,.FALSE.)
      KEYWORDS(30) = DEKEY('CONFIGURE','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(31) = DEKEY('FREEZE','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(32) = DEKEY('VARIABLES','ALL',0,.FALSE.,.FALSE.)
      KEYWORDS(33) = DEKEY('TRAJECTORY','ALL',4,.FALSE.,.FALSE.)
      KEYWORDS(34) = DEKEY('BATH','ALL',6,.FALSE.,.FALSE.)
      KEYWORDS(35) = DEKEY('VELOCITIES','ALL',3,.FALSE.,.FALSE.)
      KEYWORDS(36) = DEKEY('LPCONSERVE','ALL',2,.FALSE.,.FALSE.)
      KEYWORDS(37) = DEKEY('CONSTANTS','ALL',0,.FALSE.,.FALSE.)
      KEYWORDS(38) = DEKEY('ERIS','QM',3,.FALSE.,.FALSE.)
      KEYWORDS(39) = DEKEY('GUESS','QM',3,.FALSE.,.FALSE.)
      KEYWORDS(40) = DEKEY('MATDIA','ALL',1,.FALSE.,.FALSE.)
      KEYWORDS(41) = DEKEY('DIIS','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(42) = DEKEY('MIXING','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(43) = DEKEY('SHIFT','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(44) = DEKEY('SMEAR','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(45) = DEKEY('MOMODIFY','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(46) = DEKEY('MOEXCHANGE','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(47) = DEKEY('FIXMOS','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(48) = DEKEY('LOCALIZATION','QM',0,.FALSE.,.FALSE.)
      KEYWORDS(49) = DEKEY('GRID','QM',5,.FALSE.,.FALSE.)
      KEYWORDS(50) = DEKEY('WEIGHTING','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(51) = DEKEY('CFPINTEGRATION','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(52) = DEKEY('HESSIAN','ALL',2,.FALSE.,.FALSE.)
      KEYWORDS(53) = DEKEY('UPDATE','ALL',2,.FALSE.,.FALSE.)
      KEYWORDS(54) = DEKEY('STEPTYPE','ALL',1,.FALSE.,.FALSE.)
      KEYWORDS(55) = DEKEY('FREQUENCY','ALL',3,.FALSE.,.FALSE.)
      KEYWORDS(56) = DEKEY('THERMO','ALL',4,.FALSE.,.FALSE.)
      KEYWORDS(57) = DEKEY('EXCITATION','QM',3,.FALSE.,.FALSE.)
      KEYWORDS(58) = DEKEY('NONCOLLINEAR','QM',0,.FALSE.,.FALSE.)
      KEYWORDS(59) = DEKEY('DIPOLE','QM',0,.FALSE.,.FALSE.)
      KEYWORDS(60) = DEKEY('POLARIZABILITY','QM',8,.FALSE.,.FALSE.)
      KEYWORDS(61) = DEKEY('MAGOUT','QM',0,.FALSE.,.FALSE.)
      KEYWORDS(62) = DEKEY('NMR','QM',3,.FALSE.,.FALSE.)
      KEYWORDS(63) = DEKEY('NQR','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(64) = DEKEY('MAGNETIZABILITY','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(65) = DEKEY('XRAY','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(66) = DEKEY('PLOT','QM',4,.FALSE.,.FALSE.)
      KEYWORDS(67) = DEKEY('POPULATION','QM',4,.FALSE.,.FALSE.)
      KEYWORDS(68) = DEKEY('TOPOLOGY','QM',4,.FALSE.,.FALSE.)
      KEYWORDS(69) = DEKEY('DOS','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(70) = DEKEY('SIGPI','QM',0,.FALSE.,.FALSE.)
      KEYWORDS(71) = DEKEY('FUKUI','QM',0,.FALSE.,.FALSE.)
      KEYWORDS(72) = DEKEY('VISUALIZATION','ALL',3,.FALSE.,.FALSE.)
      KEYWORDS(73) = DEKEY('ISOSURFACE','QM',6,.FALSE.,.FALSE.)
      KEYWORDS(74) = DEKEY('GEOSURFACE','QM',3,.FALSE.,.FALSE.)
      KEYWORDS(75) = DEKEY('CPSEARCH','QM',2,.FALSE.,.FALSE.)
      KEYWORDS(76) = DEKEY('BOX','QM',3,.FALSE.,.FALSE.)
      KEYWORDS(77) = DEKEY('POINTS','QM',1,.FALSE.,.FALSE.)
      KEYWORDS(78) = DEKEY('PHASE','QM',MAXPLOT,.FALSE.,.FALSE.)
      KEYWORDS(79) = DEKEY('RDF','ALL',3,.FALSE.,.FALSE.)
      KEYWORDS(80) = DEKEY('SIMULATION','ALL',4,.FALSE.,.FALSE.)
      KEYWORDS(81) = DEKEY('QUADRATURE','QM',3,.FALSE.,.FALSE.)
      KEYWORDS(82) = DEKEY('DAVIDSON','QM',4,.FALSE.,.FALSE.)
      KEYWORDS(83) = DEKEY('PARALLEL','ALL',2,.FALSE.,.FALSE.)
      KEYWORDS(84) = DEKEY('STOREWF','QM',0,.FALSE.,.FALSE.)
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE DEFKEY ***
C
      END
