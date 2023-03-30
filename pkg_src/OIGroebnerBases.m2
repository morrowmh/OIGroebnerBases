-- -*- coding: utf-8 -*-

-- PURPOSE: Algorithms for computing Gröbner bases, syzygies and free resolutions for submodules of free OI-modules over Noetherian polynomial OI-algebras
-- PROGRAMMER: Michael Morrow
-- COMMENT: This package was made using Macaulay2-Package-Template, available here: https://github.com/morrowmh/Macaulay2-Package-Template

newPackage("OIGroebnerBases",
    Headline => "Computation in OI-modules over Noetherian polynomial OI-algebras",
    Version => "2.0.0-dev",
    Date => "April 4, 2022", -- Project birthday
    Keywords => { "Commutative Algebra" },
    Authors => {
        { Name => "Michael Morrow", HomePage => "https://michaelmorrow.org", Email => "michaelhmorrow98@gmail.com" }
    },
    DebuggingMode => true,
    HomePage => "https://github.com/morrowmh/OIGroebnerBases"
)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- EXPORT AND PROTECT ----------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

load "ExportAndProtect.m2"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- BODY ------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

load "OIMap.m2"

load "PolynomialOIAlgebra.m2"

load "FreeOIModule.m2"

load "Terms.m2"

load "InducedModuleMap.m2"

load "FreeOIModuleMap.m2"

load "Algorithms.m2"

load "OIResolution.m2"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- DOCUMENTATION ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

beginDocumentation()

load "Documentation.m2"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- TESTS -----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

load "Tests.m2"

end

restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 3);
b = (x_(1,1)*x_(2,3)-x_(1,3)*x_(2,1))*e_(3,{},1);
time C = oiRes({b}, 3, Verbose => true)

restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 2);
b = (x_(1,1)*x_(2,2)-x_(1,2)*x_(2,1))*e_(2,{},1);
time D = oiRes({b}, 3, Verbose => true)