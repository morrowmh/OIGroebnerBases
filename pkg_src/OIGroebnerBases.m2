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

-- 2x2 minor example
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 2);
b = (x_(1,1)*x_(2,2)-x_(1,2)*x_(2,1))*e_(2,{},1);
time C = oiRes({b}, 3, Verbose => true)

-- Free module example
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {1,1,2});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
time C = oiRes({b1,b2}, 2, Verbose => true)

-- Single quadratic in width 3
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {1,2});
installBasisElements(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
time C = oiRes({b}, 4, Verbose => true) -- 5 is possible, but takes a long time

-- Single quadratic in width 2
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {1,1});
installBasisElements(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
time C = oiRes({b}, 5, Verbose => true)

-- Small Gröbner basis example
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {1,1,2});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
time B = oiGB({b1,b2}, Verbose => true)

-- OI-ideal example
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = (x_(2,1)^2+x_(1,1))*e_(1,{},1);
use F_2; b2 = (x_(2,2)+x_(1,2)*x_(1,1))*e_(2,{},1);
time B = oiGB({b1,b2}, Verbose => true)