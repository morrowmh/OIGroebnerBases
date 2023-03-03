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

-- Scratch work
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,1,x);
F = makeFreeOIModule(P, e, {1});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)^3*e_(1,{1},1);
use F_2; b2 = x_(1,1)^2*e_(2,{1},1); b3 = x_(1,2)^2*e_(2,{2},1); b4 = x_(1,1)*x_(1,2)*e_(2,{2},1);
B = oiGB({b1, b2, b3, b4}, Verbose => true)
C = oiSyz(B, d, Verbose => true)
isOIGB C

restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,1,x);
F = makeFreeOIModule(P, e, {1});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; f = x_(1,1)^3*e_(1,{1}, 1);
use F_2; h = x_(1,2)^2*e_(2, {2}, 1) + x_(1,1)*x_(1,2)*e_(2, {2}, 1);
time B = oiGB({f, h}, Verbose => true)
time C = oiSyz(B, d, Verbose => true)
time isOIGB C

load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,1,x);
F = makeFreeOIModule(P, e, {1,1,2});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(1,1)*e_(1,{1},2);
use F_2; b2 = x_(1,1)*e_(2,{2},2) + x_(1,2)*e_(2,{1,2},3); b3 = e_(2,{2},1);
time C = oiRes({b1,b2,b3}, 3, Verbose => true)

-- GB Example 0
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {1,1,2});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+ x_(2,1)*x_(2,2)*e_(2,{1,2},3);
time B = oiGB({b1, b2}, Verbose => true)

-- GB Example
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{},1) + x_(2,1)^2*e_(1,{},1);
use F_2; b2 = (x_(1,2)*x_(1,1)+x_(2,2))*e_(2,{},1);
time B = oiGB({b1,b2}, Verbose => true, Strategy => 2)

-- Res Example
restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 3);
use F_3; b = (x_(1,1)*x_(2,3)-x_(1,3)*x_(2,1))*e_(3,{},1);
time C = oiRes({b}, 5, Verbose => true)

load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,1,x);
F = makeFreeOIModule(P, e, {1,2});
installBasisElements(F, 1);
installBasisElements(F, 2);
installBasisElements(F, 3);
installBasisElements(F, 4);

-- Res example 1
use F_3; b1 = x_(1,1)*x_(1,2)*e_(3,{2},1); b2 = (x_(1,1)+x_(1,2))*e_(3,{1,3},2)+x_(1,3)*e_(3,{2,3},2);
C = oiRes({b1,b2}, 3, Verbose => true)

-- Res example 2
use F_1; b1 = x_(1,1)*e_(1,{1},1);
use F_2; b2 = x_(1,1)*e_(2,{2},1); b3 = x_(1,2)*e_(2,{1},1);
C = oiRes({b1, b2, b3}, 1, Verbose => true)