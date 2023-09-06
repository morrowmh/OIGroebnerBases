-- -*- coding: utf-8 -*-

-*
Copyright 2023 Michael Morrow

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-

newPackage("OIGroebnerBases",
    Headline => "OI-modules over Noetherian polynomial OI-algebras",
    Version => "1.0.0",
    Date => "TBD",
    Keywords => { "Commutative Algebra" },
    Authors => {
        { Name => "Michael Morrow", HomePage => "https://michaelmorrow.me", Email => "michaelhmorrow98@gmail.com" }
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

load "Division.m2"

load "SPolynomial.m2"

load "OIPair.m2"

load "OIGB.m2"

load "oiSyz.m2"

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

-- GB example 1: one linear and one quadratic
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 3.20
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1,2}, P);
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
time B = oiGB({b1, b2}, Verbose => true)

-- Res example 1: single quadratic in width 3
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 5.5 (i)
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,2}, P);
installBasisElements(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
time C = oiRes({b}, 2, Verbose => true)

-- Res example 2: single quadratic in width 2
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 5.5 (ii)
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installBasisElements(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
time C = oiRes({b}, 4, Verbose => true)

-- Res example 3: single quadratic in width 2
-- Comment: compare with res example 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1}, P);
installBasisElements(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},1);
time C = oiRes({b}, 5, Verbose => true) -- Takes my laptop 30 minutes (minimal ranks 1, 2, 5, 9, 14)

-- Res example 4: single quadratic in width 3
-- Comment: compare with res example 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installBasisElements(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1},2);
time C = oiRes({b}, 5, Verbose => true)

-- OI-ideal example
-- Comment: 2x2 minors with a gap of at least 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {0}, P);
installBasisElements(F, 3);
b = (x_(1,1)*x_(2,3)-x_(2,1)*x_(1,3))*e_(3,{},1);
time C = oiRes({b}, 2, Verbose => true)