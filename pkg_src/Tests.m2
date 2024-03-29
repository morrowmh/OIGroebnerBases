-- TEST 0
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1,2}, P);
installGeneratorsInWidth(F, 1);
installGeneratorsInWidth(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
B = oiGB {b1, b2};
assert(#B === 3);
assert isOIGB B
///

-- TEST 1
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installGeneratorsInWidth(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
B = oiGB {b};
assert(#B === 2);
C = oiSyz(B, d);
assert(#C === 3);
assert isOIGB C
///

-- TEST 2
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,2}, P);
installGeneratorsInWidth(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
C = oiRes({b}, 2);
assert isComplex C;
assert(getRank C_0 === 1);
assert(getRank C_1 === 2);
assert(apply(getBasisElements C_0, getWidth) === {3});
assert(apply(getBasisElements C_1, getWidth) === {5, 5})
///

-- TEST 3
TEST ///
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installGeneratorsInWidth(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
C = oiRes({b}, 3);
assert isComplex C;
assert isHomogeneous(C.dd_0);
assert isHomogeneous(C.dd_1);
assert isHomogeneous(C.dd_2);
assert isHomogeneous(C.dd_3);
assert(getRank C_0 === 1);
assert(getRank C_1 === 1);
assert(getRank C_2 === 2);
assert(apply(getBasisElements C_0, getWidth) === {2});
assert(apply(getBasisElements C_1, getWidth) === {4});
assert(apply(getBasisElements C_2, getWidth) === {5,5})
///