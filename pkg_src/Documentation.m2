doc ///
    Key
        OIGroebnerBases
    Headline
        OI-modules over Noetherian polynomial OI-algebras
    Description
        Text
            {\em OIGroebnerBases} is a package for Gröbner bases, syzygies and free resolutions for submodules of free OI-modules over Noetherian polynomial OI-algebras. For an introduction to the theory of OI-modules, see [3].

            Given a Noetherian polynomial OI-algebra $\mathbf{P} := (\mathbf{X}^{\text{OI},1})^{\otimes c}$ for some integer $c > 0$, one can consider free OI-modules $\mathbf{F} := \bigoplus_{i=1}^s\mathbf{F}^{\text{OI}, d_i}$ over $\mathbf{P}$ for integers $d_i\geq 0$.
            
            Gröbner bases for submodules of $\mathbf{F}$ were introduced in [3]. Free resolutions and homological aspects of submodules have been studied in [2,3]. Using the methods of [1], Gröbner bases, syzygy modules, and free resolutions for submodules can be computed with @TO oiGB@, @TO oiSyz@ and @TO oiRes@ respectively.

            {\em References:}

            [1] M. Morrow and U. Nagel, {\it Computing Gröbner Bases and Free Resolutions of OI-Modules}, Preprint, arXiv:2303.06725, 2023.

            [2] N. Fieldsteel and U. Nagel, {\it Minimal and cellular free resolutions over polynomial OI-algebras}, Preprint, arXiv:2105.08603, 2021.

            [3] U. Nagel and T. Römer, {\it FI- and OI-modules with varying coefficients}, J. Algebra 535 (2019), 286-322.
    Subnodes
        :Polynomial OI-algebras
        PolynomialOIAlgebra
        makePolynomialOIAlgebra
        VariableOrder
        ColUpRowUp
        ColUpRowDown
        ColDownRowUp
        ColDownRowDown
        RowUpColUp
        RowUpColDown
        RowDownColUp
        RowDownColDown
        (net,PolynomialOIAlgebra)
        (toString,PolynomialOIAlgebra)
        (symbol _,PolynomialOIAlgebra,ZZ)
        :Free OI-modules
        FreeOIModule
        FreeOIModuleMap
        ModuleInWidth
        VectorInWidth
        makeFreeOIModule
        DegreeShifts
        OIMonomialOrder
        installBasisElements
        isZero
        getGenerators
        getFreeOIModule
        getWidth
        getRank
        oiOrbit
        (isZero,FreeOIModuleMap)
        (isZero,VectorInWidth)
        (symbol SPACE,FreeOIModuleMap,VectorInWidth)
        (net,FreeOIModule)
        (net,FreeOIModuleMap)
        (net,ModuleInWidth)
        (net,VectorInWidth)
        (toString,FreeOIModule)
        (symbol _,FreeOIModule,ZZ)
        (degree,VectorInWidth)
        (use,ModuleInWidth)
        (terms,VectorInWidth)
        (leadTerm,VectorInWidth)
        (leadMonomial,VectorInWidth)
        (leadCoefficient,VectorInWidth)
        (symbol *,Number,VectorInWidth)
        (symbol +,VectorInWidth,VectorInWidth)
        (symbol *,RingElement,VectorInWidth)
        (symbol -,VectorInWidth)
        (symbol -,VectorInWidth,VectorInWidth)
        (isHomogeneous,FreeOIModuleMap)
        (isHomogeneous,VectorInWidth)
        :OI-Gröbner bases
        oiGB
        minimizeOIGB
        reduceOIGB
        isOIGB
        :OI-syzygies
        oiSyz
        :OI-resolutions
        OIResolution
        oiRes
        TopNonminimal
        isComplex
        (describe,OIResolution)
        (net,OIResolution)
        (symbol _,OIResolution,ZZ)
///

doc ///
    Key
        PolynomialOIAlgebra
    Headline
        the class of all Noetherian polynomial OI-algebras over a field
    Description
        Text
            This type implements OI-algebras of the form $(\mathbf{X}^{\text{OI},1})^{\otimes c}$ for some integer $c>0$. To make a @TT "PolynomialOIAlgebra"@ object, use @TO makePolynomialOIAlgebra@. Each @TT "PolynomialOIAlgebra"@ object is equipped with a variable order; see @TO VariableOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ, VariableOrder => RowDownColUp)
///

doc ///
    Key
        makePolynomialOIAlgebra
        (makePolynomialOIAlgebra,ZZ,Symbol,Ring)
        [makePolynomialOIAlgebra,VariableOrder]
    Headline
        make a PolynomialOIAlgebra object
    Usage
        makePolynomialOIAlgebra(c,x,K)
    Inputs
        c:ZZ
        x:Symbol
        K:Ring
    Outputs
        :PolynomialOIAlgebra
    Description
        Text
            Makes a polynomial OI-algebra over the field @TT "K"@ with @TT "c"@ rows of variables in the symbol @TT "x"@. The @TO VariableOrder@ option is used to specify the ordering of the variables.
        Example
            P = makePolynomialOIAlgebra(1, x, QQ)
            Q = makePolynomialOIAlgebra(2, y, QQ, VariableOrder => RowDownColUp)
///

doc ///
    Key
        VariableOrder
    Headline
        variable ordering for polynomial OI-algebras
    Description
        Text
            Used as an optional argument in @TO makePolynomialOIAlgebra@ to specify an ordering on the variables of a polynomial OI-algebra.
        
            Permissible values:
        Code
            UL {
                {TT "VariableOrder => ", TT TO ColUpRowUp},
                {TT "VariableOrder => ", TT TO ColUpRowDown},
                {TT "VariableOrder => ", TT TO ColDownRowUp},
                {TT "VariableOrder => ", TT TO ColDownRowDown},
                {TT "VariableOrder => ", TT TO RowUpColUp},
                {TT "VariableOrder => ", TT TO RowUpColDown},
                {TT "VariableOrder => ", TT TO RowDownColUp},
                {TT "VariableOrder => ", TT TO RowDownColDown}
            }
        Example
            P = makePolynomialOIAlgebra(2, x, QQ, VariableOrder => RowDownColUp)
///

doc ///
    Key
        ColUpRowUp
    Headline
        column up row up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'<j$ or $j'=j$ and $i'<i$.
///

doc ///
    Key
        ColUpRowDown
    Headline
        column up row down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'<j$ or $j'=j$ and $i'>i$.
///

doc ///
    Key
        ColDownRowUp
    Headline
        column down row up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'>j$ or $j'=j$ and $i'<i$.
///

doc ///
    Key
        ColDownRowDown
    Headline
        column down row down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $j'>j$ or $j'=j$ and $i'>i$.
///

doc ///
    Key
        RowUpColUp
    Headline
        row up column up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'<i$ or $i'=i$ and $j'<j$.
///

doc ///
    Key
        RowUpColDown
    Headline
        row up column down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'<i$ or $i'=i$ and $j'>j$.
///

doc ///
    Key
        RowDownColUp
    Headline
        row up column up variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'>i$ or $i'=i$ and $j'<j$.
///

doc ///
    Key
        RowDownColDown
    Headline
        row down column down variable order
    Description
        Text
            Orders the variables of a polynomial OI-algebra according to $x_{i',j'} < x_{i,j}$ if $i'>i$ or $i'=i$ and $j'>j$.
///

doc ///
    Key
        (net,PolynomialOIAlgebra)
    Headline
        display a polynomial OI-algebra
    Usage
        net P
    Inputs
        P:PolynomialOIAlgebra
    Outputs
        :Net
    Description
        Text
            Displays the base field, number of variable rows, variable symbol, and variable order of a @TO PolynomialOIAlgebra@ object.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            net P
///

doc ///
    Key
        (toString,PolynomialOIAlgebra)
    Headline
        display a polynomial OI-algebra in condensed form
    Usage
        toString P
    Inputs
        P:PolynomialOIAlgebra
    Outputs
        :String
    Description
        Text
            Displays the base field, number of variable rows, variable symbol, and variable order of a @TO PolynomialOIAlgebra@ object as a string.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            toString P
///

doc ///
    Key
        (symbol _,PolynomialOIAlgebra,ZZ)
    Headline
        get the $K$-algebra in a specified width of a polynomial OI-algebra
    Usage
        P_n
    Inputs
        P:PolynomialOIAlgebra
        n:ZZ
    Outputs
        :PolynomialRing
    Description
        Text
            Returns the $K$-algebra in width @TT "n"@ of a polynomial OI-algebra.
        Example
            P = makePolynomialOIAlgebra(2, y, QQ);
            P_4
///

doc ///
    Key
        FreeOIModule
    Headline
        the class of all free OI-modules over a polynomial OI-algebra
    Description
        Text
            This type implements free OI-modules over polynomial OI-algebras. To make a @TT "FreeOIModule"@ object, use @TO makeFreeOIModule@. To get the generators and rank of a free OI-module, use @TO getGenerators@ and @TO getRank@ respectively. To install the basis elements of a free OI-module in a specified width, use @TO installBasisElements@.

            Each @TT "FreeOIModule"@ object comes equipped with either the @TO Lex@ monomial order induced by the monomial order on its underlying polynomial OI-algebra, or the Schreyer monomial order induced by another free OI-module; see @TO makeFreeOIModule@ and @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P)
///

doc ///
    Key
        FreeOIModuleMap
    Headline
        the class of all maps between free OI-modules
    Description
        Text
            This type implements morphisms between free OI-modules. Given free OI-modules $\mathbf{F}$ and $\mathbf{G}$ over an OI-algebra $\mathbf{P}$, a $\mathbf{P}$-linear map $\varphi:\mathbf{F}\to\mathbf{G}$ is determined by the images of the basis elements of $\mathbf{F}$.
            
            To evaluate $\varphi$ on an element of $\mathbf{F}$, use @TO (symbol SPACE,FreeOIModuleMap,VectorInWidth)@.
            
            One obtains @TT "FreeOIModuleMap"@ objects through the use of @TO oiRes@, as in the below example.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2)
            phi = C.dd_1
            G = getGenerators C_1
            phi G#0
            phi G#1
///

doc ///
    Key
        ModuleInWidth
    Headline
        the class of all modules that appear as a widthwise component of a free OI-module
    Description
        Text
            The width $n$ component of a free OI-module is implemented as a @TT "ModuleInWidth"@ object. To obtain a @TT "ModuleInWidth"@ object, one restricts a given free OI-module to a specified width using @TO (symbol _,FreeOIModule,ZZ)@, as seen in the example below.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            F_2
///

doc ///
    Key
        VectorInWidth
    Headline
        the class of all elements of a free OI-module
    Description
        Text
            An element of a free OI-module $\mathbf{F}$ is defined to be an element of $\mathbf{F}_n$ for some integer $n\geq0$. Such an element is implemented as a @TT "VectorInWidth"@ object. One typically makes @TT "VectorInWidth"@ objects by defining a @TO FreeOIModule@ object, calling @TO installBasisElements@, and then manipulating the generators; see below for an example.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)
            instance(b, VectorInWidth)
///

doc ///
    Key
        makeFreeOIModule
        (makeFreeOIModule,Symbol,List,PolynomialOIAlgebra)
        [makeFreeOIModule,DegreeShifts]
        [makeFreeOIModule,OIMonomialOrder]
    Headline
        make a FreeOIModule object
    Usage
        makeFreeOIModule(e,L,P)
    Inputs
        e:Symbol
        L:List
        P:PolynomialOIAlgebra
    Outputs
        :FreeOIModule
    Description
        Text
            Makes a free OI-module of the form $\bigoplus_{i=1}^s\mathbf{F}^{\text{OI}, d_i}$ over the @TO PolynomialOIAlgebra@ object @TT "P"@ with $L=\{d_1,\ldots,d_s\}$ and basis symbol @TT "e"@.

            The @TO DegreeShifts@ option is used to specify a shift of grading. This option must be set to either @TT "null"@ for no shifts, or a list of integers describing the desired shifts.

            The @TO OIMonomialOrder@ option must be set to either @TO Lex@, for the lexicographic order, or a list of elements of some free OI-module for the Schreyer order. See below for examples.
        
            @HEADER2 "Example 1: Lex"@
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P, DegreeShifts => {3,2,1})
        Text
            @HEADER2 "Example 2: Schreyer"@
        Example
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2);
            G = makeFreeOIModule(d, {2}, P, DegreeShifts => {-degree b}, OIMonomialOrder => {b})
///

doc ///
    Key
        DegreeShifts
    Headline
        grading shifts for free OI-modules
    Description
        Text
            Used as an optional argument in @TO makeFreeOIModule@ to specify shifts of grading.

            Permissible values:
        Code
            UL {
                {TT "DegreeShifts => ", TT "null", " for no shift of grading"},
                {TT "DegreeShifts => ", TT TO List, " for a shift of grading indicated by a list of integers"}
            }
        Text
            @HEADER2 "Example 1: no shifts"@

            The free OI-module $\mathbf{F}^{\text{OI},1}\oplus\mathbf{F}^{\text{OI},2}$ has its generators in degree zero.
        Example
            P = makePolynomialOIAlgebra(1, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P)
            apply(getGenerators F, degree)
        Text
            @HEADER2 "Example 2: nontrivial shifts"@
            
            The free OI-module $\mathbf{F}^{\text{OI},1}(-3)\oplus\mathbf{F}^{\text{OI},2}(-4)$ has its generators in degrees $3$ and $4$.
        Example
            F = makeFreeOIModule(e, {1,2}, P, DegreeShifts => {-3,-4})
            apply(getGenerators F, degree)
///

doc ///
    Key
        OIMonomialOrder
    Headline
        monomial order option for free OI-modules
    Description
        Text
            Used as an optional argument in @TO makeFreeOIModule@ to specify the desired monomial order.
        
            Permissible values:
        Code
            UL {
                {TT "OIMonomialOrder => ", TT TO Lex, " for the lexicographic order induced by the monomial order of the underlying polynomial OI-algebra; see [1, Example 3.2]"},
                {TT "OIMonomialOrder => ", TT TO List, " for the Schreyer order induced by a list of elements of a free OI-module; see [1, Definition 4.2]"}
            }
        Text
            {\em References:}

            [1] M. Morrow and U. Nagel, {\it Computing Gröbner Bases and Free Resolutions of OI-Modules}, Preprint, arXiv:2303.06725, 2023.            
///

doc ///
    Key
        installBasisElements
        (installBasisElements,FreeOIModule,ZZ)
    Headline
        install basis elements in a specified width of a free OI-module
    Usage
        installBasisElements(F, n)
    Inputs
        F:FreeOIModule
        n:ZZ
    Description
        Text
            This method assigns the basis elements of the width @TT "n"@ component of @TT "F"@ to the appropriate @TO IndexedVariable@ corresponding to the basis symbol of @TT "F"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            e_(2,{1},1)
            e_(2,{1},2)
            e_(2,{2},1)
            e_(2,{2},2)
            x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
///

doc ///
    Key
        getGenerators
        (getGenerators,FreeOIModule)
    Headline
        get the generators of a free OI-module
    Usage
        getGenerators F
    Inputs
        F:FreeOIModule
    Outputs
        :List
    Description
        Text
            Returns the generators of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            getGenerators F
///

doc ///
    Key
        getFreeOIModule
        (getFreeOIModule,VectorInWidth)
    Headline
        get the free OI-module of an element
    Usage
        getFreeOIModule f
    Inputs
        f:VectorInWidth
    Outputs
        :FreeOIModule
    Description
        Text
            Returns the free OI-module in which the element @TT "f"@ lives.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installBasisElements(F, 4);
            f = x_(2,4)*e_(4,{3},1)+x_(1,3)^2*e_(4,{2,4},3)
            getFreeOIModule f
///

doc ///
    Key
        getWidth
        (getWidth,VectorInWidth)
    Headline
        get the width of an element of a free OI-module
    Usage
        getWidth f
    Inputs
        f:VectorInWidth
    Outputs
        :ZZ
    Description
        Text
            Returns the width of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installBasisElements(F, 4);
            f = x_(2,4)*e_(4,{3},1)+x_(1,3)^2*e_(4,{2,4},3)
            getWidth f
///

doc ///
    Key
        getRank
        (getRank,FreeOIModule)
    Headline
        get the rank of a free OI-module
    Usage
        getRank F
    Inputs
        F:FreeOIModule
    Outputs
        :ZZ
    Description
        Text
            Returns the rank of a free OI-module. Recall that the rank of a free OI-module $\bigoplus_{i=1}^s\mathbf{F}^{\text{OI}, d_i}$ is defined to be $s$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            getRank F
///

doc ///
    Key
        oiOrbit
        (oiOrbit,List,ZZ)
    Headline
        compute the n-orbit of a list of elements of a free OI-module
    Usage
        oiOrbit(L, n)
    Inputs
        L:List
        n:ZZ
    Outputs
        :List
    Description
        Text
            Returns the $n$-orbit of the list @TT "L"@ of @TO VectorInWidth@ objects.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)
            oiOrbit({b}, 4)
///

doc ///
    Key
        (symbol SPACE,FreeOIModuleMap,VectorInWidth)
    Headline
        apply a free OI-module map to an element
    Usage
        phi f
    Inputs
        phi:FreeOIModuleMap
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Evaluates @TT "phi"@ at @TT "f"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2)
            phi = C.dd_1
            G = getGenerators C_1
            phi G#0
            phi G#1
///

doc ///
    Key
        (net,FreeOIModule)
    Headline
        display a free OI-module
    Usage
        net F
    Inputs
        F:FreeOIModule
    Outputs
        :Net
    Description
        Text
            Displays the basis symbol, generator widths, generator shifts, underlying polynomial OI-algebra, and monomial order of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            net F
///

doc ///
    Key
        (net,FreeOIModuleMap)
    Headline
        display a free OI-module map
    Usage
        net phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :Net
    Description
        Text
            Displays the source module, target module, and generator images of a free OI-module map.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2);
            phi = C.dd_1;
            net phi
///

doc ///
    Key
        (net,ModuleInWidth)
    Headline
        display a component of a free OI-module in a specified width
    Usage
        net M
    Inputs
        M:ModuleInWidth
    Outputs
        :Net
    Description
        Text
            Displays information about a widthwise component of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            net F_5
///

doc ///
    Key
        (net,VectorInWidth)
    Headline
        display an element of a free OI-module
    Usage
        net f
    Inputs
        f:VectorInWidth
    Outputs
        :Net
    Description
        Text
            Displays an element of a free OI-module. Note: terms are displayed in descending order according to the monomial order of the free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2);
            net f
///

doc ///
    Key
        (toString,FreeOIModule)
    Headline
        display a free OI-module in condensed form
    Usage
        toString F
    Inputs
        F:FreeOIModule
    Outputs
        :String
    Description
        Text
            Displays the basis symbol, generator widths, and generator shifts of a free OI-module as a string.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            toString F
///

doc ///
    Key
        (symbol _,FreeOIModule,ZZ)
    Headline
        get the width n component of a free OI-module
    Usage
        F _ n
    Inputs
        F:FreeOIModule
        n:ZZ
    Outputs
        :ModuleInWidth
    Description
        Text
            Returns the module in width @TT "n"@ of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            F_5
///

doc ///
    Key
        isZero
        (isZero,FreeOIModule)
    Headline
        check if a free OI-module is zero
    Usage
        isZero F
    Inputs
        F:FreeOIModule
    Outputs
        :Boolean
    Description
        Text
            Checks if a free OI-module is the zero module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            G = makeFreeOIModule(d, {}, P);
            isZero F
            isZero G
///

doc ///
    Key
        (isZero,FreeOIModuleMap)
    Headline
        check if a free OI-module map is zero
    Usage
        isZero phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :Boolean
    Description
        Text
            Checks if a free OI-module map is the zero map.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {2}, P);
            installBasisElements(F, 2);
            b = e_(2,{1,2},1);
            C = oiRes({b}, 2)
            phi0 = C.dd_0
            phi1 = C.dd_1
            isZero phi0
            isZero phi1
///

doc ///
    Key
        (isZero,VectorInWidth)
    Headline
        check if an element of a free OI-module is zero
    Usage
        isZero f
    Inputs
        f:VectorInWidth
    Outputs
        :Boolean
    Description
        Text
            Checks if an element of a free OI-module is zero.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            isZero f
            isZero(f-f)
///

doc ///
    Key
        (degree,VectorInWidth)
    Headline
        get the degree of an element of a free OI-module
    Usage
        degree f
    Inputs
        f:VectorInWidth
    Outputs
        :ZZ
    Description
        Text
            Returns the degree of the lead term of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            degree f
///

doc ///
    Key
        (use,ModuleInWidth)
    Headline
        use a component of a free OI-module
    Usage
        use M
    Inputs
        M:ModuleInWidth
    Description
        Text
            This method invokes @TO use@ on the underlying polynomial ring of @TT "M"@. This is useful since distinct components of a free OI-module need not be modules over the same polynomial ring.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installBasisElements(F, 1);
            installBasisElements(F, 2);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2)
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3)
///

doc ///
    Key
        (terms,VectorInWidth)
    Headline
        get the terms of an element of a free OI-module
    Usage
        terms f
    Inputs
        f:VectorInWidth
    Outputs
        :List
    Description
        Text
            Returns the list of terms of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            terms f
    Caveat
        The list of terms need not be in order. To find the lead term of an element, see @TO (leadTerm,VectorInWidth)@.
///

doc ///
    Key
        (leadTerm,VectorInWidth)
    Headline
        get the lead term of an element of a free OI-module
    Usage
        leadTerm f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Returns the lead term of an element of a free OI-module according to the specified @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            leadTerm f
///

doc ///
    Key
        (leadMonomial,VectorInWidth)
    Headline
        get the lead monomial of an element of a free OI-module
    Usage
        leadMonomial f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Returns the lead monomial of an element of a free OI-module according to the specified @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            leadMonomial f
    Caveat
        Asking for the lead monomial of a zero element will cause an error.
///

doc ///
    Key
        (leadCoefficient,VectorInWidth)
    Headline
        get the lead coefficient of an element of a free OI-module
    Usage
        leadCoefficient f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Returns the lead coefficient of an element of a free OI-module according to the specified @TO OIMonomialOrder@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-5*x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            leadCoefficient f
///

doc ///
    Key
        (symbol *,Number,VectorInWidth)
    Headline
        multiply an element of a free OI-module by a number
    Usage
        n * f
    Inputs
        n:Number
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Multiplies @TT "f"@ by @TT "n"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-5*x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            22*f
///

doc ///
    Key
        (symbol +,VectorInWidth,VectorInWidth)
    Headline
        add two elements of a free OI-module
    Usage
        f + g
    Inputs
        f:VectorInWidth
        g:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Adds @TT "f"@ and @TT "g"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-5*x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            g = 5*x_(1,2)*e_(2,{2},1)
            f + g
///

doc ///
    Key
        (symbol *,RingElement,VectorInWidth)
    Headline
        multiply an element of a free OI-module by a ring element
    Usage
        r * f
    Inputs
        r:RingElement
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Multiplies @TT "f"@ by @TT "r"@ provided that @TT "r"@ belongs to the underlying polynomial ring of the component in which @TT "f"@ lives.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            f=x_(1,1)*e_(2,{1},1)+x_(2,1)*e_(2,{1},2)-x_(1,2)*e_(2,{2},1)-x_(2,2)*e_(2,{2},2)
            (x_(1,2)+x_(2,2)^2)*f
///

doc ///
    Key
        (symbol -,VectorInWidth)
    Headline
        multiply an element of a free OI-module by -1
    Usage
        - f
    Inputs
        f:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Flips the sign of an element of a free OI-module.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            -f
///

doc ///
    Key
        (symbol -,VectorInWidth,VectorInWidth)
    Headline
        subtract an element of a free OI-module from another
    Usage
        f - g
    Inputs
        f:VectorInWidth
        g:VectorInWidth
    Outputs
        :VectorInWidth
    Description
        Text
            Subtracts @TT "g"@ from @TT "f"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            g = x_(2,2)*x_(2,1)*e_(3,{1,3},2)+x_(2,1)*e_(3,{1,2},2)
            f - g
///

doc ///
    Key
        (isHomogeneous,VectorInWidth)
    Headline
        check if an element of a free OI-module is homogeneous
    Usage
        isHomogeneous f
    Inputs
        f:VectorInWidth
    Outputs
        :Boolean
    Description
        Text
            Checks if an element of a free OI-module is homogeneous, i.e., if every term has the same degree.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            f = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2)
            g = x_(2,2)*x_(2,1)*e_(3,{1,3},2)+x_(2,1)*e_(3,{1,2},2)
            isHomogeneous f
            isHomogeneous g
///

doc ///
    Key
        (isHomogeneous,FreeOIModuleMap)
    Headline
        checks if a free OI-module map is homogeneous
    Usage
        isHomogeneous phi
    Inputs
        phi:FreeOIModuleMap
    Outputs
        :Boolean
    Description
        Text
            Checks if a map of free OI-modules is homogeneous, i.e., if it preserves degrees of elements.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,2}, P);
            installBasisElements(F, 3);
            b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
            C = oiRes({b}, 2);
            phi = C.dd_1
            isHomogeneous phi
///

doc ///
    Key
        oiGB
        (oiGB,List)
        [oiGB,Verbose]
        [oiGB,Strategy]
    Headline
        compute a Gröbner basis for a submodule of a free OI-module
    Usage
        oiGB L
    Inputs
        L:List
    Outputs
        :List
    Description
        Text
            Given a list of elements @TT "L"@ belonging to a free OI-module $\mathbf{F}$, this method computes a Gröbner basis for the submodule generated by @TT "L"@ with respect to the monomial order of $\mathbf{F}$. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.

            The @TO Strategy@ option has the following permissible values:
        Code
            UL {
                {TT "Strategy => ", TT TO FastNonminimal, " for no post-processing of the basis"},
                {TT "Strategy => ", TT TO Minimize, " to minimize the basis after it is computed; see ", TO minimizeOIGB},
                {TT "Strategy => ", TT TO Reduce, " to reduce the basis after it is computed; see ", TO reduceOIGB}
            }
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installBasisElements(F, 1);
            installBasisElements(F, 2);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
            time oiGB {b1, b2}
///

doc ///
    Key
        minimizeOIGB
        (minimizeOIGB,List)
        [minimizeOIGB,Verbose]
    Headline
        minimize an OI-Gröbner basis
    Usage
        minimizeOIGB G
    Inputs
        G:List
    Outputs
        :List
    Description
        Text
            This method minimizes @TT "G"@ with respect to the Gröbner property, i.e., removes any elements whose leading monomial is OI-divisible by the leading monomial of another element of @TT "G"@. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installBasisElements(F, 1);
            installBasisElements(F, 2);
            installBasisElements(F, 3);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
            time B = oiGB {b1, b2}
            use F_3; b3 = x_(2,3)*x_(2,2)*x_(1,1)*e_(3,{2,3},3)-x_(2,1)*x_(1,2)^2*e_(3,{1,3},3);
            C = append(B, b3) -- dump in a redundant element
            minimizeOIGB C -- an element gets removed
///

doc ///
    Key
        reduceOIGB
        (reduceOIGB,List)
        [reduceOIGB,Verbose]
    Headline
        reduce an OI-Gröbner basis
    Usage
        reduceOIGB G
    Inputs
        G:List
    Outputs
        :List
    Description
        Text
            This method reduces @TT "G"@ as a Gröbner basis, i.e., ensures that no monomial of any element of @TT "G"@ is OI-divisible by the leading monomial of any other element of @TT "G"@. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installBasisElements(F, 1);
            installBasisElements(F, 2);
            use F_1; b1 = x_(2,1)*e_(1,{1},2)+x_(1,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(1,2)*e_(2,{2},2);
            time B = oiGB({b1, b2}, Strategy => FastNonminimal)
            minimizeOIGB B -- does not change the basis
            reduceOIGB B -- changes the basis
///

doc ///
    Key
        isOIGB
        (isOIGB,List)
        [isOIGB,Verbose]
    Headline
        check if a list of elements of a free OI-module forms a Gröbner basis
    Usage
        isOIGB L
    Inputs
        L:List
    Outputs
        :Boolean
    Description
        Text
            Checks if a @TT "L"@ forms a Gröbner basis for the submodule it generates. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1,2}, P);
            installBasisElements(F, 1);
            installBasisElements(F, 2);
            installBasisElements(F, 3);
            use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
            use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
            isOIGB {b1, b2}
            time B = oiGB {b1, b2}
            isOIGB B
///

doc ///
    Key
        oiSyz
        (oiSyz,List,Symbol)
        [oiSyz,Verbose]
        [oiSyz,Strategy]
    Headline
        compute a Gröbner basis for the syzygy module of a submodule of a free OI-module
    Usage
        oiSyz(G, d)
    Inputs
        G:List
        d:Symbol
    Outputs
        :List
    Description
        Text
            Given a Gröbner basis @TT "G"@ for a submodule $\mathbf{M}$ of a free OI-module $\mathbf{F}$, this method computes a Gröbner basis for the syzygy module of $\mathbf{M}$ with respect to the Schreyer order induced by @TT "G"@; see @TO OIMonomialOrder@.

            The new Gröbner basis lives in an appropriate free OI-module with basis symbol @TT "d"@.

            The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.

            The @TO Strategy@ option has the following permissible values:
        Code
            UL {
                {TT "Strategy => ", TT TO FastNonminimal, " for no post-processing of the basis"},
                {TT "Strategy => ", TT TO Minimize, " to minimize the basis after it is computed; see ", TO minimizeOIGB},
                {TT "Strategy => ", TT TO Reduce, " to reduce the basis after it is computed; see ", TO reduceOIGB}
            }
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            B = oiGB {b}
            oiSyz(B, d)
///

doc ///
    Key
        OIResolution
    Headline
        the class of all resolutions of submodules of free OI-modules
    Description
        Text
            This type implements free resolutions of submodules of free OI-modules. To make an @TT "OIResolution"@ object, use @TO oiRes@. To verify that an OI-resolution is a complex, use @TO isComplex@. To get the $n$th differential in an OI-resolution @TT "C"@, use @TT "C.dd_n"@.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1)
            C.dd_0
///

doc ///
    Key
        (describe,OIResolution)
    Headline
        describe an OI-resolution
    Usage
        describe C
    Inputs
        C:OIResolution
    Outputs
        :Net
    Description
        Text
            Displays the free OI-modules and differentials of an OI-resolution.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1);
            describe C
///

doc ///
    Key
        (net,OIResolution)
    Headline
        display an OI-resolution
    Usage
        net C
    Inputs
        C:OIResolution
    Outputs
        :Net
    Description
        Text
            Displays the generator widths and degree shifts of the free OI-modules in an OI-resolution.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1);
            net C
///

doc ///
    Key
        (symbol _,OIResolution,ZZ)
    Headline
        get a module of an OI-resolution is specified homological degree
    Usage
        C _ n
    Inputs
        C:OIResolution
        n:ZZ
    Outputs
        :FreeOIModule
    Description
        Text
            Returns the free OI-module of $C$ in homological degree $n$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 1);
            C_0
            C_1
///

doc ///
    Key
        oiRes
        (oiRes,List,ZZ)
        [oiRes,Verbose]
        [oiRes,Strategy]
        [oiRes,TopNonminimal]
    Headline
        compute an OI-resolution
    Usage
        oiRes(L, n)
    Inputs
        L:List
        n:ZZ
    Outputs
        :OIResolution
    Description
        Text
            Computes an OI-resolution of the submodule generated by @TT "L"@ out to homological degree @TT "n"@. If @TT "L"@ consists of homogeneous elements, then the resulting resolution will be minimal out to homological degree $n-1$. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.

            The @TO Strategy@ option has the following permissible values:
        Code
            UL {
                {TT "Strategy => ", TT TO FastNonminimal, " for no post-processing of the Gröbner basis computed at each step"},
                {TT "Strategy => ", TT TO Minimize, " to minimize the Gröbner basis after it is computed at each step; see ", TO minimizeOIGB},
                {TT "Strategy => ", TT TO Reduce, " to reduce the Gröbner basis after it is computed at each step; see ", TO reduceOIGB}
            }
        Text
            The @TO TopNonminimal@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants the Gröbner basis in homological degree $n-1$ to be minimized. Therefore, use @TT "TopNonminimal => true"@ for no minimization of the basis in degree $n-1$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 2, TopNonminimal => true)
///

doc ///
    Key
        TopNonminimal
    Headline
        option for minimizing the top Gröbner basis of an OI-resolution
    Description
        Text
            Used as an optional argument in the @TO oiRes@ method. The @TT "TopNonminimal"@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants the Gröbner basis in homological degree $n-1$ to be minimized. Therefore, use @TT "TopNonminimal => true"@ for no minimization of the basis in degree $n-1$.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 2, TopNonminimal => true)
///

doc ///
    Key
        isComplex
        (isComplex,OIResolution)
        [isComplex,Verbose]
    Headline
        verify that an OI-resolution is a complex
    Usage
        isComplex C
    Inputs
        C:OIResolution
    Outputs
        :Boolean
    Description
        Text
            This method verifies that an OI-resolution is indeed a complex. The @TO Verbose@ option must be either @TT "true"@ or @TT "false"@, depending on whether one wants debug information printed.
        Example
            P = makePolynomialOIAlgebra(2, x, QQ);
            F = makeFreeOIModule(e, {1,1}, P);
            installBasisElements(F, 2);
            b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
            time C = oiRes({b}, 2, TopNonminimal => true)
            isComplex C
///