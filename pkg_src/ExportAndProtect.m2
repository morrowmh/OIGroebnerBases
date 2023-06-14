export {
    -- Types
        -- From PolynomialOIAlgebra.m2
        "PolynomialOIAlgebra",

        -- From FreeOIModule.m2
        "ModuleInWidth", "VectorInWidth",
    
    -- Keys
        -- From PolynomialOIAlgebra.m2
        "ColUpRowUp", "ColUpRowDown", "ColDownRowUp", "ColDownRowDown",
        "RowUpColUp", "RowUpColDown", "RowDownColUp", "RowDownColDown",
    
    -- Methods
        -- From PolynomialOIAlgebra.m2
        "makePolynomialOIAlgebra",

        -- From FreeOIModule.m2
        "makeFreeOIModule", "installBasisElements"
}

scan({
    -- Keys
        -- From OIMap.2
        targWidth, img,

        -- From PolynomialOIAlgebra.m2
        varRows, varSym, baseField, varOrder, algebras, maps,

        -- From FreeOIModule.m2
        basisSym, genWidths, degShifts, polyOIAlg, monOrder, modules, wid, rawMod, freeOIMod,
    
    -- Options
        -- From PolynomialOIAlgebra.m2
        VariableOrder,

        -- From FreeOIModule.m2
        DegreeShifts
}, protect)