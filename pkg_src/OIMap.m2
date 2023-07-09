-- Should be of the form {targWidth => ZZ, img => List}
OIMap = new Type of HashTable

net OIMap := f -> "Source: [" | net(#f.img) | "] Target: [" | net f.targWidth | "]" || "Image: " | net f.img

-- Evaluate an OI-map f at an integer n
OIMap ZZ := (f, n) -> f.img#(n - 1)

-- Make a new OIMap
-- Args: n = ZZ, L = List
makeOIMap := (n, L) -> new OIMap from {targWidth => n, img => L}

-- Get the OI-maps between two widths
-- Args: m = ZZ, n = ZZ
getOIMaps := (m, n) -> (
    if n < m then return {};

    sets := subsets(1..n, m);
    for i to #sets - 1 list makeOIMap(n, sets#i)
)

-- Compose two OI-maps
OIMap OIMap := (f, g) -> makeOIMap(f.targWidth, for i from 1 to #g.img list f g i)