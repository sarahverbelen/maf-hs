module Agreement.Agreement where 

import Data.Map

-- an agreement is a set of mappings of variables to the property they have to agree on
type Agreement p = Map Ide 