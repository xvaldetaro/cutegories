module Platform.Firebase.Firestore.QL where

import Platform.Firebase.Firestore.Query (Clause(..), Direction, LeftHand(..), Operator(..), Query, QueryType(..), RightHand(..))

collection :: String -> Array Clause -> Query
collection path clauses = { qtype: Collection , ancestor: 0, path , clauses }

group :: String -> Array Clause -> Query
group path clauses = { qtype: Group , path, ancestor: 0, clauses }

whereFieldEquals :: String -> String -> Clause
whereFieldEquals field value = Where (Field field) Equals (Single value)

whereFieldIn :: String -> Array String -> Clause
whereFieldIn field arr = Where (Field field) In (Multiple arr)

whereDocIdIn :: Array String -> Clause
whereDocIdIn arr = Where DocId In (Multiple arr)

orderByField :: String -> Direction -> Clause
orderByField field dir = OrderBy (Field field) dir

ancestor :: Int -> Query -> Query
ancestor n q = q { ancestor = n }