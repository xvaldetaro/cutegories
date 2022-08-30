module Platform.Firebase.Firestore.Query where

import Prelude

import Foreign (Foreign)
import Simple.JSON as JSON

type Query = { qtype :: QueryType, path :: String, ancestor :: Int, clauses :: Array Clause}

data QueryType
  = Collection
  | Group

data LeftHand
  = Field String
  | DocId

data RightHand
  = Single String
  | Multiple (Array String)

data Clause
  = Where LeftHand Operator RightHand
  | OrderBy LeftHand Direction

data Operator
  = In
  | Equals
  | NotEqual

data Direction = Asc | Desc

toJs :: Query -> Foreign
toJs {qtype, path, clauses, ancestor} =
  JSON.writeImpl { qtype: showQType qtype, path, clauses: jsClauses clauses, ancestor }
  where
  showQType Collection = "collection"
  showQType Group = "group"

jsClauses :: Array Clause -> Array Foreign
jsClauses arr = go <$> arr
  where
  go (Where left op right) = JSON.writeImpl
    { type: "where", expression: [jsLeftHand left, JSON.writeImpl $ jsOp op, jsRightHand right] }
  go (OrderBy left dir) = JSON.writeImpl
    { type: "orderBy", expression: [jsLeftHand left, jsDir dir] }

jsLeftHand :: LeftHand -> Foreign
jsLeftHand (Field str) = JSON.writeImpl {field: str}
jsLeftHand DocId = JSON.writeImpl {docId: true}

jsRightHand :: RightHand -> Foreign
jsRightHand (Single str) = JSON.writeImpl {single: str}
jsRightHand (Multiple arr) = JSON.writeImpl {multiple: arr}

jsOp :: Operator -> Foreign
jsOp = JSON.writeImpl <<< go
  where
  go In = "in"
  go Equals = "=="
  go NotEqual = "!="

jsDir :: Direction -> Foreign
jsDir = JSON.writeImpl <<< go
  where
  go Asc = "asc"
  go Desc = "desc"