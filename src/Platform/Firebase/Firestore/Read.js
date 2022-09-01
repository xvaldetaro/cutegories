import {documentId, where, orderBy, query, collection, getDoc, getDocs, doc, onSnapshot, collectionGroup} from "firebase/firestore"

export function getDoc_(db, path, id, just, nothing) {
  console.log("getDoc", path, id);
  return getDoc(doc(db, path, id)).then((r) => {
    if (r.exists()) {
      const res = r.data();
      if (res) {
        res.id = id
      } else {
        return nothing
      }
      console.log("got doc: ", res)
      return just(res);
    } else {
      return nothing
    }
  });
}
export function queryDocs_(db, queryDesc) {
  return getDocs(buildQuery(db, queryDesc))
    .then((snapshot) => {
      const res = snapshot.docs.map((d) => {
        if (d.exists()) {
          const data = d.data();
          data.id = d.id;
          data.ref = d.ref;
          return data
        } else {
          throw Exception(`document doesn't exist in DocumentSnapshot!!!`)
        }
      })
      console.log(`queryDocs ${queryDesc.path} returned: ${res.length} items`)
      return res;
    }, (e) => {
      console.log(e);
      throw e
    });
}

const docObserversByPath = {}
export function observeDoc_(db, path, id, onNext, onError, nothing, just) {
  console.log(`observeDoc_. path:${path}, id:${id}`);
  const combinedPath = path + id
  const currentObservers = docObserversByPath[combinedPath] || 0;
  docObserversByPath[combinedPath] = currentObservers + 1;
  if (currentObservers > 0) {
    console.warn(`${currentObservers} consecutive doc observers at ${combinedPath}`);
  }
	const unsub =  onSnapshot(
		doc(db, path, id),
		(d) => {
      try {
        console.log(`onNext doc. path:${path} id:${id} ${d}`);
        if (d.exists()) {
          const toPublish = d.data();
          toPublish.id = id
          onNext(just(toPublish))();
        } else {
          onNext(nothing)
        }
      } catch (e) {
        const errorStr = `Error1 in observeDoc_. code:${e.code}. msg:${e.message}`
        console.log(errorStr)
        onError(errorStr)();
      }
		},
		(e) => {
			const errorStr = `Error in observeDoc_. code:${e.code}. msg:${e.message}`
			console.log(errorStr)
			onError(errorStr)();
		},
		() => {
			onCompleteEffect()
		},
	)
  return () => {
    unsub();
    console.log(`unsubscribing from observeDoc_. path:${path}, id:${id}`);
    docObserversByPath[combinedPath] = docObserversByPath[combinedPath] - 1;
  }
}

const collectionObserversByPath = {}
export function observeCollection_(db, queryDesc, onNext, onError) {
  const path = queryDesc.path;
  const currentObservers = collectionObserversByPath[path] || 0;
  collectionObserversByPath[path] = currentObservers + 1;
  if (currentObservers > 0) {
    console.warn(`${currentObservers} consecutive collection ${path} observers`);
  }
  console.log(`observeCollection_. path:${path}`);

	const unsub = onSnapshot(
    buildQuery(db, queryDesc),
		(qs) => {
      try {
        console.log(`qs. path:${path} size:${qs.size} isEmpty:${qs.empty} ${qs}`);
        const out = [];
        qs.forEach((doc) => {
          const data = doc.data()
          if (data) {
            data.id = doc.id;
            out.push(data);
          }
        })
        onNext(out)();
      } catch (e) {
        const errorStr = `Error1 in observeCollection_. code:${e.code}. msg:${e.message}`;
        console.log(errorStr);
        onError(errorStr)();
      }
		},
		(e) => {
			const errorStr = `Error in observeCollection_. code:${e.code}. msg:${e.message}`
			console.log(errorStr)
			onError(errorStr)();
		},
	)
  return () => {
    unsub();
    console.log(`unsubscribing from observeCollection_. path:${path}`);
    collectionObserversByPath[path] = collectionObserversByPath[path] - 1;
  }
}

/*
Query example:

{
  "qtype": {"collection": true},
  "path": "asdfasdf/",
  "clauses": [
    {
      "type": "where",
      "expression": [
        {
          "docId": true
        },
        "in",
        {
          "multiple": [
            "asd",
            "ddd"
          ]
        }
      ]
    },
    {
      "type": "where",
      "expression": [
        {
          "field": "myField"
        },
        "==",
        {
          "single": "sss"
        }
      ]
    },
    {
      "type": "orderBy",
      "expression": [
        {
          "field": "field2"
        },
        "asc"
      ]
    }
  ]
}
 */
function buildQuery(db, desc) {
  const clauses = desc.clauses.map(buildClause);
  try {
    let collectionObj;
    if (desc.qtype == "collection") {
      collectionObj = collection(db, desc.path);
    } else if (desc.qtype == "group"){
      collectionObj = collectionGroup(db, desc.path);
    } else {
      throw Exception("invalid query qtype:", desc.qtype)
    }
    if (clauses.length == 0) {
      return collectionObj;
    } else {
      return query(collectionObj, ...clauses);
    }
  } catch (e) {
    console.log("error building query:", e);
    console.log(desc);
    throw e;
  }
}

function buildClause(desc) {
  if (desc.type == "where") {
    return buildWhere(desc.expression);
  } else if (desc.type == "orderBy") {
    return buildOrderBy(desc.expression);
  } else {
    throw Exception("Invalid clause type: " + desc.type)
  }
}

function buildOrderBy(expr) {
  const [l, dir] = expr;
  let left = leftHand(l);
  if (left == null) {
    throw Exception("Invalid LeftHand in OrderBy clause: " + expr)
  }
  return orderBy(left, dir);
}

function leftHand(l) {
  if (l.field && l.field.length > 0) {
    return l.field;
  } else if (l.docId) {
    return documentId();
  } else {
    return null;
  }
}

function buildWhere(expr) {
  const [l, op, r] = expr;
  let left = leftHand(l);
  if (left == null) {
    throw Exception("Invalid LeftHand in where clause: " + expr)
  }

  let right;
  if (r.single && r.single.length > 0) {
    right = r.single;
  } else if (r.multiple && r.multiple.length > 0) {
    right = r.multiple
  } else {
    throw Exception("Invalid RightHand in where clause: " + expr)
  }
  return where(left, op, right);
}
