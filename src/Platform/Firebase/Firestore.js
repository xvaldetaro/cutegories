import {arrayUnion, arrayRemove, documentId, where, orderBy, query, collection, getDoc, getDocs, updateDoc, setDoc, addDoc, doc, onSnapshot} from "firebase/firestore"

// Initialize Cloud Firestore and get a reference to the service
export const firestoreDb = (app) => () =>
	import("firebase/firestore").then(({ getFirestore }) => {
		console.log("get firestore");
		return getFirestore(app);
	});

export function addDoc_(db, path, docObj) {
		console.log("addDoc", docObj);
		return addDoc(collection(db, path), docObj);
}

function processArrayUpdates(aus) {
  const objFragment = {}
  aus.forEach((au) => {
    if (au.elements.length < 1) {
      throw Exception("processArrayUpdates got 0 elements");
    }
    if (au.op == "union") {
      objFragment[au.field] = arrayUnion(...au.elements);
    } else if (au.op == "remove") {
      objFragment[au.field] = arrayRemove(...au.elements);
    } else {
      throw Exception("processArrayUpdates invalid op " + au.op);
    }
  })
  return objFragment;
}

export function updateDoc_(db, path, id, objectFragment, arrayUpdates) {
  const aus = processArrayUpdates(arrayUpdates);
  const final = {...objectFragment, ...aus};
  return updateDoc(doc(db, path, id), final).then((x) => {
  })
}

export function setDoc_(db, path, id, docObj) {
		console.log("setDoc", path, id, docObj);
		return setDoc(doc(db, path, id), docObj);
}

export function getDoc_(db, path, id) {
		console.log("getDoc", path, id);
		return getDoc(doc(db, path, id)).then((r) => {
			const res = r.data();
      if (res) {
        data.id = d.id
      }
			console.log("got doc: ", res)
			return res;
		});
}
export function getDocs_(db, path) {
		return getDocs(collection(db, path))
			.then((snapshot) => {
				const res = snapshot.docs.map((d) => {
					const data = d.data();
          if (data) {
            data.id = d.id
          }
					return data
				})
				console.log("got docs: ", res)
				return res;
			});
}

export function observeDoc_(db, path, id, onNext, onError, onEmpty, onCompleteEffect) {
	return onSnapshot(
		doc(db, path, id),
		(d) => {
      try {
        console.log(`onNext doc. path:${path} id:${id} ${d}`);
        const toPublish = d.data();
        if (toPublish) {
          toPublish.id = id
          onNext(toPublish)();
        } else {
          onEmpty()
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
}

function getConstraints(ctrs) {
  const constraints = []
  ctrs.forEach((c) => {
    const type = c.type;
    const args = c.args;
    if (type == "orderBy") {
      constraints.push(orderBy(args[0], args[1]))
    } else if (type == "whereDocIds") {
      if (args.length == 0) {
        throw Exception("whereDocIds constraint got zero ids in args")
      }
      constraints.push(where(documentId(), "in", args))
    } else {
      const errorStr = `Invalid type ${c.type}`;
      console.log(errorStr);
      throw Exception(errorStr)
    }
  });
  return constraints;
}

export function observeCollection_(db, path, ctrs, onNext, onError) {
  let constraints;
  try {
    constraints = getConstraints(ctrs);
  } catch (e) {
    const errorStr = `Error processing constraints in observeCollection_: ${e.message}`;
    console.log(errorStr);
    onError(errorStr)();
  }
	return onSnapshot(
		query(collection(db, path), ...constraints),
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
}

// export const claimPlayer = (auth) => (db) => (player) => (player) => () =>
// 	import("firebase/firestore").then(({ updateDoc, doc }) => {
// 		const update = {};
// 		if (!auth.currentPlayer) {
// 			return Promise.reject(new Error("No current player"));
// 		}
// 		update[player] = auth.currentPlayer.uid;
// 		console.log("claimPlayer", player, player);
// 		return updateDoc(doc(db, PLAYERS, player), update);
// 	});

export const removeUndefineds = (obj) => JSON.parse(JSON.stringify(obj));

// export const listenToRemoteChannelChanges = (db) => (chan) => (pub) => () =>
// 	import("firebase/firestore").then(({ doc, onSnapshot }) =>
// 		onSnapshot(doc(db, PLAYERS, chan), (doc) => {
// 			if (doc.metadata.hasPendingWrites) {
// 				// ignore, local
// 			} else {
// 				const toPublish = doc.data();
// 				console.log('publishing', chan, toPublish);
// 				pub(toPublish)();
// 			}
// 		})
// 	);

// export const sendMyPointsAndPenaltiesToFirebaseImpl =
// 	(db) => (auth) => (chan) => (keyPoints) => (keyPenalty) => (points) => (penalties) => () =>
// 		import("firebase/firestore").then(({ updateDoc, doc }) => {
// 			const update = {};
// 			if (!auth.currentPlayer) {
// 				return Promise.reject(new Error("No current player"));
// 			}
// 			update[keyPoints] = points;
// 			update[keyPenalty] = penalties;
// 			console.log("sendMyPointsAndPenaltiesToFirebaseImpl", chan, update);
// 			return updateDoc(doc(db, PLAYERS, chan), update);
// 		});
