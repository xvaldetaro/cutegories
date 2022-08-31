import {deleteDoc, arrayUnion, arrayRemove, collection, updateDoc, setDoc, addDoc, doc} from "firebase/firestore"

export const removeUndefineds = (obj) => JSON.parse(JSON.stringify(obj));

export function addDoc_(db, path, docObj) {
		console.log("addDoc: ", path);
		console.log(docObj);
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
  console.log(`updateDoc_: ${path} ${id}`);
  console.log(objectFragment);
  console.log(arrayUpdates);
  const aus = processArrayUpdates(arrayUpdates);
  const final = {...objectFragment, ...aus};
  return updateDoc(doc(db, path, id), final).then((x) => {
  })
}

export function deleteDoc_(db, path, id) {
		console.log("deleteDoc", path, id);
		return deleteDoc(doc(db, path, id));
}

export function setDoc_(db, path, id, docObj) {
		console.log("setDoc", path, id);
		console.log(docObj);
		return setDoc(doc(db, path, id), docObj);
}