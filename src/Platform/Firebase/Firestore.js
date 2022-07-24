import {collection, getDoc, getDocs, addDoc, doc, onSnapshot} from "firebase/firestore"

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

export function getDoc_(db, path, id) {
		console.log("getDoc", path, id);
		return getDoc(doc(db, path, id)).then((r) => {
			const res = r.data();
			res.id = r.id
			console.log("got doc: ", res)
			return res;
		});
}
export function getDocs_(db, path) {
		return getDocs(collection(db, path))
			.then((snapshot) => {
				const res = snapshot.docs.map((d) => {
					const data = d.data();
					data.id = d.id
					return data
				})
				console.log("got docs: ", res)
				return res;
			});
}

export function observeDoc_(db, path, id, pub) {
	onSnapshot(doc(db, path, id), (d) => {
		const toPublish = doc.data();
		toPublish.id = id
		console.log('publishing', chan, toPublish);
		pub(toPublish)();
	})
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
