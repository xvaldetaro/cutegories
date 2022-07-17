// Initialize Cloud Firestore and get a reference to the service
export const firestoreDb = (app) => () =>
	import("firebase/firestore").then(({ getFirestore }) => {
		console.log("get firestore");
		return getFirestore(app);
	});

const PLAYERS = "players";

export const addPlayer = (db) => (player) => () =>
	import("firebase/firestore").then(({ collection, addDoc }) => {
		console.log("addPlayer", player);
		return addDoc(collection(db, PLAYERS), player);
	});
export const getPlayer = (db) => (player) => () =>
	import("firebase/firestore").then(({ getDoc, doc }) => {
		console.log("getPlayer", db, PLAYERS, player);
		return getDoc(doc(db, PLAYERS, player)).then((r) => r.data());
	});
export const getPlayers = (db) => () =>
	import("firebase/firestore").then(({ collection, getDocs }) => {
		console.log("getPlayers", db, PLAYERS);
		return getDocs(collection(db, PLAYERS))
			.then((snapshot) => snapshot.docs.map((d) => d.data()));
	});

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
