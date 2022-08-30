// Initialize Cloud Firestore and get a reference to the service
export const firestoreDb = (app) => () =>
	import("firebase/firestore").then(({ getFirestore }) => {
		console.log("get firestore");
		return getFirestore(app);
	});
