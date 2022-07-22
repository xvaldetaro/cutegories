const firebaseConfig = {
  apiKey: "AIzaSyDliVUsdrwdYH_afnPF_xD2MmVZyqPMUlg",
  authDomain: "picspotter-e7f69.firebaseapp.com",
  projectId: "picspotter-e7f69",
  storageBucket: "picspotter-e7f69.appspot.com",
  messagingSenderId: "298746103875",
  appId: "1:298746103875:web:26bbd2096529ee358d2cf6",
  measurementId: "G-0H9VN0WFVR"
};

// Initialize Firebase
export const firebaseApp = () =>import("firebase/app").then(({ initializeApp }) =>
	initializeApp(firebaseConfig)
);