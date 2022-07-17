// Import the functions you need from the SDKs you need
import { initializeApp } from "firebase/app";
// import { getAnalytics } from "firebase/analytics";
import { getFirestore, collection, getDocs } from 'firebase/firestore/lite';
// TODO: Add SDKs for Firebase products that you want to use
// https://firebase.google.com/docs/web/setup#available-libraries

// Your web app's Firebase configuration
// For Firebase JS SDK v7.20.0 and later, measurementId is optional
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
const app = initializeApp(firebaseConfig);
// const analytics = getAnalytics(app);

const db = getFirestore(app);

// Get a list of cities from your database
async function getUsers(db) {
  const citiesCol = collection(db, 'users');
  const citySnapshot = await getDocs(citiesCol);
  const cityList = citySnapshot.docs.map(doc => doc.data());
  return cityList;
}

const aa = await getUsers(db)

console.log(aa)