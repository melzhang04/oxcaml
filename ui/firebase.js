import { initializeApp } 
  from "https://www.gstatic.com/firebasejs/10.7.1/firebase-app.js";
import {
  getAuth,
  signInAnonymously,
  signOut,
  onAuthStateChanged
} from "https://www.gstatic.com/firebasejs/10.7.1/firebase-auth.js";
import {
  getFirestore,
  doc,
  setDoc,
  getDoc,
  deleteDoc,
  onSnapshot,
  addDoc,
  collection,
  serverTimestamp,
  query,
  orderBy,
  limit,
  getDocs,
  updateDoc,
  arrayUnion
} from "https://www.gstatic.com/firebasejs/10.7.1/firebase-firestore.js";

const firebaseConfig = {
  apiKey: "AIzaSyBiZ3plu8OsjrnR_c3diG57nbbHrRZuSeA",
  authDomain: "ocaml-battleship.firebaseapp.com",
  databaseURL: "https://ocaml-battleship-default-rtdb.firebaseio.com",
  projectId: "ocaml-battleship",
  storageBucket: "ocaml-battleship.firebasestorage.app",
  messagingSenderId: "225330041738",
  appId: "1:225330041738:web:0005d4d303f8f48456ee73",
  measurementId: "G-YRKR08M63M"
};

const app = initializeApp(firebaseConfig);
const auth = getAuth(app);
const db = getFirestore(app);

export function signInGuest() {
  return signInAnonymously(auth)
    .then((cred) => {
      console.log("Signed in as guest:", cred.user.uid);
      return cred.user.uid;
    })
    .catch((err) => {
      console.error("signInGuest error:", err);
      throw err;
    });
}

export function signOutUser() {
  return signOut(auth)
    .then(() => {
      console.log("Signed out successfully");
    })
    .catch((err) => {
      console.error("signOut error:", err);
      throw err;
    });
}

export function getCurrentUid() {
  return auth.currentUser ? auth.currentUser.uid : null;
}

function encodeMove(row, col, player) {
  return `${row},${col},${player}`;
}

export async function requestQuickMatch(onMatched) {
  const uid = getCurrentUid();
  if (!uid) {
    console.error("requestQuickMatch: no current uid");
    return;
  }
  const q = query(
    collection(db, "matchQueue"),
    orderBy("timestamp"),
    limit(1)
  );

  const snap = await getDocs(q);

  if (!snap.empty) {
    const docSnap = snap.docs[0];
    const opponentUid = docSnap.id;

    if (opponentUid !== uid) {
      await deleteDoc(doc(db, "matchQueue", opponentUid));

      const seed = Math.floor(Math.random() * 1_000_000);
      const gameRef = await addDoc(collection(db, "games"), {
        p1: opponentUid,
        p2: uid,
        seed: seed,
        moves: [],
        createdAt: serverTimestamp()
      });

      const gameId = gameRef.id;
      console.log("Matched as P2 in game:", gameId);

      onMatched(`${gameId}|P2`);
      return;
    }
  }

  await setDoc(doc(db, "matchQueue", uid), {
    uid,
    timestamp: serverTimestamp()
  });

  console.log("Joined queue as P1:", uid);

  const pollInterval = setInterval(async () => {
    const queueDoc = await getDoc(doc(db, "matchQueue", uid));

    if (!queueDoc.exists()) {
      const gq = query(
        collection(db, "games"),
        orderBy("createdAt", "desc"),
        limit(20)
      );
      const gamesSnap = await getDocs(gq);

      let found = null;
      gamesSnap.forEach((g) => {
        const data = g.data();
        if (data.p1 === uid || data.p2 === uid) {
          found = { id: g.id, data };
        }
      });

      if (found) {
        clearInterval(pollInterval);
        const role = (found.data.p1 === uid) ? "P1" : "P2";
        console.log("Matched as", role, "in game:", found.id);
        onMatched(`${found.id}|${role}`);
      }
    }
  }, 500);
}
export function subscribeGame(gameId, onUpdate) {
  const gameRef = doc(db, "games", gameId);

  const unsub = onSnapshot(gameRef, (snap) => {
    if (!snap.exists()) return;
    const data = snap.data();
    const moves = data.moves || [];
    const joined = moves.join(";");
    onUpdate(joined);
  });

  return unsub;
}

export async function sendMove(gameId, row, col, player) {
  const gameRef = doc(db, "games", gameId);
  const moveStr = encodeMove(row, col, player);
  await updateDoc(gameRef, {
    moves: arrayUnion(moveStr)
  });
  console.log("Sent move:", moveStr, "to game:", gameId);
}

export async function setPlayerShips(gameId, player, shipsJson) {
  const gameRef = doc(db, "games", gameId);
  const fieldName = player === "P1" ? "p1Ships" : "p2Ships";
  await updateDoc(gameRef, {
    [fieldName]: shipsJson
  });
  console.log(`Set ships for ${player} in game ${gameId}`);
}

export async function markPlayerReady(gameId, player) {
  const gameRef = doc(db, "games", gameId);
  const fieldName = player === "P1" ? "p1Ready" : "p2Ready";
  await updateDoc(gameRef, {
    [fieldName]: true
  });
  console.log(`Marked ${player} ready in game ${gameId}`);
}

export function subscribeReadyStatus(gameId, onReady) {
  const gameRef = doc(db, "games", gameId);
  
  const unsub = onSnapshot(gameRef, (snap) => {
    if (!snap.exists()) return;
    const data = snap.data();
    const p1Ready = data.p1Ready || false;
    const p2Ready = data.p2Ready || false;
    
    if (p1Ready && p2Ready) {
      onReady("BOTH_READY");
    }
  });
  
  return unsub;
}

export async function getShips(gameId, callback) {
  const gameRef = doc(db, "games", gameId);
  const snap = await getDoc(gameRef);

  if (!snap.exists()) {
    console.error("getShips: game not found", gameId);
    callback("");
    return;
  }

  const data = snap.data();
  const p1Ships = data.p1Ships || "[]";
  const p2Ships = data.p2Ships || "[]";
  const joined = `${p1Ships};${p2Ships}`;
  callback(joined);
}

window.firebaseBindings = {
  signInGuest,
  signOutUser,
  getCurrentUid,
  requestQuickMatch,
  subscribeGame,
  sendMove,
  setPlayerShips,
  markPlayerReady,
  subscribeReadyStatus,
  getShips
};