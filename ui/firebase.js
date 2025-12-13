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
  arrayUnion,
  runTransaction
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

let tabId = sessionStorage.getItem('battleship_tab_id');
if (!tabId) {
  tabId = `tab_${Date.now()}_${Math.random().toString(36).substring(2, 15)}`;
  sessionStorage.setItem('battleship_tab_id', tabId);
}

function getUniquePlayerId() {
  const uid = getCurrentUid();
  if (!uid) return null;
  return `${uid}_${tabId}`;
}

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

  const playerId = getUniquePlayerId();
  if (!playerId) {
    console.error("requestQuickMatch: no player id");
    return;
  }

  console.log("Starting matchmaking for:", playerId);

  const matchResult = await runTransaction(db, async (transaction) => {
    const queueRef = collection(db, "matchQueue");
    const queueSnap = await getDocs(query(queueRef, orderBy("timestamp"), limit(50)));

    let opponentDoc = null;

    for (const docSnap of queueSnap.docs) {
      const opponentPlayerId = docSnap.id;

      if (opponentPlayerId === playerId) {
        continue;
      }

      opponentDoc = docSnap;
      break;
    }

    if (opponentDoc) {
      const opponentPlayerId = opponentDoc.id;
      const opponentData = opponentDoc.data();
      
      console.log("Found opponent:", opponentPlayerId);
      
      const seed = Math.floor(Math.random() * 1_000_000);
      const gameRef = doc(collection(db, "games"));

      transaction.set(gameRef, {
        p1: opponentPlayerId,
        p2: playerId,
        p1Uid: opponentData.uid,
        p2Uid: uid,
        seed: seed,
        moves: [],
        status: "ACTIVE",
        p1Ships: "[]",
        p2Ships: "[]",
        p1Ready: false,
        p2Ready: false,
        createdAt: serverTimestamp()
      });

      transaction.delete(doc(db, "matchQueue", opponentPlayerId));

      console.log("Created match as P2 in game:", gameRef.id);
      return { gameId: gameRef.id, role: "P2" };
    }

    transaction.set(doc(db, "matchQueue", playerId), {
      playerId,
      uid,
      timestamp: serverTimestamp()
    });

    console.log("No opponent found, joining queue");
    return null;
  });

  if (matchResult) {
    onMatched(`${matchResult.gameId}|${matchResult.role}`);
    return;
  }

  console.log("Listening for match as P1...");
  
  let hasMatched = false;
  let unsubQueue = null;
  let pollTimeout = null;

  const checkForMatch = async () => {
    if (hasMatched) return;

    const gamesQuery = query(
      collection(db, "games"),
      orderBy("createdAt", "desc"),
      limit(20)
    );
    const gamesSnap = await getDocs(gamesQuery);

    for (const gameDoc of gamesSnap.docs) {
      const data = gameDoc.data();
      const isRecent = data.createdAt &&
        (Date.now() - data.createdAt.toMillis()) < 30000;
      const isActive = data.status === "ACTIVE";

      if (isActive && isRecent && data.p1 === playerId) {
        hasMatched = true;
        if (unsubQueue) unsubQueue();
        if (pollTimeout) clearTimeout(pollTimeout);
        console.log("Matched as P1 in game:", gameDoc.id);
        onMatched(`${gameDoc.id}|P1`);
        return;
      }
    }
  };

  await checkForMatch();
  if (hasMatched) return;

  unsubQueue = onSnapshot(doc(db, "matchQueue", playerId), async (queueSnap) => {
    if (!queueSnap.exists() && !hasMatched) {
      console.log("Queue entry deleted - checking for match");
      await checkForMatch();
    }
  });

  pollTimeout = setTimeout(() => {
    if (!hasMatched) {
      console.log("Matchmaking timeout - removing from queue");
      if (unsubQueue) unsubQueue();
      deleteDoc(doc(db, "matchQueue", playerId)).catch(() => {});
    }
  }, 60000);
}

export function subscribeGame(gameId, onUpdate) {
  const gameRef = doc(db, "games", gameId);
  
  let lastProcessedStatus = null;

  const unsub = onSnapshot(gameRef, (snap) => {
    if (!snap.exists()) return;
    const data = snap.data();

    const status = data.status || "ACTIVE";

    if (status === "RESET") {
      const resetBy = data.resetBy || "UNKNOWN";
      const message = `__RESET__:${resetBy}`;
      
      if (lastProcessedStatus !== message) {
        lastProcessedStatus = message;
        onUpdate(message);
      }
      return;
    }

    if (status === "FORFEIT") {
      const forfeitBy = data.forfeitBy || "UNKNOWN";
      const message = `__FORFEIT__:${forfeitBy}`;
      
      if (lastProcessedStatus !== message) {
        lastProcessedStatus = message;
        onUpdate(message);
      }
      return;
    }

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

export async function cancelMatchmaking() {
  const playerId = getUniquePlayerId();
  if (!playerId) return;
  
  try {
    await deleteDoc(doc(db, "matchQueue", playerId));
    console.log("Removed from matchmaking queue:", playerId);
  } catch (err) {
    console.error("Error removing from queue:", err);
  }
}

export async function resetGame(gameId, player) {
  const gameRef = doc(db, "games", gameId);
  await updateDoc(gameRef, {
    status: "RESET",
    resetBy: player,
    resetAt: serverTimestamp()
  });
  console.log(`Game reset by ${player} for game:`, gameId);
}

export function subscribeReadyStatus(gameId, onReady) {
  const gameRef = doc(db, "games", gameId);
  
  let alreadyNotified = false;
  
  const unsub = onSnapshot(gameRef, (snap) => {
    if (!snap.exists()) return;
    const data = snap.data();
    
    if (data.status === "RESET") {
      return;
    }
    
    const p1Ready = data.p1Ready || false;
    const p2Ready = data.p2Ready || false;
    
    if (p1Ready && p2Ready && !alreadyNotified) {
      alreadyNotified = true;
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

let heartbeatInterval = null;
let forfeitOnUnloadHandler = null;

export function setupForfeitOnDisconnect(gameId, player) {
  console.log(`Setting up forfeit detection for ${player} in game ${gameId}`);

  clearForfeitOnDisconnect();
  
  const gameRef = doc(db, "games", gameId);
  const heartbeatField = player === "P1" ? "p1Heartbeat" : "p2Heartbeat";
  
  updateDoc(gameRef, {
    [heartbeatField]: serverTimestamp()
  }).catch(err => console.error("Error setting initial heartbeat:", err));
  
  heartbeatInterval = setInterval(async () => {
    try {
      await updateDoc(gameRef, {
        [heartbeatField]: serverTimestamp()
      });
      console.log(`Heartbeat sent for ${player}`);
    } catch (err) {
      console.error("Error updating heartbeat:", err);
    }
  }, 3000);

  forfeitOnUnloadHandler = (event) => {
    console.log(`${player} is leaving page - triggering forfeit`);
    
    const forfeitData = {
      status: "FORFEIT",
      forfeitBy: player,
      forfeitAt: serverTimestamp()
    };

    updateDoc(gameRef, forfeitData).catch(err => 
      console.error("Error setting forfeit status:", err)
    );
  };
  
  window.addEventListener("beforeunload", forfeitOnUnloadHandler);
  window.addEventListener("pagehide", forfeitOnUnloadHandler);
  
  return () => {
    if (heartbeatInterval) {
      clearInterval(heartbeatInterval);
      heartbeatInterval = null;
    }
    if (forfeitOnUnloadHandler) {
      window.removeEventListener("beforeunload", forfeitOnUnloadHandler);
      window.removeEventListener("pagehide", forfeitOnUnloadHandler);
      forfeitOnUnloadHandler = null;
    }
  };
}

export function clearForfeitOnDisconnect() {
  if (heartbeatInterval) {
    clearInterval(heartbeatInterval);
    heartbeatInterval = null;
  }
  if (forfeitOnUnloadHandler) {
    window.removeEventListener("beforeunload", forfeitOnUnloadHandler);
    window.removeEventListener("pagehide", forfeitOnUnloadHandler);
    forfeitOnUnloadHandler = null;
  }
  console.log("Cleared forfeit on disconnect handler");
}

window.firebaseBindings = {
  signInGuest,
  signOutUser,
  getCurrentUid,
  requestQuickMatch,
  cancelMatchmaking,
  subscribeGame,
  sendMove,
  setPlayerShips,
  markPlayerReady,
  subscribeReadyStatus,
  getShips,
  resetGame,
  setupForfeitOnDisconnect,
  clearForfeitOnDisconnect
};