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

  let matchResult = null;
  
  try {
    await runTransaction(db, async (transaction) => {
      const queueRef = collection(db, "matchQueue");
      const q = query(queueRef, orderBy("timestamp"), limit(10));
      const snap = await getDocs(q);
      
      for (const docSnap of snap.docs) {
        const opponentUid = docSnap.id;
        if (opponentUid !== uid) {
          const opponentRef = doc(db, "matchQueue", opponentUid);
          const opponentDoc = await transaction.get(opponentRef);
          
          if (opponentDoc.exists()) {
            const seed = Math.floor(Math.random() * 1_000_000);
            const gameRef = doc(collection(db, "games"));
            
            transaction.set(gameRef, {
              p1: opponentUid,
              p2: uid,
              seed: seed,
              moves: [],
              status: "ACTIVE",
              p1Ships: "[]",
              p2Ships: "[]",
              p1Ready: false,
              p2Ready: false,
              createdAt: serverTimestamp()
            });
            
            transaction.delete(opponentRef);
            matchResult = { gameId: gameRef.id, role: "P2" };
            return;
          }
        }
      }
      
      if (!matchResult) {
        const myQueueRef = doc(db, "matchQueue", uid);
        transaction.set(myQueueRef, {
          uid,
          timestamp: serverTimestamp()
        });
      }
    });
  } catch (err) {
    console.error("Error in matchmaking transaction:", err);
    return;
  }

  if (matchResult) {
    console.log("Matched as", matchResult.role, "in game:", matchResult.gameId);
    onMatched(`${matchResult.gameId}|${matchResult.role}`);
    return;
  }

  console.log("Joined queue as P1:", uid);

  let pollTimeout = null;
  const unsubQueue = onSnapshot(doc(db, "matchQueue", uid), async (queueSnap) => {
    if (!queueSnap.exists()) {
      if (pollTimeout) {
        clearTimeout(pollTimeout);
      }
      
      const gq = query(
        collection(db, "games"),
        orderBy("createdAt", "desc"),
        limit(20)
      );
      const gamesSnap = await getDocs(gq);

      let found = null;
      gamesSnap.forEach((g) => {
        const data = g.data();
        const isRecent = data.createdAt && 
          (Date.now() - data.createdAt.toMillis()) < 30000;
        const isActive = data.status === "ACTIVE";
        
        if (isActive && isRecent && (data.p1 === uid || data.p2 === uid)) {
          found = { id: g.id, data };
        }
      });

      if (found) {
        unsubQueue();
        const role = (found.data.p1 === uid) ? "P1" : "P2";
        console.log("Matched as", role, "in game:", found.id);
        onMatched(`${found.id}|${role}`);
      } else {
        console.log("Queue entry deleted but no game found - rejoining");
        await setDoc(doc(db, "matchQueue", uid), {
          uid,
          timestamp: serverTimestamp()
        });
      }
    }
  });

  pollTimeout = setTimeout(() => {
    console.log("Matchmaking timeout - removing from queue");
    unsubQueue();
    deleteDoc(doc(db, "matchQueue", uid)).catch(() => {});
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
  const uid = getCurrentUid();
  if (!uid) return;
  
  try {
    await deleteDoc(doc(db, "matchQueue", uid));
    console.log("Removed from matchmaking queue:", uid);
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

  forfeitOnUnloadHandler = () => {
    console.log(`${player} is leaving - triggering forfeit`);
    
    // Use synchronous approach for mobile reliability
    const forfeitData = {
      status: "FORFEIT",
      forfeitBy: player,
      forfeitAt: serverTimestamp()
    };
    
    // Try to update immediately (may not complete on mobile)
    updateDoc(gameRef, forfeitData).catch(err => 
      console.error("Error setting forfeit status:", err)
    );
  };
  
  window.addEventListener("beforeunload", forfeitOnUnloadHandler);
  window.addEventListener("pagehide", forfeitOnUnloadHandler);
  
  const visibilityHandler = () => {
    if (document.hidden) {
      console.log(`${player} tab hidden - will forfeit if not back soon`);
      setTimeout(async () => {
        if (document.hidden) {
          console.log(`${player} still hidden - forfeiting`);
          try {
            await updateDoc(gameRef, {
              status: "FORFEIT",
              forfeitBy: player,
              forfeitAt: serverTimestamp()
            });
          } catch (err) {
            console.error("Error setting forfeit on visibility change:", err);
          }
        }
      }, 10000);
    }
  };
  
  document.addEventListener("visibilitychange", visibilityHandler);
  
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
    document.removeEventListener("visibilitychange", visibilityHandler);
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