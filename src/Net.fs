module SmashClone.Net

// ---------------------------------------------------------------------------
// Online-Verbindung zwischen zwei Geräten über WebRTC (PeerJS).
// Der öffentliche PeerJS-Server vermittelt nur den Verbindungsaufbau,
// danach laufen die Eingaben direkt von Gerät zu Gerät.
// ---------------------------------------------------------------------------

open Fable.Core
open Fable.Core.JsInterop

[<Import("Peer", "peerjs")>]
let private PeerClass: obj = jsNative

[<Emit("new $0($1)")>]
let private newPeerWithId (_cls: obj) (_id: string) : obj = jsNative

[<Emit("new $0()")>]
let private newPeer (_cls: obj) : obj = jsNative

[<Emit("Math.random()")>]
let private random () : float = jsNative

/// Buchstaben ohne Verwechslungsgefahr (kein O/0, I/1)
let private alphabet = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

let newCode () =
    System.String(Array.init 4 (fun _ -> alphabet[int (random () * float alphabet.Length) % alphabet.Length]))

let private peerId (code: string) = "smashclone-v1-" + code.ToUpperInvariant()

let mutable private peer: obj = null
let mutable private conn: obj = null

/// Wird mit Statusmeldungen aufgerufen: "open", "connected", "closed", "error:<typ>"
let mutable onStatus: string -> unit = ignore

/// Wird mit jeder empfangenen Nachricht aufgerufen
let mutable onData: obj -> unit = ignore

let isConnected () = not (isNull conn) && (conn?``open``: bool)

let private errorType (e: obj) : string =
    if isNull e then "unbekannt"
    elif isNull e?``type`` then string e
    else string e?``type``

let private attach (c: obj) =
    conn <- c
    c?on ("open", fun () -> onStatus "connected")
    c?on ("data", fun (d: obj) -> onData d)
    c?on ("close", fun () -> onStatus "closed")
    c?on ("error", fun (e: obj) -> onStatus ("error:" + errorType e))

let close () =
    try
        if not (isNull conn) then conn?close ()
    with _ ->
        ()

    try
        if not (isNull peer) then peer?destroy ()
    with _ ->
        ()

    conn <- null
    peer <- null

/// Raum eröffnen: wartet unter dem Raumcode auf genau einen Mitspieler
let host (code: string) =
    close ()
    peer <- newPeerWithId PeerClass (peerId code)
    peer?on ("open", fun (_: obj) -> onStatus "open")

    peer?on (
        "connection",
        fun (c: obj) ->
            if isNull conn then
                attach c
            else
                c?close ()
    )

    peer?on ("error", fun (e: obj) -> onStatus ("error:" + errorType e))
    peer?on ("disconnected", fun () -> if isNull conn then onStatus "error:server")

/// Einem Raum beitreten
let join (code: string) =
    close ()
    peer <- newPeer PeerClass

    peer?on (
        "open",
        fun (_: obj) ->
            onStatus "open"
            attach (peer?connect (peerId code, createObj [ "reliable" ==> true; "serialization" ==> "json" ]))
    )

    peer?on ("error", fun (e: obj) -> onStatus ("error:" + errorType e))

let send (msg: obj) =
    if isConnected () then
        try
            conn?send msg
        with _ ->
            ()
