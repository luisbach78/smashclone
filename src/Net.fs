module SmashClone.Net

// ---------------------------------------------------------------------------
// Online-Verbindung zwischen zwei Geräten.
//
// Weg 1 (bevorzugt): direkte Verbindung per WebRTC (PeerJS). Am schnellsten,
//   klappt aber in manchen Netzen nicht (Mobilfunk, strenge WLANs).
// Weg 2 (Ausweichweg): Relay über einen öffentlichen MQTT-Server per WebSocket.
//   Etwas mehr Verzögerung, funktioniert dafür fast überall.
//
// Der Gastgeber lauscht von Anfang an auf beiden Wegen. Der Gast versucht
// zuerst die direkte Verbindung und wechselt nach kurzer Zeit automatisch
// zum Relay, wenn sie nicht zustande kommt.
// ---------------------------------------------------------------------------

open Fable.Core
open Fable.Core.JsInterop

[<Import("Peer", "peerjs")>]
let private PeerClass: obj = jsNative

[<ImportAll("mqtt")>]
let private mqttModule: obj = jsNative

[<Emit("new $0($1, $2)")>]
let private newPeerWithId (_cls: obj) (_id: string) (_options: obj) : obj = jsNative

[<Emit("new $0($1)")>]
let private newPeer (_cls: obj) (_options: obj) : obj = jsNative

[<Emit("Math.random()")>]
let private random () : float = jsNative

[<Emit("setTimeout($0, $1)")>]
let private setTimeout (_f: unit -> unit) (_ms: int) : int = jsNative

[<Emit("clearTimeout($0)")>]
let private clearTimeout (_id: int) : unit = jsNative

[<Emit("setInterval($0, $1)")>]
let private setInterval (_f: unit -> unit) (_ms: int) : int = jsNative

[<Emit("clearInterval($0)")>]
let private clearInterval (_id: int) : unit = jsNative

[<Emit("JSON.stringify($0)")>]
let private toJson (_o: obj) : string = jsNative

[<Emit("JSON.parse($0)")>]
let private fromJson (_s: string) : obj = jsNative

/// Buchstaben ohne Verwechslungsgefahr (kein O/0, I/1)
let private alphabet = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

let newCode () =
    System.String(Array.init 4 (fun _ -> alphabet[int (random () * float alphabet.Length) % alphabet.Length]))

let private peerId (code: string) = "smashclone-v1-" + code.ToUpperInvariant()

/// STUN-Server helfen beim direkten Verbindungsaufbau durch Router hindurch
let private peerOptions () =
    createObj
        [ "debug" ==> 0
          "config"
          ==> createObj
                  [ "iceServers"
                    ==> [| createObj [ "urls" ==> "stun:stun.l.google.com:19302" ]
                           createObj [ "urls" ==> "stun:stun1.l.google.com:19302" ]
                           createObj [ "urls" ==> "stun:global.stun.twilio.com:3478" ] |] ] ]

/// Öffentliche MQTT-Server (WebSocket über TLS) für den Relay-Weg
let private brokers =
    [ "wss://broker.hivemq.com:8884/mqtt"; "wss://broker.emqx.io:8084/mqtt"; "wss://test.mosquitto.org:8081/mqtt" ]

let private topicBase (code: string) = "smashclone-game/v1/" + code.ToUpperInvariant()

type Transport =
    | NoTransport
    | Direct
    | Relay

let mutable private transport = NoTransport
let mutable private peer: obj = null
let mutable private conn: obj = null
let mutable private relayClients: obj list = []
let mutable private relayActive: obj = null
let mutable private relayOut = ""
let mutable private timers: int list = []
let mutable private session = 0

/// Statusmeldungen: "open", "connected", "closed", "error:<typ>"
let mutable onStatus: string -> unit = ignore

/// Empfangene Spielnachrichten
let mutable onData: obj -> unit = ignore

let currentTransport () = transport

let private errorType (e: obj) : string =
    if isNull e then "unbekannt"
    elif isNull e?``type`` then string e
    else string e?``type``

let private later (ms: int) (f: unit -> unit) =
    let id = setTimeout f ms
    timers <- id :: timers

let private clearTimers () =
    timers |> List.iter clearTimeout
    timers <- []

let private endClient (c: obj) =
    try
        c?``end`` (true) |> ignore
    with _ ->
        ()

let private closePeer () =
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

let private closeRelay () =
    relayClients |> List.iter endClient
    relayClients <- []
    relayActive <- null

let close () =
    session <- session + 1
    clearTimers ()
    closePeer ()
    closeRelay ()
    transport <- NoTransport

/// Verbindung steht: den anderen Weg aufräumen
let private established (t: Transport) =
    if transport = NoTransport then
        transport <- t
        clearTimers ()

        match t with
        | Direct -> closeRelay ()
        | Relay ->
            closePeer ()
            // Nur den Server behalten, über den die Verbindung zustande kam
            relayClients
            |> List.filter (fun c -> not (obj.ReferenceEquals(c, relayActive)))
            |> List.iter endClient

            relayClients <- [ relayActive ]
        | NoTransport -> ()

        onStatus "connected"

// ---------------------------------------------------------------------------
// Weg 1: direkte Verbindung (WebRTC)
// ---------------------------------------------------------------------------

let private attachDirect (c: obj) =
    conn <- c
    c?on ("open", fun () -> established Direct)
    c?on ("data", fun (d: obj) -> if transport = Direct then onData d)
    c?on ("close", fun () -> if transport = Direct then onStatus "closed")
    c?on ("error", fun (e: obj) -> if transport = Direct then onStatus ("error:" + errorType e))

// ---------------------------------------------------------------------------
// Weg 2: Relay über MQTT
// ---------------------------------------------------------------------------

let private publishOn (client: obj) (topic: string) (msg: obj) =
    try
        client?publish (topic, toJson msg, createObj [ "qos" ==> 0 ]) |> ignore
    with _ ->
        ()

let private connectRelay (url: string) (subscribeTo: string) (onMessage: obj -> obj -> unit) (onReady: obj -> unit) =
    let connectFn: obj =
        if isNull mqttModule?connect then mqttModule?``default``?connect else mqttModule?connect

    try
        let client: obj =
            connectFn
            $ (url,
               createObj
                   [ "clientId" ==> ("sc-" + string (int (random () * 1e9)))
                     "keepalive" ==> 20
                     "reconnectPeriod" ==> 2000
                     "connectTimeout" ==> 6000
                     "clean" ==> true ])

        relayClients <- client :: relayClients

        client?on (
            "connect",
            fun () ->
                client?subscribe (subscribeTo, createObj [ "qos" ==> 0 ]) |> ignore
                onReady client
        )

        client?on (
            "message",
            fun (_topic: obj) (payload: obj) ->
                try
                    onMessage client (fromJson (payload?toString ()))
                with _ ->
                    ()
        )

        client?on ("error", fun (_e: obj) -> ())
    with _ ->
        ()

let private relayData (client: obj) (msg: obj) =
    if transport = Relay && obj.ReferenceEquals(client, relayActive) then
        onData msg

/// Gastgeber lauscht auf allen Relay-Servern auf ein "hello" des Gasts
let private hostRelay (code: string) (mySession: int) =
    let topics = topicBase code

    for url in brokers do
        connectRelay
            url
            (topics + "/h")
            (fun client msg ->
                if session = mySession then
                    match string msg?t with
                    | "hello" ->
                        if transport = NoTransport then
                            relayActive <- client
                            relayOut <- topics + "/g"
                            publishOn client relayOut (createObj [ "t" ==> "welcome" ])
                            established Relay
                        elif transport = Relay && obj.ReferenceEquals(client, relayActive) then
                            // Gast hat das "welcome" verpasst: noch einmal schicken
                            publishOn client relayOut (createObj [ "t" ==> "welcome" ])
                    | _ -> relayData client msg)
            (fun _ -> if session = mySession then onStatus "open")

/// Gast: auf allen Relay-Servern anklopfen, der erste antwortende gewinnt
let private joinRelay (code: string) (mySession: int) =
    let topics = topicBase code
    let mutable ready: obj list = []

    for url in brokers do
        connectRelay
            url
            (topics + "/g")
            (fun client msg ->
                if session = mySession then
                    match string msg?t with
                    | "welcome" ->
                        if transport = NoTransport then
                            relayActive <- client
                            relayOut <- topics + "/h"
                            established Relay
                    | _ -> relayData client msg)
            (fun client -> ready <- client :: ready)

    let knock =
        setInterval
            (fun () ->
                if session = mySession && transport = NoTransport then
                    for c in ready do
                        publishOn c (topics + "/h") (createObj [ "t" ==> "hello" ]))
            700

    later 16000 (fun () -> clearInterval knock)

    later 15000 (fun () ->
        if session = mySession && transport = NoTransport then
            onStatus "error:peer-unavailable")

// ---------------------------------------------------------------------------
// Öffentliche Schnittstelle
// ---------------------------------------------------------------------------

/// Raum eröffnen: lauscht direkt (WebRTC) und über das Relay
let host (code: string) =
    close ()
    let mySession = session

    try
        peer <- newPeerWithId PeerClass (peerId code) (peerOptions ())
        peer?on ("open", fun (_: obj) -> if session = mySession then onStatus "open")

        peer?on (
            "connection",
            fun (c: obj) ->
                if session = mySession && transport = NoTransport && isNull conn then
                    attachDirect c
                else
                    c?close ()
        )

        peer?on (
            "error",
            fun (e: obj) ->
                // Nur ein belegter Code ist ein echtes Problem, sonst bleibt das Relay als Weg offen
                if session = mySession && errorType e = "unavailable-id" then
                    onStatus "error:unavailable-id"
        )

        // Verbindung zum Vermittlungsserver verloren (z. B. Seite im Hintergrund): neu verbinden
        peer?on (
            "disconnected",
            fun () ->
                if session = mySession && transport = NoTransport then
                    try
                        peer?reconnect ()
                    with _ ->
                        ()
        )
    with _ ->
        peer <- null

    hostRelay code mySession

/// Einem Raum beitreten: erst direkt, nach kurzer Zeit (oder bei Fehlern) über das Relay
let join (code: string) =
    close ()
    let mySession = session

    let startRelay () =
        if session = mySession && transport = NoTransport && List.isEmpty relayClients then
            onStatus "trying-relay"
            joinRelay code mySession

    try
        peer <- newPeer PeerClass (peerOptions ())

        peer?on (
            "open",
            fun (_: obj) ->
                if session = mySession then
                    onStatus "open"
                    attachDirect (peer?connect (peerId code, createObj [ "reliable" ==> true; "serialization" ==> "json" ]))
        )

        peer?on ("error", fun (_: obj) -> startRelay ())
    with _ ->
        startRelay ()

    later 5000 startRelay

let send (msg: obj) =
    match transport with
    | Direct ->
        if not (isNull conn) && (conn?``open``: bool) then
            try
                conn?send msg
            with _ ->
                ()
    | Relay ->
        if not (isNull relayActive) then
            publishOn relayActive relayOut msg
    | NoTransport -> ()
