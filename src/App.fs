module SmashClone.App

open System.Collections.Generic
open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open SmashClone.Types

// ---------------------------------------------------------------------------
// Canvas
// ---------------------------------------------------------------------------

let private canvas = document.getElementById "game" :?> HTMLCanvasElement
let private ctx = canvas.getContext_2d ()

/// Interne Auflösung an die Pixeldichte anpassen (scharf auf Retina-Displays)
let private pixelScale () = max 1.0 (min 2.0 window.devicePixelRatio)

let private resizeCanvas () =
    let scale = pixelScale ()
    canvas.width <- screenWidth * scale
    canvas.height <- screenHeight * scale

// ---------------------------------------------------------------------------
// Input-Handling
// ---------------------------------------------------------------------------

type private KeyMap =
    { KLeft: string
      KRight: string
      KUp: string
      KDown: string
      KAttack: string
      KSpecial: string
      KShield: string
      /// Virtuelle Controller-Tasten: Sprungknopf und Stick-Oben (ohne Sprung)
      KJump: string
      KAim: string }

let private player1Keys =
    { KLeft = "KeyA"
      KRight = "KeyD"
      KUp = "KeyW"
      KDown = "KeyS"
      KAttack = "KeyF"
      KSpecial = "KeyG"
      KShield = "KeyH"
      KJump = "Pad1Jump"
      KAim = "Pad1Up" }

let private player2Keys =
    { KLeft = "ArrowLeft"
      KRight = "ArrowRight"
      KUp = "ArrowUp"
      KDown = "ArrowDown"
      KAttack = "KeyK"
      KSpecial = "KeyL"
      KShield = "Semicolon"
      KJump = "Pad2Jump"
      KAim = "Pad2Up" }

/// Aktuell gehaltene Tasten und seit dem letzten Simulationsschritt neu gedrückte Tasten
let private keysDown = HashSet<string>()
let private keysPressed = HashSet<string>()

/// Layout-unabhängiger Tastencode; "Ö" liegt auf deutschen Tastaturen auf "Semicolon"
let private keyId (e: KeyboardEvent) =
    if e.key = "ö" || e.key = "Ö" then "Semicolon" else e.code

let private blockedDefaults =
    set [ "ArrowLeft"; "ArrowRight"; "ArrowUp"; "ArrowDown"; "Space"; "Enter" ]

[<Emit("($0.target && $0.target.tagName === 'INPUT')")>]
let private fromTextField (_e: Event) : bool = jsNative

let private onKeyDown (e: Event) =
    let k = e :?> KeyboardEvent
    let id = keyId k
    SmashClone.Audio.unlock ()

    if not (fromTextField e) then
        if blockedDefaults.Contains id then
            k.preventDefault ()

        if not k.repeat then
            keysDown.Add id |> ignore
            keysPressed.Add id |> ignore

let private onKeyUp (e: Event) =
    let k = e :?> KeyboardEvent
    keysDown.Remove(keyId k) |> ignore

// ---------------------------------------------------------------------------
// Controller (Gamepad-API, Standard-Mapping, z. B. PS4/PS5, Xbox, Switch Pro)
// Controller 1 steuert Spieler 1, Controller 2 steuert Spieler 2. Die Knöpfe
// werden in virtuelle Tasten übersetzt, damit Kampf und Menüs identisch reagieren.
// ---------------------------------------------------------------------------

[<Emit("(typeof navigator !== 'undefined' && navigator.getGamepads) ? Array.from(navigator.getGamepads()).filter(p => p != null && p.connected) : []")>]
let private connectedPads () : obj array = jsNative

[<Emit("(($0.buttons[$1] != null) && ($0.buttons[$1].pressed || $0.buttons[$1].value > 0.5))")>]
let private buttonDown (_pad: obj) (_index: int) : bool = jsNative

[<Emit("($0.axes[$1] != null ? $0.axes[$1] : 0)")>]
let private axis (_pad: obj) (_index: int) : float = jsNative

/// Von Controllern gehaltene virtuelle Tasten (getrennt von der Tastatur)
let private padDown = HashSet<string>()

let private isDown (key: string) = keysDown.Contains key || padDown.Contains key

let private stickThreshold = 0.5

/// Welche virtuellen Tasten ein Controller gerade hält
let private padKeys (inMatch: bool) (m: KeyMap) (pad: obj) : string list =
    let b i = buttonDown pad i
    let x = axis pad 0
    let y = axis pad 1

    [ if b 14 || x < -stickThreshold then m.KLeft
      if b 15 || x > stickThreshold then m.KRight
      if b 13 || y > stickThreshold then m.KDown

      if inMatch then
          // Stick/Steuerkreuz oben zielt nur (Up-Air, Up-B), gesprungen wird mit Kreuz oder Dreieck
          if b 12 || y < -stickThreshold then m.KAim
          if b 0 || b 3 then m.KJump
          if b 2 then m.KAttack
          if b 1 then m.KSpecial
          if b 4 || b 5 || b 6 || b 7 then m.KShield
      else
          // Menüs: Kreuz bestätigt, Kreis geht zurück
          if b 12 || y < -stickThreshold then m.KUp
          if b 0 || b 2 then m.KAttack
          if b 1 then m.KShield

      if b 8 then "Escape"
      if b 9 then "Enter" ]

// ---------------------------------------------------------------------------
// Touch-Steuerung (Handy/Tablet): virtueller Stick links, Knöpfe rechts.
// Gehaltene Touch-Eingaben landen wie Controller-Knöpfe bei Spieler 1.
// ---------------------------------------------------------------------------

let private touchHeld = HashSet<string>()
/// Kurze Tipper, die zwischen zwei Frames schon wieder losgelassen wurden, zählen trotzdem einmal
let private touchTapped = HashSet<string>()
let private stickRadius = 46.0
let private stickDead = 16.0

[<Emit("(typeof window !== 'undefined') && ((window.matchMedia && window.matchMedia('(pointer: coarse)').matches) || ('ontouchstart' in window))")>]
let private isTouchDevice () : bool = jsNative

[<Emit("(document.documentElement.requestFullscreen ? document.documentElement.requestFullscreen().catch(() => {}) : null)")>]
let private tryFullscreen () : unit = jsNative

let private el (tag: string) (cls: string) (parent: HTMLElement) =
    let e = document.createElement tag
    e.className <- cls
    parent.appendChild e |> ignore
    e

let private setupTouch () =
    if isTouchDevice () then
        document.body.classList.add "touch"
        let layer = el "div" "touch-layer" document.body
        let zone = el "div" "touch-stick-zone" layer
        let ring = el "div" "touch-stick" zone
        let knob = el "div" "touch-knob" ring
        let mutable stickPointer = -1.0
        let mutable originX = 0.0
        let mutable originY = 0.0

        let setStick (dx: float) (dy: float) =
            touchHeld.Remove "left" |> ignore
            touchHeld.Remove "right" |> ignore
            touchHeld.Remove "up" |> ignore
            touchHeld.Remove "down" |> ignore

            if dx < -stickDead then touchHeld.Add "left" |> ignore
            if dx > stickDead then touchHeld.Add "right" |> ignore
            if dy < -stickDead * 1.4 then touchHeld.Add "up" |> ignore
            if dy > stickDead * 1.4 then touchHeld.Add "down" |> ignore

            let len = sqrt (dx * dx + dy * dy)
            let k = if len > stickRadius then stickRadius / len else 1.0
            knob?style?transform <- sprintf "translate(%.0fpx, %.0fpx)" (dx * k) (dy * k)

        zone.addEventListener (
            "pointerdown",
            fun (e: Event) ->
                e.preventDefault ()
                SmashClone.Audio.unlock ()
                tryFullscreen ()
                stickPointer <- e?pointerId
                originX <- e?clientX
                originY <- e?clientY
                zone?setPointerCapture (e?pointerId) |> ignore
                // Stick erscheint dort, wo der Daumen aufsetzt
                let rect = zone.getBoundingClientRect ()
                ring?style?left <- sprintf "%.0fpx" (originX - rect.left)
                ring?style?top <- sprintf "%.0fpx" (originY - rect.top)
                ring.classList.add "active"
                setStick 0.0 0.0
        )

        zone.addEventListener (
            "pointermove",
            fun (e: Event) ->
                if e?pointerId = stickPointer then
                    e.preventDefault ()
                    setStick (e?clientX - originX) (e?clientY - originY)
        )

        let release (e: Event) =
            if e?pointerId = stickPointer then
                stickPointer <- -1.0
                ring.classList.remove "active"
                setStick 0.0 0.0

        zone.addEventListener ("pointerup", release)
        zone.addEventListener ("pointercancel", release)

        let pad = el "div" "touch-buttons" layer

        let button (cls: string) (label: string) (action: string) (parent: HTMLElement) =
            let b = el "div" ("touch-btn " + cls) parent
            b.textContent <- label

            b.addEventListener (
                "pointerdown",
                fun (e: Event) ->
                    e.preventDefault ()
                    SmashClone.Audio.unlock ()
                    tryFullscreen ()
                    b?setPointerCapture (e?pointerId) |> ignore
                    b.classList.add "down"
                    touchHeld.Add action |> ignore
                    touchTapped.Add action |> ignore
            )

            let up (_: Event) =
                b.classList.remove "down"
                touchHeld.Remove action |> ignore

            b.addEventListener ("pointerup", up)
            b.addEventListener ("pointercancel", up)

        button "btn-attack" "A" "attack" pad
        button "btn-special" "B" "special" pad
        button "btn-jump" "▲" "jump" pad
        button "btn-shield" "S" "shield" pad
        let top = el "div" "touch-top" layer
        button "btn-small" "MENÜ" "back" top
        button "btn-small" "START" "start" top
        // Langes Drücken und Kontextmenü auf dem Spielfeld verhindern
        document.addEventListener ("contextmenu", fun (e: Event) -> e.preventDefault ())

let private touchKeys (inMatch: bool) (m: KeyMap) : string list =
    let active = HashSet<string>(touchHeld)

    for t in touchTapped do
        active.Add t |> ignore

    touchTapped.Clear()

    [ for t in active do
          match t with
          | "left" -> m.KLeft
          | "right" -> m.KRight
          | "down" -> m.KDown
          | "up" -> if inMatch then m.KAim else m.KUp
          | "jump" -> if inMatch then m.KJump else m.KUp
          | "attack" -> m.KAttack
          | "special" -> m.KSpecial
          | "shield" -> m.KShield
          | "start" -> "Enter"
          | "back" -> "Escape"
          | _ -> () ]

let private pollPads (inMatch: bool) =
    let now = HashSet<string>()
    touchKeys inMatch player1Keys |> List.iter (fun k -> now.Add k |> ignore)

    connectedPads ()
    |> Array.truncate 2
    |> Array.iteri (fun i pad ->
        let m = if i = 0 then player1Keys else player2Keys
        padKeys inMatch m pad |> List.iter (fun k -> now.Add k |> ignore))

    for k in now do
        if not (padDown.Contains k) then
            keysPressed.Add k |> ignore
            SmashClone.Audio.unlock ()

    padDown.Clear()

    for k in now do
        padDown.Add k |> ignore

let private readInput (m: KeyMap) : Input =
    { Left = isDown m.KLeft
      Right = isDown m.KRight
      Up = isDown m.KUp || isDown m.KAim
      Down = isDown m.KDown
      Jump = isDown m.KUp || isDown m.KJump
      Attack = isDown m.KAttack
      Special = isDown m.KSpecial
      Shield = isDown m.KShield }

let private readPad (m: KeyMap) : MenuPad =
    { Up = keysPressed.Contains m.KUp
      Down = keysPressed.Contains m.KDown
      Left = keysPressed.Contains m.KLeft
      Right = keysPressed.Contains m.KRight
      Accept = keysPressed.Contains m.KAttack
      Cancel = keysPressed.Contains m.KShield }

let private readUi () : UiInput =
    { P1 = readPad player1Keys
      P2 = readPad player2Keys
      Start = keysPressed.Contains "Enter" || keysPressed.Contains "NumpadEnter" || keysPressed.Contains "Space"
      Escape = keysPressed.Contains "Escape"
      ToggleDebug = keysPressed.Contains "KeyT" }

// ---------------------------------------------------------------------------
// State-Management: reine Funktionen (Eingaben, alter Zustand) -> neuer Zustand
// ---------------------------------------------------------------------------

let private merged (ui: UiInput) : MenuPad =
    { Up = ui.P1.Up || ui.P2.Up
      Down = ui.P1.Down || ui.P2.Down
      Left = ui.P1.Left || ui.P2.Left
      Right = ui.P1.Right || ui.P2.Right
      Accept = ui.P1.Accept || ui.P2.Accept
      Cancel = ui.P1.Cancel || ui.P2.Cancel }

let private withSound (s: Sfx) (g: Game) = { g with Sounds = g.Sounds @ [ s ] }

let private menuMove (pad: MenuPad) (count: int) (g: Game) : Game =
    let delta = (if pad.Down then 1 else 0) - (if pad.Up then 1 else 0)

    if delta = 0 then g
    else { g with MenuIndex = (g.MenuIndex + delta + count) % count } |> withSound SfxMenuMove

let private shiftDifficulty (delta: int) (g: Game) : Game =
    let i = allDifficulties |> List.findIndex (fun d -> d = g.Difficulty)
    let next = max 0 (min 3 (i + delta))

    if next = i then g
    else { g with Difficulty = List.item next allDifficulties } |> withSound SfxMenuMove

let private toTitle (g: Game) : Game =
    { g with
        Screen = Title
        MenuIndex = (if g.Mode = VsCpu then 0 else 1)
        Projectiles = []
        Beams = []
        Particles = []
        ShakeMag = 0.0
        ImpactFlash = 0
        HitStop = 0 }
    |> withSound SfxMenuBack

let private toCharSelect (mode: GameMode) (g: Game) : Game =
    { g with
        Screen = CharSelect
        Mode = mode
        Select = { g.Select with Locked = [ false; false ] }
        OptionIndex = (if mode = VsCpu then 0 else 1)
        Ball = None
        Ultra = None
        Projectiles = []
        Beams = []
        Particles = []
        ShakeMag = 0.0
        ImpactFlash = 0
        HitStop = 0 }
    |> withSound SfxMenuSelect

let private toStageSelect (g: Game) : Game =
    { g with Screen = StageSelect } |> withSound SfxMenuSelect

let private startMatch (g: Game) : Game =
    let started = Physics.newMatch g
    { started with Sounds = SfxMenuSelect :: started.Sounds }

let private updateTitle (ui: UiInput) (g: Game) : Game =
    let pad = merged ui
    let moved = menuMove pad 4 g

    // Links/Rechts ändert die CPU-Stärke, egal welche Zeile gewählt ist
    let tuned =
        if pad.Left || pad.Right then shiftDifficulty (if pad.Right then 1 else -1) moved
        else moved

    if ui.Start || pad.Accept then
        match tuned.MenuIndex with
        | 0 -> toCharSelect VsCpu tuned
        | 1 -> toCharSelect TwoPlayer tuned
        | 2 -> { tuned with OpenOnline = true } |> withSound SfxMenuSelect
        | _ ->
            let i = allDifficulties |> List.findIndex (fun d -> d = tuned.Difficulty)
            { tuned with Difficulty = List.item ((i + 1) % 4) allDifficulties } |> withSound SfxMenuMove
    else
        tuned

/// Cursor im 4x2-Raster: Links/Rechts innerhalb der Reihe, Hoch/Runter wechselt die Reihe
let private moveCursor (pad: MenuPad) (cursor: int) =
    let count = List.length allFighters
    let cols = 6
    let row = cursor / cols
    let col = cursor % cols
    let rowLength = min cols (count - row * cols)

    if pad.Left then row * cols + (col + rowLength - 1) % rowLength
    elif pad.Right then row * cols + (col + 1) % rowLength
    elif pad.Up || pad.Down then
        let otherRow = if row = 0 then 1 else 0
        let otherLength = min cols (count - otherRow * cols)
        otherRow * cols + min col (otherLength - 1)
    else cursor

/// Match-Regeln (nur wenn beide Kämpfer feststehen): Zeile wählen und Wert ändern
let private updateOptions (pad: MenuPad) (g: Game) : Game =
    let first = if g.Mode = VsCpu then 0 else 1
    let rows = 5 - first
    let delta = (if pad.Down then 1 else 0) - (if pad.Up then 1 else 0)

    let g =
        if delta = 0 then g
        else { g with OptionIndex = first + ((g.OptionIndex - first + delta + rows) % rows) } |> withSound SfxMenuMove

    let change = (if pad.Right then 1 else 0) - (if pad.Left then 1 else 0)

    if change = 0 then
        g
    else
        match g.OptionIndex with
        | 0 -> shiftDifficulty change g
        | 1 ->
            let next = max 1 (min 5 (g.StockCount + change))
            if next = g.StockCount then g else { g with StockCount = next } |> withSound SfxMenuMove
        | 2 -> { g with SmashBallOn = not g.SmashBallOn } |> withSound SfxMenuMove
        | 3 -> { g with ItemsOn = not g.ItemsOn } |> withSound SfxMenuMove
        | _ -> { g with HazardsOn = not g.HazardsOn } |> withSound SfxMenuMove

let private setAt (i: int) (value: 'T) (list: 'T list) = list |> List.mapi (fun j x -> if j = i then value else x)

let private updateCharSelect (ui: UiInput) (g: Game) : Game =
    let sel = g.Select
    let locked i = List.item i sel.Locked
    let allLocked = locked 0 && locked 1
    let pad = merged ui

    if ui.Escape then
        toTitle g
    elif allLocked && (ui.Start || pad.Accept) then
        toStageSelect g
    elif allLocked && not pad.Cancel then
        updateOptions pad g
    else
        match g.Mode with
        | VsCpu ->
            // P1 wählt erst den eigenen Kämpfer, danach den der CPU
            let active = if not (locked 0) then Some 0 elif not (locked 1) then Some 1 else None
            if pad.Cancel then
                if locked 1 then { g with Select = { sel with Locked = setAt 1 false sel.Locked } } |> withSound SfxMenuBack
                elif locked 0 then { g with Select = { sel with Locked = setAt 0 false sel.Locked } } |> withSound SfxMenuBack
                else toTitle g
            else
                match active with
                | Some slot ->
                    let cursor = List.item slot sel.Cursors
                    let next = moveCursor pad cursor
                    let moved = if next <> cursor then { g with Select = { sel with Cursors = setAt slot next sel.Cursors } } |> withSound SfxMenuMove else g

                    if pad.Accept || ui.Start then
                        { moved with Select = { moved.Select with Locked = setAt slot true moved.Select.Locked } } |> withSound SfxPick
                    else
                        moved
                | None -> g
        | TwoPlayer ->
            // Beide Spieler wählen gleichzeitig mit ihren eigenen Tasten
            let handle (slot: int) (p: MenuPad) (state: Game) : Game =
                let s = state.Select

                if p.Cancel then
                    if List.item slot s.Locked then
                        { state with Select = { s with Locked = setAt slot false s.Locked } } |> withSound SfxMenuBack
                    else
                        state
                elif List.item slot s.Locked then
                    state
                else
                    let cursor = List.item slot s.Cursors
                    let next = moveCursor p cursor
                    let moved = if next <> cursor then { state with Select = { s with Cursors = setAt slot next s.Cursors } } |> withSound SfxMenuMove else state

                    if p.Accept then
                        { moved with Select = { moved.Select with Locked = setAt slot true moved.Select.Locked } } |> withSound SfxPick
                    else
                        moved

            let backOut = (ui.P1.Cancel && not (locked 0)) || (ui.P2.Cancel && not (locked 1))

            if backOut && not (locked 0) && not (locked 1) then toTitle g
            else g |> handle 0 ui.P1 |> handle 1 ui.P2

let private updateStageSelect (ui: UiInput) (g: Game) : Game =
    let pad = merged ui
    let i = allStages |> List.findIndex (fun s -> s = g.Stage)
    let count = List.length allStages

    if ui.Escape || pad.Cancel then
        { g with Screen = CharSelect } |> withSound SfxMenuBack
    elif ui.Start || pad.Accept then
        startMatch g
    elif pad.Left || pad.Right then
        let next = (i + (if pad.Right then 1 else count - 1)) % count
        { g with Stage = List.item next allStages } |> withSound SfxMenuMove
    else
        g

let private updateGameOver (ui: UiInput) (g: Game) : Game =
    let pad = merged ui
    let moved = menuMove pad 3 g

    if ui.Escape then
        toTitle moved
    elif ui.Start || pad.Accept then
        match moved.MenuIndex with
        | 0 -> startMatch moved
        | 1 -> toCharSelect moved.Mode moved
        | _ -> toTitle moved
    else
        moved

let update (ui: UiInput) (p1: Input) (p2: Input) (game: Game) : Game =
    let g = if ui.ToggleDebug then { game with ShowBoxes = not game.ShowBoxes } else game
    let idle = [ Input.none; Input.none ]

    match g.Screen with
    | Title -> Physics.step idle g |> updateTitle ui
    | CharSelect -> Physics.step idle g |> updateCharSelect ui
    | StageSelect -> Physics.step idle g |> updateStageSelect ui
    | GameOver -> Physics.step idle g |> updateGameOver ui
    | Playing ->
        // START pausiert, MENÜ/Esc während der Pause beendet den Kampf
        if g.Paused then
            if ui.Start then { g with Paused = false } |> withSound SfxMenuSelect
            elif ui.Escape then toTitle { g with Paused = false }
            else { g with Frame = g.Frame + 1; Sounds = [] }
        elif (ui.Start || ui.Escape) && g.Countdown <= 0 && g.Ultra.IsNone then
            { g with Paused = true } |> withSound SfxMenuBack
        else
            match g.Mode, g.Players with
            | VsCpu, [ human; cpu ] ->
                let layout = stageLayout g.Stage g.MatchFrames
                let brain, cpuInput = AI.decide g.Difficulty g.Brain cpu human layout g.Ball g.Items g.Hazard
                Physics.step [ p1; cpuInput ] { g with Brain = brain }
            | _ -> Physics.step [ p1; p2 ] g

// ---------------------------------------------------------------------------
// Main-Loop mit festem Timestep
// ---------------------------------------------------------------------------

let mutable private game = Physics.initialGame
let mutable private lastTime = 0.0
let mutable private accumulator = 0.0

// ---------------------------------------------------------------------------
// Online-Modus: Rollback-Netcode
// Beide Geräte simulieren dasselbe Spiel. Ausgetauscht werden nur die Eingaben.
// Fehlt die Eingabe des Gegners noch, wird sie vorhergesagt (letzte bekannte
// Eingabe). Kommt sie später anders an, wird zurückgespult und neu gerechnet.
// ---------------------------------------------------------------------------

let private inputDelay = 2
let private maxRollback = 12

let private bit (b: bool) (i: int) = if b then 1 <<< i else 0
let private has (bits: int) (i: int) = bits &&& (1 <<< i) <> 0
let private heldMask = 0xFF

/// Eingabe eines Spielers als Bitmaske: 0–7 gehaltene Tasten, 8–13 Menü-Tipper, 14 Start, 15 Esc
let private encode (inp: Input) (pad: MenuPad) (start: bool) (escape: bool) =
    bit inp.Left 0 ||| bit inp.Right 1 ||| bit inp.Up 2 ||| bit inp.Down 3
    ||| bit inp.Jump 4 ||| bit inp.Attack 5 ||| bit inp.Special 6 ||| bit inp.Shield 7
    ||| bit pad.Up 8 ||| bit pad.Down 9 ||| bit pad.Left 10 ||| bit pad.Right 11
    ||| bit pad.Accept 12 ||| bit pad.Cancel 13 ||| bit start 14 ||| bit escape 15

let private decodeInput (b: int) : Input =
    { Left = has b 0
      Right = has b 1
      Up = has b 2
      Down = has b 3
      Jump = has b 4
      Attack = has b 5
      Special = has b 6
      Shield = has b 7 }

let private decodePad (b: int) : MenuPad =
    { Up = has b 8
      Down = has b 9
      Left = has b 10
      Right = has b 11
      Accept = has b 12
      Cancel = has b 13 }

let private stepOnline (hostBits: int) (guestBits: int) (g: Game) =
    let ui =
        { P1 = decodePad hostBits
          P2 = decodePad guestBits
          Start = has hostBits 14 || has guestBits 14
          Escape = has hostBits 15 || has guestBits 15
          ToggleDebug = false }

    update ui (decodeInput hostBits) (decodeInput guestBits) g

/// Startzustand eines Online-Matches: auf beiden Geräten exakt identisch
let private onlineStart () =
    { Physics.initialGame with
        Screen = CharSelect
        Mode = TwoPlayer
        OnlineMatch = true
        OptionIndex = 1 }

/// Kurze Prüfsumme über alles, was das Spielgeschehen bestimmt
let private checksum (g: Game) =
    let text =
        g.Players
        |> List.map (fun p -> sprintf "%d|%.6f|%.6f|%.4f|%d|%d" p.Id p.Pos.X p.Pos.Y p.Percent p.Stocks p.StateFrame)
        |> String.concat ";"

    let mutable h = 17

    for c in text + string g.Seed + string g.Frame do
        h <- (h * 31 + int c) &&& 0x3FFFFFFF

    h

let mutable private online = false
let mutable private localSlot = 0
let mutable private netFrame = 0
let mutable private lastRemote = -1
let mutable private rollbackFrom = System.Int32.MaxValue
let mutable private pendingEdges = 0
let mutable private stalledTicks = 0
let mutable private pingMs = 0.0
let mutable private desync = false
let mutable private lastReceive = 0.0
let private localInputs = Dictionary<int, int>()
let private remoteInputs = Dictionary<int, int>()
let private usedRemote = Dictionary<int, int>()
let private stateHistory = Dictionary<int, Game>()
let private localHashes = Dictionary<int, int>()
let private remoteHashes = Dictionary<int, int>()

[<Emit("performance.now()")>]
let private now () : float = jsNative

let private resetNet (slot: int) =
    localSlot <- slot
    netFrame <- 0
    lastRemote <- inputDelay - 1
    rollbackFrom <- System.Int32.MaxValue
    pendingEdges <- 0
    stalledTicks <- 0
    desync <- false
    localInputs.Clear()
    remoteInputs.Clear()
    usedRemote.Clear()
    stateHistory.Clear()
    localHashes.Clear()
    remoteHashes.Clear()

    for f in 0 .. inputDelay - 1 do
        localInputs[f] <- 0
        remoteInputs[f] <- 0

let private receive (msg: obj) =
    lastReceive <- now ()

    match string msg?t with
    | "i" ->
        // Eingaben des Gegners (mit Wiederholung der letzten Frames gegen Verluste)
        let first: int = msg?f
        let bits: int array = msg?b

        bits
        |> Array.iteri (fun i b ->
            let f = first + i

            if not (remoteInputs.ContainsKey f) then
                remoteInputs[f] <- b

                match usedRemote.TryGetValue f with
                | true, used when used <> b -> rollbackFrom <- min rollbackFrom f
                | _ -> ())

        while remoteInputs.ContainsKey(lastRemote + 1) do
            lastRemote <- lastRemote + 1
    | "h" ->
        let f: int = msg?f
        remoteHashes[f] <- msg?h

        match localHashes.TryGetValue f with
        | true, h when h <> (msg?h: int) -> desync <- true
        | _ -> ()
    | "p" -> Net.send (createObj [ "t" ==> "q"; "s" ==> msg?s ])
    | "q" -> pingMs <- now () - (msg?s: float)
    | _ -> ()

/// Einen Frame rechnen; die Eingabe des Gegners ist bekannt oder vorhergesagt
let private simulate (f: int) (g: Game) =
    let local = match localInputs.TryGetValue f with | true, b -> b | _ -> 0

    let remote =
        match remoteInputs.TryGetValue f with
        | true, b -> b
        | _ ->
            // Vorhersage: gehaltene Tasten wie zuletzt bekannt, keine Menü-Tipper
            match remoteInputs.TryGetValue lastRemote with
            | true, b -> b &&& heldMask
            | _ -> 0

    usedRemote[f] <- remote
    stateHistory[f] <- g
    if localSlot = 0 then stepOnline local remote g else stepOnline remote local g

// ---------------------------------------------------------------------------
// Lobby (HTML-Overlay): Raum erstellen, beitreten, Einladungslink
// ---------------------------------------------------------------------------

let mutable private overlayOpen = false
let mutable private lobbyCode = ""
let mutable private hosting = false

let private lobby = document.createElement "div"
let private badge = document.createElement "div"

[<Emit("(navigator.share ? navigator.share({ title: 'SmashClone', text: $0, url: $1 }).catch(() => {}) : null)")>]
let private share (_text: string) (_url: string) : unit = jsNative

[<Emit("(navigator.clipboard ? navigator.clipboard.writeText($0).catch(() => {}) : null)")>]
let private copyText (_text: string) : unit = jsNative

[<Emit("!!navigator.share")>]
let private canShare () : bool = jsNative

[<Emit("(location.origin + location.pathname)")>]
let private pageUrl () : string = jsNative

[<Emit("(new URLSearchParams(location.search).get('raum') || '')")>]
let private roomParam () : string = jsNative

[<Emit("window.history.replaceState(null, '', location.pathname)")>]
let private clearRoomParam () : unit = jsNative

let private inviteLink (code: string) = pageUrl () + "?raum=" + code

let private escapeHtml (s: string) =
    s.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")

let private closeLobby () =
    overlayOpen <- false
    lobby.className <- "lobby"
    keysDown.Clear()
    keysPressed.Clear()

let rec private showLobby (html: string) (bind: unit -> unit) =
    overlayOpen <- true
    lobby.className <- "lobby open"
    lobby.innerHTML <- "<div class=\"lobby-card\"><h2>ONLINE-MATCH</h2>" + html + "</div>"
    bind ()

and private byId (id: string) = document.getElementById id

and private onClick (id: string) (action: unit -> unit) =
    let e = byId id

    if not (isNull e) then
        e.addEventListener ("click", fun _ -> action ())

and private lobbyMenu () =
    showLobby
        """<p>Spiel gegen einen Freund auf einem anderen Gerät.</p>
           <button id="lb-host" class="lb-main">RAUM ERSTELLEN</button>
           <div class="lb-or">oder Code eingeben</div>
           <div class="lb-row"><input id="lb-code" maxlength="4" placeholder="CODE" autocomplete="off" autocapitalize="characters" spellcheck="false" />
           <button id="lb-join">BEITRETEN</button></div>
           <button id="lb-back" class="lb-link">Zurück</button>"""
        (fun () ->
            onClick "lb-host" startHosting
            onClick "lb-join" (fun () -> joinRoom ((byId "lb-code")?value))
            onClick "lb-back" closeLobby

            let input = byId "lb-code"
            input.addEventListener ("keydown", fun (e: Event) -> if (e :?> KeyboardEvent).key = "Enter" then joinRoom input?value))

and private lobbyMessage (text: string) =
    showLobby
        (sprintf "<p>%s</p><button id=\"lb-ok\" class=\"lb-main\">OK</button>" (escapeHtml text))
        (fun () -> onClick "lb-ok" lobbyMenu)

and private startHosting () =
    hosting <- true
    lobbyCode <- Net.newCode ()
    showLobby "<p>Raum wird erstellt…</p>" ignore
    Net.host lobbyCode

and private showHostCode () =
    let link = inviteLink lobbyCode

    showLobby
        (sprintf
            """<p>Dein Raumcode</p>
               <div class="lb-code">%s</div>
               <p class="lb-small">Dein Freund öffnet das Spiel, wählt ONLINE und gibt den Code ein. Oder schick ihm direkt den Link:</p>
               <div class="lb-linkbox">%s</div>
               <div class="lb-row"><button id="lb-copy">LINK KOPIEREN</button>%s</div>
               <p class="lb-wait">Warte auf Mitspieler<span class="dots"></span></p>
               <button id="lb-cancel" class="lb-link">Abbrechen</button>"""
            lobbyCode
            (escapeHtml link)
            (if canShare () then "<button id=\"lb-share\">TEILEN</button>" else ""))
        (fun () ->
            onClick "lb-copy" (fun () ->
                copyText link
                (byId "lb-copy").textContent <- "KOPIERT ✓")
            onClick "lb-share" (fun () -> share ("Komm in meinen SmashClone-Raum " + lobbyCode + "!") link)
            onClick "lb-cancel" (fun () ->
                Net.close ()
                lobbyMenu ()))

and private joinRoom (raw: string) =
    let code = (string raw).Trim().ToUpperInvariant()

    if code.Length <> 4 then
        lobbyMessage "Der Raumcode hat 4 Zeichen."
    else
        hosting <- false
        lobbyCode <- code

        showLobby
            (sprintf "<p>Verbinde mit Raum <b>%s</b><span class=\"dots\"></span></p><button id=\"lb-cancel\" class=\"lb-link\">Abbrechen</button>" code)
            (fun () ->
                onClick "lb-cancel" (fun () ->
                    Net.close ()
                    lobbyMenu ()))

        Net.join code

let private updateBadge () =
    if online then
        let status =
            if desync then "⚠ asynchron"
            elif stalledTicks > 20 then "warte auf Mitspieler…"
            else sprintf "%.0f ms" pingMs

        badge.className <- "net-badge show"
        badge.textContent <- sprintf "ONLINE · %s · %s" (if localSlot = 0 then "P1" else "P2") status
    else
        badge.className <- "net-badge"

let private endOnline (message: string) =
    if online then
        online <- false
        Net.send (createObj [ "t" ==> "bye" ])
        Net.close ()
        game <- { Physics.initialGame with Screen = Title; MenuIndex = 2 }
        updateBadge ()
        lobbyMessage message

let private startOnline () =
    resetNet (if hosting then 0 else 1)
    lastReceive <- now ()
    game <- onlineStart ()
    online <- true
    closeLobby ()
    updateBadge ()

let private setupOnline () =
    lobby.className <- "lobby"
    document.body.appendChild lobby |> ignore
    badge.className <- "net-badge"
    document.body.appendChild badge |> ignore

    Net.onData <-
        fun msg ->
            if string msg?t = "bye" then endOnline "Dein Mitspieler hat das Online-Match verlassen."
            else receive msg

    Net.onStatus <-
        fun status ->
            match status with
            | "open" -> if hosting && not online then showHostCode ()
            | "connected" -> if not online then startOnline ()
            | "closed" -> if online then endOnline "Die Verbindung zum Mitspieler wurde getrennt." else ()
            | s when s.StartsWith "error:" ->
                let kind = s.Substring 6

                if online then
                    endOnline "Die Verbindung zum Mitspieler wurde getrennt."
                elif kind = "unavailable-id" && hosting then
                    startHosting ()
                elif kind = "peer-unavailable" then
                    Net.close ()
                    lobbyMessage (sprintf "Raum %s nicht gefunden. Stimmt der Code, und ist der Raum noch offen?" lobbyCode)
                else
                    Net.close ()
                    lobbyMessage "Keine Verbindung zum Online-Dienst. Prüfe die Internetverbindung und versuch es noch einmal."
            | _ -> ()

    // Herzschlag unabhängig von der Bildrate: erkennt abgerissene Verbindungen
    // (z. B. geschlossener Tab), auch wenn WebRTC kein "close" meldet
    window.setInterval (
        (fun () ->
            if online then
                Net.send (createObj [ "t" ==> "k" ])

                if now () - lastReceive > 10000.0 then
                    endOnline "Die Verbindung zum Mitspieler ist abgerissen."),
        1000
    )
    |> ignore

    // Einladungslink ?raum=CODE: direkt beitreten
    let code = roomParam ()

    if code <> "" then
        clearRoomParam ()
        joinRoom code

/// Ein fester Simulationsschritt im Online-Modus
let private onlineTick () =
    pollPads (game.Screen = Playing)
    let ui = readUi ()
    let p1 = readInput player1Keys
    let p2 = readInput player2Keys

    // Lokal dürfen beide Tastenbelegungen und alle lokalen Controller spielen
    let localInput =
        { Left = p1.Left || p2.Left
          Right = p1.Right || p2.Right
          Up = p1.Up || p2.Up
          Down = p1.Down || p2.Down
          Jump = p1.Jump || p2.Jump
          Attack = p1.Attack || p2.Attack
          Special = p1.Special || p2.Special
          Shield = p1.Shield || p2.Shield }

    let bits = encode localInput (merged ui) ui.Start ui.Escape
    pendingEdges <- pendingEdges ||| (bits &&& ~~~heldMask)

    // Zurückspulen, wenn eine Vorhersage falsch war
    if rollbackFrom < netFrame then
        match stateHistory.TryGetValue rollbackFrom with
        | true, past ->
            let mutable g = past

            for f in rollbackFrom .. netFrame - 1 do
                g <- simulate f g

            game <- g
        | _ -> desync <- true

    rollbackFrom <- System.Int32.MaxValue

    if netFrame - lastRemote > maxRollback then
        // Zu weit voraus: kurz auf den Gegner warten
        stalledTicks <- stalledTicks + 1
    else
        stalledTicks <- 0
        localInputs[netFrame + inputDelay] <- (bits &&& heldMask) ||| pendingEdges
        pendingEdges <- 0
        game <- simulate netFrame game
        game.Sounds |> List.iter SmashClone.Audio.play
        netFrame <- netFrame + 1

    // Eingaben senden (die letzten 8 Frames erneut, falls etwas verloren geht)
    let newest = netFrame + inputDelay - 1
    let first = max 0 (newest - 7)
    let bitsOut = [| for f in first..newest -> match localInputs.TryGetValue f with | true, b -> b | _ -> 0 |]
    Net.send (createObj [ "t" ==> "i"; "f" ==> first; "b" ==> bitsOut ])

    // Prüfsumme bestätigter Frames vergleichen
    let check = (lastRemote / 120) * 120

    if check > 0 && not (localHashes.ContainsKey check) then
        match stateHistory.TryGetValue(check + 1) with
        | true, state when check + 1 <= netFrame ->
            let h = checksum state
            localHashes[check] <- h
            Net.send (createObj [ "t" ==> "h"; "f" ==> check; "h" ==> h ])

            match remoteHashes.TryGetValue check with
            | true, r when r <> h -> desync <- true
            | _ -> ()
        | _ -> ()

    if netFrame % 30 = 0 && stalledTicks = 0 then
        Net.send (createObj [ "t" ==> "p"; "s" ==> now () ])

    // Alte Daten aufräumen
    let old = netFrame - maxRollback - 40

    if old > 0 && old % 20 = 0 then
        for f in old - 20 .. old do
            stateHistory.Remove f |> ignore
            usedRemote.Remove f |> ignore
            localInputs.Remove f |> ignore
            remoteInputs.Remove f |> ignore

    keysPressed.Clear()
    updateBadge ()

    // Zurück im Hauptmenü: Online-Match beenden
    if game.Screen = Title then
        endOnline "Online-Match beendet."

let rec private frame (time: float) =
    // Große Zeitsprünge (Tab im Hintergrund) kappen, damit keine "Spiral of Death" entsteht
    let delta = min 0.25 ((time - lastTime) / 1000.0)
    lastTime <- time
    accumulator <- accumulator + delta

    // Die Simulation läuft immer in exakt 1/60-Sekunden-Schritten, egal ob 60Hz oder 144Hz
    while accumulator >= fixedDeltaTime do
        if online then
            onlineTick ()
        elif overlayOpen then
            // Lobby offen: Spiel steht, Tastatur gehört den Eingabefeldern
            keysPressed.Clear()
        else
            pollPads (game.Screen = Playing)
            game <- update (readUi ()) (readInput player1Keys) (readInput player2Keys) game
            game.Sounds |> List.iter SmashClone.Audio.play
            keysPressed.Clear()

            if game.OpenOnline then
                game <- { game with OpenOnline = false }
                lobbyMenu ()

        accumulator <- accumulator - fixedDeltaTime

    let scale = pixelScale ()
    ctx.setTransform (scale, 0.0, 0.0, scale, 0.0, 0.0)
    Renderer.draw ctx game
    window.requestAnimationFrame frame |> ignore

let private start (time: float) =
    lastTime <- time
    window.requestAnimationFrame frame |> ignore

resizeCanvas ()
window.addEventListener ("resize", fun _ -> resizeCanvas ())
setupTouch ()
setupOnline ()
window.addEventListener ("keydown", onKeyDown)
window.addEventListener ("keyup", onKeyUp)

window.addEventListener (
    "blur",
    fun _ ->
        keysDown.Clear()
        keysPressed.Clear()
        padDown.Clear()
)

window.requestAnimationFrame start |> ignore
