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

let private onKeyDown (e: Event) =
    let k = e :?> KeyboardEvent
    let id = keyId k
    SmashClone.Audio.unlock ()

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
    let moved = menuMove pad 3 g

    // Links/Rechts ändert die CPU-Stärke, egal welche Zeile gewählt ist
    let tuned =
        if pad.Left || pad.Right then shiftDifficulty (if pad.Right then 1 else -1) moved
        else moved

    if ui.Start || pad.Accept then
        match tuned.MenuIndex with
        | 0 -> toCharSelect VsCpu tuned
        | 1 -> toCharSelect TwoPlayer tuned
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

let rec private frame (time: float) =
    // Große Zeitsprünge (Tab im Hintergrund) kappen, damit keine "Spiral of Death" entsteht
    let delta = min 0.25 ((time - lastTime) / 1000.0)
    lastTime <- time
    accumulator <- accumulator + delta

    // Die Simulation läuft immer in exakt 1/60-Sekunden-Schritten, egal ob 60Hz oder 144Hz
    while accumulator >= fixedDeltaTime do
        pollPads (game.Screen = Playing)
        game <- update (readUi ()) (readInput player1Keys) (readInput player2Keys) game
        game.Sounds |> List.iter SmashClone.Audio.play
        keysPressed.Clear()
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
