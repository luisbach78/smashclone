module SmashClone.AI

open SmashClone.Types

// ---------------------------------------------------------------------------
// Zustandsbasierte CPU-Steuerung mit Schwierigkeitsprofilen.
// Die KI erzeugt dieselbe Input-Struktur wie ein menschlicher Spieler und
// unterliegt damit exakt denselben Regeln (inklusive Energie und Stale-Moves).
// Menschlich wird sie durch
//  * Reaktionsverzögerung: Sie sieht den Gegner so, wie er vor N Frames war,
//  * Denkpausen: Sie entscheidet nur alle paar Frames neu und hält dazwischen die Eingabe,
//  * Fehler: Sie schlägt daneben, zögert, schildet selten und recovert manchmal zu spät.
// ---------------------------------------------------------------------------

type Profile =
    { ReactMin: int
      ReactMax: int
      ThinkMin: int
      ThinkMax: int
      CooldownMin: int
      CooldownMax: int
      /// Wahrscheinlichkeit, außerhalb der Reichweite oder in die falsche Richtung anzugreifen
      Whiff: float
      /// Wahrscheinlichkeit, einfach stehen zu bleiben (Lücke zum Kontern)
      Hesitate: float
      Shield: float
      Dodge: float
      /// Wahrscheinlichkeit, das Recovery zu spät zu starten
      LateRecover: float
      /// 0 = kein Edgeguard, 1 = wartet an der Kante, 2 = jagt offstage hinterher
      EdgeGuard: int
      Combo: bool
      /// Wahrscheinlichkeit pro Entscheidung, aus der Distanz Projektile zu nutzen
      Zone: float
      SmashBias: float
      /// Frames, die die CPU mit dem Ultra-Smash zögert
      UltraDelay: int }

let profile (d: Difficulty) : Profile =
    match d with
    | Easy ->
        { ReactMin = 18
          ReactMax = 30
          ThinkMin = 10
          ThinkMax = 16
          CooldownMin = 55
          CooldownMax = 95
          Whiff = 0.45
          Hesitate = 0.3
          Shield = 0.0
          Dodge = 0.0
          LateRecover = 0.4
          EdgeGuard = 0
          Combo = false
          Zone = 0.04
          SmashBias = 0.12
          UltraDelay = 240 }
    | Medium ->
        { ReactMin = 10
          ReactMax = 16
          ThinkMin = 5
          ThinkMax = 9
          CooldownMin = 30
          CooldownMax = 56
          Whiff = 0.22
          Hesitate = 0.12
          Shield = 0.04
          Dodge = 0.0
          LateRecover = 0.15
          EdgeGuard = 1
          Combo = false
          Zone = 0.08
          SmashBias = 0.2
          UltraDelay = 120 }
    | Hard ->
        { ReactMin = 5
          ReactMax = 8
          ThinkMin = 2
          ThinkMax = 4
          CooldownMin = 16
          CooldownMax = 32
          Whiff = 0.07
          Hesitate = 0.03
          Shield = 0.12
          Dodge = 0.04
          LateRecover = 0.03
          EdgeGuard = 1
          Combo = true
          Zone = 0.14
          SmashBias = 0.3
          UltraDelay = 40 }
    | Pro ->
        { ReactMin = 2
          ReactMax = 4
          ThinkMin = 0
          ThinkMax = 1
          CooldownMin = 6
          CooldownMax = 16
          Whiff = 0.0
          Hesitate = 0.0
          Shield = 0.22
          Dodge = 0.1
          LateRecover = 0.0
          EdgeGuard = 2
          Combo = true
          Zone = 0.18
          SmashBias = 0.38
          UltraDelay = 10 }

let initialBrain (seed: int) : AiBrain =
    { Mode = AiChase
      AttackCooldown = 40
      JumpHold = 0
      ShieldHold = 0
      Seed = seed
      Memory = []
      ReactDelay = 12
      Think = 0
      Held = Input.none
      RecoverLate = 0.0 }

/// Zufallswerte, die durch eine Entscheidung durchgereicht werden
type private Dice = { Seed: int }

let private roll (d: Dice) : Dice * float =
    let s, r = nextRandom d.Seed
    { Seed = s }, r

let private rollInt (lo: int) (hi: int) (d: Dice) : Dice * int =
    let d2, r = roll d
    d2, lo + int (r * float (hi - lo + 1)) |> min hi

let private isOffStageX (l: Layout) (x: float) = x < l.Left - 6.0 || x > l.Right + 6.0

let private isAirborneOffStage (l: Layout) (p: Player) =
    not p.OnGround && p.State <> LedgeHang && (isOffStageX l p.Pos.X || p.Pos.Y > l.Top + 4.0)

let private canAct (p: Player) =
    match p.State with
    | Idle
    | Running
    | Jumping
    | Falling -> true
    | _ -> false

let private isAttacking (p: Player) =
    match p.State with
    | AttackStartup _
    | AttackActive _ -> true
    | _ -> false

let private walk (dir: float) (i: Input) =
    if dir > 0.0 then { i with Right = true }
    elif dir < 0.0 then { i with Left = true }
    else i

let private sign (v: float) =
    if v > 0.0 then 1.0
    elif v < 0.0 then -1.0
    else 0.0

/// Mindestenergie, ab der die CPU Fernkampf einsetzt (teuerster Fernkampf-Special kostet 45)
let private zoneEnergy = 46.0

/// Eingabe für den Fernkampf-Angriff eines Kämpfers (dir = Richtung zum Gegner)
let private rangedInput (f: Fighter) (onGround: bool) (facing: float) (dir: float) (r: float) : Input option =
    let side = { walk dir Input.none with Special = true }
    let neutral = { Input.none with Special = true }

    match f with
    | Aether
    | Chrono
    | Bramble
    | Ronin
    | Specter -> Some side
    | Lyra -> if facing = dir && r < 0.35 then Some neutral else Some side
    | Gorm -> None
    | Ignis
    | Valkyrie -> if facing = dir then Some neutral else None
    | Titan -> if onGround then Some { Input.none with Special = true; Down = true } elif facing = dir then Some neutral else None
    | Zephyr -> if facing = dir && r < 0.6 then Some neutral else Some side

/// Nahkampf-Special zum Herankommen (Dash, Phantom-Hieb)
let private approachInput (f: Fighter) (dir: float) (facing: float) : Input option =
    match f with
    | Ignis
    | Titan
    | Gorm
    | Valkyrie -> Some { walk dir Input.none with Special = true }
    | Specter -> if facing = dir then Some { Input.none with Special = true } else None
    | _ -> None

let private approachRange (f: Fighter) =
    match f with
    | Specter -> 250.0
    | Ignis -> 280.0
    | Valkyrie -> 240.0
    | Titan -> 220.0
    | Gorm -> 90.0
    | _ -> 0.0

/// Fighter mit seitlichem Dash als Recovery-Hilfe
let private hasDash (f: Fighter) = f = Ignis || f = Titan || f = Valkyrie

/// Up-B mit freier Richtungswahl (Teleport, Schattenflug)
let private aimedUpB (f: Fighter) = f = Zephyr || f = Chrono || f = Specter

let private chooseMode (prof: Profile) (l: Layout) (self: Player) (foe: Player) (ball: SmashBall option) : AiState =
    if self.State = LedgeHang || isAirborneOffStage l self then AiRecover
    elif foe.State = KO || foe.State = Respawning || foe.Invuln > 24 then AiEvade
    elif prof.EdgeGuard > 0 && (isAirborneOffStage l foe || foe.State = LedgeHang) then AiEdgeGuard
    else
        match ball with
        | Some b when not self.HasFinal && abs (b.Pos.X - self.Pos.X) < 420.0 && b.Pos.X > l.Left - 40.0 && b.Pos.X < l.Right + 40.0 ->
            AiHuntBall
        | _ -> AiChase

/// Zur Stage zurückfinden: Driften, Doppelsprünge, Side-Special und Up-B
let private recover (l: Layout) (dice: Dice) (brain: AiBrain) (self: Player) : Dice * Input =
    let stats = fighterStats self.Fighter
    let center = stageCenter l
    let toStage = if self.Pos.X < center then 1.0 else -1.0
    let dice, r1 = roll dice

    if self.State = LedgeHang then
        if self.StateFrame > 14 + int (r1 * 10.0) then
            let dice, r2 = roll dice
            dice, (if r2 < 0.45 then { Input.none with Up = true } else walk toStage Input.none)
        else
            dice, Input.none
    else
        let underStage =
            self.Pos.X > l.Left - stats.Width / 2.0
            && self.Pos.X < l.Right + stats.Width / 2.0
            && self.Pos.Y > l.Top + 30.0

        let dir = if underStage then -toStage else toStage
        let baseInput = walk dir Input.none
        let falling = self.Vel.Y > 0.5
        let free = self.State = Jumping || self.State = Falling
        let edgeX = if toStage > 0.0 then l.Left else l.Right
        let distX = abs (self.Pos.X - edgeX)
        let late = brain.RecoverLate

        if free && falling && self.JumpsLeft > 0 && self.Pos.Y > l.Top - 40.0 + late then
            dice, { baseInput with Up = not self.Prev.Up }
        elif free && self.JumpsLeft = 0 && hasDash self.Fighter && not self.SideUsed && distX > 190.0 && self.Pos.Y < l.Top + 70.0 then
            dice, { baseInput with Special = not self.Prev.Special }
        elif free && falling && self.JumpsLeft = 0 && self.Pos.Y > l.Top + 12.0 + late * 0.6 then
            // Gerichtete Up-Bs zielen schräg zur Kante, alle anderen schießen nach oben
            let aimed = if aimedUpB self.Fighter then walk toStage Input.none else baseInput
            dice, { aimed with Up = true; Special = not self.Prev.Special }
        elif self.Fighter = Chrono && free && falling && self.JumpsLeft = 0 && self.Float > 0 then
            // Chrono schwebt zur Kante
            dice, { baseInput with Up = true }
        else
            dice, baseInput

/// Gegner ist unverwundbar: Abstand halten, notfalls schilden
let private evade (prof: Profile) (l: Layout) (dice: Dice) (self: Player) (foe: Player) : Dice * Input =
    let awaySide = if self.Pos.X >= foe.Pos.X then 1.0 else -1.0
    let wanted = foe.Pos.X + awaySide * 280.0
    let target = max (l.Left + 70.0) (min (l.Right - 70.0) wanted)
    let dx = target - self.Pos.X
    let close = abs (foe.Pos.X - self.Pos.X) < 100.0 && abs (foe.Pos.Y - self.Pos.Y) < 70.0
    let dice, r = roll dice

    if self.State = Respawning then
        dice, (if self.StateFrame > 70 then walk (sign (stageCenter l - self.Pos.X)) Input.none else Input.none)
    elif close && isAttacking foe && self.OnGround && r < prof.Shield * 3.0 then
        dice, { Input.none with Shield = true }
    elif abs dx > 14.0 then
        dice, walk (sign dx) Input.none
    else
        dice, Input.none

/// Gegner ist offstage: an der Kante warten (Mittel/Schwer) oder hinterherjagen (Profi)
let private edgeGuard (prof: Profile) (l: Layout) (dice: Dice) (brain: AiBrain) (self: Player) (foe: Player) : Dice * AiBrain * Input =
    let center = stageCenter l
    let edgeX = if foe.Pos.X < center then l.Left + 46.0 else l.Right - 46.0
    let dxEdge = edgeX - self.Pos.X
    let dx = foe.Pos.X - self.Pos.X
    let dy = foe.Pos.Y - self.Pos.Y
    let dir = sign dx
    let ready = brain.AttackCooldown = 0 && canAct self && foe.Invuln = 0
    let dice, r1 = roll dice
    let dice, cd = rollInt prof.CooldownMin prof.CooldownMax dice

    if ready && self.OnGround && abs dx < 96.0 && abs dy < 64.0 then
        let input =
            if foe.State = LedgeHang || dy > 24.0 then { walk dir Input.none with Down = true; Attack = true }
            elif r1 < 0.5 then { Input.none with Special = true }
            else { walk dir Input.none with Attack = true }

        dice, { brain with AttackCooldown = cd }, input
    elif ready && self.OnGround && self.Energy >= zoneEnergy && abs dx > 140.0 && abs dx < 460.0 && abs dy < 120.0 && r1 < prof.Zone * 2.0 then
        // Projektile auf den zurückkehrenden Gegner (nur mit genug Energie)
        match rangedInput self.Fighter true self.Facing dir r1 with
        | Some input -> dice, { brain with AttackCooldown = cd }, input
        | None -> dice, brain, walk dir Input.none
    elif prof.EdgeGuard >= 2 && ready && self.OnGround && abs dx < 200.0 && dy > 40.0 && dy < 220.0 && foe.State <> LedgeHang then
        // Profi: abspringen und spiken
        dice, { brain with JumpHold = 0 }, { walk dir Input.none with Up = not self.Prev.Up }
    elif prof.EdgeGuard >= 2 && not self.OnGround && ready && abs dx < 46.0 && dy > 20.0 && dy < 120.0 && self.JumpsLeft > 0 then
        dice, { brain with AttackCooldown = cd }, { Input.none with Down = true; Attack = true }
    elif not self.OnGround && self.Pos.Y < l.Top - 4.0 && not (isOffStageX l self.Pos.X) then
        dice, brain, walk (sign dxEdge) Input.none
    elif not self.OnGround && isOffStageX l self.Pos.X then
        dice, brain, { walk (sign (center - self.Pos.X)) Input.none with Up = not self.Prev.Up && self.Vel.Y > 0.0 }
    elif abs dxEdge > 10.0 then
        dice, brain, walk (sign dxEdge) Input.none
    else
        dice, brain, Input.none

/// Smash-Ball jagen: hinlaufen, hochspringen und draufhauen
let private huntBall (l: Layout) (dice: Dice) (brain: AiBrain) (self: Player) (b: SmashBall) : Dice * AiBrain * Input =
    let center = { X = self.Pos.X; Y = self.Pos.Y - (fighterStats self.Fighter).Height / 2.0 }
    let dx = b.Pos.X - center.X
    let dy = b.Pos.Y - center.Y
    let dir = sign dx
    let ready = brain.AttackCooldown = 0 && canAct self
    let dice, cd = rollInt 10 22 dice

    if ready && abs dx < 70.0 && abs dy < 60.0 then
        let input = if dy < -25.0 then { Input.none with Up = true; Attack = true } else { walk dir Input.none with Attack = true }
        dice, { brain with AttackCooldown = cd }, input
    elif dy < -70.0 && abs dx < 160.0 && canAct self && (self.OnGround || self.JumpsLeft > 0 && self.Vel.Y > 0.0) then
        dice, { brain with JumpHold = 14 }, { walk dir Input.none with Up = not self.Prev.Up }
    elif self.OnGround then
        let nextX = self.Pos.X + dir * 30.0
        let safe = nextX > l.Left + 16.0 && nextX < l.Right - 16.0 || self.Pos.Y < l.Top - 10.0
        dice, brain, (if safe then walk dir Input.none else Input.none)
    else
        dice, brain, walk dir Input.none

/// Verfolgen, auf Plattformen springen, Distanz-Zoning und Angreifen
let private chase (prof: Profile) (l: Layout) (dice: Dice) (brain: AiBrain) (self: Player) (foe: Player) : Dice * AiBrain * Input =
    let dx = foe.Pos.X - self.Pos.X
    let dy = foe.Pos.Y - self.Pos.Y
    let adx = abs dx
    let dir = sign dx
    let stats = fighterStats self.Fighter
    let reach = stats.Reach
    let ready = brain.AttackCooldown = 0 && canAct self && foe.Invuln = 0
    let dice, r1 = roll dice
    let dice, r2 = roll dice
    let dice, r3 = roll dice
    let dice, cd = rollInt prof.CooldownMin prof.CooldownMax dice
    // Danebenschlagen: Reichweite wird überschätzt
    let slop = if r3 < prof.Whiff then 1.0 + prof.Whiff * 2.4 else 1.0
    let range = 80.0 * reach * slop
    let canZone = self.Energy >= zoneEnergy

    if r1 < prof.Hesitate && self.OnGround && brain.ShieldHold = 0 then
        dice, { brain with AttackCooldown = max brain.AttackCooldown 8 }, Input.none
    elif self.OnGround then
        if brain.ShieldHold > 0 then
            dice, brain, { Input.none with Shield = true }
        elif isAttacking foe && adx < 120.0 && abs dy < 64.0 && canAct self && r1 < prof.Shield then
            dice, { brain with ShieldHold = 14 + int (r2 * 10.0) }, { Input.none with Shield = true }
        elif isAttacking foe && adx < 90.0 && abs dy < 50.0 && canAct self && r1 < prof.Shield + prof.Dodge then
            dice, brain, { Input.none with Shield = true; Down = true }
        elif ready && adx < range && abs dy < 40.0 then
            let smashChance = if foe.Percent > 70.0 then prof.SmashBias * 1.8 else prof.SmashBias
            // Beim Danebenschlagen schaut die CPU manchmal in die falsche Richtung
            let facing = if r3 < prof.Whiff * 0.5 then -dir else dir

            let input =
                if r2 < smashChance then
                    if self.Facing = facing then { Input.none with Special = true }
                    elif r2 < smashChance * 0.5 then { Input.none with Special = true; Down = true }
                    else { walk facing Input.none with Attack = true }
                elif r2 < 0.62 then { walk facing Input.none with Attack = true }
                elif r2 < 0.8 then { walk facing Input.none with Down = true; Attack = true }
                else { Input.none with Attack = true }

            dice, { brain with AttackCooldown = cd }, input
        elif ready && adx < 48.0 * reach && dy < -44.0 && dy > -150.0 then
            dice, { brain with AttackCooldown = cd }, { Input.none with Up = true; Attack = true }
        elif ready && canZone && adx > 170.0 && adx < 520.0 && abs dy < 70.0 && r2 < prof.Zone then
            match rangedInput self.Fighter true self.Facing dir r3 with
            | Some input -> dice, { brain with AttackCooldown = cd }, input
            | None -> dice, brain, walk dir Input.none
        elif ready && adx > 110.0 && adx < approachRange self.Fighter && abs dy < 30.0 && r2 < prof.Zone * 1.4 then
            match approachInput self.Fighter dir self.Facing with
            | Some input -> dice, { brain with AttackCooldown = cd }, input
            | None -> dice, brain, walk dir Input.none
        elif dy < -70.0 && adx < 240.0 && canAct self then
            dice, { brain with JumpHold = 12 }, { walk dir Input.none with Up = not self.Prev.Up }
        elif dy > 60.0 && self.Pos.Y < l.Top - 10.0 && adx < 220.0 && canAct self then
            dice, brain, { walk dir Input.none with Down = not self.Prev.Down }
        elif adx > 58.0 * reach then
            // Nicht über die Kante laufen
            let nextX = self.Pos.X + dir * 30.0
            let safe = nextX > l.Left + 16.0 && nextX < l.Right - 16.0 || self.Pos.Y < l.Top - 10.0
            dice, brain, (if safe then walk dir Input.none else Input.none)
        else
            dice, brain, Input.none
    else
        let overStage = not (isOffStageX l self.Pos.X)
        let comboing = prof.Combo && foe.State = Hitstun

        if ready && adx < 74.0 * reach * slop && abs dy < 50.0 then
            let input =
                if r2 < 0.2 then { Input.none with Special = true }
                elif r2 < 0.75 then { walk dir Input.none with Attack = true }
                else { Input.none with Attack = true }

            dice, { brain with AttackCooldown = (if comboing then min cd 6 else cd) }, input
        elif ready && adx < 46.0 * reach && dy < -36.0 && dy > -130.0 then
            dice, { brain with AttackCooldown = (if comboing then min cd 6 else cd) }, { Input.none with Up = true; Attack = true }
        elif ready && adx < 34.0 && dy > 44.0 && dy < 160.0 && overStage then
            dice, { brain with AttackCooldown = cd + 10 }, { Input.none with Down = true; Attack = true }
        elif (dy < -90.0 || comboing && dy < -30.0) && self.JumpsLeft > 0 && self.Vel.Y > 0.0 && adx < 180.0 && canAct self then
            dice, { brain with JumpHold = 0 }, { walk dir Input.none with Up = not self.Prev.Up }
        elif not overStage then
            dice, brain, walk (sign (stageCenter l - self.Pos.X)) Input.none
        else
            dice, brain, walk dir Input.none

/// Lohnt sich der Ultra-Smash gerade? (Gegner in Reichweite der jeweiligen Ultra-Form)
let private ultraWorthIt (self: Player) (foe: Player) =
    let dy = abs (foe.Pos.Y - self.Pos.Y)
    let dx = abs (foe.Pos.X - self.Pos.X)
    let foeOk = foe.Invuln = 0 && foe.State <> KO && foe.State <> Respawning

    foeOk
    && (match self.Fighter with
        | Aether
        | Ronin
        | Ignis -> dy < 80.0
        | Chrono -> dx < 480.0 && dy < 300.0
        | Specter -> dx < 600.0
        | _ -> true)

/// Ein Entscheidungsschritt der CPU (einmal pro 60Hz-Simulationsschritt)
let private decideRaw (difficulty: Difficulty) (brain: AiBrain) (self: Player) (foe: Player) (l: Layout) (ball: SmashBall option) : AiBrain * Input =
    let prof = profile difficulty
    let memory = foe :: List.truncate 48 brain.Memory
    // Der Gegner, wie die CPU ihn mit Verzögerung wahrnimmt
    let seen = List.item (min (List.length memory - 1) brain.ReactDelay) memory

    let ticked =
        { brain with
            Memory = memory
            AttackCooldown = max 0 (brain.AttackCooldown - 1)
            JumpHold = max 0 (brain.JumpHold - 1)
            ShieldHold = max 0 (brain.ShieldHold - 1) }

    let stuckOnLedge = self.State = LedgeHang && brain.Think > 0 && brain.Held = Input.none

    if brain.Think > 0 && not stuckOnLedge then
        // Denkpause: Eingabe halten, Tasten nur einmal antippen, nicht über die Kante laufen
        let held = brain.Held

        let safe =
            if self.OnGround && self.Pos.Y >= l.Top - 1.0 then
                { held with
                    Left = held.Left && self.Pos.X > l.Left + 24.0
                    Right = held.Right && self.Pos.X < l.Right - 24.0 }
            else
                held

        let output =
            { safe with
                Attack = false
                Special = false
                Up = safe.Up && (ticked.JumpHold > 0 || self.Prev.Up) }

        { ticked with Think = brain.Think - 1; Held = output }, output
    elif self.HasFinal && canAct self && self.StateFrame > prof.UltraDelay % 60 && ultraWorthIt self seen && not self.Prev.Special then
        // Ultra-Smash auslösen
        let input = { Input.none with Special = true }
        { ticked with Think = 2; Held = Input.none }, input
    else
        let mode = chooseMode prof l self seen ball
        let dice = { Seed = brain.Seed }
        let dice, rLate = roll dice
        let dice, rAmount = roll dice

        // Beim Wechsel ins Recovery wird ausgewürfelt, ob die CPU zu spät reagiert
        let late =
            if mode = AiRecover && brain.Mode <> AiRecover then
                if rLate < prof.LateRecover then 50.0 + rAmount * 110.0 else rAmount * 12.0
            elif mode = AiRecover then
                brain.RecoverLate
            else
                0.0

        let ticked = { ticked with Mode = mode; RecoverLate = late }

        let dice, next, input =
            match mode with
            | AiRecover ->
                let d, i = recover l dice ticked self
                d, { ticked with JumpHold = 0; ShieldHold = 0 }, i
            | AiEvade ->
                let d, i = evade prof l dice self seen
                d, ticked, i
            | AiEdgeGuard -> edgeGuard prof l dice ticked self seen
            | AiHuntBall ->
                match ball with
                | Some b -> huntBall l dice ticked self b
                | None -> chase prof l dice ticked self seen
            | AiChase -> chase prof l dice ticked self seen

        // Sprungtaste halten, damit aus dem Jump-Squat ein Full-Hop wird
        let held =
            if next.JumpHold > 0 && (mode = AiChase || mode = AiHuntBall) && not input.Attack && not input.Special then
                { input with Up = true }
            else
                input

        let dice, think = rollInt prof.ThinkMin prof.ThinkMax dice
        // In akuter Not (Recovery) denkt auch die schwache CPU etwas schneller
        let think = if mode = AiRecover then think / 2 else think
        let dice, react = rollInt prof.ReactMin prof.ReactMax dice

        { next with
            Seed = dice.Seed
            Think = think
            Held = held
            ReactDelay = react },
        held

/// Gefahrenzone eines Stage-Ereignisses (x-Bereich) für die Ausweich-Logik
let private hazardThreat (l: Layout) (h: Hazard) (self: Player) : float option =
    let warn, _ = hazardTiming h.Kind

    match h.Kind with
    | WindGust -> None
    | LaserSweep ->
        // Laser: kurz vor dem Kontakt springen
        let t = h.Age - warn
        if t >= 0 && self.OnGround && abs (laserX l h t - self.Pos.X) < 95.0 then Some 0.0 else None
    | _ ->
        h.Xs
        |> List.tryFind (fun x -> abs (self.Pos.X - x) < 95.0)
        |> Option.map (fun x -> if self.Pos.X >= x then 1.0 else -1.0)

let private wantsItem (it: Item) =
    it.OnGround && it.Kind <> Bomb

/// Die CPU springt wie auf der Tastatur über "Oben" (Tap-Jump), außer sie greift im selben Frame an.
/// Zusätzlich: Griffe, Würfe, Befreien, Items einsammeln, Bomben und Stage-Ereignissen ausweichen.
let decide
    (difficulty: Difficulty)
    (brain: AiBrain)
    (self: Player)
    (foe: Player)
    (l: Layout)
    (ball: SmashBall option)
    (items: Item list)
    (hazard: Hazard option)
    : AiBrain * Input =
    let b, raw = decideRaw difficulty brain self foe l ball
    let i = { raw with Jump = raw.Up && not raw.Attack && not raw.Special }
    let skill = match difficulty with Easy -> 0 | Medium -> 1 | Hard -> 2 | Pro -> 3
    let rhythm = self.StateFrame + foe.StateFrame + int self.Pos.X
    let dx = foe.Pos.X - self.Pos.X
    let toward = if dx >= 0.0 then 1.0 else -1.0

    let output =
        match self.State with
        | Held ->
            // Hämmern, um sich zu befreien
            let every = [| 6; 4; 2; 2 |][skill]
            { Input.none with Attack = self.StateFrame % every = 0; Left = self.StateFrame % (every * 2) = 0 }
        | Holding when self.StateFrame >= 8 ->
            // Wurf: hoch bei hohem Prozentwert, sonst Richtung nächster Kante oder runter für Combos
            let edgeDir = if self.Pos.X - l.Left < l.Right - self.Pos.X then -1.0 else 1.0
            let nearEdge = min (self.Pos.X - l.Left) (l.Right - self.Pos.X) < 160.0

            if foe.Percent > 110.0 && not nearEdge then { Input.none with Up = true }
            elif nearEdge || foe.Percent > 70.0 then walk edgeDir Input.none
            elif skill >= 2 then { Input.none with Down = true }
            else walk self.Facing Input.none
        | Holding -> Input.none
        | _ ->
            let threat = hazard |> Option.bind (fun h -> hazardThreat l h self)
            let armedBomb = items |> List.tryFind (fun it -> it.Kind = Bomb && it.Age >= bombArmFrames && abs (it.Pos.X - self.Pos.X) < 70.0 && abs (it.Pos.Y - self.Pos.Y) < 40.0)

            let pickup =
                items
                |> List.filter wantsItem
                |> List.filter (fun it -> abs (it.Pos.X - self.Pos.X) < 280.0 && abs (it.Pos.Y - self.Pos.Y) < 120.0)
                |> List.sortBy (fun it -> abs (it.Pos.X - self.Pos.X))
                |> List.tryHead

            match threat, armedBomb with
            | Some 0.0, _ when skill >= 1 || rhythm % 3 = 0 -> { i with Jump = true; Up = true; Attack = false }
            | Some away, _ when skill >= 1 && self.OnGround && rhythm % 4 <> 0 -> walk away Input.none
            | _, Some bomb when skill >= 1 -> walk (if self.Pos.X >= bomb.Pos.X then 1.0 else -1.0) Input.none
            | _ ->
                // Ronin kontert heranstürmende Nahkampfangriffe, sonst Griffe gegen Schilde
                let foeSwinging =
                    match foe.State with
                    | AttackStartup m -> m <> SideSpecial && m <> UpSpecial
                    | _ -> false

                let canCounter =
                    self.Fighter = Ronin && foeSwinging && abs dx < 95.0 && skill >= 1 && rhythm % (5 - skill) = 0
                    && (self.State = Idle || self.State = Running || self.State = Falling || self.State = Jumping)

                let grabTarget = foe.State = Shielding || (foe.State = Landing && skill >= 2)

                if canCounter then
                    { Input.none with Special = true; Down = true }
                elif self.OnGround && grabTarget && abs dx < 70.0 && abs (foe.Pos.Y - self.Pos.Y) < 20.0 && skill >= 1 && rhythm % (5 - skill) = 0 && not self.Prev.Attack then
                    { Input.none with Shield = true; Attack = true; Left = toward < 0.0; Right = toward > 0.0 }
                else
                    match pickup with
                    | Some it when abs dx > 200.0 && self.OnGround && (self.State = Idle || self.State = Running) ->
                        walk (if it.Pos.X >= self.Pos.X then 1.0 else -1.0) Input.none
                    | _ -> i

    b, output
