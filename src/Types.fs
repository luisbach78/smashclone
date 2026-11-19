module SmashClone.Types

// ---------------------------------------------------------------------------
// Grundlegende Geometrie
// ---------------------------------------------------------------------------

type Vec2 = { X: float; Y: float }

module Vec2 =
    let zero = { X = 0.0; Y = 0.0 }
    let create x y = { X = x; Y = y }
    let add a b = { X = a.X + b.X; Y = a.Y + b.Y }
    let scale k v = { X = v.X * k; Y = v.Y * k }
    let length v = sqrt (v.X * v.X + v.Y * v.Y)

    let normalize v =
        let l = length v
        if l < 0.0001 then zero else { X = v.X / l; Y = v.Y / l }

type Rect =
    { Left: float
      Top: float
      Width: float
      Height: float }

module Rect =
    let overlaps a b =
        a.Left < b.Left + b.Width
        && a.Left + a.Width > b.Left
        && a.Top < b.Top + b.Height
        && a.Top + a.Height > b.Top

    /// Mittelpunkt der Schnittfläche zweier Rechtecke (Kontaktpunkt für Effekte)
    let overlapCenter a b =
        let l = max a.Left b.Left
        let r = min (a.Left + a.Width) (b.Left + b.Width)
        let t = max a.Top b.Top
        let bt = min (a.Top + a.Height) (b.Top + b.Height)
        { X = (l + r) / 2.0; Y = (t + bt) / 2.0 }

// ---------------------------------------------------------------------------
// Spielzustand: Discriminated Unions
// ---------------------------------------------------------------------------

type GameScreen =
    | Title
    | CharSelect
    | StageSelect
    | Playing
    | GameOver

type GameMode =
    | VsCpu
    | TwoPlayer

type Difficulty =
    | Easy
    | Medium
    | Hard
    | Pro

type Fighter =
    | Aether
    | Ignis
    | Titan
    | Zephyr
    | Valkyrie
    | Chrono
    | Bramble
    | Specter
    | Ronin
    | Lyra
    | Gorm

type StageId =
    | SkyBattlefield
    | AstralAltar
    | MagmaFoundry
    | FrostPeak
    | NeonRooftop

type MoveId =
    | Jab
    | ForwardTilt
    | UpTilt
    | DownTilt
    | ForwardSmash
    | DownSmash
    | NeutralAir
    | ForwardAir
    | UpAir
    | DownAir
    | AirSmash
    | UpSpecial
    | SideSpecial
    | DownSpecial

type PlayerActionState =
    | Idle
    | Running
    | JumpSquat
    | Jumping
    | Falling
    | Landing
    | AttackStartup of MoveId
    | AttackActive of MoveId
    | AttackRecovery of MoveId
    | Hitstun
    | Shielding
    | Dodging
    | LedgeHang
    | Helpless
    | Respawning
    | KO
    /// Griff: Anlauf, Festhalten, Gehaltenwerden
    | Grabbing
    | Holding
    | Held

type Sfx =
    | SfxCharge
    | SfxGrab
    | SfxThrow
    | SfxParry
    | SfxItemSpawn
    | SfxPickup
    | SfxHeal
    | SfxExplosion
    | SfxWarning
    | SfxHazard
    | SfxJump
    | SfxDoubleJump
    | SfxLand
    | SfxSwing
    | SfxUpSpecial
    | SfxHit
    | SfxHeavyHit
    | SfxShield
    | SfxShieldHit
    | SfxShieldBreak
    | SfxDodge
    | SfxLedge
    | SfxKO
    | SfxCountdown
    | SfxGo
    | SfxMenuMove
    | SfxMenuSelect
    | SfxMenuBack
    | SfxPick
    | SfxVictory
    | SfxProjectile
    | SfxTeleport
    | SfxArmor
    | SfxShockwave
    | SfxCombo
    | SfxDash
    | SfxFizzle
    | SfxReflect
    | SfxStasis
    | SfxRift
    | SfxBallAppear
    | SfxBallHit
    | SfxBallBreak
    | SfxUltraCutIn
    | SfxUltraBlast

type ParticleKind =
    | Spark
    | Smoke
    | Dust
    | Ember
    | Ring
    | Flame
    | Bolt
    | SpeedLine
    | Burst
    | Leaf
    | Shadow
    | Rainbow

type ProjectileKind =
    | BladeBeam
    | Kunai
    | EnergyOrb
    | Shockwave
    | Fireball
    | Boulder
    | LightSpear
    | StasisOrb
    | Thorn
    | ShadowDagger
    | WindSlash
    | Arrow

type AiState =
    | AiChase
    | AiEdgeGuard
    | AiRecover
    | AiEvade
    | AiHuntBall

// ---------------------------------------------------------------------------
// Records
// ---------------------------------------------------------------------------

/// Logische Eingabe eines Spielers für genau einen Simulationsschritt
type Input =
    { Left: bool
      Right: bool
      Up: bool
      Down: bool
      /// Eigener Sprung-Knopf (Tastatur: W/↑ = Oben + Sprung, Controller: ✕/△)
      Jump: bool
      Attack: bool
      Special: bool
      Shield: bool }

module Input =
    let none =
        { Left = false
          Right = false
          Up = false
          Down = false
          Jump = false
          Attack = false
          Special = false
          Shield = false }

    let any (i: Input) =
        i.Left || i.Right || i.Up || i.Down || i.Jump || i.Attack || i.Special || i.Shield

/// Flankengetriggerte Menü-Eingaben eines Spielers
type MenuPad =
    { Up: bool
      Down: bool
      Left: bool
      Right: bool
      Accept: bool
      Cancel: bool }

module MenuPad =
    let none =
        { Up = false
          Down = false
          Left = false
          Right = false
          Accept = false
          Cancel = false }

/// Menü-/Systemeingaben (pro Simulationsschritt)
type UiInput =
    { P1: MenuPad
      P2: MenuPad
      Start: bool
      Escape: bool
      ToggleDebug: bool }

/// Hitbox relativ zum Körpermittelpunkt; OffsetX wird mit der Blickrichtung gespiegelt
type HitboxDef =
    { OffsetX: float
      OffsetY: float
      Width: float
      Height: float
      Damage: float
      BaseKnockback: float
      KnockbackScaling: float
      /// Winkel in Grad: 0 = vom Angreifer weg, 90 = nach oben, 270 = Meteor-Spike
      Angle: float }

/// Projektil, das beim ersten Active-Frame eines Angriffs erzeugt wird (relativ zur Körpermitte)
type ProjectileSpawn =
    { Kind: ProjectileKind
      OffsetX: float
      OffsetY: float
      SpeedX: float
      SpeedY: float }

type MoveData =
    { Startup: int
      Active: int
      Recovery: int
      LandingLag: int
      Hitboxes: HitboxDef list
      /// Einmaliger Impuls beim Übergang Startup -> Active
      BoostX: float
      BoostY: float
      /// Während der Active-Frames gehaltene Geschwindigkeit (Dashes, Sturzflüge, Jetpack)
      HoldVelX: float option
      HoldVelY: float option
      /// Während der Recovery-Frames gehaltene Fallgeschwindigkeit (nur über der Hauptplattform)
      SlamVelY: float option
      /// Landung während des Angriffs erzeugt Schockwellen
      LandShockwave: bool
      /// Super-Armor: Treffer unterbrechen Startup/Active nicht
      Armor: bool
      /// Teleport-Distanz beim Übergang Startup -> Active
      Teleport: float
      /// Flug in die gehaltene Richtung mit dieser Geschwindigkeit (ohne Schwerkraft)
      Flight: float
      /// Phantom-Hieb: Teleport hinter den nächsten Gegner
      Phantom: bool
      /// Reflektiert gegnerische Projektile während der Active-Frames
      Reflect: bool
      /// Konter: Nahkampftreffer während der Active-Frames werden mit einem Gegenschlag beantwortet
      Counter: bool
      /// Spezialgriff: packt auch durch den Schild und wirft sofort
      CommandGrab: bool
      Spawns: ProjectileSpawn list
      /// Energiekosten der Projektile (Anti-Spam)
      EnergyCost: float
      /// Nach dem Angriff in der Luft hilflos (Up-B)
      Helpless: bool }

/// Kämpfer-Werte: Physik, Größe, Gewicht und Anzeige im Character-Select
type FighterStats =
    { Name: string
      Role: string
      Color: string
      Dark: string
      Width: float
      Height: float
      /// Zeichen-Skalierung relativ zum Basis-Skelett (62 px hoch)
      Scale: float
      Weight: float
      RunSpeed: float
      RunAccel: float
      AirSpeed: float
      AirAccel: float
      Gravity: float
      MaxFall: float
      FastFall: float
      FullHop: float
      ShortHop: float
      AirJump: float
      AirJumps: int
      Reach: float
      StatSpeed: int
      StatPower: int
      StatWeight: int
      StatReach: int
      UpSpecialName: string
      SideSpecialName: string
      NeutralName: string
      DownSpecialName: string
      UltraName: string }

type Player =
    { Id: int
      IsCpu: bool
      Fighter: Fighter
      /// X = Körpermitte, Y = Fußpunkt
      Pos: Vec2
      Vel: Vec2
      Facing: float
      State: PlayerActionState
      StateFrame: int
      Timer: int
      Percent: float
      Stocks: int
      OnGround: bool
      JumpsLeft: int
      FastFall: bool
      AirDodged: bool
      SideUsed: bool
      HasHit: bool
      DropThrough: int
      Invuln: int
      LedgeCooldown: int
      Shield: float
      HudShake: float
      /// Frames, in denen der Kämpfer als weiße Treffer-Silhouette gezeichnet wird
      HitFlash: int
      /// Knotenkette für Umhang, Stirnband, Schal, Zopf oder Schweif
      Cloth: Vec2 list
      /// Input-Buffer: Restframes, in denen ein Tastendruck noch ausgeführt wird
      BufJump: int
      BufAttack: int
      BufSpecial: int
      /// Treffer in der laufenden Combo, die dieser Spieler einsteckt
      Combo: int
      /// Combo-Anzeige für diesen Spieler als Angreifer
      ComboShown: int
      ComboTimer: int
      LastHitBy: int option
      /// Anti-Spam: Energie für Projektile (0..100), lädt im Nahkampf schneller
      Energy: float
      /// Rotes Aufblinken der Energieleiste nach einem Fehlzünder
      EnergyFlash: int
      /// Stale-Move-Negation: zuletzt benutzte bzw. getroffene Angriffe (neuester zuerst)
      Stale: MoveId list
      /// Zusätzliche Recovery-Frames des laufenden Angriffs (gespammte Projektile)
      ExtraLag: int
      /// Stasis (Chrono): Kämpfer läuft mit halber Geschwindigkeit
      Slow: int
      /// Schwebe-Vorrat (Chrono)
      Float: int
      /// Smash-Ball eingesammelt: Ultra-Smash bereit
      HasFinal: bool
      /// Item-Effekte (Frames): Kraft-Kristall und Tempo-Feder
      PowerTimer: int
      SpeedTimer: int
      /// Perfect Shield: Aufleuchten nach erfolgreichem Parry
      ParryFlash: int
      /// Aufgeladene Frames des aktuellen Smash-Angriffs (G gehalten)
      Charge: int
      // Match-Statistik
      KOs: int
      Falls: int
      DamageDealt: float
      MaxCombo: int
      Prev: Input }

type Projectile =
    { Kind: ProjectileKind
      Owner: int
      Source: MoveId
      Pos: Vec2
      Vel: Vec2
      Life: int
      MaxLife: int
      Hit: HitboxDef
      /// Wurde reflektiert (Valkyrie)
      Reflected: bool }

type Particle =
    { Kind: ParticleKind
      Pos: Vec2
      Vel: Vec2
      Life: int
      MaxLife: int
      Size: float
      Color: string }

type Platform =
    { X1: float
      X2: float
      Y: float
      Solid: bool
      /// Bewegung seit dem letzten Frame (bewegliche Plattformen nehmen Kämpfer mit)
      Vx: float
      Vy: float }

/// Geometrie einer Stage zu einem bestimmten Zeitpunkt
type Layout =
    { Stage: StageId
      Left: float
      Right: float
      Top: float
      Bottom: float
      Soft: Platform list }

/// Smash-Lichtstrahl, der bei einem KO von der Bildschirmkante ins Zentrum schießt
type KoBeam =
    { Origin: Vec2
      Dir: Vec2
      Life: int
      MaxLife: int
      Color: string }

type SmashBall =
    { Pos: Vec2
      Vel: Vec2
      Hp: float
      Life: int
      HitCooldown: int
      Flash: int }

type ItemKind =
    | Heart
    | PowerGem
    | SpeedFeather
    | Bomb

/// Item auf der Stage: fällt vom Himmel, wird durch Berührung eingesammelt (Bombe explodiert)
type Item =
    { Kind: ItemKind
      Pos: Vec2
      Vel: Vec2
      OnGround: bool
      Age: int
      Life: int }

type HazardKind =
    | WindGust
    | Meteor
    | LavaGeyser
    | Icicles
    | LaserSweep

/// Stage-Ereignis: erst Warnung, dann aktive Phase
type Hazard =
    { Kind: HazardKind
      Age: int
      Xs: float list
      Dir: float
      /// Bereits getroffene Spieler (jeder nur einmal pro Ereignis)
      HitIds: int list }

/// Laufender Ultra-Smash: Cut-In, dann bildschirmfüllender Angriff
type Ultra =
    { Owner: int
      Fighter: Fighter
      Timer: int
      Origin: Vec2
      Facing: float
      Target: Vec2
      Victims: int list }

type AiBrain =
    { Mode: AiState
      AttackCooldown: int
      JumpHold: int
      ShieldHold: int
      Seed: int
      /// Zuletzt beobachtete Zustände des Gegners (neuester zuerst) für die Reaktionsverzögerung
      Memory: Player list
      ReactDelay: int
      /// Frames bis zur nächsten Entscheidung; dazwischen wird die letzte Eingabe gehalten
      Think: int
      Held: Input
      /// Wie viele Pixel unter der Kante die CPU mit dem Recovery wartet
      RecoverLate: float }

/// Auswahlzustand im Character-Select
type SelectState =
    { Cursors: int list
      Locked: bool list }

type Game =
    { Screen: GameScreen
      Mode: GameMode
      Difficulty: Difficulty
      MenuIndex: int
      Select: SelectState
      /// Gewählte Zeile im Optionsfeld der Kämpferwahl
      OptionIndex: int
      StockCount: int
      SmashBallOn: bool
      Stage: StageId
      Players: Player list
      Projectiles: Projectile list
      Particles: Particle list
      Brain: AiBrain
      Ball: SmashBall option
      BallTimer: int
      Ultra: Ultra option
      /// Pause-Menü während des Kampfes
      Paused: bool
      ItemsOn: bool
      HazardsOn: bool
      Items: Item list
      ItemTimer: int
      Hazard: Hazard option
      HazardTimer: int
      Frame: int
      MatchFrames: int
      Countdown: int
      GoTimer: int
      HitStop: int
      ShakeDir: Vec2
      ShakeMag: float
      /// Schwarz-Weiß-Impact-Frames bei starken Smash-Treffern
      ImpactFlash: int
      ImpactPos: Vec2
      Beams: KoBeam list
      GameOverTimer: int
      Winner: int option
      ShowBoxes: bool
      Seed: int
      /// Soundereignisse des letzten Simulationsschritts (werden von App abgespielt)
      Sounds: Sfx list }

// ---------------------------------------------------------------------------
// Konstanten
// ---------------------------------------------------------------------------

let fixedDeltaTime = 1.0 / 60.0

let screenWidth = 1280.0
let screenHeight = 720.0

// Blast-Zones (knapp außerhalb des sichtbaren Bildes)
let blastLeft = -170.0
let blastRight = screenWidth + 170.0
let blastTop = -260.0
let blastBottom = screenHeight + 170.0

// Referenzgröße des Basis-Skeletts (Ledge-Geometrie, Effekte)
let bodyWidth = 34.0
let bodyHeight = 62.0
let bodyHalf = bodyWidth / 2.0

// Allgemeine Bewegung (Einheiten: Pixel pro 60Hz-Frame)
let groundFriction = 0.78
let airFriction = 0.985
let jumpSquatFrames = 4
let ledgeJumpSpeed = 14.0

/// Großzügiger Input-Buffer für Sprung, Angriff und Special
let inputBuffer = 6

// Kampf
let maxPercent = 999.0
let knockbackToSpeed = 0.25
let knockbackDecay = 0.965
let hitstunPerKnockback = 0.4
let minHitstun = 8
let diMaxDegrees = 14.0
let diDrift = 0.1
let shieldMax = 100.0
let shieldDrain = 0.3
let shieldRegen = 0.14
let ledgeInvulnFrames = 34
let respawnInvulnFrames = 120
let respawnY = 190.0
/// Ab diesem Knockback bricht ein Treffer die Super-Armor
let armorBreakKnockback = 115.0

// Anti-Spam: Energie und Stale-Move-Negation
let energyMax = 100.0
let energyRegen = 0.1
let energyPerMeleeDamage = 2.2
let energyPerDamageTaken = 0.6
let staleWeights = [ 0.09; 0.08; 0.07; 0.06; 0.05; 0.045; 0.04; 0.035; 0.03 ]
let staleMinimum = 0.5
let staleLagPerUse = 4

// Smash-Ball & Ultra
let ballMinDelay = 25 * 60
let ballMaxDelay = 35 * 60
let ballHp = 30.0
let ballLife = 20 * 60
let ultraCutIn = 50
let ultraHitFrame = 96
let ultraEnd = 104
let stasisFrames = 80
let floatFrames = 80

// Ablauf
let countdownFrames = 210
let goFrames = 50
let gameOverDelay = 110
let maxParticles = 600
let impactFrames = 6
let koBeamFrames = 34
let comboDisplayFrames = 100

// Stoff-Simulation (Umhang / Stirnband / Schal)
let clothNodes = 6
let clothSegment = 7.5

// Items und Stage-Ereignisse
let itemMinDelay = 540
let itemMaxDelay = 900
let itemLife = 900
let itemMax = 2
let itemSize = 26.0
let bombArmFrames = 40
let bombRadius = 95.0
let powerFrames = 600
let speedFrames = 600
let hazardMinDelay = 840
let hazardMaxDelay = 1260
let parryWindow = 3
let maxCharge = 60

let itemName (k: ItemKind) =
    match k with
    | Heart -> "HEILHERZ"
    | PowerGem -> "KRAFT-KRISTALL"
    | SpeedFeather -> "TEMPO-FEDER"
    | Bomb -> "BOMBE"

/// Warnzeit und aktive Dauer der Stage-Ereignisse (Frames)
let hazardTiming (k: HazardKind) =
    match k with
    | WindGust -> 100, 260
    | Meteor -> 85, 14
    | LavaGeyser -> 90, 55
    | Icicles -> 80, 50
    | LaserSweep -> 100, 110

let stageHazard (s: StageId) =
    match s with
    | SkyBattlefield -> WindGust
    | AstralAltar -> Meteor
    | MagmaFoundry -> LavaGeyser
    | FrostPeak -> Icicles
    | NeonRooftop -> LaserSweep

let hazardName (k: HazardKind) =
    match k with
    | WindGust -> "WINDBÖE"
    | Meteor -> "STERNSCHNUPPE"
    | LavaGeyser -> "LAVA-FONTÄNE"
    | Icicles -> "EISZAPFEN"
    | LaserSweep -> "LASER"

let allFighters = [ Aether; Ignis; Titan; Zephyr; Ronin; Lyra; Valkyrie; Chrono; Bramble; Specter; Gorm ]
let allDifficulties = [ Easy; Medium; Hard; Pro ]
let allStages = [ SkyBattlefield; AstralAltar; MagmaFoundry; FrostPeak; NeonRooftop ]

let difficultyName (d: Difficulty) =
    match d with
    | Easy -> "LEICHT"
    | Medium -> "MITTEL"
    | Hard -> "SCHWER"
    | Pro -> "PROFI"

let stageName (s: StageId) =
    match s with
    | SkyBattlefield -> "SKY BATTLEFIELD"
    | AstralAltar -> "ASTRAL ALTAR"
    | MagmaFoundry -> "MAGMA FOUNDRY"
    | FrostPeak -> "FROSTGIPFEL"
    | NeonRooftop -> "NEON-DACH"

let stageDescription (s: StageId) =
    match s with
    | SkyBattlefield -> "Tempel-Arena über den Wolken mit drei Plattformen. Ereignis: Windböen schieben alle Kämpfer"
    | AstralAltar -> "Flache, breite Plattform vor der Galaxie. Ereignis: Sternschnuppen schlagen an markierten Stellen ein"
    | MagmaFoundry -> "Vulkan-Schmiede mit zwei beweglichen Plattformen. Ereignis: Lava-Fontänen schießen aus dem Boden"
    | FrostPeak -> "Eisiger Berggipfel mit Plattformen über dem Abgrund. Ereignis: Eiszapfen stürzen von oben herab"
    | NeonRooftop -> "Hochhausdach in einer Neon-Stadt bei Nacht. Ereignis: Ein Sicherheitslaser fegt über das Dach, springen!"

// ---------------------------------------------------------------------------
// Stages: Geometrie (bewegliche Plattformen sind eine Funktion der Zeit)
// ---------------------------------------------------------------------------

let private plat x1 x2 y = { X1 = x1; X2 = x2; Y = y; Solid = false; Vx = 0.0; Vy = 0.0 }

/// Position der Plattformen in Frame t (ohne Geschwindigkeit)
let private stagePlatforms (s: StageId) (t: int) : Platform list =
    let ft = float t

    match s with
    | SkyBattlefield -> [ plat 360.0 540.0 382.0; plat 740.0 920.0 382.0; plat 550.0 730.0 264.0 ]
    | AstralAltar -> []
    | MagmaFoundry ->
        let y1 = 392.0 + 62.0 * sin (ft * 0.012)
        let x2 = 800.0 + 95.0 * sin (ft * 0.009 + 1.3)
        [ plat 385.0 545.0 y1; plat (x2 - 80.0) (x2 + 80.0) 318.0 ]
    | FrostPeak -> [ plat 250.0 400.0 412.0; plat 880.0 1030.0 412.0; plat 565.0 715.0 300.0 ]
    | NeonRooftop -> [ plat 395.0 555.0 398.0; plat 725.0 885.0 398.0 ]

let stageLayout (s: StageId) (t: int) : Layout =
    let now = stagePlatforms s t
    let before = stagePlatforms s (t - 1)

    let soft =
        List.map2 (fun (a: Platform) (b: Platform) -> { a with Vx = a.X1 - b.X1; Vy = a.Y - b.Y }) now before

    match s with
    | SkyBattlefield ->
        { Stage = s
          Left = 300.0
          Right = 980.0
          Top = 500.0
          Bottom = 610.0
          Soft = soft }
    | AstralAltar ->
        { Stage = s
          Left = 220.0
          Right = 1060.0
          Top = 500.0
          Bottom = 580.0
          Soft = soft }
    | MagmaFoundry ->
        { Stage = s
          Left = 320.0
          Right = 960.0
          Top = 520.0
          Bottom = 625.0
          Soft = soft }
    | FrostPeak ->
        { Stage = s
          Left = 345.0
          Right = 935.0
          Top = 492.0
          Bottom = 600.0
          Soft = soft }
    | NeonRooftop ->
        { Stage = s
          Left = 250.0
          Right = 1030.0
          Top = 520.0
          Bottom = 610.0
          Soft = soft }

let mainPlatform (l: Layout) =
    { X1 = l.Left
      X2 = l.Right
      Y = l.Top
      Solid = true
      Vx = 0.0
      Vy = 0.0 }

let allPlatforms (l: Layout) = mainPlatform l :: l.Soft
let stageCenter (l: Layout) = (l.Left + l.Right) / 2.0

// ---------------------------------------------------------------------------
// Kämpfer
// ---------------------------------------------------------------------------

let fighterStats (f: Fighter) : FighterStats =
    match f with
    | Aether ->
        { Name = "AETHER"
          Role = "Allrounder · Schwertkämpfer"
          Color = "#58c7ff"
          Dark = "#1c5fa8"
          Width = 34.0
          Height = 62.0
          Scale = 1.0
          Weight = 100.0
          RunSpeed = 6.2
          RunAccel = 1.1
          AirSpeed = 5.2
          AirAccel = 0.45
          Gravity = 0.62
          MaxFall = 11.0
          FastFall = 16.5
          FullHop = 13.4
          ShortHop = 8.2
          AirJump = 12.6
          AirJumps = 1
          Reach = 1.2
          StatSpeed = 6
          StatPower = 6
          StatWeight = 5
          StatReach = 7
          UpSpecialName = "Skyward Slash"
          SideSpecialName = "Blade Beam ◎"
          NeutralName = "Klingen-Smash"
          DownSpecialName = "Sturzklinge"
          UltraName = "HIMMELSKLINGE" }
    | Ignis ->
        { Name = "IGNIS"
          Role = "Rushdown · Feuer-Brawler"
          Color = "#ff6b3d"
          Dark = "#a3261a"
          Width = 32.0
          Height = 60.0
          Scale = 0.97
          Weight = 84.0
          RunSpeed = 7.8
          RunAccel = 1.5
          AirSpeed = 5.9
          AirAccel = 0.55
          Gravity = 0.72
          MaxFall = 12.5
          FastFall = 18.0
          FullHop = 14.2
          ShortHop = 8.8
          AirJump = 13.0
          AirJumps = 1
          Reach = 0.8
          StatSpeed = 9
          StatPower = 6
          StatWeight = 3
          StatReach = 3
          UpSpecialName = "Rising Dragon"
          SideSpecialName = "Flare Dash"
          NeutralName = "Glutfaust + Feuerball ◎"
          DownSpecialName = "Axtkick"
          UltraName = "INFERNO-DRACHE" }
    | Titan ->
        { Name = "TITAN"
          Role = "Heavyweight · Tank"
          Color = "#ffc23d"
          Dark = "#8a5a12"
          Width = 46.0
          Height = 76.0
          Scale = 1.22
          Weight = 136.0
          RunSpeed = 4.6
          RunAccel = 0.75
          AirSpeed = 4.0
          AirAccel = 0.32
          Gravity = 0.68
          MaxFall = 12.5
          FastFall = 17.0
          FullHop = 13.2
          ShortHop = 7.8
          AirJump = 11.6
          AirJumps = 1
          Reach = 1.06
          StatSpeed = 2
          StatPower = 10
          StatWeight = 10
          StatReach = 6
          UpSpecialName = "Jetpack Slam"
          SideSpecialName = "Meteor Charge"
          NeutralName = "Titanfaust · Luft: Felsbrocken ◎"
          DownSpecialName = "Bodenstampfer"
          UltraName = "METEORSCHAUER" }
    | Zephyr ->
        { Name = "ZEPHYR"
          Role = "Zoner · Ninja-Magier"
          Color = "#b48cff"
          Dark = "#4b2d9a"
          Width = 30.0
          Height = 56.0
          Scale = 0.9
          Weight = 76.0
          RunSpeed = 6.9
          RunAccel = 1.25
          AirSpeed = 6.0
          AirAccel = 0.6
          Gravity = 0.5
          MaxFall = 9.2
          FastFall = 14.0
          FullHop = 12.6
          ShortHop = 7.6
          AirJump = 10.6
          AirJumps = 2
          Reach = 0.86
          StatSpeed = 8
          StatPower = 4
          StatWeight = 2
          StatReach = 4
          UpSpecialName = "Schattensprung"
          SideSpecialName = "Energie-Salve ◎"
          NeutralName = "Kunai-Fächer ◎"
          DownSpecialName = "Meteor-Tritt"
          UltraName = "TAUSEND KLINGEN" }
    | Valkyrie ->
        { Name = "VALKYRIE"
          Role = "Lanzen-Kriegerin · Reichweite"
          Color = "#ffe08a"
          Dark = "#3a6ea8"
          Width = 34.0
          Height = 66.0
          Scale = 1.06
          Weight = 108.0
          RunSpeed = 5.9
          RunAccel = 1.0
          AirSpeed = 5.0
          AirAccel = 0.42
          Gravity = 0.64
          MaxFall = 11.5
          FastFall = 17.0
          FullHop = 13.6
          ShortHop = 8.3
          AirJump = 12.8
          AirJumps = 1
          Reach = 1.42
          StatSpeed = 5
          StatPower = 7
          StatWeight = 6
          StatReach = 10
          UpSpecialName = "Valkyrie Ascent"
          SideSpecialName = "Aegis Dash (reflektiert)"
          NeutralName = "Lichtspeer-Wurf ◎"
          DownSpecialName = "Speersturz"
          UltraName = "WALHALLA-LANZE" }
    | Chrono ->
        { Name = "CHRONO"
          Role = "Zeit-Magier · Schwebend"
          Color = "#4fe0c8"
          Dark = "#2b2a6e"
          Width = 32.0
          Height = 64.0
          Scale = 1.03
          Weight = 94.0
          RunSpeed = 5.4
          RunAccel = 0.95
          AirSpeed = 5.0
          AirAccel = 0.5
          Gravity = 0.42
          MaxFall = 7.6
          FastFall = 12.0
          FullHop = 11.6
          ShortHop = 7.0
          AirJump = 10.4
          AirJumps = 1
          Reach = 1.0
          StatSpeed = 4
          StatPower = 6
          StatWeight = 4
          StatReach = 8
          UpSpecialName = "Chrono-Blink"
          SideSpecialName = "Stasis-Kugel ◎"
          NeutralName = "Zeit-Riss"
          DownSpecialName = "Zeitanker"
          UltraName = "ZEITSTILLSTAND" }
    | Bramble ->
        { Name = "BRAMBLE"
          Role = "Natur-Bestie · Kraftpaket"
          Color = "#7ed957"
          Dark = "#3d6b23"
          Width = 42.0
          Height = 66.0
          Scale = 1.12
          Weight = 118.0
          RunSpeed = 6.0
          RunAccel = 1.0
          AirSpeed = 5.1
          AirAccel = 0.42
          Gravity = 0.74
          MaxFall = 13.0
          FastFall = 18.0
          FullHop = 16.2
          ShortHop = 9.6
          AirJump = 15.0
          AirJumps = 1
          Reach = 1.08
          StatSpeed = 5
          StatPower = 9
          StatWeight = 8
          StatReach = 6
          UpSpecialName = "Ranken-Schwung"
          SideSpecialName = "Dornen-Salve ◎"
          NeutralName = "Pranken-Schlag"
          DownSpecialName = "Wurzelstampfer"
          UltraName = "URWALD-ZORN" }
    | Ronin ->
        { Name = "RONIN"
          Role = "Samurai · Konter-Meister"
          Color = "#ff4057"
          Dark = "#4a0d1c"
          Width = 33.0
          Height = 63.0
          Scale = 1.0
          Weight = 102.0
          RunSpeed = 6.5
          RunAccel = 1.15
          AirSpeed = 5.3
          AirAccel = 0.46
          Gravity = 0.64
          MaxFall = 11.4
          FastFall = 16.8
          FullHop = 13.6
          ShortHop = 8.2
          AirJump = 12.4
          AirJumps = 1
          Reach = 1.12
          StatSpeed = 6
          StatPower = 7
          StatWeight = 5
          StatReach = 7
          UpSpecialName = "Kranichflug"
          SideSpecialName = "Klingenwind ◎"
          NeutralName = "Iai-Schnitt"
          DownSpecialName = "Konter-Haltung"
          UltraName = "TAUSEND SCHNITTE" }
    | Lyra ->
        { Name = "LYRA"
          Role = "Mond-Bogenschützin · Distanz"
          Color = "#d6dcff"
          Dark = "#3d4280"
          Width = 31.0
          Height = 60.0
          Scale = 0.97
          Weight = 86.0
          RunSpeed = 6.8
          RunAccel = 1.2
          AirSpeed = 5.8
          AirAccel = 0.5
          Gravity = 0.58
          MaxFall = 10.4
          FastFall = 15.6
          FullHop = 14.6
          ShortHop = 8.6
          AirJump = 13.4
          AirJumps = 1
          Reach = 0.95
          StatSpeed = 7
          StatPower = 4
          StatWeight = 3
          StatReach = 9
          UpSpecialName = "Mondsprung"
          SideSpecialName = "Sternpfeil ◎"
          NeutralName = "Pfeilsalve ◎"
          DownSpecialName = "Sturzpfeil"
          UltraName = "STERNENREGEN" }
    | Gorm ->
        { Name = "GORM"
          Role = "Steingolem · Ringer"
          Color = "#d9a46a"
          Dark = "#4a3420"
          Width = 44.0
          Height = 71.0
          Scale = 1.15
          Weight = 148.0
          RunSpeed = 4.3
          RunAccel = 0.7
          AirSpeed = 3.8
          AirAccel = 0.32
          Gravity = 0.74
          MaxFall = 13.0
          FastFall = 18.0
          FullHop = 12.6
          ShortHop = 7.8
          AirJump = 11.6
          AirJumps = 1
          Reach = 1.02
          StatSpeed = 2
          StatPower = 10
          StatWeight = 10
          StatReach = 5
          UpSpecialName = "Felssprung"
          SideSpecialName = "Felsgriff (durch Schild)"
          NeutralName = "Erdbeben-Faust"
          DownSpecialName = "Meteoritensturz"
          UltraName = "TEKTONIK" }
    | Specter ->
        { Name = "SPECTER"
          Role = "Schatten-Assassine · Luftkämpfer"
          Color = "#ff4f7b"
          Dark = "#2a1638"
          Width = 30.0
          Height = 58.0
          Scale = 0.94
          Weight = 70.0
          RunSpeed = 7.2
          RunAccel = 1.4
          AirSpeed = 6.9
          AirAccel = 0.72
          Gravity = 0.6
          MaxFall = 11.0
          FastFall = 16.5
          FullHop = 13.0
          ShortHop = 8.0
          AirJump = 12.4
          AirJumps = 1
          Reach = 0.74
          StatSpeed = 9
          StatPower = 5
          StatWeight = 1
          StatReach = 2
          UpSpecialName = "Schattenflug"
          SideSpecialName = "Schatten-Dolch ◎"
          NeutralName = "Phantom-Hieb"
          DownSpecialName = "Schattensturz"
          UltraName = "SCHATTENREICH" }

let playerColor (p: Player) = (fighterStats p.Fighter).Color
let playerDarkColor (p: Player) = (fighterStats p.Fighter).Dark
let fighterName (f: Fighter) = (fighterStats f).Name

let playerName (p: Player) =
    if p.IsCpu then "CPU" else sprintf "P%d" (p.Id + 1)

/// Rückstoß-Faktor nach Gewicht: schwere Kämpfer fliegen weniger weit
let weightFactor (f: Fighter) = 200.0 / (100.0 + (fighterStats f).Weight)

/// Stale-Move-Negation: Faktor für Schaden und Knockback (1.0 = frisch)
let staleFactor (m: MoveId) (recent: MoveId list) : float =
    let penalty =
        List.zip (List.truncate (List.length staleWeights) recent) (List.truncate (List.length recent) staleWeights)
        |> List.sumBy (fun (used, w) -> if used = m then w else 0.0)

    max staleMinimum (1.0 - penalty)

/// Hurtbox (Körper) eines Spielers
let hurtbox (p: Player) : Rect =
    let s = fighterStats p.Fighter

    { Left = p.Pos.X - s.Width / 2.0
      Top = p.Pos.Y - s.Height
      Width = s.Width
      Height = s.Height }

let projectileRect (pr: Projectile) : Rect =
    { Left = pr.Pos.X - pr.Hit.Width / 2.0
      Top = pr.Pos.Y - pr.Hit.Height / 2.0
      Width = pr.Hit.Width
      Height = pr.Hit.Height }

let ballRadius = 22.0

let ballRect (b: SmashBall) : Rect =
    { Left = b.Pos.X - ballRadius
      Top = b.Pos.Y - ballRadius
      Width = ballRadius * 2.0
      Height = ballRadius * 2.0 }

/// Deterministischer Zufall (xorshift), damit der gesamte Game-State rein funktional bleibt
let nextRandom (seed: int) : int * float =
    let a = seed ^^^ (seed <<< 13)
    let b = a ^^^ (a >>> 17)
    let c = b ^^^ (b <<< 5)
    let s = if c = 0 then 0x2F6E2B1 else c
    s, float (s &&& 0x7FFFFFFF) / 2147483647.0

// ---------------------------------------------------------------------------
// Stage-Ereignisse: gemeinsame Geometrie für Physik und Renderer
// ---------------------------------------------------------------------------

/// Höhe des i-ten Eiszapfens t Frames nach Beginn der aktiven Phase (gestaffelt)
let icicleY (l: Layout) (t: int) (i: int) =
    min l.Top (-60.0 + float (t - i * 8) * 15.0)

/// x-Position des Sicherheitslasers, der einmal über das Dach fegt
let laserX (l: Layout) (h: Hazard) (t: int) =
    let _, active = hazardTiming h.Kind
    let k = float t / float active
    let a = l.Left - 80.0
    let b = l.Right + 80.0
    if h.Dir > 0.0 then a + (b - a) * k else b - (b - a) * k
