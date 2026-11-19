module SmashClone.Physics

open System
open SmashClone.Types

// ---------------------------------------------------------------------------
// Frame-Data: Basis-Normals, pro Kämpfer getunt, plus eigene Specials
// ---------------------------------------------------------------------------

let private hitbox ox oy w h dmg bkb kbs angle : HitboxDef =
    { OffsetX = ox
      OffsetY = oy
      Width = w
      Height = h
      Damage = dmg
      BaseKnockback = bkb
      KnockbackScaling = kbs
      Angle = angle }

let private move startup active recovery landingLag hitboxes : MoveData =
    { Startup = startup
      Active = active
      Recovery = recovery
      LandingLag = landingLag
      Hitboxes = hitboxes
      BoostX = 0.0
      BoostY = 0.0
      HoldVelX = None
      HoldVelY = None
      SlamVelY = None
      LandShockwave = false
      Armor = false
      Teleport = 0.0
      Flight = 0.0
      Phantom = false
      Reflect = false
      Counter = false
      CommandGrab = false
      Spawns = []
      EnergyCost = 0.0
      Helpless = false }

let private spawn kind ox oy sx sy : ProjectileSpawn =
    { Kind = kind
      OffsetX = ox
      OffsetY = oy
      SpeedX = sx
      SpeedY = sy }

/// Basis-Werte (Referenz: Aether ohne Reichweitenbonus)
let private normals (m: MoveId) : MoveData =
    match m with
    | Jab -> move 3 3 9 4 [ hitbox 30.0 -4.0 34.0 22.0 3.0 10.0 0.08 30.0 ]
    | ForwardTilt -> move 6 4 14 4 [ hitbox 37.0 -2.0 48.0 24.0 8.0 22.0 0.38 35.0 ]
    | UpTilt -> move 5 5 13 4 [ hitbox 4.0 -46.0 46.0 46.0 7.0 24.0 0.32 86.0 ]
    | DownTilt -> move 4 3 11 4 [ hitbox 34.0 24.0 46.0 16.0 6.0 18.0 0.25 72.0 ]
    | ForwardSmash -> { move 14 4 26 4 [ hitbox 44.0 -4.0 56.0 36.0 17.0 30.0 0.95 38.0 ] with BoostX = 3.2 }
    | DownSmash ->
        move
            9
            4
            24
            4
            [ hitbox 36.0 22.0 42.0 22.0 13.0 28.0 0.75 28.0
              hitbox -36.0 22.0 42.0 22.0 13.0 28.0 0.75 28.0 ]
    | NeutralAir -> move 4 12 8 6 [ hitbox 0.0 0.0 60.0 58.0 7.0 18.0 0.3 45.0 ]
    | ForwardAir -> move 7 4 14 8 [ hitbox 35.0 -4.0 44.0 36.0 10.0 22.0 0.6 40.0 ]
    | UpAir -> move 5 5 12 6 [ hitbox 0.0 -46.0 48.0 38.0 9.0 22.0 0.55 88.0 ]
    | DownAir -> move 12 5 18 14 [ hitbox 0.0 40.0 32.0 42.0 12.0 22.0 0.6 270.0 ]
    | AirSmash -> move 11 4 20 12 [ hitbox 38.0 -2.0 50.0 42.0 14.0 26.0 0.85 42.0 ]
    | UpSpecial
    | SideSpecial
    | DownSpecial -> move 5 5 10 10 []

/// Kämpferspezifische Varianten der Normals: Jeder Kämpfer hat eigene Reichweiten, Winkel und Timings
let private signature (f: Fighter) (m: MoveId) : MoveData option =
    match f, m with
    // Aether: Schwert mit Sweetspot an der Klingenspitze
    | Aether, ForwardTilt ->
        Some(move 7 4 15 4 [ hitbox 54.0 -2.0 22.0 22.0 11.0 26.0 0.46 35.0; hitbox 28.0 -2.0 34.0 24.0 7.0 20.0 0.34 35.0 ])
    | Aether, UpAir -> Some(move 5 7 12 6 [ hitbox 0.0 -48.0 70.0 34.0 9.0 22.0 0.55 85.0 ])
    // Ignis: kurze Fäuste, aber sehr schnell und mit Ausfallschritten
    | Ignis, Jab -> Some(move 3 3 7 3 [ hitbox 26.0 -4.0 30.0 22.0 3.0 8.0 0.08 35.0 ])
    | Ignis, ForwardTilt -> Some { move 5 4 13 4 [ hitbox 34.0 -4.0 40.0 26.0 8.0 20.0 0.38 32.0 ] with BoostX = 4.2 }
    | Ignis, NeutralAir -> Some(move 4 12 10 7 [ hitbox 0.0 0.0 54.0 54.0 7.0 16.0 0.28 50.0 ])
    | Ignis, DownAir -> Some(move 8 5 14 10 [ hitbox 0.0 38.0 36.0 40.0 11.0 20.0 0.55 270.0 ])
    // Titan: wenige, aber brutale Treffer
    | Titan, Jab -> Some(move 6 4 16 6 [ hitbox 32.0 -4.0 40.0 30.0 8.0 26.0 0.3 40.0 ])
    | Titan, DownTilt ->
        Some(move 9 4 20 6 [ hitbox 40.0 24.0 46.0 18.0 10.0 30.0 0.42 80.0; hitbox -40.0 24.0 46.0 18.0 10.0 30.0 0.42 80.0 ])
    | Titan, UpAir -> Some(move 8 5 16 8 [ hitbox 0.0 -44.0 54.0 44.0 13.0 28.0 0.7 88.0 ])
    // Zephyr: kurze Kunai, dafür Sprint-Schnitte und Sturztritt
    | Zephyr, ForwardTilt -> Some { move 5 5 13 4 [ hitbox 30.0 -2.0 40.0 22.0 6.5 20.0 0.34 30.0 ] with BoostX = 4.8 }
    | Zephyr, NeutralAir -> Some(move 3 14 7 5 [ hitbox 0.0 0.0 56.0 56.0 6.0 16.0 0.28 45.0 ])
    | Zephyr, DownAir ->
        Some { move 6 12 14 12 [ hitbox 14.0 30.0 34.0 34.0 9.0 22.0 0.45 300.0 ] with HoldVelX = Some 6.0; HoldVelY = Some 9.0 }
    // Valkyrie: Speer mit enormer, aber schmaler Reichweite
    | Valkyrie, Jab -> Some(move 4 3 11 4 [ hitbox 44.0 -4.0 54.0 14.0 4.0 12.0 0.1 30.0 ])
    | Valkyrie, ForwardTilt -> Some(move 8 4 16 4 [ hitbox 60.0 -2.0 84.0 14.0 9.0 24.0 0.42 32.0 ])
    | Valkyrie, UpTilt -> Some(move 7 5 15 4 [ hitbox 6.0 -70.0 18.0 84.0 9.0 24.0 0.4 90.0 ])
    | Valkyrie, DownTilt -> Some(move 6 3 13 4 [ hitbox 58.0 26.0 80.0 12.0 7.0 22.0 0.3 25.0 ])
    | Valkyrie, ForwardAir -> Some(move 9 4 16 9 [ hitbox 58.0 -2.0 78.0 16.0 11.0 24.0 0.55 38.0 ])
    // Chrono: Magie entsteht auf Distanz statt direkt am Stab
    | Chrono, ForwardTilt -> Some(move 8 5 14 4 [ hitbox 74.0 -6.0 36.0 36.0 10.0 26.0 0.46 40.0; hitbox 34.0 -6.0 40.0 30.0 6.0 18.0 0.3 50.0 ])
    | Chrono, NeutralAir -> Some(move 5 18 9 6 [ hitbox 0.0 0.0 96.0 96.0 6.0 18.0 0.26 55.0 ])
    | Chrono, UpTilt -> Some(move 8 6 16 5 [ hitbox 0.0 -76.0 60.0 50.0 8.0 26.0 0.4 90.0 ])
    // Bramble: weite Prankenhiebe und Schwanzfeger
    | Bramble, ForwardTilt ->
        Some(move 7 5 15 4 [ hitbox 36.0 -12.0 46.0 26.0 9.0 24.0 0.42 45.0; hitbox 40.0 12.0 46.0 22.0 7.0 22.0 0.36 30.0 ])
    | Bramble, DownTilt ->
        Some(move 6 4 14 4 [ hitbox 38.0 26.0 50.0 16.0 7.0 24.0 0.32 60.0; hitbox -42.0 26.0 50.0 16.0 7.0 24.0 0.32 60.0 ])
    | Bramble, ForwardAir -> Some(move 10 5 18 10 [ hitbox 38.0 -4.0 52.0 48.0 13.0 26.0 0.66 45.0 ])
    // Specter: winzige Dolche, extrem schnell, Durchstich-Angriffe
    | Specter, Jab -> Some(move 2 2 6 3 [ hitbox 24.0 -4.0 26.0 18.0 3.0 8.0 0.08 30.0 ])
    | Specter, ForwardTilt -> Some { move 5 5 14 4 [ hitbox 26.0 -2.0 36.0 20.0 6.0 18.0 0.32 30.0 ] with BoostX = 6.0 }
    | Specter, DownAir -> Some(move 7 6 14 9 [ hitbox 0.0 40.0 22.0 46.0 9.0 18.0 0.5 270.0 ])
    | Specter, UpAir -> Some(move 3 5 9 5 [ hitbox 0.0 -44.0 40.0 36.0 7.0 20.0 0.48 85.0 ])
    // Ronin: Katana mit langen, präzisen Schnitten
    | Ronin, Jab -> Some(move 3 3 9 4 [ hitbox 36.0 -4.0 44.0 16.0 4.0 10.0 0.1 30.0 ])
    | Ronin, ForwardTilt -> Some(move 7 3 15 4 [ hitbox 50.0 -6.0 62.0 18.0 10.0 24.0 0.46 32.0 ])
    | Ronin, UpTilt -> Some(move 6 5 14 4 [ hitbox 10.0 -50.0 66.0 34.0 8.0 24.0 0.38 88.0 ])
    | Ronin, ForwardAir -> Some(move 8 3 15 9 [ hitbox 46.0 -2.0 58.0 22.0 11.0 24.0 0.58 40.0 ])
    // Lyra: flinke Tritte und Bogenhiebe, schwach im Nahkampf
    | Lyra, Jab -> Some(move 3 3 8 3 [ hitbox 28.0 -2.0 32.0 18.0 3.0 9.0 0.08 35.0 ])
    | Lyra, ForwardTilt -> Some(move 6 4 13 4 [ hitbox 38.0 -10.0 46.0 22.0 7.0 22.0 0.34 40.0 ])
    | Lyra, UpAir -> Some(move 4 6 10 5 [ hitbox 0.0 -46.0 54.0 34.0 7.0 22.0 0.48 86.0 ])
    | Lyra, DownSmash ->
        Some(move 9 4 22 4 [ hitbox 40.0 22.0 44.0 22.0 11.0 28.0 0.72 30.0; hitbox -40.0 22.0 44.0 22.0 11.0 28.0 0.72 30.0 ])
    // Gorm: langsame Steinfäuste mit gewaltiger Wucht
    | Gorm, Jab -> Some(move 6 4 15 6 [ hitbox 32.0 -4.0 40.0 32.0 7.0 24.0 0.26 40.0 ])
    | Gorm, ForwardTilt -> Some(move 10 4 20 5 [ hitbox 40.0 -4.0 52.0 34.0 12.0 28.0 0.5 38.0 ])
    | Gorm, UpTilt -> Some(move 9 5 18 5 [ hitbox 6.0 -52.0 62.0 44.0 11.0 28.0 0.5 88.0 ])
    | Gorm, NeutralAir -> Some(move 7 8 14 10 [ hitbox 0.0 0.0 70.0 70.0 10.0 22.0 0.36 50.0 ])
    | _ -> None

type private Tuning =
    { Reach: float
      Damage: float
      Bkb: float
      Kbs: float
      StartupK: float
      RecoveryK: float }

let private tuning (f: Fighter) : Tuning =
    match f with
    | Aether -> { Reach = 1.2; Damage = 1.0; Bkb = 1.0; Kbs = 1.0; StartupK = 1.0; RecoveryK = 1.0 }
    | Ignis -> { Reach = 0.8; Damage = 0.82; Bkb = 0.95; Kbs = 0.95; StartupK = 0.7; RecoveryK = 0.75 }
    | Titan -> { Reach = 1.06; Damage = 1.32; Bkb = 1.13; Kbs = 1.1; StartupK = 1.22; RecoveryK = 1.26 }
    | Zephyr -> { Reach = 0.86; Damage = 0.78; Bkb = 1.0; Kbs = 0.95; StartupK = 0.85; RecoveryK = 0.85 }
    | Valkyrie -> { Reach = 1.42; Damage = 1.05; Bkb = 1.05; Kbs = 1.03; StartupK = 0.95; RecoveryK = 0.95 }
    | Chrono -> { Reach = 1.0; Damage = 1.12; Bkb = 1.04; Kbs = 1.04; StartupK = 0.9; RecoveryK = 0.92 }
    | Bramble -> { Reach = 1.08; Damage = 1.15; Bkb = 1.05; Kbs = 1.05; StartupK = 1.1; RecoveryK = 1.05 }
    | Lyra -> { Reach = 0.95; Damage = 0.95; Bkb = 0.98; Kbs = 0.98; StartupK = 0.8; RecoveryK = 0.8 }
    | Gorm -> { Reach = 1.0; Damage = 1.42; Bkb = 1.14; Kbs = 1.1; StartupK = 1.2; RecoveryK = 1.22 }
    | Ronin -> { Reach = 1.12; Damage = 0.88; Bkb = 1.02; Kbs = 1.02; StartupK = 1.0; RecoveryK = 1.0 }
    | Specter -> { Reach = 0.74; Damage = 0.74; Bkb = 0.9; Kbs = 0.9; StartupK = 0.8; RecoveryK = 0.9 }

let private halfStep (v: float) = Math.Round(v * 2.0) / 2.0

let private tune (f: Fighter) (md: MoveData) : MoveData =
    let t = tuning f
    let hy = (fighterStats f).Height / bodyHeight
    let frames (k: float) (n: int) = max 1 (int (Math.Round(float n * k)))

    { md with
        Startup = frames t.StartupK md.Startup
        Recovery = frames t.RecoveryK md.Recovery
        Hitboxes =
            md.Hitboxes
            |> List.map (fun hb ->
                { hb with
                    OffsetX = hb.OffsetX * t.Reach * (if f = Titan then 1.15 else 1.0)
                    OffsetY = hb.OffsetY * hy
                    Width = hb.Width * t.Reach
                    Height = hb.Height * hy
                    Damage = halfStep (hb.Damage * t.Damage)
                    BaseKnockback = hb.BaseKnockback * t.Bkb
                    KnockbackScaling = hb.KnockbackScaling * t.Kbs }) }

let private isSmashMove (m: MoveId) =
    m = ForwardSmash || m = DownSmash || m = AirSmash

/// Eigene Spezialangriffe jedes Kämpfers (jeder hat Nah- und Fernkampf)
let private special (f: Fighter) (m: MoveId) : MoveData option =
    match f, m with
    // Aether: Allrounder mit Schwert, Fernkampf über Blade Beam
    | Aether, UpSpecial ->
        Some { move 5 14 10 14 [ hitbox 0.0 -34.0 52.0 62.0 6.0 26.0 0.4 80.0 ] with BoostY = -15.6; Helpless = true }
    | Aether, SideSpecial ->
        Some { move 9 3 18 10 [] with Spawns = [ spawn BladeBeam 30.0 -2.0 11.5 0.0 ]; EnergyCost = 30.0 }
    | Aether, DownSpecial ->
        Some { move 8 22 14 18 [ hitbox 2.0 34.0 34.0 44.0 11.0 20.0 0.55 270.0 ] with HoldVelY = Some 14.0 }
    // Ignis: Rushdown; Glutfaust schlägt zu und schleudert einen springenden Feuerball
    | Ignis, ForwardSmash ->
        Some
            { move 10 4 22 4 [ hitbox 34.0 -4.0 44.0 34.0 14.0 28.0 0.86 38.0 ] with
                BoostX = 2.6
                Spawns = [ spawn Fireball 26.0 -4.0 8.0 -2.5 ]
                EnergyCost = 22.0 }
    | Ignis, AirSmash ->
        Some
            { move 9 4 18 12 [ hitbox 32.0 -2.0 42.0 36.0 11.0 24.0 0.76 42.0 ] with
                Spawns = [ spawn Fireball 26.0 0.0 8.0 -1.0 ]
                EnergyCost = 22.0 }
    | Ignis, UpSpecial ->
        Some
            { move 3 12 14 16 [ hitbox 14.0 -30.0 46.0 58.0 13.0 30.0 0.72 78.0 ] with
                BoostX = 2.5
                BoostY = -16.8
                Helpless = true }
    | Ignis, SideSpecial ->
        Some
            { move 7 14 16 12 [ hitbox 10.0 0.0 52.0 56.0 9.0 26.0 0.55 40.0 ] with
                HoldVelX = Some 13.5
                HoldVelY = Some 0.0 }
    | Ignis, DownSpecial ->
        Some { move 6 10 16 14 [ hitbox 10.0 30.0 44.0 40.0 12.0 22.0 0.6 270.0 ] with HoldVelY = Some 11.0 }
    // Titan: schwerer Tank mit Super-Armor; Felsbrocken (Luft) und Erdbeben-Wellen (Down-Smash)
    | Titan, UpSpecial ->
        Some
            { move 7 22 30 20 [ hitbox 0.0 -48.0 62.0 40.0 8.0 24.0 0.45 85.0 ] with
                HoldVelY = Some -8.6
                SlamVelY = Some 17.0
                LandShockwave = true
                Helpless = true }
    | Titan, SideSpecial ->
        Some
            { move 10 16 22 14 [ hitbox 24.0 0.0 62.0 72.0 14.0 34.0 0.85 35.0 ] with
                HoldVelX = Some 10.5
                HoldVelY = Some 0.6
                Armor = true }
    | Titan, DownSpecial ->
        Some
            { move 9 30 18 20 [ hitbox 0.0 42.0 66.0 32.0 14.0 26.0 0.7 270.0 ] with
                BoostY = -5.0
                HoldVelY = Some 18.0
                LandShockwave = true }
    | Titan, AirSmash ->
        Some
            { move 14 3 22 12 [ hitbox 30.0 -6.0 40.0 40.0 9.0 20.0 0.5 45.0 ] with
                Spawns = [ spawn Boulder 32.0 -14.0 7.0 -5.5 ]
                EnergyCost = 35.0 }
    | Titan, DownSmash ->
        Some
            { tune Titan (normals DownSmash) with
                Armor = true
                Spawns = [ spawn Shockwave 36.0 24.0 9.0 0.0; spawn Shockwave -36.0 24.0 -9.0 0.0 ]
                EnergyCost = 25.0 }
    // Zephyr: Zoner mit Kunai, Energie-Salven und Teleport
    | Zephyr, (ForwardSmash | AirSmash) ->
        Some
            { move 7 3 16 8 [ hitbox 22.0 -4.0 30.0 30.0 4.0 14.0 0.3 45.0 ] with
                Spawns =
                    [ spawn Kunai 22.0 -6.0 13.0 -1.6
                      spawn Kunai 22.0 -3.0 13.6 0.0
                      spawn Kunai 22.0 0.0 13.0 1.6 ]
                EnergyCost = 30.0 }
    | Zephyr, SideSpecial ->
        Some
            { move 12 4 20 12 [] with
                Spawns = [ spawn EnergyOrb 26.0 -10.0 8.2 0.0; spawn EnergyOrb 16.0 8.0 6.2 0.0 ]
                EnergyCost = 40.0 }
    | Zephyr, UpSpecial ->
        Some { move 12 3 14 12 [ hitbox 0.0 -4.0 56.0 56.0 4.0 30.0 0.3 80.0 ] with Teleport = 175.0; Helpless = true }
    | Zephyr, DownSpecial ->
        Some
            { move 6 18 14 14 [ hitbox 10.0 30.0 36.0 38.0 10.0 26.0 0.7 270.0 ] with
                HoldVelX = Some 4.5
                HoldVelY = Some 12.5 }
    // Valkyrie: Lanze mit großer Reichweite, Lichtspeer und reflektierender Aegis
    | Valkyrie, (ForwardSmash | AirSmash) ->
        Some
            { move 12 3 20 10 [ hitbox 46.0 -4.0 54.0 22.0 6.0 16.0 0.35 30.0 ] with
                Spawns = [ spawn LightSpear 40.0 -6.0 15.0 -0.6 ]
                EnergyCost = 34.0 }
    | Valkyrie, SideSpecial ->
        Some
            { move 6 16 16 12 [ hitbox 20.0 0.0 42.0 64.0 8.0 30.0 0.5 40.0 ] with
                HoldVelX = Some 10.0
                HoldVelY = Some 0.0
                Reflect = true }
    | Valkyrie, UpSpecial ->
        Some
            { move 4 16 14 16 [ hitbox 2.0 -48.0 40.0 62.0 9.0 28.0 0.5 85.0 ] with
                BoostX = 1.5
                BoostY = -16.4
                Helpless = true }
    | Valkyrie, DownSpecial ->
        Some { move 8 22 16 18 [ hitbox 0.0 42.0 30.0 52.0 12.0 22.0 0.62 270.0 ] with HoldVelY = Some 15.0 }
    // Chrono: Zeit-Magier; Zeit-Riss (Nahkampf-Explosion) und Stasis-Kugel (verlangsamt)
    | Chrono, ForwardSmash ->
        Some(move 15 6 22 4 [ hitbox 52.0 -2.0 74.0 74.0 16.0 32.0 0.92 40.0 ])
    | Chrono, AirSmash ->
        Some(move 13 5 20 12 [ hitbox 46.0 0.0 66.0 66.0 13.0 28.0 0.82 42.0 ])
    | Chrono, SideSpecial ->
        Some { move 14 3 22 12 [] with Spawns = [ spawn StasisOrb 30.0 -4.0 6.5 0.0 ]; EnergyCost = 45.0 }
    | Chrono, UpSpecial ->
        Some { move 10 3 14 12 [ hitbox 0.0 -4.0 58.0 58.0 5.0 30.0 0.3 80.0 ] with Teleport = 190.0; Helpless = true }
    | Chrono, DownSpecial ->
        Some { move 8 20 16 16 [ hitbox 0.0 36.0 40.0 40.0 11.0 22.0 0.6 270.0 ] with HoldVelY = Some 13.0 }
    // Bramble: Natur-Bestie mit Prankenhieben, Dornen-Salve und Ranken-Schwung
    | Bramble, ForwardSmash ->
        Some { move 13 5 24 4 [ hitbox 46.0 -4.0 64.0 52.0 18.0 32.0 1.0 40.0 ] with BoostX = 4.0 }
    | Bramble, AirSmash ->
        Some(move 11 5 20 12 [ hitbox 40.0 0.0 56.0 50.0 14.0 26.0 0.85 42.0 ])
    | Bramble, SideSpecial ->
        Some
            { move 11 3 20 12 [] with
                Spawns =
                    [ spawn Thorn 24.0 -10.0 9.5 -4.6
                      spawn Thorn 24.0 -6.0 10.5 -3.0
                      spawn Thorn 24.0 -2.0 11.5 -1.4 ]
                EnergyCost = 32.0 }
    | Bramble, UpSpecial ->
        Some
            { move 6 18 12 16 [ hitbox 30.0 -36.0 60.0 50.0 8.0 28.0 0.45 70.0 ] with
                BoostX = 5.0
                BoostY = -17.2
                Helpless = true }
    | Bramble, DownSpecial ->
        Some { move 9 26 18 18 [ hitbox 0.0 40.0 56.0 30.0 13.0 24.0 0.66 270.0 ] with HoldVelY = Some 16.0 }
    // Lyra: Sternpfeil, Pfeilsalve, Mondsprung und Sturzpfeil
    | Lyra, SideSpecial ->
        Some { move 9 2 14 8 [] with Spawns = [ spawn Arrow 28.0 -8.0 19.0 0.0 ]; EnergyCost = 22.0 }
    | Lyra, ForwardSmash ->
        Some
            { move 12 2 22 8 [] with
                Spawns = [ spawn Arrow 26.0 -10.0 15.0 -4.2; spawn Arrow 26.0 -8.0 16.0 0.0; spawn Arrow 26.0 -6.0 15.0 4.2 ]
                EnergyCost = 38.0 }
    | Lyra, AirSmash ->
        Some
            { move 10 2 18 10 [] with
                Spawns = [ spawn Arrow 20.0 0.0 13.0 8.0; spawn Arrow 24.0 -4.0 15.0 3.0 ]
                EnergyCost = 30.0 }
    | Lyra, UpSpecial ->
        Some
            { move 5 12 14 14 [ hitbox 6.0 -20.0 40.0 50.0 7.0 24.0 0.45 82.0 ] with
                BoostX = 2.6
                BoostY = -17.6
                Helpless = true }
    | Lyra, DownSpecial ->
        Some
            { move 7 14 14 14 [ hitbox 12.0 30.0 34.0 34.0 9.0 22.0 0.5 300.0 ] with
                HoldVelX = Some 7.0
                HoldVelY = Some 11.0 }
    // Gorm: Erdbeben-Faust, Felsgriff, Felssprung und Meteoritensturz
    | Gorm, ForwardSmash ->
        Some { move 16 4 30 4 [ hitbox 50.0 -4.0 64.0 44.0 20.0 34.0 1.0 40.0 ] with Armor = true }
    | Gorm, SideSpecial ->
        // Felsgriff: kurzer Satz nach vorn, packt durch jeden Schild
        Some { move 10 6 26 10 [ hitbox 38.0 -6.0 40.0 50.0 0.0 0.0 0.0 0.0 ] with CommandGrab = true; BoostX = 5.0 }
    | Gorm, AirSmash -> Some(move 14 5 22 14 [ hitbox 26.0 24.0 66.0 40.0 15.0 28.0 0.8 290.0 ])
    | Gorm, UpSpecial ->
        Some
            { move 8 18 18 16 [ hitbox 0.0 -26.0 56.0 56.0 11.0 30.0 0.6 85.0 ] with
                BoostX = 2.0
                BoostY = -18.0
                Armor = true
                Helpless = true }
    | Gorm, DownSpecial ->
        Some
            { move 10 30 22 22 [ hitbox 0.0 36.0 60.0 40.0 15.0 30.0 0.7 270.0 ] with
                HoldVelY = Some 17.0
                LandShockwave = true }
    // Ronin: Iai-Schnitt, Klingenwind, Kranichflug und Konter
    | Ronin, ForwardSmash ->
        Some { move 17 3 28 4 [ hitbox 58.0 -6.0 92.0 24.0 17.0 30.0 0.95 34.0 ] with BoostX = 7.0 }
    | Ronin, AirSmash -> Some(move 10 4 20 12 [ hitbox 44.0 -4.0 72.0 40.0 13.0 26.0 0.8 40.0 ])
    | Ronin, SideSpecial ->
        Some { move 11 2 18 10 [] with Spawns = [ spawn WindSlash 34.0 -6.0 12.5 0.0 ]; EnergyCost = 26.0 }
    | Ronin, UpSpecial ->
        Some
            { move 6 14 16 14 [ hitbox 10.0 -18.0 52.0 70.0 9.0 26.0 0.5 80.0 ] with
                BoostX = 3.0
                BoostY = -15.6
                Helpless = true }
    | Ronin, (DownSmash | DownSpecial) ->
        // Konter-Haltung: wer jetzt im Nahkampf zuschlägt, wird bestraft
        Some { move 4 24 22 10 [] with Counter = true }
    // Specter: Schatten-Assassine mit Phantom-Hieb, Schatten-Dolch und Schattenflug
    | Specter, (ForwardSmash | AirSmash) ->
        Some { move 12 4 24 10 [ hitbox 30.0 -4.0 52.0 50.0 11.0 26.0 0.7 38.0 ] with Phantom = true }
    | Specter, SideSpecial ->
        Some { move 8 2 14 8 [] with Spawns = [ spawn ShadowDagger 26.0 -4.0 17.0 0.0 ]; EnergyCost = 18.0 }
    | Specter, UpSpecial ->
        Some { move 10 22 14 14 [ hitbox 0.0 0.0 40.0 58.0 5.0 24.0 0.35 70.0 ] with Flight = 11.5; Helpless = true }
    | Specter, DownSpecial ->
        Some
            { move 5 16 14 14 [ hitbox 8.0 30.0 34.0 36.0 9.0 24.0 0.66 270.0 ] with
                HoldVelX = Some 3.0
                HoldVelY = Some 14.0 }
    | _ -> None

let moveData (f: Fighter) (m: MoveId) : MoveData =
    match special f m with
    | Some md -> md
    | None ->
        let md = tune f (signature f m |> Option.defaultWith (fun () -> normals m))
        if f = Titan && isSmashMove m then { md with Armor = true } else md

/// Werte der Projektile: Hitbox und Lebensdauer
let projectileData (kind: ProjectileKind) : HitboxDef * int =
    match kind with
    | BladeBeam -> hitbox 0.0 0.0 30.0 48.0 7.0 14.0 0.32 30.0, 42
    | Kunai -> hitbox 0.0 0.0 24.0 10.0 3.5 6.0 0.12 25.0, 36
    | EnergyOrb -> hitbox 0.0 0.0 28.0 28.0 8.0 20.0 0.42 40.0, 72
    | Shockwave -> hitbox 0.0 0.0 38.0 28.0 9.0 24.0 0.5 70.0, 26
    | Fireball -> hitbox 0.0 0.0 22.0 22.0 6.0 12.0 0.3 40.0, 70
    | Boulder -> hitbox 0.0 0.0 36.0 36.0 13.0 30.0 0.62 45.0, 80
    | LightSpear -> hitbox 0.0 0.0 46.0 12.0 10.0 20.0 0.5 30.0, 40
    | StasisOrb -> hitbox 0.0 0.0 30.0 30.0 5.0 8.0 0.2 60.0, 80
    | Thorn -> hitbox 0.0 0.0 16.0 10.0 4.0 8.0 0.18 35.0, 48
    | ShadowDagger -> hitbox 0.0 0.0 26.0 10.0 5.0 10.0 0.22 25.0, 26
    | WindSlash -> hitbox 0.0 0.0 30.0 44.0 7.0 18.0 0.34 35.0, 30
    | Arrow -> hitbox 0.0 0.0 28.0 10.0 6.0 14.0 0.28 30.0, 50

/// Dynamische Hitboxen eines Spielers im aktuellen Frame (nur während Active-Frames)
let activeHitboxes (p: Player) : (Rect * HitboxDef) list =
    match p.State with
    | AttackActive m ->
        let cx = p.Pos.X
        let cy = p.Pos.Y - (fighterStats p.Fighter).Height / 2.0

        (moveData p.Fighter m).Hitboxes
        |> List.map (fun hb ->
            { Left = cx + hb.OffsetX * p.Facing - hb.Width / 2.0
              Top = cy + hb.OffsetY - hb.Height / 2.0
              Width = hb.Width
              Height = hb.Height },
            hb)
    | _ -> []

// ---------------------------------------------------------------------------
// Effekte: Die Simulation beschreibt nur, WAS passiert. Partikel, Sounds,
// Hit-Stop, Screen-Shake, Projektile und Statistik werden daraus am Ende abgeleitet.
// ---------------------------------------------------------------------------

type Effect =
    | Sound of Sfx
    | DustAt of Vec2 * int
    | SparksAt of Vec2 * Vec2 * int * string
    | SmokeAt of Vec2 * string
    | RingAt of Vec2 * string
    | ExplosionAt of Vec2 * Vec2 * string
    | BoltsAt of Vec2 * int * string
    | FlamesAt of Vec2 * Vec2 * int
    | LeavesAt of Vec2 * int
    | ShadowsAt of Vec2 * int
    | RainbowAt of Vec2 * int
    | BurstAt of Vec2 * float
    | SpeedLinesAt of Vec2 * Vec2
    | ImpactFrameAt of Vec2
    | BeamAt of Vec2 * Vec2 * string
    | HitStopFor of int
    | ShakeBy of Vec2 * float
    | SpawnProjectile of Projectile
    | ShockwaveAt of int * Vec2
    | DamageCredit of int * float
    | ComboCredit of int * int
    | KoCredit of int
    | EnergyGain of int * float
    | StaleHit of int * MoveId
    | PhantomStep of int
    | StartUltra of int
    /// Parry: Angreifer ist kurz wehrlos
    | Stun of int * int

// ---------------------------------------------------------------------------
// Spieler-Erzeugung & Stoff
// ---------------------------------------------------------------------------

let private spawnX (l: Layout) (id: int) =
    let c = stageCenter l
    let d = if l.Stage = AstralAltar then 220.0 else 170.0
    if id = 0 then c - d else c + d

let respawnX (l: Layout) (id: int) =
    let c = stageCenter l
    if id = 0 then c - 70.0 else c + 70.0

let hasCloth (f: Fighter) = f <> Titan && f <> Gorm

/// Befestigungspunkt des Stoffs (Umhang, Stirnband, Schal, Zopf oder Schweif)
let clothAnchor (p: Player) : Vec2 =
    let s = (fighterStats p.Fighter).Scale
    let at x y = { X = p.Pos.X - p.Facing * x * s; Y = p.Pos.Y - y * s }

    match p.Fighter with
    | Aether
    | Chrono -> at 5.0 47.0
    | Zephyr -> at 4.0 45.0
    | Valkyrie -> at 8.0 58.0
    | Bramble -> at 12.0 24.0
    | Specter -> at 4.0 46.0
    | Ronin -> at 7.0 57.0
    | Lyra -> at 6.0 56.0
    | _ -> at 7.0 60.0

let private restCloth (anchor: Vec2) (facing: float) : Vec2 list =
    List.init clothNodes (fun i -> { X = anchor.X - facing * float i * 5.0; Y = anchor.Y + float i * 5.5 })

/// Positionsbasierte Stoffkette: Jeder Knoten behält seine Weltposition, sinkt leicht ab und wird
/// dann auf Segmentlänge an den Vorgänger gezogen. Dadurch weht der Stoff abhängig von der Geschwindigkeit.
let updateCloth (p: Player) : Player =
    if not (hasCloth p.Fighter) then
        p
    else
        let anchor = clothAnchor p

        let stale =
            match p.Cloth with
            | first :: _ -> Vec2.length { X = first.X - anchor.X; Y = first.Y - anchor.Y } > 160.0
            | [] -> true

        if stale then
            { p with Cloth = restCloth anchor p.Facing }
        else
            let sink, drift, segment =
                match p.Fighter with
                | Aether -> 1.5, 0.55, clothSegment
                | Zephyr -> 0.5, 1.3, clothSegment * 1.25
                | Valkyrie -> 1.3, 0.6, clothSegment
                | Chrono -> 1.2, 0.7, clothSegment * 1.1
                | Bramble -> 0.15, 1.2, clothSegment * 0.95
                | Specter -> 1.0, 1.0, clothSegment * 1.2
                | _ -> 0.3, 1.5, clothSegment

            let nodes =
                p.Cloth
                |> List.tail
                |> List.fold
                    (fun (acc: Vec2 list) node ->
                        let prev = List.head acc
                        let wanted = { X = node.X - p.Facing * drift; Y = node.Y + sink }
                        let d = { X = wanted.X - prev.X; Y = wanted.Y - prev.Y }
                        let len = Vec2.length d
                        let dir = if len < 0.001 then { X = -p.Facing; Y = 0.0 } else Vec2.scale (1.0 / len) d
                        Vec2.add prev (Vec2.scale segment dir) :: acc)
                    [ anchor ]
                |> List.rev

            { p with Cloth = nodes }

let createPlayer (l: Layout) (id: int) (isCpu: bool) (fighter: Fighter) (stocks: int) : Player =
    { Id = id
      IsCpu = isCpu
      Fighter = fighter
      Pos = { X = spawnX l id; Y = l.Top }
      Vel = Vec2.zero
      Facing = (if id = 0 then 1.0 else -1.0)
      State = Idle
      StateFrame = 0
      Timer = 0
      Percent = 0.0
      Stocks = stocks
      OnGround = true
      JumpsLeft = (fighterStats fighter).AirJumps
      FastFall = false
      AirDodged = false
      SideUsed = false
      HasHit = false
      DropThrough = 0
      Invuln = 0
      LedgeCooldown = 0
      Shield = shieldMax
      HudShake = 0.0
      HitFlash = 0
      Cloth = []
      BufJump = 0
      BufAttack = 0
      BufSpecial = 0
      Combo = 0
      ComboShown = 0
      ComboTimer = 0
      LastHitBy = None
      Energy = energyMax
      EnergyFlash = 0
      Stale = []
      ExtraLag = 0
      Slow = 0
      Float = floatFrames
      HasFinal = false
      PowerTimer = 0
      SpeedTimer = 0
      ParryFlash = 0
      Charge = 0
      KOs = 0
      Falls = 0
      DamageDealt = 0.0
      MaxCombo = 0
      Prev = Input.none }
    |> updateCloth

/// Kämpfer für Menüs und Vorschauen (Sky Battlefield, Standardwerte)
let previewPlayer (id: int) (fighter: Fighter) : Player =
    createPlayer (stageLayout SkyBattlefield 0) id false fighter 3

let initialGame: Game =
    { Screen = Title
      Mode = VsCpu
      Difficulty = Medium
      MenuIndex = 0
      Select = { Cursors = [ 0; 1 ]; Locked = [ false; false ] }
      OptionIndex = 0
      StockCount = 3
      SmashBallOn = true
      Stage = SkyBattlefield
      Players = [ previewPlayer 0 Aether; { previewPlayer 1 Ignis with IsCpu = true } ]
      Projectiles = []
      Particles = []
      Brain = AI.initialBrain 987654321
      Ball = None
      BallTimer = ballMinDelay
      Ultra = None
      Paused = false
      OpenOnline = false
      OnlineMatch = false
      ItemsOn = true
      HazardsOn = true
      Items = []
      ItemTimer = itemMinDelay
      Hazard = None
      HazardTimer = hazardMinDelay
      Frame = 0
      MatchFrames = 0
      Countdown = 0
      GoTimer = 0
      HitStop = 0
      ShakeDir = Vec2.zero
      ShakeMag = 0.0
      ImpactFlash = 0
      ImpactPos = Vec2.zero
      Beams = []
      GameOverTimer = 0
      Winner = None
      ShowBoxes = false
      Seed = 123456789
      Sounds = [] }

let selectedFighter (g: Game) (slot: int) : Fighter =
    List.item (List.item slot g.Select.Cursors % List.length allFighters) allFighters

let private ballDelay (seed: int) : int * int =
    let s, r = nextRandom seed
    s, ballMinDelay + int (r * float (ballMaxDelay - ballMinDelay))

let newMatch (previous: Game) : Game =
    let f1 = selectedFighter previous 0
    let f2 = selectedFighter previous 1
    let layout = stageLayout previous.Stage 0
    let seed, delay = ballDelay (previous.Seed ^^^ previous.Frame)

    { initialGame with
        Screen = Playing
        Mode = previous.Mode
        Difficulty = previous.Difficulty
        Select = previous.Select
        OptionIndex = previous.OptionIndex
        StockCount = previous.StockCount
        SmashBallOn = previous.SmashBallOn
        Paused = false
        OnlineMatch = previous.OnlineMatch
        ItemsOn = previous.ItemsOn
        HazardsOn = previous.HazardsOn
        ItemTimer = 420
        HazardTimer = 720
        Stage = previous.Stage
        MenuIndex = 0
        Players =
            [ createPlayer layout 0 false f1 previous.StockCount
              createPlayer layout 1 (previous.Mode = VsCpu) f2 previous.StockCount ]
        Brain = AI.initialBrain (previous.Seed ^^^ 0x5F3759DF ^^^ previous.Frame)
        BallTimer = delay
        Countdown = countdownFrames
        ShowBoxes = previous.ShowBoxes
        Frame = previous.Frame
        Seed = seed
        Sounds = [ SfxCountdown ] }

// ---------------------------------------------------------------------------
// Hilfsfunktionen für die Zustandsmaschine
// ---------------------------------------------------------------------------

let private pressed (current: bool) (previous: bool) = current && not previous

let private axisX (i: Input) =
    (if i.Right then 1.0 else 0.0) - (if i.Left then 1.0 else 0.0)

let private axisY (i: Input) =
    (if i.Down then 1.0 else 0.0) - (if i.Up then 1.0 else 0.0)

let private clamp (lo: float) (hi: float) (v: float) = max lo (min hi v)

let private setState (s: PlayerActionState) (p: Player) = { p with State = s; StateFrame = 0 }

/// Kämpferwerte inklusive Item-Bonus (Tempo-Feder)
let private stats (p: Player) =
    let s = fighterStats p.Fighter

    if p.SpeedTimer > 0 then
        { s with
            RunSpeed = s.RunSpeed * 1.32
            RunAccel = s.RunAccel * 1.35
            AirSpeed = s.AirSpeed * 1.25 }
    else
        s

let private supportAt (l: Layout) (x: float) (y: float) (dropThrough: int) : Platform option =
    allPlatforms l
    |> List.tryFind (fun pl -> x >= pl.X1 && x <= pl.X2 && abs (y - pl.Y) < 0.6 && (pl.Solid || dropThrough = 0))

let private onSoftPlatform (l: Layout) (p: Player) =
    l.Soft
    |> List.exists (fun pl -> p.Pos.X >= pl.X1 && p.Pos.X <= pl.X2 && abs (p.Pos.Y - pl.Y) < 0.6)

let private applyFriction (p: Player) =
    { p with Vel = { p.Vel with X = p.Vel.X * groundFriction } }

let bodyCenter (p: Player) =
    { X = p.Pos.X; Y = p.Pos.Y - (fighterStats p.Fighter).Height / 2.0 }

let private airDrift (ax: float) (factor: float) (p: Player) =
    let s = stats p
    let vx = p.Vel.X

    let accelerated =
        if ax <> 0.0 then
            let target = vx + ax * s.AirAccel * factor
            // Nie über die Maximalgeschwindigkeit hinaus beschleunigen, vorhandenen Schwung aber behalten
            if abs target > s.AirSpeed && abs target > abs vx then vx else target
        else
            vx * airFriction

    let limited = if abs accelerated > s.AirSpeed then accelerated * 0.97 else accelerated
    { p with Vel = { p.Vel with X = limited } }

/// Fast-Fall: "Unten" im Fall. Im freien Fall reicht gehaltenes "Unten", während
/// Angriffen (z. B. Down-Air) nur ein frischer Tastendruck, damit man nicht versehentlich abstürzt.
let private tryFastFall (allowHeld: bool) (inp: Input) (p: Player) : Player * Effect list =
    let wanted =
        (pressed inp.Down p.Prev.Down && p.Vel.Y > -1.0)
        || (allowHeld && inp.Down && p.Vel.Y > 0.0)

    if not p.OnGround && wanted && not p.FastFall then
        { p with
            FastFall = true
            Vel = { p.Vel with Y = (stats p).FastFall } },
        [ SparksAt({ X = p.Pos.X; Y = p.Pos.Y - (stats p).Height }, { X = 0.0; Y = -1.0 }, 4, "#ffffff") ]
    else
        p, []

let private startAttack (m: MoveId) (inp: Input) (p: Player) =
    let ax = axisX inp
    let md = moveData p.Fighter m
    // Gespammte Projektil-Angriffe bekommen längere Recovery
    let uses = p.Stale |> List.truncate 5 |> List.filter (fun x -> x = m) |> List.length
    let lag = if md.Spawns.IsEmpty then 0 else uses * staleLagPerUse

    { p with
        State = AttackStartup m
        StateFrame = 0
        HasHit = false
        Charge = 0
        ExtraLag = lag
        SideUsed = p.SideUsed || (m = SideSpecial && not p.OnGround)
        Facing = (if ax <> 0.0 then ax else p.Facing) }

/// Boden: F = Jab/Tilts, G neutral = Smash bzw. Neutral-Special, G + Seite = Side-Special,
/// G + unten = Down-Smash, G + oben = Up-B
let private chooseGroundMove (inp: Input) (prev: Input) : MoveId option =
    if pressed inp.Special prev.Special then
        if inp.Up then Some UpSpecial
        elif inp.Down then Some DownSmash
        elif inp.Left || inp.Right then Some SideSpecial
        else Some ForwardSmash
    elif pressed inp.Attack prev.Attack then
        if inp.Up then Some UpTilt
        elif inp.Down then Some DownTilt
        elif inp.Left || inp.Right then Some ForwardTilt
        else Some Jab
    else
        None

/// Luft: F = Aerials, G neutral = Air-Smash, G + Seite = Side-Special (einmal pro Sprung),
/// G + unten = Down-Special, G + oben = Up-B
let private chooseAirMove (inp: Input) (prev: Input) (sideUsed: bool) : MoveId option =
    if pressed inp.Special prev.Special then
        if inp.Up then Some UpSpecial
        elif inp.Down then Some DownSpecial
        elif (inp.Left || inp.Right) && not sideUsed then Some SideSpecial
        else Some AirSmash
    elif pressed inp.Attack prev.Attack then
        if inp.Up then Some UpAir
        elif inp.Down then Some DownAir
        elif inp.Left || inp.Right then Some ForwardAir
        else Some NeutralAir
    else
        None

let private startRoll (dir: float) (p: Player) : Player * Effect list =
    { p with
        State = Dodging
        StateFrame = 0
        Timer = 20
        Vel = { X = dir * 9.0; Y = 0.0 }
        Facing = -dir
        Invuln = max p.Invuln 14 },
    [ Sound SfxDodge; DustAt(p.Pos, 4) ]

let private startSpotDodge (p: Player) : Player * Effect list =
    { p with
        State = Dodging
        StateFrame = 0
        Timer = 18
        Vel = Vec2.zero
        Invuln = max p.Invuln 13 },
    [ Sound SfxDodge ]

let private startAirDodge (inp: Input) (p: Player) : Player * Effect list =
    let dir = Vec2.normalize { X = axisX inp; Y = axisY inp }
    let vel = if dir = Vec2.zero then Vec2.scale 0.4 p.Vel else Vec2.scale 7.0 dir

    { p with
        State = Dodging
        StateFrame = 0
        Timer = 22
        Vel = vel
        AirDodged = true
        FastFall = false
        Invuln = max p.Invuln 16 },
    [ Sound SfxDodge ]

let private breakShield (p: Player) : Player * Effect list =
    { p with
        State = Hitstun
        StateFrame = 0
        Timer = 150
        Shield = 35.0
        OnGround = false
        Pos = { p.Pos with Y = p.Pos.Y - 2.0 }
        Vel = { X = 0.0; Y = -11.0 }
        HudShake = 1.0 },
    [ Sound SfxShieldBreak
      SparksAt(bodyCenter p, { X = 0.0; Y = -1.0 }, 18, "#9fe8ff")
      ShakeBy({ X = 0.0; Y = 1.0 }, 8.0) ]

let private makeProjectile (owner: int) (source: MoveId) (k: float) (pos: Vec2) (vel: Vec2) (kind: ProjectileKind) : Projectile =
    let hb, life = projectileData kind

    { Kind = kind
      Owner = owner
      Source = source
      Pos = pos
      Vel = vel
      Life = life
      MaxLife = life
      Reflected = false
      Hit =
        { hb with
            Damage = halfStep (hb.Damage * k)
            BaseKnockback = hb.BaseKnockback * k
            KnockbackScaling = hb.KnockbackScaling * k } }

/// Erzeugt die Projektile eines Angriffs. Ohne genug Energie zündet der Angriff ohne Projektil (Anti-Spam).
let private fireProjectiles (m: MoveId) (md: MoveData) (p: Player) : Player * Effect list =
    if md.Spawns.IsEmpty then
        p, []
    elif p.Energy < md.EnergyCost then
        { p with EnergyFlash = 40 },
        [ Sound SfxFizzle
          SmokeAt({ X = p.Pos.X + p.Facing * 24.0; Y = (bodyCenter p).Y }, "#9aa0b4")
          SparksAt({ X = p.Pos.X + p.Facing * 24.0; Y = (bodyCenter p).Y }, { X = p.Facing; Y = -0.5 }, 4, "#9aa0b4") ]
    else
        let center = bodyCenter p
        let k = staleFactor m p.Stale

        let projectiles =
            md.Spawns
            |> List.map (fun sp ->
                SpawnProjectile(
                    makeProjectile
                        p.Id
                        m
                        k
                        { X = center.X + sp.OffsetX * p.Facing; Y = center.Y + sp.OffsetY }
                        { X = sp.SpeedX * p.Facing; Y = sp.SpeedY }
                        sp.Kind
                ))

        { p with
            Energy = p.Energy - md.EnergyCost
            Stale = m :: List.truncate 8 p.Stale },
        Sound SfxProjectile :: projectiles

// ---------------------------------------------------------------------------
// Zustandsmaschine: Eingabe + aktueller Zustand -> neuer Zustand
// ---------------------------------------------------------------------------

/// Griff-Anlauf: aktiv in Frame 5–9, danach lange Erholung (verfehlte Griffe sind bestrafbar)
let private grabActiveFrom = 5
let private grabActiveTo = 9
let private grabWhiff = 30

let private startGrab (p: Player) : Player * Effect list =
    { (setState Grabbing p) with Vel = { p.Vel with X = p.Vel.X * 0.4 }; Timer = 0 }, [ Sound SfxSwing ]

let private actGrounded (l: Layout) (inp: Input) (p: Player) : Player * Effect list =
    let prev = p.Prev
    let ax = axisX inp
    let s = stats p

    // Griff: Schild + Angriff (schlägt Schilde, verliert gegen Angriffe)
    let grabbing = inp.Shield && pressed inp.Attack prev.Attack

    match (if grabbing then None else chooseGroundMove inp prev) with
    | Some m -> startAttack m inp (applyFriction p), []
    | None ->
        if grabbing then
            startGrab p
        elif inp.Shield then
            if pressed inp.Shield prev.Shield && ax <> 0.0 then startRoll ax p
            elif pressed inp.Shield prev.Shield && inp.Down then startSpotDodge p
            else setState Shielding (applyFriction p), [ Sound SfxShield ]
        elif pressed inp.Jump prev.Jump then
            setState JumpSquat p, []
        elif pressed inp.Down prev.Down && onSoftPlatform l p then
            { p with
                State = Falling
                StateFrame = 0
                OnGround = false
                DropThrough = 14
                Pos = { p.Pos with Y = p.Pos.Y + 1.0 } },
            []
        elif ax <> 0.0 then
            let vx = clamp -s.RunSpeed s.RunSpeed (p.Vel.X + ax * s.RunAccel)
            let turning = p.Vel.X * ax < -2.0
            let starting = p.State <> Running && abs p.Vel.X < 1.5
            // Staub beim Antritt, beim Wenden und in regelmäßigen Abständen im Sprint
            let sprinting = p.State = Running && abs vx > s.RunSpeed * 0.75 && p.StateFrame % 7 = 0

            let fx =
                (if turning || starting then [ DustAt(p.Pos, 3) ] else [])
                @ (if starting then [ Sound SfxDash ] else [])
                @ (if sprinting then [ DustAt({ p.Pos with X = p.Pos.X - ax * 8.0 }, 1) ] else [])

            { p with
                Vel = { p.Vel with X = vx }
                Facing = ax
                State = Running },
            fx
        else
            // Bremsstaub, wenn aus vollem Lauf angehalten wird
            let braking = p.State = Running && abs p.Vel.X > s.RunSpeed * 0.55
            { applyFriction p with State = Idle }, (if braking then [ DustAt(p.Pos, 3) ] else [])

let private actAirborne (inp: Input) (p: Player) : Player * Effect list =
    let prev = p.Prev
    let ax = axisX inp
    let s = stats p

    match chooseAirMove inp prev p.SideUsed with
    | Some m -> startAttack m inp (airDrift ax 1.0 p), []
    | None ->
        if pressed inp.Shield prev.Shield && not p.AirDodged then
            startAirDodge inp p
        elif pressed inp.Jump prev.Jump && p.JumpsLeft > 0 then
            { p with
                State = Jumping
                StateFrame = 0
                JumpsLeft = p.JumpsLeft - 1
                FastFall = false
                Facing = (if ax <> 0.0 then ax else p.Facing)
                Vel =
                    { X = (if ax <> 0.0 then ax * s.AirSpeed * 0.85 else p.Vel.X * 0.8)
                      Y = -s.AirJump } },
            [ Sound SfxDoubleJump; RingAt(p.Pos, "#ffffff") ]
        else
            let drifted = airDrift ax 1.0 p
            let fallen, fx = tryFastFall true inp drifted

            // Chrono schwebt, solange Oben gehalten wird
            let floated =
                if p.Fighter = Chrono && inp.Jump && prev.Jump && fallen.Vel.Y > 0.6 && fallen.Float > 0 && not fallen.FastFall then
                    { fallen with Vel = { fallen.Vel with Y = 0.6 }; Float = fallen.Float - 1 }
                else
                    fallen

            { floated with
                State = (if floated.Vel.Y < 0.0 then Jumping else Falling)
                DropThrough = (if inp.Down then max floated.DropThrough 2 else floated.DropThrough) },
            fx

/// Ende eines Angriffs oder Dodges: sofort wieder handlungsfähig (damit gebufferte Eingaben greifen)
let private actFree (l: Layout) (inp: Input) (p: Player) : Player * Effect list =
    if p.OnGround then actGrounded l inp (setState Idle p)
    else actAirborne inp (setState Falling p)

let private actLedge (l: Layout) (inp: Input) (p: Player) : Player * Effect list =
    let prev = p.Prev
    let side = p.Facing
    let ledgeX = if side > 0.0 then l.Left else l.Right
    let s = stats p

    let towardPressed =
        (side > 0.0 && pressed inp.Right prev.Right)
        || (side < 0.0 && pressed inp.Left prev.Left)

    let awayPressed =
        (side > 0.0 && pressed inp.Left prev.Left)
        || (side < 0.0 && pressed inp.Right prev.Right)

    if p.StateFrame < 8 then
        p, []
    elif pressed inp.Jump prev.Jump then
        { p with
            State = Jumping
            StateFrame = 0
            Vel = { X = side * 2.6; Y = -ledgeJumpSpeed }
            JumpsLeft = s.AirJumps
            LedgeCooldown = 40
            Invuln = max p.Invuln 12 },
        [ Sound SfxJump ]
    elif towardPressed || pressed inp.Attack prev.Attack || pressed inp.Shield prev.Shield then
        { p with
            State = Landing
            StateFrame = 0
            Timer = 10
            Pos = { X = ledgeX + side * (s.Width / 2.0 + 11.0); Y = l.Top }
            Vel = Vec2.zero
            OnGround = true
            LedgeCooldown = 30
            Invuln = max p.Invuln 16 },
        [ DustAt({ X = ledgeX + side * 28.0; Y = l.Top }, 3) ]
    elif awayPressed || pressed inp.Down prev.Down || p.StateFrame > 240 then
        { p with
            State = Falling
            StateFrame = 0
            Pos = { p.Pos with X = p.Pos.X - side * 4.0 }
            Vel = Vec2.zero
            LedgeCooldown = 45 },
        []
    else
        p, []

let private overMainStage (l: Layout) (p: Player) =
    p.Pos.X > l.Left + 4.0 && p.Pos.X < l.Right - 4.0 && p.Pos.Y < l.Top

let private actAttack (l: Layout) (inp: Input) (p: Player) : Player * Effect list =
    let ax = axisX inp

    match p.State with
    | AttackStartup m ->
        let md = moveData p.Fighter m
        let moved = if p.OnGround then applyFriction p else airDrift ax 0.8 p
        let moved, fx = tryFastFall false inp moved

        // Teleport und Schattenflug: im Startup kurz ungreifbar bzw. in der Luft stehend
        let moved =
            if md.Teleport > 0.0 || md.Phantom then { moved with Invuln = max moved.Invuln 2; Vel = Vec2.scale 0.6 moved.Vel }
            elif md.Flight > 0.0 then { moved with Vel = Vec2.scale 0.85 moved.Vel }
            else moved

        // Smash-Angriffe am Boden lassen sich durch Halten von G aufladen (bis 1,4-fach)
        let chargeable =
            p.OnGround && (m = ForwardSmash || m = DownSmash) && md.Spawns.IsEmpty && not md.Counter
            && not md.Hitboxes.IsEmpty && md.Startup >= 4

        let holdFrame = md.Startup - 2

        if chargeable && inp.Special && p.StateFrame >= holdFrame - 1 && p.Charge < maxCharge then
            let c = bodyCenter p

            { moved with Charge = p.Charge + 1; StateFrame = holdFrame - 1 },
            fx
            @ (if p.Charge % 5 = 0 then [ SparksAt({ X = c.X + (if p.Charge % 10 = 0 then 14.0 else -14.0); Y = c.Y - 10.0 }, { X = 0.0; Y = -1.0 }, 2, (stats p).Color) ] else [])
            @ (if p.Charge = 0 then [ Sound SfxCharge ] else [])
        elif p.StateFrame + 1 >= md.Startup then
            let launched =
                md.BoostY < 0.0
                || md.Flight > 0.0
                || (md.HoldVelY |> Option.exists (fun v -> v < 0.0))

            let vx =
                if md.BoostY < 0.0 then moved.Vel.X * 0.4 + ax * 3.4 + moved.Facing * md.BoostX
                elif md.BoostX <> 0.0 then moved.Facing * md.BoostX
                else moved.Vel.X

            let vy = if md.BoostY <> 0.0 then md.BoostY else moved.Vel.Y

            let activated =
                { moved with
                    State = AttackActive m
                    StateFrame = 0
                    OnGround = (if launched then false else moved.OnGround)
                    FastFall = (if launched then false else moved.FastFall)
                    Pos = (if launched then { moved.Pos with Y = moved.Pos.Y - 1.0 } else moved.Pos)
                    Vel = { X = vx; Y = vy } }

            // Richtung aus der gehaltenen Eingabe (Standard: nach oben)
            let wanted = { X = axisX inp; Y = axisY inp }
            let dir = if wanted = Vec2.zero then { X = 0.0; Y = -1.0 } else Vec2.normalize wanted
            let faceOf (d: Vec2) (fallback: float) = if d.X > 0.0 then 1.0 elif d.X < 0.0 then -1.0 else fallback

            let moved, moveFx =
                if md.Teleport > 0.0 then
                    let target = Vec2.add activated.Pos (Vec2.scale md.Teleport dir)
                    let tint = if p.Fighter = Chrono then "#bff8ee" else "#d7c8ff"

                    { activated with
                        Pos = target
                        Vel = Vec2.scale 2.0 dir
                        OnGround = false
                        Facing = faceOf dir activated.Facing },
                    [ Sound SfxTeleport
                      SmokeAt(bodyCenter p, tint)
                      SmokeAt({ X = target.X; Y = target.Y - 28.0 }, tint)
                      RingAt({ X = target.X; Y = target.Y - 28.0 }, (stats p).Color) ]
                elif md.Flight > 0.0 then
                    { activated with
                        Vel = Vec2.scale md.Flight dir
                        OnGround = false
                        Facing = faceOf dir activated.Facing },
                    [ Sound SfxUpSpecial; ShadowsAt(bodyCenter p, 6) ]
                elif md.Phantom then
                    activated, [ PhantomStep p.Id; Sound SfxTeleport; ShadowsAt(bodyCenter p, 8) ]
                else
                    activated, []

            let fired, fireFx = fireProjectiles m md moved

            let sound =
                if not md.Spawns.IsEmpty then []
                elif m = UpSpecial then [ Sound SfxUpSpecial ]
                elif md.HoldVelX.IsSome then [ Sound SfxDash ]
                elif p.Fighter = Chrono && (m = ForwardSmash || m = AirSmash) then [ Sound SfxRift ]
                else [ Sound SfxSwing ]

            fired,
            fx
            @ moveFx
            @ sound
            @ fireFx
            @ (if launched && md.Flight = 0.0 then [ RingAt(p.Pos, "#ffe27a") ] else [])
        else
            moved, fx
    | AttackActive m ->
        let md = moveData p.Fighter m
        let moved = if p.OnGround && md.HoldVelX.IsNone then applyFriction p else airDrift ax 0.8 p
        let moved, fx = if md.HoldVelY.IsSome || md.Flight > 0.0 then moved, [] else tryFastFall false inp moved

        let held =
            if md.Flight > 0.0 then
                // Schattenflug: Geschwindigkeit bleibt konstant
                { moved with Vel = p.Vel }
            else
                { moved with
                    Vel =
                        { X = (match md.HoldVelX with Some vx -> vx * moved.Facing | None -> moved.Vel.X)
                          Y = (match md.HoldVelY with Some vy when not moved.OnGround -> vy | Some _ -> 0.0 | None -> moved.Vel.Y) } }

        if p.StateFrame + 1 >= md.Active then
            let afterVel =
                if md.HoldVelX.IsSome then { held.Vel with X = held.Vel.X * 0.35 }
                elif md.Flight > 0.0 then Vec2.scale 0.35 held.Vel
                else held.Vel

            setState (AttackRecovery m) { held with Vel = afterVel }, fx
        else
            held, fx
    | AttackRecovery m ->
        let md = moveData p.Fighter m
        let moved = if p.OnGround then applyFriction p else airDrift ax 0.8 p
        let moved, fx = tryFastFall false inp moved

        // Titans Jetpack Slam: über der Stage geht es mit voller Wucht nach unten
        let moved =
            match md.SlamVelY with
            | Some vy when not moved.OnGround && overMainStage l moved -> { moved with Vel = { X = moved.Vel.X * 0.9; Y = vy } }
            | _ -> moved

        if p.StateFrame + 1 >= md.Recovery + p.ExtraLag then
            if moved.OnGround then actFree l inp moved
            elif md.Helpless then setState Helpless moved, fx
            else actFree l inp moved
        else
            moved, fx
    | _ -> p, []

let private act (l: Layout) (inp: Input) (p: Player) : Player * Effect list =
    let prev = p.Prev
    let ax = axisX inp
    let s = stats p

    match p.State with
    | Idle
    | Running -> actGrounded l inp p
    | Landing ->
        if p.Timer <= 0 then actGrounded l inp (setState Idle p)
        else { applyFriction p with Timer = p.Timer - 1 }, []
    | JumpSquat ->
        // Up-B direkt aus dem Absprung; ein Angriff bleibt gebuffert und wird zur Luftattacke
        if pressed inp.Special prev.Special && inp.Up then
            startAttack UpSpecial inp p, []
        elif p.StateFrame + 1 >= jumpSquatFrames then
            let speed = if inp.Jump then s.FullHop else s.ShortHop

            { p with
                State = Jumping
                StateFrame = 0
                OnGround = false
                JumpsLeft = s.AirJumps
                FastFall = false
                Pos = { p.Pos with Y = p.Pos.Y - 1.0 }
                Vel = { X = p.Vel.X; Y = -speed } },
            [ Sound SfxJump; DustAt(p.Pos, 5) ]
            @ (if p.Fighter = Bramble then [ LeavesAt(p.Pos, 4) ] else [])
        else
            p, []
    | Jumping
    | Falling -> actAirborne inp p
    | Grabbing ->
        let q = applyFriction p
        if p.StateFrame >= grabWhiff then actGrounded l inp (setState Idle q) else q, []
    | Holding -> applyFriction p, []
    | Held ->
        // Befreien durch Hämmern: jede neu gedrückte Taste zählt
        let mash =
            [ pressed inp.Attack prev.Attack
              pressed inp.Special prev.Special
              pressed inp.Jump prev.Jump
              pressed inp.Shield prev.Shield
              pressed inp.Left prev.Left
              pressed inp.Right prev.Right ]
            |> List.filter id
            |> List.length

        { p with Timer = p.Timer + mash }, []
    | Helpless ->
        let drifted = airDrift ax 0.7 p
        tryFastFall true inp drifted
    | AttackStartup _
    | AttackActive _
    | AttackRecovery _ -> actAttack l inp p
    | Hitstun ->
        let groundDrag = if p.OnGround then 0.9 else 1.0

        let moved =
            { p with
                Timer = p.Timer - 1
                Vel =
                    { X = (p.Vel.X * knockbackDecay + ax * diDrift) * groundDrag
                      Y = p.Vel.Y * knockbackDecay } }

        if moved.Timer <= 0 then actFree l inp moved
        else moved, []
    | Shielding ->
        let drained = applyFriction { p with Shield = p.Shield - shieldDrain }

        if drained.Shield <= 0.0 then
            breakShield drained
        elif pressed inp.Jump prev.Jump then
            setState JumpSquat drained, []
        elif pressed inp.Attack prev.Attack then
            startGrab drained
        elif ax <> 0.0 && (pressed inp.Left prev.Left || pressed inp.Right prev.Right) then
            startRoll ax drained
        elif pressed inp.Down prev.Down then
            startSpotDodge drained
        elif not inp.Shield && p.StateFrame >= 3 then
            actGrounded l { inp with Shield = false } (setState Idle drained)
        else
            drained, []
    | Dodging ->
        let timer = p.Timer - 1

        let vel =
            if p.OnGround then
                let vx = p.Vel.X * 0.93
                // Rollen stoppt an der Plattformkante
                let supported = (supportAt l (p.Pos.X + vx) p.Pos.Y 0).IsSome
                { X = (if supported then vx else 0.0); Y = 0.0 }
            else
                Vec2.scale 0.9 p.Vel

        let moved = { p with Timer = timer; Vel = vel }

        if timer <= 0 then actFree l { inp with Shield = false } moved
        else moved, []
    | LedgeHang -> actLedge l inp p
    | Respawning ->
        let f = p.StateFrame
        let t = min 1.0 (float f / 45.0)
        let eased = 1.0 - (1.0 - t) * (1.0 - t)
        let y = -60.0 + (respawnY + 60.0) * eased

        if f > 45 && (Input.any inp || f > 210) then
            { p with
                State = Falling
                StateFrame = 0
                OnGround = false
                JumpsLeft = s.AirJumps
                Invuln = respawnInvulnFrames
                Pos = { X = respawnX l p.Id; Y = respawnY }
                Vel = Vec2.zero },
            []
        else
            { p with
                Pos = { X = respawnX l p.Id; Y = y }
                Vel = Vec2.zero
                Invuln = 2 },
            []
    | KO ->
        if p.Stocks > 0 && p.Timer <= 1 then
            { p with
                State = Respawning
                StateFrame = 0
                Timer = 0
                Percent = 0.0
                Shield = shieldMax
                Facing = (if p.Id = 0 then 1.0 else -1.0)
                Pos = { X = respawnX l p.Id; Y = -60.0 }
                Vel = Vec2.zero
                OnGround = false
                FastFall = false
                AirDodged = false
                SideUsed = false
                Combo = 0
                LastHitBy = None
                Energy = max p.Energy (energyMax * 0.5)
                Slow = 0
                Invuln = 2 },
            []
        else
            { p with Timer = max 0 (p.Timer - 1) }, []

// ---------------------------------------------------------------------------
// Input-Buffer: Ein Tastendruck bleibt einige Frames gültig, bis die Aktion möglich ist
// ---------------------------------------------------------------------------

let private bufferInput (raw: Input) (p: Player) : Player * Input * Input =
    let tick (now: bool) (before: bool) (buf: int) =
        if pressed now before then inputBuffer else max 0 (buf - 1)

    let bj = tick raw.Jump p.Prev.Jump p.BufJump
    let ba = tick raw.Attack p.Prev.Attack p.BufAttack
    let bs = tick raw.Special p.Prev.Special p.BufSpecial
    let buffered = { p with BufJump = bj; BufAttack = ba; BufSpecial = bs }

    // Effektive Eingabe: gebufferte Tasten gelten als frisch gedrückt
    let effInput =
        { raw with
            Jump = raw.Jump || bj > 0
            Attack = raw.Attack || ba > 0
            Special = raw.Special || bs > 0 }

    let effPrev =
        { p.Prev with
            Jump = p.Prev.Jump && bj = 0
            Attack = p.Prev.Attack && ba = 0
            Special = p.Prev.Special && bs = 0 }

    buffered, effInput, effPrev

/// Verbrauchte Buffer löschen, sobald die zugehörige Aktion ausgelöst wurde
let private consumeBuffers (before: Player) (after: Player) : Player =
    let startedAttack =
        match after.State with
        | AttackStartup _ -> after.StateFrame = 0 && after.State <> before.State
        | _ -> false

    let startedJump =
        (after.State = JumpSquat && before.State <> JumpSquat)
        || after.JumpsLeft < before.JumpsLeft
        || (before.State = LedgeHang && after.State <> LedgeHang)

    let interrupted = after.State = Hitstun || before.State = LedgeHang && after.State <> LedgeHang

    { after with
        BufJump = (if startedJump || interrupted then 0 else after.BufJump)
        BufAttack = (if startedAttack || interrupted then 0 else after.BufAttack)
        BufSpecial = (if startedAttack || interrupted then 0 else after.BufSpecial) }

// ---------------------------------------------------------------------------
// Integration & Kollision
// ---------------------------------------------------------------------------

let private isFrozen (p: Player) =
    match p.State with
    | LedgeHang
    | Respawning
    | Held
    | KO -> true
    | _ -> false

let private integrate (p: Player) : Player =
    let s = stats p

    if p.OnGround then
        { p with
            Pos = { p.Pos with X = p.Pos.X + p.Vel.X }
            Vel = { p.Vel with Y = 0.0 } }
    else
        let held =
            match p.State with
            | AttackActive m ->
                let md = moveData p.Fighter m
                md.HoldVelY.IsSome || md.Flight > 0.0
            | _ -> false

        let g =
            match p.State with
            | _ when held -> 0.0
            | Hitstun -> s.Gravity * 0.8
            | Dodging -> s.Gravity * 0.35
            | _ -> s.Gravity

        let cap =
            match p.State with
            | Hitstun -> 40.0
            | AttackRecovery _ -> max (if p.FastFall then s.FastFall else s.MaxFall) p.Vel.Y
            | _ when held -> 40.0
            | _ -> if p.FastFall then s.FastFall else s.MaxFall

        let vy = min cap (p.Vel.Y + g)

        { p with
            Vel = { p.Vel with Y = vy }
            Pos = { X = p.Pos.X + p.Vel.X; Y = p.Pos.Y + vy } }

let private landingLagFor (p: Player) : int =
    match p.State with
    | AttackStartup m
    | AttackActive m
    | AttackRecovery m -> max 4 (moveData p.Fighter m).LandingLag
    | Helpless -> 12
    | Dodging -> 8
    | _ -> if p.FastFall then 4 else 2

let private landsWithShockwave (p: Player) =
    match p.State with
    | AttackStartup m
    | AttackActive m
    | AttackRecovery m -> (moveData p.Fighter m).LandShockwave
    | _ -> false

/// Bewegliche Plattformen nehmen Kämpfer mit, die darauf stehen
let private carryOnPlatform (l: Layout) (p: Player) : Player =
    if not p.OnGround then
        p
    else
        let carrier =
            l.Soft
            |> List.tryFind (fun pl ->
                (pl.Vx <> 0.0 || pl.Vy <> 0.0)
                && p.Pos.X >= pl.X1 - pl.Vx
                && p.Pos.X <= pl.X2 - pl.Vx
                && abs (p.Pos.Y - (pl.Y - pl.Vy)) < 0.8
                && p.DropThrough = 0)

        match carrier with
        | Some pl -> { p with Pos = { X = p.Pos.X + pl.Vx; Y = pl.Y } }
        | None -> p

/// Landen auf Plattformen (Soft-Platforms nur von oben und nicht bei Drop-Through)
let private resolveLanding (l: Layout) (prevY: float) (p: Player) : Player * Effect list =
    if p.OnGround || p.Vel.Y < 0.0 then
        p, []
    else
        let candidate =
            allPlatforms l
            |> List.filter (fun pl ->
                p.Pos.X >= pl.X1
                && p.Pos.X <= pl.X2
                && prevY <= pl.Y - pl.Vy + 0.01
                && p.Pos.Y >= pl.Y
                && (pl.Solid || p.DropThrough = 0))
            |> List.sortBy (fun pl -> pl.Y)
            |> List.tryHead

        match candidate with
        | None -> p, []
        | Some pl ->
            let impact = p.Vel.Y
            let at = { X = p.Pos.X; Y = pl.Y }
            let jumps = (stats p).AirJumps

            if p.State = Hitstun && impact > 7.0 then
                // Meteor-Spike bzw. harter Aufprall: vom Boden abprallen
                { p with
                    Pos = { at with Y = pl.Y - 1.0 }
                    Vel = { X = p.Vel.X * 0.8; Y = -impact * 0.55 } },
                [ DustAt(at, 8); Sound SfxLand; ShakeBy({ X = 0.0; Y = 1.0 }, min 10.0 (impact * 0.5)) ]
            elif p.State = Hitstun then
                { p with
                    Pos = at
                    Vel = { p.Vel with Y = 0.0 }
                    OnGround = true
                    FastFall = false
                    AirDodged = false
                    SideUsed = false
                    Float = floatFrames
                    JumpsLeft = jumps },
                [ DustAt(at, 3) ]
            else
                let shock = landsWithShockwave p

                { p with
                    Pos = at
                    Vel = { p.Vel with Y = 0.0 }
                    OnGround = true
                    FastFall = false
                    AirDodged = false
                    SideUsed = false
                    Float = floatFrames
                    JumpsLeft = jumps
                    State = Landing
                    StateFrame = 0
                    Timer = landingLagFor p },
                [ DustAt(at, (if impact > 12.0 then 7 else 4)); Sound SfxLand ]
                @ (if shock then [ ShockwaveAt(p.Id, at) ] else [])

/// Massiver Körper der Hauptplattform (Seitenwände und Unterseite)
let private resolveSolidBody (l: Layout) (prev: Vec2) (p: Player) : Player =
    let half = (stats p).Width / 2.0
    let height = (stats p).Height
    let insideX = p.Pos.X + half > l.Left && p.Pos.X - half < l.Right
    let insideY = p.Pos.Y > l.Top + 0.5 && p.Pos.Y - height < l.Bottom

    if p.OnGround || not (insideX && insideY) then
        p
    else
        let bounce = if p.State = Hitstun then -0.6 else 0.0

        if prev.X + half <= l.Left + 1.0 then
            { p with
                Pos = { p.Pos with X = l.Left - half }
                Vel = { p.Vel with X = min 0.0 (p.Vel.X * bounce) } }
        elif prev.X - half >= l.Right - 1.0 then
            { p with
                Pos = { p.Pos with X = l.Right + half }
                Vel = { p.Vel with X = max 0.0 (p.Vel.X * bounce) } }
        elif prev.Y - height >= l.Bottom - 1.0 then
            { p with
                Pos = { p.Pos with Y = l.Bottom + height }
                Vel = { p.Vel with Y = max 0.0 (p.Vel.Y * bounce) } }
        elif p.Pos.Y - l.Top < 24.0 then
            // Knapp unter der Oberkante (z. B. nach Teleport): auf die Stage heben
            { p with Pos = { p.Pos with Y = l.Top - 0.5 }; Vel = { p.Vel with Y = 0.0 } }
        else
            // Fallback: zur näheren Seite hinausschieben
            let toLeft = p.Pos.X < stageCenter l

            { p with
                Pos = { p.Pos with X = (if toLeft then l.Left - half else l.Right + half) }
                Vel = { p.Vel with X = 0.0 } }

/// Bodenkontakt prüfen (z. B. von der Kante laufen oder Drop-Through)
let private resolveSupport (l: Layout) (p: Player) : Player =
    if not p.OnGround then
        p
    else
        match supportAt l p.Pos.X p.Pos.Y p.DropThrough with
        | Some _ -> p
        | None ->
            let next =
                match p.State with
                | Idle
                | Running
                | Landing
                | JumpSquat
                | Shielding -> Falling
                | other -> other

            { p with
                OnGround = false
                State = next
                StateFrame = (if next = p.State then p.StateFrame else 0) }

let private canGrabLedge (p: Player) =
    match p.State with
    | Falling
    | Helpless
    | AttackRecovery UpSpecial -> true
    | _ -> false

let private resolveLedge (l: Layout) (inp: Input) (p: Player) : Player * Effect list =
    if p.OnGround || p.Vel.Y <= 0.0 || p.LedgeCooldown > 0 || inp.Down || not (canGrabLedge p) then
        p, []
    else
        let inY = p.Pos.Y > l.Top + 2.0 && p.Pos.Y < l.Top + 84.0
        let leftGrab = inY && p.Pos.X > l.Left - 50.0 && p.Pos.X < l.Left - 2.0
        let rightGrab = inY && p.Pos.X < l.Right + 50.0 && p.Pos.X > l.Right + 2.0

        if leftGrab || rightGrab then
            let side = if leftGrab then 1.0 else -1.0
            let ledgeX = if leftGrab then l.Left else l.Right
            let s = stats p

            { p with
                State = LedgeHang
                StateFrame = 0
                Pos = { X = ledgeX - side * 15.0 * s.Scale; Y = l.Top + 54.0 * s.Scale }
                Vel = Vec2.zero
                Facing = side
                JumpsLeft = s.AirJumps
                FastFall = false
                AirDodged = false
                SideUsed = false
                Float = floatFrames
                Invuln = max p.Invuln ledgeInvulnFrames },
            [ Sound SfxLedge; SparksAt({ X = ledgeX; Y = l.Top }, { X = -side; Y = -1.0 }, 3, "#ffffff") ]
        else
            p, []

let private canUseUltra (p: Player) =
    match p.State with
    | Idle
    | Running
    | Jumping
    | Falling
    | Landing -> true
    | _ -> false

/// Ein kompletter 60Hz-Schritt für einen Spieler
let stepPlayer (l: Layout) (raw: Input) (p: Player) : Player * Effect list =
    let buffered, inp, effPrev = bufferInput raw p
    let acting = { buffered with Prev = effPrev }

    // Ultra-Smash: Wer den Smash-Ball hat, löst ihn mit G aus
    let acted, fx1 =
        if acting.HasFinal && canUseUltra acting && pressed inp.Special effPrev.Special then
            { acting with
                HasFinal = false
                BufSpecial = 0
                State = (if acting.OnGround then Idle else Falling)
                StateFrame = 0
                Vel = Vec2.zero },
            [ StartUltra acting.Id ]
        else
            let a, fx = act l inp acting
            consumeBuffers acting a, fx

    // StateFrame zählt die Frames im aktuellen Zustand
    let framed =
        if acted.State = p.State && acted.StateFrame = p.StateFrame then
            { acted with StateFrame = p.StateFrame + 1 }
        else
            acted

    let timed =
        { framed with
            Invuln = max 0 (framed.Invuln - 1)
            LedgeCooldown = max 0 (framed.LedgeCooldown - 1)
            DropThrough = max 0 (framed.DropThrough - 1)
            HudShake = framed.HudShake * 0.88
            HitFlash = max 0 (framed.HitFlash - 1)
            ComboTimer = max 0 (framed.ComboTimer - 1)
            EnergyFlash = max 0 (framed.EnergyFlash - 1)
            PowerTimer = max 0 (framed.PowerTimer - 1)
            SpeedTimer = max 0 (framed.SpeedTimer - 1)
            ParryFlash = max 0 (framed.ParryFlash - 1)
            Energy = min energyMax (framed.Energy + energyRegen)
            Combo = (if framed.State = Hitstun then framed.Combo else 0)
            Shield = (if framed.State = Shielding then framed.Shield else min shieldMax (framed.Shield + shieldRegen))
            Prev = raw }

    if isFrozen timed then
        updateCloth timed, fx1
    else
        let carried = carryOnPlatform l timed
        let prev = carried.Pos
        let moved = integrate carried
        let landed, fx2 = resolveLanding l prev.Y moved
        let pushed = resolveSolidBody l prev landed
        let supported = resolveSupport l pushed
        let grabbed, fx3 = resolveLedge l inp supported
        updateCloth grabbed, fx1 @ fx2 @ fx3

// ---------------------------------------------------------------------------
// Kampf: Hitbox gegen Hurtbox, Smash-Knockback-Formel, Gewicht, DI, Schild, Super-Armor, Stale-Moves
// ---------------------------------------------------------------------------

let private degToRad (d: float) = d * Math.PI / 180.0

/// Knockback = (BaseKnockback + KnockbackScaling * TargetPercent) * Gewichtsfaktor
let knockbackFor (hb: HitboxDef) (targetPercent: float) (target: Fighter) : float =
    (hb.BaseKnockback + hb.KnockbackScaling * targetPercent) * weightFactor target

let private isArmored (p: Player) =
    match p.State with
    | AttackStartup m
    | AttackActive m -> (moveData p.Fighter m).Armor
    | _ -> false

/// Element-Effekte des Angreifers
let private elementFx (attacker: Fighter) (contact: Vec2) (dir: Vec2) (damage: float) : Effect list =
    match attacker with
    | Aether -> [ BoltsAt(contact, 3 + int (damage / 4.0), "#9be8ff") ]
    | Ignis -> [ FlamesAt(contact, dir, 6 + int (damage / 1.5)) ]
    | Titan -> [ RingAt(contact, "#ffd166"); DustAt({ contact with Y = contact.Y + 10.0 }, 3 + int (damage / 4.0)) ]
    | Zephyr -> [ BoltsAt(contact, 2 + int (damage / 5.0), "#c9a2ff"); SparksAt(contact, dir, 4, "#e2d4ff") ]
    | Valkyrie -> [ SparksAt(contact, dir, 6, "#fff3b0"); RingAt(contact, "#ffe08a") ]
    | Chrono -> [ RingAt(contact, "#4fe0c8"); BoltsAt(contact, 2, "#bff8ee") ]
    | Bramble -> [ LeavesAt(contact, 5 + int (damage / 3.0)) ]
    | Specter -> [ ShadowsAt(contact, 5 + int (damage / 3.0)); SparksAt(contact, dir, 3, "#ff4f7b") ]
    | Ronin -> [ SparksAt(contact, dir, 8, "#ffffff"); SparksAt(contact, dir, 4, "#ff8aa0"); RingAt(contact, "#ffd0d8") ]
    | Lyra -> [ SparksAt(contact, dir, 6, "#e8ecff"); RingAt(contact, "#b8c0ff") ]
    | Gorm -> [ DustAt({ contact with Y = contact.Y + 8.0 }, 5 + int (damage / 3.0)); RingAt(contact, "#d9a46a"); SparksAt(contact, dir, 4, "#ffb347") ]

type private Attack =
    { AttackerId: int
      AttackerFighter: Fighter
      Dir: float
      Hit: HitboxDef
      Contact: Vec2
      /// Angriff für die Stale-Move-Negation (None = Projektil oder Ultra, bereits verrechnet)
      Move: MoveId option
      Melee: bool
      IgnoreShield: bool }

let private applyHit (atk: Attack) (target: Player) : Player * Effect list =
    let hb = atk.Hit
    let dir = atk.Dir

    if target.State = Shielding && not atk.IgnoreShield && target.StateFrame < parryWindow then
        // Perfect Shield: frisch gehobener Schild pariert, Angreifer im Nahkampf ist kurz wehrlos
        { target with ParryFlash = 22 },
        [ Sound SfxParry
          RingAt(atk.Contact, "#7df9ff")
          SparksAt(atk.Contact, { X = -dir; Y = -0.5 }, 12, "#ffffff")
          HitStopFor 10 ]
        @ (if atk.Melee && atk.AttackerId >= 0 then [ Stun(atk.AttackerId, 20) ] else [])
    elif target.State = Shielding && not atk.IgnoreShield then
        let shield = target.Shield - hb.Damage * 1.3

        let blocked =
            { target with
                Shield = shield
                Vel = { target.Vel with X = dir * (1.5 + hb.Damage * 0.18) }
                HudShake = 0.4 }

        if shield <= 0.0 then
            let broken, fx = breakShield blocked
            broken, fx @ [ HitStopFor 8 ]
        else
            blocked, [ Sound SfxShieldHit; SparksAt(atk.Contact, { X = -dir; Y = -0.4 }, 6, "#9fe8ff"); HitStopFor 3 ]
    else
        let percent = min maxPercent (target.Percent + hb.Damage)
        let kb = knockbackFor hb percent target.Fighter

        let credit =
            [ DamageCredit(atk.AttackerId, hb.Damage) ]
            @ (if atk.Melee then [ EnergyGain(atk.AttackerId, hb.Damage * energyPerMeleeDamage) ] else [])
            @ (match atk.Move with
               | Some m -> [ StaleHit(atk.AttackerId, m) ]
               | None -> [])

        let gained = min energyMax (target.Energy + hb.Damage * energyPerDamageTaken)

        if isArmored target && kb < armorBreakKnockback && not atk.IgnoreShield then
            // Super-Armor: Schaden ja, Unterbrechung nein
            { target with
                Percent = percent
                HitFlash = 4
                Energy = gained
                HudShake = min 1.2 (0.4 + hb.Damage * 0.05)
                LastHitBy = Some atk.AttackerId },
            [ Sound SfxArmor
              SparksAt(atk.Contact, { X = -dir; Y = -0.5 }, 8, "#ffd166")
              RingAt(atk.Contact, "#ffd166")
              HitStopFor 4 ]
            @ credit
        else
            let speed = kb * knockbackToSpeed
            let baseAngle = degToRad hb.Angle
            let lx = dcos baseAngle * dir
            let ly = -dsin baseAngle

            // DI: Eingaben senkrecht zur Flugrichtung drehen den Winkel leicht
            let held = target.Prev
            let ix = axisX held
            let iy = axisY held
            let cross = lx * iy - ly * ix
            let turn = degToRad (diMaxDegrees * clamp -1.0 1.0 cross)
            let vx = (lx * dcos turn - ly * dsin turn) * speed
            let vyRaw = (lx * dsin turn + ly * dcos turn) * speed

            // Geerdete Ziele prallen bei Abwärtstreffern vom Boden ab
            let vy = if target.OnGround && vyRaw > 0.0 then -vyRaw * 0.8 else vyRaw
            let heavy = kb >= 58.0 || hb.Damage >= 12.0
            let hitstun = max minHitstun (int (kb * hitstunPerKnockback))
            let hitStop = int (clamp 3.0 10.0 (2.0 + hb.Damage * 0.3 + kb * 0.03))
            let launchDir = Vec2.normalize { X = vx; Y = vy }
            let combo = if target.State = Hitstun then target.Combo + 1 else 1

            { target with
                Percent = percent
                State = Hitstun
                StateFrame = 0
                Timer = hitstun
                Vel = { X = vx; Y = vy }
                OnGround = false
                Pos = { target.Pos with Y = target.Pos.Y - 2.0 }
                FastFall = false
                HasHit = false
                SideUsed = false
                HitFlash = 5
                Combo = combo
                Energy = gained
                LastHitBy = Some atk.AttackerId
                BufJump = 0
                BufAttack = 0
                BufSpecial = 0
                HudShake = min 1.6 (0.5 + hb.Damage * 0.07) },
            [ Sound(if heavy then SfxHeavyHit else SfxHit)
              SparksAt(atk.Contact, launchDir, 5 + int hb.Damage, "#ffffff")
              SparksAt(atk.Contact, launchDir, 3 + int (hb.Damage / 3.0), (fighterStats atk.AttackerFighter).Color)
              BurstAt(atk.Contact, 16.0 + hb.Damage * 2.2)
              HitStopFor hitStop ]
            @ elementFx atk.AttackerFighter atk.Contact launchDir hb.Damage
            @ credit
            @ (if combo >= 2 then [ ComboCredit(atk.AttackerId, combo); Sound SfxCombo ] else [])
            @ (if heavy then [ ShakeBy(launchDir, min 20.0 (4.0 + kb * 0.11)); RingAt(atk.Contact, "#ffffff") ] else [])
            @ (if kb >= 75.0 then [ ImpactFrameAt atk.Contact ] else [])

let private hittable (target: Player) =
    target.Invuln = 0
    && (match target.State with
        | KO
        | Respawning -> false
        | _ -> true)

let private currentMove (p: Player) =
    match p.State with
    | AttackActive m -> Some m
    | _ -> None

let private findHit (attacker: Player) (target: Player) : (HitboxDef * Vec2) option =
    let commandGrab =
        match attacker.State with
        | AttackActive m -> (moveData attacker.Fighter m).CommandGrab
        | _ -> false

    if attacker.Id = target.Id || attacker.HasHit || commandGrab || not (hittable target) then
        None
    else
        let hurt = hurtbox target

        activeHitboxes attacker
        |> List.tryFind (fun (rect, _) -> Rect.overlaps rect hurt)
        |> Option.map (fun (rect, hb) -> hb, Rect.overlapCenter rect hurt)

/// Alle Nahkampf-Treffer werden gegen den Zustand VOR dem Schritt geprüft (Trades sind möglich)
let private resolveCombat (players: Player list) : Player list * Effect list =
    let rawHits =
        players
        |> List.collect (fun attacker ->
            players
            |> List.choose (fun target ->
                findHit attacker target
                |> Option.map (fun (hb, contact) -> attacker, target.Id, hb, contact)))

    // Konter: Trifft ein Nahkampfangriff einen Kämpfer in Konter-Haltung, schlägt dieser zurück
    let isCountering (p: Player) =
        match p.State with
        | AttackActive m -> (moveData p.Fighter m).Counter
        | _ -> false

    let hits =
        rawHits
        |> List.map (fun (attacker: Player, targetId, hb: HitboxDef, contact) ->
            let target = players |> List.find (fun q -> q.Id = targetId)

            if isCountering target && not (isCountering attacker) then
                let counterHb = hitbox 0.0 0.0 0.0 0.0 (halfStep (max 10.0 (hb.Damage * 1.4))) 58.0 0.74 38.0
                { target with Facing = (if attacker.Pos.X >= target.Pos.X then 1.0 else -1.0) }, attacker.Id, counterHb, contact
            else
                attacker, targetId, hb, contact)

    let counters =
        hits
        |> List.filter (fun (attacker: Player, _, _, _) -> isCountering attacker)

    let counterFx =
        counters
        |> List.collect (fun (_, _, _, contact) ->
            [ Sound SfxParry
              RingAt(contact, "#ffffff")
              SparksAt(contact, { X = 0.0; Y = -1.0 }, 14, "#ffd0d8")
              ImpactFrameAt contact ])

    let results =
        players
        |> List.map (fun p ->
            let landedHit = hits |> List.exists (fun (attacker, _, _, _) -> attacker.Id = p.Id)
            let marked = if landedHit then { p with HasHit = true } else p

            match hits |> List.tryFind (fun (_, targetId, _, _) -> targetId = p.Id) with
            | Some(attacker, _, hb, contact) ->
                let dx = p.Pos.X - attacker.Pos.X
                let dir = if abs dx > 6.0 then (if dx > 0.0 then 1.0 else -1.0) else attacker.Facing
                let move = currentMove attacker
                // Stale-Move-Negation: oft getroffene Angriffe werden schwächer
                let stale = match move with Some m -> staleFactor m attacker.Stale | None -> 1.0
                let charge = 1.0 + float (min maxCharge attacker.Charge) / 150.0
                let k = stale * charge * (if attacker.PowerTimer > 0 then 1.25 else 1.0)
                // Rage: wer selbst viel Schaden hat, schlägt härter zurück (bis +12 % Knockback)
                let rage = 1.0 + 0.12 * max 0.0 (min 1.0 ((attacker.Percent - 35.0) / 115.0))

                applyHit
                    { AttackerId = attacker.Id
                      AttackerFighter = attacker.Fighter
                      Dir = dir
                      Hit =
                        { hb with
                            Damage = halfStep (hb.Damage * k)
                            BaseKnockback = hb.BaseKnockback * k * rage
                            KnockbackScaling = hb.KnockbackScaling * k * rage }
                      Contact = contact
                      Move = move
                      Melee = true
                      IgnoreShield = false }
                    marked
            | None -> marked, [])

    let counterIds = counters |> List.map (fun (p: Player, _, _, _) -> p.Id)

    let finalPlayers =
        results
        |> List.map fst
        |> List.map (fun p ->
            if List.contains p.Id counterIds then
                // Nach dem Konter: kurz unverwundbar, Haltung endet in der Erholung
                { p with
                    Invuln = max p.Invuln 20
                    HasHit = true
                    State = AttackRecovery(match p.State with AttackActive m -> m | _ -> DownSmash)
                    StateFrame = 0 }
            else
                p)

    finalPlayers, List.collect snd results @ counterFx

// ---------------------------------------------------------------------------
// Griffe und Würfe: Schild + Angriff. Richtung wählt den Wurf, Hämmern befreit.
// ---------------------------------------------------------------------------

let private grabBox (p: Player) : Rect =
    let s = fighterStats p.Fighter
    let w = 30.0 + 22.0 * s.Reach
    let cx = p.Pos.X + p.Facing * (s.Width / 2.0 + w / 2.0 - 6.0)

    { Left = cx - w / 2.0
      Top = p.Pos.Y - s.Height * 0.9
      Width = w
      Height = s.Height * 0.8 }

let private heldPosition (holder: Player) (target: Player) =
    let gap = (fighterStats holder.Fighter).Width / 2.0 + (fighterStats target.Fighter).Width / 2.0 + 2.0
    { X = holder.Pos.X + holder.Facing * gap; Y = holder.Pos.Y }

/// Würfe: vorwärts, rückwärts, hoch (KO-Wurf), runter (Combo-Starter)
let private throwHitbox (f: Fighter) (kind: int) : HitboxDef =
    let k = (tuning f).Damage

    let dmg, bkb, kbs, angle =
        match kind with
        | 0 -> 8.0, 46.0, 0.52, 38.0
        | 1 -> 9.0, 50.0, 0.6, 42.0
        | 2 -> 7.0, 56.0, 0.56, 88.0
        | _ -> 5.0, 42.0, 0.26, 74.0

    hitbox 0.0 0.0 0.0 0.0 (halfStep (dmg * k)) (bkb * (0.85 + k * 0.15)) (kbs * (0.85 + k * 0.15)) angle

let private canBeGrabbed (t: Player) =
    hittable t
    && (match t.State with
        | Held
        | Holding
        | Dodging
        | LedgeHang -> false
        | _ -> true)

let private replace (p: Player) (players: Player list) =
    players |> List.map (fun q -> if q.Id = p.Id then p else q)

/// Spezialgriff (Gorm): Ein aktiver Felsgriff packt auch Schildende und wirft sofort
let private resolveCommandGrabs (players: Player list) : Player list * Effect list =
    players
    |> List.fold
        (fun (ps: Player list, fx) (p: Player) ->
            let me = ps |> List.find (fun q -> q.Id = p.Id)

            match me.State with
            | AttackActive m when not me.HasHit && (moveData me.Fighter m).CommandGrab ->
                let reach =
                    activeHitboxes me |> List.map fst

                match ps |> List.tryFind (fun t -> t.Id <> me.Id && hittable t && t.State <> Held && reach |> List.exists (fun r -> Rect.overlaps r (hurtbox t))) with
                | Some t ->
                    let hb = hitbox 0.0 0.0 0.0 0.0 (halfStep (13.0 * (tuning me.Fighter).Damage / 1.3)) 54.0 0.74 48.0
                    let contact = bodyCenter t

                    let thrown, hitFx =
                        applyHit
                            { AttackerId = me.Id
                              AttackerFighter = me.Fighter
                              Dir = me.Facing
                              Hit = hb
                              Contact = contact
                              Move = Some m
                              Melee = true
                              IgnoreShield = true }
                            { t with State = Idle; StateFrame = 0 }

                    ps |> replace { me with HasHit = true; Vel = Vec2.zero } |> replace thrown,
                    fx @ hitFx @ [ Sound SfxGrab; Sound SfxThrow; ShakeBy({ X = me.Facing; Y = 0.3 }, 10.0); DustAt(t.Pos, 8) ]
                | None -> ps, fx
            | _ -> ps, fx)
        (players, [])

let private resolveGrabs (players: Player list) : Player list * Effect list =
    // 1) Neue Griffe
    let caught, catchFx =
        players
        |> List.fold
            (fun (ps: Player list, fx) (p: Player) ->
                let me = ps |> List.find (fun q -> q.Id = p.Id)
                let active =
                    me.State = Grabbing && me.StateFrame >= grabActiveFrom && me.StateFrame <= grabActiveTo

                if not active then
                    ps, fx
                else
                    match ps |> List.tryFind (fun t -> t.Id <> me.Id && canBeGrabbed t && Rect.overlaps (grabBox me) (hurtbox t)) with
                    | Some t ->
                        let holder =
                            { me with
                                State = Holding
                                StateFrame = 0
                                Timer = 75 + int (t.Percent * 0.4)
                                Vel = Vec2.zero }

                        let target =
                            { t with
                                State = Held
                                StateFrame = 0
                                Timer = 0
                                Vel = Vec2.zero
                                FastFall = false
                                Facing = -me.Facing
                                Pos = heldPosition holder t }

                        ps |> replace holder |> replace target,
                        fx @ [ Sound SfxGrab; RingAt(bodyCenter target, "#ffffff") ]
                    | None -> ps, fx)
            (players, [])

    // 2) Festhalten, Würfe und Befreien
    caught
    |> List.fold
        (fun (ps: Player list, fx) (p: Player) ->
            let h = ps |> List.find (fun q -> q.Id = p.Id)

            if h.State <> Holding then
                ps, fx
            else
                match ps |> List.tryFind (fun t -> t.Id <> h.Id && t.State = Held) with
                | None -> replace { h with State = Idle; StateFrame = 0 } ps, fx
                | Some t ->
                    let remaining = h.Timer - 1 - t.Timer * 4
                    let i = h.Prev
                    let toward = (i.Right && h.Facing > 0.0) || (i.Left && h.Facing < 0.0)
                    let away = (i.Right && h.Facing < 0.0) || (i.Left && h.Facing > 0.0)

                    let throwKind =
                        if h.StateFrame < 6 then None
                        elif i.Up then Some 2
                        elif i.Down then Some 3
                        elif away then Some 1
                        elif toward || i.Attack then Some 0
                        else None

                    match throwKind with
                    | Some kind ->
                        let dir = if kind = 1 then -h.Facing else h.Facing
                        let thrower = { h with State = Landing; StateFrame = 0; Timer = 14; Facing = (if kind = 1 then -h.Facing else h.Facing) }
                        let contact = bodyCenter t

                        let thrown, hitFx =
                            applyHit
                                { AttackerId = h.Id
                                  AttackerFighter = h.Fighter
                                  Dir = dir
                                  Hit = throwHitbox h.Fighter kind
                                  Contact = contact
                                  Move = None
                                  Melee = true
                                  IgnoreShield = true }
                                { t with State = Idle; StateFrame = 0 }

                        ps |> replace thrower |> replace thrown,
                        fx @ hitFx @ [ Sound SfxThrow; SpeedLinesAt(contact, { X = dir; Y = (if kind = 2 then -1.0 else -0.3) }) ]
                    | None when remaining <= 0 ->
                        // Befreit: beide werden auseinandergeschoben
                        let freedHolder = { h with State = Landing; StateFrame = 0; Timer = 10; Vel = { X = -h.Facing * 3.0; Y = 0.0 } }

                        let freed =
                            { t with
                                State = Idle
                                StateFrame = 0
                                Timer = 0
                                Invuln = 12
                                Vel = { X = h.Facing * 5.0; Y = 0.0 } }

                        ps |> replace freedHolder |> replace freed, fx @ [ Sound SfxDodge; DustAt(t.Pos, 4) ]
                    | None ->
                        let holding = { h with Timer = remaining; Vel = Vec2.zero }
                        let target = { t with Timer = 0; Vel = Vec2.zero; Pos = heldPosition holding t }
                        ps |> replace holding |> replace target, fx)
        (caught, catchFx)

/// Phantom-Hieb: Specter erscheint hinter dem nächsten Gegner (sonst ein Stück voraus)
let private resolvePhantom (effects: Effect list) (players: Player list) : Player list * Effect list =
    let stepping =
        effects
        |> List.choose (fun fx ->
            match fx with
            | PhantomStep id -> Some id
            | _ -> None)

    if stepping.IsEmpty then
        players, []
    else
        let moved =
            players
            |> List.map (fun p ->
                if List.contains p.Id stepping then
                    let foe =
                        players
                        |> List.filter (fun o -> o.Id <> p.Id && o.State <> KO && o.State <> Respawning)
                        |> List.sortBy (fun o -> abs (o.Pos.X - p.Pos.X) + abs (o.Pos.Y - p.Pos.Y))
                        |> List.tryHead
                        |> Option.filter (fun o -> abs (o.Pos.X - p.Pos.X) < 260.0 && abs (o.Pos.Y - p.Pos.Y) < 160.0)

                    match foe with
                    | Some o ->
                        let side = if o.Pos.X >= p.Pos.X then 1.0 else -1.0

                        { p with
                            Pos = { X = o.Pos.X + side * 44.0; Y = o.Pos.Y }
                            Facing = -side
                            OnGround = o.OnGround
                            Vel = Vec2.zero },
                        [ ShadowsAt(bodyCenter p, 8); ShadowsAt({ X = o.Pos.X + side * 44.0; Y = o.Pos.Y - 30.0 }, 8) ]
                    | None ->
                        { p with Pos = { p.Pos with X = p.Pos.X + p.Facing * 120.0 }; OnGround = false },
                        [ ShadowsAt(bodyCenter p, 6) ]
                else
                    p, [])

        List.map fst moved, List.collect snd moved

// ---------------------------------------------------------------------------
// Projektil-System
// ---------------------------------------------------------------------------

let private projectileGravity (kind: ProjectileKind) =
    match kind with
    | Kunai -> 0.05
    | Fireball -> 0.32
    | Boulder -> 0.36
    | LightSpear -> 0.06
    | Thorn -> 0.24
    | _ -> 0.0

/// Bewegt ein Projektil; Feuerbälle springen auf Plattformen, Felsbrocken zerbrechen
let private stepProjectile (l: Layout) (pr: Projectile) : Projectile * Effect list =
    let vel = { pr.Vel with Y = pr.Vel.Y + projectileGravity pr.Kind }
    let pos = Vec2.add pr.Pos vel
    let radius = pr.Hit.Height / 2.0

    let landed =
        allPlatforms l
        |> List.tryFind (fun pl -> pos.X >= pl.X1 && pos.X <= pl.X2 && vel.Y > 0.0 && pr.Pos.Y + radius <= pl.Y + 1.0 && pos.Y + radius >= pl.Y)

    match pr.Kind, landed with
    | Fireball, Some pl ->
        { pr with Pos = { pos with Y = pl.Y - radius }; Vel = { vel with Y = -6.0 }; Life = pr.Life - 1 },
        [ FlamesAt({ pos with Y = pl.Y }, { X = 0.0; Y = -1.0 }, 2) ]
    | (Boulder | Thorn | LightSpear), Some pl ->
        { pr with Life = 0 },
        [ DustAt({ pos with Y = pl.Y }, (if pr.Kind = Boulder then 8 else 2)) ]
        @ (if pr.Kind = Boulder then [ Sound SfxLand; ShakeBy({ X = 0.0; Y = 1.0 }, 4.0) ] else [])
    | _ -> { pr with Pos = pos; Vel = vel; Life = pr.Life - 1 }, []

let private projectileTrail (frame: int) (pr: Projectile) : Effect list =
    let back = Vec2.scale -1.0 (Vec2.normalize pr.Vel)

    match pr.Kind with
    | EnergyOrb when frame % 3 = 0 -> [ SparksAt(pr.Pos, back, 1, "#d7c2ff") ]
    | BladeBeam when frame % 2 = 0 -> [ SparksAt(pr.Pos, back, 1, "#bff3ff") ]
    | Shockwave when frame % 2 = 0 -> [ DustAt({ pr.Pos with Y = pr.Pos.Y + 12.0 }, 1) ]
    | Fireball when frame % 2 = 0 -> [ FlamesAt(pr.Pos, back, 1) ]
    | StasisOrb when frame % 3 = 0 -> [ SparksAt(pr.Pos, back, 1, "#bff8ee") ]
    | ShadowDagger when frame % 2 = 0 -> [ ShadowsAt(pr.Pos, 1) ]
    | WindSlash when frame % 3 = 0 -> [ SparksAt(pr.Pos, { X = -pr.Vel.X; Y = 0.0 }, 1, "#ffd0d8") ]
    | Arrow when frame % 3 = 0 -> [ SparksAt(pr.Pos, { X = -pr.Vel.X; Y = -pr.Vel.Y }, 1, "#e8ecff") ]
    | LightSpear when frame % 2 = 0 -> [ SparksAt(pr.Pos, back, 1, "#fff3b0") ]
    | _ -> []

/// Reflektor: Valkyries Aegis Dash wirft gegnerische Projektile zurück
let private reflector (players: Player list) (pr: Projectile) : Player option =
    players
    |> List.tryFind (fun p ->
        p.Id <> pr.Owner
        && (match p.State with
            | AttackActive m -> (moveData p.Fighter m).Reflect
            | _ -> false)
        && (let h = hurtbox p
            Rect.overlaps
                { h with
                    Left = h.Left - 18.0
                    Width = h.Width + 36.0 }
                (projectileRect pr)))

/// Bewegt Projektile, löst Reflexion, Kollisionen untereinander und mit Spielern auf
let private resolveProjectiles (l: Layout) (frame: int) (players: Player list) (projectiles: Projectile list) : Player list * Projectile list * Effect list =
    let stepped = projectiles |> List.map (stepProjectile l)
    let stepFx = stepped |> List.collect snd

    let moved =
        stepped
        |> List.map fst
        |> List.filter (fun pr ->
            pr.Life > 0
            && pr.Pos.X > blastLeft
            && pr.Pos.X < blastRight
            && pr.Pos.Y > blastTop
            && pr.Pos.Y < blastBottom)

    // Reflexion
    let reflectedFx, afterReflect =
        moved
        |> List.map (fun pr ->
            match reflector players pr with
            | Some r ->
                [ Sound SfxReflect; RingAt(pr.Pos, "#ffe08a"); SparksAt(pr.Pos, { X = r.Facing; Y = -0.3 }, 6, "#fff3b0") ],
                { pr with
                    Owner = r.Id
                    Vel = { X = abs pr.Vel.X * 1.15 * r.Facing; Y = pr.Vel.Y * 0.5 }
                    Life = pr.MaxLife
                    Reflected = true
                    Hit = { pr.Hit with Damage = halfStep (pr.Hit.Damage * 1.25); BaseKnockback = pr.Hit.BaseKnockback * 1.2 } }
            | None -> [], pr)
        |> List.fold (fun (fx, acc) (f, pr) -> fx @ f, acc @ [ pr ]) ([], [])

    // Projektile verschiedener Besitzer neutralisieren sich gegenseitig
    let clashing =
        afterReflect
        |> List.filter (fun a ->
            afterReflect
            |> List.exists (fun b -> a.Owner <> b.Owner && Rect.overlaps (projectileRect a) (projectileRect b)))

    let clashFx = clashing |> List.collect (fun pr -> [ SparksAt(pr.Pos, { X = 0.0; Y = -1.0 }, 5, "#ffffff"); BurstAt(pr.Pos, 14.0) ])
    let survivors = afterReflect |> List.filter (fun pr -> not (List.contains pr clashing))

    // Treffer gegen Spieler (ein Projektil trifft höchstens einmal)
    let folded =
        survivors
        |> List.fold
            (fun (ps: Player list, kept: Projectile list, fx: Effect list) pr ->
                let target =
                    ps
                    |> List.tryFind (fun p -> p.Id <> pr.Owner && hittable p && Rect.overlaps (projectileRect pr) (hurtbox p))

                match target with
                | Some t ->
                    let owner = ps |> List.find (fun p -> p.Id = pr.Owner)
                    let contact = Rect.overlapCenter (projectileRect pr) (hurtbox t)

                    let updated, hitFx =
                        applyHit
                            { AttackerId = pr.Owner
                              AttackerFighter = owner.Fighter
                              Dir = (if pr.Vel.X >= 0.0 then 1.0 else -1.0)
                              Hit = pr.Hit
                              Contact = contact
                              Move = None
                              Melee = false
                              IgnoreShield = false }
                            t

                    // Stasis-Kugel: Ziel läuft eine Weile in Zeitlupe
                    let slowed, slowFx =
                        if pr.Kind = StasisOrb && updated.State <> Shielding then
                            { updated with Slow = stasisFrames }, [ Sound SfxStasis; RingAt(contact, "#4fe0c8"); RingAt(contact, "#bff8ee") ]
                        else
                            updated, []

                    let replaced = ps |> List.map (fun p -> if p.Id = t.Id then slowed else p)
                    replaced, kept, fx @ hitFx @ slowFx
                | None -> ps, pr :: kept, fx @ projectileTrail frame pr)
            (players, [], [])

    let ps, kept, fx = folded
    ps, List.rev kept, stepFx @ reflectedFx @ clashFx @ fx

// ---------------------------------------------------------------------------
// Smash-Ball
// ---------------------------------------------------------------------------

let private spawnBall (seed: int) : int * SmashBall =
    let s1, r1 = nextRandom seed
    let s2, r2 = nextRandom s1
    let fromLeft = r1 < 0.5

    s2,
    { Pos = { X = (if fromLeft then -30.0 else screenWidth + 30.0); Y = 140.0 + r2 * 220.0 }
      Vel = { X = (if fromLeft then 3.2 else -3.2); Y = 0.0 }
      Hp = ballHp
      Life = ballLife
      HitCooldown = 0
      Flash = 0 }

let private stepBall (b: SmashBall) : SmashBall option =
    let leaving = b.Life <= 0
    let inside = b.Life < ballLife - 90
    let wobble = dsin (float b.Life * 0.045) * 1.6
    let vx =
        if leaving then b.Vel.X * 1.03
        elif inside && ((b.Pos.X < 90.0 && b.Vel.X < 0.0) || (b.Pos.X > screenWidth - 90.0 && b.Vel.X > 0.0)) then -b.Vel.X
        else b.Vel.X * 0.995 + (if abs b.Vel.X < 2.0 then (if b.Vel.X >= 0.0 then 0.04 else -0.04) else 0.0)

    let pos = { X = b.Pos.X + vx; Y = max 90.0 (min 420.0 (b.Pos.Y + wobble + b.Vel.Y)) }
    let off = pos.X < -80.0 || pos.X > screenWidth + 80.0

    if leaving && off then
        None
    else
        Some
            { b with
                Pos = pos
                Vel = { X = vx; Y = b.Vel.Y * 0.9 }
                Life = b.Life - 1
                HitCooldown = max 0 (b.HitCooldown - 1)
                Flash = max 0 (b.Flash - 1) }

/// Treffer am Smash-Ball durch Nahkampf und Projektile; wer ihn zerschlägt, bekommt den Ultra-Smash
let private resolveBall (players: Player list) (projectiles: Projectile list) (ball: SmashBall) : SmashBall option * Player list * Projectile list * Effect list =
    let rect = ballRect ball

    let melee =
        if ball.HitCooldown > 0 then
            None
        else
            players
            |> List.tryPick (fun p ->
                activeHitboxes p
                |> List.tryFind (fun (r, _) -> Rect.overlaps r rect)
                |> Option.map (fun (_, hb) -> p.Id, hb.Damage, p.Facing))

    let shot = projectiles |> List.tryFind (fun pr -> Rect.overlaps (projectileRect pr) rect)

    let hit =
        match melee, shot with
        | Some m, _ -> Some m
        | None, Some pr -> Some(pr.Owner, pr.Hit.Damage, (if pr.Vel.X >= 0.0 then 1.0 else -1.0))
        | None, None -> None

    let projectilesLeft =
        match melee, shot with
        | None, Some pr -> projectiles |> List.filter (fun x -> x <> pr)
        | _ -> projectiles

    match hit with
    | None -> Some ball, players, projectilesLeft, []
    | Some(id, damage, dir) ->
        let hp = ball.Hp - max 4.0 damage

        if hp <= 0.0 then
            let updated = players |> List.map (fun p -> if p.Id = id then { p with HasFinal = true } else p)

            None,
            updated,
            projectilesLeft,
            [ Sound SfxBallBreak
              RainbowAt(ball.Pos, 40)
              RingAt(ball.Pos, "#ffffff")
              BurstAt(ball.Pos, 40.0)
              ShakeBy({ X = dir; Y = 0.0 }, 10.0)
              HitStopFor 8 ]
        else
            Some
                { ball with
                    Hp = hp
                    HitCooldown = 10
                    Flash = 8
                    Vel = { X = dir * 4.5; Y = -2.0 } },
            players,
            projectilesLeft,
            [ Sound SfxBallHit; SparksAt(ball.Pos, { X = dir; Y = -0.4 }, 8, "#ffffff"); RainbowAt(ball.Pos, 6) ]

// ---------------------------------------------------------------------------
// Ultra-Smash
// ---------------------------------------------------------------------------

let ultraAngle (f: Fighter) =
    match f with
    | Aether -> 35.0
    | Ignis -> 40.0
    | Titan -> 60.0
    | Zephyr -> 50.0
    | Valkyrie -> 82.0
    | Chrono -> 45.0
    | Bramble -> 75.0
    | Specter -> 40.0
    | Ronin -> 32.0
    | Lyra -> 70.0
    | Gorm -> 55.0

/// Wer vom Ultra-Smash erfasst wird (zum Zeitpunkt des Einschlags)
let private ultraHits (u: Ultra) (target: Player) : bool =
    let c = bodyCenter target
    let dx = c.X - u.Origin.X
    let dy = c.Y - u.Origin.Y
    let onScreen = c.X > -60.0 && c.X < screenWidth + 60.0 && c.Y > -60.0 && c.Y < screenHeight + 60.0

    match u.Fighter with
    | Aether -> abs dy < 110.0 && dx * u.Facing > -50.0
    | Ignis -> abs dy < 95.0 && dx * u.Facing > -50.0
    | Valkyrie -> abs (c.X - u.Target.X) < 120.0
    | Specter -> sqrt (dx * dx + dy * dy) < 700.0
    | Ronin -> abs dy < 140.0
    | Lyra
    | Gorm -> onScreen
    | Chrono -> sqrt (dx * dx + dy * dy) < 540.0
    | Titan
    | Zephyr
    | Bramble -> onScreen

let private startUltra (players: Player list) (id: int) : Ultra option =
    players
    |> List.tryFind (fun p -> p.Id = id)
    |> Option.map (fun p ->
        let foe =
            players
            |> List.filter (fun o -> o.Id <> id)
            |> List.tryHead
            |> Option.map bodyCenter
            |> Option.defaultValue { X = screenWidth / 2.0; Y = 300.0 }

        { Owner = id
          Fighter = p.Fighter
          Timer = 0
          Origin = bodyCenter p
          Facing = (if foe.X >= p.Pos.X then 1.0 else -1.0)
          Target = foe
          Victims = [] })

/// Ein Frame des Ultra-Smash: Die Arena steht still, bis der Angriff einschlägt
let private stepUltra (u: Ultra) (g: Game) : Game * Effect list =
    let t = u.Timer + 1

    if t = ultraHitFrame then
        let ultraHitbox = hitbox 0.0 0.0 0.0 0.0 32.0 72.0 1.0 (ultraAngle u.Fighter)

        let results =
            g.Players
            |> List.map (fun p ->
                if p.Id <> u.Owner && hittable p && ultraHits u p then
                    let c = bodyCenter p
                    let dir = if c.X >= u.Origin.X then 1.0 else -1.0
                    let banded = u.Fighter = Aether || u.Fighter = Ignis

                    let hit, fx =
                        applyHit
                            { AttackerId = u.Owner
                              AttackerFighter = u.Fighter
                              Dir = (if banded then u.Facing else dir)
                              Hit = ultraHitbox
                              Contact = c
                              Move = None
                              Melee = false
                              IgnoreShield = true }
                            { p with State = (if p.State = Shielding then Idle else p.State) }

                    let slowed = if u.Fighter = Chrono then { hit with Slow = stasisFrames * 2 } else hit
                    Some p.Id, slowed, fx @ [ ImpactFrameAt c; ExplosionAt(c, { X = dir; Y = -0.5 }, (fighterStats u.Fighter).Color) ]
                else
                    None, p, [])

        let victims = results |> List.choose (fun (v, _, _) -> v)
        let players = results |> List.map (fun (_, p, _) -> p)
        let effects = results |> List.collect (fun (_, _, fx) -> fx)

        { g with
            Players = players
            Ultra = Some { u with Timer = t; Victims = victims } },
        effects @ [ Sound SfxUltraBlast; ShakeBy({ X = u.Facing; Y = 0.3 }, 34.0) ]
    elif t >= ultraEnd then
        { g with Ultra = None }, []
    elif t = ultraCutIn then
        { g with Ultra = Some { u with Timer = t } }, [ Sound SfxUltraBlast; ShakeBy({ X = u.Facing; Y = 0.0 }, 14.0) ]
    else
        { g with Ultra = Some { u with Timer = t } }, []

// ---------------------------------------------------------------------------
// Blast-Zones & Stocks
// ---------------------------------------------------------------------------

let private checkBlastZone (p: Player) : Player * Effect list =
    let height = (stats p).Height

    let outside =
        p.Pos.X < blastLeft
        || p.Pos.X > blastRight
        || p.Pos.Y - height < blastTop
        || p.Pos.Y - height > blastBottom

    if not outside || p.State = KO || p.State = Respawning then
        p, []
    else
        let at =
            { X = clamp 30.0 (screenWidth - 30.0) p.Pos.X
              Y = clamp 30.0 (screenHeight - 30.0) (p.Pos.Y - height / 2.0) }

        let inward = Vec2.normalize { X = screenWidth / 2.0 - at.X; Y = screenHeight / 2.0 - at.Y }

        { p with
            State = KO
            StateFrame = 0
            Timer = 75
            Stocks = p.Stocks - 1
            Falls = p.Falls + 1
            Percent = 0.0
            Combo = 0
            HasFinal = false
            Vel = Vec2.zero
            OnGround = false
            HudShake = 2.0 },
        [ Sound SfxKO
          ExplosionAt(at, inward, playerColor p)
          BeamAt(at, inward, playerColor p)
          ShakeBy(inward, 30.0)
          HitStopFor 8 ]
        @ (match p.LastHitBy with
           | Some killer when killer <> p.Id -> [ KoCredit killer ]
           | _ -> [])

// ---------------------------------------------------------------------------
// Partikel-System
// ---------------------------------------------------------------------------

let private particle kind pos vel life size color : Particle =
    { Kind = kind
      Pos = pos
      Vel = vel
      Life = life
      MaxLife = life
      Size = size
      Color = color }

/// Erzeugt n Partikel und reicht den Zufalls-Seed durch
let private emit (n: int) (seed: int) (make: float -> float -> float -> Particle) : int * Particle list =
    let rec loop i s acc =
        if i >= n then
            s, acc
        else
            let s1, a = nextRandom s
            let s2, b = nextRandom s1
            let s3, c = nextRandom s2
            loop (i + 1) s3 (make a b c :: acc)

    loop 0 seed []

let private rainbowColors = [| "#ff4d5e"; "#ffb13d"; "#ffe27a"; "#7dffb0"; "#58c7ff"; "#b48cff" |]

let private spawnParticles (seed: int) (effect: Effect) : int * Particle list =
    match effect with
    | DustAt(pos, count) ->
        emit count seed (fun a b c ->
            let vel = { X = (a - 0.5) * 5.0; Y = -b * 1.4 }
            particle Dust { pos with Y = pos.Y - 3.0 } vel (16 + int (c * 12.0)) (5.0 + c * 5.0) "#f2ece0")
    | SparksAt(pos, dir, count, color) ->
        emit count seed (fun a b c ->
            let angle = datan2 dir.Y dir.X + (a - 0.5) * 2.2
            let speed = 4.0 + b * 11.0
            let vel = { X = dcos angle * speed; Y = dsin angle * speed }
            particle Spark pos vel (10 + int (c * 14.0)) (2.0 + c * 2.5) color)
    | SmokeAt(pos, color) ->
        emit 1 seed (fun a b c ->
            let vel = { X = (a - 0.5) * 1.2; Y = (b - 0.5) * 1.2 }
            particle Smoke pos vel (22 + int (c * 12.0)) (11.0 + c * 9.0) color)
    | RingAt(pos, color) -> seed, [ particle Ring pos Vec2.zero 16 8.0 color ]
    | ExplosionAt(pos, dir, color) ->
        let s1, embers =
            emit 46 seed (fun a b c ->
                let angle = datan2 dir.Y dir.X + (a - 0.5) * 1.5
                let speed = 5.0 + b * 26.0
                let vel = { X = dcos angle * speed; Y = dsin angle * speed }
                let tint = if c < 0.4 then "#ffffff" elif c < 0.7 then "#ffd75e" else color
                particle Ember pos vel (28 + int (c * 34.0)) (4.0 + b * 8.0) tint)

        let s2, smoke =
            emit 14 s1 (fun a b c ->
                let vel = { X = dir.X * (2.0 + a * 6.0) + (b - 0.5) * 4.0; Y = dir.Y * (2.0 + a * 6.0) + (c - 0.5) * 4.0 }
                particle Smoke pos vel (40 + int (c * 20.0)) (18.0 + b * 18.0) "#cfd3dc")

        s2,
        embers
        @ smoke
        @ [ particle Ring pos Vec2.zero 26 14.0 "#ffffff"; particle Ring pos Vec2.zero 20 6.0 color ]
    | BoltsAt(pos, count, color) ->
        emit count seed (fun a b c ->
            let angle = a * Math.PI * 2.0
            let length = 34.0 + b * 60.0
            // Blitze bewegen sich nicht: Vel beschreibt Richtung und Länge
            particle Bolt pos { X = dcos angle * length; Y = dsin angle * length } (6 + int (c * 5.0)) (2.0 + c * 2.0) color)
    | FlamesAt(pos, dir, count) ->
        emit count seed (fun a b c ->
            let angle = datan2 dir.Y dir.X + (a - 0.5) * 2.6
            let speed = 1.5 + b * 7.0
            let vel = { X = dcos angle * speed; Y = dsin angle * speed - 1.0 }
            particle Flame pos vel (14 + int (c * 16.0)) (7.0 + b * 9.0) (if c < 0.5 then "#ff7a2e" else "#ffb347"))
    | LeavesAt(pos, count) ->
        emit count seed (fun a b c ->
            let vel = { X = (a - 0.5) * 9.0; Y = -2.0 - b * 5.0 }
            particle Leaf pos vel (30 + int (c * 24.0)) (4.0 + b * 3.0) (if c < 0.5 then "#7ed957" else "#c2f06a"))
    | ShadowsAt(pos, count) ->
        emit count seed (fun a b c ->
            let vel = { X = (a - 0.5) * 4.0; Y = -0.5 - b * 1.5 }
            particle Shadow pos vel (20 + int (c * 18.0)) (8.0 + b * 8.0) (if c < 0.6 then "#2a1638" else "#ff4f7b"))
    | RainbowAt(pos, count) ->
        emit count seed (fun a b c ->
            let angle = a * Math.PI * 2.0
            let speed = 2.0 + b * 12.0
            particle Rainbow pos { X = dcos angle * speed; Y = dsin angle * speed } (24 + int (c * 24.0)) (3.0 + b * 4.0) rainbowColors[int (c * 5.99)])
    | BurstAt(pos, size) -> seed, [ particle Burst pos Vec2.zero 7 size "#ffffff" ]
    | SpeedLinesAt(pos, dir) ->
        emit 2 seed (fun a b c ->
            let side = (a - 0.5) * 90.0
            let length = 70.0 + b * 110.0
            let at = { X = pos.X - dir.Y * side - dir.X * c * 40.0; Y = pos.Y + dir.X * side - dir.Y * c * 40.0 }
            particle SpeedLine at (Vec2.scale -length dir) 8 (1.5 + c * 2.0) "#ffffff")
    | ShockwaveAt(_, pos) ->
        emit 10 seed (fun a b c ->
            let vel = { X = (a - 0.5) * 12.0; Y = -b * 2.5 }
            particle Dust { pos with Y = pos.Y - 4.0 } vel (20 + int (c * 14.0)) (7.0 + c * 7.0) "#e8dcc4")
    | Sound _
    | HitStopFor _
    | ShakeBy _
    | ImpactFrameAt _
    | BeamAt _
    | SpawnProjectile _
    | DamageCredit _
    | ComboCredit _
    | KoCredit _
    | EnergyGain _
    | StaleHit _
    | PhantomStep _
    | Stun _
    | StartUltra _ -> seed, []

let private updateParticle (pt: Particle) : Particle =
    match pt.Kind with
    | Bolt
    | SpeedLine -> { pt with Life = pt.Life - 1 }
    | Burst -> { pt with Life = pt.Life - 1; Size = pt.Size + 2.5 }
    | _ ->
        let drag, grav, grow =
            match pt.Kind with
            | Spark -> 0.9, 0.28, 0.0
            | Smoke -> 0.93, -0.04, 0.4
            | Dust -> 0.9, -0.03, 0.3
            | Ember -> 0.94, 0.14, -0.05
            | Flame -> 0.9, -0.22, -0.3
            | Leaf -> 0.95, 0.12, 0.0
            | Shadow -> 0.92, -0.05, 0.25
            | Rainbow -> 0.93, 0.05, -0.04
            | _ -> 1.0, 0.0, 4.2

        { pt with
            Pos = Vec2.add pt.Pos pt.Vel
            Vel = { X = pt.Vel.X * drag; Y = pt.Vel.Y * drag + grav }
            Size = max 0.5 (pt.Size + grow)
            Life = pt.Life - 1 }

let private updateParticles (particles: Particle list) : Particle list =
    particles |> List.map updateParticle |> List.filter (fun pt -> pt.Life > 0)

/// Schockwellen laufen vom Landepunkt in beide Richtungen über den Boden
let private shockwaves (owner: int) (pos: Vec2) : Projectile list =
    [ -1.0; 1.0 ]
    |> List.map (fun dir -> makeProjectile owner DownSpecial 1.0 { X = pos.X + dir * 24.0; Y = pos.Y - 14.0 } { X = dir * 9.0; Y = 0.0 } Shockwave)

/// Wendet alle Effekte eines Schritts auf den Game-State an
let private applyEffects (effects: Effect list) (g: Game) : Game =
    let seed, spawned =
        effects
        |> List.fold
            (fun (s, acc) fx ->
                let s2, created = spawnParticles s fx
                s2, created @ acc)
            (g.Seed, [])

    let sounds =
        effects
        |> List.choose (fun fx ->
            match fx with
            | Sound s -> Some s
            | ShockwaveAt _ -> Some SfxShockwave
            | StartUltra _ -> Some SfxUltraCutIn
            | _ -> None)
        |> List.distinct

    let hitStop =
        effects
        |> List.fold
            (fun acc fx ->
                match fx with
                | HitStopFor n -> max acc n
                | _ -> acc)
            g.HitStop

    let shakeDir, shakeMag =
        effects
        |> List.fold
            (fun (dir, mag) fx ->
                match fx with
                | ShakeBy(d, m) when m > mag -> d, m
                | ShockwaveAt _ when 9.0 > mag -> { X = 0.0; Y = 1.0 }, 9.0
                | _ -> dir, mag)
            (g.ShakeDir, g.ShakeMag)

    let impact =
        effects
        |> List.tryPick (fun fx ->
            match fx with
            | ImpactFrameAt pos -> Some pos
            | _ -> None)

    let beams =
        effects
        |> List.choose (fun fx ->
            match fx with
            | BeamAt(origin, dir, color) ->
                Some
                    { Origin = origin
                      Dir = dir
                      Life = koBeamFrames
                      MaxLife = koBeamFrames
                      Color = color }
            | _ -> None)

    let newProjectiles =
        effects
        |> List.collect (fun fx ->
            match fx with
            | SpawnProjectile pr -> [ pr ]
            | ShockwaveAt(owner, pos) -> shockwaves owner pos
            | _ -> [])

    // Match-Statistik, Energie, Stale-Moves und Combo-Anzeige
    let players =
        g.Players
        |> List.map (fun p ->
            effects
            |> List.fold
                (fun (acc: Player) fx ->
                    match fx with
                    | DamageCredit(id, dmg) when id = acc.Id -> { acc with DamageDealt = acc.DamageDealt + dmg }
                    | ComboCredit(id, n) when id = acc.Id ->
                        { acc with
                            ComboShown = n
                            ComboTimer = comboDisplayFrames
                            MaxCombo = max acc.MaxCombo n }
                    | KoCredit id when id = acc.Id -> { acc with KOs = acc.KOs + 1 }
                    | EnergyGain(id, amount) when id = acc.Id -> { acc with Energy = min energyMax (acc.Energy + amount) }
                    | StaleHit(id, m) when id = acc.Id -> { acc with Stale = m :: List.truncate 8 acc.Stale }
                    | Stun(id, n) when id = acc.Id && acc.State <> KO && acc.State <> Respawning ->
                        { acc with
                            State = Hitstun
                            StateFrame = 0
                            Timer = n
                            HasHit = true
                            Vel = { X = 0.0; Y = min 0.0 acc.Vel.Y }
                            HitFlash = 6 }
                    | _ -> acc)
                p)

    let ultra =
        match g.Ultra with
        | Some u -> Some u
        | None ->
            effects
            |> List.tryPick (fun fx ->
                match fx with
                | StartUltra id -> Some id
                | _ -> None)
            |> Option.bind (startUltra players)

    { g with
        Seed = seed
        Players = players
        Ultra = ultra
        Projectiles = g.Projectiles @ newProjectiles
        ImpactFlash = (if impact.IsSome then impactFrames else g.ImpactFlash)
        ImpactPos = defaultArg impact g.ImpactPos
        Beams = beams @ g.Beams
        Particles = List.truncate maxParticles (spawned @ g.Particles)
        Sounds = g.Sounds @ sounds
        HitStop = hitStop
        ShakeDir = shakeDir
        ShakeMag = shakeMag }

// ---------------------------------------------------------------------------
// Items und Stage-Ereignisse
// ---------------------------------------------------------------------------

let private itemRect (it: Item) : Rect =
    { Left = it.Pos.X - itemSize / 2.0
      Top = it.Pos.Y - itemSize
      Width = itemSize
      Height = itemSize }

let private itemCenter (it: Item) = { X = it.Pos.X; Y = it.Pos.Y - itemSize / 2.0 }

let private pickKind (r: float) =
    if r < 0.24 then Heart
    elif r < 0.48 then PowerGem
    elif r < 0.68 then SpeedFeather
    else Bomb

let private stepItem (l: Layout) (it: Item) : Item option =
    let aged = { it with Age = it.Age + 1; Life = it.Life - 1 }

    if aged.Life <= 0 then
        None
    else
        let platforms = allPlatforms l

        // Auf einer (beweglichen) Plattform: mitfahren
        let carrier =
            if it.OnGround then
                platforms
                |> List.tryFind (fun pl -> it.Pos.X >= pl.X1 && it.Pos.X <= pl.X2 && abs (it.Pos.Y - (pl.Y - pl.Vy)) < 2.5)
            else
                None

        match carrier with
        | Some pl -> Some { aged with Pos = { X = it.Pos.X + pl.Vx; Y = pl.Y } }
        | None ->
            let vy = min 9.0 (it.Vel.Y + 0.35)
            let ny = it.Pos.Y + vy

            let landing =
                platforms
                |> List.filter (fun pl -> it.Pos.X >= pl.X1 && it.Pos.X <= pl.X2 && it.Pos.Y <= pl.Y + 0.5 && ny >= pl.Y)
                |> List.sortBy (fun pl -> pl.Y)
                |> List.tryHead

            match landing with
            | Some pl -> Some { aged with Pos = { X = it.Pos.X; Y = pl.Y }; Vel = Vec2.zero; OnGround = true }
            | None when ny > screenHeight + 120.0 -> None
            | None -> Some { aged with Pos = { it.Pos with Y = ny }; Vel = { it.Vel with Y = vy }; OnGround = false }

let private canPickUp (p: Player) =
    match p.State with
    | KO
    | Respawning
    | Held -> false
    | _ -> true

let private applyItem (it: Item) (p: Player) : Player * Effect list =
    let c = bodyCenter p

    match it.Kind with
    | Heart ->
        { p with Percent = max 0.0 (p.Percent - 30.0) },
        [ Sound SfxHeal; SparksAt(c, { X = 0.0; Y = -1.0 }, 14, "#7dffb0"); RingAt(c, "#7dffb0") ]
    | PowerGem ->
        { p with PowerTimer = powerFrames },
        [ Sound SfxPickup; SparksAt(c, { X = 0.0; Y = -1.0 }, 14, "#ff5a6e"); RingAt(c, "#ff5a6e") ]
    | SpeedFeather ->
        { p with SpeedTimer = speedFrames },
        [ Sound SfxPickup; SparksAt(c, { X = 0.0; Y = -1.0 }, 14, "#7df9ff"); RingAt(c, "#7df9ff") ]
    | Bomb -> p, []

/// Treffer ohne Angreifer (Bombe, Stage-Ereignis); Schilde blocken
let private environmentHit (hb: HitboxDef) (dir: float) (players: Player list) (targets: int list) : Player list * Effect list =
    players
    |> List.fold
        (fun (ps: Player list, fx) (p: Player) ->
            if List.contains p.Id targets && hittable p then
                let hitP, hitFx =
                    applyHit
                        { AttackerId = -1
                          AttackerFighter = p.Fighter
                          Dir = dir
                          Hit = hb
                          Contact = bodyCenter p
                          Move = None
                          Melee = false
                          IgnoreShield = false }
                        p

                replace hitP ps, fx @ hitFx
            else
                ps, fx)
        (players, [])

let private explodeBomb (it: Item) (players: Player list) : Player list * Effect list =
    let c = itemCenter it

    let fx =
        [ Sound SfxExplosion
          ExplosionAt(c, { X = 0.0; Y = -1.0 }, "#ff7a2e")
          BurstAt(c, 70.0)
          RingAt(c, "#ffd166")
          SmokeAt(c, "#5a4a44")
          ShakeBy({ X = 0.0; Y = 1.0 }, 13.0)
          HitStopFor 5 ]

    let hb = hitbox 0.0 0.0 0.0 0.0 14.0 48.0 0.72 60.0

    players
    |> List.filter (fun p ->
        let b = bodyCenter p
        Vec2.length { X = b.X - c.X; Y = b.Y - c.Y } < bombRadius)
    |> List.fold
        (fun (ps: Player list, acc) p ->
            let dx = p.Pos.X - c.X
            let dir = if dx < 0.0 then -1.0 else 1.0
            let ps2, f2 = environmentHit hb dir ps [ p.Id ]
            ps2, acc @ f2)
        (players, fx)

let private updateItems (l: Layout) (g: Game) (players: Player list) (projectiles: Projectile list) =
    // Neue Items fallen vom Himmel
    let seed, timer, spawned, spawnFx =
        if not g.ItemsOn then
            g.Seed, g.ItemTimer, [], []
        elif g.ItemTimer > 0 then
            g.Seed, g.ItemTimer - 1, [], []
        elif List.length g.Items >= itemMax then
            g.Seed, 120, [], []
        else
            let s1, rx = nextRandom g.Seed
            let s2, rk = nextRandom s1
            let s3, rd = nextRandom s2
            let x = l.Left + 50.0 + rx * (l.Right - l.Left - 100.0)

            let it =
                { Kind = pickKind rk
                  Pos = { X = x; Y = -30.0 }
                  Vel = Vec2.zero
                  OnGround = false
                  Age = 0
                  Life = itemLife }

            s3, itemMinDelay + int (rd * float (itemMaxDelay - itemMinDelay)), [ it ], [ Sound SfxItemSpawn ]

    let moved = (g.Items @ spawned) |> List.choose (stepItem l)

    // Einsammeln und Bomben
    let items, ps, fx =
        moved
        |> List.fold
            (fun (keep: Item list, ps: Player list, fx) it ->
                let rect = itemRect it

                let toucher =
                    ps |> List.tryFind (fun p -> canPickUp p && Rect.overlaps rect (hurtbox p))

                match it.Kind with
                | Bomb ->
                    let struck =
                        ps |> List.exists (fun p -> activeHitboxes p |> List.exists (fun (r, _) -> Rect.overlaps r rect))
                        || projectiles |> List.exists (fun pr -> Rect.overlaps (projectileRect pr) rect)

                    let touched = it.Age >= bombArmFrames && toucher.IsSome

                    if struck || touched || it.Life <= 1 then
                        let ps2, f2 = explodeBomb it ps
                        keep, ps2, fx @ f2
                    else
                        keep @ [ it ], ps, fx
                | _ ->
                    match toucher with
                    | Some p ->
                        let p2, f2 = applyItem it p
                        keep, replace p2 ps, fx @ f2
                    | None -> keep @ [ it ], ps, fx)
            ([], players, [])

    { g with Seed = seed; ItemTimer = timer; Items = items }, ps, spawnFx @ fx

/// Rechteck, in dem ein Ereignis in der aktiven Phase trifft
let private hazardZones (l: Layout) (h: Hazard) (t: int) : Rect list =
    match h.Kind with
    | WindGust -> []
    | Meteor ->
        if t < 8 then
            h.Xs |> List.map (fun x -> { Left = x - 75.0; Top = l.Top - 150.0; Width = 150.0; Height = 160.0 })
        else
            []
    | LavaGeyser ->
        h.Xs |> List.map (fun x -> { Left = x - 42.0; Top = l.Top - 320.0; Width = 84.0; Height = screenHeight })
    | Icicles ->
        h.Xs
        |> List.mapi (fun i x ->
            let y = icicleY l t i
            { Left = x - 14.0; Top = y - 44.0; Width = 28.0; Height = 44.0 })
    | LaserSweep ->
        let x = laserX l h t
        [ { Left = x - 16.0; Top = l.Top - 34.0; Width = 32.0; Height = 26.0 } ]

let private hazardHitbox (k: HazardKind) =
    match k with
    | WindGust -> hitbox 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
    | Meteor -> hitbox 0.0 0.0 0.0 0.0 13.0 42.0 0.62 78.0
    | LavaGeyser -> hitbox 0.0 0.0 0.0 0.0 11.0 56.0 0.5 90.0
    | Icicles -> hitbox 0.0 0.0 0.0 0.0 9.0 36.0 0.48 70.0
    | LaserSweep -> hitbox 0.0 0.0 0.0 0.0 10.0 50.0 0.5 72.0

let private startHazard (l: Layout) (g: Game) (players: Player list) : int * Hazard =
    let kind = stageHazard g.Stage
    let s1, r1 = nextRandom g.Seed
    let s2, r2 = nextRandom s1
    let s3, r3 = nextRandom s2
    let alive = players |> List.filter (fun p -> p.State <> KO && p.State <> Respawning)

    // Ziel: ein zufälliger Kämpfer, damit Ereignisse das Geschehen aufmischen
    let aimX r =
        match alive with
        | [] -> stageCenter l
        | _ -> (List.item (int (r * float (List.length alive)) % List.length alive) alive).Pos.X

    let inStage x = clamp (l.Left + 40.0) (l.Right - 40.0) x
    let dir = if r3 < 0.5 then -1.0 else 1.0

    let xs =
        match kind with
        | WindGust
        | LaserSweep -> []
        | Meteor
        | LavaGeyser -> [ inStage (aimX r1 + (r2 - 0.5) * 80.0) ]
        | Icicles ->
            [ inStage (aimX r1)
              inStage (l.Left + r2 * (l.Right - l.Left))
              inStage (l.Left + r3 * (l.Right - l.Left)) ]

    s3,
    { Kind = kind
      Age = 0
      Xs = xs
      Dir = dir
      HitIds = [] }

let private updateHazard (l: Layout) (g: Game) (players: Player list) =
    if not g.HazardsOn then
        g, players, []
    else
        match g.Hazard with
        | None when g.HazardTimer > 0 -> { g with HazardTimer = g.HazardTimer - 1 }, players, []
        | None ->
            let seed, h = startHazard l g players
            let s2, r = nextRandom seed
            let delay = hazardMinDelay + int (r * float (hazardMaxDelay - hazardMinDelay))
            { g with Seed = s2; Hazard = Some h; HazardTimer = delay }, players, [ Sound SfxWarning ]
        | Some h ->
            let warn, active = hazardTiming h.Kind
            let aged = { h with Age = h.Age + 1 }
            let t = aged.Age - warn

            if aged.Age >= warn + active then
                { g with Hazard = None }, players, []
            elif t < 0 then
                { g with Hazard = Some aged }, players, []
            else
                let startFx =
                    if t = 0 then
                        [ Sound SfxHazard ]
                        @ (match h.Kind with
                           | Meteor -> h.Xs |> List.collect (fun x -> [ ExplosionAt({ X = x; Y = l.Top - 10.0 }, { X = 0.0; Y = -1.0 }, "#b48cff"); ShakeBy({ X = 0.0; Y = 1.0 }, 14.0) ])
                           | LavaGeyser -> h.Xs |> List.map (fun _ -> ShakeBy({ X = 0.0; Y = 1.0 }, 8.0))
                           | _ -> [])
                    else
                        []

                let ambientFx =
                    match h.Kind with
                    | LavaGeyser when t % 3 = 0 -> h.Xs |> List.map (fun x -> FlamesAt({ X = x; Y = l.Top - 40.0 }, { X = 0.0; Y = -1.0 }, 3))
                    | Icicles ->
                        h.Xs
                        |> List.mapi (fun i x -> i, x)
                        |> List.choose (fun (i, x) -> if icicleY l t i >= l.Top && icicleY l (t - 1) i < l.Top then Some(SparksAt({ X = x; Y = l.Top }, { X = 0.0; Y = -1.0 }, 10, "#d8f4ff")) else None)
                    | _ -> []

                // Wind schiebt, alle anderen Ereignisse treffen
                let pushed =
                    if h.Kind = WindGust then
                        players
                        |> List.map (fun p ->
                            match p.State with
                            | KO
                            | Respawning
                            | LedgeHang
                            | Held -> p
                            | _ when p.OnGround -> { p with Pos = { p.Pos with X = p.Pos.X + h.Dir * 1.15 } }
                            | _ -> { p with Vel = { p.Vel with X = clamp -10.0 10.0 (p.Vel.X + h.Dir * 0.24) } })
                    else
                        players

                let zones = hazardZones l aged t

                let victims =
                    pushed
                    |> List.filter (fun p -> not (List.contains p.Id h.HitIds) && hittable p && zones |> List.exists (fun z -> Rect.overlaps z (hurtbox p)))
                    |> List.map (fun p -> p.Id)

                let dirOf (p: Player) =
                    match h.Kind with
                    | LaserSweep -> h.Dir
                    | _ ->
                        match h.Xs with
                        | x :: _ -> if p.Pos.X >= x then 1.0 else -1.0
                        | [] -> 1.0

                let hitPlayers, hitFx =
                    victims
                    |> List.fold
                        (fun (ps: Player list, fx) id ->
                            let p = ps |> List.find (fun q -> q.Id = id)
                            let ps2, f2 = environmentHit (hazardHitbox h.Kind) (dirOf p) ps [ id ]
                            ps2, fx @ f2)
                        (pushed, [])

                { g with Hazard = Some { aged with HitIds = aged.HitIds @ victims } }, hitPlayers, startFx @ ambientFx @ hitFx

// ---------------------------------------------------------------------------
// Haupt-Simulationsschritt (fester Timestep, 60 Hz)
// ---------------------------------------------------------------------------

/// Läuft immer mit: Partikel, Shake-Abklingen, Frame-Zähler
let private ambient (g: Game) : Game =
    { g with
        Frame = g.Frame + 1
        Particles = updateParticles g.Particles
        ShakeMag = (if g.ShakeMag < 0.3 then 0.0 else g.ShakeMag * 0.87)
        ImpactFlash = max 0 (g.ImpactFlash - 1)
        Beams =
            g.Beams
            |> List.map (fun b -> { b with Life = b.Life - 1 })
            |> List.filter (fun b -> b.Life > 0)
        GoTimer = max 0 (g.GoTimer - 1)
        Sounds = [] }

let private isPowerMove (m: MoveId) =
    match m with
    | ForwardSmash
    | DownSmash
    | AirSmash
    | UpSpecial
    | SideSpecial -> true
    | _ -> false

/// Laufende Effekte: Rauch und Speed-Lines bei hohem Knockback, Auren bei Smash- und Spezialangriffen
let private trailEffects (frame: int) (players: Player list) : Effect list =
    players
    |> List.collect (fun p ->
        let speed = Vec2.length p.Vel
        let center = bodyCenter p

        let knockbackFx =
            if p.State = Hitstun && speed > 9.0 then
                let tint = if p.Percent > 100.0 then "#ffe2c4" else "#eef2fa"

                (if frame % 2 = 0 || p.Percent > 80.0 then [ SmokeAt(center, tint) ] else [])
                @ (if p.Percent > 120.0 then [ SmokeAt(center, "#8c8f9c") ] else [])
                @ (if speed > 14.0 then [ SpeedLinesAt(center, Vec2.normalize p.Vel) ] else [])
            else
                []

        let attackFx =
            match p.State with
            | AttackStartup m when isPowerMove m && p.Fighter = Ignis && frame % 2 = 0 ->
                [ FlamesAt({ X = p.Pos.X + p.Facing * 8.0; Y = p.Pos.Y - 30.0 }, { X = 0.0; Y = -1.0 }, 1) ]
            | AttackStartup m when (moveData p.Fighter m).Armor && frame % 3 = 0 ->
                [ SparksAt(center, { X = 0.0; Y = -1.0 }, 1, "#ffd166") ]
            | AttackStartup m when p.Fighter = Chrono && (m = ForwardSmash || m = AirSmash) && frame % 2 = 0 ->
                [ SparksAt({ X = center.X + p.Facing * 50.0; Y = center.Y }, { X = 0.0; Y = -1.0 }, 1, "#4fe0c8") ]
            | AttackActive m when isPowerMove m ->
                let md = moveData p.Fighter m
                let hold = md.HoldVelX.IsSome || md.HoldVelY.IsSome || md.Flight > 0.0

                (activeHitboxes p
                 |> List.map (fun (rect, _) ->
                     let at = { X = rect.Left + rect.Width / 2.0; Y = rect.Top + rect.Height / 2.0 }

                     match p.Fighter with
                     | Ignis -> FlamesAt(at, { X = p.Facing; Y = -0.3 }, 3)
                     | Titan -> SparksAt(at, { X = p.Facing; Y = -0.3 }, 2, "#ffd166")
                     | Zephyr -> SparksAt(at, { X = p.Facing; Y = -0.2 }, 2, "#d7c2ff")
                     | Aether -> SparksAt(at, { X = p.Facing; Y = -0.2 }, 2, "#9be8ff")
                     | Valkyrie -> SparksAt(at, { X = p.Facing; Y = -0.2 }, 2, "#fff3b0")
                     | Chrono -> RingAt(at, "#4fe0c8")
                     | Bramble -> LeavesAt(at, 1)
                     | Specter -> ShadowsAt(at, 2)
                     | Ronin -> SparksAt(at, { X = p.Facing; Y = -0.2 }, 2, "#ffd0d8")
                     | Lyra -> SparksAt(at, { X = p.Facing; Y = -0.2 }, 2, "#e8ecff")
                     | Gorm -> DustAt(at, 1)))
                @ (if hold && frame % 2 = 0 then [ SpeedLinesAt(center, Vec2.normalize p.Vel) ] else [])
                @ (if p.Fighter = Specter && md.Flight > 0.0 then [ ShadowsAt(center, 2) ] else [])
            | AttackActive DownSpecial when frame % 2 = 0 -> [ SpeedLinesAt(center, { X = 0.0; Y = 1.0 }) ]
            | AttackActive UpSpecial when p.Fighter = Titan ->
                [ FlamesAt({ X = p.Pos.X - p.Facing * 14.0; Y = p.Pos.Y - 30.0 }, { X = 0.0; Y = 1.0 }, 2) ]
            | _ -> []

        // Regenbogen-Aura mit Smash-Ball, Stasis-Funkeln, Schatten-Schleier
        let auraFx =
            (if p.HasFinal && frame % 2 = 0 then [ RainbowAt(center, 1) ] else [])
            @ (if p.Slow > 0 && frame % 4 = 0 then [ SparksAt(center, { X = 0.0; Y = -1.0 }, 1, "#4fe0c8") ] else [])
            @ (if p.Fighter = Specter && p.State = Running && frame % 4 = 0 then [ ShadowsAt(p.Pos, 1) ] else [])

        knockbackFx @ attackFx @ auraFx)

/// Smash-Ball: Countdown, Erscheinen, Flug und Treffer
let private updateBall (g: Game) (players: Player list) (projectiles: Projectile list) : Game * Player list * Projectile list * Effect list =
    if not g.SmashBallOn then
        g, players, projectiles, []
    else
        match g.Ball with
        | Some b ->
            match stepBall b with
            | None ->
                let seed, delay = ballDelay g.Seed
                { g with Ball = None; BallTimer = delay; Seed = seed }, players, projectiles, []
            | Some moved ->
                let ball, ps, prs, fx = resolveBall players projectiles moved

                let g2 =
                    match ball with
                    | Some _ -> { g with Ball = ball }
                    | None ->
                        let seed, delay = ballDelay g.Seed
                        { g with Ball = None; BallTimer = delay; Seed = seed }

                g2, ps, prs, fx
        | None ->
            let someoneHasIt = players |> List.exists (fun p -> p.HasFinal) || g.Ultra.IsSome

            if someoneHasIt then
                g, players, projectiles, []
            elif g.BallTimer <= 0 then
                let seed, ball = spawnBall g.Seed
                { g with Ball = Some ball; Seed = seed }, players, projectiles, [ Sound SfxBallAppear ]
            else
                { g with BallTimer = g.BallTimer - 1 }, players, projectiles, []

let private simulate (inputs: Input list) (g: Game) : Game =
    let layout = stageLayout g.Stage g.MatchFrames

    // Stasis: verlangsamte Kämpfer bewegen sich nur in jedem zweiten Frame
    let stepped =
        List.map2
            (fun inp (p: Player) ->
                if p.Slow > 0 && g.Frame % 2 = 1 then
                    { p with Slow = p.Slow - 1 }, []
                else
                    let q, fx = stepPlayer layout inp p
                    { q with Slow = max 0 (q.Slow - 1) }, fx)
            inputs
            g.Players

    let afterMove = stepped |> List.map fst
    let moveFx = stepped |> List.collect snd
    let afterPhantom, phantomFx = resolvePhantom moveFx afterMove
    let afterCombat, combatFx = resolveCombat afterPhantom
    let afterCommand, commandFx = resolveCommandGrabs afterCombat
    let afterGrabs, grabFx0 = resolveGrabs afterCommand
    let grabFx = commandFx @ grabFx0
    let afterProjectiles, projectiles, projectileFx = resolveProjectiles layout g.Frame afterGrabs g.Projectiles
    let ballGame, afterBall, projectilesLeft, ballFx = updateBall g afterProjectiles projectiles
    let itemGame, afterItems, itemFx = updateItems layout ballGame afterBall projectilesLeft
    let hazardGame, afterHazard, hazardFx = updateHazard layout itemGame afterItems
    let blasted = afterHazard |> List.map checkBlastZone
    let players = blasted |> List.map fst
    let blastFx = blasted |> List.collect snd
    let effects =
        moveFx @ phantomFx @ combatFx @ grabFx @ projectileFx @ ballFx @ itemFx @ hazardFx @ blastFx @ trailEffects g.Frame players
    let alive = players |> List.filter (fun p -> p.Stocks > 0)
    let decided = List.length alive <= 1
    let timer = if decided then g.GameOverTimer + 1 else 0
    let finished = decided && timer >= gameOverDelay

    let next =
        { hazardGame with
            Players = players
            Projectiles = projectilesLeft
            MatchFrames = g.MatchFrames + 1
            GameOverTimer = timer
            Winner = (if decided then alive |> List.tryHead |> Option.map (fun p -> p.Id) else None)
            Screen = (if finished then GameOver else Playing) }
        |> applyEffects effects

    if finished then
        { next with
            Sounds = next.Sounds @ [ SfxVictory ]
            MenuIndex = 0
            Projectiles = []
            Ball = None
            Items = []
            Hazard = None
            Ultra = None }
    else
        next

/// Während des Countdowns stehen die Spieler still; gehaltene Tasten zählen danach nicht als neuer Druck
let private countdown (inputs: Input list) (g: Game) : Game =
    let remaining = g.Countdown - 1
    let players = List.map2 (fun inp (p: Player) -> updateCloth { p with Prev = inp }) inputs g.Players

    let sounds =
        if remaining = 0 then [ SfxGo ]
        elif remaining % 70 = 0 then [ SfxCountdown ]
        else []

    { g with
        Players = players
        Countdown = remaining
        GoTimer = (if remaining = 0 then goFrames else g.GoTimer)
        Sounds = g.Sounds @ sounds }

let step (inputs: Input list) (game: Game) : Game =
    let g = ambient game

    match g.Screen with
    | Title
    | CharSelect
    | StageSelect
    | GameOver -> g
    | Playing ->
        match g.Ultra with
        | Some u ->
            let next, fx = stepUltra u g
            applyEffects fx next
        | None ->
            if g.Countdown > 0 then countdown inputs g
            elif g.HitStop > 0 then { g with HitStop = g.HitStop - 1 }
            else simulate inputs g
