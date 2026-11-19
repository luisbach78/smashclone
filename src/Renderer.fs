module SmashClone.Renderer

open System
open Browser
open Browser.Types
open Fable.Core.JsInterop
open SmashClone.Types

type Ctx = CanvasRenderingContext2D

// ---------------------------------------------------------------------------
// Zeichen-Helfer
// ---------------------------------------------------------------------------

let private tau = Math.PI * 2.0
let private displayFont = "Impact, 'Arial Black', 'Helvetica Neue', sans-serif"

let private fill (ctx: Ctx) (color: string) = ctx.fillStyle <- !^color
let private stroke (ctx: Ctx) (color: string) = ctx.strokeStyle <- !^color

let private circle (ctx: Ctx) (x: float) (y: float) (r: float) =
    ctx.beginPath ()
    ctx.arc (x, y, max 0.1 r, 0.0, tau)

let private oval (ctx: Ctx) (x: float) (y: float) (rx: float) (ry: float) =
    ctx.beginPath ()
    ctx.ellipse (x, y, max 0.1 rx, max 0.1 ry, 0.0, 0.0, tau)

let private roundRect (ctx: Ctx) (x: float) (y: float) (w: float) (h: float) (r: float) =
    let rr = min r (min (w / 2.0) (h / 2.0))
    ctx.beginPath ()
    ctx.moveTo (x + rr, y)
    ctx.arcTo (x + w, y, x + w, y + h, rr)
    ctx.arcTo (x + w, y + h, x, y + h, rr)
    ctx.arcTo (x, y + h, x, y, rr)
    ctx.arcTo (x, y, x + w, y, rr)
    ctx.closePath ()

/// Geschlossenes Polygon aus (x, y)-Paaren
let private poly (ctx: Ctx) (points: (float * float) list) =
    ctx.beginPath ()

    points
    |> List.iteri (fun i (x, y) -> if i = 0 then ctx.moveTo (x, y) else ctx.lineTo (x, y))

    ctx.closePath ()

let private polyV (ctx: Ctx) (points: Vec2 list) =
    poly ctx (points |> List.map (fun p -> p.X, p.Y))

let private lerp (a: float) (b: float) (t: float) = a + (b - a) * t
let private clamp01 (v: float) = max 0.0 (min 1.0 v)
let private rad (degrees: float) = degrees * Math.PI / 180.0
let private v (x: float) (y: float) : Vec2 = { X = x; Y = y }
let private (++) (a: Vec2) (b: Vec2) = Vec2.add a b
let private polar (angle: float) (r: float) : Vec2 = { X = cos angle * r; Y = sin angle * r }
let private mix (a: Vec2) (b: Vec2) (t: float) = v (lerp a.X b.X t) (lerp a.Y b.Y t)

/// Text mit dunkler Kontur
let private outlinedText (ctx: Ctx) (text: string) (x: float) (y: float) (color: string) (outline: float) =
    ctx.lineJoin <- "round"
    ctx.lineWidth <- outline
    stroke ctx "#0b0d18"
    ctx.strokeText (text, x, y)
    fill ctx color
    ctx.fillText (text, x, y)

/// Pseudo-Zufall aus einem Index (für Details, die jedes Frame gleich liegen müssen)
let private hash (i: int) (salt: float) =
    let x = sin (float i * 12.9898 + salt * 78.233) * 43758.5453
    x - floor x

// Geometrie der klassischen Sky-Battlefield-Stage (für die gecachte Stage-Zeichnung)
let private skyLayout = stageLayout SkyBattlefield 0
let private mainLeft = skyLayout.Left
let private mainRight = skyLayout.Right
let private mainTop = skyLayout.Top
let private mainBottom = skyLayout.Bottom
let private stageCenterX = stageCenter skyLayout
let private softPlatforms = skyLayout.Soft

// ---------------------------------------------------------------------------
// Parallax-Hintergrund: Himmel über den Wolken
// ---------------------------------------------------------------------------

let private sunX = 905.0
let private sunY = 452.0

/// Wolkenbank im Cel-Look: dunkle Unterseiten, helle Kuppen, geschlossene Fläche darunter
let private cloudBank
    (ctx: Ctx)
    (t: float)
    (baseY: float)
    (amp: float)
    (speed: float)
    (count: int)
    (rMin: float)
    (rMax: float)
    (shade: string)
    (light: string)
    (salt: float)
    =
    let span = screenWidth + 400.0

    for pass in 0..1 do
        fill ctx (if pass = 0 then shade else light)

        for i in 0 .. count - 1 do
            let x = ((float i * span / float count + hash i salt * 90.0 + t * speed) % span) - 200.0
            let r = rMin + hash i (salt + 1.0) * (rMax - rMin)
            let y = baseY + hash i (salt + 2.0) * amp

            if pass = 0 then circle ctx x y r else circle ctx (x - r * 0.16) (y - r * 0.2) (r * 0.8)

            ctx.fill ()

    fill ctx light
    ctx.fillRect (-400.0, baseY + amp + rMin * 0.2, screenWidth + 1200.0, screenHeight)

/// Schwebende Tempel-Ruine als zweifarbige Dunst-Silhouette
let private drawRuin (ctx: Ctx) (t: float) (x: float) (y: float) (scale: float) (dark: string) (light: string) (salt: float) =
    ctx.save ()
    ctx.translate (x, y + sin (t * 0.012 + salt) * 5.0)
    ctx.scale (scale, scale)

    // Felsinsel
    fill ctx dark

    poly
        ctx
        [ (-96.0, 0.0); (96.0, 0.0); (74.0, 22.0); (44.0, 30.0); (28.0, 62.0); (8.0, 48.0)
          (-8.0, 92.0); (-30.0, 46.0); (-58.0, 34.0); (-84.0, 18.0) ]

    ctx.fill ()

    // Säulen (teils abgebrochen), Gebälk und Giebel
    let columns = [ -64.0, 78.0; -36.0, 78.0; -6.0, 34.0 + hash 3 salt * 30.0; 30.0, 58.0; 62.0, 22.0 + hash 5 salt * 20.0 ]

    for cx, h in columns do
        fill ctx dark
        ctx.fillRect (cx - 6.0, -h, 12.0, h)
        fill ctx light
        ctx.fillRect (cx - 6.0, -h, 4.0, h)
        ctx.fillRect (cx - 8.0, -h - 4.0, 16.0, 4.0)

    fill ctx dark
    ctx.fillRect (-74.0, -90.0, 50.0, 9.0)
    poly ctx [ (-78.0, -90.0); (-49.0, -112.0); (-20.0, -90.0) ]
    ctx.fill ()
    fill ctx light
    ctx.fillRect (-96.0, -5.0, 192.0, 6.0)
    ctx.fillRect (-74.0, -90.0, 50.0, 3.0)
    poly ctx [ (-78.0, -90.0); (-49.0, -112.0); (-49.0, -106.0); (-68.0, -90.0) ]
    ctx.fill ()

    // Herabhängende Ranken
    stroke ctx dark
    ctx.lineWidth <- 2.0

    for i in 0..3 do
        let vx = -70.0 + float i * 44.0
        ctx.beginPath ()
        ctx.moveTo (vx, 6.0)
        ctx.quadraticCurveTo (vx + 6.0, 30.0, vx + sin (t * 0.02 + float i) * 4.0, 44.0 + hash i salt * 26.0)
        ctx.stroke ()

    ctx.restore ()

/// Statischer Teil des Himmels (wird einmalig in eine Ebene gezeichnet)
let private paintBackdrop (ctx: Ctx) =
    let t = 0.0

    // Dramatischer Abendhimmel
    let sky = ctx.createLinearGradient (0.0, 0.0, 0.0, screenHeight)
    sky.addColorStop (0.0, "#0c1a4f")
    sky.addColorStop (0.32, "#274f9f")
    sky.addColorStop (0.58, "#7a6fc2")
    sky.addColorStop (0.76, "#f28b6b")
    sky.addColorStop (1.0, "#ffd9a0")
    ctx.fillStyle <- !^sky
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

    // Sterne im oberen Himmel
    fill ctx "#ffffff"

    for i in 0..54 do
        let y = hash i 2.0 * 260.0
        ctx.globalAlpha <- (0.35 + 0.35 * sin (t * 0.04 + float i)) * (1.0 - y / 280.0)
        ctx.fillRect (hash i 1.0 * screenWidth, y, 1.5 + hash i 3.0 * 1.5, 1.5 + hash i 3.0 * 1.5)

    ctx.globalAlpha <- 1.0

    // Sonne mit Hof
    let halo = ctx.createRadialGradient (sunX, sunY, 10.0, sunX, sunY, 420.0)
    halo.addColorStop (0.0, "rgba(255,244,200,0.95)")
    halo.addColorStop (0.12, "rgba(255,214,150,0.6)")
    halo.addColorStop (0.45, "rgba(255,150,110,0.18)")
    halo.addColorStop (1.0, "rgba(255,150,110,0)")
    ctx.fillStyle <- !^halo
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)
    fill ctx "#fff8dc"
    circle ctx sunX sunY 44.0
    ctx.fill ()

    // God-Rays: langsam schwenkende Lichtfächer
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"

    for i in 0..8 do
        let angle = -2.9 + float i * 0.34 + sin (t * 0.006 + float i * 1.7) * 0.07
        let width = 0.035 + hash i 9.0 * 0.05
        let ray = ctx.createLinearGradient (sunX, sunY, sunX + cos angle * 900.0, sunY + sin angle * 900.0)
        ray.addColorStop (0.0, "rgba(255,230,170,0.16)")
        ray.addColorStop (1.0, "rgba(255,230,170,0)")
        ctx.fillStyle <- !^ray

        poly
            ctx
            [ (sunX, sunY)
              (sunX + cos (angle - width) * 1100.0, sunY + sin (angle - width) * 1100.0)
              (sunX + cos (angle + width) * 1100.0, sunY + sin (angle + width) * 1100.0) ]

        ctx.fill ()

    ctx.restore ()

    // Dünne Schleierwolken weiter oben
    fill ctx "#ffffff"

    for i in 0..3 do
        let x = ((float i * 420.0 + t * (0.08 + float i * 0.03)) % (screenWidth + 500.0)) - 250.0
        let y = 96.0 + float i * 58.0
        ctx.globalAlpha <- 0.1
        oval ctx x y 190.0 13.0
        ctx.fill ()
        oval ctx (x + 90.0) (y - 9.0) 110.0 9.0
        ctx.fill ()

    ctx.globalAlpha <- 1.0

let private paintFarClouds (ctx: Ctx) =
    cloudBank ctx 0.0 536.0 24.0 0.0 15 40.0 80.0 "#c9788a" "#ffb99a" 11.0

let private paintRuins (ctx: Ctx) =
    drawRuin ctx 0.0 780.0 232.0 0.42 "#6f78c4" "#98a2e0" 1.0
    drawRuin ctx 0.0 1100.0 318.0 0.74 "#5560b4" "#8d99e0" 2.0
    drawRuin ctx 0.0 176.0 352.0 0.98 "#434c9c" "#7f8cdc" 3.0

let private paintMidClouds (ctx: Ctx) =
    cloudBank ctx 0.0 640.0 26.0 0.0 13 52.0 100.0 "#b592c8" "#ffe6d6" 23.0

/// Vordere Wolkenschicht (liegt vor der Stage, damit sie über den Wolken schwebt)
let private paintFrontClouds (ctx: Ctx) =
    cloudBank ctx 0.0 742.0 12.0 0.0 10 44.0 76.0 "#c5b6e6" "#ffffff" 37.0

// ---------------------------------------------------------------------------
// Battlefield-Stage
// ---------------------------------------------------------------------------

let private paintRunes (ctx: Ctx) =
    let t = 31.0
    ctx.save ()
    ctx.lineCap <- "round"
    ctx.lineJoin <- "round"
    ctx.shadowColor <- "#5ff3ff"
    ctx.shadowBlur <- 10.0
    stroke ctx "#b8fbff"
    ctx.lineWidth <- 2.2

    // Runenband
    for i in 0..10 do
        if i <> 5 then
            let x = mainLeft + 62.0 + float i * 55.6
            let y = mainTop + 52.0
            ctx.globalAlpha <- 0.75 + 0.25 * sin (t * 0.05 + float i * 0.9)
            ctx.beginPath ()
            ctx.moveTo (x, y - 8.0)
            ctx.lineTo (x, y + 8.0)

            if hash i 4.0 > 0.4 then
                ctx.moveTo (x, y - 8.0)
                ctx.lineTo (x + 6.0, y - 2.0)

            if hash i 5.0 > 0.5 then
                ctx.moveTo (x + 6.0, y - 2.0)
                ctx.lineTo (x, y + 2.0)

            if hash i 6.0 > 0.45 then
                ctx.moveTo (x - 5.0, y + 1.0)
                ctx.lineTo (x + 5.0, y + 7.0)

            if hash i 7.0 > 0.6 then
                ctx.moveTo (x - 5.0, y - 6.0)
                ctx.lineTo (x, y - 1.0)

            ctx.stroke ()

    // Zentrales Siegel
    let cx = stageCenterX
    let cy = mainTop + 56.0
    ctx.globalAlpha <- 1.0
    circle ctx cx cy 17.0
    ctx.stroke ()
    poly ctx [ (cx, cy - 12.0); (cx + 10.5, cy + 6.0); (cx - 10.5, cy + 6.0) ]
    ctx.stroke ()
    circle ctx cx cy 3.0
    ctx.stroke ()
    ctx.restore ()

/// Statische Stage-Geometrie (wird einmalig in eine Ebene gezeichnet)
let private paintStage (ctx: Ctx) =
    let t = 0.0
    let width = mainRight - mainLeft

    // Unterbau: gestufter Fels, der nach unten spitz zuläuft
    let rockShape =
        [ (mainLeft + 6.0, mainTop + 20.0); (mainRight - 6.0, mainTop + 20.0); (mainRight - 10.0, mainBottom)
          (mainRight - 70.0, mainBottom + 8.0); (mainRight - 120.0, mainBottom + 44.0); (mainRight - 210.0, mainBottom + 52.0)
          (stageCenterX + 60.0, mainBottom + 96.0); (stageCenterX + 6.0, mainBottom + 150.0)
          (stageCenterX - 44.0, mainBottom + 92.0); (mainLeft + 190.0, mainBottom + 56.0)
          (mainLeft + 110.0, mainBottom + 40.0); (mainLeft + 64.0, mainBottom + 8.0); (mainLeft + 10.0, mainBottom) ]

    stroke ctx "#141a30"
    ctx.lineWidth <- 6.0
    ctx.lineJoin <- "round"
    poly ctx rockShape
    ctx.stroke ()
    let rock = ctx.createLinearGradient (0.0, mainTop, 0.0, mainBottom + 150.0)
    rock.addColorStop (0.0, "#8d93b8")
    rock.addColorStop (0.45, "#5b5f8c")
    rock.addColorStop (1.0, "#2c2c52")
    ctx.fillStyle <- !^rock
    ctx.fill ()

    // Cel-Schatten auf der sonnenabgewandten Seite
    ctx.save ()
    poly ctx rockShape
    ctx.clip ()
    fill ctx "rgba(24,22,60,0.38)"
    poly ctx [ (mainLeft, mainTop); (mainLeft + 250.0, mainTop); (stageCenterX - 30.0, mainBottom + 160.0); (mainLeft, mainBottom + 160.0) ]
    ctx.fill ()
    fill ctx "rgba(255,226,190,0.16)"
    poly ctx [ (mainRight - 150.0, mainTop); (mainRight, mainTop); (mainRight, mainBottom + 40.0); (mainRight - 60.0, mainBottom + 40.0) ]
    ctx.fill ()

    // Mauerwerk: Lagerfugen und versetzte Stoßfugen
    stroke ctx "rgba(20,26,48,0.45)"
    ctx.lineWidth <- 2.0

    for row in 0..3 do
        let y = mainTop + 34.0 + float row * 22.0

        if row <> 1 then
            ctx.beginPath ()
            ctx.moveTo (mainLeft, y)
            ctx.lineTo (mainRight, y)
            ctx.stroke ()

        for col in 0..11 do
            let x = mainLeft + 20.0 + float col * 60.0 + (if row % 2 = 0 then 0.0 else 30.0)

            if row <> 0 && row <> 1 then
                ctx.beginPath ()
                ctx.moveTo (x, y - 22.0)
                ctx.lineTo (x, y)
                ctx.stroke ()

    // Risse im unteren Fels
    for i in 0..5 do
        let x = mainLeft + 130.0 + float i * 84.0
        ctx.beginPath ()
        ctx.moveTo (x, mainBottom + 6.0)
        ctx.lineTo (x + 10.0 - hash i 8.0 * 20.0, mainBottom + 30.0 + hash i 9.0 * 30.0)
        ctx.stroke ()

    ctx.restore ()

    // Runenfries: dunkles Band mit leuchtenden Zeichen
    fill ctx "rgba(18,20,52,0.55)"
    ctx.fillRect (mainLeft + 30.0, mainTop + 38.0, width - 60.0, 30.0)

    // Steinerne Deckplatte mit Überstand
    stroke ctx "#141a30"
    ctx.lineWidth <- 5.0
    roundRect ctx (mainLeft - 10.0) (mainTop + 4.0) (width + 20.0) 22.0 5.0
    ctx.stroke ()
    let slab = ctx.createLinearGradient (0.0, mainTop + 4.0, 0.0, mainTop + 26.0)
    slab.addColorStop (0.0, "#dfe3f2")
    slab.addColorStop (0.5, "#a9aed0")
    slab.addColorStop (1.0, "#6f7499")
    ctx.fillStyle <- !^slab
    ctx.fill ()
    fill ctx "rgba(20,26,48,0.35)"
    ctx.fillRect (mainLeft - 8.0, mainTop + 20.0, width + 16.0, 5.0)

    // Herabhängendes Moos
    for i in 0..33 do
        let x = mainLeft - 4.0 + float i * (width + 8.0) / 33.0
        let h = 5.0 + hash i 13.0 * 13.0
        fill ctx (if i % 3 = 0 then "#2f8f5b" else "#3fae6c")
        roundRect ctx (x - 5.0) (mainTop + 6.0) 10.0 h 5.0
        ctx.fill ()

    // Rasenoberfläche
    stroke ctx "#141a30"
    ctx.lineWidth <- 4.0
    roundRect ctx (mainLeft - 10.0) (mainTop - 1.0) (width + 20.0) 11.0 5.5
    ctx.stroke ()
    let grass = ctx.createLinearGradient (0.0, mainTop - 1.0, 0.0, mainTop + 10.0)
    grass.addColorStop (0.0, "#a8f08a")
    grass.addColorStop (0.4, "#5fcf72")
    grass.addColorStop (1.0, "#2f8f5b")
    ctx.fillStyle <- !^grass
    ctx.fill ()
    fill ctx "rgba(255,255,255,0.5)"
    roundRect ctx (mainLeft - 4.0) mainTop (width + 8.0) 2.5 1.2
    ctx.fill ()

    // Grasbüschel
    fill ctx "#7fe07f"

    for i in 0..44 do
        let x = mainLeft + 4.0 + float i * (width - 8.0) / 44.0
        let h = 4.0 + hash i 17.0 * 6.0
        let sway = sin (t * 0.05 + float i) * 1.5
        poly ctx [ (x - 3.0, mainTop + 1.0); (x + sway, mainTop - h); (x + 3.0, mainTop + 1.0) ]
        ctx.fill ()

    // Ledge-Markierungen: goldene Ecksteine mit pulsierendem Greifpunkt
    for side in [ -1.0; 1.0 ] do
        let x = if side < 0.0 then mainLeft - 12.0 else mainRight - 6.0
        stroke ctx "#141a30"
        ctx.lineWidth <- 4.0
        roundRect ctx x (mainTop - 3.0) 18.0 30.0 4.0
        ctx.stroke ()
        let gold = ctx.createLinearGradient (x, 0.0, x + 18.0, 0.0)
        gold.addColorStop (0.0, "#fff0a8")
        gold.addColorStop (0.5, "#f2b63c")
        gold.addColorStop (1.0, "#a86a18")
        ctx.fillStyle <- !^gold
        ctx.fill ()

    // Schwebende Kristall-/Energie-Plattformen
    softPlatforms
    |> List.iteri (fun i pl ->
        let w = pl.X2 - pl.X1
        let cx = (pl.X1 + pl.X2) / 2.0
        let pulse = 0.75 + 0.25 * sin (t * 0.06 + float i * 2.0)

        // Schwebender Kristall unter der Plattform
        let bob = sin (t * 0.04 + float i * 1.3) * 3.0
        let ky = pl.Y + 30.0 + bob
        ctx.save ()
        ctx.shadowColor <- "#5ff3ff"
        ctx.shadowBlur <- 18.0 * pulse
        stroke ctx "#141a30"
        ctx.lineWidth <- 3.0
        poly ctx [ (cx, ky - 12.0); (cx + 8.0, ky - 2.0); (cx, ky + 16.0); (cx - 8.0, ky - 2.0) ]
        ctx.stroke ()
        let crystal = ctx.createLinearGradient (cx - 8.0, 0.0, cx + 8.0, 0.0)
        crystal.addColorStop (0.0, "#e9fdff")
        crystal.addColorStop (0.5, "#6fdcff")
        crystal.addColorStop (1.0, "#2b6fd6")
        ctx.fillStyle <- !^crystal
        ctx.fill ()
        ctx.restore ()
        fill ctx "rgba(255,255,255,0.7)"
        poly ctx [ (cx, ky - 12.0); (cx - 8.0, ky - 2.0); (cx - 2.0, ky - 3.0) ]
        ctx.fill ()

        // Energiestrahl zwischen Kristall und Platte
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let beam = ctx.createLinearGradient (0.0, pl.Y + 8.0, 0.0, ky - 10.0)
        beam.addColorStop (0.0, "rgba(120,235,255,0.55)")
        beam.addColorStop (1.0, "rgba(120,235,255,0)")
        ctx.fillStyle <- !^beam
        poly ctx [ (cx - 46.0, pl.Y + 8.0); (cx + 46.0, pl.Y + 8.0); (cx + 5.0, ky - 10.0); (cx - 5.0, ky - 10.0) ]
        ctx.fill ()
        ctx.restore ()

        // Platte
        let plate = [ (pl.X1 - 4.0, pl.Y); (pl.X2 + 4.0, pl.Y); (pl.X2 - 10.0, pl.Y + 11.0); (pl.X1 + 10.0, pl.Y + 11.0) ]
        ctx.save ()
        ctx.shadowColor <- "rgba(95,243,255,0.9)"
        ctx.shadowBlur <- 20.0 * pulse
        stroke ctx "#141a30"
        ctx.lineWidth <- 4.0
        ctx.lineJoin <- "round"
        poly ctx plate
        ctx.stroke ()
        let grad = ctx.createLinearGradient (0.0, pl.Y, 0.0, pl.Y + 11.0)
        grad.addColorStop (0.0, "#f2feff")
        grad.addColorStop (0.35, "#8fe9ff")
        grad.addColorStop (1.0, "#2f7fd8")
        ctx.fillStyle <- !^grad
        ctx.fill ()
        ctx.restore ()

        // Facetten und wandernder Glanz
        stroke ctx "rgba(255,255,255,0.55)"
        ctx.lineWidth <- 1.5

        for k in 1..4 do
            let fx = pl.X1 + w * float k / 5.0
            ctx.beginPath ()
            ctx.moveTo (fx, pl.Y + 1.0)
            ctx.lineTo (fx - 6.0, pl.Y + 10.0)
            ctx.stroke ())

// ---------------------------------------------------------------------------
// Astral Altar: Galaxie-Hintergrund, Energie-Säulen, flache Altar-Plattform
// ---------------------------------------------------------------------------

let private astral = stageLayout AstralAltar 0

let private paintAstralBackdrop (ctx: Ctx) =
    let space = ctx.createLinearGradient (0.0, 0.0, 0.0, screenHeight)
    space.addColorStop (0.0, "#03021a")
    space.addColorStop (0.45, "#120a3a")
    space.addColorStop (0.8, "#2a1260")
    space.addColorStop (1.0, "#3e1a6e")
    ctx.fillStyle <- !^space
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

    // Galaxie-Band (schräge, weiche Ellipse)
    ctx.save ()
    ctx.translate (640.0, 300.0)
    ctx.rotate -0.32
    ctx.globalCompositeOperation <- "lighter"

    for i in 0..5 do
        let w = 760.0 - float i * 110.0
        let h = 120.0 - float i * 18.0
        let band = ctx.createRadialGradient (0.0, 0.0, 4.0, 0.0, 0.0, w)
        band.addColorStop (0.0, (if i = 5 then "rgba(255,240,220,0.5)" else "rgba(190,150,255,0.12)"))
        band.addColorStop (1.0, "rgba(120,80,220,0)")
        ctx.fillStyle <- !^band
        ctx.save ()
        ctx.scale (1.0, h / w)
        circle ctx 0.0 0.0 w
        ctx.fill ()
        ctx.restore ()

    ctx.restore ()

    // Nebel-Wolken
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"

    let nebula (x: float) (y: float) (r: float) (c: string) =
        let n = ctx.createRadialGradient (x, y, 4.0, x, y, r)
        n.addColorStop (0.0, c)
        n.addColorStop (1.0, "rgba(0,0,0,0)")
        ctx.fillStyle <- !^n
        ctx.fillRect (x - r, y - r, r * 2.0, r * 2.0)

    nebula 220.0 170.0 260.0 "rgba(255,80,170,0.22)"
    nebula 1060.0 120.0 220.0 "rgba(60,200,255,0.2)"
    nebula 900.0 420.0 300.0 "rgba(140,80,255,0.18)"
    nebula 300.0 520.0 260.0 "rgba(60,255,210,0.1)"
    ctx.restore ()

    // Sterne
    for i in 0..220 do
        let x = hash i 41.0 * screenWidth
        let y = hash i 42.0 * screenHeight
        let size = 0.6 + hash i 43.0 * 1.8
        ctx.globalAlpha <- 0.35 + hash i 44.0 * 0.65
        fill ctx (if hash i 45.0 > 0.8 then "#bfe4ff" elif hash i 45.0 > 0.6 then "#ffd9f0" else "#ffffff")
        circle ctx x y size
        ctx.fill ()

    ctx.globalAlpha <- 1.0

    // Ringplanet
    let px = 1070.0
    let py = 150.0
    ctx.save ()
    ctx.translate (px, py)
    ctx.rotate -0.35
    stroke ctx "rgba(255,210,170,0.45)"
    ctx.lineWidth <- 6.0
    ctx.beginPath ()
    ctx.ellipse (0.0, 0.0, 92.0, 22.0, 0.0, Math.PI, tau)
    ctx.stroke ()
    let planet = ctx.createRadialGradient (-16.0, -18.0, 4.0, 0.0, 0.0, 54.0)
    planet.addColorStop (0.0, "#ffd9b3")
    planet.addColorStop (0.5, "#d9708f")
    planet.addColorStop (1.0, "#4a1f5c")
    ctx.fillStyle <- !^planet
    circle ctx 0.0 0.0 54.0
    ctx.fill ()
    stroke ctx "rgba(255,230,200,0.75)"
    ctx.lineWidth <- 6.0
    ctx.beginPath ()
    ctx.ellipse (0.0, 0.0, 92.0, 22.0, 0.0, 0.0, Math.PI)
    ctx.stroke ()
    ctx.restore ()

    // Ferne Säulen-Silhouetten
    for i in 0..3 do
        let x = [| 150.0; 400.0; 880.0; 1130.0 |][i]
        let top = 220.0 + float (i % 2) * 70.0
        fill ctx "rgba(30,20,70,0.85)"
        ctx.fillRect (x - 16.0, top, 32.0, screenHeight - top)
        fill ctx "rgba(120,90,220,0.35)"
        ctx.fillRect (x - 16.0, top, 6.0, screenHeight - top)
        fill ctx "rgba(40,28,90,0.95)"
        ctx.fillRect (x - 24.0, top - 14.0, 48.0, 16.0)

let private paintAstralStage (ctx: Ctx) =
    let l = astral
    let w = l.Right - l.Left
    let cx = (l.Left + l.Right) / 2.0

    // Schwebender Obsidian-Körper, nach unten spitz zulaufend
    let body =
        [ (l.Left - 6.0, l.Top + 14.0); (l.Right + 6.0, l.Top + 14.0); (l.Right - 30.0, l.Bottom)
          (cx + 160.0, l.Bottom + 40.0); (cx + 30.0, l.Bottom + 120.0); (cx - 30.0, l.Bottom + 120.0)
          (cx - 160.0, l.Bottom + 40.0); (l.Left + 30.0, l.Bottom) ]

    stroke ctx "#0a0620"
    ctx.lineWidth <- 6.0
    ctx.lineJoin <- "round"
    poly ctx body
    ctx.stroke ()
    let rock = ctx.createLinearGradient (0.0, l.Top, 0.0, l.Bottom + 120.0)
    rock.addColorStop (0.0, "#4a3a8a")
    rock.addColorStop (0.5, "#241a52")
    rock.addColorStop (1.0, "#0e0a26")
    ctx.fillStyle <- !^rock
    ctx.fill ()

    // Kristallfacetten
    ctx.save ()
    poly ctx body
    ctx.clip ()

    for i in 0..9 do
        let x = l.Left + 40.0 + float i * w / 10.0
        fill ctx (if i % 2 = 0 then "rgba(160,130,255,0.12)" else "rgba(0,0,0,0.18)")
        poly ctx [ (x, l.Top + 14.0); (x + 60.0, l.Top + 14.0); (x + 20.0, l.Bottom + 80.0) ]
        ctx.fill ()

    ctx.restore ()

    // Deckplatte aus poliertem Stein
    stroke ctx "#0a0620"
    ctx.lineWidth <- 5.0
    roundRect ctx (l.Left - 10.0) (l.Top - 1.0) (w + 20.0) 20.0 6.0
    ctx.stroke ()
    let slab = ctx.createLinearGradient (0.0, l.Top, 0.0, l.Top + 20.0)
    slab.addColorStop (0.0, "#f2eeff")
    slab.addColorStop (0.35, "#b6a8f0")
    slab.addColorStop (1.0, "#5a4aa8")
    ctx.fillStyle <- !^slab
    ctx.fill ()
    fill ctx "rgba(255,255,255,0.7)"
    roundRect ctx (l.Left - 4.0) l.Top (w + 8.0) 3.0 1.5
    ctx.fill ()

    // Goldene Ledge-Kappen
    for side in [ -1.0; 1.0 ] do
        let x = if side < 0.0 then l.Left - 12.0 else l.Right - 6.0
        stroke ctx "#0a0620"
        ctx.lineWidth <- 4.0
        roundRect ctx x (l.Top - 4.0) 18.0 28.0 4.0
        ctx.stroke ()
        let gold = ctx.createLinearGradient (x, 0.0, x + 18.0, 0.0)
        gold.addColorStop (0.0, "#fff0a8")
        gold.addColorStop (0.5, "#f2b63c")
        gold.addColorStop (1.0, "#a86a18")
        ctx.fillStyle <- !^gold
        ctx.fill ()

/// Leuchtende Energielinien auf der Altar-Front (pulsiert)
let private paintAstralGlow (ctx: Ctx) =
    let l = astral
    let cx = (l.Left + l.Right) / 2.0
    ctx.save ()
    ctx.shadowColor <- "#7ff6ff"
    ctx.shadowBlur <- 12.0
    stroke ctx "#bffaff"
    ctx.lineWidth <- 2.5
    ctx.lineCap <- "round"
    ctx.beginPath ()
    ctx.moveTo (l.Left + 30.0, l.Top + 34.0)
    ctx.lineTo (cx - 60.0, l.Top + 34.0)
    ctx.lineTo (cx - 30.0, l.Top + 56.0)
    ctx.lineTo (cx + 30.0, l.Top + 56.0)
    ctx.lineTo (cx + 60.0, l.Top + 34.0)
    ctx.lineTo (l.Right - 30.0, l.Top + 34.0)
    ctx.stroke ()
    circle ctx cx (l.Top + 82.0) 14.0
    ctx.stroke ()
    poly ctx [ (cx, l.Top + 70.0); (cx + 8.0, l.Top + 82.0); (cx, l.Top + 94.0); (cx - 8.0, l.Top + 82.0) ]
    ctx.stroke ()
    ctx.restore ()

// ---------------------------------------------------------------------------
// Magma Foundry: Vulkan-Schmiede mit Lava, Glut und beweglichen Plattformen
// ---------------------------------------------------------------------------

let private magma = stageLayout MagmaFoundry 0

let private paintMagmaBackdrop (ctx: Ctx) =
    let sky = ctx.createLinearGradient (0.0, 0.0, 0.0, screenHeight)
    sky.addColorStop (0.0, "#12040a")
    sky.addColorStop (0.4, "#3a0c10")
    sky.addColorStop (0.75, "#8a2410")
    sky.addColorStop (1.0, "#e0531a")
    ctx.fillStyle <- !^sky
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

    // Rauchschwaden am Himmel
    for i in 0..9 do
        let x = hash i 51.0 * screenWidth
        let y = 60.0 + hash i 52.0 * 220.0
        ctx.globalAlpha <- 0.18
        fill ctx "#1a0a0c"
        oval ctx x y (160.0 + hash i 53.0 * 140.0) (26.0 + hash i 54.0 * 20.0)
        ctx.fill ()

    ctx.globalAlpha <- 1.0

    // Vulkane mit Lavaströmen
    let volcano (x: float) (baseY: float) (w: float) (h: float) (dark: string) =
        fill ctx dark
        poly ctx [ (x - w, baseY); (x - w * 0.18, baseY - h); (x + w * 0.18, baseY - h); (x + w, baseY) ]
        ctx.fill ()
        let crater = ctx.createRadialGradient (x, baseY - h, 4.0, x, baseY - h, w * 0.5)
        crater.addColorStop (0.0, "rgba(255,200,90,0.9)")
        crater.addColorStop (1.0, "rgba(255,80,20,0)")
        ctx.fillStyle <- !^crater
        ctx.fillRect (x - w * 0.5, baseY - h - w * 0.5, w, w)
        stroke ctx "rgba(255,120,40,0.85)"
        ctx.lineWidth <- 3.0
        ctx.lineCap <- "round"

        for k in 0..2 do
            ctx.beginPath ()
            ctx.moveTo (x - w * 0.1 + float k * w * 0.1, baseY - h + 4.0)
            ctx.quadraticCurveTo (x - w * 0.3 + float k * w * 0.25, baseY - h * 0.5, x - w * 0.5 + float k * w * 0.45, baseY)
            ctx.stroke ()

    volcano 230.0 560.0 260.0 300.0 "#2a0a0e"
    volcano 1050.0 580.0 300.0 260.0 "#24080c"
    volcano 640.0 620.0 380.0 190.0 "#1a0609"

    // Schmiede-Schlote mit Feuer
    for i in 0..2 do
        let x = [| 120.0; 520.0; 1180.0 |][i]
        let top = [| 300.0; 360.0; 320.0 |][i]
        fill ctx "#160507"
        ctx.fillRect (x - 18.0, top, 36.0, screenHeight - top)
        fill ctx "#2e0c10"
        ctx.fillRect (x - 24.0, top - 10.0, 48.0, 12.0)
        let fire = ctx.createRadialGradient (x, top - 14.0, 2.0, x, top - 14.0, 40.0)
        fire.addColorStop (0.0, "rgba(255,230,140,0.9)")
        fire.addColorStop (1.0, "rgba(255,90,20,0)")
        ctx.fillStyle <- !^fire
        ctx.fillRect (x - 40.0, top - 54.0, 80.0, 80.0)

let private paintMagmaStage (ctx: Ctx) =
    let l = magma
    let w = l.Right - l.Left

    // Schwerer Stahlkörper mit Stützen
    stroke ctx "#0c0405"
    ctx.lineWidth <- 6.0
    ctx.lineJoin <- "round"
    poly ctx [ (l.Left, l.Top + 12.0); (l.Right, l.Top + 12.0); (l.Right - 20.0, l.Bottom); (l.Left + 20.0, l.Bottom) ]
    ctx.stroke ()
    let steel = ctx.createLinearGradient (0.0, l.Top, 0.0, l.Bottom)
    steel.addColorStop (0.0, "#6a5a5e")
    steel.addColorStop (0.5, "#3a2e32")
    steel.addColorStop (1.0, "#1e1618")
    ctx.fillStyle <- !^steel
    ctx.fill ()

    // Stützpfeiler in die Lava
    for x in [ l.Left + 70.0; l.Left + w / 2.0; l.Right - 70.0 ] do
        fill ctx "#24191b"
        ctx.fillRect (x - 16.0, l.Bottom - 2.0, 32.0, screenHeight - l.Bottom)
        fill ctx "rgba(255,120,40,0.25)"
        ctx.fillRect (x - 16.0, screenHeight - 60.0, 32.0, 60.0)

    // Panzerplatten mit Nieten
    stroke ctx "rgba(0,0,0,0.45)"
    ctx.lineWidth <- 2.0

    for i in 0..7 do
        let x = l.Left + 24.0 + float i * (w - 48.0) / 7.0
        ctx.beginPath ()
        ctx.moveTo (x, l.Top + 16.0)
        ctx.lineTo (x - 2.0, l.Bottom - 6.0)
        ctx.stroke ()

    fill ctx "#a8989c"

    for i in 0..15 do
        let x = l.Left + 30.0 + float i * (w - 60.0) / 15.0
        circle ctx x (l.Top + 24.0) 2.4
        ctx.fill ()
        circle ctx x (l.Bottom - 12.0) 2.4
        ctx.fill ()

    // Deckplatte: geriffelter Stahl
    stroke ctx "#0c0405"
    ctx.lineWidth <- 5.0
    roundRect ctx (l.Left - 10.0) (l.Top - 1.0) (w + 20.0) 16.0 4.0
    ctx.stroke ()
    let deck = ctx.createLinearGradient (0.0, l.Top, 0.0, l.Top + 16.0)
    deck.addColorStop (0.0, "#d8ccc6")
    deck.addColorStop (0.4, "#8f7f80")
    deck.addColorStop (1.0, "#4a3c3e")
    ctx.fillStyle <- !^deck
    ctx.fill ()
    stroke ctx "rgba(40,30,30,0.5)"
    ctx.lineWidth <- 1.5

    for i in 0..40 do
        let x = l.Left - 4.0 + float i * (w + 8.0) / 40.0
        ctx.beginPath ()
        ctx.moveTo (x, l.Top + 3.0)
        ctx.lineTo (x + 6.0, l.Top + 12.0)
        ctx.stroke ()

    // Warnstreifen an den Kanten (Ledges)
    for side in [ -1.0; 1.0 ] do
        let x = if side < 0.0 then l.Left - 12.0 else l.Right - 8.0
        stroke ctx "#0c0405"
        ctx.lineWidth <- 4.0
        roundRect ctx x (l.Top - 3.0) 20.0 26.0 3.0
        ctx.stroke ()
        fill ctx "#ffc23d"
        ctx.fill ()
        ctx.save ()
        roundRect ctx x (l.Top - 3.0) 20.0 26.0 3.0
        ctx.clip ()
        fill ctx "#1a1210"

        for k in 0..3 do
            poly ctx [ (x - 10.0 + float k * 10.0, l.Top + 23.0); (x - 4.0 + float k * 10.0, l.Top + 23.0); (x + 6.0 + float k * 10.0, l.Top - 3.0); (x + float k * 10.0, l.Top - 3.0) ]
            ctx.fill ()

        ctx.restore ()

/// Glühende Nähte im Stahlkörper (pulsiert)
let private paintMagmaGlow (ctx: Ctx) =
    let l = magma
    ctx.save ()
    ctx.shadowColor <- "#ff7a1a"
    ctx.shadowBlur <- 14.0
    stroke ctx "#ffb347"
    ctx.lineWidth <- 3.0
    ctx.lineCap <- "round"
    ctx.beginPath ()
    ctx.moveTo (l.Left + 40.0, l.Top + 54.0)

    for i in 1..12 do
        let x = l.Left + 40.0 + float i * (l.Right - l.Left - 80.0) / 12.0
        ctx.lineTo (x, l.Top + 54.0 + (if i % 2 = 0 then -8.0 else 8.0))

    ctx.stroke ()
    ctx.restore ()

// ---------------------------------------------------------------------------
// Ebenen-Cache: Statische Grafik wird einmal in Offscreen-Canvases gezeichnet und danach nur noch
// kopiert. Pro Frame bleiben so nur Bewegtes, Glüh-Effekte, Kämpfer, Partikel und HUD zu zeichnen.
// ---------------------------------------------------------------------------

type private Layer =
    { Canvas: HTMLCanvasElement
      X: float
      Y: float
      W: float
      H: float }

// ---------------------------------------------------------------------------
// Frostgipfel: eisiger Berggipfel im Polarlicht
// ---------------------------------------------------------------------------

let private frost = stageLayout FrostPeak 0

/// Weiche Bergkette aus Bézier-Hügeln statt harter Dreiecke
let private ridge (ctx: Ctx) (seed: float) (baseY: float) (amp: float) (count: int) (color: string) (snow: string option) =
    let step = (screenWidth + 200.0) / float count
    let peaks = [ for i in 0..count -> -100.0 + float i * step, baseY - amp * (0.45 + hash i seed * 0.55) ]
    ctx.beginPath ()
    ctx.moveTo (-100.0, screenHeight)
    ctx.lineTo (-100.0, baseY)

    peaks
    |> List.pairwise
    |> List.iteri (fun i ((x0, y0), (x1, y1)) ->
        let mx = (x0 + x1) / 2.0
        let valley = baseY - amp * 0.15 * hash i (seed + 3.0)
        ctx.quadraticCurveTo (x0 + step * 0.25, y0 - 6.0, mx, valley)
        ctx.quadraticCurveTo (x1 - step * 0.25, y1 - 6.0, x1, y1))

    ctx.lineTo (screenWidth + 100.0, screenHeight)
    ctx.closePath ()

    match snow with
    | Some c ->
        // Verschneite Gipfel: weicher Verlauf von Schneeweiß in den Fels
        let g = ctx.createLinearGradient (0.0, baseY - amp, 0.0, baseY + 40.0)
        g.addColorStop (0.0, c)
        g.addColorStop (0.35, color)
        g.addColorStop (1.0, color)
        ctx.fillStyle <- !^g
    | None -> fill ctx color

    ctx.fill ()

let private paintFrostBackdrop (ctx: Ctx) =
    let sky = ctx.createLinearGradient (0.0, 0.0, 0.0, screenHeight)
    sky.addColorStop (0.0, "#071233")
    sky.addColorStop (0.45, "#1d3f7a")
    sky.addColorStop (0.75, "#6f9fd0")
    sky.addColorStop (1.0, "#f3c6d8")
    ctx.fillStyle <- !^sky
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

    // Sterne
    for i in 0..70 do
        ctx.globalAlpha <- 0.3 + hash i 101.0 * 0.6
        fill ctx "#ffffff"
        circle ctx (hash i 102.0 * screenWidth) (hash i 103.0 * 300.0) (0.6 + hash i 104.0 * 1.1)
        ctx.fill ()

    ctx.globalAlpha <- 1.0

    // Mond mit weichem Hof
    let halo = ctx.createRadialGradient (1010.0, 120.0, 20.0, 1010.0, 120.0, 160.0)
    halo.addColorStop (0.0, "rgba(255,250,235,0.55)")
    halo.addColorStop (1.0, "rgba(255,250,235,0)")
    ctx.fillStyle <- !^halo
    ctx.fillRect (850.0, -40.0, 320.0, 320.0)
    fill ctx "#fbf6e6"
    circle ctx 1010.0 120.0 38.0
    ctx.fill ()
    fill ctx "rgba(200,205,220,0.5)"
    circle ctx 998.0 110.0 8.0
    ctx.fill ()
    circle ctx 1022.0 132.0 5.0
    ctx.fill ()

    ridge ctx 110.0 470.0 210.0 6 "#4c6fa8" (Some "rgba(235,245,255,0.75)")
    ridge ctx 120.0 540.0 170.0 8 "#365288" (Some "rgba(225,238,255,0.6)")

    // Nebelband im Tal
    let mist = ctx.createLinearGradient (0.0, 520.0, 0.0, 640.0)
    mist.addColorStop (0.0, "rgba(220,235,255,0)")
    mist.addColorStop (0.5, "rgba(220,235,255,0.45)")
    mist.addColorStop (1.0, "rgba(220,235,255,0.1)")
    ctx.fillStyle <- !^mist
    ctx.fillRect (0.0, 520.0, screenWidth, 120.0)
    ridge ctx 130.0 640.0 120.0 9 "#24396a" None

    // Tannen am Hang (weiche Silhouetten)
    for i in 0..26 do
        let x = hash i 131.0 * screenWidth
        let y = 610.0 + hash i 132.0 * 70.0
        let h = 30.0 + hash i 133.0 * 30.0
        fill ctx "#162a52"

        for k in 0..2 do
            let ty = y - h * float k * 0.3
            let tw = h * (0.42 - float k * 0.1)
            ctx.beginPath ()
            ctx.moveTo (x - tw, ty)
            ctx.quadraticCurveTo (x, ty - h * 0.55, x, ty - h * 0.6)
            ctx.quadraticCurveTo (x, ty - h * 0.55, x + tw, ty)
            ctx.closePath ()
            ctx.fill ()

/// Weiche Schneewehe (Bezier-Kuppe)
let private snowDrift (ctx: Ctx) (x1: float) (x2: float) (y: float) (h: float) =
    ctx.beginPath ()
    ctx.moveTo (x1, y)
    let w = x2 - x1
    ctx.bezierCurveTo (x1 + w * 0.15, y - h, x1 + w * 0.35, y - h * 0.8, x1 + w * 0.5, y - h * 0.6)
    ctx.bezierCurveTo (x1 + w * 0.65, y - h * 0.9, x1 + w * 0.85, y - h, x2, y)
    ctx.closePath ()

let private paintIceSlab (ctx: Ctx) (pl: Platform) =
    let w = pl.X2 - pl.X1
    ctx.beginPath ()
    ctx.moveTo (pl.X1 - 4.0, pl.Y)
    ctx.lineTo (pl.X2 + 4.0, pl.Y)
    ctx.quadraticCurveTo (pl.X2 + 2.0, pl.Y + 14.0, pl.X2 - 18.0, pl.Y + 18.0)

    // Zackige Eiszapfen an der Unterseite, aber mit runden Spitzen
    for k in 0..6 do
        let x = pl.X2 - 18.0 - (w - 36.0) * float k / 6.0
        let len = 10.0 + hash (int pl.X1 + k) 141.0 * 18.0
        ctx.quadraticCurveTo (x + 6.0, pl.Y + 18.0 + len, x, pl.Y + 18.0)

    ctx.quadraticCurveTo (pl.X1 - 2.0, pl.Y + 14.0, pl.X1 - 4.0, pl.Y)
    ctx.closePath ()
    stroke ctx "#10284a"
    ctx.lineWidth <- 3.5
    ctx.lineJoin <- "round"
    ctx.stroke ()
    let ice = ctx.createLinearGradient (0.0, pl.Y, 0.0, pl.Y + 36.0)
    ice.addColorStop (0.0, "#f4fdff")
    ice.addColorStop (0.3, "#a8e4ff")
    ice.addColorStop (1.0, "rgba(90,170,230,0.75)")
    ctx.fillStyle <- !^ice
    ctx.fill ()
    fill ctx "#ffffff"
    snowDrift ctx (pl.X1 - 2.0) (pl.X2 + 2.0) (pl.Y + 2.0) 8.0
    ctx.fill ()

let private paintFrostStage (ctx: Ctx) =
    let l = frost
    let w = l.Right - l.Left
    let cx = (l.Left + l.Right) / 2.0

    // Felsgipfel: organische Kontur bis zum unteren Bildrand
    ctx.beginPath ()
    ctx.moveTo (l.Left - 14.0, l.Top + 4.0)
    ctx.lineTo (l.Right + 14.0, l.Top + 4.0)
    ctx.bezierCurveTo (l.Right + 10.0, l.Top + 60.0, l.Right - 30.0, l.Top + 90.0, l.Right - 50.0, l.Top + 140.0)
    ctx.bezierCurveTo (l.Right - 70.0, l.Top + 190.0, cx + 120.0, screenHeight - 20.0, cx + 70.0, screenHeight + 20.0)
    ctx.lineTo (cx - 90.0, screenHeight + 20.0)
    ctx.bezierCurveTo (cx - 140.0, screenHeight - 40.0, l.Left + 60.0, l.Top + 180.0, l.Left + 40.0, l.Top + 130.0)
    ctx.bezierCurveTo (l.Left + 20.0, l.Top + 90.0, l.Left - 12.0, l.Top + 50.0, l.Left - 14.0, l.Top + 4.0)
    ctx.closePath ()
    stroke ctx "#0c1a33"
    ctx.lineWidth <- 6.0
    ctx.lineJoin <- "round"
    ctx.stroke ()
    let rock = ctx.createLinearGradient (l.Left, l.Top, l.Right, screenHeight)
    rock.addColorStop (0.0, "#7d8fb5")
    rock.addColorStop (0.5, "#4a5b86")
    rock.addColorStop (1.0, "#253457")
    ctx.fillStyle <- !^rock
    ctx.fill ()

    // Weiche Felsschichten und Eisadern
    ctx.save ()
    ctx.clip ()

    for i in 0..7 do
        let y = l.Top + 40.0 + float i * 30.0
        stroke ctx (if i % 2 = 0 then "rgba(20,30,60,0.25)" else "rgba(190,220,255,0.18)")
        ctx.lineWidth <- 6.0 + hash i 151.0 * 6.0
        ctx.beginPath ()
        ctx.moveTo (l.Left - 20.0, y)
        ctx.bezierCurveTo (l.Left + w * 0.3, y + 20.0 * hash i 152.0, l.Left + w * 0.6, y - 18.0 * hash i 153.0, l.Right + 20.0, y + 8.0)
        ctx.stroke ()

    let light = ctx.createRadialGradient (cx - 120.0, l.Top + 40.0, 10.0, cx - 120.0, l.Top + 40.0, 360.0)
    light.addColorStop (0.0, "rgba(255,240,250,0.3)")
    light.addColorStop (1.0, "rgba(255,240,250,0)")
    ctx.fillStyle <- !^light
    ctx.fillRect (l.Left - 40.0, l.Top, w + 80.0, screenHeight - l.Top)
    ctx.restore ()

    // Schneedecke mit weichen Wehen und Zapfen an der Kante
    ctx.beginPath ()
    ctx.moveTo (l.Left - 16.0, l.Top)
    ctx.lineTo (l.Right + 16.0, l.Top)
    ctx.quadraticCurveTo (l.Right + 18.0, l.Top + 14.0, l.Right + 4.0, l.Top + 16.0)

    for k in 0..18 do
        let x = l.Right + 4.0 - (w + 8.0) * float k / 18.0
        let sag = 4.0 + hash k 161.0 * 10.0
        ctx.quadraticCurveTo (x - (w / 36.0), l.Top + 16.0 + sag, x - (w / 18.0), l.Top + 16.0)

    ctx.quadraticCurveTo (l.Left - 18.0, l.Top + 14.0, l.Left - 16.0, l.Top)
    ctx.closePath ()
    stroke ctx "#0c1a33"
    ctx.lineWidth <- 4.0
    ctx.stroke ()
    let snow = ctx.createLinearGradient (0.0, l.Top, 0.0, l.Top + 26.0)
    snow.addColorStop (0.0, "#ffffff")
    snow.addColorStop (1.0, "#cfe4ff")
    ctx.fillStyle <- !^snow
    ctx.fill ()

    for (x1, x2, h) in [ (l.Left - 10.0, l.Left + 120.0, 10.0); (cx - 90.0, cx + 60.0, 6.0); (l.Right - 140.0, l.Right + 10.0, 12.0) ] do
        fill ctx "#ffffff"
        snowDrift ctx x1 x2 (l.Top + 1.0) h
        ctx.fill ()

    // Hängende Eiszapfen an den Kanten
    for side in [ -1.0; 1.0 ] do
        let x0 = if side < 0.0 then l.Left - 8.0 else l.Right + 8.0

        for k in 0..3 do
            let x = x0 - side * float k * 12.0
            let len = 18.0 + float ((k * 7) % 4) * 7.0
            ctx.beginPath ()
            ctx.moveTo (x - 4.0, l.Top + 14.0)
            ctx.quadraticCurveTo (x, l.Top + 14.0 + len * 1.1, x + 4.0, l.Top + 14.0)
            ctx.closePath ()
            fill ctx "rgba(200,240,255,0.9)"
            ctx.fill ()

    frost.Soft |> List.iter (paintIceSlab ctx)

let private paintFrostGlow (ctx: Ctx) =
    let l = frost
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"

    for i in 0..13 do
        let x = l.Left + 20.0 + hash i 171.0 * (l.Right - l.Left - 40.0)
        let y = l.Top + 6.0 + hash i 172.0 * 8.0
        fill ctx "rgba(200,240,255,0.9)"
        poly ctx [ (x, y - 4.0); (x + 1.2, y - 1.2); (x + 4.0, y); (x + 1.2, y + 1.2); (x, y + 4.0); (x - 1.2, y + 1.2); (x - 4.0, y); (x - 1.2, y - 1.2) ]
        ctx.fill ()

    ctx.restore ()

/// Polarlicht und Schneefall (animiert)
let private drawFrostAmbience (ctx: Ctx) (t: float) =
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"

    for band in 0..2 do
        let baseY = 90.0 + float band * 45.0
        let hue = [| "rgba(110,255,190,"; "rgba(120,200,255,"; "rgba(200,140,255," |][band]
        ctx.beginPath ()
        ctx.moveTo (-20.0, baseY)

        for i in 0..26 do
            let x = float i * 50.0
            ctx.lineTo (x, baseY + sin (t * 0.012 + float i * 0.45 + float band) * 26.0)

        for i in 26 .. -1 .. 0 do
            let x = float i * 50.0
            ctx.lineTo (x, baseY + 70.0 + sin (t * 0.015 + float i * 0.4 + float band * 2.0) * 22.0)

        ctx.closePath ()
        let aurora = ctx.createLinearGradient (0.0, baseY - 20.0, 0.0, baseY + 90.0)
        aurora.addColorStop (0.0, hue + "0)")
        aurora.addColorStop (0.5, hue + sprintf "%.2f)" (0.13 + 0.05 * sin (t * 0.02 + float band)))
        aurora.addColorStop (1.0, hue + "0)")
        ctx.fillStyle <- !^aurora
        ctx.fill ()

    ctx.restore ()

let private drawSnowfall (ctx: Ctx) (t: float) (front: bool) =
    fill ctx "rgba(255,255,255,0.9)"
    let n = if front then 26 else 60

    for i in 0..n do
        let seed = i + (if front then 500 else 0)
        let speed = (if front then 1.6 else 0.7) + hash seed 181.0 * 0.8
        let y = (t * speed + hash seed 182.0 * 760.0) % 760.0 - 20.0
        let x = (hash seed 183.0 * (screenWidth + 100.0) + sin (t * 0.015 + float i) * 30.0 - t * 0.25) % (screenWidth + 100.0)
        let x = if x < 0.0 then x + screenWidth + 100.0 else x
        ctx.globalAlpha <- (if front then 0.85 else 0.55)
        circle ctx (x - 50.0) y ((if front then 2.6 else 1.4) + hash seed 184.0 * 1.4)
        ctx.fill ()

    ctx.globalAlpha <- 1.0

// ---------------------------------------------------------------------------
// Neon-Dach: Hochhaus-Dach in einer Neon-Stadt bei Nacht
// ---------------------------------------------------------------------------

let private neon = stageLayout NeonRooftop 0

let private neonColors = [| "#ff4fd8"; "#4ff6ff"; "#ffe14f"; "#7d6bff"; "#ff6b4f" |]

let private paintNeonBackdrop (ctx: Ctx) =
    let sky = ctx.createLinearGradient (0.0, 0.0, 0.0, screenHeight)
    sky.addColorStop (0.0, "#0a0620")
    sky.addColorStop (0.5, "#2a0f4a")
    sky.addColorStop (0.8, "#6b1a5e")
    sky.addColorStop (1.0, "#ff6b8a")
    ctx.fillStyle <- !^sky
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

    // Großer Retro-Mond mit Streifen
    let moon = ctx.createLinearGradient (0.0, 60.0, 0.0, 300.0)
    moon.addColorStop (0.0, "#ffe9a8")
    moon.addColorStop (1.0, "#ff5fa2")
    ctx.fillStyle <- !^moon
    circle ctx 640.0 190.0 120.0
    ctx.fill ()
    fill ctx "#2a0f4a"

    for k in 0..5 do
        ctx.fillRect (510.0, 220.0 + float k * 16.0, 260.0, 2.0 + float k * 1.4)

    // Skyline in drei Tiefen, Fenster mit weichem Leuchten
    let skyline (seed: float) (baseY: float) (minH: float) (maxH: float) (color: string) (windows: float) =
        let mutable x = -20.0
        let mutable i = 0

        while x < screenWidth + 20.0 do
            let w = 50.0 + hash i seed * 70.0
            let h = minH + hash i (seed + 1.0) * (maxH - minH)
            let top = baseY - h
            fill ctx color
            roundRect ctx x top w (h + 200.0) 3.0
            ctx.fill ()

            if hash i (seed + 2.0) > 0.6 then
                // Antenne mit Blinklicht
                stroke ctx color
                ctx.lineWidth <- 2.0
                ctx.beginPath ()
                ctx.moveTo (x + w / 2.0, top)
                ctx.lineTo (x + w / 2.0, top - 26.0)
                ctx.stroke ()

            for wy in 0 .. int (h / 14.0) - 1 do
                for wx in 0 .. int (w / 12.0) - 1 do
                    let k = i * 97 + wy * 13 + wx

                    if hash k (seed + 3.0) < windows then
                        let c = if hash k (seed + 4.0) < 0.7 then "rgba(255,214,140,0.75)" else "rgba(140,230,255,0.7)"
                        fill ctx c
                        ctx.fillRect (x + 4.0 + float wx * 12.0, top + 8.0 + float wy * 14.0, 6.0, 7.0)

            x <- x + w + 4.0
            i <- i + 1

    skyline 201.0 520.0 120.0 300.0 "#24113f" 0.18
    // Dunst zwischen den Ebenen
    let haze = ctx.createLinearGradient (0.0, 380.0, 0.0, 620.0)
    haze.addColorStop (0.0, "rgba(255,90,170,0)")
    haze.addColorStop (1.0, "rgba(255,90,170,0.35)")
    ctx.fillStyle <- !^haze
    ctx.fillRect (0.0, 380.0, screenWidth, 240.0)
    skyline 211.0 640.0 100.0 260.0 "#170a2c" 0.26

    // Neon-Schilder
    let signs = [ (150.0, 330.0, "NEON"); (1000.0, 120.0, "24/7"); (300.0, 150.0, "ARENA"); (1150.0, 330.0, "SMASH") ]

    signs
    |> List.iteri (fun i (x, y, text) ->
        let c = neonColors[i % neonColors.Length]
        ctx.save ()
        ctx.shadowColor <- c
        ctx.shadowBlur <- 18.0
        stroke ctx c
        ctx.lineWidth <- 3.0
        roundRect ctx (x - 52.0) (y - 20.0) 104.0 40.0 8.0
        ctx.stroke ()
        ctx.font <- sprintf "italic 24px %s" displayFont
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        fill ctx c
        ctx.fillText (text, x, y + 1.0)
        ctx.restore ())

let private paintNeonStage (ctx: Ctx) =
    let l = neon
    let w = l.Right - l.Left

    // Gebäudefassade bis zum unteren Rand
    stroke ctx "#05030c"
    ctx.lineWidth <- 6.0
    roundRect ctx (l.Left + 14.0) (l.Top + 10.0) (w - 28.0) (screenHeight - l.Top + 20.0) 6.0
    ctx.stroke ()
    let wall = ctx.createLinearGradient (l.Left, 0.0, l.Right, 0.0)
    wall.addColorStop (0.0, "#1b1230")
    wall.addColorStop (0.5, "#2e2150")
    wall.addColorStop (1.0, "#160e28")
    ctx.fillStyle <- !^wall
    ctx.fill ()

    for row in 0..3 do
        for col in 0..10 do
            let k = row * 11 + col
            let x = l.Left + 40.0 + float col * (w - 80.0) / 11.0
            let y = l.Top + 40.0 + float row * 44.0
            let lit = hash k 221.0 < 0.55
            fill ctx (if lit then "rgba(255,200,120,0.8)" else "rgba(10,8,24,0.8)")
            roundRect ctx x y 34.0 26.0 3.0
            ctx.fill ()

    // Dachkante mit Brüstung
    stroke ctx "#05030c"
    ctx.lineWidth <- 5.0
    roundRect ctx (l.Left - 6.0) (l.Top - 2.0) (w + 12.0) 22.0 5.0
    ctx.stroke ()
    let slab = ctx.createLinearGradient (0.0, l.Top, 0.0, l.Top + 22.0)
    slab.addColorStop (0.0, "#d9d2ee")
    slab.addColorStop (0.4, "#8a80aa")
    slab.addColorStop (1.0, "#3d3560")
    ctx.fillStyle <- !^slab
    ctx.fill ()

    // Neonband entlang der Kante
    ctx.save ()
    ctx.shadowColor <- "#ff4fd8"
    ctx.shadowBlur <- 14.0
    stroke ctx "#ff8ae6"
    ctx.lineWidth <- 3.0
    ctx.beginPath ()
    ctx.moveTo (l.Left + 4.0, l.Top + 17.0)
    ctx.lineTo (l.Right - 4.0, l.Top + 17.0)
    ctx.stroke ()
    ctx.restore ()

    // Lüftungsanlage und Wassertank im Hintergrund des Dachs
    let box (x: float) (bw: float) (bh: float) =
        stroke ctx "#05030c"
        ctx.lineWidth <- 3.0
        roundRect ctx x (l.Top - bh) bw bh 4.0
        ctx.stroke ()
        let g = ctx.createLinearGradient (0.0, l.Top - bh, 0.0, l.Top)
        g.addColorStop (0.0, "#7a7398")
        g.addColorStop (1.0, "#3a3458")
        ctx.fillStyle <- !^g
        ctx.fill ()
        stroke ctx "rgba(255,255,255,0.18)"
        ctx.lineWidth <- 1.5

        for k in 1..3 do
            ctx.beginPath ()
            ctx.moveTo (x + 6.0, l.Top - bh + float k * bh / 4.0)
            ctx.lineTo (x + bw - 6.0, l.Top - bh + float k * bh / 4.0)
            ctx.stroke ()

    ctx.globalAlpha <- 0.55
    box (l.Left + 60.0) 70.0 34.0
    box (l.Right - 150.0) 90.0 26.0
    ctx.globalAlpha <- 1.0

    // Plattformen: Laufstege mit Neon-Unterseite
    for pl in neon.Soft do
        let pw = pl.X2 - pl.X1
        stroke ctx "#05030c"
        ctx.lineWidth <- 3.0
        roundRect ctx pl.X1 (pl.Y - 1.0) pw 10.0 3.0
        ctx.stroke ()
        let deck = ctx.createLinearGradient (0.0, pl.Y, 0.0, pl.Y + 10.0)
        deck.addColorStop (0.0, "#e8e2ff")
        deck.addColorStop (1.0, "#5a5180")
        ctx.fillStyle <- !^deck
        ctx.fill ()
        ctx.save ()
        ctx.shadowColor <- "#4ff6ff"
        ctx.shadowBlur <- 12.0
        stroke ctx "#8afbff"
        ctx.lineWidth <- 2.5
        ctx.beginPath ()
        ctx.moveTo (pl.X1 + 6.0, pl.Y + 11.0)
        ctx.lineTo (pl.X2 - 6.0, pl.Y + 11.0)
        ctx.stroke ()
        ctx.restore ()

let private paintNeonGlow (ctx: Ctx) =
    let l = neon
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"
    let glow = ctx.createLinearGradient (0.0, l.Top + 10.0, 0.0, l.Top + 60.0)
    glow.addColorStop (0.0, "rgba(255,80,220,0.35)")
    glow.addColorStop (1.0, "rgba(255,80,220,0)")
    ctx.fillStyle <- !^glow
    ctx.fillRect (l.Left, l.Top + 10.0, l.Right - l.Left, 50.0)
    ctx.restore ()

/// Fliegende Autos, Suchscheinwerfer und Regen
let private drawNeonAmbience (ctx: Ctx) (t: float) =
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"

    for k in 0..1 do
        let a = sin (t * 0.006 + float k * 2.0) * 0.35 - 0.2 + float k * 0.4
        let ox = 300.0 + float k * 700.0
        let beam = ctx.createLinearGradient (ox, screenHeight, ox + sin a * 600.0, 0.0)
        beam.addColorStop (0.0, "rgba(180,220,255,0.18)")
        beam.addColorStop (1.0, "rgba(180,220,255,0)")
        ctx.fillStyle <- !^beam
        poly ctx [ (ox - 10.0, screenHeight); (ox + 10.0, screenHeight); (ox + sin a * 700.0 + 60.0, -40.0); (ox + sin a * 700.0 - 60.0, -40.0) ]
        ctx.fill ()

    for i in 0..4 do
        let speed = 1.5 + hash i 231.0 * 2.0
        let dir = if i % 2 = 0 then 1.0 else -1.0
        let x = (t * speed * 3.0 + hash i 232.0 * 1600.0) % 1600.0 - 160.0
        let x = if dir > 0.0 then x else screenWidth - x
        let y = 140.0 + hash i 233.0 * 220.0
        let c = neonColors[i % neonColors.Length]
        fill ctx c
        oval ctx x y 7.0 2.5
        ctx.fill ()
        let trail = ctx.createLinearGradient (x - dir * 70.0, y, x, y)
        trail.addColorStop (0.0, "rgba(255,255,255,0)")
        trail.addColorStop (1.0, "rgba(255,200,240,0.5)")
        ctx.fillStyle <- !^trail
        ctx.fillRect (min x (x - dir * 70.0), y - 1.0, 70.0, 2.0)

    ctx.restore ()
    // Feiner Regen
    stroke ctx "rgba(200,190,255,0.25)"
    ctx.lineWidth <- 1.0
    ctx.beginPath ()

    for i in 0..70 do
        let x = (hash i 241.0 * (screenWidth + 200.0) - t * 2.0) % (screenWidth + 200.0)
        let x = if x < 0.0 then x + screenWidth + 200.0 else x
        let y = (t * 14.0 + hash i 242.0 * 760.0) % 760.0 - 20.0
        ctx.moveTo (x - 100.0, y)
        ctx.lineTo (x - 104.0, y + 16.0)

    ctx.stroke ()

type private StageLayers =
    { Backdrop: Layer
      Far: Layer option
      Ruins: Layer option
      Mid: Layer option
      Body: Layer
      Glow: Layer
      Front: Layer option }

let private cacheScale = 2.0
let private cloudSpan = screenWidth + 400.0

let private makeLayer (x: float) (y: float) (w: float) (h: float) (paint: Ctx -> unit) : Layer =
    let canvas = document.createElement "canvas" :?> HTMLCanvasElement
    canvas.width <- w * cacheScale
    canvas.height <- h * cacheScale
    let layerCtx = canvas.getContext_2d ()
    layerCtx.scale (cacheScale, cacheScale)
    layerCtx.translate (-x, -y)
    paint layerCtx
    { Canvas = canvas; X = x; Y = y; W = w; H = h }

let private blit (ctx: Ctx) (layer: Layer) (dx: float) (dy: float) =
    ctx.drawImage (!^layer.Canvas, layer.X + dx, layer.Y + dy, layer.W, layer.H)

let mutable private layerCache: (StageId * StageLayers) list = []

let private buildLayers (s: StageId) : StageLayers =
    let strip (top: float) (bottom: float) (paint: Ctx -> unit) =
        makeLayer -320.0 top (cloudSpan + 440.0) (bottom - top) paint

    let full paint = makeLayer 0.0 0.0 screenWidth screenHeight paint

    match s with
    | SkyBattlefield ->
        { Backdrop = full paintBackdrop
          Far = Some(strip 450.0 690.0 paintFarClouds)
          Ruins = Some(makeLayer 60.0 120.0 1160.0 340.0 paintRuins)
          Mid = Some(strip 530.0 730.0 paintMidClouds)
          Body = makeLayer (mainLeft - 40.0) 226.0 (mainRight - mainLeft + 80.0) 540.0 paintStage
          Glow = makeLayer (mainLeft + 20.0) (mainTop + 28.0) (mainRight - mainLeft - 40.0) 56.0 paintRunes
          Front = Some(strip 660.0 760.0 paintFrontClouds) }
    | AstralAltar ->
        { Backdrop = full paintAstralBackdrop
          Far = None
          Ruins = None
          Mid = None
          Body = makeLayer (astral.Left - 30.0) (astral.Top - 20.0) (astral.Right - astral.Left + 60.0) 260.0 paintAstralStage
          Glow = makeLayer (astral.Left) (astral.Top + 20.0) (astral.Right - astral.Left) 90.0 paintAstralGlow
          Front = None }
    | MagmaFoundry ->
        { Backdrop = full paintMagmaBackdrop
          Far = None
          Ruins = None
          Mid = None
          Body = makeLayer (magma.Left - 30.0) (magma.Top - 20.0) (magma.Right - magma.Left + 60.0) (screenHeight - magma.Top + 20.0) paintMagmaStage
          Glow = makeLayer (magma.Left) (magma.Top + 30.0) (magma.Right - magma.Left) 50.0 paintMagmaGlow
          Front = None }
    | FrostPeak ->
        { Backdrop = full paintFrostBackdrop
          Far = None
          Ruins = None
          Mid = None
          Body = makeLayer 200.0 (frost.Top - 230.0) 880.0 (screenHeight - frost.Top + 260.0) paintFrostStage
          Glow = makeLayer frost.Left (frost.Top - 6.0) (frost.Right - frost.Left) 24.0 paintFrostGlow
          Front = None }
    | NeonRooftop ->
        { Backdrop = full paintNeonBackdrop
          Far = None
          Ruins = None
          Mid = None
          Body = makeLayer (neon.Left - 20.0) (neon.Top - 140.0) (neon.Right - neon.Left + 40.0) (screenHeight - neon.Top + 160.0) paintNeonStage
          Glow = makeLayer neon.Left (neon.Top + 10.0) (neon.Right - neon.Left) 50.0 paintNeonGlow
          Front = None }

let private stageLayers (s: StageId) : StageLayers =
    match layerCache |> List.tryFind (fun (id, _) -> id = s) with
    | Some(_, built) -> built
    | None ->
        let built = buildLayers s
        layerCache <- (s, built) :: layerCache
        built

/// Endlos ziehende Wolkenbank: zwei versetzte Kopien desselben Streifens
let private scrollClouds (ctx: Ctx) (layer: Layer) (t: float) (speed: float) =
    let offset = (t * speed) % cloudSpan
    blit ctx layer offset 0.0
    blit ctx layer (offset - cloudSpan) 0.0

/// Lavasee unter der Schmiede (wellig, pulsierend)
let private drawLava (ctx: Ctx) (t: float) =
    let top = 650.0
    let lava = ctx.createLinearGradient (0.0, top - 20.0, 0.0, screenHeight)
    lava.addColorStop (0.0, "#ffd36a")
    lava.addColorStop (0.25, "#ff7a1a")
    lava.addColorStop (1.0, "#a3200c")
    ctx.fillStyle <- !^lava
    ctx.beginPath ()
    ctx.moveTo (0.0, screenHeight)

    for i in 0..32 do
        let x = float i * 40.0
        ctx.lineTo (x, top + sin (t * 0.04 + float i * 0.7) * 6.0 + sin (t * 0.023 + float i * 1.9) * 4.0)

    ctx.lineTo (screenWidth, screenHeight)
    ctx.closePath ()
    ctx.fill ()
    // Krusten
    fill ctx "rgba(90,20,10,0.55)"

    for i in 0..10 do
        let x = ((hash i 61.0 * screenWidth) + t * 0.3) % screenWidth
        oval ctx x (top + 22.0 + hash i 62.0 * 30.0) (30.0 + hash i 63.0 * 30.0) 5.0
        ctx.fill ()

let private drawBackground (ctx: Ctx) (frame: int) (s: StageId) =
    let t = float frame
    let l = stageLayers s
    blit ctx l.Backdrop 0.0 0.0

    match s with
    | SkyBattlefield ->
        l.Far |> Option.iter (fun layer -> scrollClouds ctx layer t 0.05)
        l.Ruins |> Option.iter (fun layer -> blit ctx layer 0.0 (sin (t * 0.012) * 4.0))
        l.Mid |> Option.iter (fun layer -> scrollClouds ctx layer t 0.14)
    | AstralAltar ->
        // Funkelnde Sterne und Sternschnuppen
        for i in 0..24 do
            ctx.globalAlpha <- clamp01 (0.5 + 0.5 * sin (t * 0.07 + float i * 1.7))
            fill ctx "#ffffff"
            circle ctx (hash i 71.0 * screenWidth) (hash i 72.0 * 480.0) 1.8
            ctx.fill ()

        ctx.globalAlpha <- 1.0

        for k in 0..1 do
            let period = 150.0 + float k * 70.0
            let phase = (t + float k * 60.0) % period
            let seed = int ((t + float k * 60.0) / period) + k * 17

            if phase < 34.0 then
                let sx = 200.0 + hash seed 73.0 * 900.0
                let sy = 40.0 + hash seed 74.0 * 160.0
                let p = phase / 34.0
                let x = sx + p * 380.0
                let y = sy + p * 170.0
                ctx.save ()
                ctx.globalCompositeOperation <- "lighter"
                let trail = ctx.createLinearGradient (x - 140.0, y - 62.0, x, y)
                trail.addColorStop (0.0, "rgba(255,255,255,0)")
                trail.addColorStop (1.0, sprintf "rgba(255,255,255,%.2f)" (0.9 * (1.0 - p)))
                ctx.strokeStyle <- !^trail
                ctx.lineWidth <- 2.5
                ctx.lineCap <- "round"
                ctx.beginPath ()
                ctx.moveTo (x - 140.0, y - 62.0)
                ctx.lineTo (x, y)
                ctx.stroke ()
                ctx.restore ()

        // Energie-Säulen mit aufsteigenden Lichtpunkten
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"

        for i in 0..3 do
            let x = [| 150.0; 400.0; 880.0; 1130.0 |][i]
            let top = 220.0 + float (i % 2) * 70.0
            let pulse = 0.55 + 0.45 * sin (t * 0.05 + float i * 1.3)
            let beam = ctx.createLinearGradient (x - 30.0, 0.0, x + 30.0, 0.0)
            beam.addColorStop (0.0, "rgba(120,240,255,0)")
            beam.addColorStop (0.5, sprintf "rgba(150,245,255,%.2f)" (0.35 * pulse))
            beam.addColorStop (1.0, "rgba(120,240,255,0)")
            ctx.fillStyle <- !^beam
            ctx.fillRect (x - 30.0, 0.0, 60.0, top)

            for k in 0..5 do
                let y = top - ((t * 1.6 + float k * 40.0 + float i * 13.0) % top)
                fill ctx "rgba(200,250,255,0.8)"
                circle ctx (x + sin (y * 0.05 + float k) * 6.0) y 1.8
                ctx.fill ()

        ctx.restore ()
    | MagmaFoundry ->
        drawLava ctx t

        // Aufsteigende Glut
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"

        for i in 0..44 do
            let speed = 0.6 + hash i 81.0 * 1.4
            let y = screenHeight - ((t * speed + hash i 82.0 * 800.0) % 800.0)
            let x = hash i 83.0 * screenWidth + sin (t * 0.02 + float i) * 18.0
            ctx.globalAlpha <- clamp01 (y / screenHeight) * 0.9
            fill ctx (if i % 3 = 0 then "#ffe27a" else "#ff8a2e")
            circle ctx x y (1.2 + hash i 84.0 * 2.2)
            ctx.fill ()

        ctx.restore ()
    | FrostPeak ->
        drawFrostAmbience ctx t
        drawSnowfall ctx t false
    | NeonRooftop -> drawNeonAmbience ctx t

/// Bewegliche Schmiede-Plattformen (an Ketten bzw. auf einer Schiene)
let private drawMagmaPlatforms (ctx: Ctx) (frame: int) (l: Layout) =
    l.Soft
    |> List.iteri (fun i pl ->
        let cx = (pl.X1 + pl.X2) / 2.0
        let w = pl.X2 - pl.X1

        // Aufhängung
        stroke ctx "#2a1c1e"
        ctx.lineWidth <- 4.0

        if i = 0 then
            for x in [ pl.X1 + 18.0; pl.X2 - 18.0 ] do
                ctx.beginPath ()
                ctx.moveTo (x, 0.0)
                ctx.lineTo (x, pl.Y)
                ctx.stroke ()

            stroke ctx "#6a5a5e"
            ctx.lineWidth <- 2.0

            for x in [ pl.X1 + 18.0; pl.X2 - 18.0 ] do
                for k in 0..int (pl.Y / 14.0) do
                    let y = float k * 14.0
                    oval ctx x (y + 7.0) 3.0 6.0
                    ctx.stroke ()
        else
            fill ctx "#2a1c1e"
            ctx.fillRect (0.0, pl.Y - 64.0, screenWidth, 8.0)
            fill ctx "#6a5a5e"
            ctx.fillRect (0.0, pl.Y - 64.0, screenWidth, 2.5)
            stroke ctx "#2a1c1e"
            ctx.lineWidth <- 4.0

            for x in [ pl.X1 + 22.0; pl.X2 - 22.0 ] do
                ctx.beginPath ()
                ctx.moveTo (x, pl.Y - 58.0)
                ctx.lineTo (x, pl.Y)
                ctx.stroke ()

            fill ctx "#4a3c3e"
            roundRect ctx (cx - 30.0) (pl.Y - 70.0) 60.0 16.0 4.0
            ctx.fill ()

        // Glühende Unterseite
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let glow = ctx.createLinearGradient (0.0, pl.Y + 6.0, 0.0, pl.Y + 40.0)
        glow.addColorStop (0.0, sprintf "rgba(255,140,40,%.2f)" (0.45 + 0.2 * sin (float frame * 0.1 + float i)))
        glow.addColorStop (1.0, "rgba(255,90,20,0)")
        ctx.fillStyle <- !^glow
        poly ctx [ (pl.X1 + 6.0, pl.Y + 8.0); (pl.X2 - 6.0, pl.Y + 8.0); (cx + w * 0.3, pl.Y + 40.0); (cx - w * 0.3, pl.Y + 40.0) ]
        ctx.fill ()
        ctx.restore ()

        // Gitterplatte
        poly ctx [ (pl.X1 - 4.0, pl.Y); (pl.X2 + 4.0, pl.Y); (pl.X2 - 6.0, pl.Y + 12.0); (pl.X1 + 6.0, pl.Y + 12.0) ]
        stroke ctx "#0c0405"
        ctx.lineWidth <- 4.0
        ctx.lineJoin <- "round"
        ctx.stroke ()
        let plate = ctx.createLinearGradient (0.0, pl.Y, 0.0, pl.Y + 12.0)
        plate.addColorStop (0.0, "#d8ccc6")
        plate.addColorStop (1.0, "#4a3c3e")
        ctx.fillStyle <- !^plate
        ctx.fill ()
        stroke ctx "rgba(255,160,60,0.8)"
        ctx.lineWidth <- 1.5

        for k in 1..6 do
            let x = pl.X1 + w * float k / 7.0
            ctx.beginPath ()
            ctx.moveTo (x, pl.Y + 3.0)
            ctx.lineTo (x - 3.0, pl.Y + 10.0)
            ctx.stroke ())

let private ledgeGlows (ctx: Ctx) (t: float) (l: Layout) =
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"
    ctx.globalAlpha <- 0.5 + 0.35 * sin (t * 0.09)

    for cx in [ l.Left - 8.0; l.Right + 8.0 ] do
        let glow = ctx.createRadialGradient (cx, l.Top + 4.0, 1.0, cx, l.Top + 4.0, 16.0)
        glow.addColorStop (0.0, "rgba(255,246,190,0.9)")
        glow.addColorStop (1.0, "rgba(255,246,190,0)")
        ctx.fillStyle <- !^glow
        ctx.fillRect (cx - 16.0, l.Top - 12.0, 32.0, 32.0)

    ctx.restore ()

let private drawStage (ctx: Ctx) (frame: int) (layout: Layout) =
    let t = float frame
    let l = stageLayers layout.Stage

    match layout.Stage with
    | MagmaFoundry -> drawMagmaPlatforms ctx frame layout
    | _ -> ()

    blit ctx l.Body 0.0 0.0
    ctx.globalAlpha <- 0.6 + 0.4 * sin (t * 0.05)
    blit ctx l.Glow 0.0 0.0
    ctx.globalAlpha <- 1.0
    ledgeGlows ctx t layout

    if layout.Stage = SkyBattlefield then
        // Wandernder Glanz auf den Energie-Plattformen
        softPlatforms
        |> List.iteri (fun i pl ->
            let w = pl.X2 - pl.X1
            let shine = pl.X1 + ((t * 1.4 + float i * 70.0) % (w + 120.0)) - 60.0

            if shine > pl.X1 && shine < pl.X2 - 22.0 then
                fill ctx "rgba(255,255,255,0.75)"
                poly ctx [ (shine, pl.Y + 1.0); (shine + 22.0, pl.Y + 1.0); (shine + 16.0, pl.Y + 9.0); (shine - 6.0, pl.Y + 9.0) ]
                ctx.fill ())

let private drawForeground (ctx: Ctx) (frame: int) (s: StageId) =
    match (stageLayers s).Front with
    | Some front ->
        ctx.globalAlpha <- 0.9
        scrollClouds ctx front (float frame) 0.3
        ctx.globalAlpha <- 1.0
    | None -> ()

    if s = FrostPeak then
        drawSnowfall ctx (float frame) true

    if s = MagmaFoundry then
        // Hitzeflimmern über der Lava
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let heat = ctx.createLinearGradient (0.0, 640.0, 0.0, screenHeight)
        heat.addColorStop (0.0, "rgba(255,120,40,0)")
        heat.addColorStop (1.0, sprintf "rgba(255,120,40,%.2f)" (0.18 + 0.06 * sin (float frame * 0.05)))
        ctx.fillStyle <- !^heat
        ctx.fillRect (0.0, 640.0, screenWidth, 80.0)
        ctx.restore ()

/// Dynamischer Bodenschatten: auf die nächste Plattform unter dem Kämpfer projiziert
let private drawGroundShadow (ctx: Ctx) (layout: Layout) (p: Player) =
    let below =
        allPlatforms layout
        |> List.filter (fun pl -> p.Pos.X >= pl.X1 - 12.0 && p.Pos.X <= pl.X2 + 12.0 && pl.Y >= p.Pos.Y - 2.0)
        |> List.sortBy (fun pl -> pl.Y)
        |> List.tryHead

    match below with
    | Some pl ->
        let height = pl.Y - p.Pos.Y
        let k = clamp01 (1.0 - height / 420.0)
        let rx = (12.0 + 14.0 * k) * (fighterStats p.Fighter).Width / 34.0
        let left = max (pl.X1 - 8.0) (p.Pos.X - rx)
        let right = min (pl.X2 + 8.0) (p.Pos.X + rx)

        if right > left then
            fill ctx (sprintf "rgba(10,12,40,%.2f)" (0.12 + 0.3 * k))
            oval ctx ((left + right) / 2.0) (pl.Y + 3.0) ((right - left) / 2.0) (2.5 + 2.5 * k)
            ctx.fill ()
    | None -> ()


// ---------------------------------------------------------------------------
// Kämpfer: Paletten, Posen, Skelett (2-Knochen-IK) und Cel-Shading-Zeichnung
// ---------------------------------------------------------------------------

type private Palette =
    { Outline: string
      Skin: string
      SkinShade: string
      Hair: string
      HairShade: string
      Armor: string
      ArmorShade: string
      Under: string
      UnderShade: string
      Trim: string
      Glow: string
      Cloth: string
      ClothShade: string
      Rim: string
      Eye: string }

let private basePalette (f: Fighter) : Palette =
    match f with
    | Aether ->
        { Outline = "#131a33"
          Skin = "#ffdcc0"
          SkinShade = "#e2a98c"
          Hair = "#f1f7ff"
          HairShade = "#a7c0ea"
          Armor = "#d5e0f2"
          ArmorShade = "#7e92bd"
          Under = "#2f62b8"
          UnderShade = "#1c3b7c"
          Trim = "#ffd166"
          Glow = "#7fe9ff"
          Cloth = "#2b5fd9"
          ClothShade = "#172f86"
          Rim = "rgba(255,255,255,0.85)"
          Eye = "#1aa3e8" }
    | Ignis ->
        { Outline = "#2a1012"
          Skin = "#f4bd8f"
          SkinShade = "#c9835a"
          Hair = "#ff6a2b"
          HairShade = "#b3261a"
          Armor = "#d5352c"
          ArmorShade = "#84191c"
          Under = "#33293a"
          UnderShade = "#1a1420"
          Trim = "#ffd166"
          Glow = "#ffae3d"
          Cloth = "#fff0c2"
          ClothShade = "#e8a53c"
          Rim = "rgba(255,236,200,0.85)"
          Eye = "#e8771a" }
    | Titan ->
        { Outline = "#1d1608"
          Skin = "#c98e5a"
          SkinShade = "#93603a"
          Hair = "#ff5b3a"
          HairShade = "#a8321c"
          Armor = "#ffc23d"
          ArmorShade = "#a86d16"
          Under = "#454b59"
          UnderShade = "#262a33"
          Trim = "#e8eef7"
          Glow = "#7dfcff"
          Cloth = "#ff5b3a"
          ClothShade = "#a8321c"
          Rim = "rgba(255,246,220,0.9)"
          Eye = "#7dfcff" }
    | Zephyr ->
        { Outline = "#170f2e"
          Skin = "#f2d0b6"
          SkinShade = "#c99a80"
          Hair = "#3a2f6e"
          HairShade = "#1d1640"
          Armor = "#4a3a94"
          ArmorShade = "#2a1f63"
          Under = "#22193f"
          UnderShade = "#120c26"
          Trim = "#4ff0d1"
          Glow = "#c9a2ff"
          Cloth = "#36d6c0"
          ClothShade = "#1b8f86"
          Rim = "rgba(225,205,255,0.85)"
          Eye = "#c9a2ff" }
    | Valkyrie ->
        { Outline = "#1a1630"
          Skin = "#ffe1c8"
          SkinShade = "#e0a98c"
          Hair = "#ffe08a"
          HairShade = "#c9952e"
          Armor = "#eef2fb"
          ArmorShade = "#9aa6c4"
          Under = "#3a6ea8"
          UnderShade = "#22457a"
          Trim = "#ffd166"
          Glow = "#fff3b0"
          Cloth = "#ffe08a"
          ClothShade = "#c9952e"
          Rim = "rgba(255,255,255,0.9)"
          Eye = "#3aa0e8" }
    | Chrono ->
        { Outline = "#14122e"
          Skin = "#f3d4bc"
          SkinShade = "#c99c84"
          Hair = "#dfe6f5"
          HairShade = "#9aa3c4"
          Armor = "#2f2a7a"
          ArmorShade = "#1a1650"
          Under = "#4fe0c8"
          UnderShade = "#2a9c8c"
          Trim = "#ffd166"
          Glow = "#4fe0c8"
          Cloth = "#3b3494"
          ClothShade = "#1d1a5a"
          Rim = "rgba(200,255,245,0.85)"
          Eye = "#ffb347" }
    | Bramble ->
        { Outline = "#1a2410"
          Skin = "#8a6a3a"
          SkinShade = "#5e4524"
          Hair = "#7ed957"
          HairShade = "#3d6b23"
          Armor = "#6b4a2a"
          ArmorShade = "#40291a"
          Under = "#a17c48"
          UnderShade = "#6e5230"
          Trim = "#efe3c8"
          Glow = "#c2f06a"
          Cloth = "#8a6a3a"
          ClothShade = "#5e4524"
          Rim = "rgba(230,255,200,0.8)"
          Eye = "#ffd23d" }
    | Ronin ->
        { Outline = "#1a0a10"
          Skin = "#f6d2b4"
          SkinShade = "#d39a7c"
          Hair = "#26222e"
          HairShade = "#0f0d14"
          Armor = "#2e2c3c"
          ArmorShade = "#17161f"
          Under = "#2b2d42"
          UnderShade = "#171827"
          Trim = "#e8c26a"
          Glow = "#ffd0d8"
          Cloth = "#e8203f"
          ClothShade = "#8a0d22"
          Rim = "rgba(255,230,235,0.85)"
          Eye = "#c21f3a" }
    | Lyra ->
        { Outline = "#151735"
          Skin = "#ffe2cf"
          SkinShade = "#e0ad94"
          Hair = "#eef0ff"
          HairShade = "#a7aee0"
          Armor = "#3d4280"
          ArmorShade = "#232657"
          Under = "#e7e9ff"
          UnderShade = "#9ea4d6"
          Trim = "#ffe27a"
          Glow = "#bfe0ff"
          Cloth = "#5a62b8"
          ClothShade = "#2f3478"
          Rim = "rgba(240,244,255,0.9)"
          Eye = "#6a7cff" }
    | Gorm ->
        { Outline = "#1e140b"
          Skin = "#b08458"
          SkinShade = "#7a5536"
          Hair = "#5f8f3a"
          HairShade = "#3a5a22"
          Armor = "#c99a66"
          ArmorShade = "#86603c"
          Under = "#7a5a3c"
          UnderShade = "#4f3926"
          Trim = "#ffb347"
          Glow = "#ffb347"
          Cloth = "#5f8f3a"
          ClothShade = "#3a5a22"
          Rim = "rgba(255,235,200,0.7)"
          Eye = "#ffb347" }
    | Specter ->
        { Outline = "#0d0714"
          Skin = "#cfc4dc"
          SkinShade = "#9a8aab"
          Hair = "#2a1638"
          HairShade = "#14091c"
          Armor = "#2a1638"
          ArmorShade = "#160b20"
          Under = "#3e2452"
          UnderShade = "#21122d"
          Trim = "#ff4f7b"
          Glow = "#ff4f7b"
          Cloth = "#2a1638"
          ClothShade = "#120818"
          Rim = "rgba(255,120,160,0.8)"
          Eye = "#ff4f7b" }

/// Alternativfarben für Spiegel-Matches (Spieler 2 mit demselben Kämpfer)
let private altPalette (f: Fighter) : Palette =
    let b = basePalette f

    match f with
    | Aether ->
        { b with
            Hair = "#2d2a36"
            HairShade = "#14121a"
            Armor = "#3a3546"
            ArmorShade = "#1e1a28"
            Under = "#b3243c"
            UnderShade = "#6e1424"
            Cloth = "#c42a43"
            ClothShade = "#701526"
            Glow = "#ff6f8c"
            Eye = "#e8304e" }
    | Ignis ->
        { b with
            Hair = "#3dc4ff"
            HairShade = "#1a5fb3"
            Armor = "#2a5fd5"
            ArmorShade = "#163380"
            Glow = "#7fe9ff"
            Eye = "#2aa3e8" }
    | Titan ->
        { b with
            Armor = "#8fd16a"
            ArmorShade = "#3f7a2c"
            Hair = "#ffd23d"
            HairShade = "#a8861c"
            Glow = "#ff6b6b"
            Eye = "#ff6b6b" }
    | Zephyr ->
        { b with
            Hair = "#e9e4f5"
            HairShade = "#a99fc4"
            Armor = "#e6e1f2"
            ArmorShade = "#9c94b8"
            Cloth = "#e8304e"
            ClothShade = "#8a1528"
            Trim = "#e8304e"
            Glow = "#ff8fb1"
            Eye = "#e8304e" }
    | Valkyrie ->
        { b with
            Hair = "#e8eef7"
            HairShade = "#a7b0c8"
            Cloth = "#e8eef7"
            ClothShade = "#a7b0c8"
            Armor = "#3a2f4a"
            ArmorShade = "#1e1828"
            Under = "#a8233c"
            UnderShade = "#6e1424"
            Trim = "#c9a2ff"
            Glow = "#ffb3c6"
            Eye = "#c9a2ff" }
    | Chrono ->
        { b with
            Armor = "#7a2a4a"
            ArmorShade = "#4a1430"
            Under = "#ffb347"
            UnderShade = "#b8741c"
            Glow = "#ffb347"
            Cloth = "#8a2f55"
            ClothShade = "#4f1530"
            Eye = "#4fe0c8" }
    | Bramble ->
        { b with
            Skin = "#cfd3dc"
            SkinShade = "#8f95a6"
            Under = "#e6e9f0"
            UnderShade = "#a6abba"
            Cloth = "#cfd3dc"
            ClothShade = "#8f95a6"
            Hair = "#ff8fb1"
            HairShade = "#c2456e"
            Armor = "#5a4a6a"
            ArmorShade = "#352a40"
            Glow = "#ffb3c6"
            Eye = "#58c7ff" }
    | Ronin ->
        { b with
            Hair = "#ece8f2"
            HairShade = "#a8a2b8"
            Cloth = "#f2f2f8"
            ClothShade = "#9aa0b8"
            Under = "#3a1018"
            UnderShade = "#200810"
            Trim = "#58c7ff"
            Glow = "#bfe9ff"
            Eye = "#2f6bff" }
    | Lyra ->
        { b with
            Hair = "#2a2238"
            HairShade = "#140f1e"
            Armor = "#8a2a4a"
            ArmorShade = "#521630"
            Cloth = "#c2456e"
            ClothShade = "#7a1f42"
            Trim = "#ffd0e0"
            Glow = "#ff9ec2"
            Eye = "#e8407a" }
    | Gorm ->
        { b with
            Armor = "#8f98a8"
            ArmorShade = "#545c6a"
            Skin = "#7f8796"
            SkinShade = "#4f5664"
            Under = "#4a505e"
            UnderShade = "#2c313a"
            Trim = "#58c7ff"
            Glow = "#58c7ff"
            Eye = "#58c7ff"
            Hair = "#3d6b8a"
            HairShade = "#22405a" }
    | Specter ->
        { b with
            Armor = "#e9e4f5"
            ArmorShade = "#a99fc4"
            Hair = "#e9e4f5"
            HairShade = "#a99fc4"
            Under = "#c8c0dc"
            UnderShade = "#8e86a8"
            Cloth = "#e9e4f5"
            ClothShade = "#a99fc4"
            Trim = "#58c7ff"
            Glow = "#58c7ff"
            Eye = "#58c7ff" }

let private paletteFor (f: Fighter) (alt: bool) = if alt then altPalette f else basePalette f

/// Einfarbige Palette für Hit-Flash- und Impact-Frame-Silhouetten
let private solidPalette (c: string) : Palette =
    { Outline = c
      Skin = c
      SkinShade = c
      Hair = c
      HairShade = c
      Armor = c
      ArmorShade = c
      Under = c
      UnderShade = c
      Trim = c
      Glow = c
      Cloth = c
      ClothShade = c
      Rim = c
      Eye = c }

/// Farben der Spieler-Slots (Namensschilder, Cursor, HUD-Chips)
let private slotColor (p: Player) =
    if p.IsCpu then "#a8b0c2"
    elif p.Id = 0 then "#ff4d5e"
    else "#4d8bff"

type private Expression =
    | Calm
    | Fierce
    | Hurt
    | Closed

type private RenderMode =
    | Shaded
    | Silhouette of string

type private SwingStyle =
    | Blade
    | Fist
    | Kick
    | Cast

/// Bogen eines Angriffs im lokalen Raum (0 Grad = Blickrichtung, -90 Grad = oben)
type private Swing =
    { Move: MoveId
      From: float
      Angle: float
      Reach: float
      Alpha: float
      Power: bool
      Style: SwingStyle }

/// Start- und Endwinkel (Grad) sowie Reichweite, abgestimmt auf die Hitboxen aus Physics.moveData
let private swingData (m: MoveId) : float * float * float =
    match m with
    | Jab -> -55.0, 25.0, 54.0
    | ForwardTilt -> -100.0, 45.0, 70.0
    | UpTilt -> -200.0, -25.0, 68.0
    | DownTilt -> 135.0, 12.0, 66.0
    | ForwardSmash -> -175.0, 55.0, 86.0
    | DownSmash -> 205.0, -25.0, 66.0
    | NeutralAir -> -90.0, 270.0, 44.0
    | ForwardAir -> -115.0, 60.0, 68.0
    | UpAir -> -205.0, 15.0, 68.0
    | DownAir -> 50.0, 125.0, 66.0
    | AirSmash -> -165.0, 75.0, 76.0
    | UpSpecial -> -130.0, -55.0, 72.0
    | SideSpecial -> -60.0, 15.0, 64.0
    | DownSpecial -> 55.0, 120.0, 62.0

let private isPower (m: MoveId) =
    match m with
    | ForwardSmash
    | DownSmash
    | AirSmash
    | UpSpecial
    | SideSpecial -> true
    | _ -> false

let private swingStyle (f: Fighter) (m: MoveId) : SwingStyle =
    match f with
    | Aether
    | Ronin -> Blade
    | Ignis ->
        match m with
        | DownTilt
        | DownSmash
        | DownAir
        | UpAir
        | NeutralAir
        | DownSpecial -> Kick
        | _ -> Fist
    | Titan ->
        match m with
        | DownTilt
        | DownAir -> Kick
        | _ -> Fist
    | Zephyr ->
        match m with
        | NeutralAir
        | ForwardAir
        | UpAir
        | DownAir
        | DownTilt
        | DownSpecial -> Kick
        | ForwardSmash
        | AirSmash
        | SideSpecial
        | UpSpecial -> Cast
        | _ -> Blade
    | Valkyrie ->
        match m with
        | ForwardSmash
        | AirSmash -> Cast
        | _ -> Blade
    | Chrono ->
        match m with
        | ForwardSmash
        | AirSmash
        | SideSpecial
        | UpSpecial -> Cast
        | _ -> Blade
    | Bramble ->
        match m with
        | DownTilt
        | DownAir
        | UpAir
        | DownSpecial -> Kick
        | SideSpecial -> Cast
        | _ -> Fist
    | Lyra ->
        match m with
        | Jab
        | ForwardTilt
        | ForwardAir
        | UpAir
        | DownSmash
        | DownTilt
        | DownAir
        | UpSpecial
        | DownSpecial -> Kick
        | _ -> Cast
    | Gorm ->
        match m with
        | DownTilt
        | DownAir
        | DownSpecial -> Kick
        | _ -> Fist
    | Specter ->
        match m with
        | DownTilt
        | DownAir
        | UpAir
        | DownSpecial -> Kick
        | SideSpecial -> Cast
        | _ -> Blade

let private swingOf (p: Player) : Swing option =
    let s = fighterStats p.Fighter

    let build (m: MoveId) (progress: float) (alpha: float) (windUp: float) =
        let a0, a1, reach = swingData m
        let direction = if a1 >= a0 then 1.0 else -1.0
        let style = swingStyle p.Fighter m

        Some
            { Move = m
              From = rad a0
              Angle = rad (lerp a0 a1 progress - direction * windUp)
              Reach = reach * s.Reach / 1.15 / s.Scale * (if p.Fighter = Titan then 1.25 else 1.0)
              Alpha = (if style = Cast then 0.0 else alpha)
              Power = isPower m
              Style = style }

    match p.State with
    | AttackStartup m ->
        let k = float p.StateFrame / float (max 1 (Physics.moveData p.Fighter m).Startup)
        build m 0.0 0.0 (18.0 * clamp01 k)
    | AttackActive m ->
        let k = clamp01 (float (p.StateFrame + 1) / float (max 1 (Physics.moveData p.Fighter m).Active))
        build m (1.0 - (1.0 - k) * (1.0 - k)) 1.0 0.0
    | AttackRecovery m ->
        let k = float p.StateFrame / float (max 1 (Physics.moveData p.Fighter m).Recovery)
        build m 1.0 (clamp01 (1.0 - k * 2.6)) 0.0
    | _ -> None

/// Zielpose: Wo sollen Hüfte, Hände und Füße hin? Die Gelenke dazwischen löst die IK.
type private Pose =
    { Hip: Vec2
      Lean: float
      HeadTilt: float
      HandF: Vec2
      HandB: Vec2
      FootF: Vec2
      FootB: Vec2
      Blade: float
      ShowBlade: bool
      Spin: float
      Sx: float
      Sy: float }

let private spineLength = 19.0
let private chestOf (hip: Vec2) (lean: float) = hip ++ v (sin lean * spineLength) (-(cos lean) * spineLength)

let private idlePose (frame: int) (p: Player) : Pose =
    let t = float frame + float p.Id * 37.0
    let breath = sin (t * 0.08) * 0.8

    let neutral =
        { Hip = v 0.0 (-27.0 + breath)
          Lean = 0.1
          HeadTilt = -0.05
          HandF = v 15.0 (-33.0 + breath)
          HandB = v -7.0 (-30.0 + breath)
          FootF = v 10.0 0.0
          FootB = v -9.0 0.0
          Blade = rad -58.0 + sin (t * 0.08) * 0.04
          ShowBlade = false
          Spin = 0.0
          Sx = 1.0
          Sy = 1.0 }

    match p.Fighter with
    | Aether -> { neutral with ShowBlade = true }
    | Ignis ->
        let bounce = abs (sin (t * 0.13)) * 1.6

        { neutral with
            Hip = v 0.0 (-26.5 + bounce)
            Lean = 0.16
            HeadTilt = -0.1
            HandF = v 15.0 (-41.0 + bounce)
            HandB = v 7.0 (-35.0 + bounce)
            FootF = v 11.0 0.0
            FootB = v -8.0 0.0 }
    | Titan ->
        let heave = sin (t * 0.05) * 1.0

        { neutral with
            Hip = v 0.0 (-25.5 + heave)
            Lean = 0.14
            HeadTilt = 0.05
            HandF = v 17.0 (-27.0 + heave)
            HandB = v -12.0 (-26.0 + heave)
            FootF = v 12.0 0.0
            FootB = v -11.0 0.0 }
    | Zephyr ->
        let sway = sin (t * 0.1) * 1.2

        { neutral with
            Hip = v 0.0 (-23.0 + sway * 0.5)
            Lean = 0.32
            HeadTilt = -0.15
            HandF = v 17.0 (-33.0 + sway)
            HandB = v -11.0 (-25.0 - sway)
            FootF = v 13.0 0.0
            FootB = v -12.0 0.0
            Blade = rad 25.0 }
    | Valkyrie ->
        { neutral with
            Hip = v 0.0 (-27.5 + breath)
            Lean = 0.08
            HandF = v 14.0 (-30.0 + breath)
            HandB = v 2.0 (-36.0 + breath)
            FootF = v 11.0 0.0
            FootB = v -9.0 0.0
            Blade = rad -78.0 + sin (t * 0.08) * 0.03
            ShowBlade = true }
    | Chrono ->
        // Schwebt knapp über dem Boden
        let hover = sin (t * 0.06) * 2.5

        { neutral with
            Hip = v 0.0 (-33.0 + hover)
            Lean = 0.04
            HeadTilt = -0.02
            HandF = v 15.0 (-38.0 + hover)
            HandB = v -9.0 (-34.0 + hover)
            FootF = v 5.0 (-7.0 + hover)
            FootB = v -6.0 (-5.0 + hover)
            Blade = rad -96.0
            ShowBlade = true }
    | Bramble ->
        let heave = sin (t * 0.07) * 1.2

        { neutral with
            Hip = v 0.0 (-24.0 + heave)
            Lean = 0.38
            HeadTilt = 0.12
            HandF = v 18.0 (-22.0 + heave)
            HandB = v -5.0 (-21.0 + heave)
            FootF = v 13.0 0.0
            FootB = v -11.0 0.0 }
    | Ronin ->
        // Zweihändige Kampfhaltung, Klinge schräg nach vorn
        let sway = sin (t * 0.07) * 0.8

        { neutral with
            Hip = v -1.0 (-23.0 + sway)
            Lean = 0.2
            HeadTilt = 0.04
            HandF = v 12.0 (-29.0 + sway)
            HandB = v 7.0 (-27.0 + sway)
            FootF = v 15.0 0.0
            FootB = v -14.0 0.0
            Blade = rad -38.0
            ShowBlade = true }
    | Lyra ->
        // Leichtfüßig, Bogen locker in der vorderen Hand
        let bounce = abs (sin (t * 0.1)) * 1.4

        { neutral with
            Hip = v 0.0 (-26.0 + bounce)
            Lean = 0.1
            HandF = v 13.0 (-33.0 + bounce)
            HandB = v -8.0 (-30.0 + bounce)
            FootF = v 10.0 0.0
            FootB = v -10.0 -1.0
            Blade = rad -80.0
            ShowBlade = true }
    | Gorm ->
        // Breitbeinig, Fäuste tief und schwer
        let breath = sin (t * 0.05) * 1.2

        { neutral with
            Hip = v 0.0 (-22.0 + breath * 0.4)
            Lean = 0.16
            HandF = v 18.0 (-18.0 + breath)
            HandB = v -14.0 (-17.0 + breath)
            FootF = v 15.0 0.0
            FootB = v -15.0 0.0
            ShowBlade = false }
    | Specter ->
        let sway = sin (t * 0.12) * 1.0

        { neutral with
            Hip = v 0.0 (-24.0 + sway)
            Lean = 0.36
            HeadTilt = -0.08
            HandF = v 16.0 (-30.0 + sway)
            HandB = v -12.0 (-28.0 - sway)
            FootF = v 13.0 0.0
            FootB = v -12.0 0.0
            Blade = rad 20.0
            ShowBlade = true }

let private poseOf (frame: int) (p: Player) (swing: Swing option) : Pose =
    let t = float frame + float p.Id * 37.0
    let sf = float p.StateFrame
    let idle = idlePose frame p
    let heavy = p.Fighter = Titan

    match p.State with
    | Running ->
        let ph = t * (if heavy then 0.32 else 0.45)
        let bob = abs (sin ph) * 2.0
        let stride = if p.Fighter = Zephyr then 16.0 elif heavy then 11.0 else 14.0

        { idle with
            Hip = v 1.0 (-25.0 - bob)
            Lean = (if p.Fighter = Zephyr then 0.62 elif heavy then 0.3 else 0.44)
            FootF = v (sin ph * stride) (-(max 0.0 (cos ph)) * 10.0)
            FootB = v (-(sin ph) * stride) (-(max 0.0 (-(cos ph))) * 10.0)
            HandF =
                (match p.Fighter with
                 | Aether -> v -11.0 (-27.0 - bob)
                 | Zephyr -> v -15.0 (-31.0 - bob)
                 | _ -> v (-(sin ph) * 13.0 + 3.0) (-34.0 - bob))
            HandB = (if p.Fighter = Zephyr then v -17.0 (-33.0 - bob) else v (sin ph * 12.0 - 2.0) (-33.0 - bob))
            Blade = rad 158.0 }
    | JumpSquat ->
        { idle with
            Hip = v 0.0 -17.0
            Lean = 0.32
            HandF = v 10.0 -16.0
            HandB = v -12.0 -18.0
            Blade = rad 150.0
            Sx = 1.12
            Sy = 0.88 }
    | Jumping ->
        let flip =
            // Zephyr dreht sich bei Mehrfachsprüngen ninjahaft um die eigene Achse
            if p.Fighter = Zephyr && p.JumpsLeft < (fighterStats Zephyr).AirJumps && sf < 18.0 then sf / 18.0 * Math.PI * 2.0
            else 0.0

        { idle with
            Hip = v 0.0 -29.0
            Lean = 0.06
            FootF = v 8.0 -11.0
            FootB = v -9.0 -2.0
            HandF = v 13.0 -47.0
            HandB = v -13.0 -40.0
            Blade = rad -105.0
            Spin = -flip
            Sx = 0.92
            Sy = 1.1 }
    | Falling ->
        let w = sin (t * 0.35) * 1.5

        { idle with
            Hip = v 0.0 -28.0
            Lean = -0.06
            FootF = v 6.0 (-3.0 + w)
            FootB = v -7.0 (-1.0 - w)
            HandF = v 17.0 (-49.0 + w)
            HandB = v -15.0 (-47.0 - w)
            Blade = rad -72.0
            Sx = 0.95
            Sy = 1.06 }
    | Helpless ->
        let w = sin (t * 0.5) * 2.0

        { idle with
            Hip = v 0.0 -28.0
            Lean = -0.12 + sin (t * 0.5) * 0.08
            FootF = v 5.0 (-2.0 + w)
            FootB = v -6.0 -1.0
            HandF = v 12.0 (-55.0 + w)
            HandB = v -12.0 (-54.0 - w)
            Blade = rad 100.0 }
    | Landing ->
        { idle with
            Hip = v 0.0 -17.0
            Lean = 0.36
            HandF = v 15.0 -16.0
            HandB = v -11.0 -15.0
            Blade = rad 18.0
            Sx = 1.14
            Sy = 0.86 }
    | Hitstun ->
        let w = sin (t * 0.9) * 3.0

        let spin =
            if p.OnGround then sin (t * 0.6) * 0.22
            else sf * (if heavy then 0.2 else 0.36) * (if p.Vel.X * p.Facing >= 0.0 then 1.0 else -1.0)

        { idle with
            Hip = v 0.0 -27.0
            Lean = -0.32
            HeadTilt = -0.25
            HandF = v 17.0 (-50.0 + w)
            HandB = v -17.0 (-46.0 - w)
            FootF = v 13.0 (-7.0 - w)
            FootB = v -11.0 -2.0
            Blade = rad 140.0
            Spin = spin }
    | Shielding ->
        { idle with
            Hip = v 0.0 -23.0
            Lean = 0.18
            HandF = v 13.0 -37.0
            HandB = v 10.0 -30.0
            Blade = rad -84.0 }
    | Grabbing ->
        // Ausgestreckte Hände greifen nach vorn
        let reach = if sf < 5.0 then sf / 5.0 else max 0.0 (1.0 - (sf - 9.0) / 18.0)

        { idle with
            Hip = v (4.0 * reach) -22.0
            Lean = 0.2 + 0.3 * reach
            HandF = v (12.0 + 22.0 * reach) -34.0
            HandB = v (8.0 + 18.0 * reach) -28.0
            FootF = v 12.0 0.0
            FootB = v -12.0 0.0
            Blade = rad 160.0
            ShowBlade = false }
    | Holding ->
        let pump = sin (t * 0.3) * 1.5

        { idle with
            Hip = v 0.0 -23.0
            Lean = 0.12
            HandF = v 22.0 (-38.0 + pump)
            HandB = v 18.0 (-30.0 - pump)
            FootF = v 12.0 0.0
            FootB = v -12.0 0.0
            Blade = rad 160.0
            ShowBlade = false }
    | Held ->
        let wiggle = sin (t * 0.8) * 3.0

        { idle with
            Hip = v 0.0 -27.0
            Lean = -0.22
            HeadTilt = -0.2
            HandF = v (10.0 + wiggle) -52.0
            HandB = v (-12.0 - wiggle) -50.0
            FootF = v 7.0 (-3.0 + wiggle)
            FootB = v -7.0 (-3.0 - wiggle) }
    | Dodging ->
        if p.OnGround && abs p.Vel.X > 0.5 then
            { idle with
                Hip = v 0.0 -15.0
                Lean = 0.7
                FootF = v 7.0 -5.0
                FootB = v -7.0 -5.0
                HandF = v 9.0 -17.0
                HandB = v -7.0 -17.0
                Blade = rad 170.0
                Spin = -sf * 0.55 }
        else
            { idle with Lean = -0.1; Sx = 0.88 }
    | LedgeHang ->
        let w = sin (t * 0.1) * 2.0

        { idle with
            Hip = v 1.0 -25.0
            Lean = -0.06
            HeadTilt = -0.3
            HandF = v 15.0 -55.0
            HandB = v 11.0 -54.0
            FootF = v 6.0 (-2.0 + w)
            FootB = v -3.0 (0.0 - w)
            ShowBlade = false }
    | AttackStartup m
    | AttackActive m
    | AttackRecovery m ->
        match swing with
        | None -> idle
        | Some sw ->
            let active =
                match p.State with
                | AttackActive _ -> true
                | _ -> false

            let low = m = DownTilt || m = DownSmash
            let upward = m = UpTilt || m = UpAir || m = UpSpecial

            let lean =
                if upward then -0.12
                elif m = NeutralAir || m = DownAir then 0.1
                elif m = SideSpecial && active && p.Fighter <> Aether && p.Fighter <> Zephyr then 0.62
                elif active then 0.42
                else 0.2

            let hip =
                if low then v 0.0 -20.0
                elif m = ForwardSmash && active then v 6.0 -25.0
                else v 0.0 -27.0

            let chest = chestOf hip lean
            let shoulder = chest ++ v 3.0 1.5
            let airborne = not p.OnGround

            let footF, footB =
                if airborne then v 7.0 -9.0, v -9.0 -3.0
                elif m = ForwardSmash then v 17.0 0.0, v -13.0 0.0
                else v 11.0 0.0, v -9.0 0.0

            let stretch =
                if (m = UpSpecial || m = DownSpecial) && active then 1.14
                elif m = DownAir && active then 1.1
                else 1.0

            let basePose =
                { idle with
                    Hip = hip
                    Lean = lean
                    HeadTilt = (if upward then -0.3 else 0.0)
                    FootF = footF
                    FootB = footB
                    Sx = 2.0 - stretch
                    Sy = stretch }

            match sw.Style with
            | Blade ->
                let hand = shoulder ++ polar sw.Angle 16.0

                { basePose with
                    HandF = hand
                    HandB = (if sw.Power then hand ++ polar sw.Angle -6.0 else chest ++ v -7.0 8.0)
                    Blade = sw.Angle
                    ShowBlade = p.Fighter <> Ignis && p.Fighter <> Titan && p.Fighter <> Bramble }
            | Kick ->
                { basePose with
                    Lean = (if m = UpAir then -0.5 elif low then 0.0 else -0.2)
                    HandF = chest ++ v 9.0 4.0
                    HandB = chest ++ v -8.0 6.0
                    FootF = (hip ++ v 3.5 0.0) ++ polar sw.Angle 27.0
                    FootB = (if airborne then v -7.0 -8.0 else v -6.0 0.0) }
            | Fist ->
                let reachF = if heavy then 20.0 else 22.0
                let both = sw.Power && heavy

                { basePose with
                    HandF = shoulder ++ polar sw.Angle reachF
                    HandB = (if both then shoulder ++ polar (sw.Angle + 0.25) (reachF - 3.0) else chest ++ v -9.0 4.0) }
            | Cast ->
                // Wurf- bzw. Zauberpose: Arm weit nach vorn, Hand offen
                let k =
                    match p.State with
                    | AttackStartup _ -> -0.9
                    | _ -> 0.0

                { basePose with
                    Lean = (if m = UpSpecial then 0.0 else 0.28)
                    HandF = shoulder ++ polar k 22.0
                    HandB = (if m = SideSpecial then shoulder ++ polar (k + 0.2) 19.0 else chest ++ v -10.0 3.0)
                    Blade = k
                    ShowBlade = false }
    | _ -> idle

/// 2-Knochen-IK: liefert Gelenk (Ellbogen/Knie) und das erreichbare Ende
let private solveLimb (root: Vec2) (target: Vec2) (l1: float) (l2: float) (bend: float) : Vec2 * Vec2 =
    let dx = target.X - root.X
    let dy = target.Y - root.Y
    let dist = max (abs (l1 - l2) + 0.1) (min (l1 + l2 - 0.1) (sqrt (dx * dx + dy * dy)))
    let baseAngle = atan2 dy dx
    let cosA = max -1.0 (min 1.0 ((l1 * l1 + dist * dist - l2 * l2) / (2.0 * l1 * dist)))
    let joint = root ++ polar (baseAngle + bend * acos cosA) l1
    joint, root ++ polar baseAngle dist

type private Rig =
    { Hip: Vec2
      Chest: Vec2
      Head: Vec2
      HeadAngle: float
      ShoulderF: Vec2
      ElbowF: Vec2
      HandF: Vec2
      ShoulderB: Vec2
      ElbowB: Vec2
      HandB: Vec2
      HipF: Vec2
      KneeF: Vec2
      FootF: Vec2
      HipB: Vec2
      KneeB: Vec2
      FootB: Vec2 }

let private buildRig (pose: Pose) : Rig =
    let chest = chestOf pose.Hip pose.Lean
    let headAngle = pose.Lean * 0.6 + pose.HeadTilt
    let head = chest ++ v (sin headAngle * 13.0) (-(cos headAngle) * 13.0)
    let shoulderF = chest ++ v 3.0 1.5
    let shoulderB = chest ++ v -4.0 1.5
    let hipF = pose.Hip ++ v 3.5 0.0
    let hipB = pose.Hip ++ v -3.5 0.0
    // Fußziele sind Sohlenpunkte, die Beine enden am Knöchel
    let ankle (foot: Vec2) = foot ++ v 0.0 -3.5
    let elbowF, handF = solveLimb shoulderF pose.HandF 11.5 11.5 1.0
    let elbowB, handB = solveLimb shoulderB pose.HandB 11.5 11.5 1.0
    let kneeF, footF = solveLimb hipF (ankle pose.FootF) 14.0 14.0 -1.0
    let kneeB, footB = solveLimb hipB (ankle pose.FootB) 14.0 14.0 -1.0

    { Hip = pose.Hip
      Chest = chest
      Head = head
      HeadAngle = headAngle
      ShoulderF = shoulderF
      ElbowF = elbowF
      HandF = handF
      ShoulderB = shoulderB
      ElbowB = elbowB
      HandB = handB
      HipF = hipF
      KneeF = kneeF
      FootF = footF
      HipB = hipB
      KneeB = kneeB
      FootB = footB }

/// Gliedmaßen-Segment: Outline, Grundfarbe, Cel-Schatten auf der Unterseite
let private bone (ctx: Ctx) (pal: Palette) (a: Vec2) (b: Vec2) (width: float) (color: string) (shade: string) =
    let line (ox: float) (oy: float) =
        ctx.beginPath ()
        ctx.moveTo (a.X + ox, a.Y + oy)
        ctx.lineTo (b.X + ox, b.Y + oy)
        ctx.stroke ()

    ctx.lineCap <- "round"
    stroke ctx pal.Outline
    ctx.lineWidth <- width + 3.6
    line 0.0 0.0
    stroke ctx color
    ctx.lineWidth <- width
    line 0.0 0.0
    let d = Vec2.normalize (v (b.X - a.X) (b.Y - a.Y))
    let n = if d.X >= 0.0 then v (-d.Y) d.X else v d.Y (-d.X)
    let side = if n.Y >= 0.0 then 1.0 else -1.0
    stroke ctx shade
    ctx.lineWidth <- width * 0.42
    line (n.X * side * width * 0.27 - 0.8) (n.Y * side * width * 0.27)

let private outlinedFill (ctx: Ctx) (pal: Palette) (color: string) (width: float) =
    ctx.lineJoin <- "round"
    stroke ctx pal.Outline
    ctx.lineWidth <- width
    ctx.stroke ()
    fill ctx color
    ctx.fill ()

/// Mandelauge im Anime-Stil (Blick nach +X)
let private drawEye (ctx: Ctx) (pal: Palette) (x: float) (y: float) (expr: Expression) (frame: int) (seed: int) =
    stroke ctx pal.Outline
    ctx.lineCap <- "round"
    ctx.lineWidth <- 1.8

    match expr with
    | Hurt ->
        ctx.beginPath ()
        ctx.moveTo (x - 1.6, y - 2.6)
        ctx.lineTo (x + 2.2, y + 0.2)
        ctx.lineTo (x - 1.6, y + 2.6)
        ctx.stroke ()
    | Closed ->
        ctx.beginPath ()
        ctx.moveTo (x - 2.0, y + 0.4)
        ctx.quadraticCurveTo (x + 0.4, y + 2.0, x + 2.8, y + 0.2)
        ctx.stroke ()
    | Calm
    | Fierce ->
        let blink = expr = Calm && (frame + seed * 53) % 200 < 6

        if blink then
            ctx.beginPath ()
            ctx.moveTo (x - 2.0, y + 0.4)
            ctx.lineTo (x + 2.6, y + 0.2)
            ctx.stroke ()
        else
            poly ctx [ (x - 2.4, y + 0.4); (x, y - 2.8); (x + 3.0, y - 1.6); (x + 2.8, y + 2.2); (x - 0.8, y + 2.8) ]
            outlinedFill ctx pal "#ffffff" 1.6
            fill ctx pal.Eye
            oval ctx (x + 1.2) (y + 0.2) 1.9 2.4
            ctx.fill ()
            fill ctx pal.Outline
            oval ctx (x + 1.6) (y + 0.3) 0.9 1.5
            ctx.fill ()
            fill ctx "#ffffff"
            circle ctx (x + 0.6) (y - 0.9) 0.8
            ctx.fill ()

let private drawBrow (ctx: Ctx) (pal: Palette) (expr: Expression) =
    stroke ctx pal.Outline
    ctx.lineWidth <- 2.0
    ctx.beginPath ()

    if expr = Fierce then
        ctx.moveTo (1.4, -5.2)
        ctx.lineTo (8.0, -2.4)
    else
        ctx.moveTo (1.6, -4.2)
        ctx.lineTo (7.8, -4.0)

    ctx.stroke ()

/// Kopf im Profil (Blick nach +X), Ursprung = Kopfmitte. Wird auch für Porträts, Stock-Icons und Lupe benutzt.
let private drawHead (ctx: Ctx) (pal: Palette) (fighter: Fighter) (expr: Expression) (frame: int) (detailed: bool) =
    match fighter with
    | Aether
    | Ignis
    | Ronin ->
        let aether = fighter = Aether
        let ronin = fighter = Ronin

        // Hinteres Haar (Ronin: zurückgebundenes Haar mit Samurai-Knoten)
        if ronin then
            poly ctx [ (5.0, -10.0); (-4.0, -12.5); (-11.0, -9.5); (-13.0, -2.0); (-9.0, 5.0); (-2.0, 2.0) ]
            outlinedFill ctx pal pal.HairShade 3.2
            ctx.beginPath ()
            ctx.ellipse (-6.0, -14.5, 3.4, 5.4, -0.5, 0.0, Math.PI * 2.0)
            outlinedFill ctx pal pal.Hair 2.4
            poly ctx [ (-8.0, -11.0); (-4.0, -11.6); (-5.0, -9.0) ]
        elif aether then
            poly
                ctx
                [ (4.0, -10.5); (-3.0, -13.5); (-9.0, -12.0); (-16.5, -13.0); (-12.0, -7.0); (-18.5, -3.5)
                  (-11.5, -1.0); (-16.0, 5.0); (-8.0, 4.0); (-5.0, 8.0); (-1.0, 0.0) ]
        else
            poly
                ctx
                [ (6.0, -9.0); (4.0, -17.0); (0.0, -11.5); (-3.0, -19.5); (-6.0, -11.5); (-12.0, -17.0)
                  (-11.0, -8.5); (-17.0, -8.0); (-11.5, -2.0); (-14.0, 3.0); (-7.0, 2.0); (-1.0, 0.0) ]

        outlinedFill ctx pal pal.HairShade 3.2

        // Gesicht mit Cel-Schatten
        circle ctx 0.0 0.0 9.6
        outlinedFill ctx pal pal.SkinShade 3.2
        fill ctx pal.Skin
        circle ctx 1.4 -1.4 8.0
        ctx.fill ()

        // Vorderes Haar
        if ronin then
            poly ctx [ (9.0, -5.0); (6.0, -10.5); (-1.0, -12.0); (-8.5, -9.5); (-9.5, -4.0); (-5.0, -7.5); (1.0, -8.0); (6.0, -7.0); (8.0, -2.0) ]
        elif aether then
            poly ctx [ (9.2, -3.5); (6.5, -10.0); (0.0, -12.5); (-8.0, -10.0); (-9.5, -3.0); (-4.5, -6.0); (-1.0, -1.5); (2.0, -7.0); (5.0, -2.5); (6.5, -6.0) ]
        else
            poly ctx [ (9.0, -6.0); (6.0, -11.0); (-1.0, -12.5); (-8.5, -9.5); (-9.6, -4.0); (-3.0, -7.0); (3.0, -7.5) ]

        outlinedFill ctx pal pal.Hair 2.2

        if detailed then
            fill ctx "rgba(255,255,255,0.55)"
            poly ctx [ (3.0, -11.0); (-4.0, -11.5); (-1.0, -9.0) ]
            ctx.fill ()

        // Stirnreif (Aether) bzw. Stirnband (Ignis)
        if aether then
            stroke ctx pal.Outline
            ctx.lineWidth <- 3.6
            ctx.lineCap <- "round"
            ctx.beginPath ()
            ctx.moveTo (-9.0, -4.0)
            ctx.lineTo (9.0, -5.0)
            ctx.stroke ()
            stroke ctx pal.Glow
            ctx.lineWidth <- 1.8
            ctx.stroke ()
            circle ctx -8.5 -1.0 2.6
            outlinedFill ctx pal pal.Trim 2.0
        else
            poly ctx [ (-9.6, -7.5); (9.4, -8.5); (9.8, -4.5); (-9.8, -3.0) ]
            outlinedFill ctx pal pal.Cloth 2.2
            fill ctx pal.ClothShade
            poly ctx [ (-9.6, -4.6); (9.7, -6.0); (9.8, -4.5); (-9.8, -3.0) ]
            ctx.fill ()
            circle ctx -9.0 -5.0 2.8
            outlinedFill ctx pal pal.Cloth 2.0

        if detailed then
            drawEye ctx pal 4.4 0.2 expr frame (if aether then 0 else 1)

            if expr = Calm || expr = Fierce then
                drawBrow ctx pal expr

            stroke ctx pal.Outline
            ctx.lineWidth <- 1.4
            ctx.beginPath ()

            match expr with
            | Hurt ->
                fill ctx pal.Outline
                oval ctx 5.6 5.4 1.8 2.3
                ctx.fill ()
            | Fierce ->
                ctx.moveTo (4.6, 5.6)
                ctx.lineTo (7.6, 5.0)
                ctx.lineTo (6.0, 6.6)
                ctx.stroke ()
            | _ ->
                ctx.moveTo (5.0, 5.6)
                ctx.lineTo (7.4, 5.4)
                ctx.stroke ()
    | Titan ->
        // Kammhelm mit leuchtendem Visier, Gesicht verborgen
        poly ctx [ (-4.0, -12.0); (-2.0, -21.0); (6.0, -19.0); (4.0, -11.0) ]
        outlinedFill ctx pal pal.Hair 2.6
        fill ctx pal.HairShade
        poly ctx [ (-4.0, -12.0); (-2.0, -21.0); (0.5, -20.5); (-1.0, -11.5) ]
        ctx.fill ()
        roundRect ctx -11.5 -12.5 22.5 25.0 9.0
        outlinedFill ctx pal pal.ArmorShade 3.4
        fill ctx pal.Armor
        roundRect ctx -9.0 -11.0 19.0 20.0 7.5
        ctx.fill ()
        // Visier
        let visorColor =
            match expr with
            | Hurt -> if frame % 6 < 3 then "#ff5050" else pal.Outline
            | Fierce -> "#ffb347"
            | _ -> pal.Glow

        roundRect ctx -1.0 -3.8 13.0 6.6 3.0
        outlinedFill ctx pal pal.Outline 2.0

        if detailed then
            ctx.save ()
            ctx.shadowColor <- visorColor
            ctx.shadowBlur <- 8.0
            fill ctx visorColor
            roundRect ctx 1.0 -2.0 9.6 2.8 1.4
            ctx.fill ()
            ctx.restore ()
        else
            fill ctx visorColor
            roundRect ctx 1.0 -2.0 9.6 2.8 1.4
            ctx.fill ()

        // Kinnschutz und Nieten
        poly ctx [ (2.0, 4.0); (11.0, 4.5); (8.0, 11.0); (0.0, 11.0) ]
        outlinedFill ctx pal pal.Trim 2.0
        fill ctx pal.Outline
        circle ctx -6.5 -6.0 1.2
        ctx.fill ()
        circle ctx -6.5 4.0 1.2
        ctx.fill ()

        if detailed then
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.6
            ctx.beginPath ()
            ctx.arc (0.0, 0.0, 9.0, Math.PI * 1.15, Math.PI * 1.6)
            ctx.stroke ()
    | Zephyr ->
        // Kapuze hinten, Gesicht, Maske über Mund und Nase
        poly ctx [ (2.0, -11.5); (-7.0, -12.5); (-14.0, -6.0); (-15.0, 4.0); (-9.0, 10.0); (-2.0, 9.0) ]
        outlinedFill ctx pal pal.HairShade 3.0
        circle ctx 0.0 0.0 9.4
        outlinedFill ctx pal pal.SkinShade 3.2
        fill ctx pal.Skin
        circle ctx 1.4 -1.4 7.8
        ctx.fill ()
        // Strähnen über der Stirn
        poly ctx [ (9.0, -5.0); (7.0, -11.0); (-1.0, -12.0); (-8.0, -9.0); (-9.0, -3.5); (-3.0, -6.0); (2.0, -3.5); (4.0, -7.5) ]
        outlinedFill ctx pal pal.Hair 2.2
        // Stirnband mit Metallplatte
        poly ctx [ (-9.4, -6.5); (9.4, -7.4); (9.6, -3.8); (-9.6, -2.6) ]
        outlinedFill ctx pal pal.Under 2.0
        roundRect ctx 2.5 -7.6 6.5 4.2 1.2
        outlinedFill ctx pal "#c9d2e6" 1.4

        if detailed then
            fill ctx pal.Trim
            ctx.fillRect (4.0, -6.2, 3.6, 1.2)

        // Maske
        poly ctx [ (-3.0, 1.8); (9.6, 1.0); (9.0, 7.5); (2.0, 9.6); (-4.0, 7.0) ]
        outlinedFill ctx pal pal.Cloth 2.0
        fill ctx pal.ClothShade
        poly ctx [ (-3.0, 5.0); (8.8, 5.6); (9.0, 7.5); (2.0, 9.6); (-4.0, 7.0) ]
        ctx.fill ()

        if detailed then
            match expr with
            | Fierce ->
                // Glühende Augen im Angriff
                ctx.save ()
                ctx.shadowColor <- pal.Glow
                ctx.shadowBlur <- 8.0
                fill ctx pal.Glow
                poly ctx [ (2.0, -1.5); (7.6, -2.4); (7.0, 0.4); (2.4, 0.6) ]
                ctx.fill ()
                ctx.restore ()
                drawBrow ctx pal expr
            | _ ->
                drawEye ctx pal 4.6 -1.0 expr frame 3

                if expr = Calm then
                    drawBrow ctx pal expr
    | Valkyrie ->
        // Gesicht, goldener Pony, Flügelhelm
        circle ctx 0.0 0.0 9.6
        outlinedFill ctx pal pal.SkinShade 3.2
        fill ctx pal.Skin
        circle ctx 1.4 -1.4 8.0
        ctx.fill ()
        poly ctx [ (8.0, -2.0); (5.0, -7.0); (-2.0, -7.5); (-8.5, -4.0); (-9.5, 4.0); (-6.0, 1.0); (-3.0, -3.0); (3.0, -3.5) ]
        outlinedFill ctx pal pal.Hair 2.0
        // Helmkappe
        ctx.beginPath ()
        ctx.arc (0.0, -1.0, 10.6, Math.PI * 1.02, Math.PI * 1.98)
        ctx.lineTo (10.6, -4.0)
        ctx.lineTo (-10.6, -4.0)
        ctx.closePath ()
        outlinedFill ctx pal pal.Armor 2.6
        fill ctx pal.ArmorShade
        ctx.beginPath ()
        ctx.arc (0.0, -1.0, 10.6, Math.PI * 1.02, Math.PI * 1.35)
        ctx.lineTo (-3.0, -4.0)
        ctx.closePath ()
        ctx.fill ()
        fill ctx pal.Trim
        ctx.fillRect (-10.4, -5.6, 20.8, 2.6)
        // Flügel
        poly ctx [ (-4.0, -8.0); (-12.0, -16.0); (-20.0, -15.0); (-14.0, -11.0); (-22.0, -10.0); (-14.0, -6.0); (-20.0, -3.0); (-8.0, -3.0) ]
        outlinedFill ctx pal "#ffffff" 2.0

        if detailed then
            stroke ctx "rgba(150,165,210,0.8)"
            ctx.lineWidth <- 1.2
            ctx.beginPath ()
            ctx.moveTo (-8.0, -7.0)
            ctx.lineTo (-17.0, -12.5)
            ctx.moveTo (-8.0, -5.0)
            ctx.lineTo (-17.0, -7.5)
            ctx.stroke ()
            drawEye ctx pal 4.6 1.0 expr frame 4

            if expr = Calm || expr = Fierce then
                drawBrow ctx pal expr

            stroke ctx pal.Outline
            ctx.lineWidth <- 1.4
            ctx.beginPath ()
            ctx.moveTo (5.0, 6.0)
            ctx.lineTo (7.4, 5.8)
            ctx.stroke ()
    | Chrono ->
        // Silberhaar, Gesicht, hoher Zauberhut mit Uhr-Emblem
        poly ctx [ (2.0, -6.0); (-6.0, -8.0); (-14.0, -4.0); (-11.0, 0.0); (-15.0, 4.0); (-8.0, 5.0); (-3.0, 9.0); (0.0, 2.0) ]
        outlinedFill ctx pal pal.HairShade 3.0
        circle ctx 0.0 0.0 9.4
        outlinedFill ctx pal pal.SkinShade 3.2
        fill ctx pal.Skin
        circle ctx 1.4 -1.4 7.8
        ctx.fill ()
        poly ctx [ (9.0, -3.0); (7.0, -7.0); (-1.0, -8.0); (-8.0, -5.0); (-4.0, -4.0); (2.0, -5.0) ]
        outlinedFill ctx pal pal.Hair 2.0
        // Hutkrempe und Spitze
        poly ctx [ (-14.0, -6.0); (14.0, -8.0); (12.0, -4.0); (-14.0, -2.5) ]
        outlinedFill ctx pal pal.Armor 2.4
        poly ctx [ (-8.0, -6.0); (9.0, -7.0); (2.0, -24.0); (-6.0, -30.0); (-12.0, -27.0); (-4.0, -22.0) ]
        outlinedFill ctx pal pal.Armor 2.6
        fill ctx pal.ArmorShade
        poly ctx [ (-8.0, -6.0); (-2.0, -6.4); (-4.0, -22.0); (-12.0, -27.0); (-6.0, -21.0) ]
        ctx.fill ()
        fill ctx pal.Trim
        poly ctx [ (-7.5, -9.0); (8.5, -10.0); (7.8, -12.6); (-7.0, -11.6) ]
        ctx.fill ()
        circle ctx 0.6 -15.0 3.2
        outlinedFill ctx pal "#fff6d8" 1.4

        if detailed then
            stroke ctx pal.Outline
            ctx.lineWidth <- 0.9
            let a = float frame * 0.05
            ctx.beginPath ()
            ctx.moveTo (0.6, -15.0)
            ctx.lineTo (0.6 + cos a * 2.4, -15.0 + sin a * 2.4)
            ctx.moveTo (0.6, -15.0)
            ctx.lineTo (0.6, -17.0)
            ctx.stroke ()
            drawEye ctx pal 4.4 0.4 expr frame 5

            if expr = Calm || expr = Fierce then
                drawBrow ctx pal expr

            // Monokel
            stroke ctx pal.Trim
            ctx.lineWidth <- 1.2
            circle ctx 5.4 0.4 3.9
            ctx.stroke ()
            stroke ctx pal.Outline
            ctx.lineWidth <- 1.4
            ctx.beginPath ()
            ctx.moveTo (5.0, 6.0)
            ctx.lineTo (7.4, 5.6)
            ctx.stroke ()
    | Bramble ->
        // Mähne aus Moos, Geweih, Bestien-Schnauze
        poly ctx [ (2.0, -10.0); (-4.0, -15.0); (-8.0, -10.0); (-15.0, -12.0); (-12.0, -5.0); (-18.0, -1.0); (-12.0, 2.0); (-16.0, 8.0); (-7.0, 7.0); (-4.0, 12.0); (0.0, 6.0) ]
        outlinedFill ctx pal pal.Hair 3.0
        fill ctx pal.HairShade
        poly ctx [ (-12.0, 2.0); (-16.0, 8.0); (-7.0, 7.0); (-4.0, 12.0); (-2.0, 5.0) ]
        ctx.fill ()
        // Geweih
        stroke ctx pal.Outline
        ctx.lineWidth <- 5.0
        ctx.lineCap <- "round"

        let antler () =
            ctx.beginPath ()
            ctx.moveTo (-2.0, -8.0)
            ctx.lineTo (-6.0, -18.0)
            ctx.lineTo (-12.0, -24.0)
            ctx.moveTo (-6.0, -18.0)
            ctx.lineTo (-2.0, -25.0)
            ctx.moveTo (-9.0, -21.0)
            ctx.lineTo (-16.0, -20.0)
            ctx.stroke ()

        antler ()
        stroke ctx pal.Trim
        ctx.lineWidth <- 2.4
        antler ()
        // Kopf und Schnauze
        circle ctx 0.0 0.0 10.0
        outlinedFill ctx pal pal.SkinShade 3.2
        fill ctx pal.Skin
        circle ctx 1.0 -1.6 8.4
        ctx.fill ()
        oval ctx 8.0 3.4 6.6 4.8
        outlinedFill ctx pal pal.Under 2.4
        fill ctx pal.Outline
        oval ctx 13.0 1.8 2.0 1.5
        ctx.fill ()
        // Reißzahn
        fill ctx "#ffffff"
        poly ctx [ (8.0, 6.8); (10.0, 6.8); (9.0, 10.0) ]
        ctx.fill ()

        if detailed then
            ctx.save ()
            ctx.shadowColor <- pal.Eye
            ctx.shadowBlur <- (if expr = Fierce then 8.0 else 0.0)
            fill ctx (if expr = Hurt then pal.Outline else pal.Eye)
            poly ctx [ (2.0, -3.6); (6.6, -4.8); (6.0, -2.0); (2.4, -1.6) ]
            ctx.fill ()
            ctx.restore ()
            stroke ctx pal.Outline
            ctx.lineWidth <- 2.0
            ctx.beginPath ()
            ctx.moveTo (1.4, (if expr = Fierce then -6.4 else -5.6))
            ctx.lineTo (7.4, (if expr = Fierce then -4.6 else -6.0))
            ctx.stroke ()
    | Lyra ->
        // Silberhaar mit Mondsichel-Diadem
        poly ctx [ (5.0, -10.0); (-4.0, -12.5); (-12.0, -9.0); (-14.0, -1.0); (-11.0, 7.0); (-4.0, 3.0) ]
        outlinedFill ctx pal pal.HairShade 3.0
        circle ctx 0.0 0.0 9.4
        outlinedFill ctx pal pal.SkinShade 3.2
        fill ctx pal.Skin
        circle ctx 1.4 -1.4 7.8
        ctx.fill ()
        poly ctx [ (9.0, -3.0); (6.0, -10.0); (-2.0, -12.0); (-9.0, -8.0); (-10.0, 2.0); (-6.0, -3.0); (0.0, -6.5); (5.0, -5.5) ]
        outlinedFill ctx pal pal.Hair 2.0
        // Diadem
        stroke ctx pal.Outline
        ctx.lineWidth <- 3.0
        ctx.beginPath ()
        ctx.moveTo (-8.0, -6.5)
        ctx.quadraticCurveTo (0.0, -9.5, 8.6, -6.0)
        ctx.stroke ()
        stroke ctx pal.Trim
        ctx.lineWidth <- 1.6
        ctx.stroke ()
        ctx.beginPath ()
        ctx.arc (1.0, -11.5, 3.4, Math.PI * 0.2, Math.PI * 1.6)
        ctx.arc (2.2, -12.2, 2.6, Math.PI * 1.5, Math.PI * 0.3, true)
        ctx.closePath ()
        outlinedFill ctx pal pal.Trim 1.4

        if detailed then
            fill ctx "rgba(255,255,255,0.6)"
            poly ctx [ (2.0, -10.5); (-4.0, -10.0); (-1.0, -8.0) ]
            ctx.fill ()
            drawEye ctx pal 4.6 1.0 expr frame 5

            if expr = Calm || expr = Fierce then
                drawBrow ctx pal expr

            stroke ctx pal.Outline
            ctx.lineWidth <- 1.3
            ctx.beginPath ()
            ctx.moveTo (5.2, 6.0)
            ctx.lineTo (7.2, 5.6)
            ctx.stroke ()
    | Gorm ->
        // Felsschädel mit Moosbewuchs und glühenden Augen
        ctx.beginPath ()
        ctx.moveTo (-11.0, -6.0)
        ctx.quadraticCurveTo (-12.0, -14.0, -2.0, -14.5)
        ctx.quadraticCurveTo (11.0, -15.0, 12.0, -4.0)
        ctx.quadraticCurveTo (13.0, 8.0, 4.0, 11.0)
        ctx.quadraticCurveTo (-10.0, 12.0, -11.0, 2.0)
        ctx.closePath ()
        outlinedFill ctx pal pal.ArmorShade 3.4
        fill ctx pal.Armor
        ctx.beginPath ()
        ctx.moveTo (-8.0, -6.0)
        ctx.quadraticCurveTo (-8.0, -12.0, 0.0, -12.5)
        ctx.quadraticCurveTo (9.0, -12.5, 10.0, -3.0)
        ctx.quadraticCurveTo (10.0, 5.0, 4.0, 7.0)
        ctx.quadraticCurveTo (-6.0, 8.0, -8.0, 0.0)
        ctx.closePath ()
        ctx.fill ()
        // Moos auf dem Scheitel
        fill ctx pal.Hair
        ctx.beginPath ()
        ctx.moveTo (-10.0, -9.0)
        ctx.quadraticCurveTo (-4.0, -17.0, 4.0, -15.0)
        ctx.quadraticCurveTo (9.0, -14.0, 10.0, -10.0)
        ctx.quadraticCurveTo (2.0, -11.0, -10.0, -9.0)
        ctx.closePath ()
        ctx.fill ()
        // Risse
        stroke ctx pal.Outline
        ctx.lineWidth <- 1.2
        ctx.beginPath ()
        ctx.moveTo (-6.0, -2.0)
        ctx.lineTo (-3.0, 1.0)
        ctx.lineTo (-4.0, 5.0)
        ctx.stroke ()
        // Augen
        let eyeColor = if expr = Hurt then "#ffffff" else pal.Eye
        ctx.save ()

        if detailed then
            ctx.shadowColor <- eyeColor
            ctx.shadowBlur <- 8.0

        fill ctx eyeColor
        oval ctx 4.5 -2.5 2.6 1.6
        ctx.fill ()
        oval ctx 9.5 -2.0 1.6 1.3
        ctx.fill ()
        ctx.restore ()
        // Kiefer
        stroke ctx pal.Outline
        ctx.lineWidth <- 1.6
        ctx.beginPath ()
        ctx.moveTo (2.0, 5.0)
        ctx.lineTo (9.0, 4.0)
        ctx.stroke ()
    | Specter ->
        // Kapuze mit Zipfel, Gesicht im Schatten, Maske, glühende Augen
        poly ctx [ (6.0, -10.0); (-4.0, -13.0); (-12.0, -9.0); (-22.0, -14.0); (-16.0, -4.0); (-14.0, 6.0); (-6.0, 11.0); (4.0, 10.0) ]
        outlinedFill ctx pal pal.ArmorShade 3.0
        circle ctx 0.6 0.0 9.6
        outlinedFill ctx pal pal.Armor 3.2
        // Gesichtsöffnung im Schatten
        oval ctx 4.6 1.0 6.0 7.6
        fill ctx "#08040c"
        ctx.fill ()
        // Maske unten
        poly ctx [ (0.0, 3.2); (10.0, 2.4); (9.4, 7.0); (2.0, 9.0) ]
        outlinedFill ctx pal pal.Skin 1.6
        fill ctx pal.Trim
        ctx.fillRect (3.0, 4.6, 6.0, 1.2)

        if detailed then
            ctx.save ()
            ctx.shadowColor <- pal.Eye
            ctx.shadowBlur <- 10.0
            fill ctx (if expr = Hurt then "#ffffff" else pal.Eye)

            let eye (x: float) =
                poly ctx [ (x - 1.6, -1.6); (x + 2.4, -2.6); (x + 2.0, -0.4); (x - 1.2, 0.2) ]
                ctx.fill ()

            eye 3.6
            eye 7.8
            ctx.restore ()

/// Stiefel am Knöchelpunkt, Spitze zeigt nach +X
let private drawBoot (ctx: Ctx) (pal: Palette) (ankle: Vec2) (color: string) (shade: string) (size: float) =
    let k = size

    poly
        ctx
        [ (ankle.X - 4.2 * k, ankle.Y - 3.0 * k); (ankle.X + 3.6 * k, ankle.Y - 3.0 * k); (ankle.X + 9.6 * k, ankle.Y + 1.2)
          (ankle.X + 9.6 * k, ankle.Y + 3.6); (ankle.X - 4.6 * k, ankle.Y + 3.6) ]

    outlinedFill ctx pal color 3.0
    fill ctx shade
    ctx.fillRect (ankle.X - 4.2 * k, ankle.Y + 1.8, 13.6 * k, 1.8)

let private drawLeg (ctx: Ctx) (pal: Palette) (f: Fighter) (hip: Vec2) (knee: Vec2) (ankle: Vec2) (back: bool) =
    let pick (front: string) (rear: string) = if back then rear else front

    match f with
    | Aether ->
        bone ctx pal hip knee 7.6 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 7.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        circle ctx knee.X knee.Y 3.4
        outlinedFill ctx pal (pick pal.Armor pal.ArmorShade) 2.4
        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 1.0
        fill ctx pal.Trim
        ctx.fillRect (ankle.X - 3.6, ankle.Y - 3.6, 6.6, 1.8)
    | Ignis ->
        bone ctx pal hip knee 8.8 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 7.8 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal (mix knee ankle 0.72) ankle 6.4 pal.Cloth pal.ClothShade
        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 1.0
    | Titan ->
        bone ctx pal hip knee 10.5 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 10.5 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        circle ctx knee.X knee.Y 5.0
        outlinedFill ctx pal (pick pal.Trim pal.ArmorShade) 2.6
        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 1.35
    | Zephyr ->
        bone ctx pal hip knee 6.6 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 5.6 (pick pal.Under pal.UnderShade) pal.UnderShade
        // Beinwickel
        for k in [ 0.35; 0.6; 0.85 ] do
            let a = mix knee ankle k
            let b = mix knee ankle (k + 0.08)
            bone ctx pal a b 5.0 (pick pal.Trim pal.ClothShade) pal.ClothShade

        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 0.85
    | Valkyrie ->
        bone ctx pal hip knee 7.4 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 7.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        circle ctx knee.X knee.Y 3.6
        outlinedFill ctx pal (pick pal.Trim pal.ArmorShade) 2.4
        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 1.0
        fill ctx pal.Trim
        ctx.fillRect (ankle.X - 3.6, ankle.Y - 3.6, 6.6, 1.8)
    | Chrono ->
        bone ctx pal hip knee 6.0 (pick pal.UnderShade pal.Outline) pal.Outline
        bone ctx pal knee ankle 5.6 (pick pal.UnderShade pal.Outline) pal.Outline
        drawBoot ctx pal ankle (pick pal.ArmorShade pal.Outline) pal.Outline 0.9
    | Bramble ->
        bone ctx pal hip knee 10.0 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 8.6 (pick pal.Skin pal.SkinShade) pal.SkinShade
        // Pranke mit Krallen
        oval ctx (ankle.X + 3.0) (ankle.Y + 1.6) 7.4 4.2
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 2.8
        stroke ctx pal.Trim
        ctx.lineWidth <- 2.0
        ctx.lineCap <- "round"

        for k in 0..2 do
            ctx.beginPath ()
            ctx.moveTo (ankle.X + 6.0 + float k * 1.6, ankle.Y + 3.0)
            ctx.lineTo (ankle.X + 9.5 + float k * 1.6, ankle.Y + 5.0)
            ctx.stroke ()
    | Ronin ->
        // Weite Hakama-Hose und Sandalen
        bone ctx pal hip knee 9.8 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 8.8 (pick pal.Under pal.UnderShade) pal.UnderShade
        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 0.9
    | Lyra ->
        bone ctx pal hip knee 6.6 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 6.0 (pick pal.Under pal.UnderShade) pal.UnderShade
        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 0.95
        fill ctx pal.Trim
        ctx.fillRect (ankle.X - 3.0, ankle.Y - 6.0, 6.0, 1.6)
    | Gorm ->
        // Säulenbeine aus Stein
        bone ctx pal hip knee 13.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        bone ctx pal knee ankle 12.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        circle ctx knee.X knee.Y 5.4
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 2.4
        drawBoot ctx pal ankle (pick pal.Under pal.UnderShade) pal.UnderShade 1.35
    | Specter ->
        bone ctx pal hip knee 6.4 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal knee ankle 5.4 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal (mix knee ankle 0.5) (mix knee ankle 0.8) 5.0 (pick pal.Trim pal.ClothShade) pal.ClothShade
        drawBoot ctx pal ankle (pick pal.Armor pal.ArmorShade) pal.ArmorShade 0.85

let private glowAt (ctx: Ctx) (pos: Vec2) (radius: float) (inner: string) (outer: string) =
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"
    let halo = ctx.createRadialGradient (pos.X, pos.Y, 1.0, pos.X, pos.Y, radius)
    halo.addColorStop (0.0, "rgba(255,250,220,0.95)")
    halo.addColorStop (0.4, inner)
    halo.addColorStop (1.0, outer)
    ctx.fillStyle <- !^halo
    ctx.fillRect (pos.X - radius, pos.Y - radius, radius * 2.0, radius * 2.0)
    ctx.restore ()

let private drawArm (ctx: Ctx) (pal: Palette) (f: Fighter) (shoulder: Vec2) (elbow: Vec2) (hand: Vec2) (back: bool) (glow: bool) =
    let pick (front: string) (rear: string) = if back then rear else front

    match f with
    | Aether ->
        bone ctx pal shoulder elbow 6.2 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal elbow hand 6.6 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        circle ctx hand.X hand.Y 3.6
        outlinedFill ctx pal (pick pal.Armor pal.ArmorShade) 2.4

        if not back then
            circle ctx shoulder.X (shoulder.Y - 0.5) 5.6
            outlinedFill ctx pal pal.Armor 2.8
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.6
            ctx.beginPath ()
            ctx.arc (shoulder.X, shoulder.Y - 0.5, 4.0, Math.PI * 1.05, Math.PI * 1.75)
            ctx.stroke ()
            fill ctx pal.Trim
            circle ctx shoulder.X (shoulder.Y - 0.5) 1.7
            ctx.fill ()
    | Ignis ->
        bone ctx pal shoulder elbow 6.2 (pick pal.Skin pal.SkinShade) pal.SkinShade
        bone ctx pal elbow hand 6.0 (pick pal.Skin pal.SkinShade) pal.SkinShade
        bone ctx pal (mix elbow hand 0.55) (mix elbow hand 0.8) 6.2 pal.Cloth pal.ClothShade
        circle ctx hand.X hand.Y 5.6
        outlinedFill ctx pal (pick pal.Armor pal.ArmorShade) 2.8
        fill ctx pal.Trim
        circle ctx (hand.X + 1.2) (hand.Y - 1.2) 1.8
        ctx.fill ()

        if glow then
            glowAt ctx hand 17.0 "rgba(255,150,50,0.6)" "rgba(255,80,20,0)"
    | Titan ->
        bone ctx pal shoulder elbow 8.6 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal elbow hand 10.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        // Riesiger Panzerhandschuh
        roundRect ctx (hand.X - 7.0) (hand.Y - 6.5) 14.0 13.0 4.5
        outlinedFill ctx pal (pick pal.Trim pal.ArmorShade) 3.0
        fill ctx (pick pal.Armor pal.ArmorShade)
        ctx.fillRect (hand.X + 1.5, hand.Y - 5.0, 4.0, 10.0)

        if not back then
            circle ctx shoulder.X (shoulder.Y - 1.0) 8.0
            outlinedFill ctx pal pal.Armor 3.0
            fill ctx pal.ArmorShade
            ctx.beginPath ()
            ctx.arc (shoulder.X, shoulder.Y - 1.0, 8.0, Math.PI * 0.1, Math.PI * 0.9)
            ctx.fill ()
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.8
            ctx.beginPath ()
            ctx.arc (shoulder.X, shoulder.Y - 1.0, 5.5, Math.PI * 1.1, Math.PI * 1.7)
            ctx.stroke ()

        if glow then
            glowAt ctx hand 20.0 "rgba(255,210,90,0.6)" "rgba(255,150,30,0)"
    | Zephyr ->
        bone ctx pal shoulder elbow 5.4 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        bone ctx pal elbow hand 4.8 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal (mix elbow hand 0.45) (mix elbow hand 0.7) 5.0 (pick pal.Trim pal.ClothShade) pal.ClothShade
        circle ctx hand.X hand.Y 3.2
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 2.2

        if glow then
            glowAt ctx hand 16.0 "rgba(200,160,255,0.6)" "rgba(120,60,255,0)"
    | Valkyrie ->
        bone ctx pal shoulder elbow 6.0 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal elbow hand 6.4 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        circle ctx hand.X hand.Y 3.4
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 2.2

        if not back then
            circle ctx shoulder.X (shoulder.Y - 0.5) 5.8
            outlinedFill ctx pal pal.Armor 2.8
            fill ctx pal.Trim
            ctx.fillRect (shoulder.X - 5.0, shoulder.Y + 1.5, 10.0, 2.0)
    | Chrono ->
        // Weiter Robenärmel
        bone ctx pal shoulder elbow 7.4 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        bone ctx pal elbow hand 8.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        bone ctx pal (mix elbow hand 0.75) (mix elbow hand 0.9) 8.6 pal.Trim pal.Trim
        circle ctx hand.X hand.Y 3.2
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 2.0

        if glow then
            glowAt ctx hand 18.0 "rgba(120,255,230,0.6)" "rgba(40,180,160,0)"
    | Bramble ->
        bone ctx pal shoulder elbow 9.4 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal elbow hand 9.0 (pick pal.Skin pal.SkinShade) pal.SkinShade
        circle ctx hand.X hand.Y 6.4
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 2.8
        // Krallen
        stroke ctx pal.Outline
        ctx.lineWidth <- 4.0
        ctx.lineCap <- "round"

        let claws () =
            ctx.beginPath ()

            for k in -1..1 do
                ctx.moveTo (hand.X + 4.0, hand.Y + float k * 3.4)
                ctx.lineTo (hand.X + 10.0, hand.Y + float k * 4.2 + 1.0)

            ctx.stroke ()

        claws ()
        stroke ctx pal.Trim
        ctx.lineWidth <- 2.0
        claws ()

        if not back then
            // Rindenpanzer auf der Schulter
            poly ctx [ (shoulder.X - 7.0, shoulder.Y + 2.0); (shoulder.X - 2.0, shoulder.Y - 8.0); (shoulder.X + 7.0, shoulder.Y - 5.0); (shoulder.X + 6.0, shoulder.Y + 4.0) ]
            outlinedFill ctx pal pal.Armor 2.6
            fill ctx pal.Hair
            circle ctx (shoulder.X - 1.0) (shoulder.Y - 5.0) 2.4
            ctx.fill ()

        if glow then
            glowAt ctx hand 18.0 "rgba(190,255,120,0.55)" "rgba(90,200,40,0)"
    | Ronin ->
        // Kimono-Ärmel und Unterarm mit Wickelband
        bone ctx pal shoulder elbow 8.6 (pick pal.Cloth pal.ClothShade) pal.ClothShade
        bone ctx pal elbow hand 5.6 (pick pal.Skin pal.SkinShade) pal.SkinShade
        circle ctx hand.X hand.Y 3.4
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 2.2
        stroke ctx pal.Armor
        ctx.lineWidth <- 2.0
        let wrist = mix elbow hand 0.72
        circle ctx wrist.X wrist.Y 2.6
        ctx.stroke ()
    | Lyra ->
        bone ctx pal shoulder elbow 5.4 (pick pal.Cloth pal.ClothShade) pal.ClothShade
        bone ctx pal elbow hand 5.0 (pick pal.Skin pal.SkinShade) pal.SkinShade
        circle ctx hand.X hand.Y 3.2
        outlinedFill ctx pal (pick pal.Armor pal.ArmorShade) 2.2
    | Gorm ->
        // Felsarme mit riesigen Fäusten und glühenden Adern
        bone ctx pal shoulder elbow 11.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        bone ctx pal elbow hand 12.0 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        circle ctx hand.X hand.Y 8.2
        outlinedFill ctx pal (pick pal.Skin pal.SkinShade) 3.0

        if glow then
            glowAt ctx hand 22.0 "rgba(255,190,90,0.7)" "rgba(255,120,30,0.25)"

        if not back then
            circle ctx shoulder.X (shoulder.Y - 1.0) 8.0
            outlinedFill ctx pal pal.Armor 3.0
            fill ctx pal.Hair
            ctx.beginPath ()
            ctx.arc (shoulder.X, shoulder.Y - 1.0, 8.0, Math.PI * 1.1, Math.PI * 1.9)
            ctx.closePath ()
            ctx.fill ()
    | Specter ->
        bone ctx pal shoulder elbow 5.2 (pick pal.Armor pal.ArmorShade) pal.ArmorShade
        bone ctx pal elbow hand 4.8 (pick pal.Under pal.UnderShade) pal.UnderShade
        bone ctx pal (mix elbow hand 0.4) (mix elbow hand 0.65) 5.0 (pick pal.Trim pal.ClothShade) pal.ClothShade
        circle ctx hand.X hand.Y 3.2
        outlinedFill ctx pal (pick pal.Under pal.UnderShade) 2.0

        if glow then
            glowAt ctx hand 16.0 "rgba(255,90,140,0.6)" "rgba(120,20,60,0)"

let private drawJetpack (ctx: Ctx) (pal: Palette) (rig: Rig) (lean: float) (thrust: bool) (frame: int) =
    ctx.save ()
    ctx.translate (rig.Hip.X, rig.Hip.Y)
    ctx.rotate lean

    if thrust then
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"

        for nx in [ -17.0; -9.0 ] do
            let len = 26.0 + sin (float frame * 1.9 + nx) * 6.0
            let jet = ctx.createLinearGradient (0.0, -8.0, 0.0, -8.0 + len)
            jet.addColorStop (0.0, "rgba(255,255,255,0.95)")
            jet.addColorStop (0.35, "rgba(255,190,80,0.9)")
            jet.addColorStop (1.0, "rgba(255,60,20,0)")
            ctx.fillStyle <- !^jet
            poly ctx [ (nx - 4.0, -8.0); (nx + 4.0, -8.0); (nx, -8.0 + len) ]
            ctx.fill ()

        ctx.restore ()

    roundRect ctx -22.0 -30.0 16.0 22.0 4.0
    outlinedFill ctx pal pal.UnderShade 2.6
    fill ctx pal.Under
    ctx.fillRect (-20.0, -28.0, 6.0, 18.0)

    for nx in [ -17.0; -9.0 ] do
        poly ctx [ (nx - 3.5, -9.0); (nx + 3.5, -9.0); (nx + 4.5, -5.0); (nx - 4.5, -5.0) ]
        outlinedFill ctx pal pal.Trim 2.0

    ctx.restore ()

let private drawTorso (ctx: Ctx) (pal: Palette) (f: Fighter) (rig: Rig) (lean: float) (shaded: bool) (frame: int) =
    let thick =
        match f with
        | Titan -> 25.0
        | Zephyr -> 16.0
        | _ -> 19.0

    ctx.lineCap <- "round"
    stroke ctx pal.Outline
    ctx.lineWidth <- thick
    ctx.beginPath ()
    ctx.moveTo (rig.Hip.X, rig.Hip.Y - 2.0)
    ctx.lineTo (rig.Chest.X, rig.Chest.Y + 3.0)
    ctx.stroke ()

    ctx.save ()
    ctx.translate (rig.Hip.X, rig.Hip.Y)
    ctx.rotate lean

    match f with
    | Aether ->
        fill ctx pal.Under
        roundRect ctx -6.5 -13.0 13.0 15.0 3.0
        ctx.fill ()
        poly ctx [ (-7.5, 0.0); (7.5, 0.0); (9.0, 8.0); (3.0, 5.5); (0.5, 9.5); (-3.0, 5.5); (-8.5, 7.0) ]
        outlinedFill ctx pal pal.Armor 2.4
        fill ctx pal.ArmorShade
        poly ctx [ (-7.5, 0.0); (-1.0, 0.0); (-3.0, 5.5); (-8.5, 7.0) ]
        ctx.fill ()
        poly ctx [ (-8.5, -22.0); (8.0, -22.0); (9.5, -13.5); (5.5, -8.0); (-5.5, -8.0); (-8.5, -13.5) ]
        outlinedFill ctx pal pal.Armor 2.4
        fill ctx pal.ArmorShade
        poly ctx [ (-8.5, -22.0); (-1.5, -22.0); (-2.5, -8.0); (-5.5, -8.0); (-8.5, -13.5) ]
        ctx.fill ()
        fill ctx pal.Trim
        ctx.fillRect (-7.0, -3.4, 14.0, 3.6)

        if shaded then
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.6
            ctx.beginPath ()
            ctx.moveTo (7.2, -21.0)
            ctx.lineTo (8.6, -13.8)
            ctx.lineTo (5.2, -9.0)
            ctx.stroke ()
            ctx.save ()
            ctx.shadowColor <- pal.Glow
            ctx.shadowBlur <- 8.0
            fill ctx pal.Glow
            poly ctx [ (3.0, -18.5); (5.6, -15.5); (3.0, -12.5); (0.4, -15.5) ]
            ctx.fill ()
            ctx.restore ()
    | Ignis ->
        poly ctx [ (-8.0, -22.0); (8.0, -22.0); (9.0, -12.0); (6.5, -1.0); (-6.5, -1.0); (-8.0, -12.0) ]
        outlinedFill ctx pal pal.Armor 2.4
        fill ctx pal.ArmorShade
        poly ctx [ (-8.0, -22.0); (-2.0, -22.0); (-2.5, -1.0); (-6.5, -1.0); (-8.0, -12.0) ]
        ctx.fill ()
        fill ctx pal.Skin
        poly ctx [ (0.5, -22.0); (7.2, -22.0); (4.0, -7.5) ]
        ctx.fill ()
        fill ctx pal.SkinShade
        poly ctx [ (0.5, -22.0); (2.6, -22.0); (4.0, -7.5) ]
        ctx.fill ()
        fill ctx pal.Trim
        ctx.fillRect (-7.0, -5.0, 14.0, 4.4)
        poly ctx [ (-6.5, -1.0); (-2.5, -1.0); (-5.0, 9.0); (-9.5, 8.0) ]
        outlinedFill ctx pal pal.Trim 2.0

        if shaded then
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.5
            ctx.beginPath ()
            ctx.moveTo (7.4, -21.0)
            ctx.lineTo (8.2, -12.0)
            ctx.stroke ()
    | Titan ->
        // Breiter Brustpanzer, Bauchplatten, Gürtel und Energiekern
        poly ctx [ (-10.0, 1.0); (10.0, 1.0); (11.5, 9.0); (-11.5, 9.0) ]
        outlinedFill ctx pal pal.Under 2.6
        roundRect ctx -9.5 -12.0 19.0 13.0 3.0
        outlinedFill ctx pal pal.UnderShade 2.4
        fill ctx pal.Trim

        for i in 0..2 do
            ctx.fillRect (-7.5 + float i * 5.4, -10.0, 4.2, 9.0)

        poly ctx [ (-12.5, -24.0); (12.0, -24.0); (13.5, -15.0); (8.0, -9.5); (-8.0, -9.5); (-13.0, -15.0) ]
        outlinedFill ctx pal pal.Armor 2.8
        fill ctx pal.ArmorShade
        poly ctx [ (-12.5, -24.0); (-3.0, -24.0); (-4.0, -9.5); (-8.0, -9.5); (-13.0, -15.0) ]
        ctx.fill ()
        fill ctx pal.ArmorShade
        ctx.fillRect (-11.0, -1.5, 22.0, 4.5)
        circle ctx 0.0 0.8 2.6
        outlinedFill ctx pal pal.Trim 1.6

        if shaded then
            ctx.save ()
            ctx.shadowColor <- pal.Glow
            ctx.shadowBlur <- 10.0
            fill ctx pal.Glow
            circle ctx 4.0 -17.0 (3.4 + 0.4 * sin (float frame * 0.15))
            ctx.fill ()
            ctx.restore ()
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.8
            ctx.beginPath ()
            ctx.moveTo (11.0, -23.0)
            ctx.lineTo (12.6, -15.0)
            ctx.lineTo (8.0, -10.5)
            ctx.stroke ()
    | Zephyr ->
        // Schlanker Gi mit überkreuztem Kragen und Schärpe
        poly ctx [ (-6.5, -21.0); (6.5, -21.0); (7.0, -2.0); (8.0, 8.0); (-8.0, 8.0); (-7.0, -2.0) ]
        outlinedFill ctx pal pal.Armor 2.4
        fill ctx pal.ArmorShade
        poly ctx [ (-6.5, -21.0); (-1.0, -21.0); (-1.5, 8.0); (-8.0, 8.0); (-7.0, -2.0) ]
        ctx.fill ()
        stroke ctx pal.Trim
        ctx.lineWidth <- 2.0
        ctx.beginPath ()
        ctx.moveTo (-3.0, -21.0)
        ctx.lineTo (4.0, -10.0)
        ctx.moveTo (5.0, -21.0)
        ctx.lineTo (0.0, -13.0)
        ctx.stroke ()
        fill ctx pal.Cloth
        ctx.fillRect (-7.2, -4.5, 14.4, 4.4)
        poly ctx [ (-6.5, -1.0); (-3.0, -1.0); (-6.0, 8.0); (-10.0, 6.5) ]
        outlinedFill ctx pal pal.Cloth 1.8

        if shaded then
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.4
            ctx.beginPath ()
            ctx.moveTo (6.2, -20.0)
            ctx.lineTo (6.8, -6.0)
            ctx.stroke ()

    | Valkyrie ->
        // Blauer Waffenrock, Brustpanzer, Gürtel
        poly ctx [ (-7.5, -2.0); (7.5, -2.0); (11.0, 12.0); (4.0, 9.0); (0.0, 13.0); (-4.0, 9.0); (-11.0, 12.0) ]
        outlinedFill ctx pal pal.Under 2.4
        fill ctx pal.UnderShade
        poly ctx [ (-7.5, -2.0); (-1.0, -2.0); (-4.0, 9.0); (-11.0, 12.0) ]
        ctx.fill ()
        poly ctx [ (-8.0, -22.0); (8.0, -22.0); (9.0, -12.0); (6.0, -4.0); (-6.0, -4.0); (-8.5, -12.0) ]
        outlinedFill ctx pal pal.Armor 2.4
        fill ctx pal.ArmorShade
        poly ctx [ (-8.0, -22.0); (-1.5, -22.0); (-2.0, -4.0); (-6.0, -4.0); (-8.5, -12.0) ]
        ctx.fill ()
        fill ctx pal.Trim
        ctx.fillRect (-7.0, -4.4, 14.0, 3.6)
        poly ctx [ (0.0, -19.0); (3.0, -15.0); (0.0, -11.0); (-3.0, -15.0) ]
        ctx.fill ()

        if shaded then
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.6
            ctx.beginPath ()
            ctx.moveTo (7.2, -21.0)
            ctx.lineTo (8.2, -12.0)
            ctx.stroke ()
    | Chrono ->
        // Lange Robe mit türkisem Innenfutter und Zahnrad-Emblem
        poly ctx [ (-8.0, -22.0); (8.0, -22.0); (10.0, -6.0); (15.0, 22.0); (-15.0, 22.0); (-10.0, -6.0) ]
        outlinedFill ctx pal pal.Armor 2.6
        fill ctx pal.ArmorShade
        poly ctx [ (-8.0, -22.0); (-2.0, -22.0); (-4.0, 22.0); (-15.0, 22.0); (-10.0, -6.0) ]
        ctx.fill ()
        fill ctx pal.Under
        poly ctx [ (1.0, -6.0); (5.0, -6.0); (8.0, 22.0); (2.0, 22.0) ]
        ctx.fill ()
        fill ctx pal.Trim
        ctx.fillRect (-14.0, 19.0, 29.0, 3.0)
        ctx.fillRect (-9.0, -6.0, 19.0, 3.0)

        if shaded then
            ctx.save ()
            ctx.translate (2.0, -14.0)
            ctx.rotate (float frame * 0.03)
            fill ctx pal.Trim

            for k in 0..7 do
                ctx.save ()
                ctx.rotate (float k * Math.PI / 4.0)
                ctx.fillRect (-1.0, -5.6, 2.0, 2.4)
                ctx.restore ()

            circle ctx 0.0 0.0 3.8
            ctx.fill ()
            fill ctx pal.Under
            circle ctx 0.0 0.0 1.6
            ctx.fill ()
            ctx.restore ()
    | Bramble ->
        // Breiter Fellkörper mit Rindenplatten und Moos
        roundRect ctx -11.0 -24.0 22.0 26.0 9.0
        outlinedFill ctx pal pal.Under 2.8
        fill ctx pal.UnderShade
        roundRect ctx -11.0 -24.0 9.0 26.0 7.0
        ctx.fill ()
        fill ctx "rgba(255,240,200,0.35)"
        oval ctx 3.0 -8.0 5.0 7.0
        ctx.fill ()
        poly ctx [ (-10.0, -22.0); (6.0, -24.0); (10.0, -16.0); (-4.0, -14.0) ]
        outlinedFill ctx pal pal.Armor 2.4
        fill ctx pal.Hair
        circle ctx -6.0 -20.0 2.6
        ctx.fill ()
        circle ctx 3.0 -21.0 2.0
        ctx.fill ()
        fill ctx pal.ArmorShade
        ctx.fillRect (-10.0, -2.0, 20.0, 4.0)

        if shaded then
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.6
            ctx.beginPath ()
            ctx.moveTo (10.0, -20.0)
            ctx.lineTo (10.0, -6.0)
            ctx.stroke ()
    | Ronin ->
        // Kimono mit gekreuztem Kragen, Obi und Hakama-Ansatz
        poly ctx [ (-9.0, 0.0); (9.0, 0.0); (11.0, 9.5); (-11.0, 9.5) ]
        outlinedFill ctx pal pal.Under 2.2
        poly ctx [ (-8.6, -22.0); (8.6, -22.0); (9.6, -4.0); (-9.6, -4.0) ]
        outlinedFill ctx pal pal.Cloth 2.4
        fill ctx pal.ClothShade
        poly ctx [ (-8.6, -22.0); (-2.0, -22.0); (-3.0, -4.0); (-9.6, -4.0) ]
        ctx.fill ()
        stroke ctx pal.Trim
        ctx.lineWidth <- 2.2
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (-5.5, -22.0)
        ctx.lineTo (3.5, -9.5)
        ctx.moveTo (5.5, -22.0)
        ctx.lineTo (-0.5, -13.0)
        ctx.stroke ()
        poly ctx [ (-9.8, -6.5); (9.8, -6.5); (9.4, 1.0); (-9.4, 1.0) ]
        outlinedFill ctx pal pal.Armor 2.0
        fill ctx pal.Trim
        roundRect ctx -2.2 -5.6 4.4 5.6 1.2
        ctx.fill ()

        if shaded then
            stroke ctx pal.Rim
            ctx.lineWidth <- 1.4
            ctx.beginPath ()
            ctx.moveTo (8.0, -21.0)
            ctx.lineTo (9.0, -7.0)
            ctx.stroke ()
    | Lyra ->
        // Tunika mit Umhangkragen und Gürtel
        poly ctx [ (-8.0, -22.0); (8.0, -22.0); (8.5, -2.0); (11.0, 9.0); (-11.0, 9.0); (-8.5, -2.0) ]
        outlinedFill ctx pal pal.Cloth 2.4
        fill ctx pal.ClothShade
        poly ctx [ (-8.0, -22.0); (-2.0, -22.0); (-3.0, 9.0); (-11.0, 9.0); (-8.5, -2.0) ]
        ctx.fill ()
        poly ctx [ (-9.0, -24.0); (9.0, -24.0); (6.0, -17.0); (-6.0, -17.0) ]
        outlinedFill ctx pal pal.Under 2.0
        poly ctx [ (-8.8, -4.0); (8.8, -4.0); (8.6, -0.5); (-8.6, -0.5) ]
        outlinedFill ctx pal pal.Armor 1.8
        fill ctx pal.Trim
        circle ctx 0.0 -2.2 1.8
        ctx.fill ()
    | Gorm ->
        // Massiger Felsrumpf mit glühendem Kern
        ctx.beginPath ()
        ctx.moveTo (-14.0, -24.0)
        ctx.quadraticCurveTo (0.0, -30.0, 14.0, -24.0)
        ctx.quadraticCurveTo (17.0, -10.0, 11.0, 6.0)
        ctx.quadraticCurveTo (0.0, 10.0, -11.0, 6.0)
        ctx.quadraticCurveTo (-17.0, -10.0, -14.0, -24.0)
        ctx.closePath ()
        outlinedFill ctx pal pal.ArmorShade 3.2
        fill ctx pal.Armor
        ctx.beginPath ()
        ctx.moveTo (-6.0, -25.0)
        ctx.quadraticCurveTo (8.0, -27.0, 13.0, -22.0)
        ctx.quadraticCurveTo (15.0, -10.0, 10.0, 4.0)
        ctx.quadraticCurveTo (2.0, 6.0, -4.0, 4.0)
        ctx.closePath ()
        ctx.fill ()
        // Moosflecken
        fill ctx pal.Hair
        oval ctx -8.0 -22.0 5.0 2.6
        ctx.fill ()
        oval ctx 9.0 -2.0 3.6 2.0
        ctx.fill ()
        // Glühender Kern mit Rissen
        ctx.save ()

        if shaded then
            ctx.shadowColor <- pal.Glow
            ctx.shadowBlur <- 12.0

        stroke ctx pal.Glow
        ctx.lineWidth <- 1.8
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (2.0, -18.0)
        ctx.lineTo (-1.0, -12.0)
        ctx.lineTo (3.0, -8.0)
        ctx.lineTo (0.0, -2.0)
        ctx.moveTo (-1.0, -12.0)
        ctx.lineTo (-6.0, -10.0)
        ctx.stroke ()
        fill ctx pal.Glow
        circle ctx 2.0 -11.0 2.6
        ctx.fill ()
        ctx.restore ()
    | Specter ->
        // Schlanker Schattenmantel mit karmesinroter Schärpe
        poly ctx [ (-6.5, -21.0); (6.5, -21.0); (7.0, -2.0); (9.0, 10.0); (-9.0, 10.0); (-7.0, -2.0) ]
        outlinedFill ctx pal pal.Armor 2.4
        fill ctx pal.ArmorShade
        poly ctx [ (-6.5, -21.0); (-1.0, -21.0); (-2.0, 10.0); (-9.0, 10.0); (-7.0, -2.0) ]
        ctx.fill ()
        stroke ctx pal.Trim
        ctx.lineWidth <- 2.6
        ctx.beginPath ()
        ctx.moveTo (-6.0, -20.0)
        ctx.lineTo (6.0, -4.0)
        ctx.stroke ()
        fill ctx pal.UnderShade
        ctx.fillRect (-7.0, -3.0, 14.0, 3.6)
        fill ctx pal.Trim
        poly ctx [ (4.0, -2.0); (7.0, -2.0); (8.0, 6.0) ]
        ctx.fill ()

    ctx.restore ()

/// Leuchtendes Energieschwert: Griff, Parierstange, Klinge mit Glow
let private drawSword (ctx: Ctx) (pal: Palette) (hand: Vec2) (angle: float) (shaded: bool) =
    ctx.save ()
    ctx.translate (hand.X, hand.Y)
    ctx.rotate angle
    stroke ctx pal.Outline
    ctx.lineCap <- "round"
    ctx.lineWidth <- 5.4
    ctx.beginPath ()
    ctx.moveTo (-6.0, 0.0)
    ctx.lineTo (3.0, 0.0)
    ctx.stroke ()
    stroke ctx pal.UnderShade
    ctx.lineWidth <- 2.6
    ctx.stroke ()
    poly ctx [ (2.5, -6.0); (6.0, -4.0); (6.0, 4.0); (2.5, 6.0) ]
    outlinedFill ctx pal pal.Trim 2.4
    let blade = [ (6.0, -3.0); (36.0, -2.2); (45.0, 0.0); (36.0, 2.2); (6.0, 3.0) ]

    if shaded then
        ctx.save ()
        ctx.shadowColor <- pal.Glow
        ctx.shadowBlur <- 14.0
        poly ctx blade
        stroke ctx pal.UnderShade
        ctx.lineWidth <- 4.0
        ctx.lineJoin <- "round"
        ctx.stroke ()
        fill ctx pal.Glow
        ctx.fill ()
        ctx.restore ()
        fill ctx "#ffffff"
        poly ctx [ (7.0, -1.2); (36.0, -0.9); (42.0, 0.0); (36.0, 0.9); (7.0, 1.2) ]
        ctx.fill ()
    else
        poly ctx blade
        outlinedFill ctx pal pal.Glow 3.0

    ctx.restore ()

/// Mondbogen: geschwungene Wurfarme mit Sehne und Sternspitze
let private drawBow (ctx: Ctx) (pal: Palette) (hand: Vec2) (angle: float) (shaded: bool) =
    ctx.save ()
    ctx.translate (hand.X, hand.Y)
    ctx.rotate angle
    ctx.beginPath ()
    ctx.moveTo (-25.0, 1.0)
    ctx.quadraticCurveTo (-12.0, 13.0, 0.0, 10.0)
    ctx.quadraticCurveTo (12.0, 13.0, 25.0, 1.0)
    ctx.lineCap <- "round"
    stroke ctx pal.Outline
    ctx.lineWidth <- 5.6
    ctx.stroke ()
    stroke ctx pal.Trim
    ctx.lineWidth <- 2.8
    ctx.stroke ()
    // Sehne
    stroke ctx (if shaded then "rgba(230,240,255,0.95)" else pal.Glow)
    ctx.lineWidth <- 1.1
    ctx.beginPath ()
    ctx.moveTo (-25.0, 1.0)
    ctx.lineTo (25.0, 1.0)
    ctx.stroke ()
    // Griff
    roundRect ctx -3.5 6.0 7.0 6.0 2.0
    outlinedFill ctx pal pal.Armor 1.6

    if shaded then
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"

        for tipX in [ -25.0; 25.0 ] do
            let g = ctx.createRadialGradient (tipX, 1.0, 0.5, tipX, 1.0, 7.0)
            g.addColorStop (0.0, "rgba(220,235,255,0.9)")
            g.addColorStop (1.0, "rgba(140,170,255,0)")
            ctx.fillStyle <- !^g
            circle ctx tipX 1.0 7.0
            ctx.fill ()

        ctx.restore ()

    ctx.restore ()

/// Katana: lange, leicht gebogene Klinge mit runder Tsuba
let private drawKatana (ctx: Ctx) (pal: Palette) (hand: Vec2) (angle: float) (shaded: bool) =
    ctx.save ()
    ctx.translate (hand.X, hand.Y)
    ctx.rotate angle
    // Griff mit Wicklung
    stroke ctx pal.Outline
    ctx.lineCap <- "round"
    ctx.lineWidth <- 5.0
    ctx.beginPath ()
    ctx.moveTo (-11.0, 0.0)
    ctx.lineTo (3.0, 0.0)
    ctx.stroke ()
    stroke ctx pal.Armor
    ctx.lineWidth <- 2.6
    ctx.stroke ()
    fill ctx pal.Trim

    for k in 0..3 do
        ctx.fillRect (-9.0 + float k * 3.2, -1.2, 1.4, 2.4)

    ctx.beginPath ()
    ctx.ellipse (4.0, 0.0, 2.0, 5.0, 0.0, 0.0, Math.PI * 2.0)
    outlinedFill ctx pal pal.Trim 2.0
    // Klinge
    ctx.beginPath ()
    ctx.moveTo (6.0, -2.2)
    ctx.quadraticCurveTo (30.0, -4.2, 52.0, -6.5)
    ctx.quadraticCurveTo (48.0, -2.0, 44.0, 0.4)
    ctx.quadraticCurveTo (26.0, 1.6, 6.0, 1.8)
    ctx.closePath ()

    if shaded then
        ctx.save ()
        ctx.shadowColor <- pal.Glow
        ctx.shadowBlur <- 10.0
        stroke ctx pal.Outline
        ctx.lineWidth <- 3.0
        ctx.lineJoin <- "round"
        ctx.stroke ()
        let steel = ctx.createLinearGradient (0.0, -5.0, 0.0, 2.0)
        steel.addColorStop (0.0, "#ffffff")
        steel.addColorStop (0.5, "#d8dce8")
        steel.addColorStop (1.0, "#8a90a8")
        ctx.fillStyle <- !^steel
        ctx.fill ()
        ctx.restore ()
        stroke ctx "rgba(255,255,255,0.9)"
        ctx.lineWidth <- 0.9
        ctx.beginPath ()
        ctx.moveTo (8.0, -1.4)
        ctx.quadraticCurveTo (30.0, -3.2, 50.0, -5.8)
        ctx.stroke ()
    else
        outlinedFill ctx pal pal.Glow 2.4

    ctx.restore ()

/// Kunai in Zephyrs Hand
let private drawKunai (ctx: Ctx) (pal: Palette) (hand: Vec2) (angle: float) (shaded: bool) =
    ctx.save ()
    ctx.translate (hand.X, hand.Y)
    ctx.rotate angle
    stroke ctx pal.Outline
    ctx.lineWidth <- 2.0
    circle ctx -6.0 0.0 2.4
    ctx.stroke ()
    stroke ctx pal.Outline
    ctx.lineWidth <- 4.0
    ctx.lineCap <- "round"
    ctx.beginPath ()
    ctx.moveTo (-3.5, 0.0)
    ctx.lineTo (2.0, 0.0)
    ctx.stroke ()
    poly ctx [ (2.0, -3.0); (15.0, 0.0); (2.0, 3.0) ]
    outlinedFill ctx pal "#dfe6f5" 2.0

    if shaded then
        fill ctx pal.Trim
        poly ctx [ (3.5, -0.6); (12.0, 0.0); (3.5, 0.6) ]
        ctx.fill ()

    ctx.restore ()

/// Lanze der Valkyrie: langer Schaft, leuchtende Blattklinge
let private drawSpear (ctx: Ctx) (pal: Palette) (hand: Vec2) (angle: float) (shaded: bool) =
    ctx.save ()
    ctx.translate (hand.X, hand.Y)
    ctx.rotate angle
    stroke ctx pal.Outline
    ctx.lineCap <- "round"
    ctx.lineWidth <- 5.0
    ctx.beginPath ()
    ctx.moveTo (-26.0, 0.0)
    ctx.lineTo (54.0, 0.0)
    ctx.stroke ()
    stroke ctx "#8a5a2a"
    ctx.lineWidth <- 2.4
    ctx.stroke ()
    fill ctx pal.Trim
    ctx.fillRect (50.0, -3.0, 5.0, 6.0)
    let blade = [ (54.0, -4.6); (66.0, -3.0); (78.0, 0.0); (66.0, 3.0); (54.0, 4.6) ]

    if shaded then
        ctx.save ()
        ctx.shadowColor <- pal.Glow
        ctx.shadowBlur <- 12.0
        poly ctx blade
        outlinedFill ctx pal pal.Glow 2.6
        ctx.restore ()
        fill ctx "#ffffff"
        poly ctx [ (56.0, -1.2); (72.0, 0.0); (56.0, 1.2) ]
        ctx.fill ()
    else
        poly ctx blade
        outlinedFill ctx pal pal.Glow 2.6

    ctx.restore ()

/// Runder Schild am hinteren Arm der Valkyrie
let private drawShieldDisc (ctx: Ctx) (pal: Palette) (hand: Vec2) (shaded: bool) =
    circle ctx hand.X hand.Y 10.5
    outlinedFill ctx pal pal.Armor 3.0
    fill ctx pal.Under
    circle ctx hand.X hand.Y 7.0
    ctx.fill ()
    fill ctx pal.Trim
    poly ctx [ (hand.X, hand.Y - 6.0); (hand.X + 4.0, hand.Y); (hand.X, hand.Y + 6.0); (hand.X - 4.0, hand.Y) ]
    ctx.fill ()

    if shaded then
        stroke ctx pal.Rim
        ctx.lineWidth <- 1.6
        ctx.beginPath ()
        ctx.arc (hand.X, hand.Y, 8.6, Math.PI * 1.1, Math.PI * 1.6)
        ctx.stroke ()

/// Zeitstab: Schaft mit schwebender Uhren-Kugel
let private drawStaff (ctx: Ctx) (pal: Palette) (hand: Vec2) (angle: float) (shaded: bool) (frame: int) =
    ctx.save ()
    ctx.translate (hand.X, hand.Y)
    ctx.rotate angle
    stroke ctx pal.Outline
    ctx.lineCap <- "round"
    ctx.lineWidth <- 4.6
    ctx.beginPath ()
    ctx.moveTo (-30.0, 0.0)
    ctx.lineTo (30.0, 0.0)
    ctx.stroke ()
    stroke ctx pal.Trim
    ctx.lineWidth <- 2.0
    ctx.stroke ()
    // Halterung und Kugel
    stroke ctx pal.Trim
    ctx.lineWidth <- 2.0
    ctx.beginPath ()
    ctx.arc (37.0, 0.0, 8.0, 2.3, 4.0)
    ctx.stroke ()

    if shaded then
        ctx.save ()
        ctx.shadowColor <- pal.Glow
        ctx.shadowBlur <- 14.0
        circle ctx 38.0 0.0 6.4
        outlinedFill ctx pal pal.Glow 2.0
        ctx.restore ()
        stroke ctx pal.Outline
        ctx.lineWidth <- 1.2
        let a = float frame * 0.08
        ctx.beginPath ()
        ctx.moveTo (38.0, 0.0)
        ctx.lineTo (38.0 + cos a * 4.6, sin a * 4.6)
        ctx.moveTo (38.0, 0.0)
        ctx.lineTo (38.0 + cos (a * 0.1) * 3.2, sin (a * 0.1) * 3.2)
        ctx.stroke ()
    else
        circle ctx 38.0 0.0 6.4
        outlinedFill ctx pal pal.Glow 2.0

    ctx.restore ()

/// Schattendolch (Specter)
let private drawDagger (ctx: Ctx) (pal: Palette) (hand: Vec2) (angle: float) (shaded: bool) =
    ctx.save ()
    ctx.translate (hand.X, hand.Y)
    ctx.rotate angle
    stroke ctx pal.Outline
    ctx.lineWidth <- 4.0
    ctx.lineCap <- "round"
    ctx.beginPath ()
    ctx.moveTo (-4.0, 0.0)
    ctx.lineTo (2.0, 0.0)
    ctx.stroke ()
    poly ctx [ (2.0, -3.6); (8.0, -3.0); (19.0, 0.0); (8.0, 3.0); (2.0, 3.6) ]
    outlinedFill ctx pal "#2a1e36" 2.0

    if shaded then
        fill ctx pal.Glow
        poly ctx [ (4.0, -0.8); (16.0, 0.0); (4.0, 0.8) ]
        ctx.fill ()

    ctx.restore ()

/// Zopf der Valkyrie: geflochtene Glieder entlang der Stoffkette
let private drawBraid (ctx: Ctx) (pal: Palette) (p: Player) =
    let nodes = p.Cloth
    let count = List.length nodes
    let scale = (fighterStats p.Fighter).Scale

    nodes
    |> List.iteri (fun i node ->
        let k = 1.0 - float i / float (max 1 count) * 0.45
        circle ctx node.X node.Y (4.6 * k * scale)
        outlinedFill ctx pal (if i % 2 = 0 then pal.Cloth else pal.ClothShade) 2.4)

    match List.tryLast nodes with
    | Some tip ->
        circle ctx tip.X (tip.Y + 3.0) (2.6 * scale)
        outlinedFill ctx pal pal.Trim 1.8
    | None -> ()

/// Schweif von Bramble: dick, sich verjüngend, mit Moos-Quaste
let private drawBeastTail (ctx: Ctx) (pal: Palette) (p: Player) =
    let nodes = p.Cloth
    let count = List.length nodes
    let scale = (fighterStats p.Fighter).Scale

    if count >= 2 then
        ctx.lineCap <- "round"
        ctx.lineJoin <- "round"

        let path () =
            ctx.beginPath ()
            nodes |> List.iteri (fun i pt -> if i = 0 then ctx.moveTo (pt.X, pt.Y) else ctx.lineTo (pt.X, pt.Y))

        path ()
        stroke ctx pal.Outline
        ctx.lineWidth <- 12.0 * scale
        ctx.stroke ()
        path ()
        stroke ctx pal.Skin
        ctx.lineWidth <- 8.0 * scale
        ctx.stroke ()
        let tip = List.last nodes
        circle ctx tip.X tip.Y (6.0 * scale)
        outlinedFill ctx pal pal.Hair 2.6

/// Halbmondförmiger Hieb-Bogen (Smear): dünn am Ende, breit an der Schneide
let private crescent (ctx: Ctx) (center: Vec2) (fromAngle: float) (toAngle: float) (outer: float) (thickness: float) =
    let steps = 16
    let at (i: int) = float i / float steps
    let outerPts = [ for i in 0..steps -> center ++ polar (lerp fromAngle toAngle (at i)) outer ]

    let innerPts =
        [ for i in steps .. -1 .. 0 ->
              let k = at i
              center ++ polar (lerp fromAngle toAngle k) (outer - thickness * (k ** 1.6)) ]

    polyV ctx (outerPts @ innerPts)

let private slashColors (f: Fighter) (alt: bool) : string * string * string =
    match f, alt with
    | Aether, false -> "#2f6bff", "#7fe9ff", "#ffffff"
    | Aether, true -> "#ff2a52", "#ff8fa8", "#ffffff"
    | Ignis, false -> "#ff2a12", "#ff9a2e", "#fff3c4"
    | Ignis, true -> "#1a5fff", "#5fd6ff", "#e8fbff"
    | Titan, _ -> "#ff8a1a", "#ffd166", "#fffbe6"
    | Zephyr, false -> "#6a2cff", "#c9a2ff", "#ffffff"
    | Zephyr, true -> "#ff2a5f", "#ffa2c0", "#ffffff"
    | Valkyrie, false -> "#ffb13d", "#ffe08a", "#ffffff"
    | Valkyrie, true -> "#c2456e", "#ffb3c6", "#ffffff"
    | Chrono, false -> "#1a8f8c", "#4fe0c8", "#ffffff"
    | Chrono, true -> "#b8741c", "#ffb347", "#fffbe6"
    | Bramble, false -> "#3d8f23", "#a8f06a", "#f2ffe0"
    | Bramble, true -> "#c2456e", "#ff8fb1", "#fff0f5"
    | Lyra, false -> "#4a54c8", "#c9d2ff", "#ffffff"
    | Lyra, true -> "#c2456e", "#ffb3c6", "#ffffff"
    | Gorm, false -> "#a8641c", "#ffb347", "#fff2d6"
    | Gorm, true -> "#1a6fa8", "#58c7ff", "#e8fbff"
    | Ronin, false -> "#c2102e", "#ff8aa0", "#ffffff"
    | Ronin, true -> "#2b4d8a", "#9fd0ff", "#ffffff"
    | Specter, false -> "#8a0f3a", "#ff4f7b", "#ffe0ea"
    | Specter, true -> "#1a5fb3", "#58c7ff", "#e8fbff"

let private drawSlash (ctx: Ctx) (f: Fighter) (alt: bool) (center: Vec2) (sw: Swing) =
    if sw.Alpha > 0.01 then
        let direction = if sw.Angle >= sw.From then 1.0 else -1.0
        let span = min (abs (sw.Angle - sw.From)) (rad 215.0)
        let tail = sw.Angle - direction * span
        let thick = (if sw.Power then 34.0 else 22.0) * (if f = Titan then 1.25 elif f = Zephyr then 0.8 else 1.0)
        let glow, body, core = slashColors f alt
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- sw.Alpha * 0.55
        fill ctx glow
        crescent ctx center tail sw.Angle (sw.Reach + 7.0) (thick * 1.5)
        ctx.fill ()
        ctx.globalAlpha <- sw.Alpha * 0.9
        fill ctx body
        crescent ctx center tail sw.Angle (sw.Reach + 2.0) thick
        ctx.fill ()
        ctx.globalAlpha <- sw.Alpha
        fill ctx core
        crescent ctx center (tail + direction * span * 0.35) sw.Angle sw.Reach (thick * 0.42)
        ctx.fill ()
        stroke ctx core
        ctx.lineWidth <- 1.6
        ctx.globalAlpha <- sw.Alpha * 0.7

        for i in 0..2 do
            let r = sw.Reach - thick * (0.55 + float i * 0.22)
            ctx.beginPath ()
            ctx.arc (center.X, center.Y, max 4.0 r, tail + direction * span * (0.5 + float i * 0.1), sw.Angle, direction < 0.0)
            ctx.stroke ()

        ctx.restore ()

/// Umhang aus der Stoffkette: breiter werdende Bahn mit Cel-Schattenhälfte.
/// Chrono trägt eine weite Robe, Specter einen zerrissenen Schattenumhang.
let private drawCape (ctx: Ctx) (pal: Palette) (frame: int) (p: Player) =
    let nodes = p.Cloth
    let count = List.length nodes
    let scale = (fighterStats p.Fighter).Scale

    let widthK =
        match p.Fighter with
        | Chrono -> 1.2
        | Specter -> 1.05
        | _ -> 1.0

    if count >= 3 then
        let t = float frame

        let rows =
            nodes
            |> List.mapi (fun i node ->
                let k = float i / float (count - 1)
                let prev = List.item (max 0 (i - 1)) nodes
                let next = List.item (min (count - 1) (i + 1)) nodes
                let d = Vec2.normalize (v (next.X - prev.X) (next.Y - prev.Y))
                let n = v (-d.Y) d.X
                let flutter = sin (t * 0.24 - float i * 0.95) * 3.0 * k
                let c = node ++ Vec2.scale flutter n
                let w = (6.0 + 11.0 * k) * scale * widthK
                c ++ Vec2.scale w n, c, c ++ Vec2.scale -w n)

        let lefts = rows |> List.map (fun (l, _, _) -> l)
        let mids = rows |> List.map (fun (_, m, _) -> m)
        let rights = rows |> List.map (fun (_, _, r) -> r)
        let hemL = List.last lefts
        let hemR = List.last rights
        let last = List.last mids
        let before = List.item (count - 2) mids
        let down = Vec2.normalize (v (last.X - before.X) (last.Y - before.Y))

        // Zerrissener Saum: Zacken zwischen den Saumecken
        let hem =
            if p.Fighter = Specter then
                [ for k in 1..4 ->
                      let pt = mix hemR hemL (float k / 5.0)
                      if k % 2 = 1 then pt ++ Vec2.scale (9.0 * scale) down else pt ++ Vec2.scale (2.0 * scale) down ]
            else
                []

        polyV ctx (lefts @ List.rev hem @ List.rev rights)
        outlinedFill ctx pal pal.Cloth 3.2
        fill ctx pal.ClothShade
        polyV ctx (mids @ List.rev rights)
        ctx.fill ()

        if p.Fighter <> Specter then
            stroke ctx pal.Trim
            ctx.lineWidth <- 2.0
            ctx.beginPath ()
            ctx.moveTo (hemL.X, hemL.Y)
            ctx.lineTo (hemR.X, hemR.Y)
            ctx.stroke ()

/// Stirnband-Enden (Ignis) bzw. langer Schal (Zephyr): flatternde Bänder
let private drawTails (ctx: Ctx) (pal: Palette) (frame: int) (p: Player) =
    let nodes = p.Cloth
    let count = List.length nodes
    let scarf = p.Fighter = Zephyr

    if count >= 3 then
        let t = float frame

        for tail in 0..1 do
            let used = if tail = 0 then nodes else List.truncate (count - 1) nodes

            let pts =
                used
                |> List.mapi (fun i node ->
                    let wave = sin (t * 0.3 - float i * 1.1 + float tail * 1.9) * float i * (if scarf then 1.3 else 0.9)
                    node ++ v 0.0 (wave + float tail * 3.0))

            ctx.lineCap <- "round"
            ctx.lineJoin <- "round"

            let path () =
                ctx.beginPath ()
                pts |> List.iteri (fun i pt -> if i = 0 then ctx.moveTo (pt.X, pt.Y) else ctx.lineTo (pt.X, pt.Y))

            let width = if scarf then 7.0 - float tail * 1.4 else 4.0
            path ()
            stroke ctx pal.Outline
            ctx.lineWidth <- width + 3.2
            ctx.stroke ()
            path ()
            stroke ctx (if tail = 0 then pal.Cloth else pal.ClothShade)
            ctx.lineWidth <- width
            ctx.stroke ()

let private expressionOf (p: Player) : Expression =
    match p.State with
    | Hitstun
    | Helpless -> Hurt
    | Shielding
    | Dodging -> Closed
    | AttackStartup _
    | AttackActive _
    | AttackRecovery _ -> Fierce
    | _ -> Calm

let private auraColors (f: Fighter) : string * string =
    match f with
    | Aether -> "rgba(120,220,255,0.5)", "rgba(60,120,255,0.2)"
    | Ignis -> "rgba(255,170,60,0.6)", "rgba(255,70,20,0.28)"
    | Titan -> "rgba(255,220,110,0.55)", "rgba(255,150,30,0.25)"
    | Zephyr -> "rgba(200,160,255,0.5)", "rgba(120,60,255,0.22)"
    | Valkyrie -> "rgba(255,240,170,0.55)", "rgba(255,200,80,0.24)"
    | Chrono -> "rgba(120,255,230,0.5)", "rgba(40,180,160,0.22)"
    | Bramble -> "rgba(190,255,120,0.5)", "rgba(90,200,40,0.22)"
    | Specter -> "rgba(255,90,140,0.5)", "rgba(80,10,60,0.3)"
    | Ronin -> "rgba(255,170,190,0.5)", "rgba(200,20,60,0.22)"
    | Lyra -> "rgba(210,220,255,0.55)", "rgba(90,100,220,0.22)"
    | Gorm -> "rgba(255,190,90,0.55)", "rgba(200,110,30,0.25)"

let private drawFighter (ctx: Ctx) (frame: int) (mode: RenderMode) (alt: bool) (p: Player) =
    let f = p.Fighter
    let s = fighterStats f
    let swing = swingOf p
    let pose = poseOf frame p swing
    let rig = buildRig pose
    let mid = -bodyHeight / 2.0

    let pal, shaded =
        match mode with
        | Silhouette c -> solidPalette c, false
        | Shaded -> if p.HitFlash > 0 then solidPalette "#ffffff", false else paletteFor f alt, true

    // Zephyrs Teleport: im Startup verblassen, nach dem Sprung wieder auftauchen
    let teleportAlpha =
        match p.State with
        | AttackStartup UpSpecial when f = Zephyr ->
            1.0 - clamp01 (float p.StateFrame / float (Physics.moveData f UpSpecial).Startup)
        | AttackActive UpSpecial when f = Zephyr -> 0.5
        | _ -> 1.0

    let alpha =
        if p.State = Dodging then 0.45
        elif shaded && p.Invuln > 0 && p.State <> AttackStartup UpSpecial && frame % 6 < 3 then 0.55
        else teleportAlpha

    ctx.save ()
    ctx.globalAlpha <- alpha

    // Regenbogen-Aura: Smash-Ball eingesammelt
    if shaded && p.HasFinal then
        let c = Physics.bodyCenter p
        let hue = (frame * 6) % 360
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let r = 72.0 * s.Scale * (1.0 + 0.08 * sin (float frame * 0.3))
        let aura = ctx.createRadialGradient (c.X, c.Y, 6.0, c.X, c.Y, r)
        aura.addColorStop (0.0, sprintf "hsla(%d,100%%,78%%,0.7)" hue)
        aura.addColorStop (0.55, sprintf "hsla(%d,100%%,60%%,0.35)" ((hue + 120) % 360))
        aura.addColorStop (1.0, "rgba(0,0,0,0)")
        ctx.fillStyle <- !^aura
        ctx.fillRect (c.X - r, c.Y - r, r * 2.0, r * 2.0)
        ctx.restore ()

    // Stoff liegt in Weltkoordinaten hinter der Figur
    match f with
    | Aether
    | Chrono
    | Specter -> drawCape ctx pal frame p
    | Ignis
    | Ronin
    | Zephyr -> drawTails ctx pal frame p
    | Valkyrie
    | Lyra -> drawBraid ctx pal p
    | Bramble -> drawBeastTail ctx pal p
    | Titan
    | Gorm -> ()

    ctx.translate (p.Pos.X, p.Pos.Y)
    ctx.scale (p.Facing * s.Scale, s.Scale)
    ctx.translate (0.0, mid)
    ctx.rotate pose.Spin
    ctx.translate (0.0, -mid)
    ctx.scale (pose.Sx, pose.Sy)

    let attacking = swing.IsSome
    let power = swing |> Option.exists (fun sw -> sw.Power)

    let armored =
        match p.State with
        | AttackStartup m
        | AttackActive m -> (Physics.moveData f m).Armor
        | _ -> false

    // Auren bei Smash- und Spezialangriffen, goldenes Flackern bei Super-Armor
    if shaded && (power || armored) then
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let pulse = 0.75 + 0.25 * sin (float frame * 0.9)
        let inner, outer = if armored then "rgba(255,230,140,0.7)", "rgba(255,170,40,0.3)" else auraColors f
        let aura = ctx.createRadialGradient (0.0, mid, 6.0, 0.0, mid, 58.0 * pulse)
        aura.addColorStop (0.0, inner)
        aura.addColorStop (0.55, outer)
        aura.addColorStop (1.0, "rgba(0,0,0,0)")
        ctx.fillStyle <- !^aura
        ctx.fillRect (-64.0, mid - 64.0, 128.0, 128.0)
        ctx.restore ()

    // Up-B: Lichtsäule (Aether) bzw. Feuerstoß (Ignis) unter der Figur
    match p.State with
    | AttackActive UpSpecial when shaded && (f = Aether || f = Ignis) ->
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let jet = ctx.createLinearGradient (0.0, -10.0, 0.0, 62.0)
        jet.addColorStop (0.0, "rgba(255,255,255,0.95)")
        jet.addColorStop (0.35, (if f = Aether then "rgba(110,220,255,0.8)" else "rgba(255,180,70,0.85)"))
        jet.addColorStop (1.0, "rgba(0,0,0,0)")
        ctx.fillStyle <- !^jet
        poly ctx [ (-14.0, -12.0); (0.0, 52.0 + sin (float frame * 1.7) * 9.0); (14.0, -12.0) ]
        ctx.fill ()
        ctx.restore ()
    | _ -> ()

    let kick = swing |> Option.exists (fun sw -> sw.Style = Kick)
    let casting = swing |> Option.exists (fun sw -> sw.Style = Cast)

    // Reihenfolge von hinten nach vorn
    if f = Titan then
        let thrust =
            match p.State with
            | AttackActive UpSpecial -> true
            | _ -> false

        drawJetpack ctx pal rig pose.Lean (thrust && shaded) frame

    drawArm ctx pal f rig.ShoulderB rig.ElbowB rig.HandB true (shaded && casting && pose.HandB <> rig.HandB)

    if f = Valkyrie then
        drawShieldDisc ctx pal rig.HandB shaded

    if f = Specter && pose.ShowBlade then
        drawDagger ctx pal rig.HandB (pose.Blade + 0.7) shaded
    drawLeg ctx pal f rig.HipB rig.KneeB rig.FootB true
    drawLeg ctx pal f rig.HipF rig.KneeF rig.FootF false

    if shaded && kick && (f = Ignis || f = Zephyr || f = Specter || f = Bramble || f = Lyra) then
        let inner, outer = auraColors f
        glowAt ctx rig.FootF 20.0 inner outer

    drawTorso ctx pal f rig pose.Lean shaded frame
    ctx.save ()
    ctx.translate (rig.Head.X, rig.Head.Y)
    ctx.rotate rig.HeadAngle
    drawHead ctx pal f (expressionOf p) frame shaded
    ctx.restore ()

    if f = Aether && pose.ShowBlade then
        drawSword ctx pal rig.HandF pose.Blade shaded

    if f = Zephyr && pose.ShowBlade then
        drawKunai ctx pal rig.HandF pose.Blade shaded

    if f = Valkyrie && pose.ShowBlade then
        drawSpear ctx pal rig.HandF pose.Blade shaded

    if f = Chrono && pose.ShowBlade then
        drawStaff ctx pal rig.HandF pose.Blade shaded frame

    if f = Specter && pose.ShowBlade then
        drawDagger ctx pal rig.HandF pose.Blade shaded

    if f = Lyra && pose.ShowBlade then
        drawBow ctx pal rig.HandF pose.Blade shaded

    if f = Ronin && pose.ShowBlade then
        drawKatana ctx pal rig.HandF pose.Blade shaded

        // Konter-Haltung: Klinge blitzt auf
        match p.State with
        | AttackActive m when shaded && (Physics.moveData f m).Counter ->
            let tip = rig.HandF ++ polar pose.Blade 40.0
            ctx.save ()
            ctx.globalCompositeOperation <- "lighter"
            let glint = ctx.createRadialGradient (tip.X, tip.Y, 1.0, tip.X, tip.Y, 22.0)
            glint.addColorStop (0.0, "rgba(255,255,255,0.95)")
            glint.addColorStop (1.0, "rgba(255,200,220,0)")
            ctx.fillStyle <- !^glint
            circle ctx tip.X tip.Y 22.0
            ctx.fill ()
            ctx.restore ()
        | _ -> ()

    let fistGlow = shaded && attacking && not kick && (f = Ignis || (f = Titan || f = Gorm) && power || casting)
    drawArm ctx pal f rig.ShoulderF rig.ElbowF rig.HandF false fistGlow

    // Hieb-Bogen
    match swing with
    | Some sw when shaded -> drawSlash ctx f alt (if sw.Style = Kick then rig.HipF else rig.ShoulderF) sw
    | _ -> ()

    ctx.restore ()

    // Stasis: tickender Uhren-Ring um verlangsamte Kämpfer
    if shaded && p.Slow > 0 then
        let c = Physics.bodyCenter p
        let r = 40.0 * s.Scale
        ctx.save ()
        ctx.translate (c.X, c.Y)
        ctx.rotate (float frame * 0.04)
        ctx.globalAlpha <- 0.55 + 0.3 * sin (float frame * 0.3)
        stroke ctx "#4fe0c8"
        ctx.lineWidth <- 3.0

        for k in 0..11 do
            let a0 = float k * Math.PI / 6.0
            ctx.beginPath ()
            ctx.arc (0.0, 0.0, r, a0, a0 + 0.3)
            ctx.stroke ()

        stroke ctx "#bff8ee"
        ctx.lineWidth <- 2.0
        ctx.beginPath ()
        ctx.moveTo (0.0, 0.0)
        ctx.lineTo (0.0, -r * 0.7)
        ctx.moveTo (0.0, 0.0)
        ctx.lineTo (r * 0.45, 0.0)
        ctx.stroke ()
        ctx.restore ()


/// Kämpfer außerhalb der Arena zeichnen (Menüs, Siegerbildschirm): Position und Maßstab frei wählbar
let private drawFighterAt (ctx: Ctx) (frame: int) (p: Player) (alt: bool) (x: float) (y: float) (scale: float) =
    ctx.save ()
    ctx.translate (x, y)
    ctx.scale (scale, scale)
    ctx.translate (-p.Pos.X, -p.Pos.Y)
    drawFighter ctx frame Shaded alt p
    ctx.restore ()

/// Sechseckiges Energie-Schild mit Wabenmuster, das mit dem Schildwert schrumpft
let private drawShield (ctx: Ctx) (frame: int) (p: Player) =
    if p.State = Shielding then
        let s = fighterStats p.Fighter
        let ratio = clamp01 (p.Shield / shieldMax)
        let r = (24.0 + 28.0 * ratio) * s.Scale
        let cx = p.Pos.X
        let cy = p.Pos.Y - s.Height / 2.0
        let t = float frame
        let weak = ratio < 0.3
        let tint =
            if weak then "255,120,90"
            else
                match p.Fighter with
                | Aether -> "110,220,255"
                | Ignis -> "255,170,90"
                | Titan -> "255,214,110"
                | Zephyr -> "200,160,255"
                | Valkyrie -> "255,230,150"
                | Chrono -> "120,255,230"
                | Bramble -> "190,255,120"
                | Specter -> "255,90,140"
                | Ronin -> "255,120,150"
                | Lyra -> "200,210,255"
                | Gorm -> "255,180,90"

        let hexagon (x: float) (y: float) (radius: float) = polyV ctx [ for i in 0..5 -> v x y ++ polar (rad (60.0 * float i + 30.0)) radius ]

        ctx.save ()
        let flicker = if weak && frame % 4 < 2 then 0.6 else 1.0
        ctx.globalAlpha <- flicker
        let body = ctx.createRadialGradient (cx - r * 0.3, cy - r * 0.35, 2.0, cx, cy, r)
        body.addColorStop (0.0, "rgba(255,255,255,0.55)")
        body.addColorStop (0.55, sprintf "rgba(%s,0.28)" tint)
        body.addColorStop (1.0, sprintf "rgba(%s,0.55)" tint)
        ctx.fillStyle <- !^body
        hexagon cx cy r
        ctx.fill ()
        ctx.save ()
        hexagon cx cy r
        ctx.clip ()
        stroke ctx (sprintf "rgba(%s,0.6)" tint)
        ctx.lineWidth <- 1.2
        let cell = 9.0

        for row in -4..4 do
            for col in -4..4 do
                let hx = cx + float col * cell * 1.732 + (if row % 2 = 0 then 0.0 else cell * 0.866)
                let hy = cy + float row * cell * 1.5
                hexagon hx hy cell
                ctx.stroke ()

        let sweep = ((t * 2.2) % (r * 4.0)) - r * 2.0
        fill ctx "rgba(255,255,255,0.28)"
        poly ctx [ (cx + sweep - 8.0, cy - r); (cx + sweep + 8.0, cy - r); (cx + sweep - 12.0, cy + r); (cx + sweep - 28.0, cy + r) ]
        ctx.fill ()
        ctx.restore ()
        ctx.shadowColor <- sprintf "rgba(%s,0.9)" tint
        ctx.shadowBlur <- 14.0
        stroke ctx "rgba(255,255,255,0.95)"
        ctx.lineWidth <- 2.6
        ctx.lineJoin <- "round"
        hexagon cx cy r
        ctx.stroke ()
        ctx.shadowBlur <- 0.0
        stroke ctx "rgba(255,255,255,0.8)"
        ctx.lineWidth <- 3.0
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.arc (cx, cy, r * 0.68, Math.PI * 1.12, Math.PI * 1.42)
        ctx.stroke ()
        ctx.restore ()

let private drawRespawnPlatform (ctx: Ctx) (frame: int) (p: Player) =
    if p.State = Respawning then
        let x = p.Pos.X
        let y = p.Pos.Y + sin (float frame * 0.1) * 1.5
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let halo = ctx.createRadialGradient (x, y + 8.0, 4.0, x, y + 8.0, 90.0)
        halo.addColorStop (0.0, "rgba(255,246,190,0.6)")
        halo.addColorStop (1.0, "rgba(255,246,190,0)")
        ctx.fillStyle <- !^halo
        ctx.fillRect (x - 90.0, y - 80.0, 180.0, 180.0)
        ctx.restore ()
        poly ctx [ (x - 50.0, y); (x + 50.0, y); (x + 38.0, y + 13.0); (x, y + 22.0); (x - 38.0, y + 13.0) ]
        stroke ctx "#141a30"
        ctx.lineWidth <- 4.0
        ctx.lineJoin <- "round"
        ctx.stroke ()
        let gold = ctx.createLinearGradient (0.0, y, 0.0, y + 22.0)
        gold.addColorStop (0.0, "#fffbe0")
        gold.addColorStop (0.4, "#ffd76a")
        gold.addColorStop (1.0, "#c77f1e")
        ctx.fillStyle <- !^gold
        ctx.fill ()
        fill ctx "rgba(255,255,255,0.85)"
        roundRect ctx (x - 40.0) (y + 2.0) 80.0 3.0 1.5
        ctx.fill ()

let private drawNameTag (ctx: Ctx) (p: Player) =
    let x = p.Pos.X
    let y = p.Pos.Y - (fighterStats p.Fighter).Height - 26.0
    let color = slotColor p
    ctx.font <- sprintf "italic 15px %s" displayFont
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    outlinedText ctx (playerName p) x y color 4.0
    fill ctx color
    poly ctx [ (x - 5.0, y + 10.0); (x + 5.0, y + 10.0); (x, y + 16.0) ]
    ctx.fill ()

/// Klassische Smash-Lupe am Bildrand mit Mini-Charakterkopf und Richtungspfeil
let private clamp (lo: float) (hi: float) (v: float) = max lo (min hi v)

// ---------------------------------------------------------------------------
// Items, Stage-Ereignisse und Status-Effekte
// ---------------------------------------------------------------------------

let private heartPath (ctx: Ctx) (cx: float) (cy: float) (r: float) =
    ctx.beginPath ()
    ctx.moveTo (cx, cy + r * 0.9)
    ctx.bezierCurveTo (cx - r * 1.3, cy + r * 0.1, cx - r * 1.0, cy - r * 1.0, cx, cy - r * 0.35)
    ctx.bezierCurveTo (cx + r * 1.0, cy - r * 1.0, cx + r * 1.3, cy + r * 0.1, cx, cy + r * 0.9)
    ctx.closePath ()

let private drawItem (ctx: Ctx) (frame: int) (it: Item) =
    let t = float frame
    let blinkOut = it.Life < 120 && (frame / 4) % 2 = 0
    let bob = if it.OnGround then sin (t * 0.08 + it.Pos.X) * 3.0 - 4.0 else 0.0
    let cx = it.Pos.X
    let cy = it.Pos.Y - itemSize / 2.0 + bob

    if not blinkOut then
        ctx.save ()
        // Leuchthof
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"

        let glowColor =
            match it.Kind with
            | Heart -> "255,110,150"
            | PowerGem -> "255,80,90"
            | SpeedFeather -> "110,240,255"
            | Bomb -> if it.Age >= bombArmFrames && (frame / 6) % 2 = 0 then "255,60,40" else "255,190,90"

        let halo = ctx.createRadialGradient (cx, cy, 2.0, cx, cy, 30.0)
        halo.addColorStop (0.0, sprintf "rgba(%s,0.55)" glowColor)
        halo.addColorStop (1.0, sprintf "rgba(%s,0)" glowColor)
        ctx.fillStyle <- !^halo
        circle ctx cx cy 30.0
        ctx.fill ()
        ctx.restore ()

        match it.Kind with
        | Heart ->
            heartPath ctx cx cy 13.0
            stroke ctx "#3a0612"
            ctx.lineWidth <- 4.0
            ctx.stroke ()
            let g = ctx.createRadialGradient (cx - 4.0, cy - 6.0, 1.0, cx, cy, 16.0)
            g.addColorStop (0.0, "#ffd0dc")
            g.addColorStop (0.4, "#ff4f7b")
            g.addColorStop (1.0, "#b0123e")
            ctx.fillStyle <- !^g
            ctx.fill ()
            fill ctx "rgba(255,255,255,0.8)"
            oval ctx (cx - 5.0) (cy - 5.0) 3.5 2.2
            ctx.fill ()
        | PowerGem ->
            ctx.translate (cx, cy)
            ctx.rotate (sin (t * 0.05) * 0.15)
            let gem = [ (0.0, -16.0); (11.0, -5.0); (7.0, 12.0); (-7.0, 12.0); (-11.0, -5.0) ]
            poly ctx gem
            stroke ctx "#2a0508"
            ctx.lineWidth <- 3.5
            ctx.lineJoin <- "round"
            ctx.stroke ()
            let g = ctx.createLinearGradient (-10.0, -16.0, 10.0, 12.0)
            g.addColorStop (0.0, "#ffd0d0")
            g.addColorStop (0.45, "#ff3b4e")
            g.addColorStop (1.0, "#7a0a18")
            ctx.fillStyle <- !^g
            ctx.fill ()
            fill ctx "rgba(255,255,255,0.55)"
            poly ctx [ (0.0, -16.0); (11.0, -5.0); (0.0, -2.0); (-11.0, -5.0) ]
            ctx.fill ()
        | SpeedFeather ->
            ctx.translate (cx, cy)
            ctx.rotate (-0.6 + sin (t * 0.1) * 0.2)
            ctx.beginPath ()
            ctx.moveTo (0.0, -17.0)
            ctx.bezierCurveTo (10.0, -10.0, 9.0, 8.0, 0.0, 16.0)
            ctx.bezierCurveTo (-9.0, 8.0, -10.0, -10.0, 0.0, -17.0)
            ctx.closePath ()
            stroke ctx "#05303a"
            ctx.lineWidth <- 3.0
            ctx.stroke ()
            let g = ctx.createLinearGradient (-8.0, 0.0, 8.0, 0.0)
            g.addColorStop (0.0, "#d8fbff")
            g.addColorStop (1.0, "#29b6d8")
            ctx.fillStyle <- !^g
            ctx.fill ()
            stroke ctx "#05303a"
            ctx.lineWidth <- 1.5
            ctx.beginPath ()
            ctx.moveTo (0.0, -14.0)
            ctx.lineTo (0.0, 20.0)
            ctx.stroke ()
        | Bomb ->
            let armed = it.Age >= bombArmFrames
            let pulse = if armed then 1.0 + 0.06 * sin (t * (if it.Life < 180 then 0.9 else 0.4)) else 1.0
            ctx.translate (cx, cy)
            ctx.scale (pulse, pulse)
            circle ctx 0.0 2.0 13.0
            stroke ctx "#05060c"
            ctx.lineWidth <- 3.5
            ctx.stroke ()
            let g = ctx.createRadialGradient (-4.0, -3.0, 1.0, 0.0, 2.0, 14.0)
            g.addColorStop (0.0, "#7a7f99")
            g.addColorStop (1.0, "#14161f")
            ctx.fillStyle <- !^g
            ctx.fill ()
            fill ctx (if armed && (frame / 6) % 2 = 0 then "#ff3b3b" else "#ffd166")
            circle ctx 0.0 4.0 3.0
            ctx.fill ()
            fill ctx "#3a3a48"
            roundRect ctx -4.0 -14.0 8.0 5.0 1.5
            ctx.fill ()
            stroke ctx "#c9a46a"
            ctx.lineWidth <- 2.0
            ctx.beginPath ()
            ctx.moveTo (0.0, -14.0)
            ctx.quadraticCurveTo (4.0, -20.0, 8.0, -19.0)
            ctx.stroke ()
            fill ctx (if (frame / 3) % 2 = 0 then "#fff6c0" else "#ff9a3d")
            circle ctx 8.0 -19.0 2.6
            ctx.fill ()

        ctx.restore ()

/// Warnungen und Effekte der Stage-Ereignisse (Weltkoordinaten)
let private drawHazard (ctx: Ctx) (frame: int) (l: Layout) (h: Hazard) =
    let t = float frame
    let warn, active = hazardTiming h.Kind
    let warning = h.Age < warn
    let k = if warning then float h.Age / float warn else float (h.Age - warn) / float active
    let blink = 0.5 + 0.5 * sin (t * 0.5)
    ctx.save ()

    match h.Kind with
    | WindGust ->
        ctx.globalCompositeOperation <- "lighter"
        let alpha = if warning then 0.25 * k else 0.55 * (1.0 - k * 0.5)
        stroke ctx (sprintf "rgba(230,245,255,%.2f)" alpha)
        ctx.lineCap <- "round"

        for i in 0..22 do
            let speed = 14.0 + hash i 301.0 * 10.0
            let span = screenWidth + 400.0
            let x0 = (t * speed + hash i 302.0 * span) % span - 200.0
            let x = if h.Dir > 0.0 then x0 else screenWidth - x0
            let y = 80.0 + hash i 303.0 * 560.0
            let len = 60.0 + hash i 304.0 * 140.0
            ctx.lineWidth <- 1.5 + hash i 305.0 * 2.0
            ctx.beginPath ()
            ctx.moveTo (x, y)
            ctx.quadraticCurveTo (x - h.Dir * len * 0.5, y - 8.0, x - h.Dir * len, y)
            ctx.stroke ()
    | Meteor ->
        for x in h.Xs do
            let gy = l.Top

            if warning then
                // Zielkreis am Boden und anfliegender Stern
                stroke ctx (sprintf "rgba(255,90,120,%.2f)" (0.5 + 0.5 * blink))
                ctx.lineWidth <- 4.0
                oval ctx x (gy - 2.0) (70.0 * (1.2 - k * 0.2)) 12.0
                ctx.stroke ()
                let sx = x + 520.0 * (1.0 - k)
                let sy = gy - 40.0 - 640.0 * (1.0 - k)
                ctx.globalCompositeOperation <- "lighter"
                let trail = ctx.createLinearGradient (sx + 160.0, sy - 200.0, sx, sy)
                trail.addColorStop (0.0, "rgba(180,140,255,0)")
                trail.addColorStop (1.0, "rgba(220,200,255,0.9)")
                ctx.strokeStyle <- !^trail
                ctx.lineWidth <- 14.0 * k + 4.0
                ctx.lineCap <- "round"
                ctx.beginPath ()
                ctx.moveTo (sx + 160.0, sy - 200.0)
                ctx.lineTo (sx, sy)
                ctx.stroke ()
                fill ctx "#ffffff"
                circle ctx sx sy (6.0 + 8.0 * k)
                ctx.fill ()
            else
                ctx.globalCompositeOperation <- "lighter"
                let r = 40.0 + k * 140.0
                let g = ctx.createRadialGradient (x, gy - 30.0, 4.0, x, gy - 30.0, r)
                g.addColorStop (0.0, sprintf "rgba(255,255,255,%.2f)" (1.0 - k))
                g.addColorStop (0.5, sprintf "rgba(190,150,255,%.2f)" (0.7 * (1.0 - k)))
                g.addColorStop (1.0, "rgba(120,80,255,0)")
                ctx.fillStyle <- !^g
                circle ctx x (gy - 30.0) r
                ctx.fill ()
    | LavaGeyser ->
        for x in h.Xs do
            if warning then
                // Brodelnde Stelle mit aufsteigender Hitze
                ctx.globalCompositeOperation <- "lighter"
                let g = ctx.createRadialGradient (x, l.Top, 2.0, x, l.Top, 60.0)
                g.addColorStop (0.0, sprintf "rgba(255,200,90,%.2f)" (0.4 + 0.5 * k * blink))
                g.addColorStop (1.0, "rgba(255,80,20,0)")
                ctx.fillStyle <- !^g
                ctx.fillRect (x - 60.0, l.Top - 60.0, 120.0, 70.0)

                for i in 0..4 do
                    let by = l.Top - ((t * 2.0 + float i * 12.0) % 50.0) * k
                    fill ctx "rgba(255,170,60,0.8)"
                    circle ctx (x + (hash i 311.0 - 0.5) * 50.0) by (2.0 + hash i 312.0 * 3.0)
                    ctx.fill ()
            else
                let rise = clamp01 (k * 5.0)
                let fall = clamp01 ((1.0 - k) * 4.0)
                let top = l.Top - 320.0 * rise
                let wobble = sin (t * 0.6) * 5.0
                ctx.globalAlpha <- fall
                ctx.beginPath ()
                ctx.moveTo (x - 34.0, l.Top + 10.0)
                ctx.bezierCurveTo (x - 44.0 + wobble, l.Top - 120.0, x - 28.0 - wobble, top + 40.0, x, top)
                ctx.bezierCurveTo (x + 28.0 + wobble, top + 40.0, x + 44.0 - wobble, l.Top - 120.0, x + 34.0, l.Top + 10.0)
                ctx.closePath ()
                let lava = ctx.createLinearGradient (x - 40.0, 0.0, x + 40.0, 0.0)
                lava.addColorStop (0.0, "#c2260c")
                lava.addColorStop (0.5, "#ffe08a")
                lava.addColorStop (1.0, "#c2260c")
                ctx.fillStyle <- !^lava
                ctx.fill ()
                stroke ctx "rgba(90,10,5,0.8)"
                ctx.lineWidth <- 3.0
                ctx.stroke ()
    | Icicles ->
        h.Xs
        |> List.iteri (fun i x ->
            if warning then
                // Schatten am Boden und zitternder Zapfen am oberen Rand
                fill ctx (sprintf "rgba(20,40,90,%.2f)" (0.2 + 0.35 * k))
                oval ctx x (l.Top + 2.0) (18.0 + 10.0 * k) 5.0
                ctx.fill ()
                let shake = sin (t * 1.3 + float i) * 2.0 * k
                fill ctx "rgba(210,245,255,0.95)"
                poly ctx [ (x - 12.0 + shake, -4.0); (x + 12.0 + shake, -4.0); (x + shake, 40.0) ]
                ctx.fill ()
            else
                let y = icicleY l (h.Age - warn) i

                if y < l.Top then
                    ctx.beginPath ()
                    ctx.moveTo (x - 13.0, y - 46.0)
                    ctx.lineTo (x + 13.0, y - 46.0)
                    ctx.quadraticCurveTo (x + 6.0, y - 10.0, x, y)
                    ctx.quadraticCurveTo (x - 6.0, y - 10.0, x - 13.0, y - 46.0)
                    ctx.closePath ()
                    stroke ctx "#10284a"
                    ctx.lineWidth <- 3.0
                    ctx.stroke ()
                    let ice = ctx.createLinearGradient (x - 12.0, 0.0, x + 12.0, 0.0)
                    ice.addColorStop (0.0, "#ffffff")
                    ice.addColorStop (1.0, "#7cc8f0")
                    ctx.fillStyle <- !^ice
                    ctx.fill ())
    | LaserSweep ->
        let side = if h.Dir > 0.0 then l.Left - 70.0 else l.Right + 70.0

        // Drohne als Laserquelle
        let droneX = if warning then side else laserX l h (h.Age - warn)
        let droneY = l.Top - 120.0 + sin (t * 0.1) * 6.0
        fill ctx "#1a1430"
        roundRect ctx (droneX - 22.0) (droneY - 9.0) 44.0 18.0 8.0
        ctx.fill ()
        fill ctx (if warning && blink > 0.5 then "#ff3b5c" else "#ff8aa0")
        circle ctx droneX (droneY + 6.0) 5.0
        ctx.fill ()
        stroke ctx "#8a80aa"
        ctx.lineWidth <- 2.0

        for dx in [ -18.0; 18.0 ] do
            ctx.beginPath ()
            ctx.moveTo (droneX + dx - 10.0, droneY - 12.0)
            ctx.lineTo (droneX + dx + 10.0, droneY - 12.0)
            ctx.stroke ()

        if warning then
            // Gestrichelte Warnlinie auf Knöchelhöhe
            ctx.setLineDash [| 12.0; 10.0 |]
            stroke ctx (sprintf "rgba(255,60,90,%.2f)" (0.3 + 0.5 * blink))
            ctx.lineWidth <- 3.0
            ctx.beginPath ()
            ctx.moveTo (l.Left - 40.0, l.Top - 20.0)
            ctx.lineTo (l.Right + 40.0, l.Top - 20.0)
            ctx.stroke ()
            ctx.setLineDash [||]
        else
            ctx.globalCompositeOperation <- "lighter"
            let x = droneX
            let fan = ctx.createLinearGradient (0.0, droneY, 0.0, l.Top)
            fan.addColorStop (0.0, "rgba(255,60,90,0.1)")
            fan.addColorStop (1.0, "rgba(255,60,90,0.65)")
            ctx.fillStyle <- !^fan
            poly ctx [ (x - 3.0, droneY + 8.0); (x + 3.0, droneY + 8.0); (x + 18.0, l.Top - 6.0); (x - 18.0, l.Top - 6.0) ]
            ctx.fill ()
            let wall = ctx.createLinearGradient (x - 18.0, 0.0, x + 18.0, 0.0)
            wall.addColorStop (0.0, "rgba(255,60,90,0)")
            wall.addColorStop (0.5, "rgba(255,230,240,0.95)")
            wall.addColorStop (1.0, "rgba(255,60,90,0)")
            ctx.fillStyle <- !^wall
            ctx.fillRect (x - 18.0, l.Top - 36.0, 36.0, 30.0)

    ctx.restore ()

/// Warnbanner oben in der Bildmitte (Bildschirmkoordinaten)
let private drawHazardBanner (ctx: Ctx) (frame: int) (h: Hazard) =
    let warn, _ = hazardTiming h.Kind

    if h.Age < warn then
        let k = float h.Age / float warn
        let slide = clamp01 (k * 6.0)
        let alpha = clamp01 ((1.0 - k) * 8.0) * slide
        ctx.save ()
        ctx.globalAlpha <- alpha
        ctx.translate (screenWidth / 2.0, 92.0)
        ctx.transform (1.0, 0.0, -0.2, 1.0, 0.0, 0.0)
        roundRect ctx -170.0 -20.0 340.0 40.0 8.0
        fill ctx (if (frame / 8) % 2 = 0 then "rgba(200,30,50,0.9)" else "rgba(120,10,30,0.9)")
        ctx.fill ()
        stroke ctx "#ffd166"
        ctx.lineWidth <- 3.0
        ctx.stroke ()
        ctx.restore ()
        ctx.save ()
        ctx.globalAlpha <- alpha
        ctx.font <- sprintf "italic 22px %s" displayFont
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        outlinedText ctx (sprintf "⚠  %s  ⚠" (hazardName h.Kind)) (screenWidth / 2.0) 93.0 "#ffffff" 5.0
        ctx.restore ()

/// Aura für Item-Effekte (hinter dem Kämpfer)
let private drawStatusAura (ctx: Ctx) (frame: int) (p: Player) =
    let t = float frame
    let s = fighterStats p.Fighter
    let cx = p.Pos.X
    let cy = p.Pos.Y - s.Height / 2.0

    if p.PowerTimer > 0 && p.State <> KO && p.State <> Respawning then
        let fade = clamp01 (float p.PowerTimer / 60.0)
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"

        for i in 0..5 do
            let a = t * 0.12 + float i * Math.PI / 3.0
            let r = s.Height * 0.55
            let fx = cx + cos a * r * 0.6
            let fy = cy + sin a * r * 0.3 + s.Height * 0.2 - ((t * 2.0 + float i * 9.0) % 30.0)
            let g = ctx.createRadialGradient (fx, fy, 1.0, fx, fy, 14.0)
            g.addColorStop (0.0, sprintf "rgba(255,120,90,%.2f)" (0.7 * fade))
            g.addColorStop (1.0, "rgba(255,40,60,0)")
            ctx.fillStyle <- !^g
            circle ctx fx fy 14.0
            ctx.fill ()

        ctx.restore ()

    // Aufladen eines Smash-Angriffs: pulsierendes Leuchten, das immer heller wird
    match p.State with
    | AttackStartup _ when p.Charge > 0 ->
        let k = float p.Charge / float maxCharge
        let pulse = 0.6 + 0.4 * sin (t * (0.5 + k * 0.8))
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let r = s.Height * (0.7 + 0.25 * k)
        let g = ctx.createRadialGradient (cx, cy, 4.0, cx, cy, r)
        g.addColorStop (0.0, sprintf "rgba(255,255,255,%.2f)" (0.35 * k * pulse))
        g.addColorStop (0.6, sprintf "rgba(255,230,140,%.2f)" (0.3 * k * pulse))
        g.addColorStop (1.0, "rgba(255,200,80,0)")
        ctx.fillStyle <- !^g
        circle ctx cx cy r
        ctx.fill ()
        ctx.restore ()
    | _ -> ()

    if p.SpeedTimer > 0 && abs p.Vel.X > 2.0 then
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        stroke ctx "rgba(120,240,255,0.55)"
        ctx.lineCap <- "round"

        for i in 0..3 do
            let y = cy - s.Height * 0.35 + float i * s.Height * 0.25
            let len = 26.0 + float ((frame + i * 3) % 7) * 4.0
            ctx.lineWidth <- 2.0
            ctx.beginPath ()
            ctx.moveTo (cx - p.Vel.X * 2.0, y)
            ctx.lineTo (cx - p.Vel.X * 2.0 - (if p.Vel.X > 0.0 then len else -len), y)
            ctx.stroke ()

        ctx.restore ()

/// Parry-Aufleuchten (vor dem Kämpfer)
let private drawParryFlash (ctx: Ctx) (p: Player) =
    if p.ParryFlash > 0 then
        let s = fighterStats p.Fighter
        let k = float p.ParryFlash / 22.0
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        stroke ctx (sprintf "rgba(125,249,255,%.2f)" k)
        ctx.lineWidth <- 4.0 * k + 1.0
        circle ctx p.Pos.X (p.Pos.Y - s.Height / 2.0) (s.Height * (0.6 + (1.0 - k) * 0.5))
        ctx.stroke ()
        ctx.restore ()

        if p.ParryFlash > 12 then
            ctx.save ()
            ctx.font <- sprintf "italic 18px %s" displayFont
            ctx.textAlign <- "center"
            outlinedText ctx "PARRY!" p.Pos.X (p.Pos.Y - s.Height - 34.0) "#7df9ff" 4.0
            ctx.restore ()

// ---------------------------------------------------------------------------
// Dynamische Kamera: zoomt heran, wenn die Kämpfer nah beieinander sind
// ---------------------------------------------------------------------------

let mutable private camX = screenWidth / 2.0
let mutable private camY = screenHeight / 2.0
let mutable private camZoom = 1.0
let mutable private camFrame = -1

let private maxZoom = 1.42

let private updateCamera (g: Game) =
    let alive =
        g.Players
        |> List.filter (fun p -> p.State <> KO && p.State <> Respawning)

    let tx, ty, tz =
        match alive, g.Ultra with
        | [], _
        | _, Some _ -> screenWidth / 2.0, screenHeight / 2.0, 1.0
        | ps, None ->
            let xs = ps |> List.map (fun p -> clamp -60.0 (screenWidth + 60.0) p.Pos.X)
            let tops = ps |> List.map (fun p -> clamp -60.0 (screenHeight + 60.0) (p.Pos.Y - (fighterStats p.Fighter).Height))
            let bottoms = ps |> List.map (fun p -> clamp -60.0 (screenHeight + 60.0) p.Pos.Y)
            let minX = List.min xs
            let maxX = List.max xs
            let minY = List.min tops
            let maxY = List.max bottoms
            let w = maxX - minX + 520.0
            let h = maxY - minY + 330.0
            let z = clamp 1.0 maxZoom (min (screenWidth / w) (screenHeight / h))
            (minX + maxX) / 2.0, (minY + maxY) / 2.0 - 10.0, z

    // Sprung bei neuem Match, sonst weich nachführen
    let snap = camFrame < 0 || g.MatchFrames < 2 || abs (g.Frame - camFrame) > 5
    camFrame <- g.Frame
    camZoom <- if snap then tz else camZoom + (tz - camZoom) * 0.05
    let halfW = screenWidth / 2.0 / camZoom
    let halfH = screenHeight / 2.0 / camZoom
    let cx = clamp halfW (screenWidth - halfW) tx
    let cy = clamp halfH (screenHeight - halfH) ty
    camX <- if snap then cx else camX + (cx - camX) * 0.08
    camY <- if snap then cy else camY + (cy - camY) * 0.08
    camX <- clamp halfW (screenWidth - halfW) camX
    camY <- clamp halfH (screenHeight - halfH) camY

let private applyCamera (ctx: Ctx) =
    ctx.translate (screenWidth / 2.0, screenHeight / 2.0)
    ctx.scale (camZoom, camZoom)
    ctx.translate (-camX, -camY)

/// Hintergrund mit schwächerem Zoom (Parallaxe)
let private applyBackgroundCamera (ctx: Ctx) =
    let z = 1.0 + (camZoom - 1.0) * 0.35
    let ox = (camX - screenWidth / 2.0) * 0.35
    let oy = (camY - screenHeight / 2.0) * 0.35
    ctx.translate (screenWidth / 2.0, screenHeight / 2.0)
    ctx.scale (z, z)
    ctx.translate (-screenWidth / 2.0 - ox, -screenHeight / 2.0 - oy)

let private toScreen (pos: Vec2) =
    { X = (pos.X - camX) * camZoom + screenWidth / 2.0
      Y = (pos.Y - camY) * camZoom + screenHeight / 2.0 }

let private drawOffscreenMarker (ctx: Ctx) (frame: int) (alt: bool) (p: Player) =
    let s = fighterStats p.Fighter
    let sp = toScreen { X = p.Pos.X; Y = p.Pos.Y - s.Height / 2.0 }
    let cx = sp.X
    let cy = sp.Y
    let off = cx < -6.0 || cx > screenWidth + 6.0 || cy < -6.0 || cy > screenHeight + 6.0

    if off && p.State <> KO && p.State <> Respawning then
        let mx = max 56.0 (min (screenWidth - 56.0) cx)
        let my = max 56.0 (min (screenHeight - 170.0) cy)
        let dir = Vec2.normalize (v (cx - mx) (cy - my))
        let color = slotColor p
        let r = 36.0
        let tip = v mx my ++ Vec2.scale (r + 20.0) dir
        let baseC = v mx my ++ Vec2.scale (r - 2.0) dir
        let n = v (-dir.Y) dir.X
        polyV ctx [ tip; baseC ++ Vec2.scale 13.0 n; baseC ++ Vec2.scale -13.0 n ]
        stroke ctx "#0b0d18"
        ctx.lineWidth <- 5.0
        ctx.lineJoin <- "round"
        ctx.stroke ()
        fill ctx color
        ctx.fill ()
        circle ctx mx my r
        stroke ctx "#0b0d18"
        ctx.lineWidth <- 9.0
        ctx.stroke ()
        let glass = ctx.createRadialGradient (mx - 10.0, my - 12.0, 2.0, mx, my, r)
        glass.addColorStop (0.0, "#f4fbff")
        glass.addColorStop (1.0, "#9fc6f2")
        ctx.fillStyle <- !^glass
        ctx.fill ()
        ctx.save ()
        circle ctx mx my (r - 1.0)
        ctx.clip ()
        ctx.translate (mx, my + 4.0)
        ctx.scale (p.Facing * 2.1, 2.1)
        drawHead ctx (paletteFor p.Fighter alt) p.Fighter (if p.State = Hitstun then Hurt else Calm) frame true
        ctx.restore ()
        circle ctx mx my r
        stroke ctx color
        ctx.lineWidth <- 5.0
        ctx.stroke ()
        stroke ctx "rgba(255,255,255,0.85)"
        ctx.lineWidth <- 3.0
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.arc (mx, my, r - 7.0, Math.PI * 1.1, Math.PI * 1.45)
        ctx.stroke ()
// ---------------------------------------------------------------------------
// Partikel & VFX
// ---------------------------------------------------------------------------

let private drawParticle (ctx: Ctx) (pt: Particle) =
    let ratio = clamp01 (float pt.Life / float pt.MaxLife)

    match pt.Kind with
    | Spark ->
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- ratio
        stroke ctx pt.Color
        ctx.lineWidth <- pt.Size
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (pt.Pos.X, pt.Pos.Y)
        ctx.lineTo (pt.Pos.X - pt.Vel.X * 2.0, pt.Pos.Y - pt.Vel.Y * 2.0)
        ctx.stroke ()
    | Ember ->
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- ratio
        fill ctx pt.Color
        circle ctx pt.Pos.X pt.Pos.Y (pt.Size * (0.4 + ratio * 0.6))
        ctx.fill ()
    | Flame ->
        // Zweifarbige Flammenzunge: außen Orange/Rot, innen Gelb
        ctx.globalAlpha <- clamp01 (ratio * 1.4) * 0.9
        fill ctx (if ratio > 0.55 then pt.Color else "#e8381a")
        circle ctx pt.Pos.X pt.Pos.Y pt.Size
        ctx.fill ()
        fill ctx (if ratio > 0.55 then "#ffe27a" else "#ff9a2e")
        circle ctx pt.Pos.X (pt.Pos.Y + pt.Size * 0.18) (pt.Size * 0.55 * ratio)
        ctx.fill ()
    | Bolt ->
        // Gezackter Blitz entlang von Vel, Zacken pro Lebensframe neu gewürfelt
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- ratio
        ctx.lineCap <- "round"
        ctx.lineJoin <- "round"
        let n = Vec2.normalize (v (-pt.Vel.Y) pt.Vel.X)
        let seed = pt.Life * 7 + int pt.Size * 13

        let path () =
            ctx.beginPath ()
            ctx.moveTo (pt.Pos.X, pt.Pos.Y)

            for i in 1..5 do
                let k = float i / 5.0
                let jitter = (hash (seed + i) (pt.Vel.X * 0.01) - 0.5) * 22.0 * (1.0 - k * 0.5)
                ctx.lineTo (pt.Pos.X + pt.Vel.X * k + n.X * jitter, pt.Pos.Y + pt.Vel.Y * k + n.Y * jitter)

        path ()
        stroke ctx pt.Color
        ctx.lineWidth <- pt.Size + 3.0
        ctx.stroke ()
        path ()
        stroke ctx "#ffffff"
        ctx.lineWidth <- max 1.0 (pt.Size * 0.6)
        ctx.stroke ()
    | SpeedLine ->
        ctx.globalAlpha <- ratio * 0.8
        stroke ctx pt.Color
        ctx.lineWidth <- pt.Size
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (pt.Pos.X, pt.Pos.Y)
        ctx.lineTo (pt.Pos.X + pt.Vel.X, pt.Pos.Y + pt.Vel.Y)
        ctx.stroke ()
    | Burst ->
        // Vierzackiger Trefferstern
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- ratio
        fill ctx pt.Color
        let long = pt.Size * 1.6
        let short = pt.Size * 0.22

        for k in 0..1 do
            let a = float k * Math.PI / 2.0 + 0.5
            let c = pt.Pos
            polyV ctx [ c ++ polar a long; c ++ polar (a + Math.PI / 2.0) short; c ++ polar (a + Math.PI) long; c ++ polar (a - Math.PI / 2.0) short ]
            ctx.fill ()

        circle ctx pt.Pos.X pt.Pos.Y (pt.Size * 0.4)
        ctx.fill ()
    | Smoke
    | Dust ->
        // Cel-Rauch: dunkler Rand, helle Kuppe
        ctx.globalAlpha <- ratio * 0.5
        fill ctx "rgba(60,66,96,0.5)"
        circle ctx pt.Pos.X pt.Pos.Y (pt.Size + 1.5)
        ctx.fill ()
        ctx.globalAlpha <- ratio * 0.6
        fill ctx pt.Color
        circle ctx (pt.Pos.X - pt.Size * 0.12) (pt.Pos.Y - pt.Size * 0.14) (pt.Size * 0.86)
        ctx.fill ()
    | Ring ->
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- ratio * 0.9
        stroke ctx pt.Color
        ctx.lineWidth <- 1.0 + ratio * 5.0
        circle ctx pt.Pos.X pt.Pos.Y pt.Size
        ctx.stroke ()
    | Leaf ->
        // Trudelndes Blatt
        ctx.globalAlpha <- ratio
        ctx.save ()
        ctx.translate (pt.Pos.X, pt.Pos.Y)
        ctx.rotate (float pt.Life * 0.25)
        fill ctx pt.Color
        oval ctx 0.0 0.0 pt.Size (pt.Size * 0.45)
        ctx.fill ()
        stroke ctx "rgba(30,60,10,0.6)"
        ctx.lineWidth <- 1.0
        ctx.beginPath ()
        ctx.moveTo (-pt.Size, 0.0)
        ctx.lineTo (pt.Size, 0.0)
        ctx.stroke ()
        ctx.restore ()
    | Shadow ->
        // Schattenschleier mit karmesinrotem Kern
        ctx.globalAlpha <- ratio * 0.55
        fill ctx pt.Color
        circle ctx pt.Pos.X pt.Pos.Y pt.Size
        ctx.fill ()
        ctx.globalAlpha <- ratio * 0.3
        fill ctx "#0b0612"
        circle ctx (pt.Pos.X + pt.Size * 0.2) (pt.Pos.Y - pt.Size * 0.2) (pt.Size * 0.6)
        ctx.fill ()
    | Rainbow ->
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- ratio
        fill ctx pt.Color
        circle ctx pt.Pos.X pt.Pos.Y (pt.Size * (0.5 + ratio * 0.5))
        ctx.fill ()

    ctx.globalCompositeOperation <- "source-over"
    ctx.globalAlpha <- 1.0

let private isBackParticle (pt: Particle) =
    match pt.Kind with
    | Smoke
    | Dust
    | SpeedLine -> true
    | _ -> false

/// Smash-Lichtstrahl beim KO: schießt von der Bildschirmkante ins Zentrum
let private drawBeam (ctx: Ctx) (frame: int) (b: KoBeam) =
    let k = clamp01 (float b.Life / float b.MaxLife)
    let age = float (b.MaxLife - b.Life)
    let length = min 1900.0 (age * 260.0 + 200.0)
    let width = (70.0 + 250.0 * k * k) * (0.92 + 0.08 * sin (float frame * 2.1))
    ctx.save ()
    ctx.translate (b.Origin.X, b.Origin.Y)
    ctx.rotate (atan2 b.Dir.Y b.Dir.X)
    ctx.globalCompositeOperation <- "lighter"
    ctx.globalAlpha <- clamp01 (k * 1.6)

    // Farbiger Außenstrahl, weißer Kern
    let outer = ctx.createLinearGradient (0.0, -width / 2.0, 0.0, width / 2.0)
    outer.addColorStop (0.0, "rgba(0,0,0,0)")
    outer.addColorStop (0.3, b.Color)
    outer.addColorStop (0.5, "#ffffff")
    outer.addColorStop (0.7, b.Color)
    outer.addColorStop (1.0, "rgba(0,0,0,0)")
    ctx.fillStyle <- !^outer
    poly ctx [ (-140.0, -width / 2.0); (length, -width * 0.16); (length + 120.0, 0.0); (length, width * 0.16); (-140.0, width / 2.0) ]
    ctx.fill ()
    fill ctx "#ffffff"
    poly ctx [ (-140.0, -width * 0.16); (length, -width * 0.05); (length + 80.0, 0.0); (length, width * 0.05); (-140.0, width * 0.16) ]
    ctx.fill ()

    // Lichtstreifen entlang des Strahls
    stroke ctx "#ffffff"
    ctx.lineCap <- "round"

    for i in 0..9 do
        let y = (hash i 3.0 - 0.5) * width * 0.9
        let x0 = ((hash i 5.0 * 900.0 + age * 90.0) % (length + 1.0))
        ctx.lineWidth <- 1.5 + hash i 7.0 * 3.0
        ctx.globalAlpha <- clamp01 k * 0.8
        ctx.beginPath ()
        ctx.moveTo (x0, y)
        ctx.lineTo (x0 + 120.0 + hash i 8.0 * 160.0, y * 0.8)
        ctx.stroke ()

    // Einschlagsblitz an der Kante
    ctx.globalAlpha <- clamp01 (k * 1.3)
    let flash = ctx.createRadialGradient (0.0, 0.0, 4.0, 0.0, 0.0, width * 1.1)
    flash.addColorStop (0.0, "#ffffff")
    flash.addColorStop (0.4, b.Color)
    flash.addColorStop (1.0, "rgba(0,0,0,0)")
    ctx.fillStyle <- !^flash
    circle ctx 0.0 0.0 (width * 1.1)
    ctx.fill ()
    ctx.restore ()

/// Radiale Speed-Lines der Impact-Frames
let private drawImpactLines (ctx: Ctx) (center: Vec2) (color: string) (frame: int) =
    fill ctx color

    for i in 0..33 do
        let angle = float i / 34.0 * tau + hash (i + frame) 2.0 * 0.12
        let inner = 70.0 + hash (i + frame * 3) 4.0 * 110.0
        let half = 0.008 + hash i 6.0 * 0.03
        polyV ctx [ center ++ polar angle inner; center ++ polar (angle - half) 1700.0; center ++ polar (angle + half) 1700.0 ]
        ctx.fill ()

// ---------------------------------------------------------------------------
// Smash-Ball und Ultra-Smash
// ---------------------------------------------------------------------------

/// Schwebender Smash-Ball: Regenbogenkugel mit rotierenden Bändern und Stern-Emblem
let private drawBall (ctx: Ctx) (frame: int) (b: SmashBall) =
    let t = float frame
    let r = ballRadius * (1.0 + 0.05 * sin (t * 0.2))
    let fadeOut = if b.Life < 0 then 0.8 else 1.0
    ctx.save ()
    ctx.translate (b.Pos.X, b.Pos.Y)
    ctx.globalAlpha <- fadeOut
    // Leuchthof
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"
    let hue = (frame * 5) % 360
    let halo = ctx.createRadialGradient (0.0, 0.0, r * 0.6, 0.0, 0.0, r * 3.0)
    halo.addColorStop (0.0, sprintf "hsla(%d,100%%,75%%,0.65)" hue)
    halo.addColorStop (1.0, "rgba(0,0,0,0)")
    ctx.fillStyle <- !^halo
    circle ctx 0.0 0.0 (r * 3.0)
    ctx.fill ()
    ctx.restore ()
    // Kugel mit Regenbogen-Bändern
    circle ctx 0.0 0.0 r
    stroke ctx "#0b0d18"
    ctx.lineWidth <- 4.0
    ctx.stroke ()
    ctx.save ()
    circle ctx 0.0 0.0 r
    ctx.clip ()

    for k in 0..5 do
        fill ctx (sprintf "hsl(%d,95%%,58%%)" ((hue + k * 60) % 360))
        ctx.save ()
        ctx.rotate (t * 0.05 + float k * Math.PI / 3.0)
        poly ctx [ (0.0, 0.0); (r * 2.0, -r * 0.6); (r * 2.0, r * 0.6) ]
        ctx.fill ()
        ctx.restore ()

    let shade = ctx.createRadialGradient (-r * 0.35, -r * 0.4, 2.0, 0.0, 0.0, r)
    shade.addColorStop (0.0, "rgba(255,255,255,0.65)")
    shade.addColorStop (0.6, "rgba(255,255,255,0)")
    shade.addColorStop (1.0, "rgba(0,0,0,0.35)")
    ctx.fillStyle <- !^shade
    ctx.fillRect (-r, -r, r * 2.0, r * 2.0)
    ctx.restore ()
    // Stern-Emblem
    fill ctx "#ffffff"
    stroke ctx "#0b0d18"
    ctx.lineWidth <- 2.0

    let star = [ for k in 0..7 -> let a = float k * Math.PI / 4.0 - Math.PI / 2.0 in (cos a * (if k % 2 = 0 then r * 0.62 else r * 0.2), sin a * (if k % 2 = 0 then r * 0.62 else r * 0.2)) ]

    poly ctx star
    ctx.stroke ()
    ctx.fill ()

    if b.Flash > 0 then
        fill ctx "rgba(255,255,255,0.8)"
        circle ctx 0.0 0.0 (r + 2.0)
        ctx.fill ()

    ctx.restore ()

/// Bildschirmfüllender Angriff des Ultra-Smash (nach dem Cut-In)
let private drawUltraStrike (ctx: Ctx) (frame: int) (u: Ultra) =
    let k = clamp01 (float (u.Timer - ultraCutIn) / float (ultraEnd - ultraCutIn))
    let t = float frame
    let s = fighterStats u.Fighter
    let o = u.Origin
    let grow = 1.0 - (1.0 - k) * (1.0 - k)
    ctx.save ()

    match u.Fighter with
    | Aether ->
        // Gigantischer Klingenstrahl
        ctx.globalCompositeOperation <- "lighter"
        let h = 230.0 * grow
        let x0 = o.X
        let x1 = if u.Facing > 0.0 then screenWidth + 80.0 else -80.0
        let beam = ctx.createLinearGradient (0.0, o.Y - h / 2.0, 0.0, o.Y + h / 2.0)
        beam.addColorStop (0.0, "rgba(60,120,255,0)")
        beam.addColorStop (0.3, "rgba(110,200,255,0.8)")
        beam.addColorStop (0.5, "rgba(255,255,255,1)")
        beam.addColorStop (0.7, "rgba(110,200,255,0.8)")
        beam.addColorStop (1.0, "rgba(60,120,255,0)")
        ctx.fillStyle <- !^beam
        ctx.fillRect (min x0 x1, o.Y - h / 2.0, abs (x1 - x0), h)
        fill ctx "#ffffff"

        for i in 0..9 do
            let y = o.Y + (hash i 3.0 - 0.5) * h
            let len = 200.0 + hash i 4.0 * 300.0
            let x = o.X + u.Facing * ((t * 40.0 + hash i 5.0 * 1200.0) % 1400.0)
            ctx.fillRect (min x (x - u.Facing * len), y - 1.5, len, 3.0)
    | Ignis ->
        // Inferno-Drache: Flammenwand mit Drachenkopf
        ctx.globalCompositeOperation <- "lighter"
        let head = o.X + u.Facing * grow * 1500.0

        for i in 0..40 do
            let x = o.X + u.Facing * float i * 36.0
            if (x - head) * u.Facing < 0.0 then
                let y = o.Y + sin (float i * 0.6 + t * 0.4) * 30.0
                let r = 40.0 + hash i 7.0 * 30.0
                let g = ctx.createRadialGradient (x, y, 2.0, x, y, r)
                g.addColorStop (0.0, "rgba(255,240,180,0.9)")
                g.addColorStop (0.5, "rgba(255,120,40,0.6)")
                g.addColorStop (1.0, "rgba(200,30,10,0)")
                ctx.fillStyle <- !^g
                ctx.fillRect (x - r, y - r, r * 2.0, r * 2.0)

        ctx.globalCompositeOperation <- "source-over"
        ctx.translate (head, o.Y)
        ctx.scale (u.Facing * 2.4, 2.4)
        poly ctx [ (30.0, 0.0); (6.0, -18.0); (-10.0, -26.0); (-24.0, -14.0); (-30.0, 0.0); (-24.0, 14.0); (0.0, 16.0) ]
        outlinedFill ctx (basePalette Ignis) "#ff6b3d" 3.0
        poly ctx [ (-8.0, -24.0); (-22.0, -40.0); (-16.0, -20.0) ]
        outlinedFill ctx (basePalette Ignis) "#ffd166" 2.0
        fill ctx "#fff6d0"
        poly ctx [ (8.0, -8.0); (16.0, -6.0); (10.0, -3.0) ]
        ctx.fill ()
    | Titan ->
        // Meteorschauer
        for i in 0..15 do
            let delay = hash i 11.0 * 0.5
            let p = clamp01 ((k - delay) / 0.5)

            if p > 0.0 then
                let x = 60.0 + hash i 12.0 * (screenWidth - 120.0)
                let y = -80.0 + p * 680.0
                ctx.save ()
                ctx.globalCompositeOperation <- "lighter"
                let trail = ctx.createLinearGradient (x - 60.0, y - 160.0, x, y)
                trail.addColorStop (0.0, "rgba(255,140,40,0)")
                trail.addColorStop (1.0, "rgba(255,200,90,0.85)")
                ctx.strokeStyle <- !^trail
                ctx.lineWidth <- 18.0
                ctx.lineCap <- "round"
                ctx.beginPath ()
                ctx.moveTo (x - 60.0, y - 160.0)
                ctx.lineTo (x, y)
                ctx.stroke ()
                ctx.restore ()
                circle ctx x y 16.0
                outlinedFill ctx (basePalette Titan) "#8a5a2a" 3.0
                fill ctx "#ffb347"
                circle ctx (x + 3.0) (y + 4.0) 6.0
                ctx.fill ()
    | Zephyr ->
        // Tausend Klingen: Kunai-Sturm kreuz und quer
        ctx.globalCompositeOperation <- "lighter"
        stroke ctx "rgba(200,170,255,0.85)"
        ctx.lineCap <- "round"

        for i in 0..54 do
            let a = (if i % 2 = 0 then 0.5 else Math.PI - 0.5) + (hash i 13.0 - 0.5) * 0.3
            let speed = 30.0 + hash i 14.0 * 20.0
            let d = ((t * speed + hash i 15.0 * 2000.0) % 2000.0) - 300.0
            let x0 = hash i 16.0 * screenWidth - cos a * 600.0
            let y0 = -100.0 + hash i 17.0 * 200.0
            let x = x0 + cos a * d
            let y = y0 + sin a * d
            ctx.lineWidth <- 2.0 + hash i 18.0 * 2.0
            ctx.globalAlpha <- grow
            ctx.beginPath ()
            ctx.moveTo (x, y)
            ctx.lineTo (x - cos a * 60.0, y - sin a * 60.0)
            ctx.stroke ()
    | Valkyrie ->
        // Walhalla-Lanze: Lichtsäule aus dem Himmel
        ctx.globalCompositeOperation <- "lighter"
        let x = u.Target.X
        let fall = clamp01 (k / 0.35)
        let tipY = -100.0 + fall * (u.Target.Y + 140.0)
        let w = 90.0
        let col = ctx.createLinearGradient (x - w, 0.0, x + w, 0.0)
        col.addColorStop (0.0, "rgba(255,220,120,0)")
        col.addColorStop (0.5, "rgba(255,250,220,0.95)")
        col.addColorStop (1.0, "rgba(255,220,120,0)")
        ctx.fillStyle <- !^col
        ctx.fillRect (x - w, 0.0, w * 2.0, tipY)
        poly ctx [ (x - 26.0, tipY - 60.0); (x + 26.0, tipY - 60.0); (x, tipY + 30.0) ]
        fill ctx "#ffffff"
        ctx.fill ()

        if fall >= 1.0 then
            let r = 60.0 + (k - 0.35) * 600.0
            stroke ctx "rgba(255,240,170,0.9)"
            ctx.lineWidth <- 10.0
            circle ctx x u.Target.Y r
            ctx.stroke ()
    | Chrono ->
        // Zeitstillstand: riesiges Ziffernblatt, Welt in Türkis getaucht
        fill ctx (sprintf "rgba(20,60,70,%.2f)" (0.45 * grow))
        ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)
        ctx.globalCompositeOperation <- "lighter"
        let r = 320.0 * grow
        ctx.translate (o.X, o.Y)
        stroke ctx "rgba(120,255,230,0.85)"
        ctx.lineWidth <- 8.0
        circle ctx 0.0 0.0 r
        ctx.stroke ()
        ctx.lineWidth <- 6.0

        for i in 0..11 do
            let a = float i * Math.PI / 6.0
            ctx.beginPath ()
            ctx.moveTo (cos a * r * 0.82, sin a * r * 0.82)
            ctx.lineTo (cos a * r * 0.95, sin a * r * 0.95)
            ctx.stroke ()

        stroke ctx "rgba(255,210,120,0.9)"
        ctx.lineWidth <- 10.0
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (0.0, 0.0)
        ctx.lineTo (cos (-t * 0.4) * r * 0.75, sin (-t * 0.4) * r * 0.75)
        ctx.moveTo (0.0, 0.0)
        ctx.lineTo (cos (-t * 0.05) * r * 0.5, sin (-t * 0.05) * r * 0.5)
        ctx.stroke ()
    | Bramble ->
        // Urwald-Zorn: riesige Ranken brechen aus dem Boden
        for i in 0..7 do
            let x = 80.0 + float i * 160.0 + hash i 21.0 * 40.0
            let h = grow * (360.0 + hash i 22.0 * 240.0)
            let sway = sin (t * 0.08 + float i) * 40.0
            ctx.lineCap <- "round"
            stroke ctx "#1a2410"
            ctx.lineWidth <- 34.0
            ctx.beginPath ()
            ctx.moveTo (x, screenHeight + 20.0)
            ctx.quadraticCurveTo (x + sway, screenHeight - h * 0.5, x - sway * 0.5, screenHeight - h)
            ctx.stroke ()
            stroke ctx (if i % 2 = 0 then "#5fae3a" else "#7ed957")
            ctx.lineWidth <- 26.0
            ctx.stroke ()
            fill ctx "#efe3c8"

            for j in 1..4 do
                let y = screenHeight - h * float j / 5.0
                poly ctx [ (x - 12.0, y); (x - 30.0, y - 10.0); (x - 12.0, y - 8.0) ]
                ctx.fill ()
                poly ctx [ (x + 12.0, y - 20.0); (x + 30.0, y - 30.0); (x + 12.0, y - 28.0) ]
                ctx.fill ()
    | Lyra ->
        // Sternenregen: hunderte leuchtende Pfeile fallen schräg vom Himmel
        fill ctx (sprintf "rgba(10,12,40,%.2f)" (0.5 * grow))
        ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)
        ctx.globalCompositeOperation <- "lighter"
        ctx.lineCap <- "round"

        for i in 0..70 do
            let speed = 22.0 + hash i 411.0 * 14.0
            let y = (t * speed + hash i 412.0 * 900.0) % 900.0 - 120.0
            let x = hash i 413.0 * (screenWidth + 300.0) - 150.0 + y * 0.35
            let len = 40.0 + hash i 414.0 * 40.0
            stroke ctx (if i % 4 = 0 then "#ffe27a" else "#d6e2ff")
            ctx.globalAlpha <- grow
            ctx.lineWidth <- 2.0 + hash i 415.0 * 2.0
            ctx.beginPath ()
            ctx.moveTo (x, y)
            ctx.lineTo (x - len * 0.35, y - len)
            ctx.stroke ()
            fill ctx "#ffffff"
            circle ctx x y 2.4
            ctx.fill ()
    | Gorm ->
        // Tektonik: Die Erde bricht auf, Felssäulen schießen in die Höhe
        let quake = sin (t * 1.7) * 6.0 * grow
        ctx.translate (quake, 0.0)

        for i in 0..9 do
            let x = 40.0 + float i * 135.0 + hash i 421.0 * 50.0
            let delay = hash i 422.0 * 0.4
            let rise = clamp01 ((k - delay) / 0.3)
            let h = rise * (260.0 + hash i 423.0 * 220.0)
            let w = 46.0 + hash i 424.0 * 30.0
            let baseY = screenHeight + 10.0

            if h > 0.0 then
                ctx.beginPath ()
                ctx.moveTo (x - w / 2.0, baseY)
                ctx.lineTo (x - w * 0.42, baseY - h + 20.0)
                ctx.lineTo (x - w * 0.1, baseY - h)
                ctx.lineTo (x + w * 0.3, baseY - h + 8.0)
                ctx.lineTo (x + w / 2.0, baseY)
                ctx.closePath ()
                stroke ctx "#1e140b"
                ctx.lineWidth <- 4.0
                ctx.stroke ()
                let rock = ctx.createLinearGradient (x - w / 2.0, 0.0, x + w / 2.0, 0.0)
                rock.addColorStop (0.0, "#e0b07a")
                rock.addColorStop (1.0, "#6a4a2c")
                ctx.fillStyle <- !^rock
                ctx.fill ()
                stroke ctx "rgba(255,180,70,0.8)"
                ctx.lineWidth <- 2.0
                ctx.beginPath ()
                ctx.moveTo (x, baseY - h + 30.0)
                ctx.lineTo (x - 6.0, baseY - h * 0.6)
                ctx.lineTo (x + 4.0, baseY - h * 0.3)
                ctx.stroke ()
    | Ronin ->
        // Tausend Schnitte: Welt in Rot getaucht, Kirschblüten und gewaltige Querhiebe
        fill ctx (sprintf "rgba(40,4,12,%.2f)" (0.55 * grow))
        ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

        for i in 0..40 do
            let x = (hash i 401.0 * screenWidth + t * (1.5 + hash i 402.0)) % screenWidth
            let y = (hash i 403.0 * screenHeight + t * 2.0) % screenHeight
            ctx.save ()
            ctx.translate (x, y)
            ctx.rotate (t * 0.05 + float i)
            fill ctx (if i % 3 = 0 then "#ffffff" else "#ffb3c6")
            ctx.globalAlpha <- 0.8 * grow
            ctx.beginPath ()
            ctx.ellipse (0.0, 0.0, 5.0, 2.6, 0.0, 0.0, Math.PI * 2.0)
            ctx.fill ()
            ctx.restore ()

        ctx.globalAlpha <- 1.0
        ctx.globalCompositeOperation <- "lighter"
        let cuts = int (k * 9.0)

        for i in 0..cuts do
            let y = o.Y + (hash i 404.0 - 0.5) * 220.0
            let tilt = (hash i 405.0 - 0.5) * 0.35
            let w = 4.0 + hash i 406.0 * 8.0
            let g = ctx.createLinearGradient (0.0, -w, 0.0, w)
            g.addColorStop (0.0, "rgba(255,90,120,0)")
            g.addColorStop (0.5, "rgba(255,255,255,0.95)")
            g.addColorStop (1.0, "rgba(255,90,120,0)")
            ctx.save ()
            ctx.translate (screenWidth / 2.0, y)
            ctx.rotate tilt
            ctx.fillStyle <- !^g
            ctx.fillRect (-screenWidth, -w, screenWidth * 2.0, w * 2.0)
            ctx.restore ()
    | Specter ->
        // Schattenreich: Dunkelheit und Schnitte um das Ziel
        fill ctx (sprintf "rgba(12,4,20,%.2f)" (0.7 * grow))
        ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)
        ctx.globalCompositeOperation <- "lighter"
        let target = u.Target
        let cuts = int (k * 14.0)

        for i in 0..cuts do
            let a = hash i 31.0 * Math.PI
            let len = 260.0 + hash i 32.0 * 200.0
            stroke ctx (if i % 3 = 0 then "#ffffff" else "#ff4f7b")
            ctx.lineWidth <- 3.0 + hash i 33.0 * 5.0
            ctx.beginPath ()
            ctx.moveTo (target.X - cos a * len / 2.0, target.Y - sin a * len / 2.0)
            ctx.lineTo (target.X + cos a * len / 2.0, target.Y + sin a * len / 2.0)
            ctx.stroke ()

        fill ctx "#ff4f7b"
        ctx.globalAlpha <- 0.8
        oval ctx (o.X - 14.0) (o.Y - 10.0) 8.0 3.0
        ctx.fill ()
        oval ctx (o.X + 14.0) (o.Y - 10.0) 8.0 3.0
        ctx.fill ()

    ctx.restore ()

    // Name des Angriffs während des Einschlags
    if k > 0.15 && k < 0.9 then
        ctx.save ()
        ctx.globalAlpha <- clamp01 ((0.9 - k) * 4.0)
        ctx.font <- sprintf "italic 30px %s" displayFont
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        outlinedText ctx s.UltraName (screenWidth / 2.0) 60.0 s.Color 6.0
        ctx.restore ()

/// Cut-In-Banner vor dem Ultra-Smash
let private drawUltraCutIn (ctx: Ctx) (frame: int) (u: Ultra) (alt: bool) =
    let t = float frame
    let s = fighterStats u.Fighter
    let slide = clamp01 (float u.Timer / 10.0)
    let out = clamp01 (float (ultraCutIn - u.Timer) / 8.0)
    ctx.save ()
    ctx.globalAlpha <- out
    fill ctx "rgba(5,6,15,0.6)"
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)
    ctx.translate ((1.0 - slide) * -screenWidth, 0.0)
    ctx.save ()
    ctx.transform (1.0, -0.12, 0.0, 1.0, 0.0, 0.0)
    let band = ctx.createLinearGradient (0.0, 250.0, 0.0, 470.0)
    band.addColorStop (0.0, s.Color)
    band.addColorStop (1.0, s.Dark)
    ctx.fillStyle <- !^band
    ctx.fillRect (-200.0, 300.0, screenWidth + 400.0, 220.0)
    fill ctx "#ffffff"
    ctx.fillRect (-200.0, 294.0, screenWidth + 400.0, 6.0)
    ctx.fillRect (-200.0, 520.0, screenWidth + 400.0, 6.0)
    // Speed-Lines im Banner
    fill ctx "rgba(255,255,255,0.35)"

    for i in 0..16 do
        let y = 310.0 + hash i 41.0 * 200.0
        let x = ((t * 40.0 + hash i 42.0 * 1600.0) % 1700.0) - 200.0
        ctx.fillRect (x, y, 120.0 + hash i 43.0 * 200.0, 3.0)

    // Großes Porträt
    ctx.save ()
    ctx.beginPath ()
    ctx.rect (-200.0, 300.0, screenWidth + 400.0, 220.0)
    ctx.clip ()
    ctx.translate (330.0, 430.0)
    ctx.scale (10.0, 10.0)
    drawHead ctx (paletteFor u.Fighter alt) u.Fighter Fierce frame true
    ctx.restore ()
    ctx.restore ()
    // Text
    ctx.font <- sprintf "italic 30px %s" displayFont
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    outlinedText ctx "ULTRA-SMASH!" 560.0 340.0 "#ffffff" 6.0
    ctx.font <- sprintf "italic 78px %s" displayFont
    outlinedText ctx s.UltraName 540.0 410.0 "#ffe27a" 10.0
    ctx.font <- sprintf "italic 26px %s" displayFont
    outlinedText ctx s.Name 570.0 470.0 s.Color 5.0
    ctx.restore ()


// ---------------------------------------------------------------------------
// Projektile
// ---------------------------------------------------------------------------

let private drawProjectile (ctx: Ctx) (frame: int) (pr: Projectile) =
    let dir = if pr.Vel.X >= 0.0 then 1.0 else -1.0
    let life = clamp01 (float pr.Life / float pr.MaxLife)
    let fade = clamp01 (float pr.Life / 6.0)
    let t = float frame
    ctx.save ()
    ctx.translate (pr.Pos.X, pr.Pos.Y)

    match pr.Kind with
    | BladeBeam ->
        // Halbmondförmige Schwertwelle
        ctx.scale (dir, 1.0)
        ctx.globalCompositeOperation <- "lighter"
        ctx.globalAlpha <- fade

        let wave (r: float) (th: float) (color: string) =
            ctx.beginPath ()
            ctx.arc (-r * 0.55, 0.0, r, -1.15, 1.15)
            ctx.arc (-r * 0.55 - th, 0.0, r - th * 0.3, 1.0, -1.0, true)
            ctx.closePath ()
            fill ctx color
            ctx.fill ()

        wave 32.0 12.0 "rgba(60,120,255,0.55)"
        wave 28.0 9.0 "#7fe9ff"
        wave 24.0 4.0 "#ffffff"
        stroke ctx "rgba(190,240,255,0.7)"
        ctx.lineWidth <- 2.0
        ctx.lineCap <- "round"

        for i in 0..2 do
            let y = -14.0 + float i * 14.0
            ctx.beginPath ()
            ctx.moveTo (-22.0, y)
            ctx.lineTo (-62.0 - float i * 8.0, y)
            ctx.stroke ()
    | Kunai ->
        ctx.rotate (atan2 pr.Vel.Y pr.Vel.X)
        ctx.globalAlpha <- fade
        // Leuchtspur
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let trail = ctx.createLinearGradient (-46.0, 0.0, 0.0, 0.0)
        trail.addColorStop (0.0, "rgba(79,240,209,0)")
        trail.addColorStop (1.0, "rgba(79,240,209,0.75)")
        ctx.fillStyle <- !^trail
        poly ctx [ (-46.0, -1.0); (0.0, -3.0); (0.0, 3.0); (-46.0, 1.0) ]
        ctx.fill ()
        ctx.restore ()
        stroke ctx "#170f2e"
        ctx.lineWidth <- 2.0
        circle ctx -10.0 0.0 2.6
        ctx.stroke ()
        ctx.lineWidth <- 4.0
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (-7.0, 0.0)
        ctx.lineTo (-1.0, 0.0)
        ctx.stroke ()
        poly ctx [ (-1.0, -3.6); (13.0, 0.0); (-1.0, 3.6) ]
        stroke ctx "#170f2e"
        ctx.lineWidth <- 2.4
        ctx.lineJoin <- "round"
        ctx.stroke ()
        fill ctx "#e7eefb"
        ctx.fill ()
        fill ctx "#4ff0d1"
        poly ctx [ (0.5, -0.8); (10.0, 0.0); (0.5, 0.8) ]
        ctx.fill ()
    | EnergyOrb ->
        let pulse = 1.0 + 0.12 * sin (t * 0.5 + pr.Pos.X * 0.05)
        let r = 14.0 * pulse
        ctx.globalAlpha <- fade
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let glow = ctx.createRadialGradient (0.0, 0.0, 2.0, 0.0, 0.0, r * 2.4)
        glow.addColorStop (0.0, "rgba(255,255,255,0.95)")
        glow.addColorStop (0.3, "rgba(201,162,255,0.85)")
        glow.addColorStop (0.65, "rgba(120,60,255,0.35)")
        glow.addColorStop (1.0, "rgba(120,60,255,0)")
        ctx.fillStyle <- !^glow
        circle ctx 0.0 0.0 (r * 2.4)
        ctx.fill ()
        ctx.restore ()
        stroke ctx "rgba(255,255,255,0.9)"
        ctx.lineWidth <- 2.0
        ctx.beginPath ()
        ctx.ellipse (0.0, 0.0, r * 1.3, r * 0.45, t * 0.12, 0.0, tau)
        ctx.stroke ()

        for k in 0..1 do
            let a = t * 0.25 + float k * Math.PI
            fill ctx "#ffffff"
            circle ctx (cos a * r * 1.3) (sin a * r * 0.45) 2.2
            ctx.fill ()
    | Shockwave ->
        // Aufsteigende Druckwelle aus Fels und Energie
        ctx.scale (dir, 1.0)
        let h = 30.0 * (0.4 + 0.6 * life)
        ctx.globalAlpha <- clamp01 (life * 1.6)
        let dome = ctx.createLinearGradient (0.0, 14.0 - h, 0.0, 14.0)
        dome.addColorStop (0.0, "rgba(255,240,190,0.95)")
        dome.addColorStop (0.5, "rgba(255,190,80,0.8)")
        dome.addColorStop (1.0, "rgba(200,110,30,0.2)")
        ctx.fillStyle <- !^dome
        ctx.beginPath ()
        ctx.ellipse (0.0, 14.0, 22.0, h, 0.0, Math.PI, tau)
        ctx.fill ()
        stroke ctx "rgba(255,255,255,0.85)"
        ctx.lineWidth <- 2.0
        ctx.beginPath ()
        ctx.ellipse (2.0, 14.0, 16.0, h * 0.8, 0.0, Math.PI * 1.15, Math.PI * 1.75)
        ctx.stroke ()

        for i in 0..2 do
            let rx = -14.0 + float i * 12.0
            let ry = 14.0 - h * (0.5 + hash (i + pr.Life) 2.0 * 0.6)
            fill ctx "#8a6a48"
            poly ctx [ (rx, ry); (rx + 4.0, ry - 3.0); (rx + 7.0, ry + 1.0); (rx + 3.0, ry + 4.0) ]
            ctx.fill ()

    | Fireball ->
        // Hüpfender Feuerball mit Flammenzungen
        let spin = t * 0.4
        ctx.globalAlpha <- fade
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let glow = ctx.createRadialGradient (0.0, 0.0, 2.0, 0.0, 0.0, 26.0)
        glow.addColorStop (0.0, "rgba(255,250,210,0.95)")
        glow.addColorStop (0.35, "rgba(255,170,60,0.85)")
        glow.addColorStop (1.0, "rgba(255,60,20,0)")
        ctx.fillStyle <- !^glow
        circle ctx 0.0 0.0 26.0
        ctx.fill ()
        ctx.restore ()

        for k in 0..4 do
            let a = spin + float k * tau / 5.0
            fill ctx (if k % 2 = 0 then "#ff7a2e" else "#ffd166")
            poly ctx [ (cos a * 7.0, sin a * 7.0); (cos (a + 0.35) * 15.0 - dir * 6.0, sin (a + 0.35) * 15.0); (cos (a + 0.7) * 7.0, sin (a + 0.7) * 7.0) ]
            ctx.fill ()

        fill ctx "#fff6d0"
        circle ctx 0.0 0.0 6.5
        ctx.fill ()
    | Boulder ->
        // Rotierender Felsbrocken
        ctx.rotate (t * 0.15 * dir)
        ctx.globalAlpha <- fade
        let rock = [ for k in 0..7 -> let a = float k * tau / 8.0 in (cos a * (17.0 + hash k 91.0 * 4.0), sin a * (17.0 + hash k 92.0 * 4.0)) ]
        poly ctx rock
        stroke ctx "#1a1208"
        ctx.lineWidth <- 4.0
        ctx.lineJoin <- "round"
        ctx.stroke ()
        let body = ctx.createRadialGradient (-5.0, -6.0, 2.0, 0.0, 0.0, 22.0)
        body.addColorStop (0.0, "#d2b48a")
        body.addColorStop (1.0, "#6a4a2a")
        ctx.fillStyle <- !^body
        ctx.fill ()
        stroke ctx "rgba(40,25,10,0.7)"
        ctx.lineWidth <- 2.0
        ctx.beginPath ()
        ctx.moveTo (-8.0, -4.0)
        ctx.lineTo (2.0, 2.0)
        ctx.lineTo (6.0, 10.0)
        ctx.moveTo (2.0, 2.0)
        ctx.lineTo (10.0, -3.0)
        ctx.stroke ()
    | LightSpear ->
        // Goldener Lichtspeer
        ctx.rotate (atan2 pr.Vel.Y pr.Vel.X)
        ctx.globalAlpha <- fade
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let trail = ctx.createLinearGradient (-70.0, 0.0, 26.0, 0.0)
        trail.addColorStop (0.0, "rgba(255,230,140,0)")
        trail.addColorStop (1.0, "rgba(255,240,170,0.8)")
        ctx.fillStyle <- !^trail
        poly ctx [ (-70.0, -2.0); (20.0, -5.0); (20.0, 5.0); (-70.0, 2.0) ]
        ctx.fill ()
        ctx.restore ()
        stroke ctx "#1a1630"
        ctx.lineWidth <- 4.0
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (-24.0, 0.0)
        ctx.lineTo (12.0, 0.0)
        ctx.stroke ()
        stroke ctx "#fff3b0"
        ctx.lineWidth <- 2.0
        ctx.stroke ()
        poly ctx [ (12.0, -5.0); (24.0, -3.0); (34.0, 0.0); (24.0, 3.0); (12.0, 5.0) ]
        stroke ctx "#1a1630"
        ctx.lineWidth <- 2.4
        ctx.stroke ()
        fill ctx "#ffffff"
        ctx.fill ()
    | StasisOrb ->
        // Stasis-Kugel: türkise Sphäre mit rückwärts laufenden Zeigern
        let r = 15.0 * (1.0 + 0.1 * sin (t * 0.4))
        ctx.globalAlpha <- fade
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let glow = ctx.createRadialGradient (0.0, 0.0, 2.0, 0.0, 0.0, r * 2.2)
        glow.addColorStop (0.0, "rgba(220,255,250,0.9)")
        glow.addColorStop (0.4, "rgba(79,224,200,0.6)")
        glow.addColorStop (1.0, "rgba(40,160,150,0)")
        ctx.fillStyle <- !^glow
        circle ctx 0.0 0.0 (r * 2.2)
        ctx.fill ()
        ctx.restore ()
        circle ctx 0.0 0.0 r
        stroke ctx "#ffffff"
        ctx.lineWidth <- 2.0
        ctx.stroke ()
        stroke ctx "#14122e"
        ctx.lineWidth <- 1.6

        for k in 0..11 do
            let a = float k * Math.PI / 6.0
            ctx.beginPath ()
            ctx.moveTo (cos a * r * 0.75, sin a * r * 0.75)
            ctx.lineTo (cos a * r * 0.9, sin a * r * 0.9)
            ctx.stroke ()

        ctx.lineWidth <- 2.0
        ctx.beginPath ()
        ctx.moveTo (0.0, 0.0)
        ctx.lineTo (cos (-t * 0.3) * r * 0.7, sin (-t * 0.3) * r * 0.7)
        ctx.moveTo (0.0, 0.0)
        ctx.lineTo (cos (-t * 0.05) * r * 0.45, sin (-t * 0.05) * r * 0.45)
        ctx.stroke ()
    | Thorn ->
        // Dornenstachel, folgt der Flugbahn
        ctx.rotate (atan2 pr.Vel.Y pr.Vel.X)
        ctx.globalAlpha <- fade
        poly ctx [ (-9.0, -3.4); (12.0, 0.0); (-9.0, 3.4); (-5.0, 0.0) ]
        stroke ctx "#1a2410"
        ctx.lineWidth <- 2.2
        ctx.lineJoin <- "round"
        ctx.stroke ()
        fill ctx "#a8f06a"
        ctx.fill ()
        fill ctx "#3d6b23"
        poly ctx [ (-9.0, 0.6); (8.0, 0.0); (-9.0, 3.4) ]
        ctx.fill ()
    | Arrow ->
        // Leuchtender Sternpfeil, folgt der Flugbahn
        ctx.rotate (atan2 pr.Vel.Y pr.Vel.X)
        ctx.globalAlpha <- fade
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        let trail = ctx.createLinearGradient (-60.0, 0.0, 0.0, 0.0)
        trail.addColorStop (0.0, "rgba(180,200,255,0)")
        trail.addColorStop (1.0, "rgba(220,230,255,0.75)")
        ctx.fillStyle <- !^trail
        poly ctx [ (-60.0, -1.0); (0.0, -3.0); (0.0, 3.0); (-60.0, 1.0) ]
        ctx.fill ()
        ctx.restore ()
        stroke ctx "#151735"
        ctx.lineWidth <- 3.2
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (-16.0, 0.0)
        ctx.lineTo (8.0, 0.0)
        ctx.stroke ()
        stroke ctx "#e8ecff"
        ctx.lineWidth <- 1.4
        ctx.stroke ()
        poly ctx [ (6.0, -4.0); (16.0, 0.0); (6.0, 4.0) ]
        stroke ctx "#151735"
        ctx.lineWidth <- 2.0
        ctx.stroke ()
        fill ctx "#ffe27a"
        ctx.fill ()
        fill ctx "#bfc8ff"
        poly ctx [ (-16.0, 0.0); (-20.0, -4.5); (-12.0, 0.0); (-20.0, 4.5) ]
        ctx.fill ()
    | WindSlash ->
        // Sichelförmiger Klingenwind
        ctx.scale (dir, 1.0)
        ctx.globalAlpha <- fade
        ctx.save ()
        ctx.globalCompositeOperation <- "lighter"
        ctx.beginPath ()
        ctx.moveTo (-6.0, -24.0)
        ctx.quadraticCurveTo (22.0, 0.0, -6.0, 24.0)
        ctx.quadraticCurveTo (8.0, 0.0, -6.0, -24.0)
        ctx.closePath ()
        let g = ctx.createLinearGradient (-6.0, 0.0, 16.0, 0.0)
        g.addColorStop (0.0, "rgba(255,120,150,0.4)")
        g.addColorStop (1.0, "rgba(255,255,255,0.95)")
        ctx.fillStyle <- !^g
        ctx.fill ()
        stroke ctx "rgba(255,170,190,0.6)"
        ctx.lineWidth <- 2.0

        for k in 1..3 do
            ctx.beginPath ()
            ctx.moveTo (-6.0 - float k * 10.0, -18.0 + float k * 3.0)
            ctx.quadraticCurveTo (14.0 - float k * 10.0, 0.0, -6.0 - float k * 10.0, 18.0 - float k * 3.0)
            ctx.stroke ()

        ctx.restore ()
    | ShadowDagger ->
        // Schattendolch mit rauchiger Spur
        ctx.rotate (atan2 pr.Vel.Y pr.Vel.X)
        ctx.globalAlpha <- fade
        let trail = ctx.createLinearGradient (-56.0, 0.0, 0.0, 0.0)
        trail.addColorStop (0.0, "rgba(42,22,56,0)")
        trail.addColorStop (1.0, "rgba(255,79,123,0.7)")
        ctx.fillStyle <- !^trail
        poly ctx [ (-56.0, -1.0); (0.0, -4.0); (0.0, 4.0); (-56.0, 1.0) ]
        ctx.fill ()
        stroke ctx "#0d0714"
        ctx.lineWidth <- 4.0
        ctx.lineCap <- "round"
        ctx.beginPath ()
        ctx.moveTo (-8.0, 0.0)
        ctx.lineTo (-1.0, 0.0)
        ctx.stroke ()
        poly ctx [ (-1.0, -4.0); (6.0, -3.2); (17.0, 0.0); (6.0, 3.2); (-1.0, 4.0) ]
        stroke ctx "#0d0714"
        ctx.lineWidth <- 2.2
        ctx.stroke ()
        fill ctx "#2a1e36"
        ctx.fill ()
        fill ctx "#ff4f7b"
        poly ctx [ (1.0, -0.8); (14.0, 0.0); (1.0, 0.8) ]
        ctx.fill ()

    ctx.restore ()

// ---------------------------------------------------------------------------
// Debug-Ansicht (Taste T)
// ---------------------------------------------------------------------------

let private moveLabel (m: MoveId) =
    match m with
    | Jab -> "Jab"
    | ForwardTilt -> "F-Tilt"
    | UpTilt -> "Up-Tilt"
    | DownTilt -> "Down-Tilt"
    | ForwardSmash -> "F-Smash"
    | DownSmash -> "Down-Smash"
    | NeutralAir -> "N-Air"
    | ForwardAir -> "F-Air"
    | UpAir -> "Up-Air"
    | DownAir -> "Down-Air"
    | AirSmash -> "Air-Smash"
    | UpSpecial -> "Up-B"
    | SideSpecial -> "Side-B"
    | DownSpecial -> "Down-B"

let private stateLabel (s: PlayerActionState) =
    match s with
    | Idle -> "Idle"
    | Running -> "Running"
    | JumpSquat -> "JumpSquat"
    | Jumping -> "Jumping"
    | Falling -> "Falling"
    | Landing -> "Landing"
    | AttackStartup m -> moveLabel m + " Startup"
    | AttackActive m -> moveLabel m + " Active"
    | AttackRecovery m -> moveLabel m + " Recovery"
    | Hitstun -> "Hitstun"
    | Shielding -> "Shielding"
    | Dodging -> "Dodging"
    | LedgeHang -> "LedgeHang"
    | Helpless -> "Helpless"
    | Respawning -> "Respawning"
    | KO -> "KO"
    | Grabbing -> "Grabbing"
    | Holding -> "Holding"
    | Held -> "Held"

let private drawDebug (ctx: Ctx) (g: Game) =
    stroke ctx "rgba(255,80,80,0.8)"
    ctx.lineWidth <- 2.0
    ctx.strokeRect (blastLeft, blastTop, blastRight - blastLeft, blastBottom - blastTop)
    let l = stageLayout g.Stage (max 0 (g.MatchFrames - 1))
    stroke ctx "rgba(255,255,255,0.5)"
    ctx.strokeRect (l.Left - 50.0, l.Top + 2.0, 48.0, 82.0)
    ctx.strokeRect (l.Right + 2.0, l.Top + 2.0, 48.0, 82.0)
    stroke ctx "rgba(120,255,160,0.8)"

    for pl in allPlatforms l do
        ctx.strokeRect (pl.X1, pl.Y - 2.0, pl.X2 - pl.X1, 4.0)

    match g.Ball with
    | Some b ->
        let r = ballRect b
        stroke ctx "#ff6bff"
        ctx.strokeRect (r.Left, r.Top, r.Width, r.Height)
    | None -> ()

    for p in g.Players do
        if p.State <> KO then
            let hurt = hurtbox p
            fill ctx (if p.Invuln > 0 then "rgba(80,160,255,0.35)" else "rgba(255,230,60,0.3)")
            stroke ctx (if p.Invuln > 0 then "#50a0ff" else "#ffe63c")
            ctx.lineWidth <- 2.0
            ctx.fillRect (hurt.Left, hurt.Top, hurt.Width, hurt.Height)
            ctx.strokeRect (hurt.Left, hurt.Top, hurt.Width, hurt.Height)

            for rect, _ in Physics.activeHitboxes p do
                fill ctx "rgba(255,40,40,0.45)"
                stroke ctx "#ff2828"
                ctx.fillRect (rect.Left, rect.Top, rect.Width, rect.Height)
                ctx.strokeRect (rect.Left, rect.Top, rect.Width, rect.Height)

            ctx.font <- "12px Menlo, Consolas, monospace"
            ctx.textAlign <- "center"
            ctx.textBaseline <- "middle"

            let buffers =
                (if p.BufJump > 0 then " J" else "") + (if p.BufAttack > 0 then " A" else "") + (if p.BufSpecial > 0 then " S" else "")

            outlinedText ctx (sprintf "%s [%d]%s" (stateLabel p.State) p.StateFrame buffers) p.Pos.X (p.Pos.Y + 16.0) "#ffffff" 3.0

    for pr in g.Projectiles do
        let r = projectileRect pr
        fill ctx "rgba(255,120,40,0.45)"
        stroke ctx "#ff7a28"
        ctx.fillRect (r.Left, r.Top, r.Width, r.Height)
        ctx.strokeRect (r.Left, r.Top, r.Width, r.Height)

// ---------------------------------------------------------------------------
// HUD im schrägen Smash-Look
// ---------------------------------------------------------------------------

/// Spieler 2 erhält im Spiegel-Match automatisch Alternativfarben
let private isAlt (g: Game) (p: Player) =
    p.Id = 1 && (g.Players |> List.exists (fun o -> o.Id = 0 && o.Fighter = p.Fighter))

/// Weiß -> Gelb -> Orange -> Dunkelrot
let private percentRgb (percent: float) : float * float * float =
    let stops =
        [ 0.0, (255.0, 255.0, 255.0)
          50.0, (255.0, 226.0, 80.0)
          100.0, (255.0, 128.0, 44.0)
          160.0, (205.0, 26.0, 26.0)
          260.0, (118.0, 0.0, 14.0) ]

    let rec pick remaining =
        match remaining with
        | (p0, (r0, g0, b0)) :: ((p1, (r1, g1, b1)) :: _ as rest) ->
            if percent <= p1 then
                let k = clamp01 ((percent - p0) / (p1 - p0))
                lerp r0 r1 k, lerp g0 g1 k, lerp b0 b1 k
            else
                pick rest
        | [ (_, c) ] -> c
        | [] -> 255.0, 255.0, 255.0

    pick stops

let private rgb (r: float, g: float, b: float) =
    sprintf "rgb(%d,%d,%d)" (int (max 0.0 (min 255.0 r))) (int (max 0.0 (min 255.0 g))) (int (max 0.0 (min 255.0 b)))

/// Verlaufende Ziffern mit metallischer Doppelkontur
let private metalText (ctx: Ctx) (text: string) (x: float) (y: float) (size: float) (percent: float) =
    let r, g, b = percentRgb percent
    ctx.lineJoin <- "round"
    stroke ctx "#0b0d18"
    ctx.lineWidth <- size * 0.24
    ctx.strokeText (text, x, y)
    let metal = ctx.createLinearGradient (0.0, y - size * 0.5, 0.0, y + size * 0.5)
    metal.addColorStop (0.0, "#ffffff")
    metal.addColorStop (0.45, "#c9d2e6")
    metal.addColorStop (0.55, "#6f7a96")
    metal.addColorStop (1.0, "#eef2fb")
    ctx.strokeStyle <- !^metal
    ctx.lineWidth <- size * 0.12
    ctx.strokeText (text, x, y)
    let body = ctx.createLinearGradient (0.0, y - size * 0.45, 0.0, y + size * 0.45)
    body.addColorStop (0.0, rgb (r + 70.0, g + 70.0, b + 70.0))
    body.addColorStop (0.5, rgb (r, g, b))
    body.addColorStop (1.0, rgb (r * 0.62, g * 0.5, b * 0.5))
    ctx.fillStyle <- !^body
    ctx.fillText (text, x, y)


let private drawHud (ctx: Ctx) (g: Game) =
    let t = float g.Frame

    g.Players
    |> List.iteri (fun i p ->
        let cx = screenWidth / 2.0 + (float i - 0.5) * 400.0
        let top = screenHeight - 104.0
        let s = fighterStats p.Fighter
        let alt = isAlt g p
        let pal = paletteFor p.Fighter alt
        let color = if alt then pal.Glow else s.Color
        let dark = if alt then pal.ArmorShade else s.Dark
        let out = p.Stocks <= 0

        ctx.save ()
        ctx.translate (cx, top)
        ctx.globalAlpha <- (if out then 0.45 else 1.0)

        // Schräges Panel (Parallelogramm) mit Farbkeil
        ctx.save ()
        ctx.transform (1.0, 0.0, -0.22, 1.0, 0.0, 0.0)
        roundRect ctx -112.0 6.0 262.0 76.0 8.0
        stroke ctx "#0b0d18"
        ctx.lineWidth <- 6.0
        ctx.stroke ()
        let panel = ctx.createLinearGradient (-112.0, 0.0, 150.0, 0.0)
        panel.addColorStop (0.0, dark)
        panel.addColorStop (0.42, "#161a30")
        panel.addColorStop (1.0, "#0e1122")
        ctx.fillStyle <- !^panel
        ctx.fill ()
        fill ctx color
        ctx.fillRect (-112.0, 70.0, 262.0, 6.0)
        fill ctx "rgba(255,255,255,0.12)"
        ctx.fillRect (-108.0, 9.0, 254.0, 12.0)
        ctx.restore ()

        // Gezeichnetes Porträt im Metallring
        let px = -104.0
        let py = 40.0
        circle ctx px py 38.0
        stroke ctx "#0b0d18"
        ctx.lineWidth <- 9.0
        ctx.stroke ()
        let backdrop = ctx.createRadialGradient (px - 8.0, py - 10.0, 4.0, px, py, 38.0)
        backdrop.addColorStop (0.0, "#ffffff")
        backdrop.addColorStop (0.35, color)
        backdrop.addColorStop (1.0, dark)
        ctx.fillStyle <- !^backdrop
        ctx.fill ()
        ctx.save ()
        circle ctx px py 37.0
        ctx.clip ()
        ctx.translate (px - 2.0 + sin (t * 1.9) * p.HudShake * 4.0, py + 8.0)
        ctx.scale (2.5, 2.5)
        drawHead ctx pal p.Fighter (if p.HudShake > 0.25 || out then Hurt else Calm) g.Frame true
        ctx.restore ()
        circle ctx px py 38.0
        let ring = ctx.createLinearGradient (0.0, py - 38.0, 0.0, py + 38.0)
        ring.addColorStop (0.0, "#ffffff")
        ring.addColorStop (0.5, "#8a94b0")
        ring.addColorStop (1.0, "#e6ebf7")
        ctx.strokeStyle <- !^ring
        ctx.lineWidth <- 4.0
        ctx.stroke ()

        // Spieler-Chip und Kämpfername
        ctx.textBaseline <- "middle"
        roundRect ctx -58.0 12.0 38.0 18.0 4.0
        fill ctx (slotColor p)
        ctx.fill ()
        ctx.font <- sprintf "italic 14px %s" displayFont
        ctx.textAlign <- "center"
        fill ctx "#0b0d18"
        ctx.fillText (playerName p, -39.0, 22.0)
        ctx.textAlign <- "left"
        ctx.font <- sprintf "italic 21px %s" displayFont
        outlinedText ctx s.Name -14.0 21.0 "#ffffff" 4.0

        // Stock-Icons als Mini-Köpfe
        let stockStep = if g.StockCount > 3 then 15.0 else 21.0
        let stockScale = if g.StockCount > 3 then 0.6 else 0.78

        for st in 0 .. g.StockCount - 1 do
            let sx = -42.0 + float st * stockStep
            ctx.save ()
            ctx.translate (sx, 56.0)
            ctx.scale (stockScale, stockScale)

            if st < p.Stocks then
                drawHead ctx pal p.Fighter Calm 10 false
            else
                ctx.globalAlpha <- 0.35
                drawHead ctx (solidPalette "#3a405c") p.Fighter Calm 10 false

            ctx.restore ()

        // Prozentanzeige mit Treffer-Wackeln
        let shake = p.HudShake
        let ox = sin (t * 1.9 + float i * 2.0) * shake * 9.0
        let oy = cos (t * 2.3 + float i) * shake * 7.0
        let scale = 1.0 + shake * 0.16
        ctx.save ()
        ctx.translate (74.0 + ox, 53.0 + oy)
        ctx.scale (scale, scale)
        ctx.rotate -0.04
        ctx.textBaseline <- "middle"
        ctx.textAlign <- "right"
        ctx.font <- sprintf "italic 50px %s" displayFont
        metalText ctx (sprintf "%d" (int p.Percent)) 30.0 0.0 50.0 p.Percent
        ctx.textAlign <- "left"
        ctx.font <- sprintf "italic 26px %s" displayFont
        metalText ctx "%" 34.0 8.0 26.0 p.Percent
        ctx.restore ()

        // Energieleiste (Anti-Spam): füllt sich schneller im Nahkampf
        let ratio = clamp01 (p.Energy / energyMax)
        let flash = p.EnergyFlash > 0 && (g.Frame / 3) % 2 = 0
        ctx.save ()
        ctx.transform (1.0, 0.0, -0.22, 1.0, 0.0, 0.0)
        roundRect ctx -60.0 86.0 206.0 12.0 4.0
        fill ctx "#0b0d18"
        ctx.fill ()
        let barColor =
            if flash then "#ff3b3b"
            elif ratio < 0.25 then "#ff8a3d"
            else "#4fd8ff"
        let bar = ctx.createLinearGradient (0.0, 88.0, 0.0, 96.0)
        bar.addColorStop (0.0, "#ffffff")
        bar.addColorStop (0.45, barColor)
        bar.addColorStop (1.0, "#123a5a")
        ctx.fillStyle <- !^bar
        ctx.fillRect (-58.0, 88.0, 202.0 * ratio, 8.0)
        stroke ctx "rgba(255,255,255,0.25)"
        ctx.lineWidth <- 1.0

        for k in 1..3 do
            let x = -58.0 + 202.0 * float k / 4.0
            ctx.beginPath ()
            ctx.moveTo (x, 88.0)
            ctx.lineTo (x, 96.0)
            ctx.stroke ()

        ctx.restore ()
        ctx.font <- sprintf "italic 11px %s" displayFont
        ctx.textAlign <- "right"
        outlinedText ctx "EN" -66.0 93.0 (if flash then "#ff3b3b" else "#9fe8ff") 3.0

        // ULTRA-Abzeichen, solange der Smash-Ball-Effekt aktiv ist
        if p.HasFinal then
            let hue = (g.Frame * 6) % 360
            ctx.save ()
            ctx.translate (60.0, 2.0)
            ctx.rotate -0.06
            let pulse = 1.0 + 0.08 * sin (t * 0.3)
            ctx.scale (pulse, pulse)
            ctx.textAlign <- "center"
            ctx.font <- sprintf "italic 22px %s" displayFont
            outlinedText ctx "ULTRA BEREIT!" 0.0 0.0 (sprintf "hsl(%d,95%%,65%%)" hue) 5.0
            ctx.restore ()

        // Aktive Item-Effekte als kleine Symbole mit Restzeit
        let effects =
            [ if p.PowerTimer > 0 then "#ff5a6e", "KRAFT", p.PowerTimer
              if p.SpeedTimer > 0 then "#7df9ff", "TEMPO", p.SpeedTimer ]

        effects
        |> List.iteri (fun i (color, label, timer) ->
            let ex = 160.0
            let ey = 20.0 + float i * 26.0
            roundRect ctx ex (ey - 10.0) 74.0 20.0 6.0
            fill ctx "rgba(5,6,15,0.85)"
            ctx.fill ()
            stroke ctx color
            ctx.lineWidth <- 2.0
            ctx.stroke ()
            ctx.font <- sprintf "italic 12px %s" displayFont
            ctx.textAlign <- "left"
            ctx.textBaseline <- "middle"
            fill ctx color
            ctx.fillText (sprintf "%s %d" label (timer / 60 + 1), ex + 7.0, ey + 1.0))

        // Combo-Zähler über dem Panel des Angreifers
        if p.ComboTimer > 0 && p.ComboShown >= 2 then
            let age = comboDisplayFrames - p.ComboTimer
            let pop = if age < 8 then 1.5 - 0.5 * float age / 8.0 else 1.0
            let fade = clamp01 (float p.ComboTimer / 20.0)
            ctx.save ()
            ctx.globalAlpha <- fade
            ctx.translate (20.0, -18.0)
            ctx.rotate -0.06
            ctx.scale (pop, pop)
            ctx.textAlign <- "center"
            ctx.textBaseline <- "middle"
            ctx.font <- sprintf "italic 34px %s" displayFont
            let hot = if p.ComboShown >= 4 then "#ff5a3d" elif p.ComboShown >= 3 then "#ffb13d" else "#ffe27a"
            ctx.lineJoin <- "round"
            stroke ctx "#0b0d18"
            ctx.lineWidth <- 8.0
            ctx.strokeText (sprintf "%d HIT COMBO!" p.ComboShown, 0.0, 0.0)
            let grad = ctx.createLinearGradient (0.0, -16.0, 0.0, 16.0)
            grad.addColorStop (0.0, "#ffffff")
            grad.addColorStop (0.5, hot)
            grad.addColorStop (1.0, "#c2410c")
            ctx.fillStyle <- !^grad
            ctx.fillText (sprintf "%d HIT COMBO!" p.ComboShown, 0.0, 0.0)
            ctx.restore ()

        ctx.restore ())

let private bigText (ctx: Ctx) (text: string) (y: float) (size: float) (scale: float) (alpha: float) (color: string) =
    ctx.save ()
    ctx.globalAlpha <- clamp01 alpha
    ctx.translate (screenWidth / 2.0, y)
    ctx.scale (scale, scale)
    ctx.font <- sprintf "italic %dpx %s" (int size) displayFont
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    fill ctx "rgba(0,0,0,0.35)"
    ctx.fillText (text, size * 0.05, size * 0.07)
    outlinedText ctx text 0.0 0.0 color (size * 0.12)
    ctx.restore ()

let private drawCountdown (ctx: Ctx) (g: Game) =
    if g.Countdown > 0 then
        let number = (g.Countdown - 1) / 70 + 1
        let local = 1.0 - float ((g.Countdown - 1) % 70) / 70.0
        let pop = clamp01 (local * 5.0)
        let scale = 1.7 - 0.7 * (1.0 - (1.0 - pop) * (1.0 - pop))
        let alpha = if local > 0.75 then (1.0 - local) * 4.0 else 1.0
        bigText ctx (string number) 300.0 190.0 scale alpha "#ffe27a"
    elif g.GoTimer > 0 then
        let k = 1.0 - float g.GoTimer / float goFrames
        bigText ctx "GO!" 300.0 200.0 (1.0 + k * 0.5) (1.0 - k * k) "#7dffb0"

    if g.Screen = Playing && g.GameOverTimer > 0 then
        let k = clamp01 (float g.GameOverTimer / 14.0)
        bigText ctx "GAME OVER!" 300.0 130.0 (1.6 - 0.6 * k) k "#ffffff"

// ---------------------------------------------------------------------------
// Menüs: gemeinsame Bausteine
// ---------------------------------------------------------------------------

/// Animierter Menü-Hintergrund: ziehende Wolken, Abdunklung, wandernde Lichtstreifen
let private drawMenuBackdrop (ctx: Ctx) (frame: int) =
    let t = float frame
    drawBackground ctx frame SkyBattlefield
    let shade = ctx.createLinearGradient (0.0, 0.0, 0.0, screenHeight)
    shade.addColorStop (0.0, "rgba(8,8,30,0.82)")
    shade.addColorStop (0.5, "rgba(14,10,40,0.6)")
    shade.addColorStop (1.0, "rgba(8,6,24,0.88)")
    ctx.fillStyle <- !^shade
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"

    for i in 0..6 do
        let x = ((t * (1.2 + float i * 0.15) + float i * 260.0) % (screenWidth + 600.0)) - 300.0
        ctx.globalAlpha <- 0.05 + 0.03 * float (i % 3)
        fill ctx (if i % 2 = 0 then "#7fb4ff" else "#ff9ad5")
        poly ctx [ (x, 0.0); (x + 60.0 + float i * 12.0, 0.0); (x - 240.0 + float i * 12.0, screenHeight); (x - 300.0, screenHeight) ]
        ctx.fill ()

    ctx.restore ()

    // Glitzernde Funken
    for i in 0..40 do
        let x = hash i 21.0 * screenWidth
        let y = ((hash i 22.0 * screenHeight) - t * (0.2 + hash i 23.0 * 0.5)) % screenHeight
        let y = if y < 0.0 then y + screenHeight else y
        ctx.globalAlpha <- 0.25 + 0.35 * sin (t * 0.08 + float i)
        fill ctx "#ffffff"
        circle ctx x y (0.8 + hash i 24.0 * 1.6)
        ctx.fill ()

    ctx.globalAlpha <- 1.0

/// Glänzender Rahmen im Smash-Stil (Glas-Panel mit Metallkante)
let private glossyPanel (ctx: Ctx) (x: float) (y: float) (w: float) (h: float) (accent: string) =
    roundRect ctx x y w h 14.0
    stroke ctx "#05060f"
    ctx.lineWidth <- 7.0
    ctx.stroke ()
    let body = ctx.createLinearGradient (0.0, y, 0.0, y + h)
    body.addColorStop (0.0, "rgba(40,46,84,0.92)")
    body.addColorStop (1.0, "rgba(14,16,34,0.94)")
    ctx.fillStyle <- !^body
    ctx.fill ()
    let rim = ctx.createLinearGradient (x, y, x + w, y + h)
    rim.addColorStop (0.0, "#ffffff")
    rim.addColorStop (0.3, accent)
    rim.addColorStop (0.7, "#6b7699")
    rim.addColorStop (1.0, "#e6ebf7")
    ctx.strokeStyle <- !^rim
    ctx.lineWidth <- 2.5
    ctx.stroke ()
    fill ctx "rgba(255,255,255,0.06)"
    roundRect ctx (x + 4.0) (y + 4.0) (w - 8.0) (h * 0.35) 10.0
    ctx.fill ()

/// Menüzeile mit Goldglanz für die Auswahl
let private menuRow (ctx: Ctx) (frame: int) (label: string) (cx: float) (y: float) (w: float) (selected: bool) =
    let x = cx - w / 2.0
    let t = float frame
    ctx.save ()
    ctx.translate (x, y - 25.0)
    ctx.transform (1.0, 0.0, -0.18, 1.0, 0.0, 0.0)
    roundRect ctx 0.0 0.0 w 50.0 10.0
    stroke ctx "#05060f"
    ctx.lineWidth <- 6.0
    ctx.stroke ()

    if selected then
        let gold = ctx.createLinearGradient (0.0, 0.0, 0.0, 50.0)
        gold.addColorStop (0.0, "#fff6c9")
        gold.addColorStop (0.45, "#ffd166")
        gold.addColorStop (1.0, "#d9861c")
        ctx.fillStyle <- !^gold
        ctx.fill ()
        // Wandernder Glanz
        ctx.save ()
        roundRect ctx 0.0 0.0 w 50.0 10.0
        ctx.clip ()
        let sweep = ((t * 6.0) % (w + 300.0)) - 150.0
        fill ctx "rgba(255,255,255,0.55)"
        poly ctx [ (sweep, 0.0); (sweep + 40.0, 0.0); (sweep + 10.0, 50.0); (sweep - 30.0, 50.0) ]
        ctx.fill ()
        ctx.restore ()
    else
        fill ctx "rgba(20,24,46,0.88)"
        ctx.fill ()
        stroke ctx "rgba(150,165,210,0.6)"
        ctx.lineWidth <- 2.0
        ctx.stroke ()

    ctx.restore ()
    ctx.font <- sprintf "italic 28px %s" displayFont
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"

    if selected then
        fill ctx "#22140a"
        ctx.fillText (label, cx, y + 2.0)
        let bob = sin (t * 0.2) * 5.0
        fill ctx "#ffd166"
        stroke ctx "#05060f"
        ctx.lineWidth <- 3.0
        poly ctx [ (x - 34.0 + bob, y - 13.0); (x - 12.0 + bob, y); (x - 34.0 + bob, y + 13.0) ]
        ctx.stroke ()
        ctx.fill ()
    else
        fill ctx "#e9e6f5"
        ctx.fillText (label, cx, y + 2.0)

let private footerHint (ctx: Ctx) (text: string) =
    ctx.font <- "15px 'Helvetica Neue', Arial, sans-serif"
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    fill ctx "rgba(0,0,0,0.45)"
    roundRect ctx (screenWidth / 2.0 - 430.0) 683.0 860.0 28.0 14.0
    ctx.fill ()
    fill ctx "rgba(233,230,245,0.9)"
    ctx.fillText (text, screenWidth / 2.0, 697.0)

/// Großes Logo mit wanderndem Glanzlicht
let private drawLogo (ctx: Ctx) (frame: int) (x: float) (y: float) (size: float) =
    let t = float frame
    ctx.save ()
    ctx.translate (x, y + sin (t * 0.04) * 4.0)
    ctx.rotate -0.035
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    ctx.font <- sprintf "italic %dpx %s" (int size) displayFont
    ctx.lineJoin <- "round"
    ctx.lineWidth <- size * 0.18
    stroke ctx "#0b0d18"
    ctx.strokeText ("SMASH CLONE", 0.0, 0.0)
    ctx.lineWidth <- size * 0.08
    stroke ctx "#ffffff"
    ctx.strokeText ("SMASH CLONE", 0.0, 0.0)
    let logo = ctx.createLinearGradient (0.0, -size * 0.45, 0.0, size * 0.45)
    logo.addColorStop (0.0, "#ffe27a")
    logo.addColorStop (0.55, "#ff8a3c")
    logo.addColorStop (1.0, "#e0304a")
    ctx.fillStyle <- !^logo
    ctx.fillText ("SMASH CLONE", 0.0, 0.0)
    let band = ((t * 9.0) % 1700.0) - 850.0
    let shine = ctx.createLinearGradient (band - 90.0, 0.0, band + 90.0, 0.0)
    shine.addColorStop (0.0, "rgba(255,255,255,0)")
    shine.addColorStop (0.5, "rgba(255,255,255,0.85)")
    shine.addColorStop (1.0, "rgba(255,255,255,0)")
    ctx.fillStyle <- !^shine
    ctx.fillText ("SMASH CLONE", 0.0, 0.0)
    ctx.restore ()

let private menuPlayer (id: int) (f: Fighter) (state: PlayerActionState) : Player =
    { Physics.previewPlayer id f with
        State = state
        StateFrame = 0 }

// ---------------------------------------------------------------------------
// Titelbildschirm
// ---------------------------------------------------------------------------

let private drawTitle (ctx: Ctx) (g: Game) =
    let t = float g.Frame
    drawMenuBackdrop ctx g.Frame

    // Lichtbühne und Kämpfer-Aufstellung
    let floor = ctx.createRadialGradient (640.0, 650.0, 20.0, 640.0, 650.0, 700.0)
    floor.addColorStop (0.0, "rgba(255,210,140,0.35)")
    floor.addColorStop (1.0, "rgba(255,210,140,0)")
    ctx.fillStyle <- !^floor
    ctx.fillRect (0.0, 520.0, screenWidth, 200.0)

    allFighters
    |> List.iteri (fun i f ->
        let x = 70.0 + float i * 114.0
        let side = if i < 6 then 0 else 1
        let p = menuPlayer side f Idle
        fill ctx "rgba(0,0,0,0.35)"
        oval ctx x 652.0 40.0 7.0
        ctx.fill ()
        drawFighterAt ctx g.Frame p false x 650.0 1.3)

    drawLogo ctx g.Frame (screenWidth / 2.0) 126.0 118.0
    ctx.font <- sprintf "italic 20px %s" displayFont
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    fill ctx "#cfc8ee"
    ctx.fillText ("11 KÄMPFER  ·  5 STAGES  ·  ITEMS  ·  SMASH-BALL  ·  F#  +  FABLE", screenWidth / 2.0, 210.0)

    let difficultyLabel = sprintf "CPU-STÄRKE  ◀ %s ▶" (difficultyName g.Difficulty)
    let items = [ "1P vs. CPU"; "2P LOKAL"; difficultyLabel ]
    items |> List.iteri (fun i label -> menuRow ctx g.Frame label (screenWidth / 2.0) (282.0 + float i * 62.0) 400.0 (i = g.MenuIndex))

    // Pulsierender Hinweis
    ctx.globalAlpha <- 0.6 + 0.4 * sin (t * 0.1)
    ctx.font <- sprintf "italic 18px %s" displayFont
    fill ctx "#ffe27a"
    ctx.fillText ("ENTER DRÜCKEN", screenWidth / 2.0, 470.0)
    ctx.globalAlpha <- 1.0

    footerHint ctx "W / S  wählen   ·   A / D  CPU-Stärke   ·   Enter / F  bestätigen   ·   Controller:  ✕ bestätigen,  ○ zurück"

// ---------------------------------------------------------------------------
// Character Select Screen (4x2-Raster, Match-Optionen in der Mitte)
// ---------------------------------------------------------------------------

let private cardW = 168.0
let private cardH = 112.0
let private cardGap = 10.0
let private cardCols = 6
let private cardX (i: int) = (screenWidth - (6.0 * cardW + 5.0 * cardGap)) / 2.0 + float (i % cardCols) * (cardW + cardGap)
let private cardY (i: int) = if i < cardCols then 84.0 else 208.0

let private slotLabel (g: Game) (slot: int) =
    if slot = 0 then "P1"
    elif g.Mode = VsCpu then "CPU"
    else "P2"

let private slotTint (g: Game) (slot: int) =
    if slot = 0 then "#ff4d5e"
    elif g.Mode = VsCpu then "#a8b0c2"
    else "#4d8bff"

let private drawCard (ctx: Ctx) (frame: int) (index: int) (hovered: bool list) (g: Game) =
    let f = List.item index allFighters
    let s = fighterStats f
    let x = cardX index
    let y = cardY index
    let t = float frame
    let anyHover = hovered |> List.exists id
    let lift = if anyHover then -4.0 + sin (t * 0.12) * 1.5 else 0.0
    ctx.save ()
    ctx.translate (x, y + lift)
    ctx.transform (1.0, 0.0, -0.1, 1.0, 6.0, 0.0)

    // Leuchtrand der Cursor
    hovered
    |> List.iteri (fun slot h ->
        if h then
            ctx.save ()
            ctx.shadowColor <- slotTint g slot
            ctx.shadowBlur <- 20.0
            let o = 4.0 + float slot * 5.0
            roundRect ctx -o -o (cardW + o * 2.0) (cardH + o * 2.0) 12.0
            stroke ctx (slotTint g slot)
            ctx.lineWidth <- 4.0
            ctx.stroke ()
            ctx.restore ())

    roundRect ctx 0.0 0.0 cardW cardH 10.0
    stroke ctx "#05060f"
    ctx.lineWidth <- 6.0
    ctx.stroke ()
    let bg = ctx.createLinearGradient (0.0, 0.0, cardW, cardH)
    bg.addColorStop (0.0, s.Color)
    bg.addColorStop (0.65, s.Dark)
    bg.addColorStop (1.0, "#0b0d18")
    ctx.fillStyle <- !^bg
    ctx.fill ()

    ctx.save ()
    roundRect ctx 0.0 0.0 cardW cardH 10.0
    ctx.clip ()
    fill ctx "rgba(255,255,255,0.12)"

    for k in 0..4 do
        let sx = ((t * 1.5 + float k * 55.0 + float index * 31.0) % (cardW + 140.0)) - 70.0
        poly ctx [ (sx, 0.0); (sx + 16.0, 0.0); (sx - 30.0, cardH); (sx - 46.0, cardH) ]
        ctx.fill ()

    // Porträt links
    ctx.save ()
    ctx.translate (58.0, 70.0)
    ctx.scale (3.4, 3.4)
    drawHead ctx (basePalette f) f (if anyHover then Fierce else Calm) frame true
    ctx.restore ()
    // Namensband rechts unten
    fill ctx "rgba(5,6,15,0.82)"
    ctx.fillRect (0.0, cardH - 34.0, cardW, 34.0)
    fill ctx s.Color
    ctx.fillRect (0.0, cardH - 34.0, cardW, 3.0)
    ctx.restore ()

    ctx.font <- sprintf "italic 22px %s" displayFont
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    outlinedText ctx s.Name 12.0 (cardH - 16.0) "#ffffff" 4.0
    roundRect ctx 0.0 0.0 cardW cardH 10.0
    stroke ctx "rgba(255,255,255,0.55)"
    ctx.lineWidth <- 2.0
    ctx.stroke ()
    ctx.restore ()

let private drawToken (ctx: Ctx) (frame: int) (label: string) (color: string) (x: float) (y: float) (locked: bool) =
    let bob = if locked then 0.0 else sin (float frame * 0.15 + x * 0.01) * 4.0
    let r = if locked then 21.0 else 19.0
    ctx.save ()
    ctx.translate (x, y + bob)
    fill ctx "rgba(0,0,0,0.35)"
    oval ctx 3.0 (r + 5.0) (r * 0.8) 5.0
    ctx.fill ()
    circle ctx 0.0 0.0 r
    stroke ctx "#05060f"
    ctx.lineWidth <- 6.0
    ctx.stroke ()
    let body = ctx.createRadialGradient (-6.0, -8.0, 2.0, 0.0, 0.0, r)
    body.addColorStop (0.0, "#ffffff")
    body.addColorStop (0.35, color)
    body.addColorStop (1.0, "#1a1d30")
    ctx.fillStyle <- !^body
    ctx.fill ()
    stroke ctx "#ffffff"
    ctx.lineWidth <- 2.0
    ctx.stroke ()
    ctx.font <- sprintf "italic 15px %s" displayFont
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    outlinedText ctx label 0.0 1.0 "#ffffff" 4.0

    if locked then
        circle ctx 15.0 -15.0 8.0
        fill ctx "#7dffb0"
        ctx.fill ()
        stroke ctx "#05060f"
        ctx.lineWidth <- 2.5
        ctx.stroke ()
        ctx.beginPath ()
        ctx.moveTo (11.0, -15.0)
        ctx.lineTo (14.0, -12.0)
        ctx.lineTo (19.0, -19.0)
        ctx.stroke ()

    ctx.restore ()

let private statBar (ctx: Ctx) (label: string) (value: int) (color: string) (x: float) (y: float) =
    ctx.font <- sprintf "italic 14px %s" displayFont
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    fill ctx "#cfd6ee"
    ctx.fillText (label, x, y)

    for i in 0..9 do
        let sx = x + 70.0 + float i * 10.0
        poly ctx [ (sx + 3.0, y - 6.0); (sx + 10.5, y - 6.0); (sx + 7.5, y + 6.0); (sx, y + 6.0) ]
        fill ctx (if i < value then color else "rgba(255,255,255,0.12)")
        ctx.fill ()

let private fighterTrait (f: Fighter) =
    match f with
    | Aether -> "Allrounder mit Klingenstrahl", "#8ad7ff"
    | Ignis -> "Nahkampf-Druck, hüpfender Feuerball", "#ffb36b"
    | Titan -> "Super-Armor bei Smash-Angriffen", "#ffd166"
    | Zephyr -> "Dreifachsprung, Kunai-Salven", "#7dffb0"
    | Valkyrie -> "Aegis-Sprint reflektiert Projektile", "#ffe08a"
    | Chrono -> "Schweben (↑ halten), Stasis verlangsamt", "#4fe0c8"
    | Bramble -> "Höchster Sprung, Dornen-Salve", "#a8f06a"
    | Specter -> "Leicht und schnell, teleportiert sich", "#ff4f7b"
    | Ronin -> "Konter-Haltung bestraft Nahkampf", "#ff8aa0"
    | Lyra -> "Pfeile über die ganze Stage", "#c9d2ff"
    | Gorm -> "Felsgriff packt durch Schilde", "#ffb347"

let private drawPreview (ctx: Ctx) (g: Game) (slot: int) (x: float) (y: float) =
    let f = Physics.selectedFighter g slot
    let s = fighterStats f
    let locked = List.item slot g.Select.Locked
    let w = 400.0
    let h = 300.0
    let tint = slotTint g slot
    glossyPanel ctx x y w h tint

    roundRect ctx (x + 10.0) (y + 10.0) (w - 20.0) 28.0 8.0
    fill ctx tint
    ctx.fill ()
    ctx.font <- sprintf "italic 17px %s" displayFont
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"

    let header =
        if slot = 0 then "SPIELER 1"
        elif g.Mode = VsCpu then sprintf "CPU  ·  %s" (difficultyName g.Difficulty)
        else "SPIELER 2"

    outlinedText ctx header (x + 22.0) (y + 24.0) "#ffffff" 4.0

    // Kämpfer auf Lichtsockel
    let glow = ctx.createRadialGradient (x + 92.0, y + 272.0, 4.0, x + 92.0, y + 272.0, 110.0)
    glow.addColorStop (0.0, "rgba(255,255,255,0.35)")
    glow.addColorStop (1.0, "rgba(255,255,255,0)")
    ctx.fillStyle <- !^glow
    ctx.fillRect (x, y + 150.0, 190.0, 145.0)
    let alt = slot = 1 && Physics.selectedFighter g 0 = f
    let p = { menuPlayer slot f Idle with OnGround = true }
    drawFighterAt ctx g.Frame p alt (x + 92.0) (y + 276.0) 1.95

    let tx = x + 186.0
    ctx.font <- sprintf "italic 28px %s" displayFont
    outlinedText ctx s.Name tx (y + 62.0) s.Color 5.0
    ctx.font <- "11px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "rgba(233,230,245,0.85)"
    ctx.fillText (s.Role, tx, y + 84.0)
    statBar ctx "TEMPO" s.StatSpeed "#7dffb0" tx (y + 104.0)
    statBar ctx "KRAFT" s.StatPower "#ff8a5c" tx (y + 122.0)
    statBar ctx "GEWICHT" s.StatWeight "#8ab4ff" tx (y + 140.0)
    statBar ctx "REICHW." s.StatReach "#ffd166" tx (y + 158.0)

    let specials =
        [ "G + ↑", s.UpSpecialName
          "G + ←→", s.SideSpecialName
          "G", s.NeutralName
          "G + ↓ Luft", s.DownSpecialName ]

    specials
    |> List.iteri (fun i (key, name) ->
        let ly = y + 180.0 + float i * 18.0
        ctx.font <- "bold 11px 'Helvetica Neue', Arial, sans-serif"
        fill ctx s.Color
        ctx.fillText (key, tx, ly)
        ctx.font <- "12px 'Helvetica Neue', Arial, sans-serif"
        fill ctx "#e9e6f5"
        ctx.fillText (name, tx + 66.0, ly))

    ctx.font <- "bold 11px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "#ffe27a"
    ctx.fillText ("ULTRA", tx, y + 256.0)
    ctx.font <- "italic 12px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "#ffe27a"
    ctx.fillText (s.UltraName, tx + 66.0, y + 256.0)
    let traitText, traitColor = fighterTrait f
    ctx.font <- "italic 11px 'Helvetica Neue', Arial, sans-serif"
    fill ctx traitColor
    ctx.fillText (traitText, tx, y + 278.0)

    if locked then
        ctx.save ()
        ctx.translate (x + w - 70.0, y + 24.0)
        ctx.rotate -0.08
        let pulse = 1.0 + 0.04 * sin (float g.Frame * 0.2)
        ctx.scale (pulse, pulse)
        roundRect ctx -52.0 -16.0 104.0 32.0 7.0
        stroke ctx "#05060f"
        ctx.lineWidth <- 5.0
        ctx.stroke ()
        fill ctx "#7dffb0"
        ctx.fill ()
        ctx.font <- sprintf "italic 20px %s" displayFont
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        fill ctx "#0b2a18"
        ctx.fillText ("BEREIT!", 0.0, 1.5)
        ctx.restore ()

let private difficultyText (d: Difficulty) =
    match d with
    | Easy -> "Reagiert langsam, lässt viele Lücken"
    | Medium -> "Solider Gegner mit menschlichen Fehlern"
    | Hard -> "Schnelle Reaktionen, nutzt Edgeguards"
    | Pro -> "Volle Combos, gnadenlose Edgeguards"

/// Eine Zeile im Optionsfeld: Label links, Wert mit Pfeilen rechts
let private optionRow (ctx: Ctx) (frame: int) (label: string) (value: string) (x: float) (y: float) (w: float) (active: bool) (enabled: bool) =
    roundRect ctx x (y - 17.0) w 34.0 8.0

    if active then
        let gold = ctx.createLinearGradient (0.0, y - 17.0, 0.0, y + 17.0)
        gold.addColorStop (0.0, "#fff6c9")
        gold.addColorStop (1.0, "#e0961f")
        ctx.fillStyle <- !^gold
    else
        fill ctx "rgba(255,255,255,0.07)"

    ctx.fill ()
    stroke ctx (if active then "#05060f" else "rgba(150,165,210,0.45)")
    ctx.lineWidth <- 2.0
    ctx.stroke ()
    ctx.globalAlpha <- (if enabled then 1.0 else 0.55)
    ctx.textBaseline <- "middle"
    ctx.textAlign <- "left"
    ctx.font <- sprintf "italic 15px %s" displayFont
    fill ctx (if active then "#22140a" else "#cfd6ee")
    ctx.fillText (label, x + 12.0, y + 1.0)
    ctx.textAlign <- "center"
    ctx.font <- sprintf "italic 17px %s" displayFont
    let vx = x + w - 78.0
    let nudge = if active then sin (float frame * 0.25) * 1.5 else 0.0
    fill ctx (if active then "#22140a" else "#ffffff")
    ctx.fillText (sprintf "◀  %s  ▶" value, vx + nudge, y + 1.0)
    ctx.globalAlpha <- 1.0

let private drawCharSelect (ctx: Ctx) (g: Game) =
    let t = float g.Frame
    drawMenuBackdrop ctx g.Frame

    // Kopfbanner
    ctx.save ()
    ctx.translate (0.0, 14.0)
    ctx.transform (1.0, 0.0, -0.2, 1.0, 0.0, 0.0)
    fill ctx "rgba(5,6,15,0.85)"
    ctx.fillRect (60.0, 0.0, 760.0, 50.0)
    fill ctx "#ffd166"
    ctx.fillRect (60.0, 44.0, 760.0, 6.0)
    ctx.restore ()
    ctx.font <- sprintf "italic 38px %s" displayFont
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    outlinedText ctx "KÄMPFERWAHL" 70.0 39.0 "#ffffff" 6.0
    ctx.font <- sprintf "italic 19px %s" displayFont
    let modeText = if g.Mode = VsCpu then sprintf "1P vs. CPU  ·  %s" (difficultyName g.Difficulty) else "2P LOKAL"
    outlinedText ctx modeText 370.0 41.0 "#ffd166" 4.0

    // Karten im 4x2-Raster
    for i in 0 .. List.length allFighters - 1 do
        let hovered = [ 0; 1 ] |> List.map (fun slot -> List.item slot g.Select.Cursors = i)
        drawCard ctx g.Frame i hovered g

    // Cursor-Token
    for slot in [ 1; 0 ] do
        let i = List.item slot g.Select.Cursors
        let locked = List.item slot g.Select.Locked
        let x = cardX i + (if slot = 0 then 26.0 else cardW - 12.0)
        let y = cardY i + 18.0
        drawToken ctx g.Frame (slotLabel g slot) (slotTint g slot) x y locked

    drawPreview ctx g 0 30.0 336.0
    drawPreview ctx g 1 (screenWidth - 430.0) 336.0

    // Match-Optionen
    let allLocked = g.Select.Locked |> List.forall id
    let cx = screenWidth / 2.0
    let px = 446.0
    let pw = 388.0
    glossyPanel ctx px 336.0 pw 300.0 "#ffd166"
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    ctx.font <- sprintf "italic 19px %s" displayFont
    outlinedText ctx "MATCH-REGELN" cx 358.0 "#ffd166" 4.0

    let rows =
        [ (if g.Mode = VsCpu then "CPU-STÄRKE" else "MODUS"), (if g.Mode = VsCpu then difficultyName g.Difficulty else "2 SPIELER")
          "STOCKS", string g.StockCount
          "SMASH-BALL", (if g.SmashBallOn then "AN" else "AUS")
          "ITEMS", (if g.ItemsOn then "AN" else "AUS")
          "STAGE-EREIGNISSE", (if g.HazardsOn then "AN" else "AUS") ]

    rows
    |> List.iteri (fun i (label, value) ->
        let enabled = not (i = 0 && g.Mode <> VsCpu)
        optionRow ctx g.Frame label value (px + 18.0) (386.0 + float i * 33.0) (pw - 36.0) (allLocked && i = g.OptionIndex) enabled)

    ctx.textAlign <- "center"
    ctx.font <- "12px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "rgba(233,230,245,0.75)"

    let info =
        if not allLocked then "Regeln nach der Kämpferwahl einstellbar"
        elif g.OptionIndex = 0 && g.Mode = VsCpu then difficultyText g.Difficulty
        elif g.OptionIndex = 1 then "Anzahl Leben pro Kämpfer (1 bis 5)"
        elif g.OptionIndex = 2 then "Erscheint alle 25 bis 35 Sekunden"
        elif g.OptionIndex = 3 then "Heilherz, Kraft-Kristall, Tempo-Feder und Bomben"
        else "Windböen, Sternschnuppen, Lava, Eiszapfen, Laser"

    ctx.fillText (info, cx, 547.0)

    // Kompakte Steuerung
    let controls =
        [ "Bewegen · Springen", "WASD", "Pfeile"
          "Angriff  ·  Smash / Special", "F · G", "K · L"
          "Schild · Ausweichen", "H", "Ö" ]

    stroke ctx "rgba(150,165,210,0.35)"
    ctx.lineWidth <- 1.0
    ctx.beginPath ()
    ctx.moveTo (px + 24.0, 560.0)
    ctx.lineTo (px + pw - 24.0, 560.0)
    ctx.stroke ()

    controls
    |> List.iteri (fun i (what, p1, p2) ->
        let ly = 574.0 + float i * 17.0
        ctx.textAlign <- "left"
        ctx.font <- "12px 'Helvetica Neue', Arial, sans-serif"
        fill ctx "#e9e6f5"
        ctx.fillText (what, px + 24.0, ly)
        ctx.textAlign <- "center"
        ctx.font <- "bold 12px 'Helvetica Neue', Arial, sans-serif"
        fill ctx "#ff8a95"
        ctx.fillText (p1, px + 262.0, ly)
        fill ctx "#8ab4ff"
        ctx.fillText (p2, px + 336.0, ly))

    ctx.textAlign <- "left"
    ctx.font <- "italic 11px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "#ffd166"
    ctx.fillText ("Griff: Schild + Angriff  ·  ◎ = Fernkampf (kostet Energie)", px + 24.0, 626.0)

    if allLocked then
        let pulse = 1.0 + 0.03 * sin (t * 0.2)
        ctx.save ()
        ctx.translate (cx, 668.0)
        ctx.scale (pulse, pulse)
        ctx.transform (1.0, 0.0, -0.2, 1.0, 0.0, 0.0)
        roundRect ctx -330.0 -22.0 660.0 44.0 10.0
        stroke ctx "#05060f"
        ctx.lineWidth <- 6.0
        ctx.stroke ()
        let gold = ctx.createLinearGradient (0.0, -22.0, 0.0, 22.0)
        gold.addColorStop (0.0, "#fff6c9")
        gold.addColorStop (0.5, "#ffd166")
        gold.addColorStop (1.0, "#e0861c")
        ctx.fillStyle <- !^gold
        ctx.fill ()
        ctx.restore ()
        ctx.font <- sprintf "italic 22px %s" displayFont
        ctx.textAlign <- "center"
        fill ctx "#22140a"
        ctx.fillText ("ENTER  ▶  STAGE-WAHL     ·     W/S Regel  ·  A/D ändern", cx, 670.0)
    else
        let hint =
            if g.Mode = VsCpu then
                if List.item 0 g.Select.Locked then "WASD  CPU-Kämpfer wählen   ·   F  bestätigen   ·   H  zurück"
                else "WASD  Kämpfer wählen   ·   F  bestätigen   ·   H / Esc  zurück"
            else
                "P1:  WASD  +  F      ·      P2:  Pfeile  +  K      ·      H / Ö  zurück"

        footerHint ctx hint

// ---------------------------------------------------------------------------
// Stage-Auswahl
// ---------------------------------------------------------------------------

let private stageCardW = 222.0
let private stageCardH = 160.0
let private stageCardX (i: int) = (screenWidth - (5.0 * stageCardW + 4.0 * 16.0)) / 2.0 + float i * (stageCardW + 16.0)
let private stageCardY = 130.0

let private drawStageCard (ctx: Ctx) (frame: int) (i: int) (stage: StageId) (selected: bool) =
    let x = stageCardX i
    let y = stageCardY + (if selected then -8.0 + sin (float frame * 0.1) * 2.0 else 0.0)
    let scale = stageCardW / screenWidth
    let thumbH = screenHeight * scale
    ctx.save ()
    ctx.translate (x, y)

    if selected then
        ctx.save ()
        ctx.shadowColor <- "#ffd166"
        ctx.shadowBlur <- 30.0
        roundRect ctx -6.0 -6.0 (stageCardW + 12.0) (stageCardH + 12.0) 14.0
        stroke ctx "#ffd166"
        ctx.lineWidth <- 5.0
        ctx.stroke ()
        ctx.restore ()

    roundRect ctx 0.0 0.0 stageCardW stageCardH 12.0
    stroke ctx "#05060f"
    ctx.lineWidth <- 7.0
    ctx.stroke ()
    fill ctx "#0b0d18"
    ctx.fill ()

    // Live-Vorschau der Stage im Miniaturformat
    ctx.save ()
    roundRect ctx 0.0 0.0 stageCardW stageCardH 12.0
    ctx.clip ()
    ctx.save ()
    ctx.scale (scale, scale)
    drawBackground ctx frame stage
    drawStage ctx frame (stageLayout stage frame)
    ctx.restore ()

    if not selected then
        fill ctx "rgba(5,6,15,0.45)"
        ctx.fillRect (0.0, 0.0, stageCardW, stageCardH)

    fill ctx "rgba(5,6,15,0.88)"
    ctx.fillRect (0.0, thumbH, stageCardW, stageCardH - thumbH)
    fill ctx (if selected then "#ffd166" else "#5a6488")
    ctx.fillRect (0.0, thumbH, stageCardW, 4.0)
    ctx.restore ()

    ctx.font <- sprintf "italic 17px %s" displayFont
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    outlinedText ctx (stageName stage) 10.0 (thumbH + 17.0) (if selected then "#ffffff" else "#b8c0dc") 5.0
    roundRect ctx 0.0 0.0 stageCardW stageCardH 12.0
    stroke ctx "rgba(255,255,255,0.5)"
    ctx.lineWidth <- 2.0
    ctx.stroke ()
    ctx.restore ()

let private drawStageSelect (ctx: Ctx) (g: Game) =
    let t = float g.Frame
    drawMenuBackdrop ctx g.Frame

    ctx.save ()
    ctx.translate (0.0, 14.0)
    ctx.transform (1.0, 0.0, -0.2, 1.0, 0.0, 0.0)
    fill ctx "rgba(5,6,15,0.85)"
    ctx.fillRect (60.0, 0.0, 620.0, 50.0)
    fill ctx "#8ab4ff"
    ctx.fillRect (60.0, 44.0, 620.0, 6.0)
    ctx.restore ()
    ctx.font <- sprintf "italic 38px %s" displayFont
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    outlinedText ctx "STAGE-WAHL" 70.0 39.0 "#ffffff" 6.0

    let selectedIndex = allStages |> List.findIndex (fun s -> s = g.Stage)
    allStages |> List.iteri (fun i s -> drawStageCard ctx g.Frame i s (i = selectedIndex))

    // Pfeile links und rechts
    let bob = sin (t * 0.15) * 5.0
    ctx.font <- sprintf "italic 40px %s" displayFont
    ctx.textAlign <- "center"
    outlinedText ctx "◀" (24.0 - bob) (stageCardY + 70.0) "#ffd166" 5.0
    outlinedText ctx "▶" (screenWidth - 24.0 + bob) (stageCardY + 70.0) "#ffd166" 5.0

    // Beschreibung der gewählten Stage
    glossyPanel ctx 70.0 380.0 640.0 160.0 "#8ab4ff"
    ctx.textAlign <- "left"
    ctx.font <- sprintf "italic 30px %s" displayFont
    outlinedText ctx (stageName g.Stage) 96.0 418.0 "#ffffff" 5.0
    ctx.font <- "15px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "#e9e6f5"

    let words = (stageDescription g.Stage).Split ' ' |> Array.toList

    let lines =
        words
        |> List.fold
            (fun (acc: string list) w ->
                match acc with
                | last :: rest when (last + " " + w).Length <= 62 -> (last + " " + w) :: rest
                | _ -> w :: acc)
            []
        |> List.rev

    lines |> List.iteri (fun i line -> ctx.fillText (line, 96.0, 458.0 + float i * 24.0))

    // Match-Zusammenfassung
    glossyPanel ctx 730.0 380.0 480.0 160.0 "#ffd166"
    ctx.font <- sprintf "italic 19px %s" displayFont
    ctx.textAlign <- "right"
    outlinedText ctx "MATCH" 1188.0 404.0 "#ffd166" 4.0

    [ 0; 1 ]
    |> List.iter (fun slot ->
        let f = Physics.selectedFighter g slot
        let s = fighterStats f
        let x = 790.0 + float slot * 200.0
        ctx.save ()
        ctx.translate (x, 492.0)
        ctx.scale (2.2, 2.2)
        drawHead ctx (paletteFor f (slot = 1 && Physics.selectedFighter g 0 = f)) f Fierce g.Frame true
        ctx.restore ()
        ctx.textAlign <- "left"
        ctx.font <- sprintf "italic 18px %s" displayFont
        outlinedText ctx s.Name (x + 40.0) 450.0 s.Color 4.0
        ctx.font <- sprintf "italic 13px %s" displayFont
        outlinedText ctx (slotLabel g slot) (x + 40.0) 472.0 (slotTint g slot) 3.0)

    ctx.textAlign <- "center"
    ctx.font <- sprintf "italic 26px %s" displayFont
    outlinedText ctx "VS" 935.0 486.0 "#ffffff" 5.0
    ctx.font <- "14px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "#e9e6f5"

    let rules =
        sprintf "%d Stock%s  ·  Smash-Ball %s  ·  Items %s  ·  Ereignisse %s" g.StockCount (if g.StockCount = 1 then "" else "s") (if g.SmashBallOn then "AN" else "AUS")
            (if g.ItemsOn then "AN" else "AUS") (if g.HazardsOn then "AN" else "AUS")

    ctx.font <- "12px 'Helvetica Neue', Arial, sans-serif"
    ctx.fillText (rules, 970.0, 526.0)

    // Start-Banner
    let pulse = 1.0 + 0.03 * sin (t * 0.2)
    ctx.save ()
    ctx.translate (screenWidth / 2.0, 600.0)
    ctx.scale (pulse, pulse)
    ctx.transform (1.0, 0.0, -0.2, 1.0, 0.0, 0.0)
    roundRect ctx -260.0 -26.0 520.0 52.0 10.0
    stroke ctx "#05060f"
    ctx.lineWidth <- 6.0
    ctx.stroke ()
    let gold = ctx.createLinearGradient (0.0, -26.0, 0.0, 26.0)
    gold.addColorStop (0.0, "#fff6c9")
    gold.addColorStop (0.5, "#ffd166")
    gold.addColorStop (1.0, "#e0861c")
    ctx.fillStyle <- !^gold
    ctx.fill ()
    ctx.restore ()
    ctx.font <- sprintf "italic 28px %s" displayFont
    ctx.textAlign <- "center"
    fill ctx "#22140a"
    ctx.fillText ("KAMPF STARTEN!  ·  ENTER", screenWidth / 2.0, 602.0)

    footerHint ctx "A / D  oder  ← / →  Stage wählen   ·   Enter / F  starten   ·   H / Esc  zurück zur Kämpferwahl"

// ---------------------------------------------------------------------------
// Siegerbildschirm mit Podium und Match-Statistik
// ---------------------------------------------------------------------------

let private drawConfetti (ctx: Ctx) (frame: int) =
    let t = float frame
    let colors = [| "#ffd166"; "#ff6b8b"; "#58c7ff"; "#7dffb0"; "#c9a2ff"; "#ffffff" |]

    for i in 0..90 do
        let speed = 1.2 + hash i 31.0 * 2.2
        let x = hash i 32.0 * screenWidth + sin (t * 0.03 + float i) * 30.0
        let y = ((hash i 33.0 * 900.0 + t * speed) % 820.0) - 60.0
        ctx.save ()
        ctx.translate (x, y)
        ctx.rotate (t * 0.08 * (hash i 34.0 - 0.5) * 4.0)
        ctx.scale (1.0, abs (sin (t * 0.1 + float i)))
        fill ctx colors[i % colors.Length]
        ctx.fillRect (-4.0, -2.5, 8.0, 5.0)
        ctx.restore ()

let private podium (ctx: Ctx) (x: float) (top: float) (w: float) (rank: int) =
    let h = screenHeight - top
    poly ctx [ (x - w / 2.0, top); (x + w / 2.0, top); (x + w / 2.0 + 14.0, screenHeight); (x - w / 2.0 - 14.0, screenHeight) ]
    stroke ctx "#05060f"
    ctx.lineWidth <- 6.0
    ctx.stroke ()
    let metal = ctx.createLinearGradient (x - w / 2.0, 0.0, x + w / 2.0, 0.0)

    if rank = 1 then
        metal.addColorStop (0.0, "#a8701a")
        metal.addColorStop (0.4, "#ffe08a")
        metal.addColorStop (1.0, "#b8801f")
    else
        metal.addColorStop (0.0, "#6c7590")
        metal.addColorStop (0.4, "#e6ebf7")
        metal.addColorStop (1.0, "#7a849e")

    ctx.fillStyle <- !^metal
    ctx.fill ()
    fill ctx "rgba(255,255,255,0.4)"
    ctx.fillRect (x - w / 2.0, top, w, 6.0)
    ctx.font <- sprintf "italic %dpx %s" (if rank = 1 then 64 else 48) displayFont
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    outlinedText ctx (string rank) x (top + min 70.0 (h / 2.0)) "#ffffff" 7.0

let private drawGameOver (ctx: Ctx) (g: Game) =
    let t = float g.Frame
    drawMenuBackdrop ctx g.Frame
    let winner = g.Winner |> Option.bind (fun id -> g.Players |> List.tryFind (fun p -> p.Id = id))
    let loser = g.Players |> List.tryFind (fun p -> Some p.Id <> g.Winner)

    // Rotierende Lichtstrahlen hinter dem Sieger
    let wx = 300.0
    ctx.save ()
    ctx.globalCompositeOperation <- "lighter"

    for i in 0..13 do
        let a = t * 0.004 + float i / 14.0 * tau
        ctx.globalAlpha <- 0.08
        fill ctx (match winner with Some w -> (fighterStats w.Fighter).Color | None -> "#ffffff")
        poly ctx [ (wx, 420.0); (wx + cos (a - 0.08) * 900.0, 420.0 + sin (a - 0.08) * 900.0); (wx + cos (a + 0.08) * 900.0, 420.0 + sin (a + 0.08) * 900.0) ]
        ctx.fill ()

    ctx.restore ()
    drawConfetti ctx g.Frame

    // Podium mit Kämpfern
    podium ctx 300.0 520.0 190.0 1
    podium ctx 530.0 590.0 150.0 2

    match loser with
    | Some l -> drawFighterAt ctx g.Frame { menuPlayer 1 l.Fighter Landing with Timer = 99 } (isAlt g l) 530.0 590.0 1.9
    | None -> ()

    match winner with
    | Some w ->
        let glow = ctx.createRadialGradient (300.0, 440.0, 10.0, 300.0, 440.0, 160.0)
        glow.addColorStop (0.0, "rgba(255,240,180,0.5)")
        glow.addColorStop (1.0, "rgba(255,240,180,0)")
        ctx.fillStyle <- !^glow
        ctx.fillRect (140.0, 280.0, 320.0, 260.0)
        let pose = if g.Frame % 240 < 120 then Idle else AttackActive UpTilt
        let p = { menuPlayer 0 w.Fighter pose with StateFrame = 2 }
        drawFighterAt ctx g.Frame p (isAlt g w) 300.0 520.0 2.5
    | None -> ()

    // Siegerbanner
    let text, color =
        match winner with
        | Some p -> sprintf "%s GEWINNT!" (fighterStats p.Fighter).Name, (fighterStats p.Fighter).Color
        | None -> "UNENTSCHIEDEN!", "#ffffff"

    ctx.save ()
    ctx.translate (screenWidth / 2.0, 74.0)
    ctx.scale (1.0 + sin (t * 0.06) * 0.02, 1.0 + sin (t * 0.06) * 0.02)
    ctx.rotate -0.03
    ctx.font <- sprintf "italic 92px %s" displayFont
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    ctx.lineJoin <- "round"
    stroke ctx "#05060f"
    ctx.lineWidth <- 16.0
    ctx.strokeText (text, 0.0, 0.0)
    stroke ctx "#ffffff"
    ctx.lineWidth <- 6.0
    ctx.strokeText (text, 0.0, 0.0)
    fill ctx color
    ctx.fillText (text, 0.0, 0.0)
    ctx.restore ()

    // Statistik
    let px = 690.0
    let py = 150.0
    let pw = 540.0
    glossyPanel ctx px py pw 300.0 "#ffd166"
    ctx.textBaseline <- "middle"
    ctx.textAlign <- "left"
    ctx.font <- sprintf "italic 22px %s" displayFont
    outlinedText ctx "MATCH-STATISTIK" (px + 24.0) (py + 28.0) "#ffd166" 4.0
    let seconds = g.MatchFrames / 60
    ctx.textAlign <- "right"
    ctx.font <- "14px 'Helvetica Neue', Arial, sans-serif"
    fill ctx "#cfd6ee"
    ctx.fillText (sprintf "Kampfzeit %d:%02d" (seconds / 60) (seconds % 60), px + pw - 24.0, py + 28.0)

    let colX = [ px + 330.0; px + 450.0 ]

    g.Players
    |> List.iteri (fun i p ->
        let x = List.item i colX
        ctx.save ()
        ctx.translate (x - 30.0, py + 70.0)
        ctx.scale (1.6, 1.6)
        drawHead ctx (paletteFor p.Fighter (isAlt g p)) p.Fighter (if Some p.Id = g.Winner then Fierce else Hurt) g.Frame true
        ctx.restore ()
        ctx.textAlign <- "left"
        ctx.font <- sprintf "italic 18px %s" displayFont
        outlinedText ctx (playerName p) (x - 8.0) (py + 64.0) (slotColor p) 4.0
        ctx.font <- "11px 'Helvetica Neue', Arial, sans-serif"
        fill ctx "#cfd6ee"
        ctx.fillText ((fighterStats p.Fighter).Name, x - 8.0, py + 82.0))

    let rows: (string * (Player -> float) * (Player -> string) * bool) list =
        [ "KOs", (fun p -> float p.KOs), (fun p -> string p.KOs), true
          "Stürze", (fun p -> float p.Falls), (fun p -> string p.Falls), false
          "Schaden ausgeteilt", (fun p -> p.DamageDealt), (fun p -> sprintf "%d %%" (int p.DamageDealt)), true
          "Längste Combo", (fun p -> float p.MaxCombo), (fun p -> if p.MaxCombo >= 2 then sprintf "%d Hits" p.MaxCombo else "-"), true
          "Übrige Stocks", (fun p -> float p.Stocks), (fun p -> string p.Stocks), true ]

    rows
    |> List.iteri (fun r (label, value, show, higherBetter) ->
        let ry = py + 116.0 + float r * 36.0

        if r % 2 = 0 then
            fill ctx "rgba(255,255,255,0.05)"
            ctx.fillRect (px + 12.0, ry - 16.0, pw - 24.0, 32.0)

        ctx.textAlign <- "left"
        ctx.font <- "15px 'Helvetica Neue', Arial, sans-serif"
        fill ctx "#e9e6f5"
        ctx.fillText (label, px + 26.0, ry)
        let values = g.Players |> List.map value
        let best = if higherBetter then List.max values else List.min values

        g.Players
        |> List.iteri (fun i p ->
            let x = List.item i colX
            let isBest = value p = best && List.distinct values |> List.length > 1
            ctx.textAlign <- "center"
            ctx.font <- sprintf "italic %dpx %s" (if isBest then 22 else 19) displayFont
            outlinedText ctx (show p) (x + 10.0) ry (if isBest then "#ffd166" else "#ffffff") 4.0))

    let items = [ "REVANCHE"; "KÄMPFERWAHL"; "HAUPTMENÜ" ]
    items |> List.iteri (fun i label -> menuRow ctx g.Frame label 960.0 (510.0 + float i * 60.0) 360.0 (i = g.MenuIndex))
    footerHint ctx "W / S  oder  ↑ / ↓  wählen   ·   Enter / F  bestätigen"

// ---------------------------------------------------------------------------
// Einstiegspunkt
// ---------------------------------------------------------------------------

let private shakeOffset (g: Game) : Vec2 =
    let t = float g.Frame
    let along = cos (t * 2.4) * g.ShakeMag
    let across = sin (t * 3.9) * g.ShakeMag * 0.3

    { X = g.ShakeDir.X * along - g.ShakeDir.Y * across
      Y = g.ShakeDir.Y * along + g.ShakeDir.X * across }

let private drawArena (ctx: Ctx) (g: Game) =
    let layout = stageLayout g.Stage (max 0 (g.MatchFrames - 1))
    updateCamera g
    ctx.save ()
    applyBackgroundCamera ctx
    drawBackground ctx g.Frame g.Stage
    ctx.restore ()
    let offset = shakeOffset g
    let visible = g.Players |> List.filter (fun p -> p.State <> KO)
    // Impact-Frames: erst schwarzes Bild mit weißen Silhouetten, dann ein heller Nachblitz
    let impactAge = impactFrames - g.ImpactFlash
    let blackout = g.ImpactFlash > 0 && impactAge < 4

    ctx.save ()
    ctx.translate (offset.X, offset.Y)
    applyCamera ctx
    drawStage ctx g.Frame layout
    drawForeground ctx g.Frame g.Stage
    visible |> List.iter (drawGroundShadow ctx layout)
    visible |> List.iter (drawRespawnPlatform ctx g.Frame)

    if blackout then
        fill ctx "#05060d"
        ctx.fillRect (-200.0, -200.0, screenWidth + 400.0, screenHeight + 400.0)
        drawImpactLines ctx g.ImpactPos "#ffffff" g.Frame
        visible |> List.iter (fun p -> drawFighter ctx g.Frame (Silhouette "#ffffff") false p)
        g.Particles |> List.filter (isBackParticle >> not) |> List.iter (drawParticle ctx)
    else
        g.Hazard |> Option.iter (drawHazard ctx g.Frame layout)
        g.Particles |> List.filter isBackParticle |> List.iter (drawParticle ctx)
        g.Ball |> Option.iter (drawBall ctx g.Frame)
        g.Items |> List.iter (drawItem ctx g.Frame)

        // Gehaltene Kämpfer zuerst, damit der Greifer davor steht
        let ordered = visible |> List.sortBy (fun p -> if p.State = Held then 0 else 1)

        for p in ordered do
            drawStatusAura ctx g.Frame p
            // Hit-Lag: Getroffene zittern während des Freeze-Frames
            let jitter = if g.HitStop > 0 && p.State = Hitstun && p.StateFrame = 0 then sin (float g.Frame * 2.7) * 3.5 else 0.0
            ctx.save ()
            ctx.translate (jitter, 0.0)
            drawFighter ctx g.Frame Shaded (isAlt g p) p
            ctx.restore ()
            drawShield ctx g.Frame p
            drawParryFlash ctx p

        g.Projectiles |> List.iter (drawProjectile ctx g.Frame)
        g.Particles |> List.filter (isBackParticle >> not) |> List.iter (drawParticle ctx)

    g.Beams |> List.iter (drawBeam ctx g.Frame)

    if not blackout then
        visible |> List.iter (drawNameTag ctx)

    if g.ShowBoxes then
        drawDebug ctx g

    ctx.restore ()

    match g.Ultra with
    | Some u when u.Timer >= ultraCutIn ->
        ctx.save ()
        ctx.translate (offset.X, offset.Y)
        drawUltraStrike ctx g.Frame u
        ctx.restore ()
    | _ -> ()

    if g.ImpactFlash > 0 && not blackout then
        fill ctx (sprintf "rgba(255,255,255,%.2f)" (0.5 * float g.ImpactFlash / 2.0))
        ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

    // Weiche Vignette für mehr Tiefe
    let vignette = ctx.createRadialGradient (screenWidth / 2.0, screenHeight / 2.0, 360.0, screenWidth / 2.0, screenHeight / 2.0, 820.0)
    vignette.addColorStop (0.0, "rgba(0,0,0,0)")
    vignette.addColorStop (1.0, "rgba(5,4,20,0.38)")
    ctx.fillStyle <- !^vignette
    ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)

    g.Players |> List.iter (fun p -> drawOffscreenMarker ctx g.Frame (isAlt g p) p)
    g.Hazard |> Option.iter (drawHazardBanner ctx g.Frame)
    drawHud ctx g
    drawCountdown ctx g

    match g.Ultra with
    | Some u when u.Timer < ultraCutIn ->
        let alt = g.Players |> List.tryFind (fun p -> p.Id = u.Owner) |> Option.map (isAlt g) |> Option.defaultValue false
        drawUltraCutIn ctx g.Frame u alt
    | _ -> ()

let draw (ctx: Ctx) (g: Game) =
    match g.Screen with
    | Title -> drawTitle ctx g
    | CharSelect -> drawCharSelect ctx g
    | StageSelect -> drawStageSelect ctx g
    | Playing ->
        drawArena ctx g

        if g.Paused then
            fill ctx "rgba(5,6,18,0.62)"
            ctx.fillRect (0.0, 0.0, screenWidth, screenHeight)
            ctx.save ()
            ctx.translate (screenWidth / 2.0, 300.0)
            ctx.transform (1.0, 0.0, -0.2, 1.0, 0.0, 0.0)
            fill ctx "rgba(5,6,15,0.9)"
            ctx.fillRect (-260.0, -60.0, 520.0, 120.0)
            fill ctx "#ffd166"
            ctx.fillRect (-260.0, 54.0, 520.0, 6.0)
            ctx.restore ()
            ctx.font <- sprintf "italic 64px %s" displayFont
            ctx.textAlign <- "center"
            ctx.textBaseline <- "middle"
            outlinedText ctx "PAUSE" (screenWidth / 2.0) 296.0 "#ffffff" 8.0
            ctx.font <- "16px 'Helvetica Neue', Arial, sans-serif"
            fill ctx "#e9e6f5"
            ctx.fillText ("START / Enter / OPTIONS: weiter    ·    MENÜ / Esc / SHARE: Kampf beenden", screenWidth / 2.0, 400.0)
    | GameOver -> drawGameOver ctx g
