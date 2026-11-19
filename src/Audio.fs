module SmashClone.Audio

open Fable.Core
open SmashClone.Types

// ---------------------------------------------------------------------------
// Minimale, typisierte Web-Audio-Bindings (keine externen Pakete nötig)
// ---------------------------------------------------------------------------

type AudioParam =
    abstract value: float with get, set
    abstract setValueAtTime: value: float * time: float -> unit
    abstract linearRampToValueAtTime: value: float * time: float -> unit
    abstract exponentialRampToValueAtTime: value: float * time: float -> unit

type AudioNode =
    abstract connect: destination: AudioNode -> unit

type OscillatorNode =
    inherit AudioNode
    abstract ``type``: string with get, set
    abstract frequency: AudioParam
    abstract start: time: float -> unit
    abstract stop: time: float -> unit

type GainNode =
    inherit AudioNode
    abstract gain: AudioParam

type BiquadFilterNode =
    inherit AudioNode
    abstract ``type``: string with get, set
    abstract frequency: AudioParam

type AudioBuffer =
    abstract getChannelData: channel: int -> float32[]

type AudioBufferSourceNode =
    inherit AudioNode
    abstract buffer: AudioBuffer with get, set
    abstract start: time: float -> unit
    abstract stop: time: float -> unit

type AudioContext =
    abstract currentTime: float
    abstract sampleRate: float
    abstract state: string
    abstract destination: AudioNode
    abstract resume: unit -> unit
    abstract createOscillator: unit -> OscillatorNode
    abstract createGain: unit -> GainNode
    abstract createBiquadFilter: unit -> BiquadFilterNode
    abstract createBufferSource: unit -> AudioBufferSourceNode
    abstract createBuffer: channels: int * length: int * sampleRate: float -> AudioBuffer

[<Emit("(typeof window !== 'undefined' && (window.AudioContext || window.webkitAudioContext)) ? new (window.AudioContext || window.webkitAudioContext)() : null")>]
let private createContext () : AudioContext option = jsNative

[<Emit("Math.random()")>]
let private jsRandom () : float = jsNative

let mutable private context: AudioContext option = None
let mutable private master: GainNode option = None
let mutable private noiseBuffer: AudioBuffer option = None

/// Muss aus einer Nutzergeste heraus aufgerufen werden (Autoplay-Richtlinien der Browser)
let unlock () =
    match context with
    | Some ctx -> if ctx.state = "suspended" then ctx.resume ()
    | None ->
        match createContext () with
        | Some ctx ->
            let gain = ctx.createGain ()
            gain.gain.value <- 0.55
            gain.connect ctx.destination
            let length = int ctx.sampleRate
            let buffer = ctx.createBuffer (1, length, ctx.sampleRate)
            let data = buffer.getChannelData 0

            for i in 0 .. length - 1 do
                data[i] <- float32 (jsRandom () * 2.0 - 1.0)

            context <- Some ctx
            master <- Some gain
            noiseBuffer <- Some buffer
        | None -> ()

/// Oszillator mit Tonhöhen-Sweep und perkussiver Hüllkurve
let private tone (wave: string) (freqFrom: float) (freqTo: float) (duration: float) (volume: float) (delay: float) =
    match context, master with
    | Some ctx, Some out ->
        let t0 = ctx.currentTime + delay
        let osc = ctx.createOscillator ()
        let gain = ctx.createGain ()
        osc.``type`` <- wave
        osc.frequency.setValueAtTime (freqFrom, t0)
        osc.frequency.exponentialRampToValueAtTime (max 1.0 freqTo, t0 + duration)
        gain.gain.setValueAtTime (0.0001, t0)
        gain.gain.linearRampToValueAtTime (volume, t0 + 0.006)
        gain.gain.exponentialRampToValueAtTime (0.0001, t0 + duration)
        osc.connect gain
        gain.connect out
        osc.start t0
        osc.stop (t0 + duration + 0.03)
    | _ -> ()

/// Gefiltertes Rauschen mit Filter-Sweep (Schläge, Explosionen, Luftzüge)
let private noise (filterType: string) (cutFrom: float) (cutTo: float) (duration: float) (volume: float) (delay: float) =
    match context, master, noiseBuffer with
    | Some ctx, Some out, Some buffer ->
        let t0 = ctx.currentTime + delay
        let source = ctx.createBufferSource ()
        let filter = ctx.createBiquadFilter ()
        let gain = ctx.createGain ()
        source.buffer <- buffer
        filter.``type`` <- filterType
        filter.frequency.setValueAtTime (cutFrom, t0)
        filter.frequency.exponentialRampToValueAtTime (max 20.0 cutTo, t0 + duration)
        gain.gain.setValueAtTime (volume, t0)
        gain.gain.exponentialRampToValueAtTime (0.0001, t0 + duration)
        source.connect filter
        filter.connect gain
        gain.connect out
        source.start t0
        source.stop (t0 + duration + 0.03)
    | _ -> ()

let play (sfx: Sfx) =
    match sfx with
    | SfxJump -> tone "square" 300.0 640.0 0.13 0.22 0.0
    | SfxDoubleJump ->
        tone "square" 460.0 920.0 0.12 0.2 0.0
        tone "triangle" 920.0 1380.0 0.1 0.12 0.05
    | SfxLand -> noise "lowpass" 700.0 180.0 0.07 0.25 0.0
    | SfxSwing -> noise "bandpass" 2600.0 700.0 0.09 0.22 0.0
    | SfxUpSpecial ->
        tone "sawtooth" 220.0 1100.0 0.28 0.2 0.0
        noise "highpass" 900.0 4200.0 0.25 0.16 0.0
    | SfxHit ->
        noise "lowpass" 2400.0 400.0 0.09 0.5 0.0
        tone "square" 240.0 90.0 0.1 0.28 0.0
    | SfxHeavyHit ->
        noise "lowpass" 3800.0 140.0 0.34 0.85 0.0
        tone "sawtooth" 170.0 38.0 0.38 0.5 0.0
        tone "square" 900.0 110.0 0.07 0.3 0.0
    | SfxShield -> tone "triangle" 520.0 780.0 0.09 0.14 0.0
    | SfxShieldHit ->
        tone "triangle" 980.0 720.0 0.12 0.3 0.0
        tone "sine" 1480.0 1320.0 0.16 0.18 0.0
    | SfxShieldBreak ->
        tone "sawtooth" 1200.0 80.0 0.5 0.4 0.0
        noise "highpass" 5000.0 800.0 0.4 0.4 0.0
    | SfxDodge -> noise "bandpass" 600.0 2600.0 0.14 0.2 0.0
    | SfxLedge -> tone "triangle" 660.0 880.0 0.07 0.2 0.0
    | SfxKO ->
        noise "lowpass" 5200.0 70.0 0.95 1.0 0.0
        tone "sawtooth" 420.0 28.0 0.85 0.5 0.0
        tone "sine" 95.0 34.0 0.7 0.7 0.0
        tone "square" 1500.0 200.0 0.18 0.25 0.0
    | SfxCountdown -> tone "square" 440.0 440.0 0.14 0.25 0.0
    | SfxGo ->
        tone "square" 880.0 880.0 0.4 0.28 0.0
        tone "square" 1320.0 1320.0 0.4 0.14 0.0
    | SfxMenuMove -> tone "square" 620.0 700.0 0.05 0.16 0.0
    | SfxMenuBack ->
        tone "square" 520.0 330.0 0.09 0.18 0.0
    | SfxPick ->
        tone "square" 660.0 660.0 0.07 0.2 0.0
        tone "square" 990.0 990.0 0.07 0.2 0.06
        tone "square" 1320.0 1320.0 0.16 0.22 0.12
        noise "highpass" 3000.0 6000.0 0.12 0.1 0.12
    | SfxProjectile ->
        tone "sawtooth" 980.0 320.0 0.14 0.16 0.0
        noise "highpass" 2400.0 5200.0 0.1 0.14 0.0
    | SfxTeleport ->
        tone "sine" 300.0 1800.0 0.16 0.22 0.0
        tone "sine" 1800.0 400.0 0.14 0.16 0.12
        noise "bandpass" 1200.0 5000.0 0.2 0.1 0.0
    | SfxArmor ->
        tone "square" 170.0 120.0 0.14 0.32 0.0
        noise "lowpass" 1800.0 300.0 0.14 0.3 0.0
        tone "triangle" 1400.0 1100.0 0.1 0.12 0.0
    | SfxShockwave ->
        noise "lowpass" 1300.0 60.0 0.5 0.75 0.0
        tone "sine" 90.0 30.0 0.45 0.65 0.0
    | SfxCombo ->
        tone "square" 880.0 1320.0 0.07 0.14 0.0
        tone "square" 1320.0 1760.0 0.08 0.14 0.06
    | SfxDash -> noise "bandpass" 900.0 3200.0 0.16 0.2 0.0
    | SfxFizzle ->
        tone "square" 220.0 110.0 0.12 0.16 0.0
        noise "lowpass" 900.0 200.0 0.14 0.14 0.0
    | SfxReflect ->
        tone "triangle" 1600.0 2400.0 0.1 0.28 0.0
        tone "square" 800.0 1200.0 0.08 0.14 0.0
    | SfxStasis ->
        tone "sine" 1200.0 300.0 0.4 0.24 0.0
        tone "triangle" 600.0 150.0 0.45 0.16 0.05
    | SfxRift ->
        noise "bandpass" 400.0 3000.0 0.3 0.4 0.0
        tone "sawtooth" 90.0 400.0 0.3 0.22 0.0
    | SfxBallAppear ->
        tone "sine" 880.0 880.0 0.12 0.16 0.0
        tone "sine" 1320.0 1320.0 0.12 0.16 0.1
        tone "sine" 1760.0 1760.0 0.2 0.16 0.2
    | SfxBallHit ->
        tone "triangle" 1400.0 1000.0 0.08 0.26 0.0
        noise "highpass" 3000.0 6000.0 0.08 0.14 0.0
    | SfxBallBreak ->
        noise "highpass" 2000.0 8000.0 0.5 0.4 0.0
        tone "square" 523.0 523.0 0.1 0.2 0.0
        tone "square" 784.0 784.0 0.1 0.2 0.08
        tone "square" 1046.0 1046.0 0.3 0.22 0.16
    | SfxUltraCutIn ->
        tone "sawtooth" 110.0 880.0 0.7 0.3 0.0
        noise "bandpass" 300.0 5000.0 0.7 0.25 0.0
    | SfxUltraBlast ->
        noise "lowpass" 6000.0 60.0 1.2 1.0 0.0
        tone "sawtooth" 300.0 25.0 1.1 0.55 0.0
        tone "sine" 70.0 30.0 1.0 0.8 0.0
    | SfxCharge ->
        tone "sawtooth" 180.0 720.0 0.9 0.12 0.0
        noise "bandpass" 800.0 3200.0 0.9 0.08 0.0
    | SfxGrab ->
        noise "bandpass" 1200.0 500.0 0.08 0.3 0.0
        tone "triangle" 220.0 160.0 0.1 0.2 0.0
    | SfxThrow ->
        noise "highpass" 600.0 3200.0 0.18 0.3 0.0
        tone "sawtooth" 380.0 120.0 0.2 0.18 0.0
    | SfxParry ->
        tone "sine" 1760.0 1760.0 0.18 0.25 0.0
        tone "triangle" 2637.0 2637.0 0.3 0.18 0.03
        noise "highpass" 4000.0 8000.0 0.08 0.2 0.0
    | SfxItemSpawn ->
        tone "triangle" 880.0 1320.0 0.12 0.16 0.0
        tone "triangle" 1320.0 1760.0 0.12 0.14 0.1
    | SfxPickup ->
        tone "square" 660.0 990.0 0.08 0.18 0.0
        tone "square" 990.0 1320.0 0.1 0.18 0.07
    | SfxHeal ->
        tone "sine" 523.0 784.0 0.25 0.22 0.0
        tone "sine" 784.0 1046.0 0.3 0.18 0.12
    | SfxExplosion ->
        noise "lowpass" 3000.0 80.0 0.7 0.9 0.0
        tone "sine" 110.0 35.0 0.6 0.7 0.0
    | SfxWarning ->
        tone "square" 880.0 880.0 0.12 0.16 0.0
        tone "square" 660.0 660.0 0.12 0.16 0.16
        tone "square" 880.0 880.0 0.12 0.16 0.32
    | SfxHazard ->
        noise "lowpass" 1800.0 120.0 0.6 0.6 0.0
        tone "sawtooth" 160.0 50.0 0.5 0.3 0.0
    | SfxMenuSelect ->
        tone "square" 520.0 520.0 0.08 0.2 0.0
        tone "square" 780.0 780.0 0.14 0.2 0.08
    | SfxVictory ->
        tone "square" 523.0 523.0 0.14 0.22 0.0
        tone "square" 659.0 659.0 0.14 0.22 0.14
        tone "square" 784.0 784.0 0.14 0.22 0.28
        tone "square" 1046.0 1046.0 0.4 0.24 0.42
