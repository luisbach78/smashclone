# SmashClone

**Direkt im Browser spielen:** https://luisbach78.github.io/smashclone/

Läuft auf Mac, PC, Tablet und Handy (Handy bitte quer halten, Touch-Steuerung erscheint automatisch). PS4-, PS5-, Xbox- und Switch-Pro-Controller werden unterstützt.

2D-Platform-Fighter in F# mit Fable 5.2 und HTML5 Canvas. Keine Game-Engine, keine Assets. Grafik, Animationen und Sound entstehen vollständig im Code.

## Sofort spielen

`prebuilt/SmashClone.html` per Doppelklick im Browser öffnen. Die Datei enthält das fertig kompilierte Spiel und braucht weder .NET noch Node.

## Ablauf

Titelbildschirm (Modus und CPU-Stärke) → Kämpferwahl mit Match-Regeln (Stocks 1–5, Smash-Ball, Items und Stage-Ereignisse an/aus) → Stage-Wahl → Kampf → Siegerbildschirm mit Podium und Match-Statistik (Revanche, Kämpferwahl oder Hauptmenü).

## Kämpfer

Jeder Kämpfer hat Nahkampf- und Fernkampf-Angriffe. ◎ markiert Fernkampf-Moves, die Energie kosten. Die Kämpfer unterscheiden sich deutlich in der Reichweite (Balken „REICHW.“ in der Kämpferwahl). Valkyries Speer trifft fast doppelt so weit wie Specters Dolche, Chronos Magie entsteht auf Distanz, Ignis und Zephyr gleichen ihre kurze Reichweite mit Ausfallschritten aus.

| Kämpfer | Typ | Up-B | Side-B | G neutral | G + unten (Luft) | Ultra-Smash |
| --- | --- | --- | --- | --- | --- | --- |
| **Aether** | Allrounder, Schwert mit großer Reichweite | Skyward Slash | Blade Beam ◎ | Klingen-Smash | Sturzklinge | Himmelsklinge |
| **Ignis** | Rushdown, schnell und leicht | Rising Dragon | Flare Dash | Glutfaust + Feuerball ◎ | Axtkick | Inferno-Drache |
| **Titan** | Heavyweight mit Super-Armor | Jetpack Slam | Meteor Charge | Titanfaust, in der Luft Felsbrocken ◎ | Bodenstampfer | Meteorschauer |
| **Zephyr** | Zoner, Dreifachsprung | Schattensprung | Energie-Salve ◎ | Kunai-Fächer ◎ | Meteor-Tritt | Tausend Klingen |
| **Ronin** | Samurai, Konter-Meister | Kranichflug | Klingenwind ◎ | Iai-Schnitt (Ziehschnitt) | Konter-Haltung (auch G + unten am Boden) | Tausend Schnitte |
| **Lyra** | Mond-Bogenschützin, leicht und flink | Mondsprung | Sternpfeil ◎ | Pfeilsalve ◎ (drei Pfeile) | Sturzpfeil | Sternenregen |
| **Valkyrie** | Speer und Schild, große Reichweite | Valkyrie Ascent | Aegis Dash (reflektiert Projektile) | Lichtspeer-Wurf ◎ | Speersturz | Walhalla-Lanze |
| **Chrono** | Zeit-Magier, schwebt (Sprung halten) | Chrono-Blink | Stasis-Kugel ◎ (verlangsamt) | Zeit-Riss (Explosion) | Zeitanker | Zeitstillstand |
| **Bramble** | Natur-Bestie, höchster Sprung | Ranken-Schwung | Dornen-Salve ◎ | Pranken-Schlag | Wurzelstampfer | Urwald-Zorn |
| **Specter** | Schatten-Assassine, leicht und schnell in der Luft | Schattenflug | Schatten-Dolch ◎ | Phantom-Hieb (teleportiert hinter den Gegner) | Schattensturz | Schattenreich |
| **Gorm** | Steingolem und Ringer, schwerster Kämpfer | Felssprung (Super-Armor) | Felsgriff (packt durch Schilde) | Erdbeben-Faust (Super-Armor) | Meteoritensturz | Tektonik |

Spielt Spieler 2 denselben Kämpfer wie Spieler 1, bekommt er automatisch Alternativfarben.

## Anti-Spam-System

Unter jedem HUD-Panel liegt eine Energieleiste (EN). Sie lädt langsam von selbst, deutlich schneller durch Nahkampf-Treffer und etwas durch eingesteckten Schaden. Fernkampf-Moves kosten 18–45 Energie. Reicht sie nicht, verpufft der Angriff (Leiste blinkt rot), die Erholungszeit bleibt aber.

Dazu kommt Stale-Move-Negation. Die letzten 9 Moves landen in einer Warteschlange und senken Schaden und Knockback des gleichen Moves auf bis zu 50 %. Wer denselben Fernkampf-Move wiederholt, bekommt zusätzlich längere Erholungszeiten.

## Taktik

- **Griffe und Würfe.** Schild + Angriff greift. Ein Griff geht durch den Schild, verliert aber gegen Angriffe und hat bei einem Fehlgriff lange Erholung. Mit gegriffenem Gegner wählt die Richtung den Wurf, also vorwärts, rückwärts, hoch (KO-Wurf bei hohem Prozentwert) oder runter (Combo-Starter). Gegriffene befreien sich durch schnelles Tastenhämmern.
- **Perfect Shield (Parry).** Wer den Schild genau im Moment des Treffers hebt, pariert ohne Schildschaden, und der Angreifer ist kurz wehrlos.
- **Smash-Angriffe aufladen.** Wer G für einen Smash-Angriff am Boden (G neutral oder G + unten) gedrückt hält, lädt ihn bis zu einer Sekunde auf. Er leuchtet dabei immer heller und trifft bis zu 40 % stärker.
- **Rage.** Wer selbst schon viel Schaden hat, verteilt bis zu 12 % mehr Knockback, wie in Smash Bros.
- **Konter.** Ronins Konter-Haltung beantwortet Nahkampftreffer mit einem starken Gegenschlag.
- **Dynamische Kamera.** Sie zoomt heran, wenn die Kämpfer nah beieinander sind, und zieht bei weiten Abständen auf.

## Items

| Item | Wirkung |
| --- | --- |
| Heilherz | heilt 30 % |
| Kraft-Kristall | 10 Sekunden lang 25 % mehr Schaden und mehr Knockback |
| Tempo-Feder | 10 Sekunden lang schneller laufen und in der Luft steuern |
| Bombe | explodiert bei Berührung (nach kurzer Scharfschaltung), bei Treffern und nach Ablauf der Zeit und trifft alle im Radius |

Items fallen alle 9 bis 15 Sekunden vom Himmel und werden durch Berührung eingesammelt.

## Stages

| Stage | Besonderheit |
| --- | --- |
| **Sky Battlefield** | Klassische Hauptplattform mit drei Soft-Platforms, Himmel mit Tempelruinen |
| **Astral Altar** | Flach und breit ohne Plattformen, Galaxie-Hintergrund mit Sternschnuppen und Energiesäulen |
| **Magma Foundry** | Glutfunken über Lavasee, zwei bewegliche Plattformen (auf und ab bzw. seitlich), die Kämpfer korrekt mitnehmen |

Neu sind **Frostgipfel** (verschneiter Berggipfel im Polarlicht mit Eisplatten über dem Abgrund) und **Neon-Dach** (Hochhausdach in einer Neon-Stadt bei Nacht).

Jede Stage hat ein eigenes Ereignis, das alle 14 bis 21 Sekunden mit Warnbanner angekündigt wird. Auf Sky Battlefield sind es Windböen, auf Astral Altar Sternschnuppen-Einschläge, in der Magma Foundry Lava-Fontänen, auf dem Frostgipfel fallende Eiszapfen und auf dem Neon-Dach ein Sicherheitslaser, über den man springen muss. Sternschnuppe, Lava und Eiszapfen zielen auf einen der Kämpfer, ein Schild blockt sie.

## Smash-Ball

Ist er aktiviert, erscheint alle 25–35 Sekunden ein Smash-Ball, der durch die Arena schwebt und nach 20 Sekunden wieder verschwindet. Wer ihn zerschlägt, leuchtet in einer Regenbogen-Aura. Der nächste Special (G) löst den Ultra-Smash aus, mit Cut-In-Banner, bildschirmfüllender Attacke und massivem Knockback.

## CPU-Stärke

| Stufe | Reaktionszeit | Verhalten |
| --- | --- | --- |
| Leicht | 18–30 Frames | Denkt nur alle 10–16 Frames neu, schlägt oft daneben, schildet nie, recovert häufig zu spät |
| Mittel | 10–16 Frames | Menschliche Fehler, selten Schild, wartet an der Kante |
| Schwer | 5–8 Frames | Nutzt Projektile, Edgeguards und einfache Combos |
| Profi | 2–4 Frames | Volle Combos, springt zum Edgeguarden hinterher, schildet und weicht aus |

Die CPU sieht den Gegner mit dieser Verzögerung und hält ihre Eingaben zwischen zwei Entscheidungen.

## Steuerung

| Aktion | Spieler 1 | Spieler 2 |
| --- | --- | --- |
| Laufen | A / D | ← / → |
| Springen (kurz tippen = Short-Hop, in der Luft = Doppelsprung) | W | ↑ |
| Fast-Fall, durch Soft-Platform fallen | S | ↓ |
| Angriff (neutral Jab, + Richtung Tilt bzw. Air, unten in der Luft Meteor-Spike) | F | K |
| Smash und Specials (siehe Tabelle oben, G + unten am Boden = Down-Smash) | G | L |
| Schild (+ Richtung Rolle, in der Luft Air-Dodge) | H | Ö |
| Griff (danach Richtung = Wurf) | H + F | Ö + K |

### Touch (Handy und Tablet)

Links unten ist ein virtueller Stick (erscheint dort, wo der Daumen aufsetzt), rechts liegen die Knöpfe A (Angriff), B (Smash und Specials), ▲ (Sprung) und S (Schild, zusammen mit A = Griff). Oben in der Mitte sind START und MENÜ. Im Menü bestätigt A und S geht zurück. Gespielt wird im Querformat, am besten über „Zum Home-Bildschirm“ im Vollbild.

### Controller (PS4, PS5, Xbox, Switch Pro)

Ein per Bluetooth oder USB verbundener Controller funktioniert direkt im Browser (Chrome, Safari, Edge oder Firefox). Controller 1 steuert Spieler 1, ein zweiter Controller Spieler 2. Tastatur und Controller lassen sich mischen, zum Beispiel Spieler 1 am Controller und Spieler 2 an den Pfeiltasten.

| Aktion | PS4 / PS5 |
| --- | --- |
| Laufen, Richtung, Fast-Fall | Linker Stick oder Steuerkreuz |
| Springen | ✕ oder △ (Stick nach oben springt bewusst nicht, damit Up-Air und Up-B sauber gehen) |
| Angriff | □ |
| Smash und Specials | ○ |
| Schild, Rolle, Air-Dodge | L1, R1, L2 oder R2 |
| Griff | Schild + □ |
| Menü bestätigen / zurück | ✕ / ○ |
| Start (Weiter, Kampf starten) | OPTIONS |
| Hauptmenü (nur in Menüs) | SHARE |

Der Browser meldet einen Controller erst, nachdem auf der Seite einmal ein Knopf gedrückt wurde. Den Sound gibt manche Browser erst nach einem Tastendruck oder Klick frei.

Sprung, Angriff und Special werden 6 Frames lang gebuffert. Ein zu früh gedrückter Angriff kommt also im ersten möglichen Frame.

Im Menü wählt man mit W / S bzw. ↑ / ↓, A / D ändert Werte, Enter oder F bestätigt, H geht zurück. In der Kämpferwahl bewegt man den Cursor mit WASD durch das 4x2-Raster. Im 1P-Modus wählt Spieler 1 zuerst den eigenen Kämpfer und dann den der CPU, im 2P-Modus wählen beide gleichzeitig mit ihren eigenen Tasten. Sobald beide feststehen, stellt man mit W / S und A / D die Match-Regeln ein, Enter führt zur Stage-Wahl. `T` blendet Hurtboxen, Hitboxen, Projektil-Boxen und Frame-Zähler ein, `Esc` führt ins Hauptmenü.

## Entwicklung

Voraussetzungen sind das .NET SDK 10 (Fable 5 läuft auf net10.0) und Node.js ab 20.19 bzw. 22.12 (Vite 8).

```bash
cd SmashClone
dotnet tool restore      # installiert Fable 5.2.0 aus .config/dotnet-tools.json
dotnet restore           # lädt Fable.Core und Fable.Browser.Dom
npm install              # installiert Vite
npm run dev              # Fable im Watch-Modus + Vite-Devserver auf http://localhost:5173
```

`npm run build` erzeugt einen Produktions-Build in `dist/`, `npm run preview` liefert ihn lokal aus.

## Architektur

Die Dateien stehen in `SmashClone.fsproj` in Abhängigkeitsreihenfolge.

| Datei | Inhalt |
| --- | --- |
| `src/Types.fs` | Alle DUs und Records (Kämpfer, Projektile, Stages, Smash-Ball, Ultra), Kämpfer-Werte, Stage-Layouts als Funktion der Zeit, Konstanten |
| `src/Audio.fs` | Web-Audio-Synthesizer für die Soundeffekte |
| `src/AI.fs` | CPU mit Schwierigkeitsprofilen, Reaktionsverzögerung, Energie-Management, Smash-Ball-Jagd und Ultra-Timing |
| `src/Physics.fs` | Movesets, Zustandsmaschine, Input-Buffer, Projektile, Reflektor, Stasis, Energie und Stale-Moves, bewegliche Plattformen, Smash-Ball, Ultra-Smash, Knockback, Partikel |
| `src/Renderer.fs` | Cel-Shading-Renderer mit Skelett-Kämpfern, fünf Parallax-Stages, Items, Stage-Ereignisse, dynamische Kamera, VFX, Ultra-Cut-Ins, HUD, Titel, Kämpferwahl, Stage-Wahl, Siegerbildschirm |
| `src/App.fs` | Input, Menü-Ablauf und Main-Loop |

Die Simulation ist eine reine Funktion `Physics.step : Input list -> Game -> Game` und läuft über einen Akkumulator immer in Schritten von 1/60 Sekunde. Bildwiederholraten von 60 Hz und 144 Hz ergeben dadurch identische Physik und Frame-Data. Auch der Zufall (Partikel, KI) steckt als Seed im Game-State.

Knockback folgt `(BaseKnockback + KnockbackScaling * TargetPercent) * Gewichtsfaktor`. Der Gewichtsfaktor ist `200 / (100 + Gewicht)`, also 1,0 für Aether und rund 0,83 für Titan. Daraus ergeben sich Fluggeschwindigkeit, Hitstun-Dauer und Hit-Stop. Die Normals stehen als Basiswerte in `Physics.normals` und werden pro Kämpfer getunt, die Specials stehen in `Physics.special`, die Projektile in `Physics.projectileData`.

## Grafik

Die Kämpfer sind mehrteilige Skelette. `Renderer.poseOf` legt je Zustand nur Ziele für Hüfte, Hände und Füße fest, Ellbogen und Knie löst eine 2-Knochen-IK. Die Hieb-Bögen folgen den Winkeln aus `Renderer.swingData`. Umhang, Stirnband und Schal sind eine Stoffkette im Game-State (`Physics.updateCloth`) und wehen abhängig von der Bewegung.

Die statischen Ebenen jeder Stage (Himmel, Ruinen, Galaxie, Gießerei-Hallen) werden einmalig in Offscreen-Canvases gezeichnet und danach pro Frame nur kopiert. Pro Frame neu gezeichnet werden Kämpfer, Projektile, Partikel, Effekte, HUD und Menüs.
