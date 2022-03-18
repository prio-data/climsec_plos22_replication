;Simple Conflict Event Simulator
;2020 by Sebastian Schutte
;Released under GPL-3.0

extensions [ gis ]

globals [
  min_custom
  max_custom
  min_population
  max_population
  long_stepsize
  lat_stepsize
  population
  simulated_total_population
  empirical_total_population
  custom
  country
  last_country
  ;conflict
  relevant_patches
  sim_violence_counter
  true_positive_counter
  false_positive_counter
  cell_id_counter
  run_id
  ;vidframe
  ;last_country
  max_relevant_pxcor
  max_relevant_pycor
  min_relevant_pxcor
  min_relevant_pycor
]

patches-own [
  attack_memory
  neighborhood
  neighborhood_event_list
  sidea_event_list
  sideb_event_list
  sideb_simulated_events
  n_sim_ins_attack
  patch_population
  patch_custom
  geo_coords
  sort-ll-ur
  cell_id
  random_lookup
  nearest_base
  ;is-relevant?
]

turtles-own[
  random_base_lookup
]

to setup
  ;set vidframe 0
  ;for testing
  ifelse (cell_id_counter = 0 or conflict_country != last_country)[
    show "Running initial setup"
    set run_id 1
    clear-all
    reset-ticks
    ifelse random_seed = 0[
      random-seed new-seed
    ][
      random-seed random_seed
    ]
    setup-space
    setup-agents
    set last_country conflict_country
  ][
    show "Doing a reset"
    ;setup-space
    set run_id run_id + 1
    reset-ticks
    reset-agents
    set sim_violence_counter 0
    set true_positive_counter 0
    set false_positive_counter 0
  ]
end

to setup-space
  set cell_id_counter 1

  gis:load-coordinate-system (word "country_data/" conflict_country ".prj")
  set country gis:load-dataset (word "country_data/" conflict_country ".shp")
  set population gis:load-dataset (word "country_data/" conflict_country ".asc")
  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of country)
                                                (gis:envelope-of population))
  gis:set-drawing-color blue
  gis:draw country 1


  ;;Fix coord issue:
  set max_relevant_pxcor [pxcor] of max-one-of patches with [gis:intersects? country self] [pxcor]
  set max_relevant_pycor [pycor] of max-one-of patches with [gis:intersects? country self] [pycor]
  set min_relevant_pxcor [pxcor] of min-one-of patches with [gis:intersects? country self] [pxcor]
  set min_relevant_pycor [pycor] of min-one-of patches with [gis:intersects? country self] [pycor]
  ;gis:set-transformation-ds gis:envelope-of country (list min_relevant_pxcor max_relevant_pxcor min_relevant_pycor max_relevant_pycor)

  ;Calculate geo coords for each patch
  let long_origin item 0 gis:world-envelope ;gis:envelope-of country
  let lat_origin item 2 gis:world-envelope ;gis:envelope-of country

  set long_stepsize ((item 1 gis:envelope-of country - item 0 gis:envelope-of country) / (max_relevant_pxcor - min_relevant_pxcor))
  set lat_stepsize ((item 3 gis:envelope-of country - item 2 gis:envelope-of country) / (max_relevant_pycor - min_relevant_pycor))

  ask patches[
    set geo_coords list (long_origin + (long_stepsize * pxcor)) (lat_origin + (lat_stepsize * pycor))
    set sort-ll-ur pxcor + (max-pxcor * pycor)
    set pcolor black
  ]

  ;Attempt to solve resolution issue... Not really working:
  gis:set-sampling-method population "NEAREST_NEIGHBOR"
  set population gis:resample population gis:world-envelope max-pxcor max-pycor

end


to setup-agents
  ask turtles [die]
  set min_population 10000
  set max_population 0

  ask patches[
    set patch_population gis:raster-sample population self
  ]

  ;Make sure the patches are inside the polygon and the population values are not NaN
  set relevant_patches patches with [gis:intersects? country self and ((patch_population <= 0) or (patch_population >= 0))]
  ;Modified to accomodate PG border cells:
  ;set relevant_patches patches with [((patch_population <= 0) or (patch_population >= 0))]

  ;Correct for double counting at high simulation resolutions:
  set simulated_total_population round sum [patch_population] of relevant_patches
  set empirical_total_population gis:property-value first gis:feature-list-of country "TOTAL_POP" * 1000

  let correction_factor empirical_total_population / simulated_total_population
  ask relevant_patches[
    set patch_population patch_population * correction_factor
  ]


  ;add bases for correlated errors
  ;with [gis:intersects? country self][
  ask n-of n_errors relevant_patches[
    sprout 1[
      set color red
      set size 3
      set shape "circle"

      set random_base_lookup n-values n_sim_steps [(random-float (ec_p * 2)) - ec_p]
      ]
    ]
  ask relevant_patches[
    if patch_population > max_population[
      set max_population patch_population
    ]
    if patch_population < min_population[
      set min_population patch_population
    ]
    if patch_custom > max_custom[
      set max_custom patch_custom
    ]
    if patch_custom < min_custom[
      set min_custom patch_custom
    ]
    set sidea_event_list []
    set sideb_event_list []
    set neighborhood_event_list []
    set sideb_simulated_events []
    set attack_memory []
    ;Generating random numbers for patches between 0 and 1
    set random_lookup n-values n_sim_steps [random-float 1];[random 101 / 100]
    ;set nearest_base_distance
    set nearest_base [who] of min-one-of turtles [distance myself]

  ]
  ;resetCells
end

to reset-agents
  ask patches[;relevant_patches[
    set neighborhood_event_list []
    set sideb_simulated_events []
    set attack_memory []
  ]
end

to go
  ;See if we need to run setup first
  if cell_id_counter < 1[
    setup
  ]
  let sim_ins_attack? false
  ask relevant_patches[
    ;Simulate violence for each patch, starting from a 0 probability
    ;let patch_attack_n 0

    if binary-response patch_population per_capita_effect [
    ;;An attack has happened

      ifelse diffusion_effect = false [
        ;Normal local violence
        set sideb_simulated_events fput ticks sideb_simulated_events
        set sim_violence_counter sim_violence_counter + 1
        set sim_ins_attack? true
        set pcolor gray
        set n_sim_ins_attack n_sim_ins_attack + 1
      ][
        ;Has happened, but diffusion effect will relocate it
        if diffusion_type = "escalation"[
          set sim_violence_counter sim_violence_counter + 1
          set sim_ins_attack? true
          set pcolor gray
          set n_sim_ins_attack n_sim_ins_attack + 1
          ask one-of relevant_patches with [distance myself < diffusion_range]
          [
            set sideb_simulated_events fput ticks sideb_simulated_events
            set sim_violence_counter sim_violence_counter + 1
            set sim_ins_attack? true
            set pcolor gray
            set n_sim_ins_attack n_sim_ins_attack + 1
          ]
        ]
        if diffusion_type = "relocation"[
          ask one-of relevant_patches with [distance myself < diffusion_range]
          [
            set sideb_simulated_events fput ticks sideb_simulated_events
            set sim_violence_counter sim_violence_counter + 1
            set sim_ins_attack? true
            set pcolor gray
            set n_sim_ins_attack n_sim_ins_attack + 1
          ]
        ]
      ]
    ]
  ]

  if ticks mod 7 = 0 [update-violence-plots]
  tick
  ask relevant_patches[set pcolor black]

end



to-report sim_cellsize
  report (word (round lat_stepsize )"m X " (round long_stepsize) "m")
  ;"sim_event," pxcor "," pycor "
end
to-report random_pc_violence
  report per_capita_effect * patch_population
end

to-report diffusion_patch_violence
  report 0
end


to update-violence-plots
  set-current-plot "Actions per step"

  set-current-plot-pen "Simulated"
  plot sim_violence_counter
  set sim_violence_counter 0

end


to write-event-log
  ifelse (not file-exists? eventlogfile) [
    file-open eventlogfile
    file-print (word "type, xcor, ycor, long, lat, tick, cellid, run_id, experiment,")
  ][
    file-open eventlogfile
  ]
  ask relevant_patches[
    foreach sideb_simulated_events[ ?1 ->
      file-print (word "sim_event," pxcor "," pycor "," (item 0 geo_coords) "," (item 1 geo_coords) "," ?1 "," cell_id "," run_id "," experiment ",")
      ;Add random offset again
    ]
  ]
  file-close
end


to-report binary-response [value factor]
  ;report (-(item ticks random_lookup)) < (value * factor)
  ifelse ec_effect
  [
    ifelse ec_persistent
    [
      report (base_p + (value * factor)) + ([item 0 random_base_lookup] of turtle nearest_base) > item ticks random_lookup
    ]
    [
      report (base_p + (value * factor)) + ([item ticks random_base_lookup] of turtle nearest_base) > item ticks random_lookup
    ]
  ]
  [

    report (base_p + (value * factor))   > item ticks random_lookup

  ]
end
;;Adding a normally distributed error term
;show random-normal 10.1 5.2

to-report prio_cell_size
  report 0
end

to configure-experiment
  ;;Same across all experiments
  set diffusion_range 10
  set n_errors 2
  set n_reps 100
  set n_sim_steps 10 ;;Changed from 10 to 5 for half-GED event goal and speedups

  let pc_effect 0

  ;;Country-specific settings from 'config_experiment.R'
  if conflict_country = "Afghanistan"[
    set base_p 0.01521362
    set pc_effect 5.955409e-05
    set ec_p 0.01521362
    set diffusion_range 38
    set n_errors 34
    set eventlogfile "/home/basti/Desktop/gridexp/events_afghanistan.csv"
  ]
  if conflict_country = "Iraq"[
    set base_p 0.004762178
    set pc_effect 1.149975e-05
    set ec_p 0.004762178
    set diffusion_range 42
    set n_errors 23
    set eventlogfile "/home/basti/Desktop/gridexp/events_iraq.csv"
  ]
  if conflict_country = "Pakistan"[
    set base_p 0.004658053
    set pc_effect 1.594754e-06
    set ec_p 0.004658053
    set diffusion_range 26
    set n_errors 45
    set eventlogfile "/home/basti/Desktop/gridexp/events_pakistan.csv"
  ]
  if conflict_country = "Nepal"[
    set base_p 0.00356346
    set pc_effect 1.075518e-05
    set ec_p 0.00356346
    set diffusion_range 73
    set n_errors 9
    set eventlogfile "/home/basti/Desktop/gridexp/events_nepal.csv"
  ]
  if conflict_country = "Turkey"[
    set base_p 0.002715954
    set pc_effect 3.514435e-06
    set ec_p 0.002715954
    set diffusion_range 35
    set n_errors 44
    set eventlogfile "/home/basti/Desktop/gridexp/events_turkey.csv"
  ]
  if conflict_country = "Somalia"[
    set base_p 0.005530833
    set pc_effect 2.846125e-05
    set ec_p 0.005530833
    set diffusion_range 26
    set n_errors 30
    set eventlogfile "/home/basti/Desktop/gridexp/events_somalia.csv"
  ]
  if conflict_country = "Colombia"[
    set base_p 0.004451791
    set pc_effect 4.84063e-06
    set ec_p 0.004451791
    set diffusion_range 21
    set n_errors 50
    set eventlogfile "/home/basti/Desktop/gridexp/events_colombia.csv"
  ]
  ;;Experiments
  if experiment = 1[
    ;No diffusion, iid errors, true effect
    set per_capita_effect pc_effect
    set diffusion_effect false
    set diffusion_type "relocation"
    set ec_effect false
    set ec_persistent true
  ]
  if experiment = 2[
    ;No diffusion, iid errors, no effect
    set per_capita_effect 0
    set diffusion_effect false
    set diffusion_type "relocation"
    set ec_effect false
    set ec_persistent true
  ]
  if experiment = 3[
    ;No diffusion, LOCAL non-iid errors, true effect
    set per_capita_effect pc_effect
    set diffusion_effect false
    set diffusion_type "relocation"
    set ec_effect true
    set ec_persistent true
  ]
  if experiment = 4[
    ;No diffusion, LOCAL non-iid errors, no effect
    set per_capita_effect 0
    set diffusion_effect false
    set diffusion_type "relocation"
    set ec_effect true
    set ec_persistent true
  ]
  if experiment = 5[
    ;Diffusion, iid errors, true effect
    set per_capita_effect pc_effect
    set diffusion_effect true
    set diffusion_type "relocation"
    set ec_effect false
    set ec_persistent true
  ]
  if experiment = 6[
    ;Diffusion, iid errors, no effect
    set per_capita_effect 0
    set diffusion_effect true
    set diffusion_type "relocation"
    set ec_effect false
    set ec_persistent true
  ]
  if experiment = 7[
    ;Fully monty
    set per_capita_effect pc_effect
    set diffusion_effect true
    set diffusion_type "relocation"
    set ec_effect true
    set ec_persistent true
  ]
  if experiment = 8[
    set per_capita_effect 0
    set diffusion_effect true
    set diffusion_type "relocation"
    set ec_effect true
    set ec_persistent true
  ]
end

to generate-data
  foreach [1 2 3 4 5 6 7 8][ ?1 ->
    set experiment ?1
    configure-experiment
    let current_repetitions 1
    while [current_repetitions <= n_reps][
      random-seed run_id
      setup
      while [ticks < n_sim_steps][
        go
      ]
      write-event-log
      setup-agents
      reset-ticks
      set current_repetitions current_repetitions + 1
    ]
  ]
end

to test-population
  ask relevant_patches[
    let graysteps (max_population - min_population) / 10
    set pcolor (patch_population - min_population) / graysteps
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
1
29
610
439
-1
-1
1.0
1
10
1
1
1
0
0
0
1
0
600
0
400
1
1
1
ticks
30.0

BUTTON
630
141
690
174
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1115
47
1178
80
go
if ticks < n_sim_steps[\n  go\n]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
0
463
163
496
display-population
gis:paint population 0\ngis:set-drawing-color blue\ngis:draw country 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1289
16
1489
166
Actions per step
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"simulated" 1.0 0 -14439633 true "" ""

BUTTON
163
463
306
496
display-custom
gis:paint custom 0\ngis:set-drawing-color blue\ngis:draw country 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1115
164
1253
197
write event log
write-event-log
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1115
197
1384
257
eventlogfile
/home/basti/Desktop/gridexp/events_afghanistan.csv
1
0
String

SWITCH
627
335
775
368
diffusion_effect
diffusion_effect
1
1
-1000

TEXTBOX
6
10
494
28
Prio Conflict Simulator v0.5
12
0.0
1

INPUTBOX
826
247
934
307
per_capita_effect
5.955409E-5
1
0
Number

BUTTON
1278
395
1439
428
Generate data!
generate-data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
922
335
1083
395
diffusion_range
38.0
1
0
Number

BUTTON
1178
47
1242
80
step
if ticks < n_sim_steps[\n  go\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1117
394
1278
459
n_reps
100.0
1
0
Number

MONITOR
1116
79
1173
124
NIL
run_id
0
1
11

INPUTBOX
1117
335
1278
395
n_sim_steps
10.0
1
0
Number

CHOOSER
630
37
915
82
conflict_country
conflict_country
"Afghanistan" "Albania" "Algeria" "Andorra" "Angola" "Antigua and Barbuda" "Argentina" "Armenia" "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bhutan" "Bolivia" "Bosnia and Herzegovina" "Botswana" "Brazil" "Brunei" "Bulgaria" "Burkina Faso" "Burundi" "Cambodia" "Cameroon" "Canada" "Cape Verde" "Central African Republic" "Chad" "Chile" "China" "Colombia" "Comoros" "Congo" "Congo, DRC" "Costa Rica" "Cote d'Ivoire" "Croatia" "Cuba" "Cyprus" "Czech Republic" "Denmark" "Djibouti" "Dominica" "Dominican Republic" "Ecuador" "Egypt" "El Salvador" "Equatorial Guinea" "Eritrea" "Estonia" "Ethiopia" "Fiji" "Finland" "France" "Gabon" "Georgia" "Germany" "Ghana" "Greece" "Grenada" "Guatemala" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Honduras" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq" "Ireland" "Israel" "Italy" "Jamaica" "Japan" "Jordan" "Kazakhstan" "Kenya" "Kiribati" "Kuwait" "Kyrgyzstan" "Laos" "Latvia" "Lebanon" "Lesotho" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg" "Macedonia" "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta" "Marshall Is." "Mauritania" "Mauritius" "Mexico" "Micronesia" "Moldova" "Monaco" "Mongolia" "Morocco" "Mozambique" "Myanmar" "Namibia" "Nauru" "Nepal" "Netherlands" "New Zealand" "Nicaragua" "Niger" "Nigeria" "North Korea" "Norway" "Oman" "Pakistan" "Palau" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland" "Portugal" "Qatar" "Romania" "Russia" "Rwanda" "Samoa" "San Marino" "Sao Tome and Principe" "Saudi Arabia" "Senegal" "Serbia and Montenegro" "Seychelles" "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Solomon Is." "Somalia" "South Africa" "South Korea" "Spain" "Sri Lanka" "St. Kitts and Nevis" "St. Lucia" "St. Vincent and the Grenadines" "Sudan" "Suriname" "Swaziland" "Sweden" "Switzerland" "Syria" "Taiwan" "Tajikistan" "Tanzania" "Thailand" "The Gambia" "Togo" "Tonga" "Trinidad and Tobago" "Tunisia" "Turkey" "Turkmenistan" "Tuvalu" "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom" "United States" "Uruguay" "Uzbekistan" "Vanuatu" "Venezuela" "Vietnam" "Yemen" "Zambia" "Zimbabwe"
0

SWITCH
627
399
750
432
ec_effect
ec_effect
0
1
-1000

MONITOR
1173
79
1230
124
NIL
ticks
17
1
11

BUTTON
797
141
899
174
soft reset
;resetCells\nsetup-agents\nreset-ticks
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
905
399
997
432
n_errors
n_errors
0
50
34.0
1
1
NIL
HORIZONTAL

CHOOSER
1117
291
1255
336
experiment
experiment
1 2 3 4 5 6 7 8
2

BUTTON
1254
302
1439
335
configure experiment
;set elevation_effect true\n\nconfigure-experiment
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
630
16
951
34
1) Select and load country and data sources\n
14
0.0
1

TEXTBOX
631
225
781
243
2) Specify base DGP
14
0.0
1

TEXTBOX
1117
271
1325
291
6) Configure experiments
14
0.0
1

TEXTBOX
1116
27
1266
45
4) Run it!
14
0.0
1

TEXTBOX
1114
143
1297
161
5) Save the event data
14
0.0
1

INPUTBOX
720
247
809
307
base_p
0.01521362
1
0
Number

BUTTON
690
141
798
174
clear-all
clear-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
997
399
1084
459
ec_p
0.01521362
1
0
Number

SWITCH
750
399
905
432
ec_persistent
ec_persistent
0
1
-1000

MONITOR
630
174
778
219
simulated resolution
sim_cellsize
17
1
11

TEXTBOX
630
270
780
288
P(Y_it = 1) =
14
0.0
1

TEXTBOX
812
267
962
285
+
14
0.0
1

TEXTBOX
940
268
1090
286
NIL
14
0.0
1

INPUTBOX
630
81
999
141
custom_utm_raster
NIL
1
0
String

TEXTBOX
629
316
939
335
3) Add diffusion and error correlation
14
0.0
1

CHOOSER
774
335
922
380
diffusion_type
diffusion_type
"relocation" "escalation"
0

INPUTBOX
1278
335
1439
395
random_seed
12345.0
1
0
Number

TEXTBOX
2
445
152
463
Debugging:
12
0.0
1

BUTTON
164
496
306
529
NIL
test-population
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
0
496
165
529
display_total_pop
show sum [patch_population] of relevant_patches\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
919
174
1084
219
simulated population
round sum [patch_population] of relevant_patches
17
1
11

MONITOR
778
174
919
219
simulated locations
count relevant_patches
17
1
11

@#$#@#$#@
## WHAT IS IT?

This model was built to test and demonstrate the functionality of the GIS NetLogo extension.

## HOW IT WORKS

This model loads a raster file of surface elevation for a small area near Cincinnati, Ohio. It uses a combination of the gis:convolve primitive and simple NetLogo code to compute the slope (vertical angle) and aspect (horizontal angle) of the earth surface using the surface elevation data. Then it simulates raindrops flowing downhill over that surface by having turtles constantly reorient themselves in the direction of the aspect while moving forward at a constant rate.

## HOW TO USE IT

Press the setup button, then press the go button. You may press any of the "display-..." buttons at any time; they don't affect the functioning of the model.

## EXTENDING THE MODEL

It could be interesting to extend to model so that the "raindrop" turtles flow more quickly over steeper terrain. You could also add land cover information, and adjust the speed with which the turtles flow based on the land cover.

## RELATED MODELS

The other GIS code example, GIS General Examples, provides a greater variety of examples of how to use the GIS extension.

## CREDITS AND REFERENCES
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
setup
repeat 20 [ go ]
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
