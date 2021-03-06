;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   DEFINITIONS                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; = regular comment
;; TBI = To Be Implemented

;; TBI: use table extension to setup when gis data has already been loaded in once (see https://www.jasss.org/20/1/3.html). ALternative csv method is partially set up in setup-least-cost-distances procedure
;; TBI: set walking cost of lakes to high number, so they become obstacles. Rivers would probably have been crossed readily. Current version = only most western lake inaccessible bc other ones not in range.
;; TBI: new forest growth function. Currently overshoots for some patches. Now solved using dirty fix.
;; TBI: communities check whether to convert wood to charcoal or not (now use of charcoal always implicitly assumed).
;; TBI: depreciation of the woodstock? When fields are cleared for agriculture, the resulting woodstock is so massive that it takes years to get through.
;; TBI: clay exploitation procedure with realistic values.


extensions [
  gis
  palette
  nw
  ;csv
]

globals [
  coordsys?                  ; boolean variable for checking if default coordinate system (WGS84) for all spatial data is set
  elevation-raster           ; elevation raster dataset
  fertility-raster           ; fertility raster dataset
  maxStandingStock-raster    ; maximum forest standing stock dataset
  rico-raster                ; dataset for calibrating forest growth function
  power-raster               ; dataset for calibrating forest growth function
  sites                      ; dataset with archaeological sites from area of Sagalassos
  start-date                 ; start dates of archaeological sites
  clay-raster                ; dataset with clay quantities calculated from ISRIC data
  walkingTime-raster         ; dataset with Tobler walking time precalculated
  waterBodies-raster         ; dataset with rivers and lakes
  regeneration-reserve       ; dummy value necessary for initializing agricultural regeneration
  burn-size                  ; required variable for keeping track of fire sizes
  bad-harvest-modifier       ; factor to account for occasional bad harvests
]

breed [communities community]
breed [inactive-communities inactive-community]
breed [rangers ranger]

patches-own [
  elevation              ;; elevation value, input via map.
  wood-age               ;; all patches are forest initially and are therefore given an age corresponding to a more or less mature forest. When cut, forest age is reset to zero.
  wood-maxStandingStock  ;; upper limit to grown wood contained within a forested patch (m??/ha). Calculated from GREFOS model runs, input via map.
  wood-rico              ;; Initial wood growth rate (m??/ha*year). Calculated from GREFOS model runs, input via map.
  wood-power             ;; Wood growth rate (1/year). Calculated from GREFOS model runs, input via map.
  wood?                  ;; variable to allow for wood regeneration
  wood-standingStock     ;; actual standing stock on a patch at a given time (m??/ha). Calculated from forest growth function: standingStock = rico * exp(age * power) (until standingStock = maxStandingStock)
  food-fertility         ;; crop yield on a cultivated patch (tons/(year*ha))
  original-food-value    ;; variable to allow food to be regenerated to the original value
  food?                  ;; variable to allow for food regeneration
  growth-rate            ;; crop growth rate in Verhulst function. Determined on the base of the regeneration-time variable
  clay?                  ;; variable defining whether or not a patch can be a clay source
  ;clay-quality          ;; quality of clay per source
  clay-quantity          ;; kgs of clay per patch up to a depth of 2 m. Calculated from ISRIC data, input via map.
  land?                  ;; variable defining whether or not the patch is on land
  walkingTime            ;; amount of time required to cross a patch (from preprocessed raster)
  in-range-of            ;; list of communities that could potentially make use of this patch for agriculture or forestry
  claimed-cost           ;; walking time cost from community to its claimed patches in same order as "claimed" list
  fire-return-rate       ;; number of ticks before fire should return
  time-since-fire        ;; number of ticks since last forest fire, used to test fire functionality
  time-since-abandonment ;; number of ticks since last use for agriculture (after a certain amount, convert back to forest)
]

communities-own [
  population          ;; population size
  workdays            ;; days of annual population engagement in food/wood/clay resourcing
  food-workdays       ;; population workdays spent on agriculture (limited to value by slider, see Goodchild 2007 p. 302: about 250 pc)
  food-requirement    ;; total requirement of food
  wood-requirement    ;; " wood. Depends on altitude.
  clay-requirement    ;; " clay
  wood-for-clay       ;; additional requirement of wood for firing clay
  food-stock          ;; cumulative stock of food brought in by households
  clay-stock          ;; cumulative stock of clay brought in by households
  wood-stock          ;; cumulative stock of wood brought in by households
  total-food-effort   ;; cumulative distances travelled exploiting resources by households
  total-wood-effort   ;; "
  total-clay-effort   ;; "
  site-name           ;; value for site name if historical dataset is used
  candidate-patches   ;; patches that are within range of the community.
  grain-per-grain-factor ;; factor accounting for resowing loss.
  settlement-type     ;; type of settlement. Determines amount of inhabitants
  start-period        ;; determines when a settlement needs to spawn (IA: Iron Age at start, ACH: Achaemenid after 450 years, HELL: Hellenistic after 650 years.)
  cumulative-food-stock ;; for setting out over time later on.
  cumulative-wood-stock
  cumulative-clay-stock
  saved-food-workdays
  saved-wood-workdays
  saved-clay-workdays
]

inactive-communities-own [ ; copy of communitities-breed to put communities that are yet to spawn (start-period hasn't arrived yet) on non-active.
  population
  workdays
  food-workdays
  food-requirement
  wood-requirement
  clay-requirement
  wood-for-clay
  food-stock
  clay-stock
  wood-stock
  total-food-effort
  total-wood-effort
  total-clay-effort
  site-name
  candidate-patches
  grain-per-grain-factor
  settlement-type
  start-period
  cumulative-food-stock ;; for setting out over time later on.
  cumulative-wood-stock
  cumulative-clay-stock
  saved-food-workdays
  saved-wood-workdays
  saved-clay-workdays
]

rangers-own [
  claiming
  walkingCost
]

links-own [
  weight
]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                SETUP & GO                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  ca
  import-map
  setup-topo
  setup-communities
  setup-least-cost-distances
  initial-periodization
  setup-resources
  setup-regeneration
  reset-ticks
end

to go
  exploit-resources
  ;viz-exploitation
  burn-resources
  regenerate
  disaster
  tick

  if ticks = 450 [;;;; add Achaemenid sites at 450
    add-sites-ACH
  ]
  if ticks = 650 [;;;; add Hellenistic sites at 650
    add-sites-HELL
  ]

  if ticks = time-limit [
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   PROCEDURES                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to import-map
  if coordsys? = false [
    gis:load-coordinate-system "/data/32636.prj"
    set coordsys? true
  ]
  set elevation-raster gis:load-dataset "/data/Altitude_EPSG32636_Clipped_Resampled2.asc"
  set fertility-raster gis:load-dataset "/data/fertilityFromRegressionWaterwaysExcluded_EPSG32636_Clipped_Resampled2.asc"
  set maxStandingStock-raster gis:load-dataset "/data/MaxStandingStock_GREFOS_EPSG32636_Clipped_Resampled2.asc"
  set rico-raster gis:load-dataset "/data/ricoMap_GREFOS_EPSG32636_Clipped_Resampled2.asc"
  set power-raster gis:load-dataset "/data/powerMap_GREFOS_EPSG32636_Clipped_Resampled2.asc"
  set walkingTime-raster gis:load-dataset "/data/Tobler_EPSG32636.asc"
  set waterBodies-raster gis:load-dataset "/data/lakesAndRiversRasterized_EPSG32636_Clipped.asc"
  set clay-raster gis:load-dataset "/data/Clay content_kg_per_ha_Clipped_EPSG32636.asc"

  gis:set-world-envelope (gis:envelope-of elevation-raster)

  resize-world 0 (gis:width-of elevation-raster - 1) 0 (gis:height-of elevation-raster - 1)
  set-patch-size 1;; patches should have an area of 1 ha (through this command, patch unit size is set  to 1 pixel = 100 x 100m)
end


to setup-topo
  ask patches [set elevation gis:raster-value elevation-raster pxcor (max-pycor - pycor) ]
  ask patches [set wood-maxStandingStock gis:raster-value maxStandingStock-raster pxcor (max-pycor - pycor)]
  ask patches [set walkingTime gis:raster-value walkingTime-raster pxcor (max-pycor - pycor)]
  ask patches [set wood-rico gis:raster-value rico-raster pxcor (max-pycor - pycor)]
  ask patches [set wood-power gis:raster-value power-raster pxcor (max-pycor - pycor)]
  ask patches [set clay-quantity gis:raster-value clay-raster pxcor (max-pycor - pycor)
    set clay-quantity clay-quantity / 1000 ; conversion to tonnes per ha in uppermost 2 m
  ]
  let e-min min [elevation] of patches
  let e-max max [elevation] of patches
  ask patches [
    let water gis:raster-sample waterBodies-raster self
    ifelse (water = 0) [; waterBodies raster is equal to zero when no water present.
      set pcolor palette:scale-gradient
      [[0 104 55][255 255 191][252 141 89]] elevation e-min e-max
      set land? true
    ]
    [
      set pcolor blue
      set land? false
      set wood? false
      set food? false
      set clay? false
      set clay-quantity 0
      set wood-maxStandingStock 0
    ]
  ]
  ask patches with [land? = true] [
    ifelse elevation < 1100 [
      set fire-return-rate 3 + random 26
    ]
    [
      set fire-return-rate round (elevation - 1100) * 171 / 300 + 3 + random 26
    ]
    set time-since-fire random fire-return-rate
  ]
  set burn-size []
end

to setup-communities
  set sites gis:load-dataset "/data/sagascape-sites-EPSG32636.shp"
  let valid false

  foreach gis:property-names sites [
    property-name ->
    if (property-name = "SITE")[ set valid true ]
  ]

  foreach gis:feature-list-of sites [
    site-coord ->
    let coordinates gis:location-of (first (first (gis:vertex-lists-of site-coord)))
    let long item 0 coordinates
    let lat item 1 coordinates
    if (valid) [
      let Site gis:property-value site-coord "Site"
      let S-t gis:property-value site-coord "Type"
      let S-p gis:property-value site-coord "Start"
      create-communities 1 [
        set shape "house"
        set settlement-type S-t
        set population (ifelse-value
          settlement-type = "hamlet" [round random-normal 50 10]
          settlement-type = "village" [round random-normal 500 100]
          settlement-type = "town" [round random-normal 1000 200]
        )
        set size sqrt (population / 5)
        setxy long lat
        set site-name Site
        set start-period S-p
        set food-stock 0
        set wood-stock 0
        set clay-stock 0
        set cumulative-food-stock 0
        set cumulative-wood-stock 0
        set cumulative-clay-stock 0
        set total-food-effort 0
        set total-wood-effort 0
        set total-clay-effort 0
        set saved-food-workdays 0
        set saved-wood-workdays 0
        set saved-clay-workdays 0
        set food-requirement population * 365 * food-demand-pc / 1000 ;; conversion to tonnes
        set wood-requirement population * (365 * wood-demand-pc  + 0.0661 * [elevation] of patch-here) / 695 ;; conversion to m??, considering average density of wood in Saga at MC of 30% is about 695 kg/m??. Also taking altitude of settlent into account (Boogers et al. in press).
        set clay-requirement population * clay-demand-pc / 1000;; more or less a random number for now, in tons
        set wood-for-clay 0
        set workdays population * active-percentage / 100 * 365
        set food-workdays population * active-percentage / 100 * agricultural-days
        set grain-per-grain-factor grain-per-grain-yield / (grain-per-grain-yield - 1) ; see Goodchild p. 252: grain-per-grain yield falls around 6:1.
      ]
    ]
  ]
end

to setup-least-cost-distances ;; Every community calculates the least-cost pathways to every patch in a certain search radius, taking the walkingTime raster into account.
  ask patches [
    set in-range-of []
    set claimed-cost []
   ]
  ask patches with [pxcor < 44 and pcolor = blue] [set walkingTime 10] ;; Quick fix for community D??ver Yarimada: the lake becomes inaccessible.
  ask communities [
    let claim self
    ask patches in-radius territory [ ;; Initial search radius of "territory" km: max distance a villager would walk to reach the fields on flat terrain (likely 5 km = 50 map units)
      sprout-rangers 1 [ ;; Every patch in the search radius sprouts a ranger whose only goal is to represent the patch they're on in a network
        set claiming claim
        set color white ;; Visualization of the process
        set size 1
        set walkingCost [walkingTime] of patch-here ;; Time necessary to cross patch (from the walkingTime raster) is transferred to ranger
        create-links-with rangers-on neighbors [ ;; Every ranger links up with his neighbors
          set weight 0.5 * sum [walkingCost] of both-ends ;; The weight of every link is the average of the walking time cost of both ends
        ]
       ]
      ]
    let comX pxcor ;; Needed to indicate the ranger situated on the community patch
    let comY pycor ;; Needed to indicate the ranger situated on the community patch
    let home-ranger one-of rangers with [xcor = comX and ycor = comY] ;; Indicating the ranger on the community patch
    ask rangers [ ;; Every ranger records on their patch what their least-cost-distance to the home ranger is
      let LCD nw:weighted-distance-to home-ranger weight
      let claimed claiming
      ask patch-here [
        if land? = true [
          set in-range-of sentence (in-range-of) claimed
          set claimed-cost sentence (claimed-cost) LCD
        ]
      ]
    ]
    ask rangers [die] ;; Rangers don't have any other function in the model, so they are removed
    set candidate-patches patches with [member? claim in-range-of and any? communities-here = false]
  ]

;  let csv csv:from-file "data/ranges_50.csv"
;  (foreach csv
;    [ [entry] -> ask patch item 0 entry item 1 entry [
;      set in-range-of item 2 entry
;      set claimed-cost item 3 entry
;      ]
;  ]) ;;almost correct, only need to ignore quotation marks.
end

to initial-periodization
  ask communities with [start-period != "IA" ][
    set breed inactive-communities
  ]
  ask inactive-communities [
    set size 0
  ]
end

to setup-resources ;; already included in GIS step that wood or food cannot grow on water.
  ask patches [
    ifelse not any? communities-here [    ;; settled patches are not forested and not suited for agriculture
      set wood? true
      set food? true
      set wood-age 200 + random 200 ;; all non-settled patches are more or less mature forest at the start
      let fertility gis:raster-value fertility-raster pxcor (max-pycor - pycor);; PhD Maarten Van Loo p. 50: highest modelled fertility in past = 2.8 tonnes per ha. Modern data is overestimation, therefore rescaled.
      set food-fertility adapted-fertility (fertility)
      set time-since-abandonment 0
      ifelse clay-quantity > clay-threshold * 10000 * 2  [set clay? true][set clay? false] ; clay-threshold is expressed in tons per m?? of soil while clay-quantity is expressed in tons per ha in the two uppermost m?? of soil.
    ]
    [
      set wood-maxStandingStock 0 ;; TBI: if community ever dies, reset wood-maxStandingStock
      set food-fertility 0
      set clay? false
    ]
    wood-updateStandingStock
    set wood-standingStock min (list wood-standingStock wood-maxStandingStock)

  set original-food-value food-fertility
  ]

  ;; Spinup procedure to account for forest loss and agricultural fields in previous period.
  ;; See MSc thesis of Joachim L??pez p. 32: during Early Iron Age, approx. 25% of land patches covered with agriculture (that's a lot, damn) and 40% with forest. Other 35% steppe etc.

  let total-land count patches with [land? = true]
  let total-population sum [population] of communities ; using population percentage as weighing method
  ask communities [
    let homebase self
    let initial-open-patches 0.60 * total-land * population / total-population ;; total patches not under forest cover, weighted by population
    ask min-n-of initial-open-patches patches with [wood-standingStock > 0] [distance homebase][
      set wood-standingStock 0
      set wood-age 0
    ]
  ]
  set bad-harvest-modifier 1
end

to setup-regeneration ;; Procedure required to properly initialize fertility decline and restoration.
  set regeneration-reserve 0.1 ;; required non-zero initialisation for Verhulst growth
  ask patches with [original-food-value > 0] [
    ifelse original-food-value > regeneration-reserve [
      ;set growth-rate (1 / regeneration-time) * ln (99 * original-food-value / regeneration-reserve - 99)
      set growth-rate (99 * original-food-value / regeneration-reserve - 99) ^ (- 1 / regeneration-time)
    ]
    [
      set growth-rate 0 ; incrementally small growth is set to zero instead
      set original-food-value 0
    ]
  ]
end

to add-sites-ACH
  ; add sites with Start = ACH
  ask inactive-communities with [start-period = "ACH" ] [
    set breed communities
    set shape "house"
    set size sqrt (population / 5)
    ask patch-here [
      set wood-maxStandingStock 0 ;; TBI: if community ever dies, reset wood-maxStandingStock
      set food-fertility 0
      set wood? false
      set food? false
      set clay? false
    ]
  ]
end

to add-sites-HELL
  ; add sites with Start = HELL
  ask inactive-communities with [start-period = "HELL" ] [
    set breed communities
    set shape "house"
    set size sqrt (population / 5)
    ask patch-here [
      set wood-maxStandingStock 0 ;; TBI: if community ever dies, reset wood-maxStandingStock
      set food-fertility 0
      set wood? false
      set food? false
      set clay? false
    ]
  ]
end


to exploit-resources
  ask communities [
    let homebase self
    let food-exploited 0
    let food-effort 0
    let wood-from-the-field 0
    let sorted-patches sort-on [(- food-fertility) / (item position homebase in-range-of claimed-cost)] candidate-patches ; communities strive for the best food / walking cost ratio
    let index 0
    let security-factor 1 + random-float 1 ;; Goodchild p. 277: farming communities keep a supply of 1-2 years of produce to compensate for bad harvests.
    while [food-stock < food-requirement * security-factor * grain-per-grain-factor and any? candidate-patches with [food-fertility > 0] and food-workdays > 0] [ ; 1. still resource required, 2. still patches with resource, 3. still workdays left
      let target item index sorted-patches
      ask target [
        set food-effort item position homebase in-range-of claimed-cost
        set food-exploited food-fertility
        set food-fertility 0    ;; basic assumption of exploiting all available food
        set wood-from-the-field wood-standingStock
        set wood-standingStock 0
        set wood-age 0
        set wood? false ;; assumption that when exploited for food, fields don't regenerate wood.
        set food? true
        set time-since-abandonment 0
      ]
      set index index + 1
      set food-stock food-stock + food-exploited
      set cumulative-food-stock cumulative-food-stock + food-exploited
      set total-food-effort total-food-effort + food-effort
      set food-workdays food-workdays - 42 - 42 * 2 * food-effort / 10 ; see Goodchild 2007 p. 301: 42 mandays per ha per annum. Add to this the amount of workdays spent on migrating back and forth. (no. of trips * time back and forth per trip / hours of work per day (assumed 10))
      set workdays workdays - 42 - 42 * 2 * food-effort / 10
      set saved-food-workdays saved-food-workdays + 42 + 42 * 2 * food-effort / 10
      if wood-from-the-field > 0 [
        set wood-stock wood-stock + wood-from-the-field
        set cumulative-wood-stock cumulative-wood-stock + wood-from-the-field
        set total-wood-effort total-wood-effort + food-effort ; wood from the field needs to be collected as well.
        let head-load (max list (random-normal 29.21 14.14) 4.5) / 695;; see Amutabi Kefa et al. 2018 p. 4. Converted to m?? from kg at MC 30%.
        let head-load-gathering-time 49 ; 49 hrs per m?? of wood gathering. See MSc thesis Katie Preston p. 30.
        let workdays-until-deforested (wood-from-the-field / head-load - 2) * (2 * food-effort + head-load * head-load-gathering-time ) / 10 ;; no. of trips * (time back and forth per trip (minus the trips already spent on going to perform agriculture))+ time spent gathering 1 HL) / hours of work per day (assumed 10)
        set workdays workdays - workdays-until-deforested
        set saved-wood-workdays saved-wood-workdays + workdays-until-deforested
      ]
    ]
    set food-stock food-stock * bad-harvest-modifier
  ]

  ask communities [
    let homebase self
    let wood-exploited 0
    let wood-effort 0
    let head-load (max list (random-normal 29.21 14.14) 4.5) / 695;; see Amutabi Kefa et al. 2018 p. 4. Converted to m?? from kg at MC 30%.
    let head-load-gathering-time 49 ; 49 hrs per m?? of wood gathering. See MSc thesis Katie Preston p. 30.
    let sorted-patches sort-on [(- wood-standingStock) / (item position homebase in-range-of claimed-cost)] candidate-patches
    let index 0
    while [wood-stock < wood-requirement and any? candidate-patches with [wood-standingStock > 0] and workdays > 0] [
      let target item index sorted-patches
      ask target [
        set wood-effort item position homebase in-range-of claimed-cost
        set wood-exploited wood-standingStock
        set wood-standingStock 0
        set wood-age 0
      ]
      set index index + 1
      set wood-stock wood-stock + wood-exploited
      set cumulative-wood-stock cumulative-wood-stock + wood-exploited
      set total-wood-effort total-wood-effort + wood-effort
      let workdays-until-deforested wood-exploited / head-load * (2 * wood-effort + head-load * head-load-gathering-time ) / 10 ;; no. of trips * (time back and forth per trip + time spent gathering 1 HL) / hours of work per day (assumed 10)
      set workdays workdays - workdays-until-deforested
      set saved-wood-workdays saved-wood-workdays + workdays-until-deforested
    ]
 ]

  ask communities [
    let homebase self
    let clay-exploited 0
    let clay-effort 0
    let sorted-patches sort-on [(- clay-quantity) / (item position homebase in-range-of claimed-cost)] candidate-patches with [clay? = true]
    let index 0
    set wood-for-clay 0
    while [clay-stock < clay-requirement and any? candidate-patches with [clay? = true] and workdays > 0][
      let wood-from-the-field 0
      let target item index sorted-patches
      ask target [
        set clay-effort item position homebase in-range-of claimed-cost
        set clay-exploited 19 ; 1 m?? weighs approximately 1.9 tonnes (see Delaine 1992 p. 182), excavation procedure takes place in steps of 10 m??
        set clay-quantity (clay-quantity - clay-exploited)
        if clay-quantity < clay-threshold * 10000 * 2 [
          set clay? false
        ]
        set index index + 1
        set wood-from-the-field wood-standingStock ; patch is cleared for clay exploitation: wood goes to community as well
        set food-fertility 0  ;; once a patch is exploited for clay, it cannot provide food or wood anymore
        set wood-standingStock 0  ;; once a patch is exploited for clay, it cannot provide food or wood anymore
        set wood-age 0
        set wood? false
        set food? false
      ]
      set clay-stock clay-stock + clay-exploited
      set cumulative-clay-stock cumulative-clay-stock + clay-exploited
      set total-clay-effort total-clay-effort + clay-effort
      set wood-for-clay wood-for-clay + clay-exploited * kgs-wood-per-kg-clay * 1000 / 695 ;; Janssen et al. 2017: 2 - 5 MJ per kg clay required. Further used energy content of dried, yet still moist wood (11.4 - 13.86 MJ/kg).
      let workdays-until-quarried 0.193 * clay-exploited / 1.9 ;; Delaine 1992 p. 182: 0.13 days/m?? to dig clay and 0.063 days/m?? to fill baskets.
      let baskets clay-exploited / 0.05 ;; number of 50 kg baskets needed to haul
      let workdays-hauling baskets * clay-effort * 2 * 6.5 / 10 ;; Going back and forth between quarry and community, taking into account slowing factor of 6.5 (calculated from Delaine 1992) bc of load and 10 hr-working day.
      let workdays-until-fired 4.5 / 0.980 * clay-exploited;; Janssen: 4-5 days per firing cycle of 360 - 1600 kgs of clay vessels. Only actual firing taken into account, no throwing etc.
      set workdays workdays - workdays-until-quarried - workdays-hauling - workdays-until-fired
      set saved-clay-workdays saved-clay-workdays + workdays-until-quarried + workdays-hauling + workdays-until-fired
      if wood-from-the-field > 0 [
        set wood-stock wood-stock + wood-from-the-field
        set cumulative-wood-stock cumulative-wood-stock  + wood-from-the-field
        set total-wood-effort total-wood-effort + clay-effort ; wood from the field needs to be collected as well.
        let head-load (max list (random-normal 29.21 14.14) 4.5) / 695;; see Amutabi Kefa et al. 2018 p. 4. Converted to m?? from kg at MC 30%.
        let head-load-gathering-time 49 ; 49 hrs per m?? of wood gathering. See MSc thesis Katie Preston p. 30.
        let workdays-until-deforested (wood-from-the-field / head-load - 2) * (2 * clay-effort + head-load * head-load-gathering-time ) / 10 ;; no. of trips * (time back and forth per trip (minus the trips already spent on going to perform clay excavation))+ time spent gathering 1 HL) / hours of work per day (assumed 10)
        set workdays workdays - workdays-until-deforested
        set saved-wood-workdays saved-wood-workdays + workdays-until-deforested
      ]
    ]
  ]
end

to burn-resources ;; every tick communities use (part of) available food, clay and wood to sustain themselves
  ask communities [;; possible to go below 0, so it can be corrected the following  year
    set food-stock food-stock / grain-per-grain-factor - food-requirement ;; portion  of wheat is removed as seed for next year.
    set wood-stock wood-stock - wood-requirement - wood-for-clay ;; clay is only exploited after wood, so additional wood is cut the next year.
    set clay-stock clay-stock - clay-requirement
  ]

end

to regenerate
  ask patches [
    if food? = true and wood? = false [ ;; only patches that can still grow food (e.g. not clay quarries) regrow food
      set time-since-abandonment time-since-abandonment + 1
      if food-fertility < original-food-value [
        ifelse food-fertility > 0 [
          let food-fertility-regeneration (1 - food-fertility / original-food-value) / (1 / original-food-value + growth-rate / (food-fertility * (1 - growth-rate)))
          set food-fertility food-fertility + food-fertility-regeneration
        ]
        [
          set food-fertility regeneration-reserve
        ]
      ]
    ]
  ]
  ask patches [
    wood-updateStandingStock
  ]
  ask communities [
    set workdays population * active-percentage / 100 * 365 + ifelse-value (workdays < 0)[workdays][0]
    set food-workdays population * active-percentage / 100 * agricultural-days + ifelse-value (agricultural-days < 0)[agricultural-days][0] ; no reserves from previous year are taken along, but debts are.
  ]
  set bad-harvest-modifier 1
end

to viz-exploitation
ifelse landuse-visualization [
    let w-max max [wood-standingStock] of patches
    let w-min min [wood-standingStock] of patches with [wood? = true]
    let f-max max [food-fertility] of patches
    let f-min min [food-fertility] of patches
    let c-max max [clay-quantity] of patches
    let c-min min [clay-quantity] of patches with [clay? = true]

    ask patches with [land? = true and wood? = true] [ ; forested areas
      set pcolor palette:scale-gradient [[0 109 44][186 228 179]] wood-standingStock w-max w-min
    ]

    ask patches with [land? = true and wood? = false and food? = true] [ ; agricultural areas
      set pcolor palette:scale-gradient [[153 52 4][254 217 142]] food-fertility f-max f-min
    ]

    ask patches with [clay? = true and wood? = false and food? = false] [ ; areas used for clay excavation
      set pcolor palette:scale-gradient [[0 0 0][255 255 255]] clay-quantity c-max c-min
    ]
  ]
  [
    let e-max max [elevation] of patches
    let e-min min [elevation] of patches
    ask patches [
      ifelse land? = true [
        set pcolor palette:scale-gradient [[255 0 0][255 255 191][0 104 55]] elevation e-max e-min
      ]
      [
        set pcolor blue
      ]
    ]
  ]
end

to disaster
  ;; Forest fire: ignition
  let fire-initial-patches patch-set []
  let drought random-float 1
  ask patches with [wood-standingStock > 0.72] [ ;; minimum value necessary for fire ignition: 500 kg/ha (see Seidl et al. 2014) / 695 kg/m??
    let p-base 1 / (fire-return-rate * 4) ;; mean fire size in Turkey according to effis data 1980-2016.
    let odds-ignition p-base / (1 - p-base) * drought ; some years are drier than others, resulting in higher fire incidence.
    if odds-ignition / (1 + odds-ignition) > random-float 1 [
      set wood-standingStock 0
      set wood-age 0
      set time-since-fire 0
      set pcolor orange
      set fire-initial-patches (patch-set self fire-initial-patches)
    ]
  ]

  ;; Forest fire: spread
  ask fire-initial-patches [
    let max-fire-size -4 * log (1 - random-float 1) 10 ;; again using mean fire size Turkey (about 4 ha)
    if max-fire-size > 1 [ ;; save time by only evaluating spread when fire is larger than 1 pixel.
      let patches-burned (patch-set self)
      while [count patches-burned <= max-fire-size and any? peripheral-wooded-patches (patches-burned) and random-float 1 > 0.1][
        let newly-burnt-patch one-of peripheral-wooded-patches (patches-burned)
        ask newly-burnt-patch [
          set wood-standingStock 0
          set wood-age 0
          set time-since-fire 0
          set pcolor orange
        ]
        set patches-burned (patch-set patches-burned newly-burnt-patch )
      ]
      set burn-size sentence burn-size count patches-burned ;; used to check on fire sizes
    ]
  ]
  ;; Bad harvest (same effect as postharvest loss; Goodchild: 30%)
  if random-poisson (1 / (1 + bad-harvest-interval)) > 0 [
      set bad-harvest-modifier 0.5 ; assumed that a bad harvest is equivalent to a loss of half the stock of crops belonging to a community
  ]

end


to wood-updateStandingStock
  if time-since-abandonment > forest-regrowth-lag [set wood? true]
  if wood? = true [  ;; only patches that can still grow wood (e.g. not clay quarries) regrow wood
    ifelse wood-standingStock < wood-maxStandingStock [
     set wood-standingStock wood-rico * exp(wood-power * wood-age)
    ]
    [
      set wood-standingStock wood-maxStandingStock
    ]
    set wood-age wood-age + 1
    set time-since-fire time-since-fire + 1
  ]
end

to-report peripheral-wooded-patches [central-patches] ;; procedure to report a patchset of wooded neighbor4 patches surrounding the central lump of patches. Have to be continuous.
  let periphery patch-set []
  ask central-patches [
    set periphery patch-set [neighbors4] of central-patches
    set periphery periphery with [wood-standingStock > 0.72]
  ]
  report periphery
end

to-report adapted-fertility [fertility]
  let result 0
  set result (ifelse-value
    fertility = 0 [0]
    fertility > 3.5 [3.5] ;; phd Maarten van Loo p. 94: model overestimates. 3.5 taken as cutoff.
    [fertility * 2.8 / 3.5] ;; phd Maarten van Loo p. 50: 2.8 highest yield modelled in sedimentation model.
    )
  report result
end

;to-report save-ranges-50
;  csv:to-file "data/ranges_50.csv" [(list pxcor pycor in-range-of claimed-cost)] of patches
;end

;to-report save-ranges-200
;  csv:to-file "data/ranges_100.csv" [(list pxcor pycor in-range-of claimed-cost)] of patches
;end
@#$#@#$#@
GRAPHICS-WINDOW
173
10
980
415
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
798
0
395
0
0
1
ticks
30.0

BUTTON
0
10
83
43
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
82
10
172
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
0
346
172
379
territory
territory
0
200
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
458
172
491
time-limit
time-limit
0
3000
1000.0
1
1
NIL
HORIZONTAL

SLIDER
0
244
171
277
regeneration-time
regeneration-time
1
3
2.0
1
1
NIL
HORIZONTAL

SWITCH
810
419
980
452
landuse-visualization
landuse-visualization
0
1
-1000

SLIDER
0
312
172
345
clay-threshold
clay-threshold
0.3
0.5
0.25
0.05
1
tonnes per m??
HORIZONTAL

SLIDER
0
49
172
82
food-demand-pc
food-demand-pc
0.75
1.5
0.75
0.1
1
kg/day
HORIZONTAL

SLIDER
0
83
172
116
wood-demand-pc
wood-demand-pc
1
5
1.0
0.05
1
kg/day
HORIZONTAL

SLIDER
0
114
172
147
clay-demand-pc
clay-demand-pc
0
10
1.0
1
1
kg/year
HORIZONTAL

SLIDER
0
147
172
180
active-percentage
active-percentage
0
100
25.0
1
1
%
HORIZONTAL

SLIDER
0
180
171
213
agricultural-days
agricultural-days
120
265
250.0
5
1
days/yr
HORIZONTAL

SLIDER
0
278
172
311
kgs-wood-per-kg-clay
kgs-wood-per-kg-clay
0.14
0.44
0.29
0.05
1
NIL
HORIZONTAL

SLIDER
0
213
171
246
grain-per-grain-yield
grain-per-grain-yield
2
6
6.0
0.5
1
kg/kg
HORIZONTAL

SLIDER
0
378
173
411
bad-harvest-interval
bad-harvest-interval
1
10
5.0
1
1
year
HORIZONTAL

SLIDER
0
411
174
444
forest-regrowth-lag
forest-regrowth-lag
3
10
6.0
1
1
years
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="regeneration-time" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[energy-stock / (population * ticks / 2)] of community 0</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 1</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 2</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 3</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 4</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 5</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 6</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 7</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 8</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 9</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 10</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 11</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 12</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 13</metric>
    <metric>[energy-stock / (population * ticks / 2)] of community 14</metric>
    <enumeratedValueSet variable="regeneration-time">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EAA" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[energy-stock] of community 0</metric>
    <metric>[energy-stock] of community 1</metric>
    <metric>[energy-stock] of community 2</metric>
    <metric>[energy-stock] of community 3</metric>
    <metric>[energy-stock] of community 4</metric>
    <metric>[energy-stock] of community 5</metric>
    <metric>[energy-stock] of community 6</metric>
    <metric>[energy-stock] of community 7</metric>
    <metric>[energy-stock] of community 8</metric>
    <metric>[energy-stock] of community 9</metric>
    <metric>[energy-stock] of community 10</metric>
    <metric>[energy-stock] of community 11</metric>
    <metric>[energy-stock] of community 12</metric>
    <metric>[energy-stock] of community 13</metric>
    <metric>[energy-stock] of community 14</metric>
    <metric>[wood-stock] of community 0</metric>
    <metric>[wood-stock] of community 1</metric>
    <metric>[wood-stock] of community 2</metric>
    <metric>[wood-stock] of community 3</metric>
    <metric>[wood-stock] of community 4</metric>
    <metric>[wood-stock] of community 5</metric>
    <metric>[wood-stock] of community 6</metric>
    <metric>[wood-stock] of community 7</metric>
    <metric>[wood-stock] of community 8</metric>
    <metric>[wood-stock] of community 9</metric>
    <metric>[wood-stock] of community 10</metric>
    <metric>[wood-stock] of community 11</metric>
    <metric>[wood-stock] of community 12</metric>
    <metric>[wood-stock] of community 13</metric>
    <metric>[wood-stock] of community 14</metric>
    <metric>[clay-stock] of community 0</metric>
    <metric>[clay-stock] of community 1</metric>
    <metric>[clay-stock] of community 2</metric>
    <metric>[clay-stock] of community 3</metric>
    <metric>[clay-stock] of community 4</metric>
    <metric>[clay-stock] of community 5</metric>
    <metric>[clay-stock] of community 6</metric>
    <metric>[clay-stock] of community 7</metric>
    <metric>[clay-stock] of community 8</metric>
    <metric>[clay-stock] of community 9</metric>
    <metric>[clay-stock] of community 10</metric>
    <metric>[clay-stock] of community 11</metric>
    <metric>[clay-stock] of community 12</metric>
    <metric>[clay-stock] of community 13</metric>
    <metric>[clay-stock] of community 14</metric>
    <metric>[total-food-effort] of community 0</metric>
    <metric>[total-food-effort] of community 1</metric>
    <metric>[total-food-effort] of community 2</metric>
    <metric>[total-food-effort] of community 3</metric>
    <metric>[total-food-effort] of community 4</metric>
    <metric>[total-food-effort] of community 5</metric>
    <metric>[total-food-effort] of community 6</metric>
    <metric>[total-food-effort] of community 7</metric>
    <metric>[total-food-effort] of community 8</metric>
    <metric>[total-food-effort] of community 9</metric>
    <metric>[total-food-effort] of community 10</metric>
    <metric>[total-food-effort] of community 11</metric>
    <metric>[total-food-effort] of community 12</metric>
    <metric>[total-food-effort] of community 13</metric>
    <metric>[total-food-effort] of community 14</metric>
    <metric>[total-wood-effort] of community 0</metric>
    <metric>[total-wood-effort] of community 1</metric>
    <metric>[total-wood-effort] of community 2</metric>
    <metric>[total-wood-effort] of community 3</metric>
    <metric>[total-wood-effort] of community 4</metric>
    <metric>[total-wood-effort] of community 5</metric>
    <metric>[total-wood-effort] of community 6</metric>
    <metric>[total-wood-effort] of community 7</metric>
    <metric>[total-wood-effort] of community 8</metric>
    <metric>[total-wood-effort] of community 9</metric>
    <metric>[total-wood-effort] of community 10</metric>
    <metric>[total-wood-effort] of community 11</metric>
    <metric>[total-wood-effort] of community 12</metric>
    <metric>[total-wood-effort] of community 13</metric>
    <metric>[total-wood-effort] of community 14</metric>
    <metric>[total-clay-effort] of community 0</metric>
    <metric>[total-clay-effort] of community 1</metric>
    <metric>[total-clay-effort] of community 2</metric>
    <metric>[total-clay-effort] of community 3</metric>
    <metric>[total-clay-effort] of community 4</metric>
    <metric>[total-clay-effort] of community 5</metric>
    <metric>[total-clay-effort] of community 6</metric>
    <metric>[total-clay-effort] of community 7</metric>
    <metric>[total-clay-effort] of community 8</metric>
    <metric>[total-clay-effort] of community 9</metric>
    <metric>[total-clay-effort] of community 10</metric>
    <metric>[total-clay-effort] of community 11</metric>
    <metric>[total-clay-effort] of community 12</metric>
    <metric>[total-clay-effort] of community 13</metric>
    <metric>[total-clay-effort] of community 14</metric>
    <enumeratedValueSet variable="regeneration-time">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clay-threshold">
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="CAA runs" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>[(list who population precision saved-food-workdays 2)] of communities</metric>
    <metric>[(list who precision saved-wood-workdays 2)] of communities</metric>
    <metric>[(list who precision saved-clay-workdays 2)] of communities</metric>
    <metric>[(list who precision cumulative-food-stock 2)] of communities</metric>
    <metric>[(list who precision cumulative-wood-stock 2)] of communities</metric>
    <metric>[(list who precision cumulative-clay-stock 2)] of communities</metric>
    <metric>[(list who precision total-food-effort 2)] of communities</metric>
    <metric>[(list who precision total-wood-effort 2)] of communities</metric>
    <metric>[(list who precision total-clay-effort 2)] of communities</metric>
    <metric>count patches with [land? = true and wood-age &gt; 0]</metric>
    <metric>count patches with [land? = true and food? = true and wood? = false]</metric>
    <enumeratedValueSet variable="agricultural-days">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regeneration-time">
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landuse-visualization">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="territory">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-regrowth-lag">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clay-demand-pc">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kgs-wood-per-kg-clay">
      <value value="0.29"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grain-per-grain-yield">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clay-threshold">
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="food-demand-pc">
      <value value="0.75"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="active-percentage">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wood-demand-pc">
      <value value="1"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bad-harvest-interval">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-limit">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
