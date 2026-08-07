# Running Coralcraft

This folder contains the runnable model with instructions for setting up a run.

---

## Requirements

* R (developed/tested on v4.0.0–4.4.3)
* Packages: `rgl`, `scatterplot3d`, `Matrix`, `png`

```r
install.packages(c("rgl", "scatterplot3d", "Matrix", "png"))
```

## Quick Start

1. Open R/RStudio and set your working directory to the **repository root**
   (the `Coralcraft` folder, not `0_model`) — all paths in the scripts are
   written relative to it, e.g. `setwd("path/to/Coralcraft")`.
2. Open `0_model/1_Coralcraft_simulation_code.R`.
3. (Optional) adjust any parameters in section `1. SET PARAMETERS` near the
   top — see [Key Parameters](#key-parameters) below. The defaults will run
   out of the box.
4. Run the whole script (source it, or Ctrl All - Run).

Running the script will, in order:
* build the 10 coral growth forms and save a combined preview image
  (`0_model/growth_form_shapes.png`/`.pdf`) so you can see what each
  functional type looks like at its current size and shape settings
* create a timestamped output folder under `0_model/1_simulation_output/`
* run the simulation for `runs` runs of `timesteps` weekly steps each,
  printing progress to the console as it goes
* save a `.csv` of community metrics per timestep, plus periodic snapshots
  of the 3D world, into that output folder

A first run with the defaults (`timesteps = 52*5`, `wx/wy = 300/200`) can
take a while, mostly from the 3D `rgl` plotting that happens every
timestep by default. To do a quick first test, temporarily set
`draw = 0` and `timesteps` to something small (e.g. `10`) in section 5a.

---

## File Guide

| File | Purpose |
|---|---|
| `1_coral_morphology_architecture.R` | Defines the 3D shape of each of the 10 coral functional types (encrusting, flexihemispherical, digitate, corymbose, tabular, mushroom, columnar, foliose, bushy, branching). Sourced automatically by the main script — you don't run it directly. Also generates the `growth_form_shapes.png`/`.pdf` preview. This can be further developed for new growth forms |
| `1_Coralcraft_simulation_code.R` | The main script — set your parameters here and run this to simulate. |
| `scenarios.csv` / `scenarios_id.csv` | Define named "community" scenarios (e.g. `max.div` = all 10 types, `encrusting` = just type 1) as combinations of functional-type IDs, for advanced/multi-scenario setups. Only used if you set `selected.fts = NULL` (see below) — the default run doesn't touch these. |
| `growth_forms.csv` | Not used by the simulation scripts (functional type names/order now live in `ftnames` inside `1_coral_morphology_architecture.R`) — but still read by the `2_output_analysis` scripts, so don't delete it. |
| `ftcelllist` | Auto-generated/overwritten each run — a cached R object of the growth-form shape arrays. Not meant to be edited by hand. |
| `growth_form_shapes.png` / `.pdf` | Auto-generated preview of all 10 coral shapes at their current `maxradius` settings. Regenerates every run. |
| `1_simulation_output/` | Where results land — see [Output](#output) below. |
| `2_output_analysis/` | Scripts for calculating metrics and plotting, run *after* a simulation — see its own `README.md`. |

---

## Key Parameters

All of these are set in section `1. SET PARAMETERS` at the top of
`1_Coralcraft_simulation_code.R`.

| Parameter | What it controls |
|---|---|
| `runs` | How many independent simulation repeats to run |
| `timesteps` | Length of each run, in weeks (52 = 1 year) |
| `wx`, `wy`, `wz` | World size (cm) in x, y, and z (height/depth); doesn't need to be a cube |
| `n.initial.colonies` | How many colonies to seed the world with at the start |
| `randominitial` | `0` = initial colonies cycle through the included functional types in order; `1` = random |
| `randomrecruits`, `spawn.freq`, `nnewrecruits` | Control spawning: whether new recruits get a random or cover-weighted functional type, how often spawning happens (in timesteps), and how many recruits per spawning event |
| `randomdist`, `freq.low`, `freq.high` | Disturbance regime: `"fixed"` (regular interval) or `"random"` (probabilistic), and how often low/high intensity disturbances occur |
| `background.mort`, `background.mort2` | Per-timestep chance of random colony mortality (the second is a lower rate specific to encrusting corals) |
| `maxradius` | Maximum size (radius in cm from the recruit point) each functional type can grow to, in the same order as `ftnames` in the architecture script (`Inf` = no limit) |
| `selected.fts` | The base case for which functional types to include (see below) — a vector of names, e.g. `c("encrusting","tabular")`. Defaults to all 10 types. |

### Choosing which coral types to include

There are two ways to control which of the 10 functional types appear in a
run:

* **`selected.fts` (base case, default):** a vector of functional type
  names, e.g. `selected.fts = c("encrusting", "tabular")`. This is what
  runs out of the box (defaulting to all 10 types) and doesn't touch the
  scenario CSVs at all — the simplest option for a single run or quick
  test.
* **Scenario CSV (advanced):** set `selected.fts = NULL` to instead pick a
  named scenario in section 3 (`sc.label <- ... scenario %in% c("max.div")`
  — change the scenario name there). Scenarios and their functional-type
  combinations are defined in `scenarios_id.csv` and `scenarios.csv` — use
  this once you have several reusable multi-type community compositions to
  define and switch between.

---

## Output

Each run creates a folder under `0_model/1_simulation_output/` named from
the scenario, timesteps, and run count. Inside you'll find:

* a `.csv` of community metrics (cover, colony count, rugosity, etc.) for
  every functional type at every timestep
* saved `world`/`dead`/`light` snapshots (`.RData`-style, via `save()`) at
  the first timestep and then every 13 timesteps, for later 3D
  reconstruction/analysis

If `draw = 1` (the default), a PNG snapshot of the 3D world is also saved
into the output folder every timestep.

## Analysing Results

Once you have output, see `0_model/2_output_analysis/` (and its own
`README.md`) for scripts that calculate metrics and produce plots from
saved simulation output.
