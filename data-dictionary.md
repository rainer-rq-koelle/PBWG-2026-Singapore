# OPDI Data Dictionary and Metadata
## Global Flight Time Variability and Efficiency Study - PBWG 2026 Singapore

**Date:** 2026-10-27  
**Data Source:** Open Data Performance Initiative (OPDI) - opdi.aero  
**Analysis Period:** June 2025, June 2026, September 2026 (partial)

---

## 1. Data Coverage Summary

### Temporal Coverage
- **Primary Period 1:** 2025-06-01 to 2025-06-13 (13 days)
- **Primary Period 2:** 2026-06-01 to 2026-06-13 (13 days)  
- **Additional:** 2026-09-15 (1 day)
- **Overall Date Range:** 2025-06-01 02:03:35 to 2026-09-16 07:49:37

### Spatial Coverage
- **Unique Departure Airports (ADEP):** 2,903
- **Unique Arrival Airports (ADES):** 2,629
- **Geographic Scope:** Global

### Volume
- **Flight List Records:** 5,404,654 flights
- **Flight Events Records:** 144,044,076 milestone events
- **Average Events per Flight:** ~26.7

---

## 2. Dataset: Flight List (`opdi_flight_list`)

### 2.1 File Structure
- **Format:** Parquet (snappy compressed)
- **Partitioning:** By `DOF` (Date of Flight) in format `YYYY-MM-DD`
- **Location:** `data/flight_list/opdi_flight_list/DOF=YYYY-MM-DD/*.parquet`

### 2.2 Schema and Field Descriptions

#### Flight Identification
| Field | Type | Description |
|-------|------|-------------|
| `ID` | string | Unique flight identifier (OPDI internal) |
| `ICAO24` | string | ICAO 24-bit aircraft address (Mode-S transponder code) |
| `FLT_ID` | string | Flight identification / callsign |
| `REGISTRATION` | string | Aircraft registration number |

#### Airports and Route
| Field | Type | Description |
|-------|------|-------------|
| `ADEP` | string | ICAO code - Aerodrome of Departure (actual) |
| `ADES` | string | ICAO code - Aerodrome of Destination (actual) |
| `ADEP_P` | string | ICAO code - Aerodrome of Departure (planned) |
| `ADES_P` | string | ICAO code - Aerodrome of Destination (planned) |

#### Aircraft Characteristics
| Field | Type | Description |
|-------|------|-------------|
| `MODEL` | string | Aircraft model name |
| `TYPECODE` | string | ICAO aircraft type designator |
| `ICAO_AIRCRAFT_CLASS` | string | ICAO aircraft class (wake turbulence category) |
| `ICAO_OPERATOR` | string | ICAO operator designator |

#### Runway Information
| Field | Type | Description |
|-------|------|-------------|
| `RWY_DEP` | string | Departure runway identifier |
| `RWY_DEP_BEARING_DEG` | double | Departure runway bearing in degrees |
| `RWY_ARR` | string | Arrival runway identifier |
| `RWY_ARR_BEARING_DEG` | double | Arrival runway bearing in degrees |

#### Stand/Gate Information
| Field | Type | Description |
|-------|------|-------------|
| `STND_DEP` | string | Departure stand/gate identifier |
| `STND_ARR` | string | Arrival stand/gate identifier |

#### Actual Times (Primary Milestones)
| Field | Type | Description |
|-------|------|-------------|
| `AOBT` | timestamp[ns] | Actual Off-Block Time |
| `ATOT` | timestamp[ns] | Actual Take-Off Time |
| `ALDT` | timestamp[ns] | Actual Landing Time |
| `AIBT` | timestamp[ns] | Actual In-Block Time |

#### Trajectory Metadata
| Field | Type | Description |
|-------|------|-------------|
| `FIRST_SEEN` | timestamp[ns] | First position observation timestamp |
| `LAST_SEEN` | timestamp[ns] | Last position observation timestamp |
| `TRACK_DURATION_MIN` | double | Total track duration in minutes |

#### Arrival Horizon Crossings
| Field | Type | Description |
|-------|------|-------------|
| `C40_ARR` | timestamp[ns] | 40nm horizon crossing time (arrival) |
| `C50_ARR` | timestamp[ns] | 50nm horizon crossing time (arrival) |
| `C60_ARR` | timestamp[ns] | 60nm horizon crossing time (arrival) |
| `C100_ARR` | timestamp[ns] | 100nm horizon crossing time (arrival) |
| `C110_ARR` | timestamp[ns] | 110nm horizon crossing time (arrival) |
| `C120_ARR` | timestamp[ns] | 120nm horizon crossing time (arrival) |

#### Departure Horizon Crossings
| Field | Type | Description |
|-------|------|-------------|
| `C40_DEP` | timestamp[ns] | 40nm horizon crossing time (departure) |
| `C50_DEP` | timestamp[ns] | 50nm horizon crossing time (departure) |
| `C60_DEP` | timestamp[ns] | 60nm horizon crossing time (departure) |
| `C100_DEP` | timestamp[ns] | 100nm horizon crossing time (departure) |
| `C110_DEP` | timestamp[ns] | 110nm horizon crossing time (departure) |
| `C120_DEP` | timestamp[ns] | 120nm horizon crossing time (departure) |

#### Data Quality Flags
| Field | Type | Description |
|-------|------|-------------|
| `SUPERSEDED_DEP` | bool | Departure record superseded by later version |
| `SUPERSEDED_ARR` | bool | Arrival record superseded by later version |
| `MOVEMENT_DEP` | bool | Departure movement detected |
| `MOVEMENT_ARR` | bool | Arrival movement detected |

#### Metadata
| Field | Type | Description |
|-------|------|-------------|
| `VERSION` | string | Data processing version |
| `DOF` | string | Date of Flight (partition key, format: YYYY-MM-DD) |

---

## 3. Dataset: Flight Events (`opdi_flight_events`)

### 3.1 File Structure
- **Format:** Parquet (snappy compressed)
- **Partitioning:** By `dof` (date of flight) in format `YYYY-MM-DD`
- **Location:** `data/flight_events/opdi_flight_events/dof=YYYY-MM-DD/*.parquet`

### 3.2 Schema and Field Descriptions

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Event unique identifier |
| `flight_id` | string | Flight identifier (links to Flight List.ID) |
| `type` | string | Milestone/event type (see Event Type Taxonomy below) |
| `event_time` | timestamp[ns] | Timestamp when event occurred |
| `longitude` | double | Longitude coordinate (WGS84) |
| `latitude` | double | Latitude coordinate (WGS84) |
| `altitude` | double | Altitude in feet |
| `source` | string | Data source identifier (NOT NULL) |
| `version` | string | Data processing version (NOT NULL) |
| `info` | string | Additional event-specific information (JSON or free text) |
| `dof` | string | Date of flight (partition key, format: YYYY-MM-DD) |

### 3.3 Event Type Taxonomy (44 milestone types)

#### Ground Operations - Actual Times
- `AOBT` - Actual Off-Block Time
- `ATOT` - Actual Take-Off Time  
- `ALDT` - Actual Landing Time
- `AIBT` - Actual In-Block Time

#### Ground Movement - Entry Events
- `entry-apron` - Entry to apron area
- `entry-deicing_pad` - Entry to deicing pad
- `entry-hangar` - Entry to hangar
- `entry-parking_position` - Entry to parking position
- `entry-taxiway` - Entry to taxiway system
- `entry-threshold` - Entry to runway threshold

#### Ground Movement - Exit Events
- `exit-apron` - Exit from apron area
- `exit-deicing_pad` - Exit from deicing pad
- `exit-hangar` - Exit from hangar
- `exit-parking_position` - Exit from parking position
- `exit-taxiway` - Exit from taxiway system
- `exit-threshold` - Exit from runway threshold

#### Runway Operations
- `line-up` - Aircraft lined up on runway
- `take-off-roll` - Take-off roll initiated
- `landing` - Landing event
- `go-around` - Go-around executed
- `runway-crossing-entry` - Entry to runway for crossing
- `runway-crossing-vacated` - Runway vacated after crossing
- `runway-vacated` - Runway vacated after landing

#### Trajectory Observation Bounds
- `first_seen` - First radar/ADS-B observation
- `last_seen` - Last radar/ADS-B observation

#### Climb/Descent Phase Transitions
- `top-of-climb` - Top of climb reached
- `top-of-descent` - Top of descent initiated

#### Level Flight Segments
- `level-start` - Level flight segment started
- `level-end` - Level flight segment ended
- `level-off-climb-start` - Level-off after climb started
- `level-off-climb-end` - Level-off after climb ended
- `level-off-descent-start` - Level-off during descent started
- `level-off-descent-end` - Level-off during descent ended

#### Altitude Crossings
- `xing-fl50` - Flight Level 50 crossing
- `xing-fl70` - Flight Level 70 crossing
- `xing-fl100` - Flight Level 100 crossing
- `xing-fl245` - Flight Level 245 crossing

#### Distance Horizons (from aerodrome reference point)
- `xing-40nm` - 40 nautical mile horizon crossing
- `xing-50nm` - 50 nautical mile horizon crossing
- `xing-60nm` - 60 nautical mile horizon crossing
- `xing-100nm` - 100 nautical mile horizon crossing
- `xing-110nm` - 110 nautical mile horizon crossing
- `xing-120nm` - 120 nautical mile horizon crossing

---

## 4. Key Performance Indicators Derivable

### Time-Based Metrics
- **Taxi Times:** ATOT - AOBT (departure), AIBT - ALDT (arrival)
- **Airborne Time:** ALDT - ATOT
- **Block Time:** AIBT - AOBT
- **Runway Occupancy Time:** Derived from take-off-roll to runway-vacated

### Efficiency Metrics
- **Additional Taxi Time:** Actual vs. unimpeded/reference taxi time
- **Arrival Sequencing Delay:** Time spent in holding patterns or extended vectors
- **Level Flight Efficiency:** Analysis of level-off segments during climb/descent
- **Vertical Flight Efficiency:** Analysis of top-of-climb and top-of-descent positions

### Variability Metrics
- **Temporal Variability:** Day-to-day variation in times for same city pairs
- **Spatial Variability:** Trajectory dispersion at horizon crossings
- **Seasonal Comparison:** 2025 vs 2026 June performance

---

## 5. Data Quality Considerations

### Completeness
- Not all flights have all milestone types (depends on flight profile, data coverage)
- Ground movement milestones may be airport-dependent (surveillance coverage)
- Horizon crossings dependent on flight length and trajectory

### Temporal Precision
- Timestamps are nanosecond precision but effective precision depends on source
- ADS-B typically 1-second resolution
- Radar typically 4-12 second resolution

### Superseded Records
- `SUPERSEDED_DEP` and `SUPERSEDED_ARR` flags indicate later corrections
- Analysis should filter for non-superseded records or use latest version

---

## 6. Analytical Data Layer Strategy

### Source Layer (current state)
- **Location:** `data-src/` (zip files, .gitignored if large)
- **Content:** Original OPDI downloads
- **Status:** ✓ Complete

### Analytic Layer (to be built)
- **Location:** `data/` (processed parquet, .gitignored or R2)
- **Content:** 
  - Filtered/cleaned flight list (non-superseded, specific airports)
  - Derived KPIs (taxi times, flight times, delays)
  - Aggregated statistics by airport pair, date, operator
  
### Report Layer (to be built)
- **Location:** `data/` or embedded in Quarto
- **Content:**
  - Summary tables for publication
  - Pre-computed visualizations data
  - Small datasets embeddable in reports

---

## 7. Next Steps for Scoping

### Questions to Define Analysis Scope

1. **Airport Selection:**
   - Which specific airport pairs or airports for this study?
   - PBWG regions: US (major hubs?), Canada (YYZ, YVR?), Brazil (GRU, GIG?), Europe (subset?), Thailand (BKK?), Singapore (SIN), Japan (NRT, HND?), China (PEK, PVG?)

2. **Metrics Focus:**
   - Primary: Flight time variability (actual vs scheduled, if available)?
   - Efficiency: Which indicators? (taxi time, airborne time, level segments?)
   - Benchmark: What constitutes "efficient" reference?

3. **Comparison Dimensions:**
   - Year-over-year (Jun 2025 vs Jun 2026)
   - Weekday vs weekend
   - By operator class (network, LCC, cargo)
   - By aircraft type

4. **Deliverable Structure:**
   - One Quarto document or multiple (by region, by metric)?
   - Target audience: Technical experts or mixed?
   - Length/depth: Deep dive or executive summary?

### Immediate Technical Tasks
1. Define airport/route filters for analytic layer
2. Identify if scheduled times (STD/STA) are needed - not in current dataset
3. Determine reference values for efficiency benchmarking
4. Establish data quality rules (filter criteria)
