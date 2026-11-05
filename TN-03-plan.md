# TN-03 – Towards trajectory-based operations: plan vs actual (worked example)

Status: plan, 2026-11-04.

## Scope (agreed)

- Compare the **reference trajectory** with the **actual flight (Model 3, CTFM)**. Reference = Model 2 (RTFM)
  if the flight was regulated, otherwise Model 1 (FTFM). The M1 -> M2 change is reported only as the revision term
  of the time decomposition (see Agreement stability); ATFM impact itself is out of scope.
- Model 3 is accepted as ground truth for developing the concept. OSN/OPDI is an optional quick check only (OPDI ends 2025-06-13).
- Period: 1–14 June 2025, the same window as the TN-01/02 samples.
- Pairs (both directions): EGLL–EDDM, EHAM–LEMD, LTFM–EDDF.
- Airspace: European FIRs (`eurocontrol::fir_sf()` with the AIRAC valid in June 2025, or `pruatlas::firs_nm_*`).
- Literature: about 10–15 sources and a one-page synthesis table.
- The framing is generic (planned/projected vs actual), and the European data serve as the development set before global scaling.

## Concept

Two ways of comparing the reference trajectory R with the actual trajectory A:

1. **Discrete milestones**, using the TN-01 milestone table format with `MST_TYPE` = REF/ACT and `REF_MODEL` = M1/M2.
   - Milestones: AOBT, ATOT, D40/D100/D200, TOC, FIR entry/exit, TOD, A200/A100/A40, ALDT, AIBT.
   - Measures: Δt = t_A − t_R, plus position and level offsets at each milestone.
   - Δt along the flight shows how the offset builds up, in the same way as the TN-02 chain logic.
2. **Continuous decomposition** between milestones:
   - cross-track (lateral), along-track (ahead/behind on the path), and vertical deviation;
   - pairing points by position or by time. Pairing by time gives the 4D "where should I be now" error.
   - The difference between the two pairings is the core methods point: space vs time.
3. **Cheap, scalable signatures** for global use: whether the FIR sequence matches, the FIR entry/exit Δt, and the
   distance flown (A vs R). These work with coarse or partner data.

### Matching direction (proposal, 2026-11-04)

- Milestones are computed on each trajectory independently, so the comparison is symmetric. If a milestone exists on
  only one trajectory, it is reported as a sequence mismatch.
- For the continuous measures, the reference trajectory R is the anchor, since R is what was agreed ("contract view").
  1. Densify R: interpolate along the great circle between points, linearly in time and FL, and resample
     at a fixed arc-length step (e.g. 1 NM). This gives the reference arc length s.
  2. Project each A point onto R, which gives s_A and the signed cross-track distance d. Force s_A to be
     non-decreasing (running maximum). Holdings and vectoring then appear as time spent without progress.
  3. Compute the deviations as functions of s:
     - Δt(s) = t_A(s) − t_R(s), the along-track/time error;
     - d(s), the lateral error;
     - ΔFL(s), the vertical error.
     The time-based view, Δs(t) = s_A(t) − s_R(t), follows from the same mapping.
  4. Validity mask: no projection inside 40 NM of the airports (procedures/runways; use the ASMA milestones there),
     and none where |d| > D_max (e.g. 20 NM; to be tested). Those spans are flagged as a route change, where only
     the milestone and FIR measures apply.
- The directed A -> R measure (how far A strays from R) is reported only as a summary (directed and
  symmetric Hausdorff distance). It is not used for time matching.

### Framing and reviewer input (adopted 2026-11-04)

- Framing: **trajectory-contract delivery**, not "deviation". Adherence and efficiency are separate: a flight can
  adhere to an inefficient agreement, and a deviation such as a shortcut can be beneficial. The signed decomposition
  keeps the direction of each deviation.
- The reference is an explicit label (`REF_MODEL`). In the demo it is M1 = last filed plan, or M2 = the agreed trajectory
  after ATFM regulation. Both are proxies for the "agreed trajectory", and the TN says so. The preferred trajectory,
  version history and RTA/clearance records are not available. They are named as data requirements for partners.
- Dimensions are kept separate (no composite index):
  - horizontal (d, distance difference);
  - vertical (ΔFL at matched progress, TOC/TOD displacement);
  - temporal (Δt at milestones, and **segment-wise ΔΔt** that separates departure delay from delay gained or
    recovered in the air);
- Agreement stability, revised 2026-11-04: use **M1 -> M2 -> M3 as a minimal version history**. M2 is the
  "revised (ATFM-imposed) agreement". For unregulated flights M2 := M1, so the revision is 0.
  - Time at a common event decomposes exactly:
    Δt(M1,M3) = Δt(M1,M2) + Δt(M2,M3), i.e. total = revision + execution.
  - Geometric measures (d, ΔFL) are not additive. They are reported separately for M1 vs M3 and M2 vs M3, plus
    whether M1 -> M2 changed the route or level.
  - Caveat: M2 is imposed rather than negotiated, and it is a single snapshot. Intermediate FPL changes (DLA/CHG)
    and the lead time of the revision are not captured; these are future data requirements.
  - consequence (deferred).
- Three alignment views: same time, same progress (s), same event (milestones). Shape-only matching such as DTW is
  rejected as the primary measure, because it removes the timing error that TBO monitoring needs.
- Precedents (verify): KEP/KEA horizontal flight efficiency (mature conventions, but against a geographic reference,
  not 4D adherence; differencing KEP and KEA is not adherence); en-route vertical flight efficiency (VFE) / FLair
  (planned vs actual flight level with tolerances, the precedent for vertical tolerance bands).
- The TN includes a "measurement specification" checklist for partners: reference version cut-off, phase boundaries,
  surveillance coverage, interpolation, altitude datum, tolerances, aggregation. Context factors such as weather and
  demand are listed as caveats only, not modelled.

### Literature mapping (inputs 2026-11-04: synthesis M01–M12, reviewer note A–I)

Four measurement objects frame TN section 2: agreement delivery, predictability, stability, consequence.
Each indicator in the register is tagged with its evidence level: operational / demonstrated on recorded traffic /
simulation-based / proposed only.

| Demo element | Literature anchor | Status in demo |
|---|---|---|
| Δt at milestones, segment-wise ΔΔt (airborne drift) | M01: APACHE PRED-1 (proposed, not implemented); Ruiz et al. 2018 | implemented |
| d(s), Δs, route-change mask | M03: Bongiorno et al. 2017; Andreeva-Mori 2024 | implemented |
| Signed distance difference | M04: Bongiorno et al.; APACHE ENV-1.2; HFE achieved distance | implemented |
| ΔFL(s), TOC/TOD shift | M05: RP4 vertical en-route (referenced, not adopted) | implemented; RP4 stress test |
| Revision vs execution split (M1 -> M2 -> M3) | M08: APACHE PRED-2/5 (proposed only) | minimal version history |
| CTOT window (−5/+10) as a tolerance window | M02 (proxy window, not an agreed TBO tolerance) | optional |
| FIR-sequence match, deviation hotspots | M09: Bongiorno di-fork | FIR level only |
| Prediction accuracy by horizon, ETA jumps | M06/M07: Enea et al. 2013 | data requirement (archived predictions) |
| Sharedness, commitment survival, feasibility/flexibility, probabilistic conformance | reviewer note E–H, A–D: Idris 2008, Zheng & Zhao 2012, Sotiriou 2016, Yan 2017 | outlook only |
| Fuel/cost decomposition | M11: Prats et al. 2019, Kuljanin 2021 | deferred |

Rules carried into the TN:
- Do not describe APACHE indicators as established KPIs. Lee & Fukuda is second-hand (NASA survey): verify or drop.
  Tao et al. 2021 is cited only as a caution that similarity measures rank trajectories differently.
- Report the coverage and assessable share next to every indicator. Unobserved does not mean compliant.
- Use equal flight weights for flight scorecards and exposure sums for network views. Never average percentiles.
- Report signed values, absolute severity and compliance separately.
- Synthetic test cases (WP5 pre-step): pure clock shift, shortcut, dogleg, step climb, late descent, hold,
  surveillance gap. Expected invariances: a clock shift leaves d(s) and ΔFL(s) unchanged and changes only Δt; the
  revision term is zero for unregulated flights.

### Stress test of the RP4 vertical en-route indicator (added 2026-11-04)

The RP4 indicator is referenced, not adopted. The aim is to show, on the same flights, where it does and does not
answer a meaningful question. Its definition must first be reproduced exactly from the 2025 metadata (WP0).

Indicator as published (to verify): the distance share flown WITHIN (planned FL ±1000 ft, exclusive) or ABOVE,
against the last filed plan.

Hypotheses to test with the demo data:
1. **It asks two questions at once.** Counting ABOVE as good mixes an efficiency judgement with plan adherence.
   Test: compute RP4 next to a symmetric adherence share, P(|ΔFL| < 1000 ft), and the signed ΔFL distribution.
   Report how often the two rankings disagree.
2. **The reference already contains the constraints.** If the filed plan already includes the level caps and
   restrictions, a flight can score WITHIN while flying an inefficient profile. Test: compare filed vs. requested
   cruise level (RFL), and where possible the unconstrained optimum. Indicator: share of flights that are "WITHIN"
   but capped below RFL.
3. **It is blind to revision.** M2 can change levels, but RP4 compares only with M1. Test: compute it against M1 and
   M2, and use the revision/execution split for ΔFL.
4. **It ignores time.** Distance shares say nothing about when a flight levelled off or how long it took. Test: TOC
   and TOD displacement (Δs, Δt), and the overlap with the VFE level-off measure (TN-01).
5. **Threshold and aggregation sensitivity.** Test: sweep the band (±500/1000/2000 ft); compare distance-weighted
   with flight-weighted results; compare per-pair results with the pooled result.

Demo output: one table with RP4, the symmetric share and the signed ΔFL per pair/direction, plus 2–3 showcase flights
where RP4 and adherence point in opposite directions. Wording: "complementary view", "the indicator answers a
different question". No claim of failure without evidence.

### Generic cases (archetypes) as algorithm tests (2026-11-05)

Each case isolates one dimension and has a known answer. They serve as synthetic unit tests and as the
TN's explanatory figures; real flights are combinations of them.

| Case | Synthetic construction | Expected result | Edge it exposes |
|---|---|---|---|
| A On-plan, late | M3 = M1 shifted by +Δ | d = 0, ΔFL = 0, Δt(s) = Δ constant, segment ΔΔt = 0 | baseline |
| B Recovery/loss en-route | M1 with speed change | Δt(s) drifts; segment ΔΔt ≠ 0 | departure vs air delay |
| C Shortcut (direct) | cut a dogleg | signed d, ΔL < 0, s jumps forward | projection jumps at doglegs |
| D Detour/reroute | offset segment > threshold | off-route span flagged, Δt undefined inside, re-join Δt | route-change rule |
| E Vertical | cruise FL capped, TOC/TOD shifted | ΔFL band, TOC/TOD Δ | level vs time coupling |
| F Holding/vectoring | loop near arrival | s stalls, time without progress | running max / terminal cut |
| G Sparse actual | thin out M3 on a direct | same result as dense | interpolation sensitivity |

### Algorithm adaptations (from the LEMD–EHAM showcase)

1. Densify **both** trajectories: R at 1 NM arc length, A at a fixed time step (e.g. 30 s), great-circle interpolation.
2. Project onto R **segments** (continuous s, not the nearest vertex), with a forward-only search window
   (monotone matching), instead of global nearest vertex + cummax.
3. **Signed** cross-track (left/right of R).
4. Evaluate Δt and ΔFL on a **common s grid** (t_A(s) interpolated), so the result does not depend on A's density.
5. Off-route spans: sustained |d| > D_max over a minimum length -> report the extra distance of the span and Δt at
   re-join; no point-wise Δt inside.
6. Terminal cut by each trajectory's own 40 NM ring crossing (milestone), not by s on R.
7. Extra distance decomposed by zone/segment: ΔL = ΔL_dep + ΔL_enr(per span) + ΔL_arr.

## Data and code reuse

- Flight list: `eurocontrol::flights_tidy()` filtered on the pairs, keyed by `ID` (SAM_ID). Flag regulated flights.
- Profiles: `xx-test-gotcha/R/rqutils-get-profiles.R` -> `extract_point_profiles()` for FTFM, RTFM (to be added) and CTFM.
  - Columns: FLIGHT_ID, TIME_OVER (UTC), LAT, LON, FLIGHT_LEVEL.
  - Check point density, and whether point names/types (TOC/TOD) are available.
- APDF: hub milestones through the existing TN-02 extraction (SAM_ID join).
- TOC/TOD: from the NM profile if present, otherwise from the existing VFE algorithm (`R/vfe.R`), applied to M3 and to the reference.
- FIR crossings: intersect the profiles with the FIR polygons (`xx-test-gotcha/R/eur-boundary-crossings.R` as a pattern).
- Output: `data/tbo/` (local only, gitignored).

## Work packages

| WP | Content | Output |
|---|---|---|
| 0 | Verify the literature (13 synthesis refs + 5 reviewer refs; complete the bibliographic data), indicator register | `refs.bib` + TN section 2 |
| 1 | Flight list, regulated flag, sample sizes per pair/direction | flight list parquet |
| 2 | Extract M1/M2/M3 profiles, check point density | profile parquets |
| 3 | Milestone engine: rings, FIR crossings, TOC/TOD on both trajectories; APDF join | milestone table (REF/ACT) |
| 4 | Metrics: Δt per milestone, cross/along/vertical deviation, FIR-sequence match | metric tables |
| 5 | Worked example: one showcase flight, then the pair/direction summaries | figures |
| 6 | TN-03 qmd (intro, background, concept/methods/data, worked example, discussion toward a PBWG indicator) | html/pdf |

## Status 2026-11-05

- Sample: 1–14 June 2025, six pair-directions, 950 flights.
  - 611 flights have Model 2 fields (`COBT_2` set): 391 with ATFM delay > 0, 220 with 0 min.
  - The NM Model 2 summary fields are a pure time shift of Model 1: RTE_LEN_2 = RTE_LEN_1, ARVT_2 − COBT_2 = block time of
    M1, FL_TOP_2 always empty. They cannot identify reroutes. COBT_2 − EOBT_1 = ATFM_DELAY for only 72% of flights
    (to be checked).
- RTFM point profiles are **not loaded** in `FSD.ALL_FT_POINT_PROFILE`.
  - 0 rows on the 1st of every month 2023-01 to 2026-10, under PRU_DEV and PRU_READ. The other 7 model types
    (CPF CTFM DCT FTFM SCR SRR SUR) are present on every date checked.
  - The colleague's wrapper patch (`profile = "RTFM"`) reads the same table. DBA query pending.
  - RTFM appears only in an old sample table (`CPF_CPG_ALL_FT_SAMPLE`, May 2010) and as fuel/charges columns in
    `ALL_FT_FLIGHT_PRC`. DDR2 is not reachable through the `eurocontrol` package.
- Point density: M1 ≈ 66 points/flight, M3 ≈ 154 points/flight.
- Code:
  - `R/tbo-fir.R`: NM FIR airspace profiles (M1/M3).
    - Merge FIR/UIR (first 4 letters), drop visits under 60 s.
    - Distinct crossings (first entry per region), with status delivered / planned-only / actual-only and offsets
      Δt, ΔFL, Δpos.
  - `R/tbo-milestones.R`:
    - D/A 40/100/200 rings (interpolated ARP distance crossing);
    - TOC/TOD placeholder (first/last point at max FL);
    - FIR crossings;
    - all combined in one long table, with the M1 vs M3 contract comparison.
- First results (M3 − M1):
  - Median Δt falls from 6.3 min (D40) to 4.7 min (A40), i.e. departure delay is carried through with modest
    recovery in the air.
  - ΔFL ≈ −9 FL at A100/A40.
  - A40 position offset P90 = 26 NM (vectoring).
  - FIR-crossing sequence match: EHAM–LEMD 100%, LEMD–EHAM 98.7%, EDDF–LTFM 87%, LTFM–EDDF 62%, EGLL/EDDM ~50%.
    The mismatches are mainly directs cutting FIR corners (e.g. EGTT>LFFF>EBUR).
- Showcase flight AEA36MB LEMD–EHAM 2025-06-12 (ID 286221690):
  - +24 NM in total, made up of an en-route shortcut (−) and EHAM arrival vectoring (+21 NM cross-track);
  - FIR-entry Δt 8 → 11.8 → 7.7 min.
- The prototype along-track projection (nearest vertex + cummax) is still session-only. It is to be rewritten
  following "Algorithm adaptations".
- Data: no parquet saved yet; everything is re-extractable from PRISME.

## Next steps

1. TOC/TOD from the VFE algorithm (`R/vfe.R`).
2. Segment-wise Δt between consecutive milestones.
3. Save the flight list, profiles and milestones to `data/tbo/` (local, gitignored).
4. Rewrite the projection (`R/tbo-projection.R`), with synthetic cases A–G as tests.
5. Verify the ARP coordinates in `tbo_arp` (currently from memory).
6. Add RTFM once it is located (`MODEL = "M2"`, revision + execution split).

## Internationalisation (plan 2026-11-05)

Check (1–7 June 2025, hub <-> CYYZ/KATL/RJAA/SBGR/VTBS/WSSS/ZBAA, 1,532 flights): ring milestones M3 vs M1.

| End | Ring | identical position + FL (%) | median Δt (min) |
|---|---|---|---|
| INTL arrival | A200/A100/A40 | 92–97 | 6.8–6.9 (= Δt at EUR departure, 6.5–6.9) |
| INTL departure | D200/D100/D40 | 36–48 | 12.2–12.8 (= Δt at EUR arrival, 12.5–13.1) |
| EUR end | all rings | 3–21 | – |

- Outside the NM area, M3 for flights **leaving** Europe is M1 shifted by the time offset at exit: no observation,
  and en-route gain or loss outside EUR is invisible.
- For flights **arriving** in Europe, M3 at the INTL departure end is partly updated (position P90 11–27 NM), but the
  source is unclear (FPL updates or surveillance?). Treat it as unverified.
- Point density near the INTL airports is fine (about 9 points within 40 NM, about 20 within 200 NM). The gaps are mid-ocean
  (max gap 400–3,300 NM). This is not a problem for the ring milestones.

Strategy
1. **Reference (M1) is global**: the filed trajectory covers the whole route, so rings at both ends are always defined.
   M2 is pending (RTFM access); use M1, with a synthetic M2 only for the method demonstration.
2. **Actual is labelled by observation status** per milestone: `OBS = observed | extrapolated | partner | osn`.
   Rule: M3 milestone with ΔPOS < 0.1 NM and ΔFL = 0 at both ends, outside the NM area -> `extrapolated`.
   Extrapolated milestones are reported as "not observed", never as delivered.
3. **Range rings around the INTL airport** (A/D 200/100/40) are the proxy boundaries, so no global FIRs are needed. Add one
   universal contract point: **NM-area boundary entry/exit** (`xx-test-gotcha/R/eur-boundary-crossings.R`),
   observed on the EUR side.
4. **Actual at the INTL end**: from partner data (ALDT/AIBT, ATOT/AOBT, rings from partner surveillance), which is
   the incentive framing. OSN is an optional gap filler near the INTL airports only (ring crossings, not full profiles),
   on a small sample to show feasibility. No broad profile augmentation.
5. Demonstration in the TN:
   - EUR–EUR showing full contract delivery;
   - EUR–INTL showing delivery observed on the EUR side, "not observed" at the INTL side;
   - with partner or OSN data, the gap closed.
   This shows directly what partner data adds.

### Actual-source test at INTL end (1–7 June 2025, 1,532 flights)

| INTL | NM CPF within 200 NM | OSN/OPDI C100 coverage arr / dep |
|---|---|---|
| CYYZ, KATL | dense (45 s) | 0.99 / 0.87 |
| SBGR | dense | 0.01 / 0.00 (C40_ARR 38 of 99) |
| ZBAA | dense | – / 0.04 |
| WSSS | endpoint only | 0.97 / 0.84 |
| VTBS | endpoint only | 0.71 / 0.33 (C40 ~0.8) |
| RJAA | endpoint only | 1.00 / 0.71 |

- CPF and OSN complement each other: CPF covers the Americas and ZBAA, OSN covers WSSS/RJAA/VTBS, and ZBAA is covered by CPF.
- OSN gives C40/C100 (and 50/60/110/120 NM) in the flight list, but no 200 NM; we accept 100 NM as the outer ring internationally.
- OSN Δt to M1 is plausible: e.g. WSSS A100 median +3.9 min, against M3 extrapolated +6.9; only a few cases
  with |Δt| > 60 min (linkage check needed).
- Source order for the INTL-end actual: partner > OSN ring event > CPF (with guard: points on both sides of the ring,
  gap ≤ 5 min) > M3 (labelled extrapolated). The source is recorded per milestone (`OBS`).
- Aireon (ECTRL feed, partial, NM-relevant points only): trial pending. Hive JDBC jar and secrets exist
  (data-fetcher); there is no R driver (odbc/RJDBC not installed). Options: install RJDBC, or extract once via the
  data-fetcher app.
- Aireon: parked (2026-11-05). Hive returns 401 despite a fresh workload password, so it is an environment access/role
  issue (de-live / az-live). The script `python/aireon_trial.py` is ready; use it only to fill complete gaps.
- Decision: OSN/OPDI flight-list ring crossings (C40/C100, both ends) are the primary "actual" at the INTL end;
  NM CPF (with guard) fills SBGR/ZBAA; M3 outside the NM area = extrapolated. Outer international ring = 100 NM.

## Canonical contract table (2026-11-05)

`R/tbo-contract.R` -> `contract_table()`, `contract_summary()`. One row per flight × contract point:
- identity: UID, FLTID, ADEP, ADES, REGION_PAIR;
- point: SEQ, MST, MST_CLASS (ring | vertical | fir), SIDE (EUR | ENR | INTL);
- reference: REF_MODEL (M1, later M2), REF_TIME/LAT/LON/FL;
- actual: ACT_TIME/LAT/LON/FL, ACT_SRC (NM-M3 | NM-CPF | OSN | PARTNER | NM-M3-EXTRAP | PROXY),
  ACT_OBS (observed | extrapolated | proxy | missing);
- outcome: STATUS (delivered | planned-only | actual-only | not-observed), DT_MIN, DFL, DPOS_NM;
- DDT_MIN: change in DT since the previous delivered point, i.e. the segment gain or loss.

Rules:
- Ring sources (OSN, CPF) give a time only, so the position and FL of those points are left empty.
- M3 identical to M1 on the INTL side (rings and FIR crossings into non-EUR regions) is extrapolated, so the
  point is not-observed.
- Partners contribute by filling ACT_* for their points (ACT_SRC = PARTNER-<ISO3>); not-observed -> delivered.

Week 1 international flights (1,532 flights, 32.6k points), `data/tbo/TBO-contract-INTL-20250601-20250607.parquet`:
- 7,388 of 10,341 non-EUR FIR crossings are not observed. That is the measurable gap partners can close.
- Share of observed points per flight: EUR->INTL 0.52–0.69, INTL->EUR 0.64–0.89.
- At least one observed INTL ring: ≥ 96% for most airports; ZBAA arrivals 45%, SBGR arrivals 76%.
## M2 (RTFM) added (2026-11-05)

- Source: Databricks data product `dp_all_ft_flights / output_flights` (v1.0.0), column `rtfm_all_ft_pt_list`.
  - Linked via `ifps_id` = `FLX.FLIGHT.IFPS_PLAN_ID`; latest version per flight.
  - Extracted in 7 chunks (no file writes in Databricks, preview download cap ~10k rows).
  - All 611 regulated flights, 40,378 points; parser in `R/tbo-rtfm.R`.
- **Timestamp issue:** Databricks timestamps (`time_over`, `lobt`, `cobt`) are written with a "Z" suffix but are 2 h
  behind UTC, i.e. local time CEST (Europe/Paris) labelled as UTC. Verified against PRISME `LOBT`/`COBT_2` (Δ = −120 min
  for all). Corrected with +2 h (valid for summer time only). To be raised with the data product owner.
- M2 vs M1:
  - Departure shift = `COBT_2 − EOBT_1` for 100% of flights (= ATFM delay for 76%).
  - Route identical for 595 of 611 flights.
  - 14 reroutes (EHAM–LEMD 12: two ~28 NM alternatives ±26/+51 NM; EDDF–LTFM 1; LTFM–EDDF 1); 2 minor (≤3 NM) point changes.
  - 220 zero-delay flights: 213 identical, 5 reroute, 2 minor. So zero-delay "regulated" flights are mostly
    re-issued slots without a time or route change, not reroutes.
- Ring decomposition (TOTAL = REVISION + EXECUTION, exactly additive), medians:
  - delay > 0: revision ≈ +11 min, execution −0.8 (D40) → −1.9 (A40). The flight recovers about 1–2 min against
    the regulated plan.
  - delay = 0: revision 0, execution +2.9 (D40) → +0.6 (A40).
  - Execution IQR is about 7–8 min in both groups.
## Reroutes (2026-11-05)

- Scan (Databricks, regulated flights at the 10 study airports, 1–14 June 2025): of 64,219, 1,998 (3.1%) have a
  different route string AND waypoint sequence (M1 vs M2), and 588 differ in the route string only. `rerouting_why` is not
  informative (93% `N`). Databricks `atfm_delay` is in seconds.
- **Reroute definition (adopted):** M2 deviates more than 5 NM laterally from M1 en route (first and last 40 NM of
  M1 excluded); `R/tbo-reroute.R` → `reroute_flags()`. In the 98-flight showcase, 44 meet it; the remainder are SID/STAR
  or naming changes.
- Delivery against M1 vs M2 (`compare_refs()`), en-route medians:

  | Group | Ref | cross-track mean | cross-track P95 | Δt en route (IQR) |
  |---|---|---|---|---|
  | reroute (44) | M1 | 3.8 NM | 15.2 NM | 9.9 min (13.6) |
  | reroute (43) | M2 | 2.9 NM | 12.2 NM | −0.8 min (6.5) |
  | no reroute (54) | M1 | 2.8 NM | 9.8 NM | 9.2 min (12.5) |
  | no reroute (54) | M2 | 2.8 NM | 9.9 NM | 1.1 min (6.2) |

  M2 as the reference roughly halves the timing spread for all regulated flights. For reroutes it also reduces the
  lateral deviation. The remaining ~3 NM mean / ~12 NM P95 cross-track is tactical (directs).
- Not yet in the canonical contract table: the FIR crossings for M2 (no M2 airspace profile; would need polygon
  intersection or the Databricks `rtfm_all_ft_asp_list`).

## Open points

- Whether RTFM is available through `point_profiles_tidy()`; how the regulated flag and CTOT are sourced.
- The AIRAC for the June 2025 FIRs (the scripts hardcode 544 = June 2026).
- Literature citations from the first scan are **unverified**.
