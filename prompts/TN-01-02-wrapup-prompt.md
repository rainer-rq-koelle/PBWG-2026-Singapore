# Prompt – TN-01 / TN-02 wrap-up for the PBWG meeting

Use this prompt in a new session in the project `PBWG-2026-Singapore`. Run it once for TN-01 and once for
TN-02 (or both in sequence). It is modelled on the TN-03 wrap-up of 2026-11-09.

---

## Task

Bring **TN-0X** (`TN-01-milestone-bridging.qmd` / `TN-02-turnaround-data.qmd`) to an outward-facing state for
the PBWG meeting, render HTML and PDF, and build a PBWG slide pack from it. Work autonomously; ask only where
a decision is needed (listed under "Ask me").

Read first: project memory (AGENTS.md), `TN-03-plan.md`, `TN-03-tbo-conformance.qmd` (structure and tone to
match), `TN-03-PBWG-slides.qmd` and `R/tbo-slides.R` (slide approach), and the TN-0X qmd and its `R/` helpers.

## 1. Consistency review (report before editing)

Review TN-0X as an international PBWG reader (an operational performance expert, not EUROCONTROL staff):

- Does it guide the reader through concept → method → examples → application?
- Promises vs delivery: anything announced in the method that is not shown with data?
- Factual check: every number quoted in the text against the data/tables it comes from.
- Definitions consistent between method and code; terms consistent with TN-03 (milestone names, A/D rings,
  observation status, "reference", "contract point" where relevant).
- Internal jargon or system names in the main text (PRISME, Databricks, OPDI internals, table names, M1/M2/M3
  without explanation, "canonical", etc.).
- Undefined abbreviations; references to TN-01/02/03 without a one-line explanation.
- No LLM-style phrasing; PRU technical tone for the international domain; neutral, diplomatic framing of
  partner data (describe what it contributes; gaps "can be completed with a future release").

Write the findings (missing / inconsistent / factual slips) to `TN-0X-plan.md` before making changes, then
proceed with remediation without waiting. Clear open backlog items from project memory where they fit the
meeting scope; list the rest as open.

## 2. Remediation (as for TN-03)

- Add **key messages** up front and a **summary table** of what the note proposes or delivers (for TN-01:
  milestone dataset layers, fields, sources; for TN-02: turnaround/delay measures and where demonstrated).
- Fix factual slips and wording; align definitions; demonstrate or reword open "promises".
- Number sections (`number-sections: true`); check that all cross-references resolve.
- Add **Appendix: data used by role** (outward-facing, no internal systems) and **Appendix: abbreviations**.
- EUROCONTROL-specific processing goes into a separate companion report
  (`TN-0X-internal-processing.qmd`, factual, public repo OK, no secrets/tokens/hosts/paths – scan before
  commit) or into an appendix clearly labelled "EUROCONTROL-specific processing".

## 3. AEROTHAI contribution

AEROTHAI (Aeronautical Radio of Thailand, the air navigation service provider of Thailand) contributed
milestones in the PBWG format: `data/tbo/THA-SIN-JPN-DOM-milestones-20250601-20250930.csv`
(also `data/tbo/TBO-THA-flights-wide-20250601-20250930.parquet`), ~35,000 flights, Thailand–Singapore,
Thailand–Japan, Thai domestic; AOBT, ATOT, D40, D100, A100, A40, ALDT, AIBT; **no planned/scheduled times**.

- Check where it fits: TN-01 (partner data in the analytic milestone dataset; it would close the "SIN
  pending" gap and add a Thai/Japanese/Singapore region pair), TN-02 (turnaround analysis at VTBS/VTBD if
  arrival and departure of the same aircraft can be linked – check REG/ICAO24 completeness first).
- Integrate at least one example per note where it is meaningful; do not force it.
- Recognise the contribution: section or box, acknowledgement, data annex, slide "Thank you, AEROTHAI".
- No planned times → no delay/offset measures from Thai data; say so neutrally.

## 4. Render

- HTML and PDF of the note; HTML of the companion report.
- Check the PDF visually (pdftools page renders): tables fit, figures readable, no empty pages, cross-refs
  numbered.

## 5. Slide pack (Quarto → pptx, template `assets/PBWG-tmpl.pptx`)

Purpose: educate PBWG participants to grasp the concept and method, understand it with examples, and see
the application. The implicit message: **PBWG members can complement the study by adding their milestone
lists, building a PBWG-wide study dataset.**

Structure (adapt per note): why / what we ask → concept → method (with the math) → what the measures signify
(simple or synthetic examples) → European examples → partner contribution (AEROTHAI) → data requirements
(which milestones, which fields, minimum to start) → how members contribute → proposal / decision / next
steps.

Lessons learned from TN-03 (apply directly):

- Pandoc pptx does not handle two-column layouts well: put figures **full width** with commentary in
  speaker notes (`::: {.notes}`), and add a separate **takeaway slide** after key figures/tables.
- Tables via `flextable` (font 12, autofit) – keep to ≤ 6 rows × 5 columns; no text under a table on the
  same slide (it overlaps).
- Short slide titles (they collide with the PBWG logo top right).
- ggplot base size 16; fig-width 9, fig-height 4.2.
- Check the rendered deck via PowerPoint COM export (`Slide.Export` to PNG) and contact sheets; fix overlaps.
- Rendered pptx/html/pdf are gitignored (`TN-*-slides.pptx`); commit sources only.

## 6. Wrap-up

- Commit and push code and qmd (data stays local; the git hook asks for anything under `data/`).
- Copy HTML, PDF, slides (and companion report) to
  the OneDrive-synced Desktop folder `PBWG-Singapore/`.
- Update project memory (AGENTS.md) and the TN-0X status.
- Close-out note to me: what changed, what is still open, suggested 25-minute core version of the deck.

## Constraints and conventions

- UTC throughout; force UTC on parquet/Oracle reads; never change the R session time zone.
- Never commit `data/`, `data-src/` or flight-level files; ask before committing any data file.
- Do not save editor tabs I have open; if a file is open with unsaved changes, write to disk and tell me
  to close the tab without saving.
- Recompute numbers from data; do not quote figures from memory.

## Decisions taken up front

- One slide pack per note (`TN-01-PBWG-slides.qmd`, `TN-02-PBWG-slides.qmd`); length follows scope. Common
  thread in every deck: a combined **PBWG study dataset** serves the different work threads (TN-01 milestone
  dataset → TN-02 turnaround/delay propagation → TN-03 TBO). Reuse one shared "PBWG study dataset" slide.
- EUROCONTROL-specific quirks/fixes: companion report if more than ~1 page, otherwise an appendix titled
  "EUROCONTROL-specific processing", stating that partner data environments differ and face other issues.
- Aim: advance the state of the art in international operational performance monitoring; make reasonable
  assumptions in that direction.

## Ask me – with autonomous fallback (I am away; do not block)

Log every decision below in `TN-0X-plan.md` under "Decisions for review" and list them in the close-out note.

- AEROTHAI does not fit a note meaningfully (e.g. TN-02: REG/ICAO24 coverage too low to link arrival and
  departure at VTBS/VTBD): do not add a token section; add a short "partner data – future extension" paragraph
  and a slide stating which fields would enable it.
- Corrected numbers change a conclusion: apply the correction, reword the conclusion, flag it as
  "CHANGED CONCLUSION" in the plan file and close-out note.
- Deck exceeds ~45 slides: move detail to an appendix section of the deck; keep main deck ≤ 35 slides and
  mark the 25-minute core subset in the speaker notes.
- Do not push if the secrets scan or the git hook raises anything; commit locally and report.
