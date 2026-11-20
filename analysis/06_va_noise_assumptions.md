# Noise from Virginia data centers: what is estimated and what is assumed

`analysis/06_va_noise.R` is a **screening estimate**. It shows roughly how far noise from each permitted site could reach and how many people live inside that distance. It is not a noise study and does not predict what a meter at any particular home would read. A study would need equipment layouts, the real enclosures, barriers, terrain and measurements.

Two sources are covered:

1. **Backup diesel generators.** These are built mostly from the Virginia DEQ air permits.
2. **Cooling equipment.** This is a rough, generic estimate, because no permit describes it.

Every number below is set at the top of the script (section 1). Change it there and rerun.

## 1. What the permits give

The 170 current permits with an equipment list give:

| Item | Use here |
|---|---|
| Engine count and kW ratings | Size of each source (11,339 engines, 29.3 GW) |
| Make and model (named in 149 of 170) | Recorded in `va_noise_sites.csv`. Mostly Caterpillar 3516 (78 permits), Cummins QSK, Caterpillar C175 and MTU 4000. Sound levels are scaled by rating, not looked up by model (see 2a). |
| Testing hours per engine, testing window | How often testing noise happens (section 5) |
| Site address | Where the source is (104 geocoded; 66 at county points) |

The permits **do not** cover:
- noise;
- what enclosure is fitted;
- how many engines may run at once (no Virginia permit limits this; CoreSite's Denver permit does);
- anything about cooling.

## 2. Generators

### 2a. How loud each engine is
The starting point is the makers' published sound pressure at 7 m, at full load, for a reference 2,500 kW set:

| Case | Configuration | dB(A) at 7 m | Source |
|---|---|---|---|
| quiet | Level 2 sound-attenuated enclosure | 74 | Caterpillar 3516C 60 Hz enclosure |
| **central** | **Level 1 sound-attenuated enclosure** | **79** | Caterpillar 3516C 60 Hz enclosure |
| loud | Open set, no enclosure | 97.5 | MTU 20V4000 DS2500 spec sheet (infinite exhaust, so exhaust noise is excluded) |

How these become sound power:
- **Conversion:** the 7 m level is converted with Lw = Lp + 20 log10(7) + 8, which assumes hemispherical spreading over hard ground. The central case is Lw ≈ 104 dB(A) per 2.5 MW engine.
- **Scaling:** other ratings are scaled by 10 log10(kW / 2500). The median engine is about 2.5 MW, so this matters only for small sets.

**Why Level 1 is central.** Enclosed sets are standard on data center yards, but the permits don't say which level is fitted, and some sites add walls or put engines inside buildings. The quiet and loud cases span about 23 dB, which changes distances by about ten times. The open-set case is a bound, not a likely configuration for most new campuses.

**What this leaves out:**
- **Load.** Spec-sheet levels are at full load. Routine exercise is often at partial or no load, which is a few dB quieter (the radiator fan still runs).
- **Exhaust.** The open-set figure excludes it; a critical-grade silencer would add a little.
- **Model differences.** Two makes of the same rating can differ by a few dB.

### 2b. How many engines run at once
**Routine test.** One engine at a time. This is the quietest testing can be. The permits allow a median 32 h per engine a year, about 2,050 engine-hours a year at the median site. Done one engine at a time inside a 7am–5pm weekday window, that is 79% of the window's hours. Twenty-seven sites cannot fit their permitted hours into the window one at a time (up to 31 engines would have to run together). So in practice many sites test several engines together: two add 3 dB, four add 6 dB.

**Outage or grid emergency.** Every engine runs at once, at full load. 158 of the 191 current permits allow running in grid-operator (PJM) emergencies. In a real outage the spare engines (N+1) may stay off and the rest run at the building's actual load, perhaps 1–3 dB lower. Loudoun exempts generator testing and emergency running from its sound limits.

**Height.** The source is at 3 m.

## 3. Cooling (generic)

### How much heat is rejected
No public Virginia record gives cooling equipment. So heat rejected outdoors is taken from generator capacity:

> heat rejected (MW) = generator capacity / 1.25

The factor 1.25 assumes N+1 backup on the whole facility load; essentially all the electricity a data center uses ends up as heat rejected outdoors. With 2N backup the factor would be 2.0, making cooling 2 dB quieter. This also assumes **the campus is built out**: many permits cover phases not yet built.

### How loud it is per MW
A-weighted sound power per MW of heat rejected, from published air-cooled chiller data:

| Case | Equipment | dB(A) per MW | Basis |
|---|---|---|---|
| quiet | Extra-low-noise chillers | 92 | Trane RTAF (Sintesis) range, scaled to 1 MW |
| **central** | **Low-noise chillers** | **95** | same |
| loud | Standard chillers | 98 | same; 97 dB(A) for a 732 kW unit |

A site's sound power is that value + 10 log10(MW). The central case for a 200 MW campus is about 118 dB(A).

**Check.** One trade article puts 72 chillers at about 96 dB(A) each at about 115 dB(A) combined. That is consistent with the central value for a campus of that size.

### What this leaves out
- **Equipment type.** Evaporative cooling towers, dry coolers, rooftop units and liquid-cooled systems differ. Published sound power spans about 15–30 dB across cooling technologies (Bray and Dominici 2026). Liquid cooling still rejects its heat outdoors, through fewer, slower fans.
- **Operating level.** Levels are for a design summer day at full load. Fan noise falls about 50 log10 of fan speed, so fans at 70% speed on a cool night are about 8 dB quieter.
- **Footprint.** The cooling is placed at a single point at 15 m (rooftop or raised yard). A real campus spreads it over hundreds of metres, so levels within about 150 m of the site point are overstated.

## 4. How sound travels

The script uses ISO 9613-2, the standard outdoor method, in its simplified A-weighted form:
- **Spreading:** 20 log10(d) + 11 dB.
- **Ground:** the simplified ground term (eq. 10), with its directivity correction (eq. 11).
- **Air absorption:** 1.9 dB/km for generators (500 Hz) and 1.0 dB/km for cooling (250 Hz), at 10 °C and 70% humidity.
- **Receptor height:** 1.5 m.

Under these assumptions the ISO method is accurate to about ±3 dB out to 1 km. The assumptions are:
- **Downwind, or a mild night inversion.** This is the condition the method is built for, and it favours sound travelling, which suits a screening estimate. Upwind and in the daytime, levels at distance are lower.
- **No barriers.** Data center buildings, walls and berms often cut 5–15 dB in their shadow. Leaving them out is the largest single source of overstatement.
- **Flat, mostly soft ground.**

**A-weighting understates the complaint.** Residents mostly complain about the low hum and tones from cooling fans (around 70–270 Hz near Amazon's Great Oak site). dB(A) discounts exactly those frequencies. Prince William County's 2025 ordinance added a C-weighted limit for steady tones because of this. This screening is in dB(A) only.

## 5. Thresholds

- **55 dB(A).** Loudoun's residential property-line limit, day and night. Fairfax allows 60 by day and 55 at night.
- **45 dB(A).** About 10 dB above the night background measured in a Prince William neighbourhood (32–35 dB(A)). At this level a steady source is clearly audible over the background.

## 6. People

People are counted on a 50 m grid out to 5 km from each geocoded site:
- **Population.** Each grid cell gets its block group's population density (ACS 2019–2023). This spreads people evenly over the block group, including over the data center land itself.
- **Adding up sites.** Cooling and outage levels from all sites are added together as energy. Routine tests are taken one site at a time, using the loudest single site at each point.
- **Sites at county points (66 of 170) are left out.** These include 43 of Loudoun's 80 sites, among them Vantage's campuses, so people counts are **low**, most of all in Loudoun.
- **Geocoded addresses are street addresses, not equipment yards.** The yard can be 100–300 m from the point.

## 7. Results (central case)

| Source | Distance to 55 dB(A), median site | Distance to 45 dB(A), median site | People ≥ 55 dB(A) | People ≥ 45 dB(A) |
|---|---|---|---|---|
| Routine test, one engine | 73 m | 200 m | about 700 | about 4,800 |
| Outage, all engines | 400 m | 1.1 km | about 24,000 | about 200,000 |
| Cooling, design day | 240 m | 660 m | about 8,300 | about 96,000 |

The quiet-to-loud range is in `va_noise_summary.txt`.
- **Routine test:** the range is about 300 people (quiet) to 27,000 (loud, open sets) at 55 dB(A).
- **Cooling:** the range is 4,400 to 16,000 people at 55 dB(A).

**Comparison with measurements.**
- At Vantage VA2 in Sterling, the level measured at a residential property line was 51–53 dB(A).
- Near Amazon's Great Oak site in Prince William, residents measured about 58–65 dB(A) before a fan retrofit and about 48–55 dB(A) after.

Homes a few hundred metres from a large campus fall at those levels in the central cooling case. So the screening is of the right order. It cannot be checked site by site, because Vantage VA2 is one of the sites at a county point.

**Who lives inside.** Within 45 dB(A) of cooling, compared with Virginia as a whole:
- Asian residents ×2.7, Latino ×1.9 and the highest-income fifth ×1.8.
- The lowest-income fifth ×0.04 and Black residents ×0.7.

This is a regional pattern: the sites are in Loudoun and Prince William. It is not evidence about which neighbourhoods next to the sites are affected.

## 8. What would make this a real estimate

1. **Site plans.** County site-plan noise studies, which Loudoun requires before and after construction. They give the equipment layout, enclosures and modelled levels at property lines.
2. **Measurements** following the protocol in Bray and Dominici (2026): 72 h or more, octave or narrow band, with dB(C).
3. **Census blocks and parcels** in place of block groups, and equipment-yard locations in place of addresses.
4. **A full ISO 9613-2 or CadnaA/SoundPLAN model** with buildings as barriers and octave-band spectra.

## Sources

- Caterpillar, 3516C 60 Hz sound attenuated enclosure: https://www.cat.com/en_MX/products/new/attachments/electric-power-genset-enclosures/enclosures/125120.html
- MTU 20V4000 DS2500 spec sheet, sound data: https://collicutt.com/wp-content/uploads/2019/09/mtu4000-20v-ds2500-stand.pdf
- Trane RTAF (Sintesis) air-cooled screw chiller, sound power: https://de.slideshare.net/ross_giles/rtaf-sintesis-high-efficiency-vairable-speed-air-cooled-screw-chiller
- Havtech, data center noise control (72 chillers example): https://www.havtech.com/?p=14551
- Bray D, Dominici F (2026), Noise emissions from hyperscale AI data centers: https://arxiv.org/pdf/2610.03975
- Loudoun County, data centers: noise and air quality (55 dB(A) residential limit): https://www.loudoun.gov/6405/Data-Centers-Noise-and-Air-Quality
- Fairfax County noise ordinance review, 60/55 dB(A) residential: https://www.fairfaxcounty.gov/planning-development/sites/planning-development/files/Assets/Documents/zoning%20ordinance/proposed%20amendments/Noise-Report.pdf
- Prince William County Data Center Ordinance Advisory Group notes (background 32–40 dB): https://www.pwcva.gov/assets/2025-07/DCOAG%20Meeting%20Notes%2020250709.pdf
- Planetizen on generator-testing exemptions and Prince William's dB(C) limits: https://www.planetizen.com/features/138741-noise-limit-data-centers-only-good-its-measurement
- ISO 9613-2:1996, Acoustics — Attenuation of sound during propagation outdoors, Part 2.
