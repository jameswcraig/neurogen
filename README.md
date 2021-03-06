# neurogen <img src="vignettes/img/neurogen_logo.png" width = "200" align = "right"/>

An R package for creating generative art using Brain Imaging Data. 

The `neurogen` package was born from a collaboration with brothers Daniel and [Tyler Wishard, UCLA](https://www.wishard.bio/c-v) for a proposal at the 2022 Organization of Human Brain Mapping in Glasgow, Scotland - which is titled ["Neuro-Fungible Tokens"](https://www.wishard.bio/art). Proceeds from the sale of any NFT's will be donated to organizations conducting neurological disease research.

Tyler is a Ph.D. candidate in the Neuroscience Interdisciplinary Program at UCLA whose work focuses on multimodal MRI to determine neuroimaging-based markers of age-related memory decline and optimize non-invasive neuromodulation to improve older adults' cognitive outcomes.

<img src="vignettes/img/brain_warp_blues.gif" width = "400" align = "center"/>

All example image data provided in package was produced using a 7-tesla MRI of Tyler Wishard's brain, aka 'Ty's Brain' and can be found in `inst/data`.

<img src="vignettes/img/brain_infuse_optimized.gif" width = "400" align = "center"/>

---

<img src="vignettes/img/brain_warp_alt.gif" width = "400" align = "center"/>

## Installation

```
devtools::install_github("jameswcraig/neurogen")
```

## Usage

```
library(neurogen)

# Get Ty's brain...
nii_path <- system.file(package = "neurogen", "data", "Ty_brain.nii.gz")

```

### Brain Infuse

Infuse selected color palette into greyscale image.

```
brain_infuse(file = nii_path,
             color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8),
             infuse_rate = 0.9,
             trim_start = 0.31,
             trim_end = 0.08)
```

<img src="vignettes/img/brain_infuse_alt_optimized.gif" width = "400" align = "center"/>

### Brain Warp
  
Add a color warp to brain image.

```
brain_warp(file = nii_path,
           color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8),
           trim_start = 0.31,
           trim_end = 0.08)

```

```

brain_warp(file = nii_path,
           color_palette = RColorBrewer::brewer.pal(name = "PuBuGn", n = 9),
           color_background = "#FAF9F6",
           trim_start = 0.31,
           trim_end = 0.08)

```

```
brain_warp(file = nii_path,
           color_palette = c("#F8B195", "#F67280", "#C06C84", "#6C5B7B"),
           color_background = "#355C7D",
           trim_start = 0.31,
           trim_end = 0.08)

```


### Brain Contour

Add contour color into greyscale image.

```
brain_contour(file = nii_path,
             color_palette = RColorBrewer::brewer.pal(name = "Set2", n = 8),
             color_background = "#FFFFFF",
             trim_start = 0.31,
             trim_end = 0.08)
```
