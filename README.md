# neurogen

An R package for creating generative art using brain imaging data. `neurogen` was born from a collaboration between Daniel and [Tyler Wishard, UCLA](https://www.wishard.bio/c-v) on a proposal submitted for the 2022 Organization of Human Brain Mapping Conference in Glasgow, Scotland - which is titled ["Neuro-Fungible Tokens."](https://www.wishard.bio/art). 

Tyler is a Ph.D. candidate in the Neuroscience Interdisciplinary Program at UCLA whose work focuses on multimodal MRI to determine neuroimaging-based markers of age-related memory decline and optimize non-invasive neuromodulation to improve older adults' cognitive outcomes.

All example image data provided in package was produced using a 7-tesla MRI of Tyler Wishard's brain, aka 'Ty's Brain' and can be found in `inst/data`.

<img src='vignettes/img/brain_warp_alt.gif' align = 'center' height = '750'/>


## Installation

```
devtools::install_github("jameswcraig/neurogen")
```

## Usage

```
library(neuro_gen)

# Get Ty's brain...
nii_path <- system.file(package = "neurogen", "data", "Ty_brain.nii.gz")

# Create gif
brain_warp(file = nii_path,
  color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8))
)
```

<img src='vignettes/img/brain_warp.gif' align = 'center' height = '750'/>

