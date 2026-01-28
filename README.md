# Coralcraft: A 3D Functional-Structural Coral Community Model

<p align="center">
  <img src="images_gifs/AppendixF-NL-NHanimation.gif" alt="Coralcraft Growth Simulation" width="700px">
</p>

**Coralcraft** is a mechanistic agent-based simulation model built in **R** that combines functional components, such as photosynthesis and vulnerability to disturbance, with a dynamic representation of a set of distinct 3D coral colony morphologies growing and competing for space and light. 

The voxel-based architecture naturally mirrors the modular growth of real-world coral polyps, resulting from the iteration of the single unit: the polyp. The modelled ‘environment’ is bounded by the bottom ($z = 1$, seafloor) and top planes (e.g. $z = 100$ cm, water surface), which serve as user-specified limits of growth. The smallest spatial unit is a **$1\text{ cm}^3$ voxel** governed by rules of growth and light attenuation. A standard simulation consists of $1,000,000$ voxels (equivalent to $1\text{ m}^3$), though this is customisable. To eliminate edge effects, the sides of the environment are **‘wrapped’ in a torus**, effectively simulating a larger area with a heterogeneous coral community under homogeneous environmental conditions. The model operates on a discrete **one-week time step** (52 steps per year).

---

### Development History

This model was first published as:
* Cresswell, A. K., Thomson, D. P., Trevenen, E. J., & Renton, M. (2017, December). ***A functional-structural coral model.*** In Proceedings of the 22nd International Congress on Modelling and Simulation (pp. 1-7).
* Cresswell, A. K., Thomson, D. P., Haywood, M. D., & Renton, M. (2020). ***Frequent hydrodynamic disturbances decrease the morphological diversity and structural complexity of 3D simulated coral communities.*** Coral Reefs, 39(4), 1147-1161.

It has since been further developed by Daphne Oh, A. K. Cresswell, D. P. Thomson, and M. Renton to investigate structural complexity and shelter provision, resulting in:
* Oh, D., Cresswell, A. K., Thomson, D. P., & Renton, M. (2025). ***Do greater coral cover and morphological diversity increase habitat complexity?*** Coral Reefs, 44(1), 257–272.
* Oh, D., A. K. Cresswell, D. P. Thomson, and M. Renton. (2025). ***Morphological composition influences redundancy, complementarity and ecological relevance of habitat complexity metrics in simulated coral communities.*** Ecology and Evolution 15:e72077.

---

### Key Features & Ecological Processes

Major processes—growth, recruitment, and mortality—occur at one of three hierarchical levels: **voxels, coral colonies, or the coral community.**

#### 1. Growth & Recruitment
* **Recruitment:** Represented as the colonisation of a randomly selected single voxel on the seafloor plane ($z = 1$). A voxel is successfully colonised only if it is empty and receives sufficient light.
* **Growth:** Occurs at the voxel level via the occupation of the **3D von Neumann neighbourhood** (the 6 immediately adjacent cells). While all morphologies have equal growth potential, certain shapes (e.g. branching or tabular) can occupy space and shade competitors more rapidly due to their geometry.



#### 2. Light & Shading
Light attenuation is modelled using the **Beer–Lambert–Bouguer law**, simulating a vertical gradient. Coral voxels absorb 100% of the light that reaches them, creating shading that directly influences the survival and growth of colonies beneath them.

#### 3. Mortality Mechanisms
* **Shading Mortality:** Individual voxels die if they fail to absorb enough light to maintain themselves, leading to **partial colony mortality**.
* **Background Mortality:** Represents chronic factors like predation or sand scouring; the probability is typically set at 0.1% per time step (0.5% for encrusting corals).
* **Hydrodynamic Disturbance:** Uses the **Colony Shape Factor (CSF)** to determine vulnerability. Corals with larger profile areas and smaller basal attachment areas are more likely to be removed during high-energy events.

---

### Community Metrics
The code reflects the most recent 2025 investigations and allows for the simulation of up to **10 different coral morphologies**:
*Encrusting, hemispherical, digitate, corymbose, tabular, mushroom, columnar, foliose, bushy, and branching.*

<p align="center">
  <img src="images_gifs/10_coralcraft_shapes_figure.png" alt="10 Coralcraft Morphologies" width="700px">
</p>

The model calculates a wide range of community metrics:
* **Abundance:** Colony density (per $\text{m}^2$), percentage cover, and colony volume ($\text{cm}^3$).
* **Diversity:** Community diversity and morphological composition.
* **Habitat complexity:** Linear and surface rugosity, fractal dimension, and shelter volume.
* **Ecology:** Shelter availability for demersal/pelagic predators and four variations of size-dependent predator/prey shelter.

---
*Refer to `0_model` and further `README.md` files for implementation details.*
