# Coralcraft
A 3D mechanistic simulation model of a coral community growth.

Coralcraft combines functional components, such as photosynthesis, with a dynamic representation of a set of distinct 3D coral colony morphologies growing and competing for space and light in a 3D space. The modelled ‘environment’ is bounded by the bottom (z = 1, seafloor) and top planes (z = 100, water surface), which are the limits of growth. The smallest spatial unit in Coralcraft is a 1 cm3 ‘voxel’ that is governed by the rules of coral growth and light attenuation (as detailed in Cresswell et al. 2020), with the modelled environment consisting of 1,000,000 voxels, equivalent to a volume of 1 m3. The sides of the environment are ‘wrapped’ in a torus to eliminate edge effects. This effectively simulates a large area with a heterogenous coral community under homogeneous environmental conditions. The model operates on a discrete time step which we defined as one-week, i.e. 52 time steps per year.

This model was first published as Cresswell, A. K., Thomson, D. P., Trevenen, E. J., & Renton, M. (2017, December). A functional-structural coral model. In Proceedings of the 22nd International Congress on Modelling and Simulation (pp. 1-7). and then as
Cresswell, A. K., Thomson, D. P., Haywood, M. D., & Renton, M. (2020). Frequent hydrodynamic disturbances decrease the morphological diversity and structural complexity of 3D simulated coral communities. Coral Reefs, 39(4), 1147-1161.

It has since been further developed by Daphne Oh, Cresswell, A. K., Thomson, D. P. & Renton, M. to investigate structural complexity and shelter provision in different coral communities with a study published as 
Oh, D., Cresswell, A. K., Thomson, D. P., & Renton, M. (2025). Do greater coral cover and morphological diversity increase habitat complexity? Coral Reefs, 44(1), 257–272. 
The code presented here reflects this most recent investigation as at December 2024 and allows simulation of communities composed of up to 10 different coral morpholohies (encrusting, hemispherical, digitate, corymbose, tabular, mushroom, columnar, foliose, bushy and branching) alongside the calculation of a range of different community metrics: coral colony density (per m2), percentage cover, colony volume (cm3), community diversity, linear rugosity, surface rugosity, fractal dimension, shelter volume, shelter from demersal and pelagic predators, and four variations of predator/prey size-dependent shelter. 

Refer to 0_model and further README.md information.

