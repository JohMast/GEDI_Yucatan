Name:
	Analysis_Data/
Date:
	06.01.2020
Description:
	Data for the study: Space Lidar for Archaeology? Reanalyzing GEDI Data for Detection of Ancient Maya Buildings.
	Quality and biophyiscal information for 67 pairs of footprints. Pairs consist of one footprint which is intersecting a structure and a similar neighbor which 	is not intersecting a structure.
Project Contributors:
	Žiga Kokalj, Research Centre of the Slovenian Academy of Sciences and Arts
	Johannes Mast, University of Würzburg
Contents:
	- GEDI_Yucatan_Analysis_Points: Footprints from the AOI which have been grouped in pairs and used for analysis.
	- GEDI_Yucatan_AOI_Points: Footprints from the entire AOI which are used as reference.
Fields:
	- Beam: Beam designation.
	- src_fil: Filename of the source hdf5 file.
	- int_build: Built status
		- 2 Intersecting a structure
		- 1 Partly intersecting a structure
		- 0 Not intersecting a structure
	- beam_pair: 
		- 0  Not tagged as part of a pair
		- >=1  Part of a pair, the number corresponding to the ID of the pair
	- is_bajo: 
		- 0 Not in bajo
		- 1 Partially or completely in bajo
Further Fields, layers of the GEDI L2B product. See the GEDI L2B dictionary for a detailed description.
	l2a_quality_flag1
	l2b_quality_flag1
	PAI
	cover
	FHD
	pai_z1
	pai_z2
	pai_z3
	pai_z4
	pai_z5
	pai_z6
	pai_z7
	pai_z8
	pavd_z1
	pavd_z2
	pavd_z3
	pavd_z4
	pavd_z5
	pavd_z6
	pavd_z7
	pavd_z8
	cover_z1
	cover_z2
	cover_z3
	cover_z4
	cover_z5
	cover_z6
	cover_z7
	cover_z8
			