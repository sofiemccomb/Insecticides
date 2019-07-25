This folder provides the input and output data used and created by the DataProcessing.Rmd.

Input data includes the CDL, CoA, ERS, and NF folders,
	while all output data is stored in the df (dataframes) folder. 

For all data, more information for each is provided in the subsequent data folders,
	as well as in the DataProcessing.Rmd under each input data section and the related R scripts.

Input Data:
The CDL folder contains USDA National Agricultural Statistics Service (NASS) Cropland Data Layer (CDL) data.
The CoA folder contains USDA National Agricultural Statistics Service (NASS) Census of Agriculture (COA) data.
The ERS folder contains USDA Farm Resource Regions (Economic Service Research: ERS) data.
The NF folder contains FRAGSTATS analysis data derived from National Land Cover Database (NLCD) data.

Output Data:
The df folder contains dataframes that were cretaed from the DataProcessing.Rmd.
It includes datasets for analyzed input data that provides the needed information from each dataset,
	where each row is a unique county Federal Information Processing Standards (FIPS) code per year of analysis.
Furthermore it includes a dataframe called fulldata, which is the combined information from all of the datasets together.
Detailed metadata for each dataframe is available in the df folder. 