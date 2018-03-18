# qualitative-analysis-setup
Initial formatting and reporting for qualitative analysis of NSF-Ethics research. 

## Files:
- qual-data-formatting.r: Formats the data based on each condition (exposure/no exposure) and phase (design, prototype, final). Provides frequency counts for each sub-category and outputs related csv files within the outputs directory.

The files below are ommited as they contain sensitive information:
- input/docBins.csv: Lookup table for each assignment and their corresponding phase (design, prototype, final).
- input/QualitativeCoding2018.csv: Qualitative coding raw data from 2016-2017 (does not include two SE sections from Fall 2017-18).
- output/ASSETS2019: Combined list of all exposure and no exposure groups with summary columns. Exposure and no exposure csvs are also provided within this directory.

## Summary Tables:
When running qual-data-formatting.r, there are a number of summary tables that are created:

- Phase Tag Summary: Total tags per phase per exposure and no exposure groups. These values are not unique per quote, the values may include duplicate quotes if they are categorized for multiple sub-populations

- Phase Team Summary: This table provides more detailed information for the Phase Tag Summary, providing counts of unique teams which mention accessibility based on the phase and exposure. This table can be used with the Phase Tag Summary to quickly identify outliers. 

- Tag Population: Frequency of tags for each of the subpopulations

- Tag Summary: Total tags by exposure and no exposure groups. This can be used to verify values from the Phase Tag Summary table.

- Target Market Summary: Total tags for each subpopulation within the 'target market' node label.

- Team Tag Summary: Total tags by exposure and no exposure groups. This can be used to verify values from the Phase Team Summary table.

- Temp: This useful table is located at the end of the qual-data-formatting.r file. Use it to generate the frequency of tags for each Team ID within a particular phase. This provides a finer view of the relationships between the Phase Tag Summary and Phase Team Summary tables. 