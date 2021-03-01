# wpa 1.4.0

v1.4.0 is the version where the **working patterns** family of functions are released open source as part of **wpa** (#55). These functions include:

- `workpatterns_classify()`
- `flex_index()`
- `workpatterns_report()`
- `plot_flex_index()`

In addition to the above, there are also some new functions, bug fixes, documentation, and performance improvements.

New functions:
- `import_to_fst()` (#86)
- `create_hist()` (#80)
- `identify_shifts_wp()` (#66)

Significant changes to existing functions:

- `collaboration_report()`: visuals and structure are improved (#69)
- `import_wpa()` can now specify encoding when reading in data, which now defaults to `UTF-8` (#76). This resolves a previous bug for reading in double-byte characters.
- `network_g2g()`: now auto-detects columns for collaborators and time investors. Also, if the value `'Collaborators Within Group'` does not exist in the data, a warning message is returned.

Bug fixes:

- `collab_area()` now has relaxed variable name checks which makes it less prone to fail when not all exact collaboration metrics are available or when cases do not match (#81).
- Resolved an issue of overly strict checks on `flag_outlooktime()`. Now it is possible to pass both `7:30` and `07:30` as inputs.

Documentation changes have been implemented across the board to comply with CRAN and to improve the user experience (#31)

# wpa 1.3.1

New functions, bug fixes, and performance improvements.

Significant changes to existing functions:
- New plot visual is available for `keymetrics_scan()`
- `combine_signals()` can now dynamically accept any metrics available in the Hourly Collaboration query.
- `pairwise_count()` now uses a **data.table** implementation, instead of dependent on **widyr**.

New functions:
- `network_p2p()`
- `network_leiden()`
- `network_louvain()`
- `network_describe()`
- `create_sankey()`
- `totals_col()`

Some package dependencies have been removed (see #36):
- **network**
- **GGally**
- **widyr**

# wpa 1.3.0

This is the first version of the **wpa** package to be released open-source on GitHub. If you have been using a previous developmental version, the main difference is that this release omits the more experimental _working patterns_ family of functions. The experimental functions are currently available upon request via mac@microsoft.com.
