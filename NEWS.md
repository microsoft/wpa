# wpa 1.4.0

New functions, bug fixes, and performance improvements.

New functions:
- `import_to_fst()`
- `create_hist()`

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
