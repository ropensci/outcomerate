## Test environments

* local macOS 26.6.2 (arm64), R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

Both the full local check (including the PDF manual) and the network-enabled
`R CMD check --as-cran --no-manual` check completed with `Status: OK`.

## Submission notes

This maintenance release resolves the current CRAN check notes by replacing
old-style citation calls, deprecated test attribute names, and unescaped Rd
braces. It also updates moved or unavailable URLs reported by CRAN incoming
checks.
