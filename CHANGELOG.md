# CHANGELOG

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Markdown overlays functionality with new `markdown-overlays.el` module
- License header to markdown-overlays.el
- Extended commentary for markdown-overlays.el
- Sponsor link support

### Changed
- Renamed `markdown-overlay.el` to `markdown-overlays.el`
- Improved function naming and organization in markdown overlays
- Moved defcustom declarations to top of file
- Fallback to home directory when needed (fixes chatgpt-shell issue #326)

### Fixed
- Markdown overlays rendering issues
- Divider rendering in markdown overlays

### Removed
- Shell-maker traces from markdown-overlay.el
- Duplicate functions in markdown overlays

## [0.76.2] - 2024-12-26

### Changed
- Made auto scroll behavior smarter

## [0.76.1] - 2024-12-26

### Changed
- Removed auto jump to new prompt after output

### Removed
- Deprecated `shell-maker--eval-input-on-buffer-v1` function

## [0.75.1] - 2024-12-04

### Added
- Proxy support for `shell-maker-make-http-request`

## [0.74.1] - 2024-12-03

### Changed
- Now notifies on-command-finished observer when `:validate-command` fails

## [0.73.1] - 2024-12-02

### Fixed
- Curl version check (issue #2)

## [0.72.1] - 2024-11-28

### Fixed
- Don't call `comint-clear-buffer` unless there's an existing process

## [0.70.2] - 2024-11-24

### Added
- Ability to append history to current buffer

### Fixed
- Generated temp file handling when `temporary-file-directory` is set using `~/` (issue #1)

## [0.69.1] - 2024-11-21

### Added
- Auto-scroll functionality (unless point isn't at end of buffer)

### Changed
- Updated project URL

### Removed
- `closurep` dependency (issue #248)

## [0.68.1] - 2024-11-19

### Added
- Shell-agnostic `shell-maker-restore-session-from-transcript` function

## [0.67.1] - 2024-11-18

### Fixed
- False positive format strings

## [0.66.1] - 2024-11-18

### Added
- Ability to override mode line name

### Removed
- Commented out code

## [0.64.1] - 2024-11-18

### Added
- `shell-maker--split-text` function
- Alias to clear buffer

### Changed
- Adjusted number of newlines inserted after command output
- Combined process logs and output to buffer when process finishes
- Updated README documentation

## [0.63.1] - 2024-11-04

### Added
- Form fields support for HTTP requests
- Return process for async commands

### Changed
- Ensured alist return type for both sync and async commands
- Separated stdout from stderr in sync commands

### Fixed
- Brought back `:redact-log-output` when executing commands within shell

## [0.62.1] - 2024-11-01

### Added
- Demos and documentation improvements
- Stderr output collection

### Changed
- Removed need for shell config to execute commands in process
- Fixed demo link paths and widths
- Code reformatting and indentation fixes

## [0.61.1] - 2024-10-30

### Added
- LICENSE file
- Initial sync of shell-maker.el from chatgpt-shell repository

### Fixed
- Various melpazoid warnings
