# Project Structure

## Root
The active pipeline scripts remain in the project root because several Bash and macro entry points still depend on root-relative behavior.

## Main folders
- `archive_obsolete_versions/`
  - old or exploratory files that are no longer part of the main workflow
- `config/`
  - shared settings and machine-readable manifests
- `docs/`
  - human-readable architecture, setup, and workflow guides
- `frontend/`
  - browser-facing scaffold and instruction desk
- `launchers/`
  - helper scripts for loading settings and profile reuse
- `cache/optimization_profiles/`
  - reusable optimization profile cache
- `Ilastik_model_neurites_sh-sy5y/`
  - ilastik project and related training assets

## Architectural split
- setup and preprocessing
- validation and optimization
- production analysis and downstream reporting

This split is now reflected in the frontend and in the cache structure.
