# Interactive Help Page Sync Policy

The file:
- `frontend/index.html`

is no longer just an early scaffold. It is the companion interactive help page for the functional browser app in:
- `app.R`

## Required synchronization rule
Whenever the functional app changes in a way that affects user understanding, the help page should be updated too.

This includes:
- workflow logic
- step order
- profile reuse logic
- names of active scripts
- runtime guidance
- manual checkpoints
- output interpretation modules

## Practical meaning
- `app.R` is the runnable control surface.
- `frontend/index.html` is the persistent explainer and orientation surface.
- They should describe the same workflow, not two divergent versions of it.

## What to update together
When changing the browser app, check and update together:
- `app.R`
- `frontend/index.html`
- `frontend/app.js`
- `docs/PIPELINE_OVERVIEW.md`
- `config/pipeline_manifest.json`

## Why this matters
Beginner users may arrive through the help page first, or through the app first.
If those two surfaces drift apart, the project becomes much harder to trust and use correctly.
