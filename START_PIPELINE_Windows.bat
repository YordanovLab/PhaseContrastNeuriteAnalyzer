@echo off
setlocal

set "ROOT=%~dp0"
set "APP=%ROOT%app.R"
set "PAGE=%ROOT%frontend\index.html"
cd /d "%ROOT%"

echo ============================================================
echo Neurite Analysis in Phase Contrast Images
echo ============================================================
echo.
echo Trying to start the browser app...
echo.

where Rscript >nul 2>nul
if not errorlevel 1 (
  echo Starting the app with Windows R...
  start "" "http://127.0.0.1:3838"
  Rscript app.R
  goto :end
)

where wsl >nul 2>nul
if not errorlevel 1 (
  echo Windows R was not found. Falling back to WSL R...
  start "" "http://127.0.0.1:3838"
  for /f "usebackq delims=" %%i in (`wsl wslpath -a "%ROOT%"`) do set "WSL_ROOT=%%i"
  wsl bash -lc "cd '$WSL_ROOT' 2>/dev/null || exit 1; Rscript app.R"
  goto :end
)

echo No R runtime was detected, so the launcher will open the static navigator instead.
if exist "%PAGE%" (
  start "" "%PAGE%"
) else (
  echo Could not find frontend\index.html.
)

:end
pause
