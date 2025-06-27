@echo off

rem Download Install File if it doesn't exist
IF EXIST "R-4.4.0.exe" (
  echo The installer file R-4.4.0.exe already exists! No need to download again!
  echo:
) ELSE (
  echo Downloading R install file from interwebs!
  powershell -Command "Invoke-WebRequest https://cran.r-project.org/bin/windows/base/old/4.4.0/R-4.4.0-win.exe -OutFile R-4.4.0.exe"
  echo R installation file has been acquired!
  echo:
)

rem Install to Folder
if exist .\R-4.4.0\ (
  echo R looks like it already exists! If you want to re-install, delete the R-4.4.0 folder and re-run this script!
  echo:
) else (
  echo Installing R!
  R-4.4.0.exe /SILENT /DIR=".\R-4.4.0"
  echo:
  echo R has been installed!
  echo:
)

pause