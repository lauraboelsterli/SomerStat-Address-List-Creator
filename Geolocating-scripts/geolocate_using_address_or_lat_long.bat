@echo off

rem Change directory to where this file lives, optional unless calling from elsewhere
cd %0\..

rem Check if the R setup directory exists
if not exist "setup\R-4.4.0" (
    echo Error: R setup directory not found at "setup\R-4.4.0".
    echo Please ensure that R is installed and the path is correct.
    pause
    exit /b 1
)

rem Run geolocation_scripts.R with the specified arguments
"setup\R-4.4.0\bin\Rscript.exe" "scripts\geolocation_scripts.R" 

pause