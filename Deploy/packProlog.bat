:	Nan.Common.Deployment
:	Nan.Common.Deployment/Prolog 1.2.0
:	Common Deployment Logic in Prolog
:	Copyright 2015-2017 Julio P. Di Egidio
:	<mailto:julio@diegidio.name>
:	<http://julio.diegidio.name/Projects/Nan.Common.Deployment/>
:	
:	This file is part of Nan.Common.Deployment.
:	
:	Nan.Common.Deployment is free software: you can redistribute it and/or modify
:	it under the terms of the GNU General Public License as published by
:	the Free Software Foundation, either version 3 of the License, or
:	(at your option) any later version.
:	
:	Nan.Common.Deployment is distributed in the hope that it will be useful,
:	but WITHOUT ANY WARRANTY; without even the implied warranty of
:	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:	GNU General Public License for more details.
:	
:	You should have received a copy of the GNU General Public License
:	along with Nan.Common.Deployment.  If not, see <http://www.gnu.org/licenses/>.

@rem Nan.Common.Deployment::packProlog.bat (1.2.0)
@rem Author: Julio P. Di Egidio (julio@diegidio.name)
@rem Usage: packProlog <pack_name>, <pro_path>, <pack_ver>
@rem NOTE: Specifically tailored for SWI-Prolog's pack system.
@rem Requires PowerShell 4.0 and .NET 4.5
@rem Requires the script zipDir.ps1

@echo off

set hasParams=1

if "%~1"=="" set hasParams=0
if "%~2"=="" set hasParams=0
if "%~3"=="" set hasParams=0

if "%hasParams%"=="0" (
	echo ERROR: packProlog: Not enough parameters provided.
	echo.
	echo Usage: packProlog ^<pack_name^> ^<pro_path^> ^<pack_ver^>
	echo.
	echo NOTE: Specifically tailored for SWI-Prolog's pack system.
	echo.
	pause
	exit /B 1
)

set packName=%~1
set proPath=%~2
set packVer=%~3

set infoDir=..
set codeDir=..\Code
set testDir=..\Tests
set workDir=.\.work
set targetFile=..\Publish\%packName%-%packVer%.zip

if exist "%workDir%" (
	rmdir /S /Q "%workDir%"
)

echo Copying info...

xcopy /Q "%infoDir%\COPYING" "%workDir%\"
xcopy /Q "%infoDir%\HISTORY.md" "%workDir%\"
xcopy /Q "%infoDir%\README.md" "%workDir%\"
xcopy /Q "%infoDir%\pack.pl" "%workDir%\"

echo Copying code...

xcopy /Q "%codeDir%\*.*" "%workDir%\prolog\%proPath%\"

echo Copying tests...

xcopy /Q "%testDir%\*.*" "%workDir%\test\"

echo Generating target...

if exist "%targetFile%" (
	del "%targetFile%"
)

if exist "%targetFile%" (
	echo ERROR updating target!
	
	pause
) else (
	PowerShell ^
		-ExecutionPolicy Bypass ^
		-NoLogo -NoProfile ^
		-File ".\zipDir.ps1" "%workDir%" "%targetFile%"
)

echo Cleaning up...

rmdir /S /Q "%workDir%"

echo Done.

pause
