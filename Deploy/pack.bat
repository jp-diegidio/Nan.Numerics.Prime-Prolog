:  	Nan.Numerics.Prime
:  	A simple prime number library
:  	Copyright 2016 Julio P. Di Egidio
:  	<mailto:julio@diegidio.name>
:  	<http://julio.diegidio.name/Projects/Nan.Numerics.Prime/>
:  	
:  	This file is part of Nan.Numerics.Prime.
:  	
:  	Nan.Numerics.Prime is free software: you can redistribute it and/or modify
:  	it under the terms of the GNU General Public License as published by
:  	the Free Software Foundation, either version 3 of the License, or
:  	(at your option) any later version.
:  	
:  	Nan.Numerics.Prime is distributed in the hope that it will be useful,
:  	but WITHOUT ANY WARRANTY; without even the implied warranty of
:  	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:  	GNU General Public License for more details.
:  	
:  	You should have received a copy of the GNU General Public License
:  	along with Nan.Numerics.Prime.  If not, see <http://www.gnu.org/licenses/>.

@rem Nan.Numerics.Prime::pack.bat (1.2.1-beta)
@rem Author: Julio P. Di Egidio (julio@diegidio.name)
@rem Requires PowerShell 4.0 and .NET 4.5

@echo off

set name=nan_numerics_prime
set /P ver=Version ? 

set infoDir=..
set codeDir=..\Code
set testDir=..\Tests
set workDir=.\.work
set targetFile=..\Publish\%name%-%ver%.zip

if exist "%workDir%" (
	rmdir /S /Q "%workDir%"
)

echo Copying info...

xcopy /Q "%infoDir%\COPYING" "%workDir%\"
xcopy /Q "%infoDir%\README" "%workDir%\"
xcopy /Q "%infoDir%\pack.pl" "%workDir%\"

echo Copying prolog...

xcopy /Q "%codeDir%\*.*" "%workDir%\prolog\"

echo Copying test...

xcopy /Q "%testDir%\*.*" "%workDir%\test\"

echo Generating target...

if exist "%targetFile%" (
	del "%targetFile%"
)

if exist "%targetFile%" (
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

rem pause
