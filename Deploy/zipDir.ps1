<#	Nan.Windows.Scripts
	Windows PowerShell Scripts.
	Copyright 2016 Julio P. Di Egidio
	<mailto:julio@diegidio.name>
	<http://julio.diegidio.name/Projects/Nan.Windows.Scripts/>
	
	This file is part of Nan.Windows.Scripts.
	
	Nan.Windows.Scripts is in the Public Domain.
	
	Nan.Windows.Scripts is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#>

# Usage: zipDir.ps1 <sourceDir> <targetZipFile>
# Adapted from http://stackoverflow.com/a/27289116
# Requires PowerShell 4.0 and .NET 4.5

$peTypeSource = @'
	using System.Text;
	namespace Nan.Windows.Scripts
	{
		public class PathEncoder : UTF8Encoding
		{
			public PathEncoder() {}
			public override byte[] GetBytes(string s)
			{
				s = s.Replace("\\", "/");
				return base.GetBytes(s);
		   }
		}
	}
'@

Add-Type -AssemblyName 'System'
Add-Type -AssemblyName 'System.IO.Compression'
Add-Type -AssemblyName 'System.IO.Compression.FileSystem'
Add-Type -TypeDefinition $peTypeSource

$zfType = [System.IO.Compression.ZipFile]
$clType = [System.IO.Compression.CompressionLevel]
$peType = [Nan.Windows.Scripts.PathEncoder]

$cLev = $clType::Optimal
$pEnc = New-Object $peType

$zfType::CreateFromDirectory($Args[0], $Args[1], $cLev, $false, $pEnc)
