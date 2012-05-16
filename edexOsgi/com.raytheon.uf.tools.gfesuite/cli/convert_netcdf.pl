#!/usr/bin/perl
################################################################################
#                                                                              #
# Program name:  convert_netcdf.pl                                             #
# Version:  1.1                                                                #
# Language (Perl, C-shell, etc.): perl                                         #
#                                                                              #
# Authors:  Don Britton (TFX)                                                  #
# Contributers: Virgil Middendorf, David Tomalak                               #
#                                                                              #
# Date of last revision:  07/17/08                                             #
#                                                                              #
# Script description: This script optimizes the netcdf file created by GFE.    #
#                     ifpnetCDF stores most of the data as floating point      #
#                     values. This script creates an optimized netcdf file     #
#                     where most of the data is stored as shorts and bytes.    #
#                     This makes the netcdf file much smaller and improves     #
#                     performance on the web farms.                            #
#                                                                              #
# Directory program runs from:  /awips/adapt/ifps/localbin                     #
#                                                                              #
# Revision History:                                                            #
# 12/19/07:  Done created Script. vtm                                          #
# 07/17/08:  Removed VentRate and MixHgt from the short list because they are  #
#            too large to store in a short variable, thus a digit is being     #
#            truncated at the web farm. vtm                                    #
# 07/17/08:  Added my standard comment block. vtm                              #
################################################################################

########################
# paths to ncdump and ncgen applications
########################
$ncdump = "/usr/local/netcdf/bin/ncdump";
$ncgen = "/usr/local/netcdf/bin/ncgen";

########################
# list of fields to shorts (integers) and bytes (0-255)
########################
@shorts = qw (MaxT MinT T Td WindChill HeatIndex SnowLevel Wind_Dir Wind20ft_Dir ClearIndex FreeWind_Dir TransWind_Dir Topo Swell_Dir DiffFromOfficialMaxT DiffFromOfficialMinT ClimoPoPDiff DiffFromOfficialWind RHtrend);
@bytes = qw (MaxRH MinRH RH Pop Wind_Mag Wind20ft_Mag Sky Wind_Gust FreeWind_Mag LAL Haines TransWind_Mag WindWaveHeight WaveHeight SurfHeight Swell_Mag IceCoverage StormTotalSnow HoursOfSun DSI Period2 Wetflag);


########################
# grab site id from command line
########################
if (@ARGV)
{
	$ifile = $ARGV[0];
	$ofile = $ARGV[1];
}
else
{
	die "requires a 3-letter site id argument\n";
}

########################
# create filenames
########################
# input
$cdlin_filename = $ifile . ".cdl";
$cdlout_filename = $ofile . ".opt.cdl";
# output
$cdfin_filename = $ifile;
$cdfout_filename = $ofile;

# provide some output for log file
print "ncdump...";
system "date";

########################
# ncdump netcdf file to a plain text cdl file for parsing
########################
system "$ncdump $cdfin_filename > $cdlin_filename";

# check return code of system call for success
if ($? != 0)
{
	die "ERROR: cdl file unable to be created!\n";
}
########################

# provide some output for log file
print "parsing cdl file...";
system "date";

########################
# if the cdl was successfully created, 
# open it and begin parsing
########################
if (open IFILE, $cdlin_filename)
{
        ########################
        # write out optimized cdl file
        ########################
        open OFILE, ">$cdlout_filename" or die "cannot open $cdlout_filename for output!\n";

	while (<IFILE>)
	{

		########################
		# iterate thru first 2000 lines, 
		# looking only for netcdf header information
		########################
		if ($counter < 2000) {
                	$counter++;

			########################
			# iterate thru the list of fields
			# to be converted to shorts
			########################
			for $var (@shorts)
			{
				# float field_name
				if (/float $var/)
				{
					# replace "float" with "short"
					s/float/short/;
				}
			}
		
			########################
			# iterate thru the list of fields
			# to be converted to bytes
			########################
			for $var (@bytes)
			{
				# float field_name
				if (/float $var/)
				{
					# replace "float" with "byte"
					s/float/byte/;
				}
			}

		}
		print OFILE $_;	
	}
	
	# close both files
	close IFILE;
	close OFILE;
	
	# provide some output for a log file
	printf "ncgen file...: %s\n", $cdfout_filename;
	system "date";


	########################
	# ncgen cdl file to create the optimized netcdf file
	########################
	system "$ncgen -o $cdfout_filename $cdlout_filename";

	# check return code of system call for success
	if ($? != 0)
	{
		die "ERROR: optimized netcdf file unable to be created!\n";
	}
	########################
}

# provide some output for log file
system "date";
