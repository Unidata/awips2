#!/usr/bin/perl

use strict;
use DBI;
use AppConfig qw(:expand :argcount);


#Set/define command line args
my %cfg = ( DEBUG => 0); # debug mode on or off
my $config = AppConfig->new(\%cfg);	# create config object
$config->define('type',{ARGCOUNT => ARGCOUNT_ONE, VALIDATE => '(WFO|RFC|HQ|wfo|rfc|hq)', ALIAS => 'T'});
$config->define('local-control-file',{ARGCOUNT => ARGCOUNT_ONE, ALIAS => 'L',DEFAULT => 0});
$config->define('upload',{ARGCOUNT => ARGCOUNT_NONE, ALIAS => 'U', DEFAULT => 0});
$config->define('wfo-id',{ARGCOUNT => ARGCOUNT_ONE, ALIAS => 'W', DEFAULT => 0});
$config->define('rfc-id',{ARGCOUNT => ARGCOUNT_ONE, ALIAS => 'R', DEFAULT => 0});
$config->define('out-xmlfile',{ARGCOUNT => ARGCOUNT_ONE, ALIAS => 'O', DEFAULT => 0});
$config->define('input-xmlfile',{ARGCOUNT => ARGCOUNT_ONE, ALIAS => 'I', DEFAULT => 0});
$config->define('check',{ARGCOUNT => ARGCOUNT_NONE, ALIAS => 'C', DEFAULT => 0});
$config->define('verbose',{ARGCOUNT => ARGCOUNT_NONE, ALIAS => 'V', DEFAULT => 0});
$config->define('dbname',{ARGCOUNT => ARGCOUNT_ONE, ALIAS => 'D', DEFAULT => 0});
$config->define('extract',{ARGCOUNT => ARGCOUNT_NONE, ALIAS => 'E', DEFAULT => 0});
$config->define('delete',{ARGCOUNT => ARGCOUNT_NONE, ALIAS => 'A', DEFAULT => 0});
$config->getopt(\@ARGV);

our $type = uc($config->get('type'));
our $localControlFile = $config->get('local-control-file');
our $Upload = $config->get('upload');
our $wfoID = uc($config->get('wfo-id'));
our $rfcID = uc($config->get('rfc-id'));
our $outFile = $config->get('out-xmlfile');
our $inFile = $config->get('input-xmlfile');
our $check = $config->get('check');
our $verbose = $config->get('verbose');
our $dbname_flag = $config->get('dbname');
our $extract = $config->get('extract');
our $delete = $config->get('delete');
our $office;
our $update_count = 0;
our $insert_count = 0;
our $error_count = 0;
our $total_count = 0;
our $file_name;
our $conf_dir;
my ($dbname, $host, $user, $pass, $nrldb_host, $backup_host);
my @delete_list;
my $delete_listRef;
print "db name flag: $dbname_flag\n";
if($check) {
	warn "-----Starting NRLDB installation check-----\nInstallation Complete.\n";
	print "Installation Complete.\n";
	exit 0;
}


#Get config file info
($dbname, $host, $user, $pass, $nrldb_host, $office, $backup_host) = read_config_file();

if(!$dbname_flag) 
{
  if( -e "/awips/hydroapps/public/bin/get_apps_defaults") 
  {
  	$dbname = `/awips/hydroapps/public/bin/get_apps_defaults.LX db_name`;
  }
}
else{
  $dbname = $dbname_flag;
}
# Do parameter checks
if($type eq "") 
{
	print "No office type specified.\nusage: --type WFO|RFC|HQ\n\n";
	exit 1;
}
if($type eq "HQ") 
{
	if($inFile eq 0) 
	{
		print "No xml input file specified.\nusage: --type HQ --input-xmlfile 'file'\n\n";
		exit 1;
	}
	if($rfcID eq 0 && $wfoID eq 0) 	
	{
		print "You must specify a WFO/RFC office identifier with the HQ type.\n";
		exit 1;
	}
	
	unless($rfcID eq 0) {
		$office = $rfcID; 
	}
	unless($wfoID eq 0) {
		$office = $wfoID; 
	}

}

if($type eq "RFC") 
{
	if($rfcID eq 0) 
	{
		print "You must specify an RFC office identifier with the rfc option.\nusage: --type RFC --rfc-id IDRFC\n\n";
		exit 1;
	}
}


#Connect to database
our $db = db_connect($dbname, $host, $user, $pass);

my $date = getdate();
print "---Starting NRLDB process at $office\, running as $type\---\n---$date\n\n" if($verbose);
warn "---Starting NRLDB process at $office\, running as $type\---\n---$date\n\n";
print "Connected to database: $dbname\n" if($verbose);
warn  "Connected to database: $dbname\n";
#Determine what type of office is running nrldb software
if(($type eq "WFO") | ($type eq "RFC")) 
{
	if($localControlFile eq 0) 
	{
		download_control_file($type);
	}
	create_xml();
	if($Upload) 
	{
                upload_xml($nrldb_host);
                upload_xml($backup_host);
	}
} 
elsif($type eq "HQ") 
{
	if($delete)
	{
		$delete_listRef = get_delete_list();
		@delete_list = @$delete_listRef;
		foreach my $delete_table (@delete_list)
		{
			deleteValues($delete_table);
		}
	}
	xml_parse();
}

print "\n-----------------------------\n\n" if($verbose);
warn "\n-----------------------------\n\n";
exit 0;


# sub 'create_xml' is responsible for querying the database and putting the info into xml format.
sub create_xml
{

my $table_name;
my ($select_string, $field_string);
my $xml_string;
my $record_count;
my ($st, $at);
my $table_query;
my $query_error_flag;
my $numrows;
my $lid_flag;
my $pkey;
my ($pk_name, $field_name);
my $row;
my $extract_detail;
my %infohash;
my @tables;
my @fields;
my @fields_all;
my @select_array;
my @PK;
my @keys;
my (@pk_output, @fields_output);

#read control file and put specified fields into array
my ($tables_ref, $fields_all_ref) = read_control_file();
@tables = @$tables_ref;
@fields_all = @$fields_all_ref;

	$extract_detail = '';
#	print "EXTRACT: $extract\n";
	unless($extract eq 0) 
	{
		$extract_detail = extract_detail();
	}

# Start creating xml
$xml_string = "<?xml version=\"1.0\"?>\n<NRLDB>\n";
foreach $table_name (@tables)
{

	print "TABLE: $table_name\n" if($verbose);
	warn "TABLE: $table_name\n";
	$select_string = "";
	$lid_flag = 1;
	# Get primary key list for specified tables
	@keys = $db->primary_key(undef, undef, $table_name);

	foreach $pkey (@keys)
	{
		# The following 6 lines were by mark Armstrong (HSD) on 2/26/09
		# to remove the quotes from primary keys.
		# When primary keys occurred with quotes, the update queries
		# were not successful.
		if ($pkey =~ /"/){
			my $length_pkey = length $pkey;
			$length_pkey -= 2;
			my $new_pkey = substr($pkey,1,$length_pkey);
			$pkey=$new_pkey;
		}
		push(@PK, "$table_name.$pkey");
	}

	@pk_output = grep(/$table_name\.\w*/, @PK);
	print "\tPK: @pk_output\n" if($verbose);
	warn "\tPK: @pk_output\n";
	@fields_output = grep(/$table_name\.\w*/, @fields_all);
	print "\tFIELDS: @fields_output\n" if($verbose);
	warn "\tFIELDS: @fields_output\n";

	my $pk_count = @pk_output;
	if($pk_count == 0)
	{
		print "No Primary Keys found for Table: $table_name\nContinuing\n\n" if($verbose);
		warn "No Primary Keys found for Table: $table_name\nContinuing\n\n";
		next;
	}

	#loop through arrays and put together a select string for specified table
	foreach my $pk (@pk_output)
	{
		if($pk =~ /$table_name\.\w*/)
		{
			if($select_string eq "")
			{
				$select_string = "$pk";
			}
			else
			{
				$select_string .= ",$pk";
			}
		}
	}


	foreach my $fields (@fields_output)
	{
		if($select_string =~ /.*$fields.*/)
		{
			if($field_string eq "")
			{
				$field_string = "$fields";
			}
			else
			{
				$field_string .= ",$fields";
			}
			next;
		}
		elsif($fields =~ /.*ALL.*/)
		{
			$select_string = "*";
			last;
		}
		else
		{
			if($field_string eq "")
			{
				$field_string = "$fields";
			}
			else
			{
				$field_string .= ",$fields";
			}
				$select_string .= ",$fields";
		}
	}


	#print select string to be used
	print "\n" if($verbose);
	warn "\n";
	$query_error_flag = 0;
	#if select string equal 'ALL' get a list of all fields in specified table by querying database info tables.
	if($select_string eq "*")
	{

		my $query_column1 = "SELECT c.oid
		FROM pg_catalog.pg_class c
		LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
		WHERE pg_catalog.pg_table_is_visible(c.oid)
		AND c.relname ~ '^$table_name\$'";

		my $attribute_query = "SELECT a.attname
		FROM pg_catalog.pg_attribute a
		WHERE a.attnum > 0 AND NOT a.attisdropped
		AND a.attrelid = ($query_column1)
		ORDER BY a.attnum;";

		eval
		{
			$at = $db->prepare($attribute_query);
			$at->execute() or die "Cannot execute: ".$at->errstr();
		};
		if($@)
		{print "$@\n" if($verbose); warn "$@\n";}

		my $att_count = 0;
		while ( defined ( my $attribues = $at->fetchrow_arrayref() ) )
		{
			if($att_count > 0)
			{
			    $select_string .= ",$table_name.@$attribues[0]";
			}
			else
			{
			    $select_string = "$table_name.@$attribues[0]";
			}
			$att_count++;
		}
		$field_string = $select_string;
	}

	#Check for lid in table
	if($select_string !~ /$table_name\.lid/) 
	{
		$lid_flag = lid_check($table_name);
	}

	# Determine query depending on office type and other parameters
	## Revised query to properly select only counties from primary HSA or identified WFO - Ernie Wells February 09  ##
	if($type eq "WFO") 
	{
		if($wfoID eq 0) {
			if($table_name =~ /location/) 
			{
			  $table_query = "SELECT $select_string FROM location, admin WHERE location.hsa = admin.hsa $extract_detail ORDER BY lid;";
			} elsif($table_name =~ /counties/) {
                          $table_query = "SELECT $select_string FROM counties, admin WHERE counties.wfo = admin.hsa;";
                        } elsif($table_name =~ /rpffcstgroup/) {
                          $table_query = "SELECT distinct $select_string from rpffcstgroup join rpffcstpoint rp on rp.group_id = rpffcstgroup.group_id join location l on l.lid = rp.lid join admin on l.hsa = admin.hsa;";
                        } elsif($table_name =~ /vtecevent/) {
                          $table_query = "SELECT $select_string FROM vtecevent WHERE vtecevent.geoid in (select location.lid from location, admin where location.hsa = admin.hsa) $extract_detail;";
                        } elsif($table_name eq "height" || $table_name =~ /temperature/ || $table_name =~ /curpp/ || $table_name =~ /curpc/ || $table_name eq "discharge"){
                                my $cutoff_dtime = getcutoffdate();
                                $table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location, admin WHERE location.lid = $table_name.lid AND location.hsa = admin.hsa) and obstime > '$cutoff_dtime' $extract_detail ORDER BY lid;";
                        } elsif($table_name =~ /fcstheight/ || $table_name =~ /fcstdischarge/) {
                                my $cutoff_dtime = getcutoffdate();
                                        $table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location, admin WHERE location.lid = $table_name.lid AND location.hsa = admin.hsa) and basistime > '$cutoff_dtime' $extract_detail ORDER BY lid;";
			} elsif($lid_flag == 1){
			  $table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location, admin WHERE location.lid = $table_name.lid AND location.hsa = admin.hsa) $extract_detail ORDER BY lid;";
			} 
			else {
			  $table_query = "SELECT $select_string FROM $table_name\;";
			}
		} 
		else {
			if($table_name =~ /location/) 
			{
			  if($extract eq 0) { 				
			  	$table_query = "SELECT $select_string FROM location WHERE location.hsa = '$wfoID' $extract_detail ORDER BY lid;";
			  } else {
			  	$table_query = "SELECT $select_string FROM location WHERE location.hsa like '%' $extract_detail ORDER BY lid;";
			}
			} elsif($table_name =~ /counties/) {
			  if($extract eq 0) { 				
                          	$table_query = "SELECT $select_string FROM counties WHERE counties.wfo = '$wfoID';";
			  } else {
                          	$table_query = "SELECT $select_string FROM counties WHERE counties.wfo in (select hsa from location where hsa is not null $extract_detail) ;";
			  }
                        } elsif($table_name =~ /rpffcstgroup/) {
			  if($extract eq 0) { 				
                          	$table_query = "SELECT distinct $select_string from rpffcstgroup join rpffcstpoint rp on rp.group_id = rpffcstgroup.group_id join location l on l.lid = rp.lid where l.hsa = '$wfoID';";
			  } else {
				my $rpgroup_extract_detail = $extract_detail;
                                $rpgroup_extract_detail =~ s/lid/l.lid/g;
                          	$table_query = "SELECT distinct $select_string from rpffcstgroup join rpffcstpoint rp on rp.group_id = rpffcstgroup.group_id join location l on l.lid = rp.lid where l.hsa is not null $rpgroup_extract_detail;";
			  }	
                        } elsif($table_name =~ /vtecevent/) {
			  if($extract eq 0) { 				
                          	$table_query = "SELECT $select_string FROM vtecevent WHERE vtecevent.geoid in (select location.lid from location where location.hsa = '$wfoID') ;";
			  } else {
				my $vtec_extract_detail = $extract_detail;
				$vtec_extract_detail =~ s/lid/geoid/g;
				print "vtec_extract_detail: $vtec_extract_detail\n";
                          	$table_query = "SELECT $select_string FROM vtecevent WHERE vtecevent.geoid in (select location.lid from location where location.hsa is not null) $vtec_extract_detail;";
			  }
                        } elsif($table_name eq "height" || $table_name =~ /temperature/ || $table_name =~ /curpp/ || $table_name =~ /curpc/ || $table_name eq "discharge"){
                          my $cutoff_dtime = getcutoffdate();
			  if($extract eq 0) { 				
                                $table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location WHERE location.lid = $table_name.lid AND location.hsa = '$wfoID') and obstime > '$cutoff_dtime' ORDER BY lid;";
			  } else {
                                $table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location WHERE location.lid = $table_name.lid ) and obstime > '$cutoff_dtime' $extract_detail ORDER BY lid;";
			 }
                        } elsif($table_name =~ /fcstheight/ || $table_name =~ /fcstdischarge/) {
                          my $cutoff_dtime = getcutoffdate();
			  if($extract eq 0) { 				
                          	$table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location WHERE location.lid = $table_name.lid AND location.hsa = '$wfoID') and basistime > '$cutoff_dtime' ORDER BY lid;";
			  } else {
                          	$table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location WHERE location.lid = $table_name.lid) and basistime > '$cutoff_dtime' $extract_detail ORDER BY lid;";
			 }
			} elsif($lid_flag == 1) {
			  if($extract eq 0) { 				
			  	$table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location WHERE location.lid = $table_name.lid AND location.hsa = '$wfoID') $extract_detail ORDER BY lid;";
			  } else {
			  	$table_query = "SELECT $select_string FROM $table_name WHERE exists (SELECT lid FROM location WHERE location.lid = $table_name.lid) $extract_detail ORDER BY lid;";
			}
			} else {
			  	$table_query = "SELECT $select_string FROM $table_name\;";
			}
		}
	} elsif($type eq "RFC") {
		if($table_name =~ /location/) {	
			$table_query = "SELECT $select_string FROM location WHERE location.rfc='$rfcID' $extract_detail ORDER BY lid;";
		} elsif($lid_flag == 1) {
		  $table_query = "SELECT $select_string from $table_name where exists (select lid from location where
location.lid = $table_name.lid and location.rfc='$rfcID') $extract_detail ORDER BY lid;";
		 # $table_query = "SELECT $select_string from $table_name where exists (select lid from location where
#location.lid=rating.lid and location.rfc='$rfcID') $extract_detail ORDER BY lid;";
		} else	{
			$table_query = "SELECT $select_string FROM $table_name\;";
		}
	}

	# print the query for log purpose and execute the query
	print "$table_query\n\n" if($verbose);
	warn "$table_query\n\n";
	$record_count = 0;
	eval
	{
		$st = $db->prepare($table_query);
		$row = $db->selectall_arrayref($st,{Slice => {}});
		#$st->execute() or die "Cannot execute: ".$st->errstr();
	};
	if ($@)
	{
		print "$@\n" if($verbose);
		warn "$@\n";
		$xml_string .= "  <Table name=\"$table_name\"/>\n";
		$query_error_flag = 1;
	}

	# if no db error continue adding info to xml file for the table.
	if($query_error_flag == 0)
	{
		$numrows = $st->rows;
		print "Number of records obtained: $numrows\n" if($verbose);
		warn "Number of records obtained: $numrows\n";
		if ($numrows == 0)
		{
			$xml_string .= "  <Table name=\"$table_name\"/>\n";
		}
		else
		{
			$xml_string .= "  <Table name=\"$table_name\">\n";
		}

		foreach my $sref (@$row)
		{
			%infohash=%{$sref};
			#print record number to xml file
			$xml_string .= "    <Record record_num=\"$record_count\">\n      <PK>\n";

			#print primary key to xml file
			my $pk_count = 0;
			foreach my $pk (@pk_output)
			{
				if($pk =~ /$table_name\.(.*)/)
				{
					$pk_name=$1;
					#$infohash{$pk_name}=~ s/\r|\n//g;
					$xml_string .= "        <$pk>$infohash{$pk_name}</$pk>\n";
					$pk_count++;
				}
			}
			$xml_string .= "      </PK>\n      <Fields>\n";
			@select_array = split(/,/, $field_string);
			#start printing fields to xml file
			my $field_count = 0;
			foreach my $select (@select_array)
			{
				if($select =~ /.*$table_name\.(.*)/)
				{
					$field_name = $1;
					if($infohash{$field_name} !~/^\s*$/)
					{
					#$infohash{$field_name} =~ s/\r|\n//g;
					$xml_string .= "        <$select>$infohash{$field_name}</$select>\n";
					}
					else
					{
						$xml_string .= "        <$select/>\n";
					}
				$field_count++;
				}
			}
			$xml_string .="      </Fields>\n";
			$xml_string .="    </Record>\n";
			$record_count++;
		}

	}
	if($numrows != 0 && $query_error_flag == 0)
	{
		$xml_string .="  </Table>\n";
	}
	@select_array = ();
	$field_string = "";

	print "\n---------------\n" if($verbose);
	warn "\n---------------\n";

}
$xml_string .="</NRLDB>\n";

if ($type eq "WFO" && $wfoID eq 0)
{
	my $hsa_admin_query = "SELECT admin.hsa FROM admin;";
	my $st_admin;
	eval
	{
		$st_admin = $db->prepare($hsa_admin_query);
		$st_admin->execute() or die "Cannot execute: ".$st_admin->errstr();
	};
	if ($@)
	{
		print "$@\n" if($verbose);
		warn "$@\n";
	}
	while ( defined ( my $row = $st_admin->fetchrow_arrayref() ) )
	{
		$wfoID = @$row[0];
	}

}

if($type eq "WFO")
{
	$file_name = "$wfoID\_from-$office\_nrldb.xml";
}
elsif($type eq "RFC")
{
	$file_name = "$rfcID\_from-$office\_nrldb.xml";
}


#determine output file
if($outFile eq 0)
{
	$outFile = $file_name;
}

my $outDir;

if( -e "/awips/hydroapps/public/bin/get_apps_defaults"){
	$outDir = `/awips/hydroapps/public/bin/get_apps_defaults.LX nrldb_data`;

	chomp($outDir); 
} else {
	print "Could not access /awips/hydroapps/public/bin/get_apps_defaults.LX.  Exiting";
	exit -1;
}

$outFile = $outDir . "/" . $outFile; 
open(XMLFILE, ">$outFile") || die "Could not open $outFile for writing.\n$!\nExiting\n";
printf XMLFILE "$xml_string";
close(XMLFILE);

my $end = $db->disconnect;
zip_xml($outFile);
}

sub zip_xml
{
my $filename = shift;
my $zip_string;

	$zip_string = "zip -j $filename.zip $filename";
	print "$zip_string\n" if($verbose);
	warn "$zip_string\n";
	my $zip_exe = `$zip_string`;
	print "$zip_exe\n" if($verbose);
	warn "$zip_exe\n";
	print "Failed:  \"$zip_string\"\n" if ($? && $verbose);
	warn "Failed:  \"$zip_string\"\n" if $?;
}


sub read_control_file
{
my @fields_all;
my @tables;
my @fields;
my $table_name;
my $control_file;

if($localControlFile eq 0)
{
	if($type eq "WFO")
	{
		$control_file = "${conf_dir}/nrldb_control_wfo";
	}
	elsif($type eq "RFC")
	{
		$control_file = "${conf_dir}/nrldb_control_rfc";
	}
}
else
{
	$control_file = $localControlFile;
}
open(FILE, "$control_file") || die "Could not open control file: $control_file\n$!\nExiting\n";
my @infile = <FILE>;
close(FILE);

foreach my $line (@infile)
{
chomp($line);
	if($line =~ /^#.*$/)
	{
		next;
	}
	elsif($line =~ /\[(.*)\]/)
	{
		$table_name = $1;
		push (@tables, $table_name);
	}
	elsif($line =~ /^(fields)/)
	{
		$line =~ /fields = (.*)/;
		@fields = split(/,/, $1);

		foreach my $tmp_field (@fields)
		{
			$tmp_field =~ s/\s*//;
			push(@fields_all, "$table_name.$tmp_field");
		}
	}
}


return (\@tables, \@fields_all);
}

sub extract_detail()
{

my $wfo = $office;
my $wfo_fh_pointer = 0;
my $info_found = 0;
my ($ex_type, $ex_list);
my @extract_lid;
my $uclid;
my $compare_symbol;
my $extract_query = '';

open(FILE, "nrldb_extract") || die "Could not open detail extract file nrldb_extract:\n$!\nExiting\n";
my @infile = <FILE>;
close(FILE);

	foreach my $line (@infile)
	{
	  chomp($line);
	     if($line =~ m/type:\s*(\w*)/)
	     {$ex_type= $1;}
	     if($line =~ m/list:\s*(.*)/)
	     {
		$ex_list= $1;
	         if(defined($ex_type) && defined($ex_list))
	         {$info_found = 1;}
	     }
	
	   if($info_found eq 1)
	   {last;}
	}
	if($info_found eq 1)
	{
	  print "EXTRACT: $ex_type, [$ex_list]\n" if($verbose);
	  warn  "EXTRACT: $ex_type, [$ex_list]\n";
	  @extract_lid = split(/,/,$ex_list);
	  
	  if(lc($ex_type) eq 'only')
	  {$compare_symbol = '=';}
	  elsif(lc($ex_type) eq 'except')
	  {$compare_symbol = '!=';}
	  else
	  {
	  	  print "Undefined extraction type '$ex_type', should be only|except\n" if($verbose);
	  	  warn  "Undefined extraction type '$ex_type', should be only|except\n";
	  	  return($extract_query);
	  }
	  # The following has been modified by Mark Armstrong HSD
	  # Originally, the query for multiple lids using the "only" extract 
	  #  was incorrect.  It used the AND condition for each lid which 
	  #  would never be true.  I added another if condition and a new
 	  #  for loop to handle this case.		  
	  if(lc($ex_type) eq 'only'){
	  my $count = 0;
	  $extract_query=" AND (";
	  foreach my $lid (@extract_lid)
	  {
	  	if($lid eq '')
	  	{next;}
	  	
	  	$uclid=uc($lid);
		$uclid =~ s/\s*//g;
		if ( $count eq 0)
		{	   
			$extract_query .= " lid $compare_symbol '$uclid'";
		}     
		else
		{
  	  		$extract_query .= " OR lid $compare_symbol '$uclid'";
		}
		$count = $count + 1;
	  }
	  $extract_query .= ") ";
	}
	else{
          foreach my $lid (@extract_lid)
          {
                if($lid eq '')
                {next;}

                $uclid=uc($lid);
                $uclid =~ s/\s*//g;
                $extract_query .= " AND lid $compare_symbol '$uclid'";

          }
        }
	}
	return($extract_query);
}

sub read_config_file()
{

my $dbname;
my $host;
my $pass;
my $user;
my $nrldb_host;
my $site_conf;
my $backup_host;
my $conf_file;

if( -e "/awips/hydroapps/public/bin/get_apps_defaults")
{
	$conf_dir = `/awips/hydroapps/public/bin/get_apps_defaults.LX nrldb_config`;
	chomp($conf_dir);
	$conf_file = "${conf_dir}/nrldb.conf";
}
else
{
	print "nrldb_conf token not specified. Exiting";
	exit -1;
}
open(FILE, "${conf_file}") || die "Could not open configuration ${conf_file}:\n$!\nExiting\n";
my @infile = <FILE>;
close(FILE);

  foreach my $line (@infile)
  {
  chomp($line);
	if($line =~ /(^\s*dbname\s*=\s*"(.*)")/)
	{
		$dbname = "$2";
	}
	elsif($line =~ /(^\s*dbhost\s*=\s*"(.*)")/)
	{
		$host = "$2";
	}
	elsif($line =~ /(^\s*dbpass\s*=\s*"(.*)")/)
	{
		$pass = "$2";
	}
	elsif($line =~ /(^\s*dbuser\s*=\s*"(.*)")/)
	{
		$user = "$2";
	}
	elsif($line =~ /(^\s*nrldb_host\s*=\s*"(.*)")/)
	{
		$nrldb_host = "$2";
	}
	elsif($line =~ /(^\s*site\s*=\s*"(.*)")/)
	{
		$site_conf = "$2";
	}
        elsif($line =~ /(^\s*backup_host\s*=\s*"(.*)")/)
        {
                $backup_host = "$2";
        }

  }
  return($dbname, $host, $user, $pass, $nrldb_host, $site_conf, $backup_host);
}


sub xml_parse
{
my $xmlfile = $inFile;              # the file to parse
my $lineCount = 0;
my @rawLine;
my $last_f;
my $record_num;
my $table;
my ($i, $j, $k);
my ($PK_name, $PK_value, $Field_name, $Field_value);
sub insertValues($table, $record_num, $PK_name, $PK_value, $Field_name, $Field_value);

print "Parsing and Inserting Values from $xmlfile into database\n\n" if($verbose);
warn "Parsing and Inserting Values from $xmlfile into database\n\n";

open(XML_FH, "$xmlfile") or die("Cant open file $xmlfile for reading: $!\nExiting\n");
while (<XML_FH>)
{
	# $_ is the line that <XML_FH> has set.
	  $rawLine[$lineCount] = "$_";
	  $lineCount++;
}



close(XML_FH);

$i=0;

  while (!$last_f)
  {
	if ($rawLine[$i] =~ m/<Table name="(.*)">/)
	{
		print "Current Table: $1\n" if($verbose);
		warn "Current Table: $1\n";
		$table = $1;
		while($rawLine[$i] !~ m/<\/Table>/)
		{
		  if($rawLine[$i] =~ /<Record record_num=\"(\d*)\">/)
		  {
		    $record_num = $1;
		    while ($rawLine[$i] !~ m/<\/Record>/)
		    {
			if($rawLine[$i] =~ /<PK>/)
			{ $i++;
			  $j = 0;
			  while($rawLine[$i] !~ m/<\/PK>/)
			  {
				if($rawLine[$i] =~ m/<$table\.(.*?)>(.*)<\/$table\..*>/)
				{
				$$PK_name[$j] = $1;
				$$PK_value[$j] = $2;
				$j++;
				}
				elsif($rawLine[$i] =~ m/<$table\.(.*)\/>/)
				{
				$$PK_name[$j] = $1;
				$$PK_value[$j] = "NULL";
				$j++;
				}
				elsif($rawLine[$i] =~ m/<$table\.(.*?)>.*/)
				{

					{$$PK_name[$k] = $1;}
					$$PK_value[$j] = '';
					do
					{
					  $$PK_value[$j] .= $rawLine[$i];
					  $i++;
					} until ($rawLine[$i] =~ m/<\/$table\..*>$/);
					$$PK_value[$j] .= $rawLine[$i];
					$$PK_value[$j] =~ s/^\s*<$table\.(.*)>//g;
					$$PK_value[$j] =~ s/<\/$table\..*>$//g; #/
					$j++;
				}
				$i++;
			  }
			}
			if($rawLine[$i] =~ /<Fields>/)
			{ $i++;
			  $k = 0;
			  while($rawLine[$i] !~ m/<\/Fields>/)
			  {
				if($rawLine[$i] =~ m/<$table\.(.*?)>(.*)<\/$table\..*>/)
				{
				$$Field_name[$k] = $1;
				$$Field_value[$k] = $2;
				$k++;
				}
				elsif($rawLine[$i] =~ m/<$table\.(.*)\/>/)
				{
				$$Field_name[$k] = $1;
				$$Field_value[$k] = "NULL";
				$k++;
				}
				elsif($rawLine[$i] =~ m/<$table\.(.*?)>.*/)
				{

					{$$Field_name[$k] = $1;}
					$$Field_value[$k] = '';
					do
					{
					  $$Field_value[$k] .= $rawLine[$i];
					  $i++;
					} until ($rawLine[$i] =~ m/<\/$table\..*>$/);
					$$Field_value[$k] .= $rawLine[$i];
					$$Field_value[$k] =~ s/^\s*<$table\.(.*)>//g;
					$$Field_value[$k] =~ s/<\/$table\..*>$//g; #/
					$k++;
				}
				$i++;
			  }
			}
			$i++;
		    }
		    &insertValues($table, $record_num, $PK_name, $PK_value, $Field_name, $Field_value);
		    $#$PK_name = -1; $#$PK_value = -1; $#$Field_name = -1; $#$Field_value = -1;
		    $total_count++;
		  }
		  $i++;
		}
		print "\tTotal Inserts: $insert_count\n" if($verbose);
		warn "\tTotal Inserts: $insert_count\n";
		print "\tTotal Updates: $update_count\n" if($verbose);
		warn "\tTotal Updates: $update_count\n";
		print "\tTotal Errors: $error_count\n" if($verbose);
		warn "\tTotal Errors: $error_count\n";
		print "\tTOTAL: $total_count\n\n" if($verbose);
		warn "\tTOTAL: $total_count\n\n";
		$insert_count = 0;
		$update_count = 0;
		$error_count = 0;
		$total_count = 0;
	}
	elsif ($rawLine[$i] =~ /<\/NRLDB>/)
	{$last_f = 1;}
	else
	{$i++;}
  }

}

sub get_delete_list
{
	my @list;
	my $table;
	
	open(FILE, "${conf_dir}/nrldb_control_delete") || die "Could not open detail extract file ${conf_dir}/nrldb_control_delete:\n$!\nExiting\n";
	my @infile = <FILE>;
	close(FILE);

	foreach my $line (@infile)
	{
	  chomp($line);
	  if($line =~ m/^\s*#/)
	  {next;}
	  
	  if($line =~ m/^\s*\w+\s*$/)
	  {
	    $line =~ s/\s*//g;
	    $table=lc($line);
	    push(@list, $table);
	  }
	}

	return(\@list);
}

sub deleteValues
{
	my $deleteTable = shift;
	my $deleteWFO = $office;
	my $lid_flag = lid_check($deleteTable);
	my ($delete_query, $st);
	
	my ($delete_detail, $total);
	
	if($lid_flag == 1)
	{
		($delete_detail, $total)=getDeleteLid($deleteTable);
		if($total !=0)
		{
			$delete_query = "DELETE FROM $deleteTable $delete_detail\;";
		print "DELETE: $delete_query\n";
		}
	}
	else
	{
		$delete_query = "DELETE FROM $deleteTable\;";
	}
	
	eval
	{
		$st = $db->prepare($delete_query);
		$st->execute() or die "Cannot execute: ".$st->errstr();
	};
	if($@)
	{print "$@\n" if($verbose); warn "$@\n";}
	
}


sub getDeleteLid
{

my $xmlfile = $inFile;              # the file to parse
my $lineCount = 0;
my @rawLine;
my $last_f;
my $record_num;
my $table;
my ($i, $j, $k);
my $lid_name;

my $deleteTable = shift;
my $total_count = 0;

open(XML_FH, "$xmlfile") or die("Cant open file $xmlfile for reading: $!\nExiting\n");
while (<XML_FH>)
{
	# $_ is the line that <XML_FH> has set.
	  $rawLine[$lineCount] = "$_";
	  $lineCount++;
}

close(XML_FH);

$i=0;
my $delete_str = "";
my $last_lid = -1; 
  while (!$last_f)
  {
	if ($rawLine[$i] =~ m/<Table name="($deleteTable)">/)
	{
		print "Delete Table: $1\n" if($verbose);
		warn "Delete Table: $1\n";
		$table = $1;
		while($rawLine[$i] !~ m/<\/Table>/)
		{
		  if($rawLine[$i] =~ /<Record record_num=\"(\d*)\">/)
		  {
		    $record_num = $1;
		    while ($rawLine[$i] !~ m/<\/Record>/)
		    {
			if($rawLine[$i] =~ /<PK>/)
			{ $i++;
			  while($rawLine[$i] !~ m/<\/PK>/)
			  {
				if($rawLine[$i] =~ m/<$table\.lid>(.*)<\/$table\.lid>/)
				{
					if(($last_lid != -1) && ($last_lid eq $1))
					{$i++; next;}
					#print "$1\n";
					if ($total_count == 0)
					{
						$delete_str .= "WHERE $table.lid = '$1'";
					}
					else
					{
						$delete_str .= " OR $table.lid = '$1'";
					}
					
					$last_lid = $1;
					
				}
				$i++;
			  }
			}
			$i++;
		    }
		    $total_count++;
		  }
		  $i++;
		}
		print "\tTotal Delete LIDs: $total_count\n" if($verbose);
		warn "\tTotal Delete LIDs: $total_count\n";
		$last_f = 1;
	}
	elsif ($rawLine[$i] =~ /<\/NRLDB>/)
	{$last_f = 1;}
	else
	{$i++;}
  }
	#print "$delete_str, $total_count\n";
	return ($delete_str, $total_count);
	
}


sub insertValues($table, $record_num, $PK_name, $PK_value, $Field_name, $Field_value)
{
	my $num;
	my ($fields, $values);
	my ($update_set, $update_where);
	my $Field_value_quoted;
	my $table = shift;
	my $record_num = shift;
	my $PK_name = shift;
	my $PK_value = shift;
	my $Field_name = shift;
	my $Field_value = shift;
	my $update_flag = 0;
	my $st_handle;
	my $insertrows;

	for($num = 0; $num <= $#$Field_value; $num++)
	{
		if($num == 0)
		{
			$fields = "($$Field_name[$num]";
			if($$Field_value[$num] ne "NULL")
			{
			$$Field_value[$num] = $db->quote($$Field_value[$num]);
			$values = "($$Field_value[$num]";
			$update_set = "$$Field_name[$num]=$$Field_value[$num]";
			}
			else
			{
			$values = "($$Field_value[$num]";
			$update_set = "$$Field_name[$num]=$$Field_value[$num]";
			}
		}
		else
		{
			$fields .= ", $$Field_name[$num]";
			if($$Field_value[$num] ne "NULL")
			{
			$$Field_value[$num] =~ s/\n//g;
			$$Field_value[$num] =~ s/\r//g;
			$$Field_value[$num] = $db->quote($$Field_value[$num]);
			$values .= ", $$Field_value[$num]";
			$update_set .= ", $$Field_name[$num]=$$Field_value[$num]";
			}
			else
			{
			$values .= ", $$Field_value[$num]";
			$update_set .= ", $$Field_name[$num]=$$Field_value[$num]";
			}
		}
	}
	for($num = 0; $num <= $#$PK_name; $num++)
	{
		if($num == 0)
		{
			$$PK_value[$num] = $db->quote($$PK_value[$num]);
			$update_where = "$$PK_name[$num]=$$PK_value[$num] ";
		}
		else
		{
			$$PK_value[$num] = $db->quote($$PK_value[$num]);
			$update_where .= "AND $$PK_name[$num]=$$PK_value[$num]";
		}
	}

	$fields .= ")";
	$values .= ")";
	my $insert_cmd = "INSERT INTO $table $fields VALUES $values\;";
	my $update_cmd = "UPDATE $table SET $update_set WHERE $update_where\;";

	eval {
	$insert_count++;
	$st_handle = $db->prepare($insert_cmd);
	$st_handle->execute() or die "Cannot execute: ".$st_handle->errstr();
	$insertrows = $st_handle->rows();
	  if($insertrows == 0)
	  {
	  $insert_count--;
	  $error_count++;
	  print "ZERO ROWS FOR QUERY: $insert_cmd\n\n" if($verbose);
	  warn "ZERO ROWS FOR QUERY: $insert_cmd\n\n";
	  }
	};

	if ($@) {
		if($@ =~ /duplicate key/)
		{
			$update_flag = 1;
			$insert_count--;
		}
		else
		{
			print "$@\n" if($verbose);
			warn "$@\n";
			$insert_count--;
			$error_count++;
			print "INSERT ERROR ON QUERY: $insert_cmd\n\n" if($verbose);
			warn "INSERT ERROR ON QUERY: $insert_cmd\n\n";

		}
	}

	if($update_flag == 1)
	{
		eval {
		$update_count++;
		$st_handle = $db->prepare($update_cmd);
		$st_handle->execute() or die "Cannot execute: ".$st_handle->errstr();
		$insertrows = $st_handle->rows();
		  if($insertrows == 0)
		  {
		  $update_count--;
		  $error_count++;
		  print "ZERO ROWS FOR QUERY: $update_cmd\n\n" if($verbose);
		  warn "ZERO ROWS FOR QUERY: $update_cmd\n\n";
		  }
		};

		if ($@) {
			print "$@\n" if($verbose);
			warn "$@\n";
			$update_count--;
			$error_count++;
			print "UPDATE ERROR ON QUERY: $update_cmd\n\n" if($verbose);
			warn "UPDATE ERROR ON QUERY: $update_cmd\n\n";
		}
	}

}


sub db_connect
{
my $dbname = shift;
my $host = shift;
my $user = shift;
my $pass = shift;

my %db_attr = (
	PrintError => 0,
	RaiseError => 0,
);

my $dsn = "DBI:Pg:dbname=$dbname;host=$host";
my $db = DBI->connect($dsn, $user, $pass, \%db_attr) or die "Can't connect() to database $dbname: $DBI::errstr";
return ($db);
}

sub upload_xml
{
	print "---UPLOAD XML FILE----\n" if($verbose);
	warn  "---UPLOAD XML FILE----\n";
	my $upload_string = "rsync -av --chmod=ugo+rw $outFile.zip $nrldb_host\::nrldb_xml/";
	print "$upload_string\n" if($verbose);
	warn "$upload_string\n";
	my $upload_exe = `$upload_string`;
	print "$upload_exe\n" if($verbose);
	warn "$upload_exe\n";
	print "Failed:  \"$upload_string\"\n" if ($? && $verbose);
	warn "Failed:  \"$upload_string\"\n" if $?;
	return;
}
sub download_control_file
{
	my $office_type = shift;
	my $download_string;
	print "---DOWNLOAD $office_type CONTROL FILE----\n" if($verbose);
	warn  "---DOWNLOAD $office_type CONTROL FILE----\n";

	if ($office_type eq "WFO")
	{
		$download_string = "rsync -av $nrldb_host\::nrldb_control/nrldb_control_wfo ${conf_dir}/";
	}
	elsif ($office_type eq "RFC")
	{
		$download_string = "rsync -av $nrldb_host\::nrldb_control/nrldb_control_rfc ${conf_dir}/";
	}
	print "$download_string\n" if($verbose);
	warn "$download_string\n";
	my $download_exe = `$download_string`;
	print "$download_exe\n" if($verbose);
	warn "$download_exe\n";
	print "Failed:  \"$download_string\"\n" if ($? && $verbose);
	warn "Failed:  \"$download_string\"\n" if $?;
	return;
}

sub getdate()
{
my ($Second, $Minute, $Hour, $Day, $Month, $Year, $WeekDay, $DayOfYear, $IsDST) = localtime(time) ;
my $RealMonth = $Month + 1 ; # Months of the year are not zero-based
my $FixedYear;

if ($Hour < 10)
{
	$Hour = "0" . $Hour
}

if ($Minute < 10)
{
	$Minute = "0" . $Minute
}

if ($Second < 10)
{
	$Second = "0" . $Second
}

if ($RealMonth < 10)
{
	$RealMonth = "0" . $RealMonth;
}

if ($Day < 10)
{
	$Day = "0" . $Day;
}

if ($Year >= 100)
{
	$FixedYear = $Year - 100;
}
else
{
	$FixedYear = $Year;
}

if ($FixedYear < 10)
{
	$FixedYear = "0" . $FixedYear;
}

my $clean_date = "$Hour:$Minute:$Second $RealMonth/$Day/$FixedYear";

return($clean_date);
}

sub lid_check {
	my $table_name = shift;
	my $at;
	my $lid_flag = 0;

	my $query_column1 = "SELECT c.oid 
			FROM pg_catalog.pg_class c
			LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
			WHERE pg_catalog.pg_table_is_visible(c.oid)
			AND c.relname ~ '^$table_name\$'";

	my $attribute_query = "SELECT a.attname
			FROM pg_catalog.pg_attribute a
			WHERE a.attnum > 0 AND NOT a.attisdropped
			AND a.attrelid = ($query_column1)
			ORDER BY a.attnum;";

	eval {
		$at = $db->prepare($attribute_query);
		$at->execute() or die "Cannot execute: ".$at->errstr();
	};
	if($@) {
	  print "$@\n";
	}

	while ( defined ( my $attribues = $at->fetchrow_arrayref() ) ) {
		if(@$attribues[0] =~ /^lid$/) {
			$lid_flag = 1;
		}
	}

return ($lid_flag);
}

BEGIN {
	use CGI::Carp qw(carpout);
	my $logDir;
	if( -e "/awips/hydroapps/public/bin/get_apps_defaults"){
		$logDir = `/awips/hydroapps/public/bin/get_apps_defaults.LX nrldb_log`;
		chomp($logDir);
	} else {
		print "Could not access /awips/hydroapps/public/bin/get_apps_defaults.LX. Exiting\n";
		exit -1;
	}
	print "log dirlogDir\n";
	my $log = "${logDir}/nrldb.log";
	open(LOG, ">>$log") or die "Unable to open $log. $! ";
	carpout(*LOG);
}

END {
	my $date = `date`;
	print LOG "End $0 at $date\tElapsed time: " . (time - $^T) . " seconds\n\n";
	close LOG;
}

sub getcutoffdate()
{
my ($Second, $Minute, $Hour, $Day, $Month, $Year, $WeekDay, $DayOfYear, $IsDST) = gmtime(time-172800) ;
my $RealMonth = $Month + 1 ; # Months of the year are not zero-based
my $FixedYear;

if ($Hour < 10)
{
        $Hour = "0" . $Hour
}

if ($Minute < 10)
{
        $Minute = "0" . $Minute
}

if ($Second < 10)
{
        $Second = "0" . $Second
}

if ($RealMonth < 10)
{
        $RealMonth = "0" . $RealMonth;
}

if ($Day < 10)
{
        $Day = "0" . $Day;
}

        $FixedYear = $Year + 1900;

my $clean_date = "$FixedYear-$RealMonth-$Day $Hour:$Minute";

return($clean_date);
}
