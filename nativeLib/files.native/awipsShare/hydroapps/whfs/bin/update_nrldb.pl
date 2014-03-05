#!/usr/bin/perl
################################################################################
# update_nrldb.pl is the GUI for the Ad-Hoc update process.                    ## This process was put in place so that WFOs could update information          #
# between daily runs of the NRLDB update process.  The information is          #
# collected at the WFO, sent to the NRLDB central server and then forwarded to #
# CMS servers outside of the AWIPS firewall.                                   #
#                                                                              #
# Developer: Mark Armstrong (OCWWS/HSD)                                        #
# Developed 2011 - Modified for AWIPS2 2013                                    #
################################################################################
  
use Tk;
use strict;
use warnings;
use AppConfig qw(:expand :argcount);
use DBI;

our $BIN_DIR = `echo \$BIN_DIR`;
chomp($BIN_DIR);
our $NRLDB_LOG = `echo \$NRLDB_LOG`;
chomp($NRLDB_LOG);

my $lids;
my $tables;

# Set up some inial configuration.  Most of this comes from the hydroGen input file: hg.cfg
$ENV{HYDROGENHOME} = "/awips/hydroapps/HydroGen" if ! defined $ENV{HYDROGENHOME};
my %cfg = ( DEBUG => 0,         # debug mode on or off
                        PEDANTIC => 0,  # be patient with warnings/errors
                        CREATE => 1,    # create variables, defining not required...
                        GLOBAL => {     # for all config options unless overridden...
                                EXPAND => EXPAND_ALL, # expand ~, $ENV{*}, and $(var)
                                ARGCOUNT => ARGCOUNT_ONE,       # each config expects an arg unless overriden...
                                ARGS => '=s'    # each arg is a string unless overriden
                        }
                  );

my $config = AppConfig->new(\%cfg);     # create config object

$config->define('version',{ ALIAS => 'V',ARGCOUNT => ARGCOUNT_NONE, ARGS => '!',DEFAULT => 0});
$config->define('help',{ ALIAS => 'h',ARGCOUNT => ARGCOUNT_NONE, ARGS => '!',DEFAULT => 0});
$config->define('man',{ ALIAS => 'm',ARGCOUNT => ARGCOUNT_NONE, ARGS => '!',DEFAULT => 0});
$config->define('DBengine',{ VALIDATE => '[\w]+',DEFAULT => "Pg"});
$config->define('DBname',{ VALIDATE => '[\w]+',DEFAULT => "hd_ob8xxx"});
$config->define('DBhost',{ VALIDATE => '[-\w]+',DEFAULT => "dx1f"});
$config->define('DBport',{ ARGS => '=i',DEFAULT => 5432});
$config->define('master',{ VALIDATE => '[.\w]+',DEFAULT => "HGstation"});
$config->define('basedir',{ VALIDATE => '[- /.\w]+',DEFAULT => $ENV{HYDROGENHOME} . "/bin"});

$config->file($ENV{HYDROGENHOME} . "/input/hg.cfg");  # look in user's $HYDROGENHOME to find configured settings
$config->args(\@ARGV);  # get config settings from the command-line, overwriting any settings from the file...

my $master = $config->get('master');    # name of DB table or view which holds master list of IDs for which MXD files are to be generated...
my $DBengine = $config->get('DBengine');
my $DBname = $config->get('DBname');
my $DBhost = $config->get('DBhost');
my $DBport = $config->get('DBport');
my $baseDir = `pwd`;
chomp $baseDir;
my $DBstr;
my $wildcard;

#Open a database connection and get the list of LIDs from the IHFS DB
if($DBengine eq "Pg") {
        $DBstr = "dbi:$DBengine:dbname=$DBname;host=$DBhost;port=$DBport";
        $wildcard = '%';
} else {
        $DBstr = "dbi:$DBengine:$DBname";
        $wildcard = '*';
}

my $dbh = DBI->connect("$DBstr",undef,undef,{ChopBlanks => 1}) or warn $DBI::errstr;
# creates the list of WFOs based on the HydroGen .xxx_backup files
# and builds the query to create the list of LIDs
my $wfo=`ls -a /awips/hydroapps/HydroGen/ | grep _backup | cut -c2-4`;
my $list_len=length $wfo;
my $num_wfos=$list_len/4;
my $index=1;
my $off=0;
my $wfoid=substr($wfo,$off,3);
my $wfoID=uc $wfoid;
my $wfo_query = "(location.hsa = \'$wfoID\'";
while ($index < $num_wfos){
        $off+=4;
        $wfoid=substr($wfo,$off,3);
        $wfoID=uc $wfoid;
        $wfo_query .= " or location.hsa = \'$wfoID\'";
        $index++;
}
$wfo_query .= ")";

#my $list_type="river";
our $mw = MainWindow->new;
$mw->title('Ad-Hoc NRLDB Update');

my $lst_lab= $mw->Label(-text => 'Add any Unlisted Locations (comma-separated): ');
my $sql = "select distinct hgstation.lid,location.name,location.hsa from hgstation,location where hgstation.lid = location.lid and $wfo_query order by 3,1;";

# get the list of LIDs
my $qhw = $dbh->prepare("$sql") or warn $DBI::errstr;

our @lid_list; # = ($wildcard);

#get the data from the DB
get_results($qhw,\@lid_list);

#set up a static array with the tables that are allowed for ad-hoc updates
#table_list is the actual name of the DB tables, while tabledesc is a friendlier description that is displayed to the user
our @table_list = ('location','riverstat','crest','floodstmt','hgstation','floodcat','lowwater');
my @tabledesc = ('Location','Riverstat','Crest History','Impacts','HGstation','Flood Categories','Low Water');

$dbh->disconnect();

#manipulate the results of the lid/hsa/name query for better display 
my @liddeschsa;
our @lidsend;
$index=0;
my $num_lids=scalar(@lid_list);
while ($index < $num_lids){
        my $line = $lid_list[$index];
        my @results = split('\|',$line);
        #my $lid = $lid_list[$index];
        my $lid_lid = $results[0];
        my $lid_name = $results[1];
        my $lid_hsa = $results[2];
        push(@liddeschsa,"$lid_hsa     |     $lid_lid     |     $lid_name");
        push(@lidsend,$lid_lid);
        $index++;
}

# Create the GUI object
# Labels for the LID and table scroll boxes
my $misc_ent = $mw->Entry();
my $label1 = $mw->Label(-text => 'HSA|LID|Location Name');
my $label2 = $mw->Label(-text => 'Tables');

# Create the scroll boxes for the LIDs and tables
my $lb1 = $mw->Scrolled('Listbox',
                       -scrollbars => 'osoe',-width=>50,
                       -selectmode => 'multiple', -exportselection=>0);
my $lb2 = $mw->Scrolled('Listbox',
                       -scrollbars => 'osow',-width=>20,
                       -selectmode => 'multiple',-exportselection=>0);

# Add the arrays that we want to display in the list boxes
$lb1->insert('end', @liddeschsa);
$lb2->insert('end', @tabledesc);

# Create the buttons
my $exit = $mw->Button(-text => 'Exit',
                       -command => [$mw => 'destroy']);
my $send = $mw->Button(-text => 'Send',
                       -command => \&send_button);
my $show_log = $mw->Button(-text => 'Show Log',
                       -command => \&show_log);
my $update_list = $mw->Button(-text => 'Update List', -command => \&upd_list); 
# create the label and text box for the last pdate window 
my $status_box = $mw->Text(-width=>20, -height=>3);
my $lb_status = $mw->Label(-width=>20, -height=>3,-text=>"Last Ad-Hoc Update:");
my $last_update = `cat $NRLDB_LOG/last_nrldb_update.txt`;

$status_box->insert('end',"$last_update");

# Crate the GUI using grid to specify the physical locations of the objects
$label1->grid(-row=>1, -column=>1, -columnspan=>3) ;
$label2->grid(-row=>1, -column=>4) ;
$lb1->grid(-row=>2, -column=>1, -columnspan=>3, -sticky=>"ew") ;#pack;
$lb2->grid(-row=>2, -column=>4, -columnspan=>1, -sticky=>"w") ;#pack;
$lst_lab->grid(-row=>3, -column=>1, -columnspan=>1);
$misc_ent->grid(-row=>3, -column=>2);
$lb_status->grid(-row=>4, -column=>1);
$status_box->grid(-row=>4, -column=>2, -columnspan=>3, -sticky=>"ew");
$send->grid(-row=>5, -column=>1) ;#pack;
$show_log->grid(-row=>5,-column=>2);
$exit->grid(-row=>5, -column=>4) ;#pack;

MainLoop;

# End of main
#

# The Send button functionality function
sub send_button {
	# Get the indices of the selected array items
	my @LIDindex = $lb1->curselection;
	my @Tableindex = $lb2->curselection;
	my $index=1;
	my $misc_lid = $misc_ent-> get();
	# build the lists of LIDs and tables
	$tables = $table_list[$Tableindex[0]];
	my $numLIDs=@LIDindex;
	print "numLIDs: $numLIDs\n";
	my $numTables=@Tableindex;
	if ($numLIDs > 0){
		$lids = $lidsend[$LIDindex[0]];
		while ($index < $numLIDs){
        		$lids .= "," . $lidsend[$LIDindex[$index]];
        		$index++;
		}
		$lids .= "," . $misc_lid;
	} else {
		$lids=$misc_lid;
	}
	$index=1;
	while ($index < $numTables){
        	$tables .= "," . $table_list[$Tableindex[$index]];
        	$index++;
	}
	
	# Create the call to the script and execute it using system()
	my $cmd = "${BIN_DIR}/send_nrldb_update.sh -table $tables -lid $lids > ${NRLDB_LOG}/send_nrldb_update.log\n";
	system($cmd);

	# Create a dialog box to inform the user that their data has been sent
	my $dsend=$mw->Dialog(-title=>'Sent NRLDB Update',-buttons=>['OK']);
	my $text_field="NRLDB Update Sent for LIDs: $lids \n and tables: $tables\n";
	my $box=$dsend->add('Label',-text=>"$text_field")->pack(-side => 'left',-fill => 'both',-expand => 1);
	my $button = $dsend->Show;
}
# This subroutine, copied from Mark Fenbers bless program, takes a db query and returns an array of results  
sub get_results
{
        my $qh = shift;
        my $array = shift;
        my $record;

        if(defined $qh) {
                if($qh->execute(@_)) {
                        while($record = $qh->fetchrow_arrayref) {
                                foreach (@$record) { $_ = "" if ! defined $_; }
                                push @$array,(join '|',@$record);
                        }
                } else {
                        warn $DBI::errstr;
                }
        } else { warn "unable to prepare query \"$sql\"\n"; }
}

#This subroutine displays the log from the send script in the form of a dialog box
sub show_log
{
	use Tk::Dialog;
	my $text_field=`cat ${NRLDB_LOG}/send_nrldb_update.log`;
	my $d = $mw->Dialog(-title=>'Show Log',-buttons => ['OK']);
	my $box=$d->add('Label',-text=>"$text_field")->pack(-side => 'left',-fill => 'both',-expand => 1);
	my $button = $d->Show;
}
	
