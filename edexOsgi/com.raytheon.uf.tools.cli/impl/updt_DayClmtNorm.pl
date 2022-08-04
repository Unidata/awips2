#!/usr/bin/perl

#########################
# 
#  updt_DayClmtNorm.pl
#
#  Description:
#  ------------
#
#  updt_DayClmtNorm.pl perform the task to update day_climate_norm table
#  in climate database. It first search in day_climate_extreme table that have 
#  all extreme new data records, put data in to @clm_nrm_rcd structure array.
#  then, it update in day_climate_norm table. 
#
#
#  Xiaochuan Huang /ASM/MDL	May 2009
#
###########################  

use DBI;

    $norm_tb = "day_climate_norm";
    $temp_tb = "day_climate_extreme";
    undef @recd_ary;
    $clmns = 0;
    @clm_nrm_rcd = {};

   print "\n#####################################";
   print "\n----  Start Database updating!!\n";
   print "\n#####################################\n";

   $yy = `date -u +%Y`;
   chomp( $yy );
   print "\nCurrent year is: $yy. ";

#  Work on climate database

    $dbh = DBI->connect("DBI:Pg:database=climate") || 
		die "Can't connect: $DBI::errstr\n";

    print "\nConnect to climate database.";

    $rcd_r = get_extreme_data();
  
    print "\n---- get_extreme_data() return rcd_r=$rcd_r\n";
    if( $rcd_r > 0 )
    {
	print "\nGot total $rcd_r records from day_climate_extreme table.";
#	check_climteArray();

	print "\nWill Update in day_climate_norm table";     
	$rows_updt = update_DayClmtNorm();
	print "\nUpdate in day_climate_norm table total record rows: $rows_updt";
    }
    else
    {
	print "\n day_climate_extreme table is empty. No data records fund!\n";
    }

    $done = $dbh->disconnect();
    print "\n-!-!-! climate database closed!";

    print "\n----- Updating day_climate_nor table Finished! \n";

####################
#  get_extreme_data fetch and process data from day_climate_extreme table
#  for maximum temp, minimum temp, maximum precipetation day. Each item 
#  have three records (*_record1, *_record2, *_record3) with relevant 
#  years (*_yr1, *_yr2, *_yr3). After data processing, it set data in      
#  in structure array @clm_nrm_rcd[] followed these roul:
#    1. if *_yr2 *_yr3 is null or 0, set it to 9999; 
#    2. for each item, if *_record2 != *_record1, *_record3 != *_record1,
#  	set *_yr2=9999, *_yr3=9999;  
#
##################### 
sub get_extreme_data
{
    my $rows = 0;

    $sql_str = "SELECT b.station_id, a.station_code, a.day_of_year, ";
    $sql_str .= "a.h_max_temp_record1, a.h_max_temp_record2, a.h_max_temp_record3, a.h_max_temp_rec_yr1, a.h_max_temp_rec_yr2, a.h_max_temp_rec_yr3, a.l_min_temp_record1, a.l_min_temp_record2, a.l_min_temp_record3, a.l_min_temp_rec_yr1, a.l_min_temp_rec_yr2, a.l_min_temp_rec_yr3, a.precip_day_max1, a.precip_day_max2, a.precip_day_max3, a.precip_day_max_yr1, a.precip_day_max_yr2, a.precip_day_max_yr3 ";
    $sql_str .= "from day_climate_extreme as a, station_location as b ";
    $sql_str .= "where a.station_code=b.station_code ";

#    print "\n sql_str == $sql_str,,,";

    $sth=$dbh->prepare( $sql_str) or die $dbh_c->errstr;;
    $sth->execute();

    $rs = $sth->bind_columns(\$station_id, \$station_code, \$day_of_year, \$h_max_temp_record1, \$h_max_temp_record2, \$h_max_temp_record3, \$h_max_temp_rec_yr1, \$h_max_temp_rec_yr2, \$h_max_temp_rec_yr3, \$l_min_temp_record1, \$l_min_temp_record2, \$l_min_temp_record3, \$l_min_temp_rec_yr1, \$l_min_temp_rec_yr2, \$l_min_temp_rec_yr3, \$precip_day_max1, \$precip_day_max2, \$precip_day_max3, \$precip_day_max_yr1, \$precip_day_max_yr2, \$precip_day_max_yr3);

    my $i = 0;

    while ($sth->fetch)
    {
	$clm_nrm_rcd[$i]->{sta_id} = $station_id;
	$clm_nrm_rcd[$i]->{sta_code} = $station_code;
	$clm_nrm_rcd[$i]->{day_of_y} = $day_of_year;
	$clm_nrm_rcd[$i]->{max_temp_rec} = $h_max_temp_record1;
	$clm_nrm_rcd[$i]->{max_temp_yr1} = $h_max_temp_rec_yr1;
     
    	if( $h_max_temp_rec_yr2 eq null || $h_max_temp_rec_yr2 == 0 
	    || $h_max_temp_record1 != $h_max_temp_record2 )
	{
	    $h_max_temp_rec_yr2 = 9999;
	}
	$clm_nrm_rcd[$i]->{max_temp_yr2} = $h_max_temp_rec_yr2;

	if( $h_max_temp_rec_yr3 eq null || $h_max_temp_rec_yr3 == 0 
	    || $h_max_temp_record1 != $h_max_temp_record3 )
	{
	    $h_max_temp_rec_yr3 = 9999;
	}
	$clm_nrm_rcd[$i]->{max_temp_yr3} = $h_max_temp_rec_yr3;
	$clm_nrm_rcd[$i]->{min_temp_rec} = $l_min_temp_record1;
	$clm_nrm_rcd[$i]->{min_temp_yr1} = $l_min_temp_rec_yr1;

	if( $l_min_temp_rec_yr2 eq null || $l_min_temp_rec_yr2 == 0 
	    || $l_min_temp_record1 != $l_min_temp_record2 )
	{
	    $l_min_temp_rec_yr2 = 9999
	}
	$clm_nrm_rcd[$i]->{min_temp_yr2} = $l_min_temp_rec_yr2;
	if( $l_min_temp_rec_yr3 eq null || $l_min_temp_rec_yr3 == 0 
	    || $l_min_temp_record1 != $l_min_temp_record3 )
	{
	    $l_min_temp_rec_yr3 = 9999;
	}
	$clm_nrm_rcd[$i]->{min_temp_yr3} = $l_min_temp_rec_yr3;
	$clm_nrm_rcd[$i]->{prcpt_day_max} = $precip_day_max1;
	$clm_nrm_rcd[$i]->{prcpt_day_max_yr1} = $precip_day_max_yr1;

	if( $precip_day_max_yr2 eq null || $precip_day_max_yr2 == 0 
	    || $precip_day_max1 != $precip_day_max2 )
	{
	    $precip_day_max_yr2 = 9999;
	}
	$clm_nrm_rcd[$i]->{prcpt_day_max_yr2} = $precip_day_max_yr2;

	if( $precip_day_max_yr3 eq null || $precip_day_max_yr3 == 0 
	    || $precip_day_max1 != $precip_day_max3 )
	{
	    $precip_day_max_yr3 = 9999;
	}
	$clm_nrm_rcd[$i]->{prcpt_day_max_yr3} = $precip_day_max_yr3;

	$i++;
    }
    $rows = $i;

#    print "\n----- \@row_ary";
    
    return ( $rows );
}

#####################
# It update day_climate_norm table. If the temp and prcpt (max, min) year1
# is in current year (such as this year 2009), it can't update.
#
# Return:
#
#	$updt_cnt -- Total number of records that updated in 
#		     day_climate_norm table
# 
####################
sub update_DayClmtNorm
{
    my $updt_cnt = 0;
    my $sql_up = "";
    my $stat = 0;
    for( $i = 0; $i < $rcd_r; $i++)
    {
	$stat = checkCurrentYear($clm_nrm_rcd[$i]->{sta_id}, $clm_nrm_rcd[$i]->{day_of_y} );

	if( $stat > 0 )
	{
#	    print "\n---- max_t_crnt_y=$max_t_crnt_y, min_t_crnt_y=$min_t_crnt_y, max_p_crnt_y=$max_p_crnt_y";

            $sql_up = "UPDATE day_climate_norm set ";
	    if($max_t_crnt_y != 1 )
	    {
	    	$sql_up .= "max_temp_record=$clm_nrm_rcd[$i]->{max_temp_rec},
		    max_temp_rec_yr1=$clm_nrm_rcd[$i]->{max_temp_yr1}, 
		    max_temp_rec_yr2=$clm_nrm_rcd[$i]->{max_temp_yr2},
		    max_temp_rec_yr3=$clm_nrm_rcd[$i]->{max_temp_yr3}, ";
	    }
	    if($min_t_crnt_y != 1 )
	    {
	    	$sql_up .= "min_temp_record=$clm_nrm_rcd[$i]->{min_temp_rec},
		    min_temp_rec_yr1=$clm_nrm_rcd[$i]->{min_temp_yr1},
		    min_temp_rec_yr2=$clm_nrm_rcd[$i]->{min_temp_yr2},
		    min_temp_rec_yr3=$clm_nrm_rcd[$i]->{min_temp_yr3} ";
		if($max_p_crnt_y != 1 )
		{
		    $sql_up .= ", ";
		}
	    }
	    if($max_p_crnt_y != 1 )
	    {
	    	$sql_up .= "precip_day_max = $clm_nrm_rcd[$i]->{prcpt_day_max},
		    precip_day_max_yr1=$clm_nrm_rcd[$i]->{prcpt_day_max_yr1},
		    precip_day_max_yr2=$clm_nrm_rcd[$i]->{prcpt_day_max_yr2},
		    precip_day_max_yr3=$clm_nrm_rcd[$i]->{prcpt_day_max_yr3} ";
  	    }
 
	    $sql_up .= "WHERE station_id=$clm_nrm_rcd[$i]->{sta_id} and day_of_year='$clm_nrm_rcd[$i]->{day_of_y}' ";

#	    print "\nsql_up --- $sql_up \n";

	    my $updt_sth = $dbh->prepare( $sql_up ) or die $dbh_c->errstr;
	    $rs = $updt_sth->execute();

#	    print "\n$i .... update station $clm_nrm_rcd[$i]->{sta_id}, $clm_nrm_rcd[$i]->{day_of_y}, returned: $rs ";
	    if( $rs == 1 )
	    {
	    	$updt_cnt++;
	    }
    	}
    }	 
 
    return $updt_cnt;
}

#########################
#  checkCurrentYear check in day_climate_norm table to see if three values  
#  of year1 for max_temp_rec, min_temp_rec, prcipt_max_day they are in
#  the current year (2009). If it was, set flag.
#
#  Parameters:
#
#	$Id -- station_id;
#	$d_y -- day_of_year;
#
#  Return:
#	$clms -- columns in searched record, or 
#	$cunt -- count selection for required $Id and $d_y;
#
##################################  
sub checkCurrentYear
{
    my ( $Id, $d_y ) = ( @_ );

    my ($max_temp_y, $min_temp_y, $precip_max_y);
    my $clms = 0;
    my $cunt = 0;
    undef @row_rec;
    undef @cunt_rec;

    $max_t_crnt_y = 0;
    $min_t_crnt_y = 0;
    $max_p_crnt_y = 0;
#    print "\n checkCurrentYear(), Id=$Id, d_y=$d_y";

    my $qury = "SELECT COUNT(station_id) FROM day_climate_norm
		WHERE station_id=$Id and day_of_year='$d_y' ";
#    print "\nCheck if record is existed. qury:: $qury";

    my $slct = $dbh->prepare( $qury );
    $slct->execute();
    @cunt_rec = $slct->fetchrow_array; 
    
    foreach $l ( @cunt_rec )
    {
	$cunt = $l;
#        print "\n!!! $Id, $d_y have $cunt record.\n";

    }
    if( $cunt == 0 )
    {
	return $cunt;
    }

    $qury = "SELECT max_temp_rec_yr1, min_temp_rec_yr1, precip_day_max_yr1 
		FROM day_climate_norm 
		WHERE station_id=$Id and day_of_year='$d_y' ";
#    print "\nqury:: $qury";

    $slct = $dbh->prepare( $qury );
    $slct->execute();
    @row_rec = $slct->fetchrow_array; 
    
    foreach $ln ( @row_rec )
    {
#    	print "\n&&&& $ln";
    	$recd_y[$clms] = $ln;	
#    	print "\n.... recd_y[$clms] = $recd_y[$clms]";
    	$clms++;    
    }
	
    if( $recd_y[0] == $yy )
    {
     	$max_t_crnt_y = 1;
	print "\n--- In $Id, $d_y, the max_temp_rec_yr1 is in current year $yy. The value of max_temp_rec and max_temp years no updating!\n"; 
    }
    if( $recd_y[1] == $yy )
    {
        $min_t_crnt_y = 1;
	print "\n--- In $Id, $d_y, the min_temp_rec_yr1 is in current year $yy. The value of min_temp_rec and min_temp years no updating!\n"; 
    }
    if( $recd_y[2] == $yy )
    {
        $max_p_crnt_y = 1;
	print "\n--- In $Id, $d_y, the precip_day_max_yr1 is in current year $yy. The value of precip_day_max and precip day max years no updating!\n"; 
    }	 

    return $clms;
}

sub check_climteArray
{
    print "\ncheck_climteArray(). rcd_r=$rcd_r, clmns =$clmns ";

#    for( $i=0; $i < $rcd_r; $i++)
    for( $i=0; $i < 20; $i++)
    {
	print "\n$i: --- $clm_nrm_rcd[$i]->{sta_id}, $clm_nrm_rcd[$i]->{sta_code},$clm_nrm_rcd[$i]->{day_of_y}";
	print "\n    $clm_nrm_rcd[$i]->{max_temp_rec}, $clm_nrm_rcd[$i]->{max_temp_yr1}, $clm_nrm_rcd[$i]->{max_temp_yr2}, $clm_nrm_rcd[$i]->{max_temp_yr3}";
	print "\n    $clm_nrm_rcd[$i]->{min_temp_rec}, $clm_nrm_rcd[$i]->{min_temp_yr1}, $clm_nrm_rcd[$i]->{min_temp_yr2}, $clm_nrm_rcd[$i]->{min_temp_yr3}";
	print "\n    $clm_nrm_rcd[$i]->{prcpt_day_max}, $clm_nrm_rcd[$i]->{prcpt_day_max_yr1}, $clm_nrm_rcd[$i]->{prcpt_day_max_yr2}, $clm_nrm_rcd[$i]->{prcpt_day_max_yr3} \n";
     
    }

}	
