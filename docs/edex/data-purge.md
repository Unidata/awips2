

    psql metadata

    select * from purgejobs;

         plugin      | failedcount | running |        starttime        
    -----------------+-------------+---------+-------------------------
     acars           |           0 | f       | 2016-01-07 16:30:00.028
     acarssounding   |           0 | f       | 2016-01-07 16:30:00.033
     airep           |           0 | f       | 2016-01-07 16:31:00.023
     airmet          |           0 | f       | 2016-01-07 16:31:00.028
     ...
     mcidas          |           3 | f       | 2016-01-04 00:15:00.03
     ...
     
Notice the failedcount value of 3 for the mcidas plugin.  This means that purging is locked for this plugin because something failed three times.  Let's find out what failed. 
