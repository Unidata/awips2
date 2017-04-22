

# Grant Users Permission to Create Case Study Archives

The file `/awips2/edex/data/utility/common_static/base/roles/archiveAdminRoles.xml` controls which users can run the archiving tools from CAVE.

    <nwsRoleData xmlns:ns2="group">
       <application>Data Archiving</application>
       <permission id="archive.retention">
          <description>
             This permission allows the user to access Archive Retention.
           </description>
       </permission>
       <permission id="archive.casecreation">
          <description>
             This permission allows the user to access Archive Case Creation.
           </description>
       </permission>
       <user userId="ALL">
          <userPermission>archive.retention</userPermission>
          <userPermission>archive.casecreation</userPermission>
       </user>
    </nwsRoleData>

`<user userId="ALL">` will allow any connected CAVE user to run both the Archive Retention and the Archive Case Creation tools.  If you want to control access to individual users, such as the example bwlo, which will allow any user to create case studies, but only the *awips* user to run the Archive Retention tool.

        <user userId="awips">
          <userPermission>archive.retention</userPermission>
        </user>
        <user userId="ALL">
          <userPermission>archive.casecreation</userPermission>
        </user>

# Define EDEX User Administration Roles

Admins can use the CAVE User Administration interface to manage user access roles.  The file `/awips2/edex/data/utility/common_static/base/roles/awipsUserAdminRoles.xml` controls access to this tool.

    <nwsRoleData xmlns:ns2="group">
       <application>User Administration</application>
       <permission id="awips.user.admin">
          <description>
             This permission allows the user to access and edit AWIPS 2 User Administration
           </description>
       </permission>
       <user userId="awips">
          <userPermission>awips.user.admin</userPermission>
       </user>
    </nwsRoleData>


# EDEX Archiver

/awips2/edex/conf/resources/com.raytheon.uf.edex.archive.cron.properties

    # enable archive
    archive.enable=false
    # runs database and hdf5 archive for archive server to pull data from 
    archive.cron=0+40+*+*+*+?
    # path to store processed archive data
    archive.path=/awips2/archive
    
    # enable archive purge
    archive.purge.enable=true
    # when to purge archives
    archive.purge.cron=0+5+0/2+*+*+?
    # compress database records
    archive.compression.enable=false
    
    # To change Default case directory.
    #archive.case.directory=/awips2/edex/data/archiver/
    
    # to disable a specific archive, use property archive.disable=pluginName,pluginName...
    #archive.disable=grid,text,acars



The EDEX Archiver plugin can be used to automate data backup or create case study archive files to be retained by EDEX. The file `/awips2/edex/data/utility/common_static/base/archiver/purger/PROCESSED_DATA.xml` controls which products are ardhived, and how.



## Archive Log

The file `/awips2/edex/logs/edex-ingest-archive-*.log` will report status of the archiver whenever it is run.  With regular archiving disabled (by default) will see messages such as 
    
    INFO  2016-11-30 09:40:00,010 [Archiver] DataArchiver: EDEX - Archival of plugin data disabled, exiting
    INFO  2016-11-30 10:40:00,009 [Archiver] DataArchiver: EDEX - Archival of plugin data disabled, exiting
    INFO  2016-11-30 11:40:00,010 [Archiver] DataArchiver: EDEX - Archival of plugin data disabled, exiting
    INFO  2016-11-30 12:40:00,010 [Archiver] DataArchiver: EDEX - Archival of plugin data disabled, exiting




## /awips2/edex/data/utility/common_static/base/archiver/purger/PROCESSED_DATA.xml

`<name>`, `<rootDir>`, `<defaultRetentionHours>`, and `<category>` are the four tags which configure the EDEX Archiver. 

    <archive>
        <name>Processed</name>
        <rootDir>/awips2/archive/</rootDir>
        <defaultRetentionHours>168</defaultRetentionHours>
        <category>
            <name>Model</name>
            <selectedRetentionHours>168</selectedRetentionHours>
            <dataSet>
                <dirPattern>(grid)/(.*)/(.*)/.*-(\d{4})-(\d{2})-(\d{2})-(\d{2})-.*</dirPattern>
                <displayLabel>{2}</displayLabel>
                <dateGroupIndices>4,5,6,7</dateGroupIndices>
            </dataSet>
            <dataSet>
                <dirPattern>(modelsounding)/(.*)/.*/.*(\d{4})-(\d{2})-(\d{2})-(\d{2}).*</dirPattern>
                <dirPattern>(bufrmos)(.*)/.*(\d{4})-(\d{2})-(\d{2})-(\d{2})</dirPattern>
                <displayLabel>{1} - {2}</displayLabel>
                <dateGroupIndices>3,4,5,6</dateGroupIndices>
            </dataSet>
        </category>

`<category>` is used as a logical grouping of the archive sub-directories, and contains the following tags:

- `<name>`              - The id for the category, used in CAVE.
- `<selectedRetentionHours>`    - Optional. The hours to retain data in directories of selected Data Sets for a category. Default is 1 hour.
- `<selectedDisplayNames>`  - A directory matching `<dirPattern>`. These are selected directories from the Retention GUI. The purger will used the category's `<selectedRetentionHours>` instead of the Arhivie's `<defaultRetentionHours>`. An Optional. may have more then one. (NOTE these are set internally when a selection configuration file is loaded. They should not appear in the configuration file.)
- `<dataSet>`           - Required to have a least one. Each one contains a set of tags explained below.
                          
    The `<dataSet>` tag contains:
     
    - `<dirPattern>`        - A regex pattern for finding directories for this category.  Required to have at least one. The pattern is relative to `<rootDir>`. Wildcard patterns do not cross directory delimiter `/`. Thus to match 3 levels of directories you would need `.*/.*/.*` (see patterns and groups section). There may be more then one `<dirPattern>` in a `<dataSet>`, but they must all have the same number of groupings and be in the same order to match up with `<displayLabel>`, and `<dateGroupIndicies>`.
    - `<filePattern>`       - Optional.  A pattern to find files in the directories that match `<dirPattern>`. Default is everything in the directories that match `<dirPattern>`. See patterns and groups section.
    - `<displayLabel>`      - The label to display for directories that match `<dirPattern>`. Any group in the `<dirPattern>` may be made part of the label by placing the group's index inside parenthesis, `{1}`. More then one directory may match the `<dirPattern>`. The archive GUIs may collapse them into a single table entry.
    - `<timeType>`          - Optional tag to determine what type of time stamp is being used to get files/directories for retention and case creation. The value dictates how many groupings in the `<dirPattern>`s and/or `<filePattern>` are used to get the time stamp for a file. 
    
        The five values are:
        
        - **Date** - (default) the time stamp is made up of 3 or 4 groups in the patterns: **year**, **month**, **day** and (optional) **hour**.
        - **Julian** - Time stamp is made up of 2 or 3 groups in the patterns: **year**, **day_of_year** and (optional) **hour**.
        - **EpochSec** - Time stamp epoch time in seconds.
        - **EpochMS** - Time stamp epoch time in milliseconds.
        - **File** - Instead use the files date of last modification. No group is used to get the time stamp. 
        
    - `<dateGroupIndicies>` - Required tag when `<timeType>` has any value but **File**. 
    
        - **Date** - A comma separated list of 3 or 4 numbers which are in order the index for **year**, **month**, **day** and **hour**. When only 3 numbers the hour is value is 23.
        - **Julian** - A comma separated list of 2 or 3 numbers which are in order the index for **year**, **day of year**, and **hour**.  When only two numbers the hour value is 23.
        - **EpochSec** - A number which is the index for the epoch in seconds.
        - **EpochMS** - A number which is the index for the epoch in milliseconds.
        - **File** - Not needed since no group is used to get the time stamp.
        
        This is used to determine what files/directories to retain or a range of directories/files to copy
        for case creation. Note to get the group's index the `<dirPattern>` and `<filePattern>` are combined.
        Thus if there are 5 groups in the `<dirPattern>` then the first group in the `<filePattern>` is index 6.
 
 ## Patterns and groups.
 
`<dirPattern>` and `<filePattern>` use [Java regex expressions](http://docs.oracle.com/javase/tutorial/essential/regex/), similar to the ldm's pqact.conf file.
 
The groupings index start at one.  The groups in the `<dirPattern>` can be used in the `<displayLabel>`. For example:
 
    <dirPattern>(grib2)/(\d{4})(\d{2})(\d{2})/(\d{2})/(.*)</dirPattern>
    <displayLabel>{1} - {6}</displayLabel>
    <dateGroupIndices>2,3,4,5</dateGroupIndices>
 
`<dirPattern>` contains six groups. The first group is the literal grib2 which matches only a directory named grib2 that is a sub-directory of the `<rootDir>`. The groups 2, 3 and 4 break apart the next level of sub-directories into a 4 digit and two 2 digit groups.  This is the expected **year**, **month**, **day** sub-subdirectory indicated by the first 3 entries in `<dateGroupIndices>`.  The next sub-directory contains the fifth group which is a two digit number representing the hour. Finally the sixth group will match any sub-directory that in the hour directory.
 
Thus the directory paths `<rootPath>/grib2/20130527/18/GFS` will generate the display string, **grib2 - GFS**, and from the grouping we can find the year, **2013**; month, **05**; day, **27** and hour, **18**.
 
####  Example with **&lt;filePattern&gt;**

    <dirPattern>hdf5/(redbook)</dirPattern>
    <displayLabel>{1}</displayLabel>
    <filePattern>redbook-(\d{4})-(\d{2})-(\d{2})-(\d{2})\..*</filePattern>
    <dateGroupIndices>2,3,4,5</dateGroupIndices>
    
####  Example with multiple **&lt;dirPattern&gt;** 

    <category>
        <name>Observation</name>
        <selectedRetentionHours>168</selectedRetentionHours>
        <dataSet>
           <dirPattern>(acars|airep|airmet|taf)</dirPattern>
           <dirPattern>(bufrsigwx|sfcobs)/.*</dirPattern>
           <displayLabel>{1}</displayLabel>
           <timeType>Date</timeType>
           <dateGroupIndices>2,3,4,5</dateGroupIndices>
           <filePattern>.*-(\d{4})-(\d{2})-(\d{2})-(\d{2})\..*</filePattern>
        </dataSet>
    </category>
 
The first `<dirPattern>` looks for files matching the `<filePattern>` in the directories **acars**, **airep**, **airmet** or **taf**.
 
The second `<dirPattern>` expects to find the files in subdirectories of **bufrsigwx** or **sfcobs** such as **bufrsigwx/SWH**.
 
Here the display will only show, redbook. The directory looked at will be `<rootPath>/redbook/`. The `<dateGroupIndices>` all come from the `<filePattern>` since there is one group in the `<dirPattern>` the groups in the `<filePattern>` start at two. This matches file names `redbook-YYYY-MM-DD-HH.<extension>`. Thus the file name `redbook-2013-05-28-00.hd5` would match the `<filePattern>`.
 
> NOTE group `{0}` is a string that matches the whole `<dirPattern>`. If this is used in the `<displayLabel>` would see every directory that matches the pattern.