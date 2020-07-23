# Purging and Retention

## Purge Types

There are two main forms of data puring in AWIPS.  The most often thought of is the purging for [**processed data**](#processed-data-purging).  This has to do with how long data is stored for **after** it has been decoded and processed.

The second type of purging has to do with [**raw data**](#raw-data-purging).  This has to do with how long data is stored for **before** it has been decoded.

## Processed Data Purging

AWIPS uses a plugin-based purge strategy for processed **HDF5 data**.  This allows the user to change the purge frequency for each plugin individually, and even set purge rules for specific products for a particular plugin.  There is also a default purge rules file for those products which do not have specific rules written.

> **Note**: Purging is triggered by a quartz timer event that fires at 30 minutes after each hour. 

Purging rules are defined in XML files in the Localization Store.  On EDEX, most are located in `/awips2/edex/data/utility/common_static/base/purge`, and follow the **base/site** localization pattern (e.g. site purge files are in `site/XXX/purge` rather than `base/purge`, where XXX is the site identifier).

Each data set can have a purge rule defined, and the xml file is named after the data set:

    ls /awips2/edex/data/utility/common_static/base/purge/
    
    acarsPurgeRules.xml          bufruaPurgeRules.xml         pirepPurgeRules.xml
    acarssoundingPurgeRules.xml  ccfpPurgeRules.xml           poessoundingPurgeRules.xml
    aggregatePurgeRules.xml      convsigmetPurgeRules.xml     pointsetPurgeRules.xml
    airepPurgeRules.xml          cwaPurgeRules.xml            profilerPurgeRules.xml
    ...

---

### Time-based purge

If a plugin has no XML file, the default rule of 1 day (24 hours) is used, from `/awips2/edex/data/utility/common_static/base/purge/defaultPurgeRules.xml`:
<!--
Use html <pre> tag to be able to bold a specific section.  Make sure to replace '<' with '&lt;' and '>' with '&gt;'.
-->
<pre>
&lt;purgeRuleSet&gt;
    &lt;defaultRule&gt;
        <b>&lt;period&gt;01-00:00:00&lt;/period&gt;</b>
    &lt;/defaultRule&gt;
&lt;/purgeRuleSet&gt;
</pre>

Time-based purging is set with the *period* tag and uses the *reference time* of the data. The reference time of the data is determined by the decoder. 

---

### 30-day NEXRAD3 Example

Modify `/awips2/edex/data/utility/common_static/base/purge/radarPurgeRules.xml` to increase the data retention period from 1 to 31 days:

<pre>
&lt;purgeRuleSet&gt;
        &lt;defaultRule&gt;
                <b>&lt;period&gt;31-00:00:00&lt;/period&gt;</b>
        &lt;/defaultRule&gt;
&lt;/purgeRuleSet&gt;
</pre>

>**Note**: you do NOT have to restart EDEX when you change a purge rule!

---

### Frame-Based Purge

Some plugins use frame-base purging, retaining and certain number of product "versions". 

`/awips2/edex/data/utility/common_static/base/purge/gridPurgeRules.xml`
    
<pre>    
<b>&lt;defaultRule&gt;
    &lt;versionsToKeep&gt;2&lt;/versionsToKeep&gt;</b>
    &lt;period&gt;07-00:00:00&lt;/period&gt;
  &lt;/defaultRule&gt;
  &lt;rule&gt;
    <b>&lt;keyValue&gt;LAPS&lt;/keyValue&gt;
    &lt;versionsToKeep&gt;30&lt;/versionsToKeep&gt;</b>
  &lt;/rule&gt;
  &lt;rule regex="true"&gt;
    <b>&lt;keyValue&gt;NAM(?:12|20|40)&lt;/keyValue&gt;
    &lt;versionsToKeep&gt;2&lt;/versionsToKeep&gt;
    &lt;modTimeToWait&gt;00-00:15:00&lt;/modTimeToWait&gt;</b>
  &lt;/rule&gt;
  ...
</pre>

In the above example, notice a *default rule* (2) is specified, as well as specific models with their own rules.  
The tag *modTimeToWait* can be used in conjunction with *versionsToKeep* and will increase the versionsToKeep by 1 if data matching this rule has been stored within modTimeToWait.

---

### Purge Logs

Data purge events are logged to the file `edex-ingest-purge-[yyyymmdd].log`, where `[yyyymmdd]` is the date stamp. 


    tail -f edex-ingest-purge-20120327.log 
    
    --------START LOG PURGE---------
    INFO  2012-03-27 00:30:00,027 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Skipped file with invalid fileName: afos-trigger.log
    INFO  2012-03-27 00:30:00,193 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Removed 1 old files
    INFO  2012-03-27 00:31:23,155 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Archived 14 files
    INFO  2012-03-27 00:31:23,155 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Skipped processing 1 files
    INFO  2012-03-27 00:31:23,155 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::---------END LOG PURGE-----------

---

### All Purge Rules

To see all purge rule directories (base, site, configured):

    find /awips2/edex/data/utility -name purge
    
    /awips2/edex/data/utility/common_static/base/purge

If any overrides have been made, then it's possible that *site* directories may show up as results from the find command as well.

---

## Raw Data Purging

Raw data are files that have been brought in by the LDM and recognized by an action in the **pqact.conf** file.  These files are written to subdirectories of `/awips2/data_store/`.  This data will wait here until it is purged, from the purging rules defined in `/awips2/edex/data/utility/common_static/base/archiver/purger/RAW_DATA.xml`.

If the purge time is too short, and the processing latencies on EDEX are too long, it is possible that EDEX will miss some of this data, and the purge times will need to be adjusted by changing the [`<defaultRetentionHours>`](#default-retention) or [`<selectedRetentionHours>`](#selected-retention) tag on the relevent data sets.

---

### Default Retention

The **defaultRetentionHours** tag is defined at the beginning of the RAW_DATA.xml file.  It is the duration that will apply to any piece of data that does not fall under an explicitly defined [category](#selected-retention).

The default value for our EDEX is 1 hour:

<pre>
&lt;archive&gt;
  &lt;name&gt;Raw&lt;/name&gt;
  &lt;rootDir&gt;/awips2/data_store/&lt;/rootDir&gt;
  <b>&lt;defaultRetentionHours&gt;1&lt;/defaultRetentionHours&gt;</b>
  &lt;category&gt;
  ...
</pre>
    
---

### Selected Retention

Data sets are broken up into *categories* in the RAW_DATA.xml file.  These categories are groupings of similar data.  Each category has a **selectedRetentionHours** tag which specifies how long the matching data will be kept for.

For example, there is a **Model** category which sets the purge time to 3 hours for all grib, bufrmos, and modelsounding data:

<pre>
...
<b>&lt;category&gt;
    &lt;name&gt;Model&lt;/name&gt;
    &lt;selectedRetentionHours&gt;3&lt;/selectedRetentionHours&gt;</b>
    &lt;dataSet&gt;
      <b>&lt;dirPattern&gt;(grib|grib2)/(\d{4})(\d{2})(\d{2})/(\d{2})/(.*)&lt;/dirPattern&gt;</b>
      &lt;displayLabel&gt;{1} - {6}&lt;/displayLabel&gt;
      &lt;dateGroupIndices&gt;2,3,4,5&lt;/dateGroupIndices&gt;
    &lt;/dataSet&gt;
    &lt;dataSet&gt;
      <b>&lt;dirPattern&gt;(bufrmos|modelsounding)/(\d{4})(\d{2})(\d{2})/(\d{2})&lt;/dirPattern&gt;</b>
      &lt;displayLabel&gt;{1}&lt;/displayLabel&gt;
      &lt;dateGroupIndices&gt;2,3,4,5&lt;/dateGroupIndices&gt;
    &lt;/dataSet&gt;
&lt;/category&gt;
...
</pre>

### Logging

Raw data purging can be seen in the **Ingest** logs (`/awips2/edex/logs/edex-ingest-[yyyymmdd].log` where `[yyyymmdd]` is the date stamp).

    [centos@tg-atm160027-edex-dev purge]$ grep -i 'archive' /awips2/edex/logs/edex-ingest-20200723.log
    INFO  2020-07-23 01:40:00,002 2588 [DefaultQuartzScheduler_Worker-10] CurrentTimeClusterLockHandler: EDEX - Overriding lock for cluster task [ClusteredQuartz/clusteredquartz://archive/archiveScheduled/?cron=0+40+*+*+*+%3F] time out [1800000] exceeded by 1800000 ms.
    INFO  2020-07-23 01:40:00,025 2592 [DefaultQuartzScheduler_Worker-10] JmsPooledProducer: EDEX - Creating AMQ producer archiveScheduledWork
    INFO  2020-07-23 01:42:44,188 7793 [Camel (camel) thread #2 - timer://jmsPooledResourceCheck] JmsPooledProducer: EDEX - Closing AMQ producer archiveScheduledWork
    INFO  2020-07-23 04:05:00,002 9517 [DefaultQuartzScheduler_Worker-10] CurrentTimeClusterLockHandler: EDEX - Overriding lock for cluster task [ClusteredQuartz/clusteredquartz://archive/archivePurgeScheduled/?cron=0+5+0%2F2+*+*+%3F] time out [3599999] exceeded by 3600001 ms.
    INFO  2020-07-23 04:05:00,020 9521 [DefaultQuartzScheduler_Worker-10] JmsPooledProducer: EDEX - Creating AMQ producer archivePurgeScheduledWork
    INFO  2020-07-23 04:07:46,389 1638 [Camel (camel) thread #2 - timer://jmsPooledResourceCheck] JmsPooledProducer: EDEX - Closing AMQ producer archivePurgeScheduledWork
    ...
