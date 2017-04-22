---
layout: default
type: guide
shortname: Docs
title: Data distribution files
subtitle: EDEX Admin
---

    
EDEX alerts the appropriate decoding plug-ins of new data by use of XML files that describe data based on regular expressions.  If the WMO header, or file name, matches a regular expression listed in a distribution XML, then EDEX will put a message into the QPID queue for the decoder which indicates that a piece of data is available. If a piece of data does not match any distribution XML, EDEX will:

* Create an entry in `/awips2/edex/logs/edex-ingest-unrecognized-files-yyyymmdd.log`

* Skip processing of the unrecognized file.

Distribution filters are stored in the **edex_static** branch of the Localization Store, and a list of available files can be found in the base-level directory.  

For each plug-in, the distribution file is named `<data-type>.xml`; for example, the distribution file for satellite data is `satellite.xml`. The distribution files follow the AWIPS base/site localization pattern: 
    
    [root@edex]# cd /awips2/edex/data/utility/edex_static/base/distribution
    [root@edex distribution]# ls
    acars.xml         ffg.xml            poessounding.xml
    airep.xml         goessounding.xml   profiler.xml
    airmet.xml        grib.xml           q2.xml
    atcf.xml          hpe.xml            radar.xml
    aww.xml           idft.xml           recco.xml
    binlightning.xml  intlsigmet.xml     redbook.xml
    bufrascat.xml     ldadhydro.xml      satellite.mcidas.xml
    bufrhdw.xml       ldadmanual.xml     satellite.xml
    bufrmos.xml       ldadmesonet.xml    sfcobs.xml
    bufrmthdw.xml     ldadprofiler.xml   shef.xml
    bufrncwf.xml      loctables.xml      svrwx.xml
    bufrquikscat.xml  lsr.xml            taf.xml
    bufrsgwhv.xml     mcidas.xml         tcg.xml
    bufrsgwh.xml      modelsounding.xml  tcm.xml
    bufrsigwx.xml     mosaic.xml         tcs.xml
    bufrssha.xml      ncccfp.xml         textlightning.xml
    bufrssmi.xml      ncgrib.xml         text.xml
    bufrua.xml        ncscat.xml         uair.xml
    ccfp.xml          nctext.xml         vaa.xml
    convsigmet.xml    nonconvsigmet.xml  warning.xml
    cwa.xml           obs.xml            wcp.xml
    dhr.xml           pafm.xml
    dpa.xml           pirep.xml

* base files are located in **/awips2/edex/data/utility/edex_static/base/distribution **

* site override distribution files are located in **/awips2/edex/data/utility/edex_static/site/XXX/distribution**, where **XXX** is the site identifier.

Note that site-level files override the base files; as a result, local modifications to distribution files must be made as follows:

1. The base distribution file must be copied from **/awips2/edex/data/utility/edex_static/base/distribution** to **/awips2/edex/data/utility/edex_static/site/XXX/distribution**

2. The local modification must be made to the file in **/awips2/edex/data/utility/edex_static/site/XXX/distribution**

The basic structure of the distribution file is:

    <requestPatterns xmlns:ns2="group">
        <regex>pattern</regex>
        <regex>pattern</regex>
    </requestPatterns>

In each **<regex></regex> **tag, **_pattern_** is replaced with a regular expression that will match either the filename or the WMO header of the raw data. Only data that matches a pattern in the distribution file will be processed.

The contents of the base version of the radar distribution file:

    [root@edex]# cd /awips2/edex/data/utility/edex_static/base/distribution/
    [root@edex]# tail -4 radar.xml
    
    <requestPatterns xmlns:ns2="group">
        <regex>^SDUS[234578]. .*</regex>
        <regex>^RadarServer.*</regex>
    </requestPatterns>

Looking at the base radar.xml distribution file in the previous example, there are two regular expressions.  The first regular expression matches the standard WMO ID of radar products.   Via the edexBridge the LDM will place a message in the external.dropbox QPID queue, indicating a radar product has arrived.  EDEX will then take the message containing the radar WMO ID and compare it against the regular expressions in radar.xml.  If a match is found, EDEX places a message in the QPID queue Ingest.radar.  The radar decoder will then consume the message and process the radar data accordingly. 


# Adding a REGEX to the Satellite Data Distribution File

As a quick example, suppose we have a local data source for satellite imagery that does not have a WMO header; also suppose that the data source writes to files whose names start with **LOCAL.sat**.

To add this locally produced satellite data file to the EDEX distribution; perform the following steps.

1. Copy the base version of **satellite.gini.xml** from the base distribution directory **/awips2/edex/data/utility/edex_static/base/distribution** into the site distribution directory **/awips2/edex/data/utility/edex_static/site/XXX/distribution**

2. Edit the site version of **satellite.xml**, adding a new `<regex></regex>` tag immediately below the existing regular expression (`<regex></regex>`) tag. The contents of the tag will be **LOCAL.sat**. The final result will be:

        <requestPatterns xmlns:ns2="group">
            <regex>TI[CGT]... ....</regex>
            <regex>.*NEXRCOMP.*</regex>
            <regex>^LOCAL.sat.*</regex>
        </requestPatterns>

3. Save the file and exit the editor. EDEX will automatically pick up the new distribution pattern.

Raw files are written to **/data_store**, and a message is sent via QPID to the EDEX distribution service from the LDM. When a regular expression match is found in a data distribution file, the raw data file is placed in a queue for the matching plugin to decode and process. The distribution files are used to match file headers as well as filenames, which is how files dropped into EDEXâ€™s manual endpoint (**/awips2/data_store/ingest**) are processed.

# Editing an EDEX Data Distribution File

Because these files are in the **edex_static/** directory, they have to be manually edited using a text editor. You should not edit the base files; rather, you should copy the base version to your site and then edit the site version. The regular expressions in the distribution files need to correspond with the regular expressions in the LDM **_pqact.conf_**** **file.

If patterns exist in **_pqact.conf_** but are not in the distribution files, then raw data files will be written to **/data_store** but will not be ingested and processed by EDEX. Entries for these non-ingested files would be written to the unrecognized files log in **/awips/edex/logs**.

# EDEX Data Distribution File Examples

## Surface Obs

**/awips2/edex/data/utility/edex_static/base/distribution/obs.xml** Processes any file header that starts with **SA** or **SP**, which should match any WMO header that contains METAR data (e.g.**SAUS**, **SPUS**, **SACN**, **SAMX**).

    <requestPatterns xmlns:ns2="group">
      <regex>^S[AP].*</regex>
    </requestPatterns> 

## Text Data

**/awips2/edex/data/utility/edex_static/base/distribution/text.xml** Processes lots of WM patterns. The second pattern ^S[A-CEG-Z].* matches any header that starts with **S** except for **SD**or **SF**, so it also matches the **SA** and **SP** files that the **obs.xml** plugin matches. This means that METARs are processed by both plugins simultaneously.

    <requestPatterns>
      <regex>^[ACFNRUW][A-Z].*</regex>
      <regex>^S[ACEG-Z].*</regex>
      <regex>^T[BCX].*</regex>
      <regex>^SF[A-OQ-TVZ].*</regex>
      <regex>^SDUS1.*</regex>
      <regex>^SDUS4[1-6].*</regex>
      <regex>^SDUS9[^7].*</regex>
      <regex>^SFU[^S].*</regex>
      <regex>^SFUS4[^1].*</regex> 
      <regex>^SFP[^A].*</regex>
      <regex>^SFPA[^4].*</regex> 
      <regex>^SFPA4[^1].*</regex>
      <regex>^BMBB91.*</regex> 
      <regex>^N.*</regex>
      <regex>^F[EHIJKLMQVWX].*</regex> 
    </requestPatterns> 

## Grib Data

**/awips2/edex/data/utility/edex_static/base/distribution/grib.xml** The grib/grid decoder distribution file matches all numerical grids distributed over the IDD NGRID feed by matching WMO header, and from CONDUIT by matching the *.grib* file extension.

    <requestPatterns>
        <!-- Super Set of all possible WMO grib patterns -->
        <regex>^[EHLMOYZ][A-Z]{3}\d{2}</regex>
        <!-- This to match Unidata CONDUIT products w/o standard headers -->
        <regex>.*grib.*</regex>
        <regex>^US058.*</regex>
        <regex>^CMC_reg.*</regex>
    </requestPatterns>

Important notes about regular expressions:

* Any time a new entry is placed in the **pqact.conf** file on LDM, a corresponding entry needs to be added to the appropriate Data Distribution file in the data distribution directory, or the data file will be logged to **edex-ingest-unrecognized-files-YYYYMMDD.log**

* Any time an entry is removed from the **pqact.conf** file, the corresponding entry should be removed from the appropriate Data Distribution file in the data distribution directory.
