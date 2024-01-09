# Data Distribution Files

## Overview

EDEX uses **distribution files** to alert the appropriate decoding plug-in that new data has been recieved.  These files do so by use of XML and regular expressions.  If the WMO header, or file name*, matches a regular expression listed in a distribution XML, then EDEX will put a message into the QPID queue for its corresponding decoder to recognize and process.  It is worth noting that more than one distribution file can recognize a single peice of data and notify their decoders to act.

!!! note "Sometimes the distribution file will not look at the filename.  If this file is coming in through the LDM using a proper FILE action, then the it is possible the distribution file will only look at the header and not the filename.  If the file is ingested using the *manual* endpoint (/awips2/data_store/ingest/), then this behaviour could be different."

If a piece of data **does not** match any distribution XML, EDEX will:

* Create an entry in `/awips2/edex/logs/edex-ingest-unrecognized-files-yyyymmdd.log`

* Skip processing of the unrecognized file.

Distribution files are stored in the **common_static** branch of the *Localization Store* (series of directories that exist in **/awips2/edex/data/utility/**), and a list of available files can be found in the base-level directory.  The base directory is: `/awips2/edex/data/utility/common_static/base/distribution/`.

For each plug-in, the distribution file is named `[data-type].xml`.  For example, the distribution file for radar data is `radar.xml`. The distribution files follow the AWIPS base/site localization pattern: 
    
    [root@edex]# cd /awips2/edex/data/utility/common_static/base/distribution
    [root@edex distribution]# ls
    acars.xml         goesr.xml            poessounding.xml
    airep.xml         goessounding.xml     profiler.xml
    airmet.xml        grib.xml             radar.xml
    atcf.xml          intlsigmet.xml       redbook.xml
    aww.xml           lsr.xml              satellite.gini.xml
    ...

---

## Creating a Site Override

* *Base* files are located in **/awips2/edex/data/utility/common_static/base/distribution/**

* *Site* override distribution files are located in **/awips2/edex/data/utility/common_static/_site/XXX_/distribution/**, where **_XXX_** is the site identifier.

Note that site-level files override the base files; as a result, local modifications to distribution files must be made as follows:

1. The base distribution file must be copied from **/awips2/edex/data/utility/common_static/base/distribution** to **/awips2/edex/data/utility/common_static/site/XXX/distribution**

2. The local modification must be made to the file in **/awips2/edex/data/utility/common_static/site/XXX/distribution**

The basic structure of the distribution file is:

    <requestPatterns xmlns:ns2="group">
        <regex>[pattern]</regex>
        <regex>[pattern]</regex>
    </requestPatterns>

In each **<regex></regex> **tag, **_[pattern]_** is replaced with a regular expression that will match either the filename or the WMO header of the raw data. Only data that matches a pattern in the distribution file will be processed.

The contents of the base version of the radar distribution file:

    [root@edex]# cd /awips2/edex/data/utility/common_static/base/distribution/
    [root@edex]# tail -4 radar.xml
    
    <requestPatterns >
        <regex>^SDUS[234578]. .*</regex>
        <regex>^Level3.*</regex>
    </requestPatterns>

Looking at the base radar.xml distribution file in this example, there are two regular expressions.  The first regular expression matches the standard WMO ID of radar products.   Using edexBridge the LDM will place a message in the **external.dropbox** QPID queue, indicating a radar product has arrived.  EDEX will then take the message containing the radar WMO ID (which comes from the file header) and compare it against the regular expressions in radar.xml.  If a match is found, EDEX places a message in the QPID queue Ingest.radar.  The radar decoder will then consume the message and process the radar data accordingly. 

---

## Adding a REGEX to the Satellite Data Distribution File

As a quick example, suppose we have a local data source for satellite imagery that does not have a WMO header; also suppose that the data source writes to files whose names start with **LOCAL.sat**.

To add this locally produced satellite data file to the EDEX distribution; perform the following steps.

1. Copy the base version of **satellite.gini.xml** from the base distribution directory **/awips2/edex/data/utility/common_static/base/distribution** into the site distribution directory **/awips2/edex/data/utility/common_static/site/XXX/distribution**

2. Edit the site version of **satellite.gini.xml**, adding a new `<regex> </regex>` tag immediately below the existing regular expression (`<regex> </regex>`) tag. The contents of the tag will be **^LOCAL.sat**. The final result will be:
<!---
Note: this next code block is formatted using html so that one of the lines of code can be bolded for emphasis.  This is why &lt; and &gt; are used instead of < and >.
-->
<pre>
  &lt;requestPatterns xmlns:ns2="group"&gt;
      &lt;regex&gt;TI[CGT]... ....&lt;/regex&gt;
      &lt;regex&gt;rad/NEXRCOMP&lt;/regex&gt;
      &lt;regex&gt;.\*.gini.\*&lt;/regex&gt;
      <b>&lt;regex&gt;^LOCAL.sat.*&lt;/regex&gt;</b>
  &lt;/requestPatterns&gt;
</pre>

3. Save the file and exit the editor. EDEX will automatically pick up the new distribution pattern.

Raw files are written to subdirectories in  **/awips2/data_store/**, and a message is sent via QPID to the EDEX distribution service from the LDM. When a regular expression match is found in a data distribution file, the raw data file is placed in a queue for the matching plugin to decode and process. The distribution files are used to match file headers as well as filenames, which is how files dropped into EDEX's manual endpoint (**/awips2/data_store/ingest/**) are processed.

---

## Editing an EDEX Data Distribution File

Because these files are in the **common/_static** directory, they have to be manually edited using a text editor. You should not edit the base files; rather, as stated above, [you should copy the base version to your site and then edit the site version](#creating-a-site-override). 

The regular expressions in the distribution files do not necessarily need to correspond with the regular expressions in the LDM **_pqact.conf_** file.  It is important to note that:

  * The regex in the **pqact.conf** file applies to the *productID* that is passed through the LDM.

and

* The regex in the **distribution** files (.xml) typically applies to the header in the file.  It can also apply to the filename, if the file is coming through the manual endpoint, or if the data has no header to begin with.

If patterns exist in **_pqact.conf_** but there are no corresponding matching regex expressions in any distribution file, then raw data files will be written to **/awips2/data_store/** but will not be ingested and processed by EDEX. Entries for these non-ingested files would be written to the unrecognized files log in **/awips/edex/logs**.

---

## Examples

### Surface Obs

Its distribution file is located at: **/awips2/edex/data/utility/common_static/base/distribution/obs.xml**:
  
    <requestPatterns xmlns:ns2="group">
      <regex>^S[AP].*</regex>
    </requestPatterns> 

It will process any file header that starts with **SA** or **SP**, which should match any WMO header that contains METAR data (e.g.**SAUS**, **SPUS**, **SACN**, **SAMX**).

---

### Text Data

Its distribution file is located at **/awips2/edex/data/utility/common_static/base/distribution/text.xml**:

    <requestPatterns>
      <regex>^[ACFNRUW][A-Z][A-Z0-9]{4} [A-Z0-9]{4}</regex>
      <regex>^S[A-CEG-Z].*</regex>
      <!-- Only have AFOS mapping for T[BCX] -->
      <regex>^T[BCX].*</regex>
      <regex>^SF[A-OQ-TV-Z].*</regex>
      <regex>^SDUS1.*</regex>
      <regex>^SDUS4[1-6].*</regex>
      <regex>^SDUS9[^7].*</regex>
      <regex>^SFU[^S].*</regex>
      <regex>^SFUS4[^1].*</regex>
      <regex>^SFP[^A].*</regex>
      <regex>^SFPA[^4].*</regex>
      <regex>^SFPA4[^12].*</regex>
      <regex>^BMBB91.*</regex>
      <regex>^N[A-Z][A-Z0-9]{4} [A-Z0-9]{4}</regex>
      <regex>^F[EHIJKLMQVWX].*</regex>
      <regex>wcl_decrypted</regex>
      <regex>ecmwf_mos_decrypted</regex>
    </requestPatterns> 

Processes lots of WM patterns. The second pattern ^S[A-CEG-Z].\* matches any header that starts with **S** except for **SD** or **SF**.  This is because it matches A through C (*A-C*), E, and G through Z (*G-Z*).  So it also matches the **SA** and **SP** files that the **obs.xml** plugin matches. This means that METARs are processed by both plugins simultaneously.

---

### Grib Data

Its distribution file is located at **/awips2/edex/data/utility/common_static/base/distribution/grib.xml**:

    <requestPatterns>
        <!-- Super Set of all possible WMO grib patterns -->
        <!-- Is specifically not restricting on CCCC since HPE isn't populating it -->
        <regex>^[EHLMOYZ][A-Z]{3}\d{2}</regex>
        <!-- Exclude Data Delivery specific patterns -->
        <regexExclude>^LZ[ABC][ABC]9[123] (KWBC|KNCF)</regexExclude>

        <!-- ECMWF decrypted -->
        <regex>ecmwf_decrypted</regex>

        <!-- NWPS pattern -->
        <regex>\p{Alpha}{3}_nwps_CG1</regex>
        <regex>\p{Alpha}{3}_nwps_CG0_Trkng</regex>

        <!--  grib files without WMO headers -->
        <regex>.*grib.*</regex>
        <regex>.*GRIB.*</regex>
        <regex>.*grb.*</regex>
        <regex>^US058.*</regex>
        <regex>^CMC_reg.*</regex>
    </requestPatterns>

The grib/grid decoder distribution file matches all numerical grids distributed over the IDD NGRID feed by matching WMO header, and from CONDUIT by matching various *.grib* file extensions.  It also includes an example of a *regexExclude* message which can be used to single out matching values that aren't to be included.

---

### Addtional Information

Important notes about regular expressions:

* Any time a new entry is placed in the **pqact.conf** file on LDM, it is likely a corresponding entry needs to be added to the appropriate Data Distribution file in the data distribution directory, or the data file will be logged to **edex-ingest-unrecognized-files-YYYYMMDD.log**.  

!!! note "The exception to this rule is if the new data coming from the LDM is a type of data that already exists and EDEX already has a distribution file with a matching regex that will recognize it."

* If you have written a new regex for a distribution file to match on a filename, and it is not matching, then the file most likely has a header. In this case EDEX will only look at the header to match the regex.  You must change your regex to something that matches the header, not the filename.
