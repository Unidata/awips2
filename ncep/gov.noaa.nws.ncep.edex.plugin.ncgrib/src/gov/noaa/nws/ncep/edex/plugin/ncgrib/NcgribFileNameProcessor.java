/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

/**
 * 
 * Based off a file name for a grib file, determine the modelName, secondaryId,
 * and ensembleId. It is ok not to set any headers because the decoder can find
 * the modelName and ensembleId. Those fields should only be set if the filename
 * has information beyond what is in the actual grib file.The grib decoder will
 * not set a secondary id so if it isn't set here it will be null.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class NcgribFileNameProcessor implements Processor {

    // TODO does this actually match all ensemble patterns?
    // grab all known ensemble ids
    private static final Pattern ENSEMBLE_ID_PATTERN = Pattern
            .compile("^(p|n|ctl)\\d{0,2}$");

    // TODO merge this pattern in with the patterns in GridLookupFileName
    // anything that ends in nest is assumed to be a nested grid identifier
    private static final Pattern FIREWXNEST_ID_PATTERN = Pattern
            .compile("^firewxnest$");

    // TODO merge this pattern in with the patterns in GridLookupFileName
    // anything that ends in nest is assumed to be a nested grid identifier
    private static final Pattern NEST_ID_PATTERN = Pattern.compile("^.*nest$");

    // TODO merge this pattern in with the patterns in GridLookupFileName
    // SREF gets special handling, does this apply to other models?
    private static final Pattern SREF_PATTERN = Pattern.compile("^sref_.*$");

    // TODO merge this pattern in with the patterns in GridLookupFileName
    // This is the least generic pattern ever, are there any constraints on
    // event names, who knows?
    private static final Pattern HURRICANE_PATTERN = Pattern
            .compile("^([a-z]*)\\d{1,2}[lewcs]$");

    @Override
    public void process(Exchange exchange) throws Exception {
        String flName = (String) exchange.getIn()
                .getHeader("CamelFileNameOnly");
        String datasetid = null;
        String secondaryid = null;
        String ensembleid = null;
        String[] nameTokens = flName.split("\\.");
        for (String token : nameTokens) {
            if (ENSEMBLE_ID_PATTERN.matcher(token).find()) {
                ensembleid = token;
            } else if (FIREWXNEST_ID_PATTERN.matcher(token).find()) {
                datasetid = "NAMFIREWX";
                secondaryid = token;
            } else if (NEST_ID_PATTERN.matcher(token).find()) {
                secondaryid = token;
            } else if (SREF_PATTERN.matcher(token).find()) {
                String[] tokens = token.split("_");
                datasetid = tokens[0].toUpperCase();
                secondaryid = tokens[1].toUpperCase();
            } else if (HURRICANE_PATTERN.matcher(token).find()) {
                Matcher matcher = HURRICANE_PATTERN.matcher(token);
                matcher.find();
                secondaryid = matcher.group(1);
                datasetid = "GHM";
                if (nameTokens[2].equalsIgnoreCase("gribn3")) {
                    datasetid = "GHMNEST";
                } else if (nameTokens[2].equalsIgnoreCase("grib6th")) {
                    datasetid = "GHM6TH";
                } else if (nameTokens[2].equalsIgnoreCase("hwrfprs_n")) {
                    datasetid = "HWRFNEST";
                } else if (nameTokens[2].equalsIgnoreCase("hwrfprs_p")) {
                    datasetid = "HWRF";
                }
            }
        }
        datasetid = GridLookupFileName.getInstance().getModelName(flName);
        if (datasetid != null) {
            exchange.getIn().setHeader("datasetid", datasetid);
        }
        if (secondaryid == null) {
            // TODO does everything really need secondaryid or should this only
            // be set for events?
            secondaryid = nameTokens[0];
        }
        exchange.getIn().setHeader("secondaryid", secondaryid);
        if (ensembleid != null) {
            exchange.getIn().setHeader("ensembleid", ensembleid);
        }
    }

}
