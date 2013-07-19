package com.raytheon.edex.plugin.grib.filenameprocessor;
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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

/**
 * 
 * Processor for ncep grib files, this processor has lots of hard coded
 * assumptions about file naming that need to be more generic based off ncep
 * file names.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            bsteffen     Initial creation
 * May 29, 2013		995		B. Yin		Get model name from NcgribModelNameMap
 * June, 2013				T. Lee		Added NFCENS
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class NcgribFileNameProcessor implements Processor {

    // grab all known ensemble ids; mainly SREF
    private static final Pattern ENSEMBLE_ID_PATTERN = Pattern
            .compile("^(p|n|ctl)\\d{0,2}$");
    
    // grab global wind and wave ensemble IDs
    private static final Pattern ENSEMBLE_WAVE_PATTERN = Pattern
    		.compile("^gep(\\d{0,2}{2})$");

    // grab global wind and wave ensemble IDs
    private static final Pattern ENSEMBLE_NFC_PATTERN = Pattern
    		.compile("^HTSGW_(\\d{0,2}{2})$"); 
    // anything that ends in nest is assumed to be a nested grid identifier
    // might add alaska fire weather later...
    private static final Pattern FIREWXNEST_ID_PATTERN = Pattern
            .compile("^firewxnest$");

    // anything that ends in nest is assumed to be a nested grid identifier
    //private static final Pattern NEST_ID_PATTERN = Pattern.compile("^.*nest$");

    // SREF gets special handling, does this apply to other models?
    //private static final Pattern SREF_PATTERN = Pattern.compile("^sref_.*$");

    // This is the least generic pattern ever, are there any constraints on
    // event names, who knows?
    private static final Pattern HURRICANE_PATTERN = Pattern
            .compile("^([a-z]*)\\d{1,2}[lewcs]$");
    
    private static NcgribModelNameMap modelMap = null;

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
            } else if (ENSEMBLE_WAVE_PATTERN.matcher(token).find()) {
                Matcher matcher = ENSEMBLE_WAVE_PATTERN.matcher(token);
                matcher.find();
            	ensembleid = matcher.group(1);
            } else if (ENSEMBLE_NFC_PATTERN.matcher(token).find()) {
                Matcher matcher = ENSEMBLE_NFC_PATTERN.matcher(token);
                datasetid = "nfcens";
                matcher.find();
            	ensembleid = matcher.group(1);
            } else if (FIREWXNEST_ID_PATTERN.matcher(token).find()) {
                //datasetid = "NAMFIREWX";
                datasetid = "fireWxNAM";
                //secondaryid = token;
            //} else if (NEST_ID_PATTERN.matcher(token).find()) {
            //    secondaryid = token;
            //} else if (SREF_PATTERN.matcher(token).find()) {
            //    String[] tokens = token.split("_");
            //    datasetid = tokens[0].toUpperCase();
                //secondaryid = tokens[1].toUpperCase();
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
        
    	if ( modelMap == null ) {
    		modelMap = NcgribModelNameMap.load();
    	}
    	
        if (datasetid == null) {
        	datasetid = modelMap.getModelName(flName);
        }

        exchange.getIn().setHeader("datasetid", datasetid);
        exchange.getIn().setHeader("secondaryid", secondaryid);
        exchange.getIn().setHeader("ensembleid", ensembleid);
    }

}
