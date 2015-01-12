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
package com.raytheon.uf.edex.plugin.hpe.process;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

/**
 * Processor for HPE grib files. Adds the filename without the extension as the
 * secondaryId.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2014    3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeGribFileNameProcessor implements Processor {
    /** Filename pattern to match for HPE grib files */
    private static final Pattern FILENAME_PATTERN = Pattern
            .compile("^([A-Za-z]*MOSAIC[0-9]*z)\\.grib$");

    @Override
    public void process(Exchange exchange) throws Exception {
        String fileName = (String) exchange.getIn().getHeader(
                "CamelFileNameOnly");
        Matcher matcher = FILENAME_PATTERN.matcher(fileName);
        if (matcher.matches()) {
            // Take the text before the last "."
            String productName = matcher.group(1);
            exchange.getIn().setHeader("secondaryid", productName);
        }
    }
}
