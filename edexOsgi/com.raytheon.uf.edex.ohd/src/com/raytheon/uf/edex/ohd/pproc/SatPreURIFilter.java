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
package com.raytheon.uf.edex.ohd.pproc;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;

/**
 * Filters URIs for Satellite Precip and generates an xmrg file if the filter
 * matches.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            snaples     Initial creation
 * Feb 12, 2010 4635       snaples     Updated to match more generically.
 * Feb 15, 2013 1638       mschenke    Moved DataURINotificationMessage to uf.common.dataplugin
 * May 05, 2014 2060       njensen     Cleanup
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class SatPreURIFilter {

    private static final String AUTOSPE = "AUTOSPE";

    private String startFilter;

    private SatPrecipFileBuilder builder;

    public SatPreURIFilter() throws Exception {
        startFilter = DataURI.SEPARATOR + GridConstants.GRID;
        builder = new SatPrecipFileBuilder();
    }

    public void matchURI(DataURINotificationMessage msg) throws Exception {
        if (msg instanceof DataURINotificationMessage) {
            String uri = "";
            for (String data : msg.getDataURIs()) {
                if (data.startsWith(startFilter)) {
                    if (data.contains(AUTOSPE)) {
                        uri = data;
                        break;
                    } else {
                        continue;
                    }
                } else {
                    continue;
                }
            }
            if (uri.length() > 1) {
                builder.createSatPre(uri);
            }
        }
    }
}
