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

import com.raytheon.edex.msg.DataURINotificationMessage;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            snaples     Initial creation
 * Feb 12, 2010  4635      snaples     Updated to match more generically.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class SatPreURIFilter {

    DataURINotificationMessage message;

    String outpath = "";

    static final String AUTOSPE = "AUTOSPE";

    public SatPreURIFilter() {
    }

    public void matchURI(DataURINotificationMessage msg) {
        message = msg;

        if (message instanceof DataURINotificationMessage) {
            String uri = "";
            for (String data : message.getDataURIs()) {
                if (data.startsWith("/grid")) {
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
                SatPrecipFileBuilder sb = new SatPrecipFileBuilder(uri);
                sb.createSatPre();
            } else {
                return;
            }
        }
    }
}
