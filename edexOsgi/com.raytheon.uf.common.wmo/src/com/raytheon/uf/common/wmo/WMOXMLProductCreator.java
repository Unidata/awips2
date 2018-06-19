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
package com.raytheon.uf.common.wmo;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.UUID;

/**
 * Creates WMO standard meteorological bulletins in XML format
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2017 6549       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class WMOXMLProductCreator {
    private static final String BULLETIN_FORMAT = ""
            + "<MeteorologicalBulletin xmlns=\"http://def.wmo.int/collect/2014\" "
            + "xmlns:gml=\"http://www.opengis.net/gml/3.2\" "
            + "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
            + "xsi:schemaLocation=\"http://def.wmo.int/collect/2014 "
            + "http://schemas.wmo.int/collect/1.2/collect.xsd\" gml:id=\"uuid.%s\">"
            + "%s" + "<bulletinIdentifier>%s</bulletinIdentifier>"
            + "</MeteorologicalBulletin>";

    private static final String BULLETIN_ID_FORMAT = "A_%s%s%s_C_%s_%s%s%s--.xml";

    public static String createBulletin(String wmoId, String cccc,
            Date xmitTime, String[] products) {
        String ddhhmm = new SimpleDateFormat("ddHHmm").format(xmitTime);
        String yyyy = new SimpleDateFormat("yyyy").format(xmitTime);
        String mm = new SimpleDateFormat("MM").format(xmitTime);
        String bulletinId = String.format(BULLETIN_ID_FORMAT, wmoId, cccc,
                ddhhmm, cccc, yyyy, mm, ddhhmm);
        StringBuilder metInfos = new StringBuilder();
        for (String product : products) {
            metInfos.append("<meteorologicalInformation>").append(product)
                    .append("</meteorologicalInformation>");
        }
        return String.format(BULLETIN_FORMAT, UUID.randomUUID(),
                metInfos.toString(), bulletinId);
    }
}
