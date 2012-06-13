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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Utility methods for handling radar Text Products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2009            askripsk     Initial creation
 * Apr 14, 2010 4734        mhuang      Corrected StdTextProduct import 
 *                                       dependency
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class RadarTextProductUtil {

    private static final int RADAR_CODED_MSG = 74;

    public static final HashMap<Integer, String> radarTable = new HashMap<Integer, String>() {
        private static final long serialVersionUID = 1L;

        {
            put(6, "WSRAAP");
            put(8, "WSRPTL");
            put(9, "WSRASM");
            put(48, "WSRVWP");
            put(58, "WSRSTI");
            put(59, "WSRHAI");
            put(60, "WSRMES");
            put(61, "WSRTVS");
            put(62, "WSRSST");
            put(73, "WSRUAM");
            put(74, "WSRRCM");
            put(75, "WSRFTM");
            put(77, "WSRPTM");
            put(78, "WSROHP");
            put(79, "WSRTHP");
            put(80, "WSRSTP");
            put(82, "WSRSPD");
            put(83, "WSRRMU");
            put(87, "WSRCSH");
            put(88, "WSRCSC");
            put(132, "WSRCLR");
            put(133, "WSRCLD");
            put(139, "WSRMRU");
            put(141, "WSRMDP");
            put(143, "WSRTRU");
            put(144, "WSROSW");
            put(145, "WSROSD");
            put(146, "WSRSSW");
            put(147, "WSRSSD");
            put(150, "WSRUSW");
            put(151, "WSRUSD");
            put(171, "WSRSTA");
        }
    };

    public RadarTextProductUtil() {
    }

    /**
     * Returns the AFOS ID for the specified radar product.
     * 
     * @param productCode
     * @param radarName
     * @param afosId
     * @param radarTable
     * @return
     */
    public static String createAfosId(int productCode, String radarName) {
        String afosId;

        // Check the product code from the message to determine the NNN of the
        // id.
        // Use WSR for the CCC and the last 3 letters of the Radar name for the
        // XXX.
        afosId = radarTable.get(productCode);

        // PATCH FOR HANDLING TWIN LAKES RCM
        if (productCode == RADAR_CODED_MSG) {
            if (radarName == "KOKC" || radarName == "OKC") {
                return "OKCRCMTLX";
            }
        }

        // Normal case
        afosId += radarName;
        return afosId;
    }

    public static List<String> getRadarTableEntries() {
        return new ArrayList<String>(radarTable.values());
    }
}
