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
package com.raytheon.viz.texteditor.qc;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2011  10764      rferrel     Use QualityControlCfg.xml for
 *                                     configuable information.
 * Apr 29, 2013 3033       jsanchez    Updated method to retrieve files in localization.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class QualityControl {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QualityControl.class);

    /** Maps warning PILs to appropriate warning text */
    private static Map<String, String> productTypeMap;

    private static Map<String, String> followupNNN;

    private static Map<String, String> nnnOfIdent;

    private static Map<String, String> countyTypes;

    public static ArrayList<String> segmentedNNN;

    private static String[] immediateCause;

    private String errorMsg;

    static {

        try {
            QualityControl.loadQualityControlCfg();
            String file = WarnFileUtil.convertFileContentsToString("countyTypes.txt", null, null);
            countyTypes = new HashMap<String, String>();
            for (String line : file.split("\n")) {
                String[] parts = line.split("\\\\");
                String key = parts[0].trim();
                String value = parts.length < 2 || parts[1] == null ? ""
                        : parts[1];
                countyTypes.put(key, value);
            }
        } catch (Exception pokemon) {
            // got to catch them all
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to read/load countTypes.txt for QC check\n",
                    pokemon);
        }
    }

    private static void loadQualityControlCfg() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile("textws/gui/QualityControlCfg.xml");
            QualityControlCfg qcCfg = null;
            qcCfg = JAXB.unmarshal(path, QualityControlCfg.class);
            immediateCause = qcCfg.getImmediateCause();
            followupNNN = qcCfg.getFollowupNNN();
            nnnOfIdent = qcCfg.getNnnOfIdent();
            productTypeMap = qcCfg.getProductTypeMap();
            segmentedNNN = qcCfg.getSegmentedNNN();
        } catch (RuntimeException ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to read/load QualityControlCfg.xml for QC check\n",
                    ex);
        }
    }

    /**
     * 
     * @param header
     * @param body
     * @param nnn
     * @return true, if it passes the QC check
     */
    public boolean checkWarningInfo(String header, String body, String nnn) {
        String[] productTypes = new String[] { "TOR", "SVR", "SMW", "FFW",
                "EWW", "SVS", "FFS", "FLS", "FLW", "MWS" };
        List<String> qcList = Arrays.asList(productTypes);
        if (!qcList.contains(nnn)) {
            return true;
        }

        IQCCheck[] checks = new IQCCheck[] { new WmoHeaderCheck(),
                new MndHeaderCheck(), new TextSegmentCheck(),
                new TimeConsistentCheck(), new CtaMarkerCheck(),
                new TwoDollarCheck() };

        errorMsg = "";
        for (IQCCheck check : checks) {
            errorMsg = check.runQC(header, body, nnn);
            if (errorMsg.length() > 0) {
                errorMsg += "\nPlease fix this problem so complete\n check of text can be performed.\n";
                return false;
            }
        }

        return true;
    }

    public static String getProductWarningType(String nnn) {

        String warningType = null;

        if (productTypeMap.containsKey(nnn)) {
            warningType = productTypeMap.get(nnn);
        } else {
            warningType = "Unknown Warning";
        }

        return warningType;
    }

    public static boolean match(String nnn, String phensig) {
        String mappedNnn = "";

        if (segmentedNNN.contains(nnn)) {
            mappedNnn = followupNNN.get(phensig);
        } else {
            mappedNnn = nnnOfIdent.get(phensig);
        }

        return mappedNnn != null && mappedNnn.equals(nnn);
    }

    public static String[] getImmediateCauses() {
        return immediateCause;
    }

    public static Map<String, String> getCountyTypeMap() {
        return countyTypes;
    }

    public String getErrorMessage() {
        return errorMsg;
    }

    public static void main(String[] args) {
        int pairs = 0;
        String latLon = "LAT...LON 4149 9717 4149 9678 4121 9679 4121 9716";
        Pattern p = Pattern.compile("(\\d{3,4})\\s(\\d{3,5})");

        Matcher m = p.matcher(latLon.substring(9));
        while (m.find()) {
            pairs++;
            double lat = Double.parseDouble(m.group(1));
            double lon = Double.parseDouble(m.group(2));
            if (lat > 9000 || lon > 18000) {
                // Data error in the LAT...LON line.\n";
                break;
            }
        }

        if (pairs <= 2 || pairs > 20) {
            // LAT...LON line missing or malformed.\n";
        }
    }
}
