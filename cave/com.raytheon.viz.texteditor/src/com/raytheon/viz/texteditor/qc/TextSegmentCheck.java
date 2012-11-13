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
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * 25 AUG 2011  10719      rferrel     Changed ugcPtrn to handle multi-line UGCs
 * 01 SEP 2011  10764      rferrel     Allow multiple bullet types for given Vtec.
 * 20 JUL 2012  15003	   mgamazaychikov	Allow standalone MWS have no headline
 * 											Add vtec to checkHeadline signature
 * 20 JUL 2012  15006	   mgamazaychikov	Do not perform search for a list of 
 * 											county/zones names in the MWS segment heading.
 * 07 NOV 2012  15003	   mgamazaychikov	Do not perform QC check on standalone MWS headline.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class TextSegmentCheck implements IQCCheck {

    private static final Pattern ugcPtrn = Pattern
            .compile("(((\\w{2}[CZ](\\d{3}-){1,}){1,})|(\\d{3}-){1,})(((\\d{2})(\\d{2})(\\d{2})-){0,1})");

    private static Map<String, List<String>> bulletTypeMaps;
    static {
        IPathManager pm = PathManagerFactory.getPathManager();
        File path = pm.getStaticFile("textws/gui/QualityControlCfg.xml");
        QualityControlCfg qcCfg = null;
        try {
            qcCfg = JAXB.unmarshal(path, QualityControlCfg.class);
            bulletTypeMaps = qcCfg.getBulletTypeMap();
        } catch (RuntimeException e) {
            IUFStatusHandler statusHandler = UFStatus
                    .getHandler(QualityControl.class);
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to read/load QualityControlCfg.xml for QC check\n",
                    e);
        }
    }

    @Override
    public String runQC(String header, String body, String nnn) {
        int countyOrZoneCounter = 0;
        int ugcLength = 0;
        int czmType = 1;
        int nb = 0;
        int segmentCount = 0;
        boolean expecHTEC = false;
        boolean expectNamesList = false;
        boolean insideFirstBullet = false;
        boolean secondBulletFound = false;
        boolean checkIC = false;
        boolean countUGC = false;
        boolean headlineFound = false;
        boolean insideLatLon = false;
        boolean countyBased = false;
        VtecObject vtec = null;
        String ic = null;
        String ugc = "";
        String latLon = "";
        String tml = "";
        String headline = "";
        StringBuilder errorMsg = new StringBuilder();
        String segment = "Primary";
        Matcher m = htecPtrn.matcher(body);
        if (m.find()) {
            ic = m.group(1);
        }

        if (nnn.equalsIgnoreCase("FFW") || nnn.equalsIgnoreCase("FFS")
                || nnn.equalsIgnoreCase("FLW") || nnn.equalsIgnoreCase("FLS")) {
            // DR 11060 Need to check if the product is a hydro product
            // hydro products that are zone based do not check the first bullet
            // area count against the UGC count
            czmType = 2;
        } else if (nnn.equalsIgnoreCase("SMW") || nnn.equalsIgnoreCase("MWS")) {
            czmType = 3;
        }

        String[] separatedLines = body.split("\n");

        for (String line : separatedLines) {

            if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
                if (line.contains(TEST_MESSAGE_LABEL)) {
                    line = line.replaceFirst(TEST_MESSAGE_LABEL, "");
                }
            }

            if (line.equals("$$")) {

                if (ugc.length() == 0 && vtec == null) {
                    errorMsg.append("Badly placed segment end.\n");
                    return errorMsg.toString();
                }

                segmentCount++;
                segment = "Secondary";
                ugc = "";

                /*
                 * DR15003 - Add vtec to signature ias n order 
                 * to distinguish between standalone
                 * and followup MWS a check of VTEC is needed.
                 */
                errorMsg.append(checkHeadline(headline, nnn, vtec));
                headline = "";

                if (segmentCount > 1
                        && !QualityControl.segmentedNNN.contains(nnn)) {
                    errorMsg.append("Segments exist in unsegmented product.\n");
                }
                continue;
            }

            m = ugcPtrn.matcher(line);
            if (m.find()) {
                ugc += line;
                countUGC = true;
                continue;
            }

            // Verify UGC line(s) syntax and get count of zones or counties.
            if (countUGC) {
                int countyZoneCnt = 0;
                if (ugc.matches("\\w{2}[CZ]\\d{3}[->].*") == false) {
                    errorMsg.append("First UGC does not specify a zone or county.\n");
                }
                if (ugc.length() > 2 && ugc.charAt(2) == 'C') {
                    ++countyZoneCnt;
                    countyBased = true;
                }
                if (ugc.length() > 2 && ugc.charAt(2) == 'Z') {
                    ++countyZoneCnt;
                }

                if (countyZoneCnt == 0) {
                    errorMsg.append("No zone or county specified in UGCs.\n");
                } else if (countyZoneCnt == 2) {
                    errorMsg.append("Illegal mixture of zone/county UGCs.\n");
                }

                String[] ranges = ugc.replaceFirst("\\d{6}-", "").split("-");
                for (String range : ranges) {
                    if (range.contains(">")) {
                        int index = range.indexOf(">");
                        String left = range.substring(index - 3, index);
                        String right = range.substring(index + 1);
                        int start = Integer.valueOf(left);
                        int end = Integer.valueOf(right);
                        for (int val = start; val <= end; ++val) {
                            ugcLength++;
                        }
                    } else {
                        ugcLength++;
                    }
                }
            }

            m = vtecPtrn.matcher(line);
            if (m.find()) {
                vtec = VtecUtil.parseMessage(line);
                if (vtec.getPhenomena().equals("FF")
                        || vtec.getPhenomena().equals("FL")
                        || vtec.getPhenomena().equals("FA")) {
                    expecHTEC = true;
                }
                if (QualityControl.segmentedNNN.contains(nnn)) {
                    expectNamesList = true;
                }
                countUGC = false;
                continue;
            } else if (countUGC) {
                if (VtecUtil.parseMessage(body) != null) {
                    errorMsg.append(segment).append(
                            " VTEC not right after UGC\n");
                }
                countUGC = false;
            }

            if (expecHTEC) {
                m = htecPtrn.matcher(line);
                if (!m.find()) {
                    errorMsg.append("Hydro VTEC line must follow FF or FL.\n");
                }
                expecHTEC = false;
                continue;
            }

            if (expectNamesList) {
                m = listOfAreaNamePtrn.matcher(line);
                /*
                 * DR15006 - MWS does not have the list of 
                 * marine zones names in the segment heading,
                 * so skip the check for MWS
                 */
                if ( !nnn.equalsIgnoreCase("MWS")) {
                	if (!m.find()) {
                        errorMsg.append("List of county/zone names missing.\n");
                    }
                }                
                expectNamesList = false;
                continue;
            }

            if (line.startsWith("...")) {
                // followup headline found
                headline = line;
                headlineFound = true;
                continue;
            }
            if (line.trim().length() == 0) {
                // blank line?
                headlineFound = false;
                continue;
            } else if (headlineFound) {
                headline += line;
                continue;
            }

            if (line.startsWith("*")) {
                nb++;
            }

            // third bullet
            if (line.startsWith("*") && nb == 3) {
                m = thirdBulletPtrn.matcher(line);
                if (!line.substring(0, 5).equals("* AT ")) {
                    errorMsg.append("Event bullet does not start with '* AT '\n.");
                } else if (!m.find()) {
                    errorMsg.append(
                            "Event bullet starts with badly formatted time\n")
                            .append(" or event bullet does not start with a time.\n");
                }
            }

            // second bullet
            if (line.startsWith("*") && nb == 2) {
                m = secondBulletPtrn.matcher(line);
                if (m.find()) {
                    secondBulletFound = true;
                    insideFirstBullet = false;
                    continue;
                }
            }

            // first bullet
            if (nb == 1) {
                m = firstBulletPtrn.matcher(line);
                if (m.find()) {
                    List<String> bulletTypesList = null;

                    if (vtec != null) {
                        String key = vtec.getPhenomena() + "."
                                + vtec.getSignificance();
                        bulletTypesList = bulletTypeMaps.get(key);
                    }

                    if (bulletTypesList != null) {
                        // Vtec may have Bullet types that do not match the
                        // warning type.
                        boolean badBullet = true;
                        for (String bulletType : bulletTypesList) {
                            if (line.contains(bulletType)) {
                                badBullet = false;
                                break;
                            }
                        }
                        if (badBullet) {
                            errorMsg.append("first bullet not valid for ")
                                    .append(nnn).append("\n");
                        }
                    } else {
                        // bullet type and warning type should match.
                        String warningType = QualityControl
                                .getProductWarningType(nnn).toUpperCase();
                        if (!line.contains(warningType)) {
                            errorMsg.append(nnn).append(
                                    " does not match first bullet.\n");
                        }
                    }
                    insideFirstBullet = true;
                    checkIC = true;
                    continue;
                } else if (!insideFirstBullet
                        && !secondBulletFound
                        && (line.contains("AREA...")
                                || line.contains("AREAS...") || line
                                .contains("AREA WAS..."))) {
                    insideFirstBullet = true;
                    continue;
                }
            }

            if (insideFirstBullet) {
                if (ic != null && !ic.equals("ER") && !ic.equals("MC")
                        && !ic.equals("UU") && checkIC) {
                    boolean validIC = false;
                    for (String causes : QualityControl.getImmediateCauses()) {
                        if (causes.startsWith(ic)
                                && line.contains(causes.split("\\\\")[1])) {
                            validIC = true;
                            break;
                        }
                    }

                    if (!validIC) {
                        errorMsg.append("Immediate cause missing in first bullet\n or is inconsistent with VTEC.\n");
                        // need to return or else it will incorrectly count
                        // counties
                        return errorMsg.toString();
                    }
                    // Immediate cause should be the line after the first bullet
                    checkIC = false;
                    continue;
                }

                if (czmType == 3) {
                    if (line != null && line.trim().startsWith("INCLUDING ")) {
                        insideFirstBullet = false; // stop adding counties/zones
                        continue;
                    }
                } else {
                    if (line.contains("THIS INCLUDES")) {
                        // line indicates cities not counties/zones
                        continue;
                    }

                    boolean invalidCountyOrZone = true;
                    if (ugc.length() > 2 && ugc.charAt(2) == 'Z') {
                        // zones do not use countyTypes
                        if (line.contains(" IN ")) {
                            invalidCountyOrZone = false;
                        }
                    } else {
                        for (String state : QualityControl.getCountyTypeMap()
                                .keySet()) {
                            if (line.contains(state)
                                    && line.contains(QualityControl
                                            .getCountyTypeMap().get(state))) {
                                invalidCountyOrZone = false;
                            }
                        }
                        if (invalidCountyOrZone) {
                            for (String countyType : QualityControl
                                    .getCountyTypeMap().values()) {
                                if (countyType.trim().length() > 0
                                        && line.contains(" "
                                                + countyType.trim())) {
                                    invalidCountyOrZone = false;
                                    break;
                                }
                            }
                        }
                    }
                    if (invalidCountyOrZone && !line.contains(" OF ")) {
                        continue;
                    }

                }

                if (line.trim().length() > 0) {
                    countyOrZoneCounter++;
                    continue;
                } else {
                    // ran into a blank line, done
                    insideFirstBullet = false;
                }
            }

            m = latLonPtrn.matcher(line);
            if (m.find()) {
                latLon = line;
                insideLatLon = true;
                continue;
            }

            if (insideLatLon) {
                m = subLatLonPtrn.matcher(line);

                if (!line.startsWith("TIME...") && m.find()) {
                    latLon += " " + line.trim();
                    continue;
                } else {
                    insideLatLon = false;
                }
            }

            m = tmlPtrn.matcher(line);
            if (m.find()) {
                tml = line;
            }

        }

        if (ugcLength == 0) {
            errorMsg.append("No UGC text was found\n");
        } else if (nb > 0 && (czmType == 1 || (czmType == 2 && countyBased))
                && ugcLength != countyOrZoneCounter) {
            // DR 11060 - Don't count zone areas for hydro products
            errorMsg.append(ugcLength).append(" UGCs while ")
                    .append(countyOrZoneCounter)
                    .append(" counties/zones listed.\n");
            errorMsg.append("Area descriptions count does not\n match UGC count.\n");
        }

        if (body.contains("LAT...LON")) {
            errorMsg.append(checkLatLon(latLon));
        }
        if (body.contains("TIME...MOT...LOC")) {
            errorMsg.append(checkTML(tml));
        }

        return errorMsg.toString();
    }

    private String checkLatLon(String latLon) {
        String errorMsg = "";
        int pairs = 0;
        Pattern p = Pattern.compile("(\\d{3,4})\\s(\\d{3,5})");

        if (latLon.length() == 0) {
            errorMsg += "LAT...LON line is malformed.\n";
            return errorMsg;
        }

        Matcher m = p.matcher(latLon.substring(9));
        while (m.find()) {
            pairs++;
            double lat = Double.parseDouble(m.group(1));
            double lon = Double.parseDouble(m.group(2));
            if (lat > 9000 || lon > 18000) {
                errorMsg += "Data error in the LAT...LON line.\n";
                return errorMsg;
            }
        }

        if (pairs <= 2 || pairs > 20) {
            errorMsg += "LAT...LON line missing or malformed.\n";
        }

        return errorMsg;
    }

    private String checkTML(String tml) {
        String errorMsg = "";
        if (tml.length() == 0) {
            errorMsg += "TIME...MOT...LOC line is malformed.\n";
            return errorMsg;
        }

        return errorMsg;
    }

    private String checkHeadline(String headline, String nnn, VtecObject vtec) {
        String errorMsg = "";
        if (!QualityControl.segmentedNNN.contains(nnn) || nnn.equals("FLS")) {
            // non-follow ups do not have a head line
            return errorMsg;
        }
        /*
         * DR15003: no headline QC on standalone MWS.
         * To distinguish between standalone and follow up MWS
         * the VTEC check is performed as standalone MWS 
         * do not contain VTEC
         */
        if (nnn.equals("MWS") && vtec == null) {
            return "";  	
        }

        if (headline.length() == 0) {
            errorMsg += "Headline is missing or malformed.\n";
        } else if (!headline.endsWith("...")) {
            errorMsg += "Headline should end with '...'.\n";
        }
        return errorMsg;
    }

}
