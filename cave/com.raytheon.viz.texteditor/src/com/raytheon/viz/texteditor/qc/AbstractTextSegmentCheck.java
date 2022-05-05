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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.viz.vtec.VtecObject;
import com.raytheon.uf.viz.vtec.VtecUtil;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * Abstract base class containing common functions and constants used by the
 * text segment QC checks.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2017  #6252     dgilling     Initial creation
 *
 * </pre>
 *
 * @author dgilling
 */

public abstract class AbstractTextSegmentCheck implements IQCCheck {

    /*
     * DR 11060 Need to check if the product is a hydro product hydro products
     * that are zone based do not check the first bullet area count against the
     * UGC count
     */
    protected static final Collection<String> COUNTY_ZONE_TYPE_2_PILS = new HashSet<>(
            Arrays.asList("FFW", "FLW", "FLS", "FFS"));

    protected static final Collection<String> COUNTY_ZONE_TYPE_3_PILS = new HashSet<>(
            Arrays.asList("SMW", "MWS"));

    /*
     * List of immediate causes to be excluded from quality control check in the
     * first bullet
     */
    protected static final Collection<String> IMMEDIATE_CAUSE_EXCLUSIONS = new HashSet<>(
            Arrays.asList("ER", "MC", "UU", "IC"));

    @Override
    public String runQC(String header, String body, String nnn) {
        header = header.toUpperCase();
        body = body.toUpperCase();

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
        Matcher m = QCCheckConstants.HVTEC_PATTERN.matcher(body);
        if (m.find()) {
            ic = m.group(1);
        }

        nnn = nnn.toUpperCase();
        if (COUNTY_ZONE_TYPE_2_PILS.contains(nnn)) {
            /*
             * DR 11060 Need to check if the product is a hydro product hydro
             * products that are zone based do not check the first bullet area
             * count against the UGC count
             */
            czmType = 2;
        } else if (COUNTY_ZONE_TYPE_3_PILS.contains(nnn)) {
            czmType = 3;
        }

        Collection<String> countyParishMunicipality = getZoneTypes();

        for (String line : body.split("\n")) {
            if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
                if (line.contains(QCCheckConstants.TEST_MESSAGE_LABEL)) {
                    line = line.replaceFirst(
                            QCCheckConstants.TEST_MESSAGE_PATTERN,
                            StringUtils.EMPTY);
                }
            }

            if (line.equals("$$")) {

                if ((ugc.length() == 0) && (vtec == null)) {
                    errorMsg.append("Badly placed segment end.\n");
                    return errorMsg.toString();
                }

                segmentCount++;
                segment = "Secondary";
                ugc = "";

                /*
                 * DR15003 - Add vtec to signature ias n order to distinguish
                 * between standalone and followup MWS a check of VTEC is
                 * needed.
                 */
                errorMsg.append(checkHeadline(headline, nnn, vtec));
                headline = "";

                if ((segmentCount > 1) && (!isSegmented(nnn, vtec))) {
                    errorMsg.append("Segments exist in unsegmented product.\n");
                }
                continue;
            }

            m = QCCheckConstants.UGC_PATTERN.matcher(line);
            if (m.find() && (m.start() != m.end())) {
                ugc += line;
                countUGC = true;
                continue;
            }

            // Verify UGC line(s) syntax and get count of zones or counties.
            if (countUGC) {
                int countyZoneCnt = 0;
                if (ugc.matches("\\w{2}[CZ]\\d{3}[->].*") == false) {
                    errorMsg.append(
                            "First UGC does not specify a zone or county.\n");
                }
                if ((ugc.length() > 2) && (ugc.charAt(2) == 'C')) {
                    ++countyZoneCnt;
                    countyBased = true;
                }
                if ((ugc.length() > 2) && (ugc.charAt(2) == 'Z')) {
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

            m = QCCheckConstants.VTEC_PATTERN.matcher(line);
            if (m.find()) {
                vtec = VtecUtil.parseMessage(line);
                if (vtec.getPhenomena().equals("FF")
                        || vtec.getPhenomena().equals("FL")
                        || vtec.getPhenomena().equals("FA")) {
                    expecHTEC = true;
                }
                if (isSegmented(nnn, vtec)) {
                    expectNamesList = true;
                }
                countUGC = false;
                continue;
            } else if (countUGC) {
                if (VtecUtil.parseMessage(body) != null) {
                    errorMsg.append(segment)
                            .append(" VTEC not right after UGC\n");
                }
                countUGC = false;
            }

            if (expecHTEC) {
                m = QCCheckConstants.HVTEC_PATTERN.matcher(line);
                if (!m.find()) {
                    errorMsg.append("Hydro VTEC line must follow FF or FL.\n");
                }
                expecHTEC = false;
                continue;
            }

            if (expectNamesList) {
                m = QCCheckConstants.AREA_NAME_LIST_PATTERN.matcher(line);
                /*
                 * DR15006 - MWS does not have the list of marine zones names in
                 * the segment heading, so skip the check for MWS
                 */
                if (!nnn.equalsIgnoreCase("MWS")) {
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

            if (line.startsWith("* ")) {
                nb++;
            }

            // third bullet
            if (line.startsWith("* ") && (nb == 3)) {
                m = QCCheckConstants.THIRD_BULLET_PATTERN.matcher(line);
                if (!line.substring(0, 5).equals("* AT ")) {
                    errorMsg.append(
                            "Event bullet does not start with '* AT '\n.");
                } else if (!m.find()) {
                    errorMsg.append(
                            "Event bullet starts with badly formatted time\n")
                            .append(" or event bullet does not start with a time.\n");
                }
            }

            // second bullet
            if (line.startsWith("* ") && (nb == 2)) {
                m = QCCheckConstants.SECOND_BULLET_PATTERN.matcher(line);
                if (m.find() || line.contains("* UNTIL NOON")
                        || line.contains("* UNTIL MIDNIGHT")) {
                    secondBulletFound = true;
                    insideFirstBullet = false;
                    continue;
                }
            }

            // first bullet
            if (nb == 1) {
                m = QCCheckConstants.FIRST_BULLET_PATTERN.matcher(line);
                if (m.find()) {
                    List<String> bulletTypesList = null;

                    if (vtec != null) {
                        String key = vtec.getPhenomena() + "."
                                + vtec.getSignificance();
                        bulletTypesList = QualityControl.getInstance()
                                .getBulletTypeMap().get(key);
                    }

                    if (bulletTypesList != null) {
                        // Vtec may have Bullet types that do not match the
                        // warning type.
                        boolean badBullet = true;
                        for (String bulletType : bulletTypesList) {
                            if (line.contains(bulletType.toUpperCase())) {
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
                        String warningType = QualityControl.getInstance()
                                .getProductWarningType(nnn)
                                .orElse("Unknown Warning").toUpperCase();
                        if (!line.contains(warningType)) {
                            errorMsg.append(nnn)
                                    .append(" does not match first bullet.\n");
                        }
                    }
                    insideFirstBullet = true;
                    checkIC = true;
                    continue;
                } else if (!insideFirstBullet && !secondBulletFound
                        && (line.contains("AREA...")
                                || line.contains("AREAS...")
                                || line.contains("AREA WAS..."))) {
                    insideFirstBullet = true;
                    continue;
                }
            }

            if (insideFirstBullet) {
                if ((ic != null) && !IMMEDIATE_CAUSE_EXCLUSIONS.contains(ic)
                        && checkIC) {
                    boolean validIC = false;
                    for (String causes : QualityControl.getInstance()
                            .getImmediateCauses()) {
                        if (causes.startsWith(ic)
                                && line.contains(causes.split("\\\\")[1])) {
                            validIC = true;
                            break;
                        }
                    }

                    if (!validIC) {
                        errorMsg.append(
                                "Immediate cause missing in first bullet\n or is inconsistent with VTEC.\n");
                        // need to return or else it will incorrectly count
                        // counties
                        return errorMsg.toString();
                    }
                    // Immediate cause should be the line after the first bullet
                    checkIC = false;
                    continue;
                }

                if (czmType == 3) {
                    if ((line != null)
                            && line.trim().startsWith("INCLUDING ")) {
                        insideFirstBullet = false; // stop adding counties/zones
                        continue;
                    }
                } else {
                    if (line.contains("THIS INCLUDES")) {
                        // line indicates cities not counties/zones
                        continue;
                    }

                    boolean invalidCountyOrZone = true;
                    if ((ugc.length() > 2) && (ugc.charAt(2) == 'Z')) {
                        // zones do not use countyTypes
                        if (line.contains(" IN ")) {
                            invalidCountyOrZone = false;
                        }
                    } else {
                        for (String state : QualityControl.getInstance()
                                .getCountyTypeMap().keySet()) {
                            if (line.contains(state.toUpperCase())
                                    && line.contains(QualityControl
                                            .getInstance().getCountyTypeMap()
                                            .get(state).toUpperCase())) {
                                invalidCountyOrZone = false;
                            }
                        }
                        if (invalidCountyOrZone) {
                            for (String countyType : QualityControl
                                    .getInstance().getCountyTypeMap()
                                    .values()) {
                                if ((countyType.trim().length() > 0)
                                        && line.contains(" " + countyType.trim()
                                                .toUpperCase())) {
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

                int cpmCounter = 0;
                Iterator<String> iter = countyParishMunicipality.iterator();
                while (iter.hasNext()) {
                    if (line.contains(iter.next())) {
                        break;
                    } else {
                        cpmCounter += 1;
                        continue;
                    }
                }
                if (cpmCounter != countyParishMunicipality.size()) {
                    if (line.trim().length() > 0) {
                        countyOrZoneCounter++;
                        continue;
                    } else {
                        // ran into a blank line, done
                        insideFirstBullet = false;
                    }
                }
            }

            m = QCCheckConstants.LAT_LON_PATTERN.matcher(line);
            if (m.find()) {
                latLon = line;
                insideLatLon = true;
                continue;
            }

            if (insideLatLon) {
                m = QCCheckConstants.SUB_LAT_LON_PATTERN.matcher(line);

                if (!line.startsWith("TIME...") && m.find()) {
                    latLon += " " + line.trim();
                    continue;
                } else {
                    insideLatLon = false;
                }
            }

            m = QCCheckConstants.TIME_MOT_LOC_PATTERN.matcher(line);
            if (m.find()) {
                tml = line;
            }

        }

        if (ugcLength == 0) {
            errorMsg.append("No UGC text was found\n");
        } else if ((nb > 0)
                && ((czmType == 1) || ((czmType == 2) && countyBased))
                && (ugcLength != countyOrZoneCounter)) {
            // DR 11060 - Don't count zone areas for hydro products
            errorMsg.append(ugcLength).append(" UGCs while ")
                    .append(countyOrZoneCounter)
                    .append(" counties/zones listed.\n");
            errorMsg.append(
                    "Area descriptions count does not\n match UGC count.\n");
        }

        if (body.contains("LAT...LON")) {
            errorMsg.append(checkLatLon(latLon));
        }
        if (body.contains("TIME...MOT...LOC")) {
            errorMsg.append(checkTML(tml));
        }

        return errorMsg.toString();
    }

    protected abstract boolean isSegmented(String nnn, VtecObject vtec);

    protected Collection<String> getZoneTypes() {
        Set<String> countyParishMunicipality = new HashSet<>();
        for (Entry<String, String> entry : QualityControl.getInstance()
                .getCountyTypeMap().entrySet()) {
            String key = entry.getKey();
            String countyType = entry.getValue();
            if (countyType.length() > 1) {
                countyParishMunicipality.add(countyType.trim().toUpperCase());
            } else if (key.length() > 1) {
                countyParishMunicipality.add(key.toUpperCase());
            }
        }
        countyParishMunicipality.remove("AK");
        countyParishMunicipality.remove("DC");
        countyParishMunicipality.add("CITY");

        return countyParishMunicipality;
    }

    protected String checkLatLon(String latLon) {
        if (latLon.isEmpty()) {
            return "LAT...LON line is malformed.\n";
        }

        int pairs = 0;

        Matcher m = QCCheckConstants.LAT_LON_PAIR_PATTERN
                .matcher(latLon.substring(9));
        while (m.find()) {
            pairs++;
            double lat = Double.parseDouble(m.group(1));
            double lon = Double.parseDouble(m.group(2));
            if ((lat > 9000) || (lon > 18000)) {
                return "Data error in the LAT...LON line.\n";
            }
        }

        if ((pairs <= 2) || (pairs > 20)) {
            return "LAT...LON line must have at least 3 and no more than 20 points.\n";
        }

        return StringUtils.EMPTY;
    }

    protected String checkTML(String tml) {
        if (tml.isEmpty()) {
            return "TIME...MOT...LOC line is malformed.\n";
        }

        return StringUtils.EMPTY;
    }

    protected String checkHeadline(String headline, String nnn,
            VtecObject vtec) {
        nnn = nnn.toUpperCase();

        if (!QualityControl.getInstance().getSegmentedNNN().contains(nnn)
                || nnn.equals("FLS")) {
            // non-follow ups do not have a head line
            return StringUtils.EMPTY;
        }

        /*
         * DR15003: no headline QC on standalone MWS. To distinguish between
         * standalone and follow up MWS the VTEC check is performed as
         * standalone MWS do not contain VTEC
         */
        if (nnn.equals("MWS") && (vtec == null)) {
            return StringUtils.EMPTY;
        }

        if ((nnn.equals("DSW") || nnn.equals("SQW"))
                && ((vtec != null) && ("NEW".equals(vtec.getAction())
                        || ("COR".equals(vtec.getAction()))))) {
            return StringUtils.EMPTY;
        }

        if (headline.isEmpty()) {
            return "Headline is missing or malformed.\n";
        }

        if (!headline.endsWith("...")) {
            return "Headline should end with '...'.\n";
        }

        return StringUtils.EMPTY;
    }
}
