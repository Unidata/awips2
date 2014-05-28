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
package com.raytheon.viz.warngen.text;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;
import com.raytheon.viz.warngen.gis.AffectedAreas;

/**
 * Abstract class that locks certain text patterns in the warning text.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012    15332   jsanchez     Initial creation
 * Oct 18, 2012    15332   jsanchez     Updated the find method.
 * Jan  8, 2013    15664   Qinglu Lin   Appended WarningAction to lock()'s parameter list;
 *                                      moved the following methods from InitialLockingBehavior to this class:
 *                                      bulletIndices(), header(), firstBullet(), secondBullet(), getImmediateCausesPtrn();
 *                                      updated body(), header(), and secondBullet();
 * Mar 13, 2013  DR 15892  D. Friedman  Fix bullet parsing.
 * Apr 29, 2014    3033    jsanchez     Moved patterns into ICommonPatterns
 * May  1, 2014  DR 16627  Qinglu Lin   Added hasStateAbbrev(), isOtherType(), lockListOfNames(), and updated lock(). 
 * May 13, 2014  DR 17177  Qinglu Lin   Updated secondBullet().
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
abstract public class AbstractLockingBehavior implements ICommonPatterns {

    /**
     * Sorts in reverse order the areas based on the length of the name.
     * 
     * @author jsanchez
     * 
     */
    private class AreaLockingComparator implements Comparator<AffectedAreas> {

        @Override
        public int compare(AffectedAreas o1, AffectedAreas o2) {
            Integer i1 = new Integer(o1.getName().length());
            Integer i2 = new Integer(o2.getName().length());
            return i2.compareTo(i1);
        }

    }

    protected List<AffectedAreas> affectedAreas;

    protected String text;

    protected static final String REPLACEMENT = LOCK_START + "$0" + LOCK_END;

    protected String warningType = "(FLOOD ADVISORY)|(FLOOD WARNING)|(FLOOD STATEMENT)"
            + "|(SEVERE WEATHER STATEMENT)|(EXTREME WIND WARNING)|(FIRE WARNING)"
            + "|(FLASH FLOOD WARNING)|(FLASH FLOOD STATEMENT)|(SEVERE THUNDERSTORM WARNING)"
            + "|(TORNADO WARNING)|(MARINE WEATHER STATEMENT)|(SHORT TERM FORECAST)"
            + "|(SPECIAL WEATHER STATEMENT)|(SPECIAL MARINE WARNING)";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractLockingBehavior.class);

    private static Pattern immediateCausePtrn = null;

    protected WarningAction action = null;

    /**
     * Locks the particular strings in the texts based on common patterns and
     * the affected areas.
     * 
     * @param text
     * @param affectedAreas
     * @param canceledAreas
     * @return
     */
    public String lock(String text, AffectedAreas[] affectedAreas,
            AffectedAreas[] canceledAreas, WarningAction action) {
        this.text = text;
        this.action = action;
        intialize(affectedAreas, canceledAreas);
        ugc();
        htec();
        vtec();
        if (isDesiredPil() && !hasStateAbbrev()) {
            lockListOfNames();
        } else {
            areaNames();
        }
        mnd();
        date();
        body();
        callToActions();
        latLon();
        tml();
        testMessages();
        clean();

        return this.text;
    }

    protected void body() {
        header();
        firstBullet();
        secondBullet();
    }

    /**
     * Helper method to determine the index of each bullet.
     * 
     * @return
     */
    private Integer[] bulletIndices() {
        List<Integer> bulletIndices = new ArrayList<Integer>();

        /*
         * Assumes first line cannot be a bullet and that the '*' is at the
         * start of a line.
         */
        int index = text.indexOf("\n* ");
        while (index >= 0) {
            bulletIndices.add(index + 1);
            index = text.indexOf("\n* ", index + 3);
        }

        return bulletIndices.toArray(new Integer[bulletIndices.size()]);
    }

    /**
     * Locks the header before the first bullet.
     */
    private void header() {
        // LOCK_END should not be found at the beginning since the previous line
        // should be blank.

        find(header.matcher(text));
    }

    /**
     * Locks the affected area names in the first bullet, the immediate cause.
     * 
     * @param start
     * @param end
     */
    private void firstBullet() {
        Integer[] bulletIndices = bulletIndices();

        // Short term forecasts don't follow normal bullets?
        if (bulletIndices.length < 2) {
            return;
        }
        int start = bulletIndices[0];
        int end = bulletIndices[1];

        if (immediateCausePtrn == null) {
            immediateCausePtrn = getImmediateCausesPtrn();
        }

        String firstBulletText = text.substring(start, end);

        // According to the original WarningTextHandler, marine zone names
        // should not be locked. For some reason, this differs from followups as
        // stated in DR 15110. Need verification from NWS. This is a variance?
        if (!isMarineProduct()) {
            Matcher m = null;
            for (String line : firstBulletText.split("\\n")) {

                if (immediateCausePtrn != null) {
                    // immediate cause
                    m = immediateCausePtrn.matcher(line);
                    if (m.find()) {
                        String i = line.replace(line, LOCK_START + line
                                + LOCK_END);
                        firstBulletText = firstBulletText.replace(line, i);
                        continue;
                    }
                }

                for (AffectedAreas affectedArea : affectedAreas) {
                    String name = affectedArea.getName();
                    String areaNotation = affectedArea.getAreaNotation();
                    String parentRegion = affectedArea.getParentRegion();
                    if (name != null && name.trim().length() != 0
                            && line.contains(name.toUpperCase())) {
                        name = name.toUpperCase();
                        String t = line;
                        if (!hasBeenLocked(line, name)) {
                            t = t.replace(name, LOCK_START + name + LOCK_END);
                        }

                        if (areaNotation != null
                                && areaNotation.trim().length() != 0) {
                            areaNotation = areaNotation.toUpperCase();
                            if (!hasBeenLocked(line, areaNotation.toUpperCase())) {
                                t = t.replace(areaNotation, LOCK_START
                                        + areaNotation + LOCK_END);
                            }
                        }

                        if (parentRegion != null
                                && parentRegion.trim().length() != 0) {
                            parentRegion = parentRegion.toUpperCase();
                            if (!hasBeenLocked(line, parentRegion)) {
                                t = t.replace(parentRegion, LOCK_START
                                        + parentRegion + LOCK_END);
                            }
                        }

                        if (validate(t)) {
                            firstBulletText = firstBulletText.replace(line, t);
                        }
                        break;
                    }
                }
            }
        }

        firstBulletText = firstBulletText.replaceAll(firstBullet, LOCK_START
                + "$0" + LOCK_END);

        this.text = text.replace(text.substring(start, end), firstBulletText);
    }

    /**
     * Locks the second bullet.
     */
    private void secondBullet() {
        find(secondBulletPtrn.matcher(text));
    }

    /**
     * Set the immediateCausePtrn with the info in immediateCause.text.
     */
    private static Pattern getImmediateCausesPtrn() {
        String filename = "immediateCause.txt";
        StringBuffer pattern = new StringBuffer();

        try {
            String immediateCause = WarnFileUtil.convertFileContentsToString(filename, null, null);
            pattern.append("(.*)(A DAM BREAK");
            for (String ic : immediateCause.split("\n")) {
                String[] parts = ic.split("\\\\");
                pattern.append("| " + parts[1].trim());
            }

            pattern.append(")(.*)");
            return Pattern.compile(pattern.toString());
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.ERROR,
                            "Unable to process immediateCause.txt in the base directory",
                            e);
        }

        return null;
    }

    /**
     * Adds the affect areas into the appropriate lists.
     * 
     * @param affectedAreas
     * @param canceledAreas
     */
    private void intialize(AffectedAreas[] affectedAreas,
            AffectedAreas[] canceledAreas) {
        AreaLockingComparator comparator = new AreaLockingComparator();

        if (affectedAreas != null) {
            this.affectedAreas = new ArrayList<AffectedAreas>(
                    Arrays.asList(affectedAreas));
        } else {
            this.affectedAreas = new ArrayList<AffectedAreas>(0);
        }

        if (canceledAreas != null) {
            this.affectedAreas.addAll(Arrays.asList(canceledAreas));
        }
        Collections.sort(this.affectedAreas, comparator);
    }

    /**
     * Locks the UGC line or FIPS line.
     */
    private void ugc() {
        Pattern ugcPtrn = Pattern.compile(ugc + NEWLINE, Pattern.MULTILINE);
        find(ugcPtrn.matcher(text));
    }

    /**
     * Locks the HTEC line.
     */
    private void htec() {
        find(htecPtrn.matcher(text));
    }

    /**
     * Locks the VTEC line.
     */
    private void vtec() {
        find(vtecPtrn.matcher(text));
    }

    /**
     * Locks the list of area names.
     */
    private void areaNames() {
        Pattern listOfAreaNamePtrn = Pattern.compile(listOfAreaName + NEWLINE,
                Pattern.MULTILINE);
        find(listOfAreaNamePtrn.matcher(text));
    }

    /**
     * Helper method to replace the found patterns.
     * 
     * @param m
     */
    protected void find(Matcher m) {
        while (m.find()) {
            String group = m.group();
            // If line already starts with a LOCK_END, the LOCK_END is removed
            // and the line just adds to the LOCK_END at the end.
            if (group.startsWith(LOCK_END)) {
                this.text = text.replace(group,
                        group.substring(LOCK_END.length()) + LOCK_END);
            } else {
                this.text = m.replaceAll(LOCK_START + "$0" + LOCK_END);
            }
        }
    }

    /**
     * This method locks the entire MND Header block.
     */
    private void mnd() {
        int start = -1;
        int end = -1;

        StringBuffer mnd = new StringBuffer(
                "(BULLETIN - IMMEDIATE BROADCAST REQUESTED)|(BULLETIN - EAS ACTIVATION REQUESTED)|"
                        + warningType);

        Pattern startMND = Pattern.compile(mnd.toString());
        Matcher m = startMND.matcher(text);
        if (m.find()) {
            start = m.start();
        }

        m = datePtrn.matcher(text);
        if (m.find()) {
            end = m.start();
        }

        if (start != -1 && end != -1 && start < end) {
            String substring = text.substring(start, end);
            // There should be only one instance of an MND Header block
            this.text = text.replace(substring, LOCK_START + substring
                    + LOCK_END);
        }
    }

    /**
     * Locks the date.
     */
    private void date() {
        find(datePtrn.matcher(text));
    }

    /**
     * Locks the TIME...MOT...LINE (Can be multiple lines).
     */
    private void tml() {
        find(tmlPtrn.matcher(text));
    }

    /**
     * Locks the coordinates of the polygon.
     */
    private void latLon() {

        find(latLonPtrn.matcher(text));
    }

    /**
     * Locks the Call To Action header and the segment tags.
     */
    private void callToActions() {
        find(cta.matcher(text));
    }

    /**
     * Locks the test messages.
     */
    private void testMessages() {
        find(testPtrn.matcher(text));
    }

    private void clean() {
        // where a lock close and lock open are only separated by whitespace
        // remove the close and open to join the two locked areas
        text = text.replaceAll("</L>([\\s\\n\\r]*)<L>", "$1");
    }

    /**
     * Returns true if this is a Marine Weather Statement and not a Standalone
     * MWS. Standalone MWS do not have VTECs.
     * 
     * @return
     */
    protected boolean isMarineProduct() {
        VtecObject vtecObj = VtecUtil.parseMessage(text);
        boolean marineProduct = vtecObj != null
                && vtecObj.getPhenomena() != null
                && vtecObj.getPhenomena().equals("MA");
        return marineProduct;
    }

    /**
     * Tests if the LOCK_START count matches the LOCK_END count.
     * 
     * @return
     */
    public static boolean validate(String modified) {
        int startCount = StringUtils.countMatches(modified, LOCK_START);
        int endCount = StringUtils.countMatches(modified, LOCK_END);

        return startCount == endCount;
    }

    /**
     * Tests if the name has been locked already by the template to avoid
     * stacked lock tags.
     * 
     * @return
     */
    protected boolean hasBeenLocked(String line, String name) {
        int index = line.indexOf(name);
        if (index != -1) {
            int startBefore = line.lastIndexOf(LOCK_START, index);
            int endBefore = line.lastIndexOf(LOCK_END, index);
            int startAfter = line.indexOf(LOCK_START, index);
            int endAfter = line.indexOf(LOCK_END, index);

            if (startBefore != -1 && endAfter != -1 && startBefore < endAfter) {
                if (startAfter != -1 && startAfter < endAfter
                        && startAfter > startBefore) {
                    return false;
                } else if (endBefore != -1 && endBefore > startBefore
                        && endBefore < endAfter) {
                    return false;
                }

                return true;
            }
        }

        return false;
    }

    /**
     * Check out if warning text has state abbreviations, e.g., " MD".
     */
    private boolean hasStateAbbrev() {
        Pattern stateAbbrevPtrn = Pattern.compile(listOfAreaName + NEWLINE,
                Pattern.MULTILINE);
        Matcher m = stateAbbrevPtrn.matcher(this.text);
        if (m.find()) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Check out if pil of the product is SPS or NOW.
     */
    public boolean isDesiredPil() {
        String[] type = {"SPS", "NOW"};
        int index = text.indexOf("000000") + 8;
        String pilAndCWA = text.substring(index, index + 6);
        for (String s: type) {
            if (pilAndCWA.contains(s)) {
                return true;
            } 
        }
        return false;
    }

    /**
     * Locks the list of area names in which state abbreviations, e.g., " MD", 
     * are not included.
     */
    private void lockListOfNames() {
        String[] timePattern = {" AM ", " PM ", "NOON", "MIDNIGHT"};
        String text1 = "";
        int indexOfDash = text.indexOf('-');
        String text2 = text.substring(indexOfDash, text.length() - 1);
        int indexOfTimePattern;
        for (String s: timePattern) {
            indexOfTimePattern = text2.indexOf(s);
            if (indexOfTimePattern != -1) {
                text1 = text2.substring(0, indexOfTimePattern);
                break;
            }
        }
        int index1, index2;
        if (text1.length() > 0) {
            index1 = text1.indexOf("</L>");
            index2 = text1.lastIndexOf("-");
            text2 = text1.substring(index1 + 4, index2 + 1);
            index1 = text.indexOf(text2);
            text = text.substring(0, index1) + "<L>" + text2 + "</L>" 
                    + text.substring(index1 + text2.length(), text.length() - 1);
        }
    }

}
