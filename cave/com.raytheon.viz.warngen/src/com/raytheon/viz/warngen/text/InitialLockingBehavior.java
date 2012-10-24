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
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.warning.util.FileUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.warngen.gis.AffectedAreas;

/**
 * Locks text patterns on initial or new warning.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012     15322  jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class InitialLockingBehavior extends AbstractLockingBehavior {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(InitialLockingBehavior.class);

    private static Pattern immediateCausePtrn = null;

    /**
     * Locks the appropriate text of the body of an initial warning.
     */
    @Override
    public void body() {
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

        int index = text.indexOf("* ");
        while (index >= 0) {
            bulletIndices.add(index);
            index = text.indexOf("* ", index + 2);
        }

        return bulletIndices.toArray(new Integer[bulletIndices.size()]);
    }

    /**
     * Locks the header before the first bullet.
     */
    private void header() {
        // LOCK_END should not be found at the beginning since the previous line
        // should be blank.
        String h = "^((THE NATIONAL WEATHER SERVICE IN .{1,} HAS ISSUED A)"
                + newline + ")$";
        Pattern header = Pattern.compile(h, Pattern.MULTILINE);
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
        // LOCK_END should not be found at the beginning since the previous line
        // should be blank.
        String secondBullet = "^(\\* UNTIL \\d{3,4} (AM|PM) \\w{3,4}" + newline
                + ")";
        Pattern secondBulletPtrn = Pattern.compile(secondBullet,
                Pattern.MULTILINE);
        find(secondBulletPtrn.matcher(text));
    }

    /**
     * Set the immediateCausePtrn with the info in immediateCause.text.
     */
    private static Pattern getImmediateCausesPtrn() {
        String filename = "immediateCause.txt";
        StringBuffer pattern = new StringBuffer();

        try {
            String immediateCause = FileUtil.open(filename, "base");
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

}
