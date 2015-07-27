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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.viz.warngen.gis.AffectedAreas;

/**
 * Handles the raw text message by applying locked text patterns, wrapping, etc.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012    15322   jsanchez    Initial creation
 * Jan  8, 2013    15664   Qinglu Lin  Appended WarningAction to handle()'s parameter list, etc. 
 * May  7, 2015 ASM #17438 D. Friedman Clean up debug and performance logging.
 * May 29, 2014    4440    randerso    Allow mixed case in WarnGen text products
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class WarningTextHandler {
    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("WG:");

    private AbstractLockingBehavior lockingBehavior;

    private IModifyTextBehavior modifyTextBehavior;

    public String handle(String text, AffectedAreas[] affectedAreas,
            AffectedAreas[] canceledAreas, WarningAction action) {
        long t0 = System.currentTimeMillis();

        if (modifyTextBehavior != null) {
            text = modifyTextBehavior.modify(text);
            System.out.println("Modified text...");
        }

        if (lockingBehavior != null) {
            text = lockingBehavior.lock(text, affectedAreas, canceledAreas,
                    action);
            System.out.println("Locked text...");
        }

        text = WrapUtil.wrap(text);
        System.out.println("Wrapped text...");

        text = clean(text);

        perfLog.logDuration("Handle the text", System.currentTimeMillis() - t0);
        return text;
    }

    /**
     * Sets locking behavior
     * 
     * @param lockingbehavior
     */
    public void setLockingBehavior(AbstractLockingBehavior lockingBehavior) {
        this.lockingBehavior = lockingBehavior;
    }

    /**
     * Sets the modify text behavior.
     * 
     * @param modifyTextBehavior
     */
    public void setModifyTextBehavior(IModifyTextBehavior modifyTextBehavior) {
        this.modifyTextBehavior = modifyTextBehavior;
    }

    private String clean(String text) {
        text = removeExtraLines(text);
        text = removeLockedBlankLines(text);
        return text;
    }

    /**
     * Removes blank extra lines.
     * 
     * @param text
     * @return
     */
    private String removeExtraLines(String text) {
        StringBuffer sb = new StringBuffer();
        String[] seperatedLines = text.replaceAll("\r", "").trim().split("\n");
        boolean blankLine = false;
        for (String line : seperatedLines) {
            if (line.replace(WarnGenPatterns.LOCK_START, "")
                    .replace(WarnGenPatterns.LOCK_END, "").trim().length() > 0) {
                sb.append(line + "\n");
                blankLine = false;
            } else if (blankLine == false) {
                sb.append(line + "\n");
                blankLine = true;
            }
        }

        return sb.toString().trim();
    }

    /**
     * Removes the locked blank lines.
     * 
     * @param text
     * @return
     */
    private String removeLockedBlankLines(String text) {
        Pattern lockedBlankLinesPattern = Pattern.compile("<L>(\\s*+)</L>",
                Pattern.MULTILINE);
        Matcher matchLockedBlankLines = lockedBlankLinesPattern.matcher(text);
        return matchLockedBlankLines.replaceAll("$1");
    }

}
