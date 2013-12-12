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
package com.raytheon.viz.gfe.vtec;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableSet;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * Utility class to set ETNs on GFE VTEC products prior to transmission. Logic
 * for ETN assignment/replacement based on A1 HazardsTable.py code; thus didn't
 * want to contaminate the generic <code>VtecUtil</code> with A1 GFE-specific
 * logic.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2013  #1842     dgilling     Initial creation
 * Jul 19, 2013  #1842     dgilling     Use VtecUtil.replaceFirstVtecString()
 *                                      to ensure start times of in progress
 *                                      events aren't set to the wrong time.
 * Aug 07, 2013  #1842     dgilling     Fix ETN assignment for products with
 *                                      multiple NEW segments with the same 
 *                                      phensig.
 * Nov 22, 2013  #2578     dgilling     Fix ETN assignment for products with
 *                                      multiple NEW VTEC lines for the same
 *                                      phensig but disjoint TimeRanges.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GFEVtecUtil {

    public static final Collection<String> TROPICAL_PHENSIGS = ImmutableSet
            .copyOf(GFEVtecConfig.getInstance().getTropicalEtnPhensigs());

    public static final Collection<String> NATIONAL_PHENSIGS = ImmutableSet
            .copyOf(GFEVtecConfig.getInstance().getNationalEtnPhensigs());

    public static final Collection<String> IGNORE_NATIONAL_ETN = ImmutableSet
            .copyOf(GFEVtecConfig.getInstance().getSitesIgnoreNationalEtn());

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private GFEVtecUtil() {
        throw new AssertionError();
    }

    public static int getNextEtn(String office, String phensig, boolean lockEtn)
            throws VizException {
        return VtecUtil.getNextEtn(office, phensig, lockEtn);
    }

    public static String finalizeETNs(String message) throws VizException {
        if (Strings.isNullOrEmpty(message)) {
            return message;
        }

        // With GFE VTEC products, it's possible to have multiple segments with
        // NEW vtec action codes and the same phensig. For this reason,
        // HazardsTable.py implemented a "cache" that would ensure all NEWs for
        // the same phensig would be assigned the same ETN. This Map replicates
        // that legacy behavior.
        //
        // This "cache" has two levels:
        // 1. The first level is keyed by the hazard's phensig.
        // 2. The second level is keyed by the valid period of the hazard.
        // Effectively, making this a Map<Phensig, Map<ValidPeriod, ETN>>.
        Map<String, Map<TimeRange, Integer>> etnCache = new HashMap<String, Map<TimeRange, Integer>>();

        Matcher vtecMatcher = VtecUtil.VTEC_REGEX.matcher(message);
        StringBuffer finalOutput = new StringBuffer();
        while (vtecMatcher.find()) {
            VtecObject vtec = new VtecObject(vtecMatcher.group());
            // To best match the ETN assignment logic in HazardsTable.py, it
            // seems we should assume all ETNs assigned to tropical products are
            // automatically correct
            if (("NEW".equals(vtec.getAction()))
                    && ((!NATIONAL_PHENSIGS.contains(vtec.getPhensig())) || (IGNORE_NATIONAL_ETN
                            .contains(vtec.getOffice()) && TROPICAL_PHENSIGS
                            .contains(vtec.getPhensig())))) {
                // Some more clarification on the ETN assignment behavior: all
                // NEW VTEC lines with the same phensig should be assigned the
                // same ETN if the hazards' valid periods are adjacent or
                // overlapping.
                // If there's a discontinuity in TimeRanges we increment to the
                // next ETN.
                Integer newEtn = null;
                String phensig = vtec.getPhensig();
                TimeRange validPeriod = new TimeRange(vtec.getStartTime()
                        .getTime(), vtec.getEndTime().getTime());

                Map<TimeRange, Integer> etnsByTR = etnCache.get(phensig);
                if (etnsByTR != null) {
                    for (TimeRange tr : etnsByTR.keySet()) {
                        if ((validPeriod.isAdjacentTo(tr))
                                || (validPeriod.overlaps(tr))) {
                            newEtn = etnsByTR.get(tr);
                            break;
                        }
                    }
                }

                if (newEtn == null) {
                    newEtn = VtecUtil.getNextEtn(vtec.getOffice(),
                            vtec.getPhensig(), true);
                }

                Map<TimeRange, Integer> cacheLevel2 = (etnsByTR != null) ? etnsByTR
                        : new HashMap<TimeRange, Integer>();
                cacheLevel2.put(validPeriod, newEtn);
                etnCache.put(phensig, cacheLevel2);

                vtec.setSequence(newEtn);
            }
            vtecMatcher
                    .appendReplacement(
                            finalOutput,
                            VtecUtil.replaceFirstVtecString(
                                    vtec.getVtecString(), vtec));
        }
        vtecMatcher.appendTail(finalOutput);
        return finalOutput.toString();
    }
}
