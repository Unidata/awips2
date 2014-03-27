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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

import com.google.common.collect.ImmutableSet;
import com.raytheon.uf.common.activetable.response.GetNextEtnResponse;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.StringUtil;
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
 * Aug 29, 2013  #1843     dgilling     Add hooks for inter-site ETN assignment.
 * Oct 21, 2013  #1843     dgilling     Use new GetNextEtnResponse.
 * Nov 22, 2013  #2578     dgilling     Fix ETN assignment for products with
 *                                      multiple NEW VTEC lines for the same
 *                                      phensig but disjoint TimeRanges.
 * Dec 18, 2013  #2641     dgilling     Force ordering of items returned by
 *                                      getVtecLinesThatNeedEtn().
 * Feb 05, 2014  #2774     dgilling     Additional correction to previous fix.
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

    private static final Comparator<VtecObject> VTEC_COMPARATOR = new Comparator<VtecObject>() {

        @Override
        public int compare(VtecObject vtec1, VtecObject vtec2) {
            TimeRange tr1 = new TimeRange(vtec1.getStartTime(),
                    vtec1.getEndTime());
            TimeRange tr2 = new TimeRange(vtec2.getStartTime(),
                    vtec2.getEndTime());

            int retVal = tr1.getStart().compareTo(tr2.getStart());
            if (retVal == 0) {
                retVal = tr1.getEnd().compareTo(tr2.getEnd());
            }

            return retVal;
        }
    };

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private GFEVtecUtil() {
        throw new AssertionError();
    }

    /**
     * Gets the next available ETN for a specific product and office.
     * 
     * @param office
     *            The 4-character site ID of the office.
     * @param phensig
     *            The phenomenon and significance of the hazard concatenated
     *            with a '.' (e.g., TO.W or DU.Y)
     * @param lockEtn
     *            Whether or not to request an exclusive ETN--if true, this will
     *            cause the server to increment its running ETN sequence to the
     *            next number after determining the next ETN for this request.
     *            If false, the next ETN will be returned, but it will not
     *            increment the server's running sequence, so the ETN return
     *            could be used by another client that makes a
     *            GetNextEtnRequest.
     * @return The next ETN in sequence, given the office and phensig.
     * @throws VizException
     *             If an error occurred sending the request to the server.
     */
    public static GetNextEtnResponse getNextEtn(String office, String phensig,
            boolean lockEtn) throws VizException {
        return getNextEtn(office, phensig, lockEtn, false, false, null);
    }

    /**
     * Gets the next available ETN for a specific product and office.
     * 
     * @param office
     *            The 4-character site ID of the office.
     * @param phensig
     *            The phenomenon and significance of the hazard concatenated
     *            with a '.' (e.g., TO.W or DU.Y)
     * @param lockEtn
     *            Whether or not to request an exclusive ETN--if true, this will
     *            cause the server to increment its running ETN sequence to the
     *            next number after determining the next ETN for this request.
     *            If false, the next ETN will be returned, but it will not
     *            increment the server's running sequence, so the ETN return
     *            could be used by another client that makes a
     *            GetNextEtnRequest.
     * @param performISC
     *            Whether or not to collaborate with neighboring sites to
     *            determine the next ETN. See {@link
     *            GetNextEtnUtil#getNextEtnFromPartners(String, ActiveTableMode,
     *            String, Calendar, List<IRequestRouter>)} for more information.
     * @return The next ETN in sequence, given the office and phensig.
     * @throws VizException
     *             If an error occurred sending the request to the server.
     */
    public static GetNextEtnResponse getNextEtn(String office, String phensig,
            boolean lockEtn, boolean performISC) throws VizException {
        return getNextEtn(office, phensig, lockEtn, performISC, false, null);
    }

    /**
     * Gets the next available ETN for a specific product and office.
     * 
     * @param office
     *            The 4-character site ID of the office.
     * @param phensig
     *            The phenomenon and significance of the hazard concatenated
     *            with a '.' (e.g., TO.W or DU.Y)
     * @param lockEtn
     *            Whether or not to request an exclusive ETN--if true, this will
     *            cause the server to increment its running ETN sequence to the
     *            next number after determining the next ETN for this request.
     *            If false, the next ETN will be returned, but it will not
     *            increment the server's running sequence, so the ETN return
     *            could be used by another client that makes a
     *            GetNextEtnRequest.
     * @param performISC
     *            Whether or not to collaborate with neighboring sites to
     *            determine the next ETN. See {@link
     *            GetNextEtnUtil#getNextEtnFromPartners(String, ActiveTableMode,
     *            String, Calendar, List<IRequestRouter>)} for more information.
     * @param reportOnlyConflict
     *            Affects which kinds of errors get reported back to the
     *            requestor. If true, only cases where the value of
     *            <code>etnOverride</code> is less than or equal to the last ETN
     *            used by this site or any of its partners will be reported.
     *            Else, all significant errors will be reported back.
     * @param etnOverride
     *            Allows the user to influence the next ETN assigned by using
     *            this value unless it is less than or equal to the last ETN
     *            used by this site or one of its partners.
     * @return The next ETN in sequence, given the office and phensig.
     * @throws VizException
     *             If an error occurred sending the request to the server.
     */
    public static GetNextEtnResponse getNextEtn(String office, String phensig,
            boolean lockEtn, boolean performISC, boolean reportOnlyConflict,
            Integer etnOverride) throws VizException {
        return VtecUtil.getNextEtn(office, phensig, lockEtn, performISC,
                reportOnlyConflict, etnOverride);
    }

    /**
     * Reads through a GFE VTEC product and returns VTEC lines with NEW action
     * codes that need to be assigned an ETN.
     * 
     * @param product
     *            The product's text.
     * @return A <code>Set</code> of <code>VtecObject</code>s that need to have
     *         a new ETN assigned to them.
     */
    public static Map<String, List<VtecObject>> initETNCache(String product) {
        if (StringUtil.isEmptyString(product)) {
            return Collections.emptyMap();
        }

        Map<String, List<VtecObject>> cache = new HashMap<String, List<VtecObject>>();

        Matcher vtecMatcher = VtecUtil.VTEC_REGEX.matcher(product);
        while (vtecMatcher.find()) {
            VtecObject vtec = new VtecObject(vtecMatcher.group());
            if (("NEW".equals(vtec.getAction()))
                    && ((!NATIONAL_PHENSIGS.contains(vtec.getPhensig())) || (IGNORE_NATIONAL_ETN
                            .contains(vtec.getOffice()) && TROPICAL_PHENSIGS
                            .contains(vtec.getPhensig())))) {
                List<VtecObject> vtecsForPhensig = cache.get(vtec.getPhensig());
                if (vtecsForPhensig == null) {
                    vtecsForPhensig = new ArrayList<VtecObject>();
                }
                vtec.setSequence(0);
                vtecsForPhensig.add(vtec);
                cache.put(vtec.getPhensig(), vtecsForPhensig);
            }
        }

        for (String phensig : cache.keySet()) {
            List<VtecObject> vtecsByTimeOrder = new ArrayList<VtecObject>(
                    cache.get(phensig));
            Collections.sort(vtecsByTimeOrder, VTEC_COMPARATOR);
            cache.put(phensig, vtecsByTimeOrder);
        }

        return cache;
    }

    /**
     * For any NEW VTEC lines contained within the specified product texts,
     * generates a <code>GetNextEtnRequest</code> to retrieve the next canonical
     * ETN in sequence for the given phensig.
     * 
     * @param product
     *            The product's text.
     * @return The product's text with any NEW VTEC lines having their ETN
     *         values replaced with the next ETN in sequence.
     * @throws VizException
     *             If an error occurred sending the request to the server.
     */
    public static String finalizeETNs(String product,
            Map<String, Map<TimeRange, Integer>> etnCache) {
        if (StringUtil.isEmptyString(product)) {
            return product;
        }

        Matcher vtecMatcher = VtecUtil.VTEC_REGEX.matcher(product);
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
                String phensig = vtec.getPhensig();
                TimeRange validPeriod = new TimeRange(vtec.getStartTime()
                        .getTime(), vtec.getEndTime().getTime());
                Integer newEtn = etnCache.get(phensig).get(validPeriod);
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