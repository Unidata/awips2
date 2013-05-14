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
import java.util.regex.Matcher;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableSet;
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
                int newEtn = VtecUtil.getNextEtn(vtec.getOffice(),
                        vtec.getPhensig(), true);
                vtec.setSequence(newEtn);
            }
            vtecMatcher.appendReplacement(finalOutput, vtec.getVtecString());
        }
        vtecMatcher.appendTail(finalOutput);
        return finalOutput.toString();
    }
}
