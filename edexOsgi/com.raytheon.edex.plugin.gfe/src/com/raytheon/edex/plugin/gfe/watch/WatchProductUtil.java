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
package com.raytheon.edex.plugin.gfe.watch;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.util.StringUtil;

/**
 * Common methods for dealing with watch products that are received from
 * national centers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2014  #3157     dgilling     Initial creation.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class WatchProductUtil {

    private static final Pattern ATTN_PATTERN = Pattern
            .compile("\\QATTN...WFO...\\E((?:[A-Z]{3}\\Q...\\E)+)");

    private WatchProductUtil() {
        throw new AssertionError();
    }

    /**
     * Searches the specified watch product text for a "ATTN...WFO..." line and
     * returns a collection of WFOs that appeared in that line of the product.
     * 
     * @param lines
     *            The lines that comprise the watch product.
     * @return The list of WFOs that appear in the "ATTN...WFO..." line of the
     *         product.
     */
    public static Collection<String> findAttnWFOs(List<String> lines) {
        return findAttnWFOs(StringUtil.join(lines, '\n'));
    }

    /**
     * Searches the specified watch product text for a "ATTN...WFO..." line and
     * returns a collection of WFOs that appeared in that line of the product.
     * 
     * @param rawMessage
     *            The full text of the watch product in a single String.
     * @returnThe list of WFOs that appear in the "ATTN...WFO..." line of the
     *            product.
     */
    public static Collection<String> findAttnWFOs(String rawMessage) {
        Collection<String> retVal = Collections.emptySet();

        // decode the ATTN line, which tells us which WFOs are affected
        // only used for WCL and WOU products
        Matcher m = WatchProductUtil.ATTN_PATTERN.matcher(rawMessage);
        if (m.find()) {
            // eliminate ATTN...WFO...
            String found = m.group(1);
            String[] wfos = found.split(Pattern.quote("..."));
            retVal = new HashSet<String>(Arrays.asList(wfos));
        }

        return retVal;
    }
}
