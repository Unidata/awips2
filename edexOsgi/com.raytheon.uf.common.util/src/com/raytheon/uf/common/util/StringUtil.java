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
package com.raytheon.uf.common.util;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;

/**
 * This class is for static methods that manipulate strings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class StringUtil {

    /**
     * Splits a string using given separator characters; strings are trimmed and
     * empty entries removed.
     * 
     * @see org.apache.commons.lang.StringUtils#split
     * 
     * @param str
     *            the string to split
     * @param separatorChar
     *            Characters to use as separators
     * @return An array of trimmed non-empty strings.
     * 
     */
    public static String[] split(final String str, final String separatorChar) {
        String[] result = null;
        if (str != null) {
            result = StringUtils
                    .stripAll(StringUtils.split(str, separatorChar));
            List<String> list = new ArrayList<String>();

            for (String s : result) {
                if (s.isEmpty() == false) {
                    list.add(s);
                }
            }

            if (result.length != list.size()) {
                result = new String[list.size()];
                list.toArray(result);
            }
        }
        return result;
    }
}
