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
package com.raytheon.uf.viz.datadelivery.browser;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Data Browser Utility Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2013   1588     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataBrowserUtils {

    private static final Pattern WILDCARD_PATTERN = Pattern.compile("\\*");

    private static final Pattern SPACES_PATTERN = Pattern.compile("\\s+");

    /**
     * Search a list of items.
     * 
     * @param search
     *            The search string
     * @param fullList
     *            List of items to search
     * @param matchAnyFlag
     *            The match any/all flag, true for match any
     * @param caseSensitiveFlag
     *            The case sensitive flag, true for case sensitive
     * @param excludeSearchFlag
     *            The excludeSearchFlag, true for an exclude search
     * @return List of matching items
     */
    public static List<String> search(String search, String[] fullList,
            boolean matchAnyFlag, boolean caseSensitiveFlag,
            boolean excludeSearchFlag) {
        List<String> results = new ArrayList<String>();

        if (search == null) {
            return results;
        }

        // this is used for match all, holds the matched terms to see if all get
        // matched or not
        List<String> holder = new ArrayList<String>();
        String testCaseItem;

        if (!caseSensitiveFlag) {
            search = search.toLowerCase();
        }

        String[] searchTerms = SPACES_PATTERN.split(search);

        for (String item : fullList) {
            for (String term : searchTerms) {
                if (!caseSensitiveFlag) {
                    testCaseItem = item.toLowerCase();
                } else {
                    testCaseItem = item;
                }

                if (term.contains("*")) {
                    String[] parts = WILDCARD_PATTERN.split(term);
                    boolean valid = true;
                    String part;
                    for (int i = 0; i < parts.length; i++) {
                        if (valid == false) {
                            break;
                        }
                        part = parts[i];

                        if (!testCaseItem.contains(part) != excludeSearchFlag) {
                            valid = false;
                            continue;
                        }
                        if (i > 0) {
                            if (!excludeSearchFlag) {
                                // check the order
                                if (!(testCaseItem.indexOf(parts[i - 1]) < testCaseItem
                                        .indexOf(parts[i]))) {
                                    valid = false;
                                }
                            }
                        }
                    }

                    if (valid) {
                        results.add(item);
                    }
                } else {
                    if (testCaseItem.contains(term) != excludeSearchFlag) {
                        if (matchAnyFlag) {
                            results.add(item);
                        } else {
                            holder.add(term);
                        }
                    }
                }
            }

            if (!matchAnyFlag) {
                if (holder.size() == searchTerms.length) {
                    results.add(item);
                }
                holder.clear();
            }
        }

        return results;
    }
}
