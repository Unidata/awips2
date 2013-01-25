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
package com.raytheon.uf.edex.ebxml.registry.memory.matchers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.ConstructorUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ebxml.query.matcher.AndMatcher;
import com.raytheon.uf.edex.ebxml.query.matcher.CompositeMatcher;
import com.raytheon.uf.edex.ebxml.query.matcher.IMatcher;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class MatcherManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MatcherManager.class);

    private static final IMatcher DEFAULT = new DefaultMatcher();

    private static Map<String, Class<?>> matcherMap = new HashMap<String, Class<?>>();
    static {
        matcherMap.put("name", NameMatcher.class);
        matcherMap.put("description", DescriptionMatcher.class);
        matcherMap.put("objectType", ObjectTypeMatcher.class);
        matcherMap.put("status", StatusMatcher.class);
        matcherMap.put("classifications", ClassificationMatcher.class);
    }

    public static IMatcher getMatcher(String fieldName, List<Object> values) {
        String[] vals = new String[values.size()];
        for (int i = 0; i < values.size(); i++) {
            vals[i] = (String) values.get(i);
        }
        return getMatcher(fieldName, vals);
    }

    public static IMatcher getMatcher(String fieldName, String[] values) {
        Class<?> matcherClass = matcherMap.get(fieldName);
        if (matcherClass == null) {
            return DEFAULT;
        }

        CompositeMatcher andMatcher = null;
        andMatcher = null;
        for (String value : values) {
            IMatcher subMatcher = null;
            try {
                subMatcher = (IMatcher) ConstructorUtils.invokeConstructor(
                        matcherClass, value);
            } catch (Exception e) {
                statusHandler.error("Error instantiating matcher class", e);
            }
            if (values.length == 1) {
                return subMatcher;
            } else {
                if (andMatcher == null) {
                    andMatcher = new AndMatcher();
                }
                andMatcher.add(subMatcher);
            }
        }

        return DEFAULT;
    }
}
