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
package com.raytheon.uf.viz.alertview.filter;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.viz.alertview.Alert.Priority;

/**
 * 
 * Provides a lookup mechanism for getting {@link AlertFilter}s using an id.
 * This is especially useful when a filter is stored in a preference file
 * because the id provides a convenient representation to store it and the
 * manager provides a reliable way to lookup filters.
 * 
 * TODO the current implementation just uses a small set of hard-coded filters.
 * This should be implemented so that new filters can be added by other plugins
 * and possibly even allow user defined filters to do things like regex the
 * message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class FilterManager {

    /** Name for a filter that accepts all alerts. */
    public static final String ALL = "all";

    /** Name for a filter that accepts no alerts. */
    public static final String NONE = "none";

    /** Name for a filter that accepts alerts with priority >= WARN. */
    public static final String WARN_PLUS = "warnPlus";

    /** Name for a filter that accepts alerts with priority >= INFO. */
    public static final String INFO_PLUS = "infoPlus";

    private static Logger logger = LoggerFactory.getLogger(FilterManager.class);

    private static final AlertFilter NONE_FILTER = new ConstantFilter(false);

    private final Map<String, AlertFilter> filters = new HashMap<>();

    public FilterManager() {
        loadDefaultFilters();
    }

    private void loadDefaultFilters() {
        filters.put(ALL, new ConstantFilter(true));
        filters.put(NONE, new ConstantFilter(false));
        for (Priority p : Priority.values()) {
            filters.put(p.name().toLowerCase(), new PriorityFilter(p));
        }
        filters.put(WARN_PLUS, new MinPriorityFilter(Priority.WARN));
        filters.put(INFO_PLUS, new MinPriorityFilter(Priority.INFO));
    }

    public AlertFilter getFilter(String id) {
        AlertFilter filter = filters.get(id);
        if (filter == null) {
            logger.warn("AlertView FilterManager failed to find a filter with an id of "
                    + id);
            filter = NONE_FILTER;
        }
        return filter;
    }

}
