package com.raytheon.viz.pointdata.def;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * This class returns different types of ConditionalFilterMgr instances. NCP and
 * D2D have different requirements
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/10/2019   72281      K Sunil     Initial Creation
 * Jan 07, 2020 73083      ksunil      Minor changes from code review.
 *
 * </pre>
 *
 * @author ksunil
 */
public class ConditionalFilterFactory {
    public static final String NCP = "NCP";

    public static final String D2D = "D2D";

    private static Map<String, IConditionalFilterMngr> conditionalFilters = new HashMap<>();

    private ConditionalFilterFactory() {
    }

    /**
     * Returs the Conditional Filter Manager instance for the given perspective
     *
     * @param perspective
     *            the perspective
     * @return the Conditional Filter Manager instance
     */

    public static synchronized IConditionalFilterMngr getFilterManagerInstance(
            String perspective) {

        IConditionalFilterMngr instance = conditionalFilters.get(perspective);

        if (instance == null) {
            // Lazily create instance
            if (NCP.equals(perspective)) {
                instance = new ConditionalFilterMngr();
            } else if (D2D.equals(perspective)) {
                instance = new ConditionalFilterMngrD2D();
            }
            // Add it to map
            conditionalFilters.put(perspective, instance);
        }
        return instance;
    }

}
