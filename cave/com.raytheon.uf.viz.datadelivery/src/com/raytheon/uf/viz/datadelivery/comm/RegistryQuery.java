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

package com.raytheon.uf.viz.datadelivery.comm;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * An interface to the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/20/12     #218       dhladky    Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class RegistryQuery {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryQuery.class);

    /**
     * Sets a query to return only distinct values from the index.
     */
    public static final int DISTINCTVALUES = 0;

    /**
     * Sets a query to return all values from the index.
     */
    public static final int ALLVALUES = 1;

    private static final int REQUESTTIMEOUT = 60000;

    public static enum Mode {
        DISTINCT, MAX
    };

    /**
     * Find data entries in the Data Delivery Registry
     * 
     * @param fieldName
     *            The fieldname to search for
     * @param queryTerms
     *            A hashmap containing the parameter name/value to search for
     * @return A string array containing the found values or null if nothing is
     *         found
     * @throws VizException
     */
    public static Object[] performQuery(
            Map<String, RequestConstraint> queryTerms) throws VizException {
        String query = assembleQuery(queryTerms, Mode.DISTINCT);
        System.out.println("Query: " + query);
        DatadeliveryConnector connection = DatadeliveryConnector.getInstance();

        Object[] registryObjects = connection.connect(query, null,
                REQUESTTIMEOUT);

        // TODO: Figure out what magic is required for specific queries.
        // Eventually I'd like to make this several methods

        return registryObjects;
    }

    private static String assembleQuery(
            Map<String, RequestConstraint> queryTerms, Mode mode)
            throws VizException {
        HashMap<String, RequestConstraint> map = new HashMap<String, RequestConstraint>(
                queryTerms);
        if (mode == Mode.DISTINCT) {
            map.put("searchField1", new RequestConstraint());

        } else {
            throw new IllegalArgumentException("Unsupported mode: " + mode);
        }

        return ScriptCreator.createScript(map, 1, "catalog");
    }

}
