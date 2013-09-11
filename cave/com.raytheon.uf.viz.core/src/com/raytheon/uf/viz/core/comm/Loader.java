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
package com.raytheon.uf.viz.core.comm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Loader - Loads data from a script or LayerProperty object.
 * 
 * This interface provides an API for loading data with time matching.
 * 
 * NOTE: Most users should use RequestJob to facilitate threading
 * 
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Aug 13, 2007             chammack    Initial Creation.
 *    Dec 03, 2007 461         bphillip    Modified Time Matching to use VizTim
 *    Aug 19, 2009 2586        rjpeter     Updated error handling.
 *    Jul 05, 2013 1869        bsteffen    Fix goes sounding updates.
 *    Sep  9, 2013 2277        mschenke    Got rid of LayerProperty references
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class Loader {

    public static final String SELECT_MODE = "select";

    public static final String ROW_NAME = "rowname";

    public static final String CLASS_NAME = "classname";

    public static final String DATABASE_NAME = "databasename";

    /**
     * Private constructor
     */
    private Loader() {

    }

    /**
     * Takes an array of scripts and returns a list of responses returned from
     * the scripts
     * 
     * @param scripts
     *            the scripts to use
     * @param timeOut
     *            the time out in milliseconds
     * @return a list of responses returned by the scripts
     * @throws VizException
     */
    public static List<Object> loadScripts(String[] scripts, int timeOut)
            throws VizException {
        List<Object> responseList = new ArrayList<Object>();
        String curScript = null;

        try {
            for (int k = 0; k < scripts.length; k++) {
                curScript = scripts[k];
                Object[] response = Connector.getInstance().connect(curScript,
                        null, timeOut);

                if (response != null) {
                    for (Object r : response) {
                        responseList.add(r);
                    }
                }
            }
        } catch (Exception e1) {
            throw new VizException("Script: " + curScript + "\nError: "
                    + e1.getMessage(), e1);
        }

        return responseList;
    }

    /**
     * Load a plugin data object from a data uri
     * 
     * @param obj
     *            the request map
     * @return the fully filled out plugindataobject
     * @throws VizException
     */
    public static PluginDataObject loadData(String dataURI) throws VizException {
        Map<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();

        try {
            vals.putAll(RequestConstraint.toConstraintMapping(DataURIUtil
                    .createDataURIMap(dataURI)));
        } catch (PluginException e) {
            throw new VizException(e);
        }

        DbQueryRequest request = new DbQueryRequest(vals);
        request.setLimit(1);
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(request);
        PluginDataObject[] pdos = response
                .getEntityObjects(PluginDataObject.class);
        if (pdos.length == 0) {
            return null;
        }
        return pdos[0];
    }

    /**
     * Loads a list of objects from a script
     * 
     * @param script
     *            the EDEX script
     * @param timeOut
     *            the time out in milliseconds
     * @return a list of objects
     * @throws VizException
     */
    public static List<Object> loadData(String script, int timeOut)
            throws VizException {
        Validate.notNull(script, "Script can not be null");

        List<Object> responses = loadScripts(new String[] { script }, timeOut);

        return responses;
    }

}