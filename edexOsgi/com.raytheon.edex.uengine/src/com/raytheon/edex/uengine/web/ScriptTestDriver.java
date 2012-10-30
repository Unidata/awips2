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

package com.raytheon.edex.uengine.web;

import java.io.File;
import java.util.HashMap;

import com.raytheon.edex.scriptfactory.ScriptFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Creates the scripts on the fly based on input from the web page.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    11/25/06                  brockwoo    Initial Creation.
 *    12/07/06      #108        brockwoo    Added reproject to the grid and
 *                                          radar test interfaces
 *    05/10/07      #273        brockwoo    Updated script generation to
 *                                          Javascript
 *    11/27/07      #214        brockwoo    Added colormap capability to radar
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
public class ScriptTestDriver {

    private static File DEFAULT_TEMPLATE;

    public ScriptTestDriver() {
    }

    public String createScript(HashMap<String, String> terms,
            String imageCount, String engine) {
        if (DEFAULT_TEMPLATE == null) {
            DEFAULT_TEMPLATE = new File(
                    "../webapps/uEngineWeb/templates/standardTemplate.vm");
        }

        HashMap<String, RequestConstraint> queryTerms = new HashMap<String, RequestConstraint>();

        String pluginName = null;
        String library = "BaseRequest";

        for (String term : terms.keySet()) {
            RequestConstraint newRequest = new RequestConstraint();
            newRequest.setConstraintValue(terms.get(term));
            queryTerms.put(term, newRequest);
            if ("pluginName".equals(term)) {
                pluginName = terms.get(term);
            }
        }

        String script = null;

        if (pluginName == null) {
            script = new String();
        } else {
            if ("grid".equals(pluginName)) {
                library = "GridRequest";
            } else if ("satellite".equals(pluginName)) {
                library = "SatelliteRequest";
            } else if ("radar".equals(pluginName)) {
                library = "RadarRequest";
            } else if ("obs".equals(pluginName)) {
                library = "ObsRequest";
            }
        }

        try {
            int maxRecords = Integer.parseInt(imageCount);

            String logDir = "";
            EnvProperties properties = PropertiesFactory.getInstance()
                    .getEnvProperties();
            if (properties != null) {
                logDir = properties.getEnvValue("LOGDIR");
            }
            script = ScriptFactory.getInstance().createScript(DEFAULT_TEMPLATE,
                    DEFAULT_TEMPLATE.getParentFile(), maxRecords, engine,
                    library, queryTerms, logDir);
        } catch (Exception e) {
            System.out.println("Error creating script for " + library);
            e.printStackTrace();
        }
        return script;
    }
}
