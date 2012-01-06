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

package com.raytheon.uf.edex.alertviz.handler;

import com.raytheon.edex.uengine.Activator;
import com.raytheon.uf.common.alertviz.AlertVizRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/19/2010   5849       cjeanbap    Initial creation
 * 11/22/2010   2235       cjeanbap    Pass audioFile value to EDEXUtility.
 * 02/02/2011   6500       cjeanbap    Pass source value to EDEXUtility.
 * 03/14/2011   5149	   cjeanbap	   Removed debug log statements.
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */

public class AlertVizRequestHandler implements IRequestHandler<AlertVizRequest> {

    private static final String RESULT = "None";

    /**
     * Default constructor
     */
    public AlertVizRequestHandler() {
        super();
    }

    @Override
    public Object handleRequest(AlertVizRequest request) throws Exception {

        String sourceKey = request.getSourceKey();
        Priority priority = findPriority(request);
        String pluginName = Activator.PLUGIN_ID;
        String category = request.getCategory();
        String message = request.getMessage();
        String details = request.toString();
        String audioFile = request.getAudioFile();

        EDEXUtil.sendMessageAlertViz(priority, pluginName, sourceKey, category,
                message, details, audioFile);

        return RESULT;
    }

    private Priority findPriority(AlertVizRequest request) {
        return Priority.valueOf(request.getPriority());
    }
}
