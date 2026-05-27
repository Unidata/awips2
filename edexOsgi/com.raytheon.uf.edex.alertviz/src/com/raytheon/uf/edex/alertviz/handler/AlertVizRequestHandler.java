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

import java.util.Map;

import com.raytheon.uf.common.alertviz.AlertVizRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * AlertViz Request Handler
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 19, 2010  5849     cjeanbap  Initial creation
 * Nov 22, 2010  2235     cjeanbap  Pass audioFile value to EDEXUtility.
 * Feb 02, 2011  6500     cjeanbap  Pass source value to EDEXUtility.
 * Mar 14, 2011  5149     cjeanbap  Removed debug log statements.
 * Feb 27, 2013  1638     mschenke  Removed dependency on uengine project
 * Jul 27, 2015  4654     skorolev  Added filters
 * Sep 11, 2018  7456     randerso  Changed the details string for
 *                                  AlertVizRequests
 *
 * </pre>
 *
 * @author cjeanbap
 */

public class AlertVizRequestHandler
        implements IRequestHandler<AlertVizRequest> {

    private static final String PLUGIN_ID = "com.raytheon.uf.edex.alertviz";

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
        String category = request.getCategory();
        String message = request.getMessage();
        String audioFile = request.getAudioFile();
        Map<String, String> filters = request.getFilters();

        StringBuilder details = new StringBuilder();
        details.append(request.getMachine()).append(" | ");
        details.append('(').append(priority.ordinal()).append(") | ");
        details.append(sourceKey).append(" | ");
        details.append(category);

        EDEXUtil.sendMessageAlertViz(priority, PLUGIN_ID, sourceKey, category,
                message, details.toString(), audioFile, filters);

        return RESULT;
    }

    private Priority findPriority(AlertVizRequest request) {
        return Priority.valueOf(request.getPriority());
    }
}
