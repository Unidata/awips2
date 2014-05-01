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
package com.raytheon.edex.services;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.uengine.ResponseUtil;
import com.raytheon.edex.uengine.runners.IMicroEngine;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Product service based on injection of the &mu;Engine script runner type.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12Nov2008    1709        MW Fegan    Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */

public class PlgProductSrv {
    /**
     * This is the &mu;Engine instance used to run the scripts.
     * It will normally be set by the container via dependency
     * injection.
     */
    private IMicroEngine runner = null;

    private Log logger = LogFactory.getLog(getClass());

    
 
    public Object process(String scriptText) throws EdexException {
        String dataURI = ResponseUtil.EMPTY_DATA_URI;

       
        if (this.runner == null) {
            String msg = "Endoint misconfigured - no script runner specified. Contact System Administrator.";
            logger.error(msg);
            ResponseMessageError error = ResponseMessageError.generateErrorResponse(msg, null);
            return ResponseUtil.createErrorMessage(error);
        }
        List<AbstractResponseMessage> responseList = null;
        try {
            runner.initialize(scriptText);
            runner.execute();
            responseList = runner.getResult();
        } catch (Exception e) {
            responseList = new ArrayList<AbstractResponseMessage>();
            ResponseMessageError error = ResponseMessageError
                    .generateErrorResponse("Error executing script.",e);
            responseList.add(error);
        } finally {
            runner.release();
        }

        Message msg = ResponseUtil.createMessageObject(dataURI, "", responseList);
        return msg;
    }


    /**
     * Returns the runner <code>IMicroEngine</code> script runner
     * configured for this end point.
     */
    public IMicroEngine getRunner() {
        return runner;
    }

    /**
     * Sets the runner <code>IMicroEngine</code> script runner
     * configured for this end point.
     */
    public void setRunner(IMicroEngine runner) {
        this.runner = runner;
    }
}
