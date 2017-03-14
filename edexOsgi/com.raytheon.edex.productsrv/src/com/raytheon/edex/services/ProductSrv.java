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

import jep.JepException;

import com.raytheon.edex.productsrv.PythonCallable;
import com.raytheon.edex.productsrv.PythonExecThreadPool;
import com.raytheon.edex.uengine.ResponseUtil;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2008            njensen     Initial creation
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProductSrv {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ProductSrv.class);

    private static final IUFStatusHandler productSrvLogger = UFStatus
            .getNamedHandler("ProductSrvRequestLogger");

    private static final long LOG_TIME_THRESHOLD = 200; // milliseconds

    private static final long LOG_SCRIPT_THRESHOLD = 5000; // millseconds

    /**
     * Executes a python script in the uEngine
     * 
     * @param scriptText
     *            the python to execute
     * @return a Message object containing AbstractResponseMessages
     */
    @SuppressWarnings(value = "unchecked")
    public Message executePython(String scriptText) {
        if (productSrvLogger.isPriorityEnabled(Priority.DEBUG)) {
            productSrvLogger.debug("Running ProductSrv script:\n" + scriptText);
        }

        long t0 = System.currentTimeMillis();

        ArrayList<AbstractResponseMessage> responseList = null;
        // create PythonCallable
        PythonCallable callable = new PythonCallable(scriptText);
        // send the callable to the thread pool, catch any exceptions and create
        // and error response
        Object result = null;
        try {
            PythonExecThreadPool pool = PythonExecThreadPool.getInstance();
            result = pool.queueScriptBlocking(callable);
        } catch (Exception e1) {
            responseList = new ArrayList<AbstractResponseMessage>();
            ResponseMessageError empty = ResponseMessageError
                    .generateErrorResponse(
                            "UEngine expects scripts to return AbstractResponseMessages"
                                    + " or ArrayList<AbstractResponseMessage>.",
                            new Exception(e1));
            responseList.add(empty);
        }

        // handle the return object
        if (result instanceof JepException) {
            JepException e = (JepException) result;
            responseList = new ArrayList<AbstractResponseMessage>();
            ResponseMessageError error = ResponseMessageError
                    .generateErrorResponse("Error executing script.",
                            new Exception(e));
            responseList.add(error);
            logger.error("The following script caused an error:\n" + scriptText
                    + "\n", e);
        } else if (result instanceof ArrayList) {
            responseList = (ArrayList<AbstractResponseMessage>) result;
        } else if (result instanceof AbstractResponseMessage) {
            responseList = new ArrayList<AbstractResponseMessage>();
            responseList.add((AbstractResponseMessage) result);
        } else if (result == null) {
            responseList = new ArrayList<AbstractResponseMessage>();
            ResponseMessageError empty = ResponseMessageError
                    .generateErrorResponse(
                            "Script returned empty response list.", null);
            responseList.add(empty);
        } else {
            responseList = new ArrayList<AbstractResponseMessage>();
            ResponseMessageError empty = ResponseMessageError
                    .generateErrorResponse(
                            "UEngine expects scripts to return AbstractResponseMessages"
                                    + " or ArrayList<AbstractResponseMessage>.",
                            null);
            responseList.add(empty);
        }

        Message msg = ResponseUtil
                .createMessageObject(null, null, responseList);
        long t1 = System.currentTimeMillis();
        long timeTaken = t1 - t0;
        if (productSrvLogger.isPriorityEnabled(Priority.DEBUG)) {
            int size = 0;
            try {
                size = SerializationUtil.transformToThrift(msg).length;
            } catch (Exception e) {
                productSrvLogger.handle(Priority.WARN,
                        "Unable to serialize response to determine size", e);
            }
            String sizeType = " bytes";
            if (size > 1024) {
                size /= 1024;
                sizeType = " kb";
            }
            if (size > 1024) {
                size /= 1024;
                sizeType = " mb";
            }
            productSrvLogger.debug("ProductSrv took " + timeTaken
                    + "ms and generated response of " + size + sizeType);
        } else if (timeTaken > LOG_TIME_THRESHOLD) {
            logger.info("ProductSrv took " + (t1 - t0) + "ms");
            if (timeTaken > LOG_SCRIPT_THRESHOLD) {
                logger.info("ProductSrv script was:\n" + scriptText);
            }
        }

        return msg;
    }

}
