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
package com.raytheon.edex.plugin.gfe.isc;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.request.IscDataRecRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

import jep.JepException;

/**
 * ISC data receive service. Takes incoming request and executes iscDataRec
 * script using provided parameters.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 05, 2012  361      dgilling  Initial creation
 * Mar 12, 2013  1759     dgilling  Re-implement using IscScript.
 * Mar 14, 2013  1794     djohnson  Consolidate common FilenameFilter
 *                                  implementations.
 * Dec 10, 2014  4953     randerso  Properly handle single file reception
 * May 06, 2015  4383     dgilling  Properly XML parse incoming XML file.
 * May 20, 2015  4491     dgilling  Remediate path manipulation possibilities.
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 * Oct 19, 2017  6279     randerso  Moved prepareIscDataRec to
 *                                  IscDataRecRequestHandler. Removed thread
 *                                  pool.
 *
 * </pre>
 *
 * @author dgilling
 */

public class IscReceiveSrv {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscReceiveSrv.class);

    private static final String METHOD_NAME = "main";

    private static final String ISC_DATA_REC_SCRIPT_NAME = "iscDataRec";

    private static final ThreadLocal<IscScript> threadLocalScript = new ThreadLocal<IscScript>() {

        @Override
        protected IscScript initialValue() {
            try {
                statusHandler.info(
                        String.format("Creating new %s script for thread %s.",
                                ISC_DATA_REC_SCRIPT_NAME,
                                Thread.currentThread().getName()));
                return new IscScript(ISC_DATA_REC_SCRIPT_NAME + ".py");
            } catch (JepException e) {
                statusHandler.error(
                        String.format("Unable to create GFE ISC script [%s]",
                                ISC_DATA_REC_SCRIPT_NAME),
                        e);
            }

            return null;
        }
    };

    /**
     * Process IscDataRecRequest
     *
     * @param request
     */
    public void processRequest(IscDataRecRequest request) {
        String[] reqArgs = request.getArgString().trim().split(" ");
        Map<String, Object> args = new HashMap<>();
        args.put("argv", Arrays.asList(reqArgs));
        IscScript script = threadLocalScript.get();
        try {
            script.execute(METHOD_NAME, args, request.getSiteID());
        } catch (JepException e) {
            String msg = "Python error executing GFE ISC script ["
                    + script.getScriptName() + "]: " + e.getLocalizedMessage();
            statusHandler.handle(Priority.ERROR, msg, e);
        } catch (Throwable t) {
            String msg = "Unexpected Java Exception executing GFE ISC script ["
                    + script.getScriptName() + "]: " + t.getLocalizedMessage();
            statusHandler.handle(Priority.ERROR, msg, t);
        }
    }

}
