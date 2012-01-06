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
package com.raytheon.uf.edex.auth;

import java.text.DecimalFormat;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.resp.AuthServerErrorResponse;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.serialization.comm.util.ExceptionWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Wrapper for camel route so Serialization exceptions can be caught and
 * returned
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RemoteRequestRouteWrapper {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteRequestRouteWrapper.class);

    private RemoteRequestServer server;

    private static final int MEGABYTE = 1024 * 1024;

    private static final int timeLogLevel;

    private static final long sizeLogLevel;

    static {
        EnvProperties props = PropertiesFactory.getInstance()
                .getEnvProperties();
        int tmp = 0;
        String prop = props.getEnvValue("REQUEST_TIME_FILTER");
        if (prop != null) {
            try {
                tmp = Integer.parseInt(prop);
            } catch (NumberFormatException e) {

            }
        }
        timeLogLevel = (tmp <= 0 ? Integer.MAX_VALUE : tmp);

        tmp = 0;
        prop = props.getEnvValue("RESPONSE_SIZE_FILTER");
        if (prop != null) {
            try {
                tmp = Integer.parseInt(prop) * MEGABYTE;
            } catch (NumberFormatException e) {

            }
        }
        sizeLogLevel = (tmp <= 0 ? Integer.MAX_VALUE : tmp);
    }

    public byte[] executeThrift(byte[] data) {
        try {
            long startTime = System.currentTimeMillis();
            Object obj = SerializationUtil.transformFromThrift(data);
            Object rvalObj = server.handleThriftRequest((IServerRequest) obj);
            byte[] rval = SerializationUtil.transformToThrift(rvalObj);
            long endTime = System.currentTimeMillis();
            if (rval.length > sizeLogLevel
                    || (endTime - startTime) > timeLogLevel) {
                DecimalFormat df = new DecimalFormat("0.##");
                statusHandler.handle(
                        Priority.INFO,
                        "Request " + obj + " took " + (endTime - startTime)
                                + "ms, request was size "
                                + df.format((double) data.length / 1024)
                                + "kb, and has response size "
                                + df.format(((double) rval.length / MEGABYTE))
                                + "mb");
            }
            return rval;
        } catch (AuthException e) {
            AuthServerErrorResponse resp = new AuthServerErrorResponse();
            resp.setUpdatedData(e.getUpdatedData());
            resp.setException(ExceptionWrapper.wrapThrowable(e));
            try {
                return SerializationUtil.transformToThrift(resp);
            } catch (SerializationException e1) {
                e1.printStackTrace();
                return new byte[] {};
            }
        } catch (Throwable t) {
            try {
                ServerErrorResponse resp = new ServerErrorResponse();
                resp.setException(ExceptionWrapper.wrapThrowable(t));
                return SerializationUtil.transformToThrift(resp);
            } catch (SerializationException e) {
                e.printStackTrace();
                return new byte[] {};
            }
        }
    }

    public RemoteRequestServer getServer() {
        return server;
    }

    public void setServer(RemoteRequestServer server) {
        this.server = server;
    }

}
