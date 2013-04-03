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

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.resp.AuthServerErrorResponse;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestWrapper;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.SizeUtil;

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
 * Jul 24, 2012              njensen         Enhanced logging
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RemoteRequestRouteWrapper {

    private static final IUFStatusHandler thriftSrvLogger = UFStatus
            .getNamedHandler("ThriftSrvRequestLogger");

    private RemoteRequestServer server;

    public byte[] executeThrift(byte[] data) {
        try {
            long startTime = System.currentTimeMillis();
            Object obj = SerializationUtil.transformFromThrift(data);
            IServerRequest request = null;
            if (obj instanceof RequestWrapper) {
                request = ((RequestWrapper) obj).getRequest();
            } else {
                request = (IServerRequest) obj;
            }
            Object rvalObj = server.handleThriftRequest(request);
            byte[] rval = SerializationUtil.transformToThrift(rvalObj);
            long endTime = System.currentTimeMillis();
            StringBuilder sb = new StringBuilder(300);
            sb.append("Handled ").append(obj.toString()).append(" in ")
                    .append((endTime - startTime)).append("ms, ");
            sb.append("request was size ").append(
                    SizeUtil.prettyByteSize(data.length));
            sb.append(", response was size ").append(
                    SizeUtil.prettyByteSize(rval.length));
            thriftSrvLogger.info(sb.toString());
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
