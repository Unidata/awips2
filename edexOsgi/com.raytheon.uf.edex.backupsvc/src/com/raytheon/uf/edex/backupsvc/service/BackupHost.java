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
package com.raytheon.uf.edex.backupsvc.service;

import java.text.ParseException;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.backupsvc.request.GetBackupServiceCapabilitiesRequest;
import com.raytheon.uf.common.backupsvc.request.GetEDEXVersionRequest;
import com.raytheon.uf.common.backupsvc.response.GetEDEXVersionResponse;
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestWrapper;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.common.util.app.Version;
import com.raytheon.uf.common.util.rate.TokenBucket;

/**
 * Backup host and optional port as specified in config XML file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2016 5937       tgurney     Initial creation
 * Dec  9, 2016 5937       tgurney     Add copy constructor
 * Jul 20, 2017 6352       tgurney     Add equals(), hashCode(), DynamicSerialize
 * Jul 24, 2017 6352       tgurney     Move request-sending from BackupService
 *
 * </pre>
 *
 * @author tgurney
 */

@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class BackupHost {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final int DEFAULT_THRIFT_PORT = Integer
            .parseInt(System.getenv("HTTP_PORT"));

    private static final String THRIFT_HTTP_PATH = System
            .getenv("HTTP_SERVER_PATH") + "/thrift";

    private static final String MY_HOST_NAME = SystemUtil.getHostName();

    @DynamicSerializeElement
    @XmlElement
    private int port = DEFAULT_THRIFT_PORT;

    @DynamicSerializeElement
    @XmlElement(required = true)
    private String name;

    private String hostEdexVersion;

    /**
     * No-arg constructor for serialization
     */
    public BackupHost() {
    }

    /**
     * Copy constructor
     *
     * @param aBackupHost
     *            The BackupHost to copy
     */
    public BackupHost(BackupHost aBackupHost) {
        port = aBackupHost.port;
        name = aBackupHost.name;
        hostEdexVersion = aBackupHost.hostEdexVersion;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (name == null ? 0 : name.hashCode());
        result = prime * result + port;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!obj.getClass().equals(BackupHost.class)) {
            return false;
        }
        BackupHost other = (BackupHost) obj;
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        if (port != other.port) {
            return false;
        }
        return true;
    }

    /**
     * Send an IServerRequest to this host
     *
     * @param request
     * @return the object the server returns
     * @throws Exception
     *             If sending the request fails for some reason
     */
    public Object sendRequest(IServerRequest request) throws Exception {
        return sendRequest(request, null);
    }

    /**
     * Send an IServerRequest to this host
     *
     * @param request
     * @param rateLimiter
     *            Token bucket to rate-limit the request, can be null
     * @return the object the server returns
     * @throws Exception
     *             If sending the request fails for some reason
     */
    public Object sendRequest(IServerRequest request, TokenBucket rateLimiter)
            throws Exception {
        RequestWrapper wrapper = new RequestWrapper(request, new WsId());
        Object rval = null;
        if (rateLimiter != null) {
            rval = HttpClient.getInstance().postDynamicSerialize(
                    "http://" + name + ":" + port + THRIFT_HTTP_PATH, wrapper,
                    true, rateLimiter);
        } else {
            rval = HttpClient.getInstance().postDynamicSerialize(
                    "http://" + name + ":" + port + THRIFT_HTTP_PATH, wrapper,
                    true);
        }
        if (rval instanceof ServerErrorResponse) {
            ServerErrorResponse resp = (ServerErrorResponse) rval;
            Throwable serverException = ExceptionWrapper
                    .unwrapThrowable(resp.getException());
            throw new BackupServiceException(serverException.getMessage(),
                    serverException);
        }
        return rval;
    }

    /**
     * @param useCached
     *            If true, use cached version string if it is available
     * @return version string. Will query the remote host if necessary. null if
     *         there is no cached value and the host could not be contacted.
     */
    public String getEDEXVersion(boolean useCached) {
        if (hostEdexVersion == null || !useCached) {
            try {
                GetEDEXVersionRequest request = new GetEDEXVersionRequest();
                request.setRequestingHost(MY_HOST_NAME);
                Object response = sendRequest(new GetEDEXVersionRequest());
                if (response instanceof GetEDEXVersionResponse) {
                    hostEdexVersion = ((GetEDEXVersionResponse) response)
                            .getEdexVersion();
                }
            } catch (Exception e) {
                logger.error(
                        "Error when sending GetEDEXVersionRequest to " + name,
                        e);
                hostEdexVersion = null;
            }
        }
        return hostEdexVersion;
    }

    /*
     * @return version string. Will use cached version string if it is
     * available, otherwise will query the remote host.
     */
    public String getEDEXVersion() {
        return getEDEXVersion(true);
    }

    /**
     * @param minVersionRequired
     *            If null, defaults to this host's EDEX version
     * @param maxVersionRequired
     *            If null, defaults to this host's EDEX version
     * @return Zero if this host's EDEX version falls within the range of
     *         (minVersionRequired, maxVersionRequired). A positive integer if
     *         this host's EDEX version is greater than maxVersionRequired. A
     *         negative integer if this host's EDEX version is less than
     *         minVersionRequired. Null if unable to contact the host
     */
    public Integer compareVersion(String minVersionRequired,
            String maxVersionRequired) {
        // refresh cached version string
        getEDEXVersion(false);
        if (hostEdexVersion == null) {
            return null;
        }
        if (minVersionRequired == null) {
            minVersionRequired = hostEdexVersion;
        }
        if (maxVersionRequired == null) {
            maxVersionRequired = hostEdexVersion;
        }
        Version minVer;
        Version maxVer;
        Version ver;
        try {
            minVer = Version.fromString(minVersionRequired);
            maxVer = Version.fromString(maxVersionRequired);
            ver = Version.fromString(hostEdexVersion);
        } catch (ParseException e) {
            logger.warn("Failed to parse version string (hostEdexVersion: "
                    + hostEdexVersion + ", minVersionRequired: "
                    + minVersionRequired + ", maxVersionRequired: "
                    + maxVersionRequired + ")", e);
            return null;
        }
        if (ver.compareTo(minVer) < 0) {
            // Version is less than the minimum required
            return -1;
        } else if (ver.compareTo(maxVer) > 0) {
            // Version is greater than the max allowed
            return 1;
        } else {
            // Version falls within allowed range
            return 0;
        }
    }

    /** @return List of all this host's capabilities. */
    public Set<String> getCapabilities() {
        Set<String> rval = null;
        try {
            Object response = sendRequest(
                    new GetBackupServiceCapabilitiesRequest());
            if (response instanceof Set<?>) {
                rval = (Set<String>) response;
            }
        } catch (Exception e) {
            logger.error(
                    "Error when sending GetBackupServiceCapabilitiesRequest to "
                            + name,
                    e);
        }
        return rval;
    }
}
