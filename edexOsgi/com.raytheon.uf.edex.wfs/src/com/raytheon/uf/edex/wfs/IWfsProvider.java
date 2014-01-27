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
 * 
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      renamed for standards
 *
 */
package com.raytheon.uf.edex.wfs;

import java.io.InputStream;
import java.util.Map;

import com.raytheon.uf.edex.ogc.common.http.EndpointInfo;
import com.raytheon.uf.edex.ogc.common.output.IOgcHttpResponse;

/**
 * Interface for WFS version specific implementations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * November 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface IWfsProvider {

    public enum WfsOpType {
        GetCapabilities, GetFeature, DescribeFeatureType, ListStoredQueries, DescribeStoredQueries, GetPropertyValue
    }

    public String getVersion();

    public void handlePost(InputStream body, EndpointInfo info,
            IOgcHttpResponse response) throws Exception;

    public void handleGet(Map<String, Object> headers, EndpointInfo info,
            IOgcHttpResponse response) throws Exception;

    /**
     * Return the OGC HTTP (get/post) service location in the form [:port]/path
     * for appending to the hostname of the server
     * 
     * @return
     */
    public String getHttpServiceLocation();

}
