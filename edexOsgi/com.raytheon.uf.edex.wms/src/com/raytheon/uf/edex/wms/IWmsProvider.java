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
 **********************************************************************/
package com.raytheon.uf.edex.wms;

import java.io.InputStream;

import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.http.MimeType;

/**
 * Interface for WMS provider
 * 
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      renamed for standards
 */
public interface IWmsProvider {

	public static final String WMS_NAME = "WMS";

	public enum WmsOpType {
		GetCapabilities, GetMap, GetFeatureInfo, GetLegendGraphic
	}

	public OgcResponse getCapabilities(BaseRequest<WmsOpType> req);

	public OgcResponse getMap(GetMapRequest req);

    public OgcResponse getError(WmsException e, MimeType exceptionFormat);

	public OgcResponse handlePost(InputStream in);

	public OgcResponse getFeatureInfo(GetFeatureInfoRequest req);

	public OgcResponse getLegendGraphic(GetLegendGraphicRequest req);
}
