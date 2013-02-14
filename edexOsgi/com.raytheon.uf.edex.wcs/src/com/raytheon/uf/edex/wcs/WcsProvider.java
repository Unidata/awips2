/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package com.raytheon.uf.edex.wcs;

import java.io.InputStream;

import javax.servlet.http.HttpServletResponse;

import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wcs.provider.OgcWcsProvider.WcsOpType;
import com.raytheon.uf.edex.wcs.request.DescCoverageRequest;
import com.raytheon.uf.edex.wcs.request.GetCapRequest;
import com.raytheon.uf.edex.wcs.request.GetCoverageRequest;
import com.raytheon.uf.edex.wcs.request.WcsRequest;

/**
 * TODO - Class comment here
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */

public interface WcsProvider {

	public OgcResponse getCapabilities(OgcServiceInfo<WcsOpType> serviceinfo,
			GetCapRequest request);

	public OgcResponse describeCoverageType(
			OgcServiceInfo<WcsOpType> serviceinfo, DescCoverageRequest request);

	public void getCoverage(OgcServiceInfo<WcsOpType> serviceinfo,
			GetCoverageRequest request, HttpServletResponse response);

	public WcsRequest getRequest(InputStream in);

	public OgcResponse getError(WcsException e, String exceptionFormat);

}
