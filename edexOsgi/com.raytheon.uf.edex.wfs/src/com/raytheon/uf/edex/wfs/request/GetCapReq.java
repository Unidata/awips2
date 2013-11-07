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
package com.raytheon.uf.edex.wfs.request;

import net.opengis.wfs.v_1_1_0.GetCapabilitiesType;

/**
 * Request wrapper for WFS get capabilities request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GetCapReq extends WfsRequest {

	/**
	 * @param type
	 */
	public GetCapReq() {
		super(Type.GetCapabilities);
	}

	public GetCapReq(GetCapabilitiesType getCap) {
		super(Type.GetCapabilities);
		setRawrequest(getCap);
    }

    /**
     * @param obj
     */
    public GetCapReq(net.opengis.wfs.v_2_0_0.GetCapabilitiesType getCap) {
        super(Type.GetCapabilities);
        setRawrequest(getCap);
    }

}
