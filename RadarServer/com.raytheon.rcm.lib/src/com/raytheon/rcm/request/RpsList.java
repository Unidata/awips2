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
package com.raytheon.rcm.request;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.ProductInfo.Selector;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;

/**
 * Representation of an RPS list
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2009-04-22   #1693      D. Friedman Initial checkin
 * ...
 * 2013-01-31   DR 15458   D. Friedman Define UNSPECIFIED_VCP
 * </pre>
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class RpsList implements Cloneable {
    /**
     * Indicates an a list that is appropriate for any VCP or an unspecified
     * VCP.
     */
    public static final int UNSPECIFIED_VCP = -1;

	/* just a guess of what will be useful... */
	protected int opMode;
	protected int vcp;
	protected Request[] requests;
	
	// Needed for JAXB
	public RpsList() {
		this(0,0);
	}
	
	public RpsList(int opMode, int vcp) {
		this.opMode = opMode;
		this.vcp = vcp;
		this.requests = new Request[0];
	}

	public RpsList(int opMode, int vcp, Request[] requests) {
		this.opMode = opMode;
		this.vcp = vcp;
		this.requests = requests;
	}

	public int getOpMode() {
		return opMode;
	}
	public void setOpMode(int opMode) {
		this.opMode = opMode;
	}
	public int getVcp() {
		return vcp;
	}
	public void setVcp(int vcp) {
		this.vcp = vcp;
	}
	public Request[] getRequests() {
		return requests;
	}
	public void setRequests(Request[] requests) {
		this.requests = requests;
	}
	
    /**
     * Determine the number of requests in the list as determined by the RPG.
     * This could be more than the number of entries.
     * 
     * @param radarID
     * @param radarType
     * @return the total number of requests or -1 if it could not be
     *         determined
     */
    public int getRequestCount(String radarID, RadarType radarType) {
        return getRequestCount(requests, radarID, vcp, radarType);
    }
	
    /**
     * Determines the number of additional requests that will be added to the
     * list when sent to a TDWR in VCP 80. By convention, the RadarServer adds
     * a request for mini-volume 1 for every request that specifies a
     * mini-volume other than 1 (i.e, 2.)
     * 
     * @return the number of mini-volume requests that will be added
     */
	/*
	 * This must be kept in sync with 
	 * RPSListManager.maybeAddSPGMiniVolumeProducts.
	 */
	public int getAdditionalMiniVolumeProductCount() {
	    int result = 0;
	    Selector sel = new Selector();
	    sel.radarType = RadarType.TDWR;
	    for (Request r : requests) {
	        sel.code = (int) r.productCode;
	        RadarProduct prod = ProductInfo.getInstance().selectOne(sel);
	        if (prod != null && prod.params.contains(Param.MINI_VOLUME) &&
	                r.getMiniVolume() != 1)
	            ++result;
	    }
	    return result;
	}

	public Object clone() {
		RpsList other;
		try {
			other = (RpsList) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new RuntimeException(e);
		}
		other.requests = new Request[requests.length];
		for (int i = 0; i < other.requests.length; ++i)
			other.requests[i] = (Request) requests[i].clone();
		return other;
	}
	
	/**
	 * Determine the number of requests in the list as determined by the RPG.
	 * @see  #getRequestCount(String radarID, RadarType radarType)
	 */
	public static int getRequestCount(Request[] requests, String radarID, int vcp, RadarType radarType) {
	    int elevList[] = ElevationInfo.getInstance().getScanElevations(radarID, vcp);
	    if (elevList != null)
	        return getRequestCount(requests, elevList, radarType);
	    else
	        return -1;
	}
	
    /**
     * Determine the number of requests in the list as determined by the RPG.
     * @see  #getRequestCount(String radarID, RadarType radarType)
     */
	public static int getRequestCount(Request[] requests, int[] elevList, RadarType radarType) {
	    int total = 0;
	    for (Request request : requests) {
	        int count = getRequestCount(request, elevList, radarType);
	        if (count >= 0)
	            total += count;
	        else
	            return -1;
	    }
	    return total;
	}
	
    /**
     * Determine the number of requests in the list as determined by the RPG.
     * @see  #getRequestCount(String radarID, RadarType radarType)
     */
    public static int getRequestCount(Request request, String radarID, int vcp, RadarType radarType) {
        int elevList[] = ElevationInfo.getInstance().getScanElevations(radarID, vcp);
        if (elevList != null)
            return getRequestCount(request, elevList, radarType);
        else
            return -1;
    }
    
    /**
     * Determine the number of requests in the list as determined by the RPG.
     * @see  #getRequestCount(String radarID, RadarType radarType)
     */
    /* Logic based on CODE 88D src/cpc101/lib003/orpgprq.c: ORPGPRQ_get_requested_elevations */
	public static int getRequestCount(Request request, int[] elevList, RadarType radarType) {
        Selector sel = new Selector(radarType, null, Integer.valueOf(request.productCode), null);
        RadarProduct rp = ProductInfo.getInstance().selectOne(sel);
        if (rp != null) {
            if (rp.params.contains(Param.ELEVATION)) {
                int elevationSelection = request.getElevationSelection();
                switch (elevationSelection) {
                case Request.SPECIFIC_ELEVATION:
                    return elevList.length > 0 ? 1 : 0;
                case Request.LOWER_ELEVATIONS:
                {
                    int count = 0;
                    int requestedAngle = request.getElevationAngle();
                    for (int angle : elevList)
                        if (angle <= requestedAngle)
                            ++count;
                    return count; 
                } 
                case Request.N_ELEVATIONS:
                    return Math.min(request.getElevationAngle(), elevList.length);
                case Request.ALL_ELEVATIONS:
                    if (request.getElevationAngle() == 0)
                        return elevList.length;
                    else {
                        /* Find all angles that differ by less than 0.01 
                         * degrees.  Our units are in 0.1 degrees so just
                         * test for equality.
                         */
                        int count = 0;
                        int requestedAngle = request.getElevationAngle();
                        for (int angle : elevList)
                            if (angle == requestedAngle)
                                ++count;
                        return count;
                    }
                default:
                    return -1;
                }
            } else {
                return 1;
            }
        } else
            return -1;
	}
	
}
