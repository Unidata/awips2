package com.raytheon.uf.common.monitor.scan;
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

/**
 * SCTI Utilities
 * 
 * Generates the CWA Threat messages for SCTI
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/06/2009   2037       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */


public class SCTI {
	/**
	 * gets the threat assessment message
	 * @param value
	 * @return
	 */
	public static String getSCTImessage(int value) {
		String message = null;
	    switch (value) {
		case 0:
		    message = "Miminal or No Activity";
		    break;
	        case 10:
	            message = "General Thunderstorms";
	            break;
	        case 30:
	            message = "SvrWxProb<30%, MESO or TVS";
	            break;
	        case 40:
	            message = "SvrWxProb<30%, MESO and TVS";
	            break;
	        case 50:
	            message = "30<=SvrWxProb<70%";
	            break;
	        case 60:
	            message = "30<=SvrWxProb<70%, MESO or TVS";
	            break;
	        case 70:
	            message = "30<=SvrWxProb<70%, MESO and TVS";
	            break;
	        case 80:
	            message = "SvrWxProb>=70%";
	            break;
	        case 90:
	            message = "SvrWxProb>=70%, MESO or TVS";
	            break;
	        case 100:
	            message = "SvrWxProb>=70%, MESO and TVS";
	            break;
	        default:
	            message = "Not Evaluated";
	            break;
	    }
	    return message;
	}

	
}
