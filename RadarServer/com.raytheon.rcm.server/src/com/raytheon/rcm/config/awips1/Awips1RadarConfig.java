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
package com.raytheon.rcm.config.awips1;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.request.RpsList;

public class Awips1RadarConfig extends RadarConfig {
	// not really public...
	private RpsList[] nationalRpsLists;
	private RpsList[] localRpsLists;

	public RpsList[] getNationalRpsLists() {
		return nationalRpsLists;
	}
	public void setNationalRpsLists(RpsList[] nationalRpsLists) {
		this.nationalRpsLists = nationalRpsLists;
	}
	public RpsList[] getLocalRpsLists() {
		return localRpsLists;
	}
	public void setLocalRpsLists(RpsList[] localRpsLists) {
		this.localRpsLists = localRpsLists;
	}
	
	public RpsList getLocalRpsList(int opMode, int vcp) {
		if (localRpsLists != null) {
			for (RpsList l : localRpsLists)
				if (l.getOpMode() == opMode && 
						(opMode == GSM.OP_MODE_MAINTENANCE || l.getVcp() == vcp))
					return l;
		}
		return null;
	}
	
	/* AWIPS 1 handling of the national RPS list for TDWRs has an additional
	 * quirk.  See Awips1RpsListUtil.maybeTransformNationalRpsList.
	 */

	public RpsList getNationalRpsList(int opMode, int vcp, int[] cuts) {
		// TODO: check for tdwr and opMode != storm since we will not
		// have such a thing?
		if (nationalRpsLists != null) {
			for (RpsList l : nationalRpsLists)
				if (l.getOpMode() == opMode  /*&& l.getVcp() == vcp*/) {
					return Awips1RpsListUtil.maybeTransformForTDWR(this, l, cuts);
				}
		}
		return null;
	}
}
