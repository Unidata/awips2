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
package com.raytheon.rcm.coll;

import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.ProductDistributionInfo;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.MD;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;

/** 
 * Implements central collection of radar products 
 */
public class Collector extends RadarEventAdapter {

	protected RadarServer radarServer;

	public Collector(RadarServer radarServer) {
		this.radarServer = radarServer;
	
		// Call this now to log errors as soon as possible
		Distribution.getMhsDistributionDir();
	}

	@Override
	public void handleRadarEvent(RadarEvent event) {
		if (event.getType() == RadarEvent.CONNECTION_UP) {
			DuplicateInfo inf = duplicateProducts.get(event.getRadarID());
			if (inf != null)
				inf.volumeScanNumber = -1;
			return;
		}
		
		if (event.getType() != RadarEvent.MESSAGE_RECEIVED)
			return;
		
		Configuration config = radarServer.getConfiguration();
		if (! config.isCollectionEnabled())
			return;

		RadarConfig rc = config.getConfigForRadar(
				event.getRadarID());
		if (!rc.isCollectionEnabled())
			return;

		int radarOpMode;
		int vcp; // Only needed for duplicate filtering in VCP 80

		byte[] msg = event.getMessageData();
		int messageCode = Message.messageCodeOf(msg);

		// Simple is-a-product test
		if (messageCode != Message.GSM && messageCode < 16)
			return;

		PDB pdb = null;

		if (messageCode == Message.GSM) {
			GSM gsm = (GSM) MD.decode(msg);
			radarOpMode = gsm.opMode;
			vcp = gsm.vcp; // Not actually used
		} else {
			pdb = GraphicProduct.pdbOfMessage(msg);
			// TODO: This is not a substitute for a real is-a-product-message-code
			// check.
			// If decoding fails, it was not a valid product we don't care about
			// it so there is no need for an exception handler.
			//
			// Could extract all possible message codes that we would care about
			// from the database of products to be sent.
			if (pdb.productCode != Message.messageCodeOf(msg))
				return;

			radarOpMode = pdb.opMode;
			vcp = pdb.vcp;
		}

		if (radarOpMode == GSM.OP_MODE_MAINTENANCE)
			return;

		ProductDistributionInfo prod;
		if (pdb != null)
			prod = radarServer.getConfiguration().getProductDistInfo(
					rc.getRadarID(), pdb);
		else
			prod = radarServer.getConfiguration().getProductDistInfo(
					rc.getRadarID(), messageCode);

		if (prod == null)
			return;

		// TODO: vcp == 80 is hard-coded
		if (vcp == 80 && config.isTdwrCollectionLimited() && pdb != null)
			if (filterDuplicateProducts(rc, messageCode, pdb)) {
				Log.debugf("Matched %s %s for WAN distribution, but was filtered as a duplicate",
						event.getRadarID(), messageCode);
				return;
			}

		Distribution.sendProductMessage(msg, prod.getTtaai(), prod.getNnn(),
                rc.getRadarID(), "matched prodList", radarServer
                        .getConfiguration());
	}

	private static class DuplicateInfo {
		public int volumeScanNumber;
		public HashSet<List<Integer>> sentProducts = 
			new HashSet<List<Integer>>();
		public Calendar volumeScanTime;
	}
	HashMap<String, DuplicateInfo> duplicateProducts =
		new HashMap<String, DuplicateInfo>();
	
	private boolean filterDuplicateProducts(RadarConfig rc, int messageCode, PDB pdb) {
		/* TDWR VCP 80 has duplicate elevations because of multiple 
		 * vertical sweeps ("mini-volumes"). There is a requirement(??) to 
		 * allow reduction of bandwidth by not sending "duplicate 
		 * elevations".
		 * 
		 * AWIPS I code uses the storage directory name as a discriminator.
		 * This means it goes through a tilt angle groups filter which I'm
		 * trying to avoid here...  There would be an issue if a TDWR had
		 * two elevations in the 0 - 4.6 range that belonged to the same tilt 
		 * bucket (not counting the 0.6 elevation.)
		 */
		
		/* Original AWIPS 1 code used product 185 or 186 to indicate
		 * the start of a new volume scan.  This looks for a new volume
		 * scan number and later scan time.  For TDWR, the scan number 
		 * changes on every completely new volume, but the volume scan 
		 * time changes for every mini-volume.
		 */
		DuplicateInfo inf = duplicateProducts.get(rc.getRadarID());
		if (inf == null) {
			inf = new DuplicateInfo();
			inf.volumeScanNumber = pdb.volumeScan;
			inf.volumeScanTime = pdb.volumeScanTime;
			duplicateProducts.put(rc.getRadarID(), inf);
		} else {
			if (pdb.volumeScan != inf.volumeScanNumber) {
				if (pdb.volumeScanTime.after(inf.volumeScanTime)) {
					inf.volumeScanNumber = pdb.volumeScan;
					inf.volumeScanTime = pdb.volumeScanTime;
					inf.sentProducts.clear();
				} else
					/* We can't tell if this a duplicate or not because we only
					 * keep track of one scan. Should it be sent? Could argue
					 * for not sending because all products from the previous
					 * scan should have already been sent. AWIPS 1 code does 
					 * not really handle this case. For now, allow it to be sent.
					 */
					return false;
			}
		}
		List<Integer> id = Arrays.asList(pdb.productCode, pdb.getElevationAngle());
		if (inf.sentProducts.contains(id))
			return true;
		
		inf.sentProducts.add(id);
		return false;
	}
}
