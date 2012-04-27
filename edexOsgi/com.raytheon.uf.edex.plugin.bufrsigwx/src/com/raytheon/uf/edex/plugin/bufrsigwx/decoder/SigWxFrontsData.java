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
package com.raytheon.uf.edex.plugin.bufrsigwx.decoder;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SigWxFrontsData extends SigWxDataAdapter {
	private Log logger = LogFactory.getLog(getClass());

	SigWxType wxType;

	/**
	 * 
	 * @param container
	 */
	public SigWxFrontsData(PointDataDescription pdd,
			PointDataPluginDao<SigWxData> dao, String pluginName) {
		super(pdd, dao, pluginName);
	}

	/**
	 * 
	 * @param pointData
	 * @param locPoint
	 * @param dataPoint
	 * @param index
	 */
	List<SigWxData> getSigWxData(SigWxData sigWx, List<IBUFRDataPacket> dataList) {
		List<SigWxData> sList = new ArrayList<SigWxData>();

		if (sigWx != null) {
			IBUFRDataPacket p1 = dataList.get(15);
			List<IBUFRDataPacket> tropList = getPacketSubList(p1);
			if (tropList != null) {
				int key = 0;
				for (IBUFRDataPacket pp : tropList) {
					SigWxData front = getReport(pp, sigWx);
					if (front != null) {
						front.setKey(key++);
						sList.add(front);
					}
				}
			}
		}

		return sList;
	}

	/**
	 * 
	 * @param packet
	 * @param sigWx
	 * @return
	 */
	private SigWxData getReport(IBUFRDataPacket packet, SigWxData sigWx) {
		SigWxData currWx = null;
		if (packet != null) {
			// get a copy
			List<IBUFRDataPacket> sList = getPacketSubList(packet);

			currWx = sigWx.copyObs();

			PointDataContainer container = getContainer(currWx, 1);
			if (container != null) {
				PointDataView view = container.append();

				long vt = currWx.getDataTime().getValidTime().getTimeInMillis();
				view.setLong("validTime", vt);

				view.setFloat("baseHgt", currWx.getBaseHeight().floatValue());
				view.setFloat("topHgt", currWx.getTopHeight().floatValue());

				setViewData("featureType", view, sList.get(0));

				// pickup the frontal feature info

				int index = 0;
				try {

					List<IBUFRDataPacket> dList = getPacketSubList(sList.get(2));
					for (IBUFRDataPacket p : dList) {
						List<IBUFRDataPacket> jetData = getPacketSubList(p);

						setViewData("latitude", view, jetData.get(0), index);
						setViewData("longitude", view, jetData.get(1), index);
						setViewData("featureDir", view, jetData.get(2), index);
						setViewData("featureSpd", view, jetData.get(3), index);
						index++;
					}
				} catch (IllegalArgumentException e) {
					logger.error("Frontal data truncated at " + index
							+ " features");
				}
				view.setInt("numOfPoints", index);
				currWx.setPointDataView(view);
			}
		}
		return currWx;
	}

	/**
	 * @see com.raytheon.uf.edex.plugin.bufrsigwx.decoder.SigWxDataAdapter#getType()
	 */
	@Override
	SigWxType getType() {
		return wxType;
	}

	/**
	 * @see com.raytheon.uf.edex.plugin.bufrsigwx.decoder.SigWxDataAdapter#getType()
	 */
	@Override
	void setType(SigWxType type) {
		wxType = type;
	}

}
