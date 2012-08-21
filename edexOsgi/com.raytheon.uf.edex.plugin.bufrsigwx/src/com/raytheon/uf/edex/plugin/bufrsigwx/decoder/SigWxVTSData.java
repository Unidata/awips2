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

public class SigWxVTSData extends SigWxDataAdapter {
	private Log logger = LogFactory.getLog(getClass());

	private static final int POINT = 0;

	SigWxType wxType;

	private int key;

	/**
	 * 
	 * @param container
	 */
	public SigWxVTSData(PointDataDescription pdd,
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

		key = 0;
		sList.addAll(getStormReports(dataList.get(15), sigWx));

		sList.addAll(getVolcanoReports(dataList.get(16), sigWx));

		sList.addAll(getIncidentReports(dataList.get(17), sigWx));

		return sList;
	}

	/**
	 * 
	 * @param packet
	 * @param sigWx
	 * @return
	 */
	private List<SigWxData> getStormReports(IBUFRDataPacket packet,
			SigWxData sigWx) {

		List<SigWxData> wxList = new ArrayList<SigWxData>();

		if (packet != null) {
			// get a copy
			List<IBUFRDataPacket> stormData = getPacketSubList(packet);
			for (IBUFRDataPacket s : stormData) {
				List<IBUFRDataPacket> ss = getPacketSubList(s);

				SigWxData currWx = sigWx.copyObs();

				PointDataContainer container = getContainer(currWx, 1);
				if (container != null) {
					PointDataView view = container.append();

					long vt = currWx.getDataTime().getValidTime()
							.getTimeInMillis();
					view.setLong("validTime", vt);

					setViewData("attribSig", view, ss.get(0));
					setViewData("featureName", view, ss.get(2));
					setViewData("latitude", view, ss.get(3));
					setViewData("longitude", view, ss.get(4));
					setViewData("synopticFeature", view, ss.get(5));

					currWx.setKey(key++);
					currWx.setPointDataView(view);
					wxList.add(currWx);
				}
			}
		}

		return wxList;
	}

	/**
	 * 
	 * @param packet
	 * @param sigWx
	 * @return
	 */
	private List<SigWxData> getVolcanoReports(IBUFRDataPacket packet,
			SigWxData sigWx) {

		List<SigWxData> wxList = new ArrayList<SigWxData>();

		if (packet != null) {
			List<IBUFRDataPacket> volcanoData = getPacketSubList(packet);
			for (IBUFRDataPacket v : volcanoData) {
				List<IBUFRDataPacket> vv = getPacketSubList(v);

				SigWxData currWx = sigWx.copyObs();

				PointDataContainer container = getContainer(currWx, 1);
				if (container != null) {
					PointDataView view = container.append();

					long vt = currWx.getDataTime().getValidTime()
							.getTimeInMillis();
					view.setLong("validTime", vt);

					setViewData("metFeature", view, vv.get(0));
					setViewData("featureName", view, vv.get(1));
					int dim = getInt(vv.get(2), MISSING);
					if (dim == POINT) {
						List<IBUFRDataPacket> dList = getPacketSubList(vv
								.get(3));
						List<IBUFRDataPacket> locData = getPacketSubList(dList
								.get(0));
						setViewData("latitude", view, locData.get(0));
						setViewData("longitude", view, locData.get(1));

						setViewData("specialClouds", view, vv.get(10));

						currWx.setKey(key++);
						currWx.setPointDataView(view);
						wxList.add(currWx);
					} else {

					}
				}
			}
		}
		return wxList;
	}

	/**
	 * No implementation for now.
	 * 
	 * @param packet
	 * @param sigWx
	 * @return
	 */
	private List<SigWxData> getIncidentReports(IBUFRDataPacket packet,
			SigWxData sigWx) {

		List<SigWxData> wxList = new ArrayList<SigWxData>();

		if (packet != null) {
			// get a copy
			List<IBUFRDataPacket> incidentData = getPacketSubList(packet);
			if ((incidentData != null) && (incidentData.size() == 13)) {
				// SigWxData currWx = sigWx.copyObs();
				//
				// PointDataContainer container = getContainer(currWx, 1);
				// if (container != null) {
				// PointDataView view = container.append();
				//
				// long vt = currWx.getDataTime().getValidTime()
				// .getTimeInMillis();
				// view.setLong("validTime", vt);
				// }
			}
		}
		return wxList;
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
