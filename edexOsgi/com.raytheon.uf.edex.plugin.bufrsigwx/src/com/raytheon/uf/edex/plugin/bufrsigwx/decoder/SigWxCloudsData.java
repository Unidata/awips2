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
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxLayer;
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

public class SigWxCloudsData extends SigWxDataAdapter {
	private Log logger = LogFactory.getLog(getClass());

	SigWxType wxType;

	/**
	 * 
	 * @param container
	 */
	public SigWxCloudsData(PointDataDescription pdd,
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
			if (SigWxLayer.SWM.equals(sigWx.getWxLayer())) {
				sList = getSWMReport(dataList, sigWx);
			} else {
				IBUFRDataPacket p1 = dataList.get(15);
				List<IBUFRDataPacket> tropList = getPacketSubList(p1);
				if (tropList != null) {
					int key = 0;
					for (IBUFRDataPacket pp : tropList) {
						SigWxData clouds = getSWHReport(pp, sigWx);
						if (clouds != null) {
							clouds.setKey(key++);
							sList.add(clouds);
						}
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
	private SigWxData getSWHReport(IBUFRDataPacket packet, SigWxData sigWx) {
		SigWxData currWx = null;
		if (packet != null) {
			// get a copy
			List<IBUFRDataPacket> cloudData = getPacketSubList(packet);

			currWx = sigWx.copyObs();

			PointDataContainer container = getContainer(currWx, 1);
			if (container != null) {
				PointDataView view = container.append();

				long vt = currWx.getDataTime().getValidTime().getTimeInMillis();
				view.setLong("validTime", vt);

				view.setFloat("baseHgt", currWx.getBaseHeight().floatValue());
				view.setFloat("topHgt", currWx.getTopHeight().floatValue());

				setViewData("cloudBase", view, cloudData.get(2));
				setViewData("cloudTop", view, cloudData.get(3));

				// pickup the Cloud boundary

				int numOfPoints = 0;
				try {
					List<IBUFRDataPacket> dList = getPacketSubList(cloudData
							.get(4));
					for (IBUFRDataPacket p : dList) {
						List<IBUFRDataPacket> jetData = getPacketSubList(p);

						setViewData("latitude", view, jetData.get(0),
								numOfPoints);
						setViewData("longitude", view, jetData.get(1),
								numOfPoints);
						numOfPoints++;
					}
				} catch (IllegalArgumentException e) {
					logger.error("Cloud outline truncated at " + numOfPoints
							+ " points");
				}
				view.setInt("numOfPoints", numOfPoints);

				setViewData("cloudDistribution", view, cloudData.get(5));
				setViewData("cloudType", view, cloudData.get(6));

				currWx.setPointDataView(view);
			}
		}
		return currWx;
	}

	/**
	 * 
	 * @param packet
	 * @param sigWx
	 * @return
	 */
	private List<SigWxData> getSWMReport(List<IBUFRDataPacket> sList,
			SigWxData sigWx) {

		List<SigWxData> wxList = new ArrayList<SigWxData>();

		SigWxData currWx = null;
		if (sList != null) {
			// get a copy

			int key = 0;
			int base = 16;
			while (base < sList.size()) {

				currWx = sigWx.copyObs();

				PointDataContainer container = getContainer(currWx, 1);
				if (container != null) {
					PointDataView view = container.append();

					long vt = currWx.getDataTime().getValidTime()
							.getTimeInMillis();
					view.setLong("validTime", vt);

					setViewData("featureType", view, sList.get(15));

					setViewData("dimensionSig", view, sList.get(base));
					List<IBUFRDataPacket> dList = getPacketSubList(sList
							.get(base + 1));

					int index = 0;
					try {
						for (IBUFRDataPacket p : dList) {
							List<IBUFRDataPacket> featureOutline = getPacketSubList(p);

							setViewData("latitude", view,
									featureOutline.get(0), index);
							setViewData("longitude", view,
									featureOutline.get(1), index);
							index++;
						}
					} catch (IllegalArgumentException e) {
						logger.error("Frontal data truncated at " + index
								+ " features");
					}
					view.setInt("numOfVertices", index);

					dList = getPacketSubList(sList.get(base + 4));
					getTurbData(dList, view);

					dList = getPacketSubList(sList.get(base + 5));
					getIcingData(dList, view);

					dList = getPacketSubList(sList.get(base + 6));
					getCloudLevelData(dList, view);

					currWx.setPointDataView(view);
					base += 7;

					if (currWx != null) {
						currWx.setKey(key++);
						wxList.add(currWx);
					}
				} // while
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
	private void getTurbData(List<IBUFRDataPacket> dList, PointDataView view) {
		int index = 0;
		try {
			for (IBUFRDataPacket p : dList) {
				List<IBUFRDataPacket> featureOutline = getPacketSubList(p);
				setViewData("turbBase", view, featureOutline.get(0), index);
				setViewData("turbTop", view, featureOutline.get(1), index);
				setViewData("turbType", view, featureOutline.get(2), index);
				index++;
			}
		} catch (IllegalArgumentException e) {
			logger.error("Cloud.turb data truncated at " + index + " features");
		}
		view.setInt("numOfTurbLevels", index);
	}

	/**
	 * 
	 * @param packet
	 * @param sigWx
	 * @return
	 */
	private void getIcingData(List<IBUFRDataPacket> dList, PointDataView view) {
		int index = 0;
		try {
			for (IBUFRDataPacket p : dList) {
				List<IBUFRDataPacket> icingData = getPacketSubList(p);
				setViewData("icingBase", view, icingData.get(0), index);
				setViewData("icingTop", view, icingData.get(1), index);
				setViewData("icingType", view, icingData.get(2), index);
				index++;
			}
		} catch (IllegalArgumentException e) {
			logger.error("Cloud.icing data truncated at " + index + " features");
		}
		view.setInt("numOfIcingLevels", index);
	}

	/**
	 * 
	 * @param packet
	 * @param sigWx
	 * @return
	 */
	private void getCloudLevelData(List<IBUFRDataPacket> dList,
			PointDataView view) {
		int index = 0;
		try {
			for (IBUFRDataPacket p : dList) {
				List<IBUFRDataPacket> cloudData = getPacketSubList(p);
				setViewData("cloudBase", view, cloudData.get(0), index);
				setViewData("cloudTop", view, cloudData.get(1), index);
				setViewData("cloudDistribution", view, cloudData.get(2), index);
				setViewData("cloudType", view, cloudData.get(3), index);
				index++;
			}
		} catch (IllegalArgumentException e) {
			logger.error("Cloud.cloud data truncated at " + index + " features");
		}
		view.setInt("numOfCloudLevels", index);
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
