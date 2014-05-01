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
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.TropopauseLayerData;
import com.raytheon.uf.common.pointdata.Dimension;
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

public class SigWxTropData extends SigWxDataAdapter {
	private Log logger = LogFactory.getLog(getClass());

	private static final int STATS_TROP_HEIGHT = 63;

	SigWxType wxType;

	/**
	 * 
	 * @param container
	 */
	public SigWxTropData(PointDataDescription pdd,
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
				for (IBUFRDataPacket pp : tropList) {
					SigWxData tropLayer = getReport(pp, sigWx);
					if (tropLayer != null) {
						sList.add(tropLayer);
					}
				}
			}
		}

		int maxEntries = -1;
		Dimension[] dims = getPointDataDescription().dimensions;
		for (Dimension d : dims) {
			if ("maxEntries".equals(d.getDimensionName())) {
				maxEntries = d.getDimensionLength();
			}
		}

		for (SigWxData s : sList) {

			PointDataContainer container = getContainer(s, 1);
			if (container != null) {
				PointDataView view = container.append();

				long vt = s.getDataTime().getValidTime().getTimeInMillis();
				view.setLong("validTime", vt);

				view.setFloat("baseHgt", s.getBaseHeight().floatValue());
				view.setFloat("topHgt", s.getTopHeight().floatValue());

				TropopauseLayerData trop = s.getTropData();
				view.setInt("tropType", trop.getTropType());
				s.setKey(trop.getTropType());
				List<Double> la = trop.getLatitude();
				List<Double> lo = trop.getLongitude();
				List<Double> ht = trop.getHeight();

				int size = Math.min(la.size(), maxEntries);
				if (size < la.size()) {
					logger.info("Tropopause data truncated at " + size
							+ " needed " + la.size());
				}
				int numOfPoints = 0;
				try {
					for (int i = 0; i < size; i++) {
						view.setFloat("latitude", la.get(i).floatValue(), i);
						view.setFloat("longitude", lo.get(i).floatValue(), i);
						view.setFloat("height", ht.get(i).floatValue(), i);
						numOfPoints++;
					}
				} catch (IllegalArgumentException e) {

				}
				view.setInt("numOfPoints", numOfPoints);
				s.setPointDataView(view);
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

			int vertSoundingSig = getInt(sList.get(0), MISSING);
			int dimensionSig = getInt(sList.get(1), MISSING);
			if ((vertSoundingSig == 16) && (dimensionSig == 0)) {
				int statistics = getInt(sList.get(2), MISSING);
				if (statistics == MISSING) {
					statistics = STATS_TROP_HEIGHT;
				}
				currWx = sigWx.copyObs();
				// pickup all of the tropopause level data
				TropopauseLayerData trop = new TropopauseLayerData(statistics);

				List<IBUFRDataPacket> dList = getPacketSubList(sList.get(3));
				for (IBUFRDataPacket p : dList) {
					List<IBUFRDataPacket> location = getPacketSubList(p);

					double lat = getDouble(location.get(0), MISSING);
					double lon = getDouble(location.get(1), MISSING);
					double hgt = getDouble(location.get(2), MISSING);

					trop.addLevel(hgt, lat, lon);
				}
				currWx.setTropData(trop);
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
