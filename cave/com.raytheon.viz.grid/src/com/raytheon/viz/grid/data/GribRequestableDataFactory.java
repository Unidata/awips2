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
package com.raytheon.viz.grid.data;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GribRequestableDataFactory implements IAlertObserver {
    private static GribRequestableDataFactory instance = new GribRequestableDataFactory();

    public static GribRequestableDataFactory getInstance() {
        return instance;
    }

    private Map<String, GribRequestableData> requestableDataMap = new ConcurrentHashMap<String, GribRequestableData>();

    public GribRequestableDataFactory() {
        ProductAlertObserver.addObserver("grib", this);
    }

    public GribRequestableData getGribRequestableData(GribRecord record) {
        String uri = record.getDataURI();
        GribRequestableData rval = requestableDataMap.get(uri);

        if (rval == null) {
            synchronized (this) {
                // double check value still null
                rval = requestableDataMap.get(uri);
                if (rval == null) {
                    rval = new GribRequestableData(record);
                    requestableDataMap.put(uri, rval);
                }
            }
        }

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.IAlertObserver#alertArrived(java.util.Collection)
     */
    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        for (AlertMessage mess : alertMessages) {
        	DataTime dataTime = (DataTime) mess.decodedAlert.get("dataTime");
			if (dataTime.getRefTime().before(
					SimulatedTime.getSystemTime().getTime())) {
				requestableDataMap.remove(mess.dataURI);
			}
		}
	}
}
