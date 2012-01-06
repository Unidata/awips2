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
package com.raytheon.viz.grid.inv;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * 
 * A class for parsing alerts which retrieves the data using the data cube,
 * which makes it work well for anything which may have derived parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GribDataCubeAlertMessageParser extends DataCubeAlertMessageParser {

    public GribDataCubeAlertMessageParser() {

    }

    @Override
    public Object parseAlertMessage(AlertMessage message,
            AbstractRequestableResourceData reqResourceData)
            throws VizException {
        Object objectToSend = null;
        Map<String, Object> attribs = new HashMap<String, Object>(
                message.decodedAlert);
        String dataURI = message.dataURI;
        if (reqResourceData.isUpdatingOnMetadataOnly()) {
            PluginDataObject record = RecordFactory.getInstance()
                    .loadRecordFromUri(dataURI);
            objectToSend = record;
        } else {
            attribs.put("dataURI", message.dataURI);
            Map<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();
            for (String column : attribs.keySet()) {
                if (column.equals("dataURI")) {
                    continue;
                }
                if ((attribs.get(column) == null)
                        || attribs.get(column).toString().equals("null")) {
                    vals.put(column, new RequestConstraint(null,
                            RequestConstraint.ConstraintType.ISNULL));
                } else {
                    vals.put(column, new RequestConstraint(attribs.get(column)
                            .toString()));
                }
            }

            // remove cache'd entry from grib time cache
            GribMapKey mapKey = new GribMapKey(attribs);
            GribTimeCache.getInstance().clearTimes(mapKey);

            // Make sure there is really data before sending it to be loaded
            DataTime[] availableTimes = DataCubeContainer.performTimeQuery(
                    vals, false);
            if (availableTimes == null) {
                return objectToSend;
            }
            for (DataTime time : availableTimes) {
                if (time.equals(attribs.get("dataTime"))) {
                    LayerProperty lp = new LayerProperty();
                    lp.setDesiredProduct(ResourceType.PLAN_VIEW);
                    lp.setEntryQueryParameters(vals, false);
                    lp.setSelectedEntryTimes(new DataTime[] { time });
                    List<Object> resp = DataCubeContainer.getData(lp, 60000);
                    if (resp.size() == 0)
                        return null;
                    objectToSend = resp.get(0);
                    break;
                }
            }
        }
        return objectToSend;
    }

}
