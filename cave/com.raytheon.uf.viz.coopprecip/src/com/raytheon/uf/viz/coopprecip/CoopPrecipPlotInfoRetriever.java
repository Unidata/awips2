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
package com.raytheon.uf.viz.coopprecip;

import java.sql.Time;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.pointdata.PlotData;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.retrieve.AbstractPlotInfoRetriever;

/**
 * 
 * A plotInfoRetriever for all pointData types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CoopPrecipPlotInfoRetriever extends AbstractPlotInfoRetriever {

    @Override
    public void getStations(IResourceDataChanged listener, DataTime time,
            HashMap<String, RequestConstraint> metadataMap) throws VizException {
        PointDataContainer pdc = DataCubeContainer.getPointData(metadataMap
                .get("pluginName").getConstraintValue(), new String[] {
                "latitude", "longitude", "time" }, metadataMap);
        List<PlotInfo> info = new ArrayList<PlotInfo>();
        if (pdc != null) {
            for (int uriCounter = 0; uriCounter < pdc.getCurrentSz(); uriCounter++) {
                PointDataView pdv = pdc.readRandom(uriCounter);
                PlotInfo stationInfo = new PlotInfo();
                stationInfo.latitude = pdv.getNumber("latitude").doubleValue();
                stationInfo.longitude = pdv.getNumber("longitude")
                        .doubleValue();
                stationInfo.dataTime = new DataTime(new Time(
                        pdv.getLong("time")));
                stationInfo.stationId = pdv.getString("stationId");
                if (stationInfo.pdv == null) {
                    stationInfo.pdv = new PlotData();
                }
                stationInfo.pdv.addData(pdv);
                stationInfo.dataURI = pdv.getString("dataURI");
                info.add(stationInfo);
            }
        }
        listener.resourceChanged(ChangeType.DATA_UPDATE,
                info.toArray(new PlotInfo[0]));

    }

}
