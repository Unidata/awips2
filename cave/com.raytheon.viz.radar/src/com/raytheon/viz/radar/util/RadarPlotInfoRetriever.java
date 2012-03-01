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
package com.raytheon.viz.radar.util;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.retrieve.AbstractDbPlotInfoRetriever;

/**
 * 
 * A Plot info retriever for radar data.
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
public class RadarPlotInfoRetriever extends AbstractDbPlotInfoRetriever {

    @Override
    protected void addColumns(DbQuery dq) {
        dq.addColumn("location.lat");
        dq.addColumn("location.lon");
        dq.addColumn("icao");
        dq.addColumn("dataTime");
        dq.addColumn("dataURI");
    }

    @Override
    protected PlotInfo getPlotInfo(Object[] data) {
        PlotInfo stationInfo = new PlotInfo();
        stationInfo.latitude = ((Float) data[0]).doubleValue();
        stationInfo.longitude = ((Float) data[1]).doubleValue();
        stationInfo.stationId = (String) data[2];
        if (stationInfo.stationId == null) {
            stationInfo.stationId = "" + data[0] + "#" + data[1];
        }
        stationInfo.dataTime = (DataTime) data[3];
        stationInfo.dataURI = (String) data[4];

        return stationInfo;
    }

}
