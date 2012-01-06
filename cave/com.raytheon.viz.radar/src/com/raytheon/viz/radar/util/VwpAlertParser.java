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

import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.pointdata.PlotInfo;

/**
 * AlertMessage parser for Vwp Plots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 09, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VwpAlertParser extends AbstractAlertMessageParser {

    @Override
    public Object parseAlertMessage(AlertMessage message,
            AbstractRequestableResourceData resourceData) throws VizException {
        String dataURI = message.dataURI;
        if (dataURI == null) {
            return null;
        }
        HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>();
        metadataMap.put("pluginName", new RequestConstraint("radar"));
        metadataMap.put("dataURI", new RequestConstraint(dataURI));
        // It just does a db query, which is just as fast as anything I could
        // do.
        List<PlotInfo> stations = (new RadarPlotInfoRetriever())
                .getStations(metadataMap);
        if (stations.size() < 1) {
            return null;
        }
        return stations.get(0);

    }
}
