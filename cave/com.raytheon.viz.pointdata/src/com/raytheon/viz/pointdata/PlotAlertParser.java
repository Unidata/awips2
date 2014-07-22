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
package com.raytheon.viz.pointdata;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;

/**
 * AlertMessage parser for Plot types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2009            mschenke     Initial creation
 * Jul 23, 2014 3410       bclement     location changed to floats
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class PlotAlertParser extends AbstractAlertMessageParser {

    @Override
    public Object parseAlertMessage(AlertMessage message,
            AbstractRequestableResourceData resourceData) throws VizException {
        String stationId = (String) message.decodedAlert
                .get("location.stationId");
        Double lat = ((Number) message.decodedAlert.get("location.latitude"))
                .doubleValue();
        Double lon = ((Number) message.decodedAlert.get("location.longitude"))
                .doubleValue();
        DataTime dataTime = (DataTime) message.decodedAlert.get("dataTime");
        return new PlotInfo(stationId, lat, lon, dataTime,
                message.dataURI);
    }
}
