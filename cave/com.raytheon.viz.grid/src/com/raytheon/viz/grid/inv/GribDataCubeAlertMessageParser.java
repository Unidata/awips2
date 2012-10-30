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
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;

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
        Map<String, Object> attribs = new HashMap<String, Object>(
                message.decodedAlert);

        // remove cache'd entry from grib time cache
        GridMapKey mapKey = new GridMapKey(attribs);
        GridTimeCache.getInstance().clearTimes(mapKey);

        return super.parseAlertMessage(message, reqResourceData);
    }

}
