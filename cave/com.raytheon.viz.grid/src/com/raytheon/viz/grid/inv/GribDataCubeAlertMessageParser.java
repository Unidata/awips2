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

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.derivparam.GridMapKey;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridTimeCache;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.grid.rsc.GridResourceData;

/**
 *
 * A class for parsing alerts which retrieves the data using the data cube,
 * which makes it work well for anything which may have derived parameters
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 16, 2009           bsteffen  Initial creation
 * Jul 28, 2021  8611     randerso  Set level in DataTimes when resource data is
 *                                  spatial.
 * Oct 29, 2022  8959     mapeters  Update how data time levels are set
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GribDataCubeAlertMessageParser extends DataCubeAlertMessageParser {

    public GribDataCubeAlertMessageParser() {

    }

    @Override
    public Object parseAlertMessage(AlertMessage message,
            AbstractRequestableResourceData reqResourceData)
            throws VizException {
        Map<String, Object> attribs = new HashMap<>(message.decodedAlert);

        // remove cache'd entry from grib time cache
        GridMapKey mapKey = new GridMapKey(attribs);
        GridTimeCache.getInstance().clearTimes(mapKey);

        if (reqResourceData instanceof GridResourceData) {
            GridResourceData gridResourceData = (GridResourceData) reqResourceData;
            if (gridResourceData.isSpatial()) {
                DataTime dataTime = (DataTime) attribs
                        .get(PluginDataObject.DATATIME_ID);
                dataTime.setLevel((Double) attribs.get(GridConstants.LEVEL_ONE),
                        (String) attribs.get(GridConstants.MASTER_LEVEL_NAME));
            }
        }

        return super.parseAlertMessage(message, reqResourceData);
    }

}
