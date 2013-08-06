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
package com.raytheon.uf.viz.sounding.providers;

import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract {@link IVerticalSoundingProvider} that uses {@link PluginDataObject}
 * s for sounding creation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractPDOVerticalSoundingProvider extends
        AbstractVerticalSoundingProvider<PluginDataObject[]> {

    @Override
    protected PluginDataObject[] queryForData(
            Map<String, RequestConstraint> constraints, DataTime time,
            Coordinate location) {
        try {
            LayerProperty props = new LayerProperty();
            props.setEntryTimes(new DataTime[] { time });
            props.setSelectedEntryTimes(new DataTime[] { time });
            props.setEntryQueryParameters(constraints, false);
            return DataCubeContainer.getData(props, Integer.MAX_VALUE).toArray(
                    new PluginDataObject[0]);
        } catch (VizException e) {
            throw new RuntimeException("Error querying for sounding records: "
                    + constraints, e);
        }
    }

}
