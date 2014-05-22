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
package com.raytheon.uf.edex.plugin.bufrmos;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class MOSPointDataState {

    private Log logger = LogFactory.getLog(getClass());

    private Map<String, PointDataContainer> pointData;

    private static Map<String, PointDataDescription> descriptions = new HashMap<String, PointDataDescription>();

    public MOSPointDataState() {
        pointData = new HashMap<String, PointDataContainer>();
    }

    public PointDataContainer getContainer(String type) {
        PointDataContainer container = pointData.get(type);
        if (container == null) {
            PointDataDescription pdd = null;
            try {
                pdd = getDescription(type);
                container = PointDataContainer.build(pdd);
                if (container != null) {
                    pointData.put(type, container);
                }
            } catch (SerializationException e) {
                logger.error("Could not create PointDataContainer for " + type
                        + " model soundings", e);
            }
        }

        return container;
    }

    public static synchronized PointDataDescription getDescription(String type)
            throws SerializationException {
        PointDataDescription pdd = descriptions.get(type);
        if (pdd == null) {
            String strmPath = "/res/pointdata/bufrmos" + type + ".xml";
            pdd = PointDataDescription.fromStream(MOSPointDataState.class
                    .getResourceAsStream(strmPath));
            if (pdd != null) {
                descriptions.put(type, pdd);
            }
        }

        return pdd;
    }

}
