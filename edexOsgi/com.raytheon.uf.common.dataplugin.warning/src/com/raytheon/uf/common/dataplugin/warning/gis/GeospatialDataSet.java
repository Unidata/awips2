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
package com.raytheon.uf.common.dataplugin.warning.gis;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class GeospatialDataSet {
    @DynamicSerializeElement
    private GeospatialData[] areas;

    @DynamicSerializeElement
    private GeospatialData[] parentAreas;
    
    @DynamicSerializeElement
    private GeospatialData[] timezones;

    public GeospatialData[] getAreas() {
        return areas;
    }

    public void setAreas(GeospatialData[] areas) {
        this.areas = areas;
    }

    public GeospatialData[] getParentAreas() {
        return parentAreas;
    }

    public void setParentAreas(GeospatialData[] parentAreas) {
        this.parentAreas = parentAreas;
    }

    public GeospatialData[] getTimezones() {
        return timezones;
    }

    public void setTimezones(GeospatialData[] timezones) {
        this.timezones = timezones;
    }
    
}
