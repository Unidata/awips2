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
package com.raytheon.uf.edex.plugin.cwat.common;

import java.util.HashMap;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class LocationBinList {
    @DynamicSerializeElement
    private Coordinate siteCoord;

    @DynamicSerializeElement
    private HashMap<String, LocationBin> locationBins;

    private transient long lastModifiyTime;

    public Coordinate getSiteCoord() {
        return siteCoord;
    }

    public void setSiteCoord(Coordinate siteCoord) {
        this.siteCoord = siteCoord;
    }

    public HashMap<String, LocationBin> getLocationBins() {
        return locationBins;
    }

    public void setLocationBins(HashMap<String, LocationBin> locationBins) {
        this.locationBins = locationBins;
    }

    public long getLastModifiyTime() {
        return lastModifiyTime;
    }

    public void setLastModifiyTime(long lastModifiyTime) {
        this.lastModifiyTime = lastModifiyTime;
    }
}
