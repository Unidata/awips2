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
package com.raytheon.uf.common.dataaccess.response;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.geospatial.LatLonReprojection;
import com.raytheon.uf.common.geospatial.LatLonWrapper;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response for <code>GetGridDataRequest</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class GetGridDataResponse {

    @DynamicSerializeElement
    private List<GridResponseData> gridData;

    @DynamicSerializeElement
    private Map<String, Integer> siteNxValues;

    @DynamicSerializeElement
    private Map<String, Integer> siteNyValues;

    @DynamicSerializeElement
    private Map<String, float[]> siteLatGrids;

    @DynamicSerializeElement
    private Map<String, float[]> siteLonGrids;

    public GetGridDataResponse() {
        // no-op, for serialization only
    }

    public GetGridDataResponse(final Collection<IGridData> gridData) {
        this.gridData = new ArrayList<GridResponseData>(gridData.size());
        siteNxValues = new HashMap<String, Integer>(gridData.size(), 1);
        siteNyValues = new HashMap<String, Integer>(gridData.size(), 1);
        siteLatGrids = new HashMap<String, float[]>(gridData.size(), 1);
        siteLonGrids = new HashMap<String, float[]>(gridData.size(), 1);

        for (IGridData grid : gridData) {
            this.gridData.add(new GridResponseData(grid));

            String locationName = grid.getLocationName();
            if (!siteNxValues.containsKey(locationName)) {
                GridGeometry2D gridGeometry = grid.getGridGeometry();
                GridEnvelope gridShape = gridGeometry.getGridRange();
                siteNxValues.put(locationName, gridShape.getSpan(0));
                siteNyValues.put(locationName, gridShape.getSpan(1));
                LatLonWrapper latLonData = LatLonReprojection
                        .getLatLons(gridGeometry);
                siteLatGrids.put(locationName, latLonData.getLats());
                siteLonGrids.put(locationName, latLonData.getLons());
            }
        }
    }

    public List<GridResponseData> getGridData() {
        return gridData;
    }

    public void setGridData(List<GridResponseData> gridData) {
        this.gridData = gridData;
    }

    public Map<String, Integer> getSiteNxValues() {
        return siteNxValues;
    }

    public void setSiteNxValues(Map<String, Integer> siteNxValues) {
        this.siteNxValues = siteNxValues;
    }

    public Map<String, Integer> getSiteNyValues() {
        return siteNyValues;
    }

    public void setSiteNyValues(Map<String, Integer> siteNyValues) {
        this.siteNyValues = siteNyValues;
    }

    public Map<String, float[]> getSiteLatGrids() {
        return siteLatGrids;
    }

    public void setSiteLatGrids(Map<String, float[]> siteLatGrids) {
        this.siteLatGrids = siteLatGrids;
    }

    public Map<String, float[]> getSiteLonGrids() {
        return siteLonGrids;
    }

    public void setSiteLonGrids(Map<String, float[]> siteLonGrids) {
        this.siteLonGrids = siteLonGrids;
    }
}
