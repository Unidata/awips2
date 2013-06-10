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
import java.util.List;

import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response for <code>GetGeometryDataRequest</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class GetGeometryDataResponse {

    @DynamicSerializeElement
    private List<GeometryResponseData> geoData;

    public GetGeometryDataResponse() {
        // no-op, for serialization only
    }

    public GetGeometryDataResponse(final Collection<IGeometryData> geoData) {
        this.geoData = new ArrayList<GeometryResponseData>(geoData.size());
        for (IGeometryData element : geoData) {
            this.geoData.add(new GeometryResponseData(element));
        }
    }

    public List<GeometryResponseData> getGeoData() {
        return geoData;
    }

    public void setGeoData(List<GeometryResponseData> geoData) {
        this.geoData = geoData;
    }
}
