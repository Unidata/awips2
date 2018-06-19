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
package com.raytheon.uf.viz.volumebrowser.dataplugin.point;

import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightRenderableDisplay;
import com.raytheon.uf.viz.xy.timeheight.rsc.TimeHeightResourceData;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.loader.ProductCreator;
import com.raytheon.viz.volumebrowser.loader.TimeHeightProductCreator;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * {@link ProductCreator} for loading point data on a
 * {@link TimeHeightRenderableDisplay}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointTimeHeightProductCreator extends TimeHeightProductCreator {

    @Override
    protected TimeHeightResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry, DisplayType displayType) {
        TimeHeightResourceData resourceData = super.createNewResourceData(
                dataCatalog, catalogEntry, displayType);
        if (!(dataCatalog instanceof PointDataCatalog)) {
            return resourceData;
        }
        PointDataCatalog pointCatalog = (PointDataCatalog) dataCatalog;
        String sourceKey = catalogEntry.getSelectedData().getSourcesKey();
        Coordinate coordinate = PointLineUtil.getPointCoordinate(catalogEntry);
        SurfaceObsLocation closestLoc = pointCatalog.getClosestStation(
                coordinate, sourceKey);
        Coordinate closestCoord = new Coordinate(closestLoc.getLongitude(),
                closestLoc.getLatitude());
        resourceData.setPointCoordinate(closestCoord);

        String pointLetter = PointLineUtil.getPointLetter(catalogEntry);
        PointsDataManager.getInstance()
                .setCoordinate(pointLetter, closestCoord);
        return resourceData;
    }

}
