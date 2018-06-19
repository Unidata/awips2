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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionRenderableDisplay;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.loader.CrossSectionProductCreator;
import com.raytheon.viz.volumebrowser.loader.ProductCreator;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * {@link ProductCreator} for loading point data on a
 * {@link CrossSectionRenderableDisplay}.
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
public class PointCrossSectionProductCreator extends CrossSectionProductCreator {

    @Override
    protected CrossSectionResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry, DisplayType displayType) {
        CrossSectionResourceData resourceData = super.createNewResourceData(
                dataCatalog, catalogEntry, displayType);
        if (!(dataCatalog instanceof PointDataCatalog)) {
            return resourceData;
        }
        PointDataCatalog pointCatalog = (PointDataCatalog) dataCatalog;
        String sourceKey = catalogEntry.getSelectedData().getSourcesKey();
        List<String> closest = new ArrayList<String>();
        String letter = catalogEntry.getSelectedData().getPlanesKey()
                .replace("Line", "");
        LineString line = ToolsDataManager.getInstance().getBaseline(letter);
        Coordinate[] newLine = new Coordinate[line.getNumPoints()];
        for (int i = 0; i < line.getNumPoints(); i++) {
            SurfaceObsLocation loc = pointCatalog.getClosestStation(
                    line.getCoordinateN(i),
                    sourceKey, closest);
            if (loc == null) {
                break;
            }
            closest.add(loc.getStationId());
            newLine[i] = new Coordinate(loc.getLongitude(), loc.getLatitude());
        }
        ToolsDataManager.getInstance().setBaseline(letter,
                line.getFactory().createLineString(newLine));
        resourceData.setStationIDs(closest);
        return resourceData;
    }


}
