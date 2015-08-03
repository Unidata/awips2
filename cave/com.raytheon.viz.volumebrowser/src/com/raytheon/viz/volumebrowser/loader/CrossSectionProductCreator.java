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
package com.raytheon.viz.volumebrowser.loader;

import java.awt.Rectangle;
import java.util.Arrays;

import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionRenderableDisplay;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.CrossSectionUtil;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VbUtil;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * Creates a {@link CrossSectionRenderableDisplay} containing a
 * {@link CrossSectionResourceData}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial Creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class CrossSectionProductCreator extends AbstractProductCreator {

    @Override
    protected AbstractRenderableDisplay createNewRenderableDisplay(
            VolumeBrowserDialogSettings dialogSettings,
            SelectedData selectedData) {
        CrossSectionRenderableDisplay display = new CrossSectionRenderableDisplay();
        CrossSectionDescriptor descriptor = display.getDescriptor();
        descriptor.setRenderableDisplay(display);

        SpaceTimeMenu currentSpaceTime = dialogSettings.getSpaceTimeSelection();
        String planesKey = selectedData.getPlanesKey();
        if (currentSpaceTime == SpaceTimeMenu.TIME) {
            /*
             * The Selected Plane in Time setting will be either a Baseline or a
             * Lat/Lon Line
             */
            Rectangle coverageRectangle = VbUtil.getMapCoverageRectangle();
            LineString line = null;

            if (planesKey.startsWith("Lon")) {
                double x = Double.parseDouble(planesKey.substring(3));
                Coordinate start = new Coordinate(x,
                        coverageRectangle.getMaxY());
                Coordinate end = new Coordinate(x, coverageRectangle.getMinY());
                line = new GeometryFactory().createLineString(new Coordinate[] {
                        start, end });
            } else if (planesKey.startsWith("Lat")) {
                double y = Double.parseDouble(planesKey.substring(3));
                Coordinate start = new Coordinate(coverageRectangle.getMinX(),
                        y);
                Coordinate end = new Coordinate(coverageRectangle.getMaxX(), y);
                line = new GeometryFactory().createLineString(new Coordinate[] {
                        start, end });

            } else if (planesKey.startsWith("Line")) {
                ToolsDataManager dataManager = ToolsDataManager.getInstance();
                line = dataManager.getBaseline(planesKey.substring(4));
                /* default to Baseline A if all else fails */
                if (line == null) {
                    line = dataManager.getBaseline("A");
                }
            } else {
                throw new RuntimeException("Invalid line has been selected: "
                        + planesKey);
            }

            /* set the line at which the results are displayed */
            descriptor.setLines(Arrays.asList(line));
            descriptor.setLineID(selectedData.getPlanesText());
        } else if (currentSpaceTime == SpaceTimeMenu.SPACE) {
            System.out
                    .println("Loading a Cross Section Space Resource from the Volume Browser.");
            if (planesKey.equals("LATS")) {
                descriptor.setLines(CrossSectionUtil.getAllLats());
                descriptor.setLineID("AllLAT");
            } else if (planesKey.equals("LONS")) {
                descriptor.setLines(CrossSectionUtil.getAllLons());
                descriptor.setLineID("AllLON");
            }
        }
        descriptor.setHeightScale(dialogSettings.getHeightScaleSelection());
        return display;
    }

    @Override
    protected CrossSectionResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        SelectedData selectedData = catalogEntry.getSelectedData();
        CrossSectionResourceData resourceData = new CrossSectionResourceData();
        resourceData.setParameter(selectedData.getFieldsKey());
        resourceData.setParameterName(selectedData.getFieldsText());
        resourceData.setSource(selectedData.getSourcesText());
        return resourceData;
    }

    @Override
    protected LoadProperties createNewLoadProperties(DisplayType displayType) {
        return new GridLoadProperties(displayType);
    }

}
