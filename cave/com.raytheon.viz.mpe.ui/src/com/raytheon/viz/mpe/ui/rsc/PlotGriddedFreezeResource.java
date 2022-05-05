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
package com.raytheon.viz.mpe.ui.rsc;

import java.awt.Point;
import java.awt.Rectangle;
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.UnitConverter;
import si.uom.NonSI;
import systems.uom.common.USCustomary;

import javax.measure.Unit;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;
import com.raytheon.viz.mpe.ui.actions.DrawDQCStations;
import com.raytheon.viz.mpe.util.CreateMap;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;

/**
 * The MPE Gridded Freeze Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 09, 2009  2589      snaples     Initial creation
 * Feb 28, 2017  6157      bkowal      No longer alter the data when legend filtering
 *                                     is enabled.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class PlotGriddedFreezeResource
        extends AbstractVizResource<AbstractResourceData, MapDescriptor>
        implements IMpeResource {

    MPEDisplayManager displayMgr = null;

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private DrawDQCStations ddq = DrawDQCStations.getInstance();

    private GriddedImageDisplay gridDisplay;

    private GriddedContourDisplay contourDisplay;

    private GridGeometry2D gridGeometry;

    private float brightness = 1.0f;

    private float contrast = 1.0f;

    private boolean isInterpolated;

    private HRAPSubGrid subGrid;

    int first = 1;

    int hed;

    int time_pos;

    int display_flag;

    private ColorMapParameters parameters = new ColorMapParameters();

    private static final GeometryFactory gf = new GeometryFactory();

    private List<Colorvalue> colorSet;

    public PlotGriddedFreezeResource(MPEDisplayManager displayMgr,
            LoadProperties loadProperties, List<Colorvalue> colorSet) {
        super(new GenericResourceData(), loadProperties);
        this.displayMgr = displayMgr;
        this.colorSet = colorSet;
    }

    ColorMap freeze_colormap = ddq.colorMap;

    RGB color = null;

    IGraphicsTarget target;

    FloatBuffer buf;

    public void plot_gridded_freeze(String prefix, int num) {
        boolean wfo_all = dqc.wfo_all;
        int[] wfo_in_use = dqc.wfo_in_use;
        CreateMap cm = new CreateMap();
        float value = 0;

        int i, j, m = 0;
        String file = prefix;

        /* Retrieve the temperature colormap. */
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName("6hGRID_FREEZL");

        DataMappingPreferences dmPref = new DataMappingPreferences();
        i = 0;
        for (Colorvalue cv : colorSet) {
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            colorMap.setColor(i, new Color(rgb.red / 255f, rgb.green / 255f,
                    rgb.blue / 255f));

            DataMappingEntry entry = new DataMappingEntry();
            entry.setPixelValue((double) i);
            entry.setDisplayValue(cv.getId().getThresholdValue());
            dmPref.addEntry(entry);

            i++;
        }
        DataMappingEntry entry = new DataMappingEntry();
        entry.setPixelValue((double) (i - 1));
        entry.setDisplayValue(Double.MAX_VALUE);
        dmPref.addEntry(entry);

        dmPref.getEntries().get(0).setLabel("");

        ColorMapCapability cmc = getCapability(ColorMapCapability.class);
        parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = USCustomary.FOOT;
        Unit<?> dataUnit = USCustomary.FOOT.divide(100.0);
        parameters.setDataUnit(dataUnit);
        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setFormatString("0");

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cmc.setColorMapParameters(parameters);

        UnitConverter cvt = parameters.getDataToImageConverter();

        if (dqc.pcp_in_use[num] == -1) {
            return;
        }

        cm.read_file(file, num, dqc.pcp);

        buf = FloatBuffer
                .allocate(dqc.getHrap_grid().maxi * dqc.getHrap_grid().maxj);

        /* Get value in the HRAP grid bins. */
        for (j = dqc.getHrap_grid().maxj - 1; j >= 0; j--) {
            for (i = 0; i < dqc.getHrap_grid().maxi; ++i) {

                if (dqc.getHrap_grid().owner[i][j] == -1) {
                    continue;
                }

                if (wfo_all != true) {

                    for (m = 0; m < 20; m++) {
                        if (wfo_in_use[m] == -1) {
                            break;
                        }

                        if (dqc.getHrap_grid().owner[i][j] == wfo_in_use[m]) {
                            break;
                        }
                    }
                }

                Float fg = 0f;
                value = dqc.pcp.value[i][j];
                if (value <= 0.01 && value >= -0.01) {
                    continue;
                }
                fg = (float) cvt.convert(value);

                if (fg.isNaN()) {
                    fg = -99.0f;
                }
                float f = (float) Math.floor(fg);
                buf.put(f);
            }
        }
        buf.rewind();

        Rectangle extent = new Rectangle(dqc.getHrap_grid().hrap_minx,
                dqc.getHrap_grid().hrap_miny, dqc.getHrap_grid().maxi,
                dqc.getHrap_grid().maxj);

        if (extent.x == 0 && extent.y == 0) {
            Rectangle coord = null;
            try {
                coord = HRAPCoordinates.getHRAPCoordinates();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            if (extent.width == coord.width && extent.height == coord.height) {
                extent = coord;
            } else {
                extent = null;
                return;
            }
        }

        try {
            subGrid = new HRAPSubGrid(extent);

            gridGeometry = MapUtil.getGridGeometry(subGrid);

            project(gridGeometry.getCoordinateReferenceSystem());

        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> Values = interrogate(coord);

        if (Values == null) {
            return "NO DATA";
        } else {
            return Values.get("Value").toString();
        }
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        if (buf == null) {
            return null;
        }

        Map<String, Object> values = new HashMap<String, Object>();

        try {
            Coordinate gridCell = coord.asGridCell(
                    HRAP.getInstance().getGridGeometry(),
                    PixelInCell.CELL_CORNER);
            Coordinate l = coord.asLatLon();

            Point p = new Point((int) gridCell.x, (int) gridCell.y);
            values.put("X", Integer.toString(p.x));
            values.put("Y", Integer.toString(p.y));
            values.put("Lon", String.format("%5.2f", l.x));
            values.put("Lat", String.format("%4.2f", l.y));
            values.put("Value", "----");
            values.put("County", "Not Defined");
            values.put("Basin", "Not Defined");

            Rectangle extent = subGrid.getExtent();
            if (extent.contains(p)) {
                int x = p.x - extent.x;
                int y = p.y - extent.y;

                short s = (short) dqc.pcp.value[x][y];

                double d = parameters.getDataToDisplayConverter().convert(s);

                DecimalFormat df = new DecimalFormat(
                        parameters.getFormatString());
                float aa = (float) ((Math.floor((int) (d * 100))) / 100.0);
                values.put("Value", df.format(aa));
            }

            ISpatialQuery query = SpatialQueryFactory.create();

            org.locationtech.jts.geom.Point point = gf
                    .createPoint(coord.asLatLon());

            SpatialQueryResult[] results = query.query("county",
                    new String[] { "countyname" }, point, null, false,
                    SearchMode.WITHIN);

            String county = null;
            if (results != null && results.length > 0) {
                county = (String) results[0].attributes.get("countyname");
            }

            if (!StringUtils.isBlank(county)) {
                values.put("County", county);
            }

            results = query.query("basins", new String[] { "name" }, point,
                    null, false, SearchMode.WITHIN);

            String basin = null;
            if (results != null && results.length > 0) {
                basin = (String) results[0].attributes.get("name");
            }

            if (!StringUtils.isBlank(basin)) {
                values.put("Basin", basin);
            }
        } catch (Exception e) {
            throw new VizException("Error performing interrogation", e);
        }

        return values;
    }

    /**
     * convert Color to RGB
     * 
     * @param color
     * @return
     */
    public static RGB convertC(Color color) {
        int blue = (int) (color.getBlue() * 255f);
        int green = (int) (color.getGreen() * 255f);
        int red = (int) (color.getRed() * 255f);

        return new RGB(red, green, blue);
    }

    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }

        if (contourDisplay != null) {
            contourDisplay.dispose();
            contourDisplay = null;
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.target = target;
        time_pos = ddq.time_pos;
        plot_gridded_freeze(ddq.prefix, time_pos);

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (buf == null || dqc.grids_flag != 1
                || displayMgr.isZflag() != true) {
            return;
        }

        Set<DisplayMode> mode = displayMgr.getDisplayMode();

        if (mode.contains(DisplayMode.Image)) {
            if (gridDisplay == null) {
                gridDisplay = new GriddedImageDisplay(buf, descriptor,
                        gridGeometry);

                gridDisplay.setColorMapParameters(
                        getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
            }

            GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                    paintProps, brightness, contrast, isInterpolated);

            gridDisplay.paint(target, giProps);
        }

        if (mode.contains(DisplayMode.Contour)) {
            if (contourDisplay == null) {
                contourDisplay = new GriddedContourDisplay(descriptor,
                        gridGeometry, buf);

                contourDisplay.setColor(ColorUtil.WHITE);
                contourDisplay.setLineStyle(LineStyle.SOLID);
                contourDisplay.setOutlineWidth(1);
            }
            contourDisplay.paint(target, paintProps);
        }

        first = 0;
    }

    @Override
    public String getName() {
        if (ddq.qcmode == "") {
            return "No Data Available";
        }

        return ddq.qcmode;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }

        if (contourDisplay != null) {
            contourDisplay.dispose();
            contourDisplay = null;
        }
    }
}
