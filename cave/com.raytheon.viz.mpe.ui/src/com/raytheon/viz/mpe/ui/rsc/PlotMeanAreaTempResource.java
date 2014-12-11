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

import java.util.List;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.DrawDQCStations;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class PlotMeanAreaTempResource extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> implements
        IMpeResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotMeanAreaTempResource.class);

    private DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    private DrawDQCStations ddq = DrawDQCStations.getInstance();
    
    MPEDisplayManager displayMgr = null;

    private ColorMapParameters parameters = new ColorMapParameters();

    private List<Colorvalue> colorSet;

    public PlotMeanAreaTempResource(MPEDisplayManager displayMgr,
            List<Colorvalue> colorSet) {
        super(new PlotMeanAreaTempResourceData(), new LoadProperties());
        this.displayMgr = displayMgr;
        this.colorSet = colorSet;

    }

    ColorMap colorMap;

    DataMappingPreferences dmPref = new DataMappingPreferences();

    RGB color;

    private IWireframeShape outlineShape;

    private IShadedShape shadedShape;

    int hed;

    int time_pos;

    int display_flag;

    int first = 1;

    GeometryFactory jtsGeometryFactory;

    Hrap_Grid hrap_grid = dqc.getHrap_grid();

    private void plot_mean_areal_temp(IGraphicsTarget target, int num) {

        double[][] dqc_temp_delim = ddq.dqc_temp_delim;
        int dqc_temp_numcol = 0;
//        Maps mean_areal_precip_global[] = DailyQcUtils.mean_areal_precip_global;
        int pcp_in_use[] = dqc.pcp_in_use;
        boolean wfo_all = dqc.wfo_all;
        int[] wfo_in_use = dqc.wfo_in_use;

        double mapvalue;
        int hh = 0;
        int k;
        int l;
        int minx, miny, ix, iy, maxx, maxy;
        int x, y, ip, ib;
        int xpos;
        int ypos;
        float uz, mz, lz, gz;
        int i;
        int tscale = ddq.tscale;
        Coordinate[] points = new Coordinate[1];
        Coordinate[] PolyPoints = new Coordinate[5];

        /* Retrieve the precipitation colormap. */
        colorMap = new ColorMap(colorSet.size());

        dqc_temp_numcol = colorMap.getSize();
        colorMap.setName("sixhMAREA_TEMP");

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

        Unit<?> displayUnit = Unit.ONE;
        Unit<?> dataUnit = Unit.ONE;

        displayUnit = NonSI.FAHRENHEIT;
        dataUnit = NonSI.FAHRENHEIT.divide(100);
        parameters.setFormatString("0");

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cmc.setColorMapParameters(parameters);

        /* check pcp_in_use flag before plot */
        if (pcp_in_use[num] == -1) {
            return;
        }
        IMapDescriptor descriptor = (IMapDescriptor) displayMgr
                .getRenderableDisplay().getDescriptor();
        jtsGeometryFactory = new GeometryFactory();
        outlineShape = target.createWireframeShape(false, descriptor, 0.0f);

        shadedShape = target.createShadedShape(false, descriptor, true);

        JTSCompiler jtsCompiler = new JTSCompiler(shadedShape, outlineShape,
                descriptor);

        /* Retrieve the HRAP grid. */
        minx = hrap_grid.hrap_minx;
        miny = hrap_grid.hrap_miny;
        maxx = hrap_grid.maxi;
        maxy = hrap_grid.maxj;

        for (ib = 0; !dqc.mean_areal_precip_global[ib].hb5.equals(""); ib++) {
            if (dqc.mean_areal_precip_global[ib].tmaps_done[num - 150] <= 0) {
                continue;
            }

            if (wfo_all != true) {

                for (@SuppressWarnings("unused")
                int eh : wfo_in_use) {
                    if (hh == 20) {
                        break;
                    }
                    if (wfo_in_use[hh] == -1) {
                        break;
                    }

                    if (dqc.mean_areal_precip_global[ib].owner == wfo_in_use[hh]) {
                        break;
                    }

                    hh++;
                }

                if (hh == 20) {
                    hh = 19;
                }

                if (wfo_in_use[hh] == -1) {
                    continue;
                }

            }
            points = new Coordinate[dqc.mean_areal_precip_global[ib].basin_points];

            for (l = 0; l < dqc.mean_areal_precip_global[ib].basin_points; l++) {
                Coordinate ll = new Coordinate();
                ll.x = dqc.mean_areal_precip_global[ib].basin[l].lon;
                ll.y = dqc.mean_areal_precip_global[ib].basin[l].lat;
                points[l] = ll;
            }

            lz = dqc.mean_areal_precip_global[ib].tlz[num - 150];
            mz = dqc.mean_areal_precip_global[ib].tmz[num - 150];
            uz = dqc.mean_areal_precip_global[ib].tuz[num - 150];
            gz = dqc.mean_areal_precip_global[ib].tgz[num - 150];

            /* If there are no subareas and this is raster mode. */
            if ((dqc.mean_areal_precip_global[ib].zones[1] != 1)
                    && (dqc.mean_areal_precip_global[ib].zones[2] != 1)
                    && (dqc.mean_areal_precip_global[ib].zones[3] != 1)) {
                mapvalue = lz;

                if (MPELegendResource.dVal != 0) {
                    if (mapvalue < MPELegendResource.dVal) {
                        continue;
                    }
                }
                /* If the value is smaller than 0, then do not draw this basin. */
                if (mapvalue < 0) {
                    continue;
                }

                color = setColor(mapvalue);

                /* Draw a filled basin. */
                LinearRing lr = jtsGeometryFactory.createLinearRing(points);
                Polygon pg = jtsGeometryFactory.createPolygon(lr, null);
                try {
                    jtsCompiler.handle(pg, color);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error reprojecting MAT outline", e);
                }
            }

            for (l = 0; l < dqc.mean_areal_precip_global[ib].hrap_points; l++) {

                /*
                 * Retrieve the HRAP Coordinates of the HRAP Cell being
                 * processed.
                 */
                x = dqc.mean_areal_precip_global[ib].hrap_data[l].x;
                y = dqc.mean_areal_precip_global[ib].hrap_data[l].y;

                /* search for highest zone number is hrap block */
                ip = 1;

                if (dqc.mean_areal_precip_global[ib].hrap_data[l].zone[1] >= 0) {
                    ip = 2;
                }

                if (dqc.mean_areal_precip_global[ib].hrap_data[l].zone[2] >= 0) {
                    ip = 3;
                }

                if (dqc.mean_areal_precip_global[ib].hrap_data[l].zone[3] >= 0) {
                    ip = 4;
                }

                if (ip == 4) {
                    mapvalue = gz;
                } else if (ip == 3) {
                    mapvalue = uz;
                } else if (ip == 2) {
                    mapvalue = mz;
                } else {
                    mapvalue = lz;
                }

                /*
                 * If the value is smaller than 0, then do not draw this HRAP
                 * grid bin.
                 */
                if (mapvalue < 0) {
                    continue;
                }

                /* Not sure if Linesegs is relative or absolute. */
                ix = x - minx;
                iy = y - miny;

                if (ix < 0 || iy < 0 || ix >= maxx - minx || iy >= maxy - miny) {
                    /* The HRAP grid cell is out of range. Don't draw it. */
                    continue;
                }

                for (k = 0; k < dqc_temp_numcol - 1; k++) {
                    if (MPELegendResource.dVal != 0) {
                        if (MPELegendResource.up == true) {
                            if (mapvalue >= MPELegendResource.dVal) {
                                mapvalue = (float) MPELegendResource.dVal;
                            }
                        } else {
                            if (mapvalue < MPELegendResource.dVal) {
                                continue;
                            }
                        }
                    }

                    if (mapvalue >= dqc_temp_delim[tscale][k]
                            && mapvalue < dqc_temp_delim[tscale][k + 1]) {
                        color = convertC(colorMap.getColors().get(k));
                        break;
                    }
                }
                if (mapvalue <= -999) {
                    continue;
                } else if (k == (dqc_temp_numcol - 1)) {
                    color = convertC(colorMap.getColors().get(
                            dqc_temp_numcol - 1));
                }

                /* Using the MPE Lat/Lon Grid, draw the point. */

                xpos = dqc.mean_areal_precip_global[ib].hrap_data[l].x;
                ypos = dqc.mean_areal_precip_global[ib].hrap_data[l].y;

                PolyPoints[0] = new Coordinate();
                PolyPoints[0].x = (short) xpos;
                PolyPoints[0].y = (short) ypos;

                ypos = ypos + 1;

                PolyPoints[1] = new Coordinate();
                PolyPoints[1].x = (short) xpos;
                PolyPoints[1].y = (short) ypos;

                xpos = xpos + 1;
                ypos = ypos + 1;

                PolyPoints[2] = new Coordinate();
                PolyPoints[2].x = (short) xpos;
                PolyPoints[2].y = (short) ypos;

                xpos = xpos + 1;
                ypos = ypos - 2;

                PolyPoints[3] = new Coordinate();
                PolyPoints[3].x = (short) xpos;
                PolyPoints[3].y = (short) ypos;

                PolyPoints[4] = new Coordinate();
                PolyPoints[4].x = PolyPoints[0].x;
                PolyPoints[4].y = PolyPoints[0].y;

                LinearRing lr = jtsGeometryFactory.createLinearRing(PolyPoints);
                Polygon pg = jtsGeometryFactory.createPolygon(lr, null);

                try {
                    jtsCompiler.handle(pg, color);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error reprojecting MAT basin outline", e);
                }
            }

        }
        shadedShape.compile();
        outlineShape.compile();
        issueRefresh();
    }

    @Override
    protected void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {

        float alpha = paintProps.getAlpha();
        boolean isShaded = true;

        if (dqc.map_flag != 1 || displayMgr.isMaxmin() != true) {
            return;
        }

        if (shadedShape != null && shadedShape.isDrawable() && isShaded) {
            aTarget.drawShadedShape(shadedShape, alpha);
        } else if (shadedShape == null && isShaded) {
            System.out.println("Shaded shape is null");
        }

        color = RGBColors.getRGBColor("WHITE");
        if (outlineShape != null && outlineShape.isDrawable()
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            aTarget.drawWireframeShape(outlineShape, color,
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());
        } else if (outlineShape == null
                && getCapability(OutlineCapability.class).isOutlineOn()) {
            System.out.println("Outline shape is null");
        }

        // first = 0;
    }

    private RGB setColor(double gval) {
        double value = gval;
        if (value == -999.0) {
            value = -9999.0;
        }
        int i = 0;
        RGB gcol = null;
        for (DataMappingEntry entry : dmPref.getEntries()) {
            if (i == colorMap.getColors().size()) {
                break;
            }
            if (value == entry.getDisplayValue()) {
                gcol = convertC(colorMap.getColors().get(i));
                break;
            } else if (value < entry.getDisplayValue()) {
                gcol = convertC(colorMap.getColors().get(i));
                break;
            }
            i++;
        }
        if (gcol == null) {
            i = colorMap.getSize();
            gcol = convertC(colorMap.getColors().get(i - 1));
        }
        return gcol;
    }

    /**
     * convert Color to RGB
     * 
     * @param color
     * @return
     */
    private RGB convertC(Color color) {
        int blue = (int) (color.getBlue() * 255f);
        int green = (int) (color.getGreen() * 255f);
        int red = (int) (color.getRed() * 255f);

        return new RGB(red, green, blue);
    }

    @Override
    protected void disposeInternal() {
        if (shadedShape != null) {
            shadedShape.dispose();
            shadedShape = null;
        }
        if (outlineShape != null) {
            outlineShape.dispose();
            outlineShape = null;
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        display_flag = ddq.display_flag;
        time_pos = ddq.time_pos;
        plot_mean_areal_temp(target, time_pos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        if (ddq.qcmode == "") {
            return "No Data Available";
        }

        return ddq.qcmode;
    }

}
