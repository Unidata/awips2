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
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.util.CreateMap;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Maps;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;
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
 * May 2, 2013  15970     snaples     Updated setColor to use the correct color.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class PlotMeanAreaPrecipResource extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> implements
        IMpeResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotMeanAreaPrecipResource.class);

    MPEDisplayManager displayMgr = null;

    int first = 1;

    int hed;

    int time_pos;

    int display_flag;

    private ColorMapParameters parameters = new ColorMapParameters();

    private final List<Colorvalue> colorSet;

    public PlotMeanAreaPrecipResource(MPEDisplayManager displayMgr,
            List<Colorvalue> colorSet) {
        super(new PlotMeanAreaPrecipResourceData(), new LoadProperties());
        this.displayMgr = displayMgr;
        this.colorSet = colorSet;
    }

    ColorMap colorMap;

    DataMappingPreferences dmPref = new DataMappingPreferences();

    RGB color;

    IGraphicsTarget target;

    private IWireframeShape outlineShape;

    private IShadedShape shadedShape;

    GeometryFactory jtsGeometryFactory;

    Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();

    Pcp pcp = DailyQcUtils.pcp;

    Pcp spf = DailyQcUtils.spf;

    Pcp tpf = DailyQcUtils.tpf;

    public void plot_mean_areal_precip(int num) {

        double[][] dqc_precip_delim = DrawDQCStations.dqc_precip_delim;
        int dqc_precip_numcol = 0;
        Maps mean_areal_precip_global[] = DailyQcUtils.mean_areal_precip_global;
        int pcp_in_use[] = DailyQcUtils.pcp_in_use;
        int pcpn_time_step = MPEDisplayManager.pcpn_time_step;
        int rsmode = OtherPrecipOptions.rsmode;
        boolean wfo_all = DailyQcUtils.wfo_all;
        int[] wfo_in_use = DailyQcUtils.wfo_in_use;
        CreateMap cm = new CreateMap();

        double mapvalue;
        int hh = 0;
        int k;
        int l;
        int minx, miny, ix, iy, maxx, maxy;
        int x, y, ip, ib;
        int xpos;
        int ypos;
        float uz, mz, lz, gz;
        int i1 = 0;
        int i, j;
        int kscale = DrawDQCStations.kscale;
        String file = "pcp";
        Coordinate[] points = new Coordinate[1];
        Coordinate[] PolyPoints = new Coordinate[5];

        /* Retrieve the precipitation colormap. */
        colorMap = new ColorMap(colorSet.size());

        dqc_precip_numcol = colorMap.getSize();

        if (pcpn_time_step == 1) {
            colorMap.setName("24hMAREA_PRECIP");
        } else if (pcpn_time_step == 0) {
            colorMap.setName("6hMAREA_PRECIP");
        }
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

        displayUnit = NonSI.INCH;
        dataUnit = NonSI.INCH.divide(100.0);
        parameters.setFormatString("0.00");

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
        if (pcpn_time_step == 0 && rsmode != 1) {
            i1 = 1;

            if (num == 0) {
                i1 = 0;
            }

            if (pcp_in_use[num + 100] != -1 && pcp_in_use[num + 100 - i1] != -1) {
                cm.read_file(file, num + 100, spf);
                cm.read_file(file, num + 100 - i1, pcp);

                for (i = 0; i < hrap_grid.maxi - hrap_grid.hrap_minx; i++) {
                    for (j = 0; j < hrap_grid.maxj - hrap_grid.hrap_miny; j++) {
                        spf.value[i][j] = (spf.value[i][j] + pcp.value[i][j]) / 2;
                    }
                }
            } else if (pcp_in_use[num + 100] == 1) {
                cm.read_file(file, num + 100, spf);
            } else if (pcp_in_use[num + 100 - i1] == 1) {
                cm.read_file(file, num + 100 - i1, spf);
            }
        }

        minx = hrap_grid.hrap_minx;
        miny = hrap_grid.hrap_miny;
        maxx = hrap_grid.maxi;
        maxy = hrap_grid.maxj;

        for (ib = 0; ib < DailyQcUtils.getMax_basins(); ib++) {
            if (mean_areal_precip_global[ib].maps_done[num] <= 0) {
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

                    if (mean_areal_precip_global[ib].owner == wfo_in_use[hh]) {
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
            points = new Coordinate[mean_areal_precip_global[ib].basin_points];

            for (l = 0; l < mean_areal_precip_global[ib].basin_points; l++) {
                Coordinate ll = new Coordinate();
                ll.x = mean_areal_precip_global[ib].basin[l].lon;
                ll.y = mean_areal_precip_global[ib].basin[l].lat;
                points[l] = ll;
            }

            lz = mean_areal_precip_global[ib].lz[num];
            mz = mean_areal_precip_global[ib].mz[num];
            uz = mean_areal_precip_global[ib].uz[num];
            gz = mean_areal_precip_global[ib].gz[num];

            /* If there are no subareas and this is raster mode. */
            if ((mean_areal_precip_global[ib].zones[1] != 1)
                    && (mean_areal_precip_global[ib].zones[2] != 1)
                    && (mean_areal_precip_global[ib].zones[3] != 1)
                    && (rsmode == 1)) {
                mapvalue = lz;

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
                            "Error reprojecting MAP outline", e);
                }
            }

            for (l = 0; l < mean_areal_precip_global[ib].hrap_points; l++) {

                /*
                 * Retrieve the HRAP Coordinates of the HRAP Cell being
                 * processed.
                 */
                x = mean_areal_precip_global[ib].hrap_data[l].x;
                y = mean_areal_precip_global[ib].hrap_data[l].y;

                /* search for highest zone number is hrap block */
                ip = 1;

                if (mean_areal_precip_global[ib].hrap_data[l].zone[1] >= 0) {
                    ip = 2;
                }

                if (mean_areal_precip_global[ib].hrap_data[l].zone[2] >= 0) {
                    ip = 3;
                }

                if (mean_areal_precip_global[ib].hrap_data[l].zone[3] >= 0) {
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

                if (rsmode == 1) {
                    for (k = 0; k < dqc_precip_numcol - 1; k++) {
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
                        if (mapvalue >= dqc_precip_delim[kscale][k]
                                && mapvalue < dqc_precip_delim[kscale][k + 1]) {
                            color = convertC(colorMap.getColors().get(k));
                            break;
                        }
                    }
                    if (mapvalue <= 0.01 && mapvalue >= -0.01) {
                        continue;

                    } else if (k == (dqc_precip_numcol - 1)) {
                        color = convertC(colorMap.getColors().get(
                                dqc_precip_numcol - 1));
                    }
                } else {
                    for (k = 0; k < 4; k++) {
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

                        if (mapvalue >= dqc_precip_delim[kscale][k]
                                && mapvalue < dqc_precip_delim[kscale][k + 1]) {
                            color = convertC(colorMap.getColors().get(k));
                            break;
                        }
                    }
                    if (mapvalue <= 0.01 && mapvalue >= -0.01) {
                        continue;
                    }

                    if (k == 4) {
                        color = convertC(colorMap.getColors().get(4));
                    }

                    if (mapvalue < -99.98) {
                        color = convertC(colorMap.getColors().get(7));
                    }

                    if (color.equals(convertC(colorMap.getColors().get(0)))) {
                        continue;
                    }
                }

                if (rsmode != 1
                        && (pcp_in_use[100 + num] == 1 || pcp_in_use[100 + num
                                - i1] == 1)) {

                    if ((spf.value[ix][iy] * 10 - DrawDQCStations.dmvalue < hrap_grid.elev[ix][iy])
                            && spf.value[ix][iy] >= 0) {
                        color = convertC(colorMap.getColors().get(k + 5));
                    }

                }

                /* Using the MPE Lat/Lon Grid, draw the point. */

                xpos = mean_areal_precip_global[ib].hrap_data[l].x;
                ypos = mean_areal_precip_global[ib].hrap_data[l].y;

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
                            "Error reprojecting MAP basin outline", e);
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

        if (DailyQcUtils.map_flag != 1 || displayMgr.isQpf() != true) {
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
            // DR 15970
            // Adjusted the index value of the color returned, was
            // returning one level higher than value should have.
            if (value == entry.getDisplayValue()) {
            	if (i == 0){
                gcol = convertC(colorMap.getColors().get(i));
            	} else {
            		gcol = convertC(colorMap.getColors().get(i - 1));
            	}
                break;
            } else if (value < entry.getDisplayValue()) {
            	if (i == 0){
                    gcol = convertC(colorMap.getColors().get(i));
                	} else {
                		gcol = convertC(colorMap.getColors().get(i - 1));
                	}
            	break;
            }
            i++;
        }
        if (gcol == null) {
            i = dmPref.getEntries().size();
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
        display_flag = DrawDQCStations.display_flag;
        time_pos = DrawDQCStations.time_pos;
        this.target = target;
        plot_mean_areal_precip(time_pos);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        if (DrawDQCStations.qcmode == "") {
            return "No Data Available";
        }

        return DrawDQCStations.qcmode;
    }

}
