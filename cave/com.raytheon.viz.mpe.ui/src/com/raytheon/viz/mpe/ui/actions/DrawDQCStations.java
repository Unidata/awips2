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
package com.raytheon.viz.mpe.ui.actions;

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorLookupParameters;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;
import com.raytheon.viz.mpe.ui.actions.MPELegendOverride.OverrideType;
import com.raytheon.viz.mpe.ui.mouse.MPELegendInputHandler;
import com.raytheon.viz.mpe.ui.rsc.MPELegendResource;
import com.raytheon.viz.mpe.ui.rsc.PlotGriddedFreezeResource;
import com.raytheon.viz.mpe.ui.rsc.PlotGriddedPrecipResource;
import com.raytheon.viz.mpe.ui.rsc.PlotGriddedPrecipResourceData;
import com.raytheon.viz.mpe.ui.rsc.PlotGriddedTempResource;
import com.raytheon.viz.mpe.ui.rsc.PlotMeanAreaFreezeResource;
import com.raytheon.viz.mpe.ui.rsc.PlotMeanAreaFreezeResourceData;
import com.raytheon.viz.mpe.ui.rsc.PlotMeanAreaPrecipResource;
import com.raytheon.viz.mpe.ui.rsc.PlotMeanAreaPrecipResourceData;
import com.raytheon.viz.mpe.ui.rsc.PlotMeanAreaTempResource;
import com.raytheon.viz.mpe.ui.rsc.PlotMeanAreaTempResourceData;
import com.raytheon.viz.mpe.ui.rsc.PointFreezePlotResource;
import com.raytheon.viz.mpe.ui.rsc.PointFreezeResourceData;
import com.raytheon.viz.mpe.ui.rsc.PointPrecipPlotResource;
import com.raytheon.viz.mpe.ui.rsc.PointPrecipResourceData;
import com.raytheon.viz.mpe.ui.rsc.PointTempPlotResource;
import com.raytheon.viz.mpe.ui.rsc.PointTempResourceData;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Zdata;

/**
 * Handles the drawing of the Daily QC Stations.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            snaples     Initial creation
 * Apr 05, 2016 18350     snaples     Fixed issue with resources not being removed.
 * Feb 28, 2017 10478     bkowal      Utilize {@link MPELegendOverride} to eliminate a few
 *                                    static variables.
 * Jan 24, 2017  6790     mduff       Don't clear the display when drawing DQC stations.
 * Jan 29, 2019  7131     tgurney     setDisplay() check for null data
 * Feb 27, 2019 7731      smanoj      Fixed issue with MPE legends on the lower part of
 *                                    the screen disappear and generate paint error.
 *
 * </pre>
 *
 * @author snaples
 */

public class DrawDQCStations {

    private static DrawDQCStations instance = null;

    private static final List<NamedColorUseSet> pColorSetGroup = MPEColors
            .build_mpe_colors();

    private int points_flag;

    private boolean qpf_on = false;

    private boolean flf_on = false;

    private boolean maxmin_on = false;

    private int pcpn_time_step = 0;

    public static int map_flag = 0;

    private int pcpn_day = 0;

    public static int grids_flag = 0;

    private int pcpn_time = 0;

    private Pdata pdata[] = DailyQcUtils.pdata;

    private Tdata tdata[] = DailyQcUtils.tdata;

    private Zdata zdata[] = DailyQcUtils.zdata;

    public static final ColorMap colorMap = new ColorMap();

    public static final double dqc_precip_delim[][] = {
            { -9999.0, 0.0, 0.01, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1, 1.1,
                    1.2, 1.3, 1.4, 1.5 },
            { -9999.0, 0.0, 0.01, .2, .4, .6, .8, 1., 1.2, 1.4, 1.6, 1.8, 2.0,
                    2.2, 2.4, 2.6, 2.8, 3.0 },
            { -9999.0, 0.0, 0.01, .3, .6, .9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3.0,
                    3.3, 3.6, 3.9, 4.2, 4.5 },
            { -9999.0, 0.0, 0.01, .3, .6, 1.2, 1.8, 2.4, 3.0, 3.6, 4.2, 4.8,
                    5.4, 6.0, 6.6, 7.2, 7.8, 8.4 },
            { -9999.0, 0.0, 0.01, .3, .6, 1.2, 2.4, 3.6, 4.8, 6.0, 7.2, 8.4,
                    9.6, 10.8, 12.0, 13.2, 14.4, 15.6 } };

    public static final double dqc_temp_delim[][] = {
            { -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60 },
            { -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70 },
            { 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80 } };

    public static final double dqc_freezing_delim[][] = {
            { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 } };

    /* global variables */

    public static int kscale = 0;

    public static int zscale = 0;

    public static int tscale = 0;

    public static String qcmode = "";

    private static IRenderableDisplay display = null;

    private static MPEDisplayManager mpd = MPEDisplayManager.getCurrent();

    private static IMapDescriptor md = null;

    private static PlotMeanAreaPrecipResource pmp;

    private static PlotGriddedPrecipResource pgp;

    private static PointPrecipPlotResource mpq;

    private static PointTempPlotResource tpq;

    private static PlotGriddedTempResource tgp;

    private static PlotMeanAreaTempResource tmp;

    private static PointFreezePlotResource zpq;

    private static PlotGriddedFreezeResource zgp;

    private static PlotMeanAreaFreezeResource zmp;

    public static int time_pos = 0;

    public static int display_flag = 0;

    public static int hed;

    public static final String prefix = "pcp";

    private static final String APPLICATION_NAME = "hmapmpe";

    private static MPELegendResource legend;

    private static MPELegendInputHandler legList;

    private int dur = 0;

    private DrawDQCStations() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                plotDQCData(new MPELegendOverride());
            }
        });

    }

    public static synchronized DrawDQCStations getInstance() {
        if (instance == null) {
            instance = new DrawDQCStations();
        }
        mpd = MPEDisplayManager.getCurrent();
        display = mpd.getRenderableDisplay();
        md = (IMapDescriptor) display.getDescriptor();

        List<MPELegendResource> rscs = display.getDescriptor().getResourceList()
                .getResourcesByTypeAsType(MPELegendResource.class);
        for (MPELegendResource rsc : rscs) {
            legend = rsc;
            break;
        }

        if (legend != null) {
            IDisplayPaneContainer container = legend.getResourceContainer();
            legList = new MPELegendInputHandler(legend);
            container.registerMouseHandler(legList);
        }
        return instance;
    }

    public void reloadDQC() {
        reloadDQC(new MPELegendOverride());
    }

    public void reloadDQC(final MPELegendOverride mpeLegendOverride) {
        plotDQCData(mpeLegendOverride);
        display.getContainer().refresh();
    }

    public void reexposeDQC() {
        display.getContainer().refresh();
    }

    public void destroy() {
        if (instance != null) {
            if (md.getResourceList().containsRsc(mpq)) {
                md.getResourceList().removeRsc(mpq);
            }
            if (md.getResourceList().containsRsc(pgp)) {
                md.getResourceList().removeRsc(pgp);
            }
            if (md.getResourceList().containsRsc(pmp)) {
                md.getResourceList().removeRsc(pmp);
            }
            if (md.getResourceList().containsRsc(tpq)) {
                md.getResourceList().removeRsc(tpq);
            }
            if (md.getResourceList().containsRsc(tgp)) {
                md.getResourceList().removeRsc(tgp);
            }
            if (md.getResourceList().containsRsc(tmp)) {
                md.getResourceList().removeRsc(tmp);
            }
            if (md.getResourceList().containsRsc(zpq)) {
                md.getResourceList().removeRsc(zpq);
            }
            if (md.getResourceList().containsRsc(zgp)) {
                md.getResourceList().removeRsc(zgp);
            }
            if (md.getResourceList().containsRsc(zmp)) {
                md.getResourceList().removeRsc(zmp);
            }
            mpd.setQpf(false);
            mpd.setMaxmin(false);
            mpd.setZflag(false);
            IDisplayPaneContainer container = legend.getResourceContainer();
            if (container != null) {
                container.unregisterMouseHandler(legList);
            }
            instance = null;
        }
    }

    private void plotDQCData(final MPELegendOverride mpeLegendOverride) {
        time_pos = 0;
        display_flag = 0;
        hed = 0;
        int plot_view = DailyQcUtils.plot_view;
        int contour_flag = DailyQcUtils.contour_flag;
        points_flag = DailyQcUtils.points_flag;
        qpf_on = MPEDisplayManager.getCurrent().isQpf();
        flf_on = MPEDisplayManager.getCurrent().isZflag();
        maxmin_on = MPEDisplayManager.getCurrent().isMaxmin();
        pcpn_time_step = MPEDisplayManager.pcpn_time_step;
        map_flag = DailyQcUtils.map_flag;
        int pcp_flag = DailyQcUtils.pcp_flag;
        pcpn_day = DailyQcUtils.pcpn_day;
        grids_flag = DailyQcUtils.grids_flag;
        pcpn_time = DailyQcUtils.pcpn_time;
        pdata = DailyQcUtils.pdata;
        tdata = DailyQcUtils.tdata;
        zdata = DailyQcUtils.zdata;

        /*
         * get the token dqc_preprocessor_basetime, the default value is 12Z
         * which means that the temperature/freezingL point level1 data are at
         * 6~12Z, 12~18Z, 18~00Z nd 00~06Z time periods. If the token is set as
         * 18Z, then the temperature/freezingL point level1 data are at 12~18Z,
         * 18~00Z, 00~06Z and 06~12Z. If the token is set as 00Z, then the
         * temperature/freezingL point level1 data are at 18~00Z, 00~06Z, 06~12Z
         * and 12~18Z. If the token is set as 06Z, then the
         * temperature/freezingL point level1 data are at 00~06Z, 06~12Z,
         * 12~18Z. The label is changed for temperature and freezing level based
         * on the token value
         */

        if (MPEDisplayManager.pcpn_time_step == 1) {
            dur = TimeUtil.SECONDS_PER_DAY;
        } else if (MPEDisplayManager.pcpn_time_step == 0) {
            dur = TimeUtil.SECONDS_PER_HOUR * 6;
        }

        String user_id = System.getProperty("user.name");
        List<Colorvalue> pColorSet = GetColorValues.get_colorvalues(user_id,
                APPLICATION_NAME, getCvUse(), dur, "E", pColorSetGroup);
        /*
         * Retrieve the first color.
         */
        if (mpeLegendOverride.getOverrideType() != OverrideType.OFF) {
            final OverrideType overrideType = mpeLegendOverride
                    .getOverrideType();
            final double overrideIndex = mpeLegendOverride.getOverrideIndex()
                    .doubleValue();
            if (overrideType == OverrideType.UP) {
                int index = 0;
                String colorToUse = null;
                for (Colorvalue colorValue : pColorSet) {
                    if (index == overrideIndex) {
                        /*
                         * This is the color to use. Everything in the legend
                         * above this point will be set to this color.
                         */
                        colorToUse = colorValue.getColorname().getColorName();
                    }
                    if (colorToUse != null) {
                        colorValue.getColorname().setColorName(colorToUse);
                    }
                    ++index;
                }
            } else {
                final String firstColorValueName = pColorSet.iterator().next()
                        .getColorname().getColorName();
                int index = 0;
                for (Colorvalue colorValue : pColorSet) {
                    if (index == 0) {
                        index = 1;
                        /*
                         * Skip the first color. All colors within the range are
                         * updated to be the same.
                         */
                        continue;
                    }
                    if (index > overrideIndex) {
                        /*
                         * Everything is included inclusive of the selected
                         * legend entry.
                         */
                        break;
                    }

                    colorValue.getColorname().setColorName(firstColorValueName);

                    ++index;
                }
            }
        }
        ColorLookupParameters parameters = new ColorLookupParameters(
                APPLICATION_NAME, getCvUse(), dur, "E");

        qcmode = setDisplay();

        for (hed = 0; hed < display_flag + 1; hed++) {

            /* precipitation point, gridded, MAP, and contoured data. */
            if (qpf_on) {

                if (map_flag == 1) {

                    if (pcpn_time_step == 0) {
                        time_pos = pcp_flag;
                    } else {
                        time_pos = 40 + pcpn_day;
                    }
                    if (md.getResourceList().containsRsc(pmp)) {
                        md.getResourceList().removeRsc(pmp);
                        pmp.dispose();
                    }
                    ResourcePair rp = new ResourcePair();
                    PlotMeanAreaPrecipResourceData pmpRscData = new PlotMeanAreaPrecipResourceData(
                            MPEDisplayManager.getCurrent(), pColorSet);
                    rp.setResourceData(pmpRscData);
                    md.getResourceList().add(rp);
                    md.getResourceList().instantiateResources(md, true);
                    pmp = (PlotMeanAreaPrecipResource) rp.getResource();
                    mpd.setDisplayedResource(pmp);
                } else {
                    if (md.getResourceList().containsRsc(pmp)) {
                        md.getResourceList().removeRsc(pmp);
                    }
                }

                /* gridded precipitation */

                if (grids_flag == 1) {

                    Set<DisplayMode> mode = mpd.getDisplayMode();
                    if (display.getDescriptor().getResourceList()
                            .containsRsc(pgp)) {
                        display.getDescriptor().getResourceList()
                                .removeRsc(pgp);
                        pgp.dispose();
                    }
                    if (mode.contains(DisplayMode.Image)) {
                        // we are ok
                    } else {
                        mpd.toggleDisplayMode(DisplayMode.Image);
                        if (mode.contains(DisplayMode.Contour)) {
                            mpd.toggleDisplayMode(DisplayMode.Contour);
                        }
                    }

                    if (pcpn_time_step == 0) {
                        time_pos = pcp_flag;
                    } else {
                        time_pos = 40 + pcpn_day;
                    }

                    ResourcePair rp = new ResourcePair();
                    PlotGriddedPrecipResourceData pgpRscData = new PlotGriddedPrecipResourceData(
                            MPEDisplayManager.getCurrent(), pColorSet);
                    rp.setResourceData(pgpRscData);
                    display.getDescriptor().getResourceList().add(rp);
                    display.getDescriptor().getResourceList()
                            .instantiateResources(display.getDescriptor(),
                                    true);
                    pgp = (PlotGriddedPrecipResource) rp.getResource();
                    mpd.setDisplayedResource(pgp);
                } else {
                    if (display.getDescriptor().getResourceList()
                            .containsRsc(pgp)) {
                        display.getDescriptor().getResourceList()
                                .removeRsc(pgp);
                        pgp.dispose();
                    }
                }

                /* Contoured precipitation. */
                if (contour_flag == 1) {

                    Set<DisplayMode> mode = mpd.getDisplayMode();
                    if (display.getDescriptor().getResourceList()
                            .containsRsc(pgp)) {
                        display.getDescriptor().getResourceList()
                                .removeRsc(pgp);
                        pgp.dispose();
                    }
                    if (mode.contains(DisplayMode.Contour)) {
                        // we are ok
                    } else {
                        mpd.toggleDisplayMode(DisplayMode.Contour);
                        mode = mpd.getDisplayMode();
                        if (mode.contains(DisplayMode.Image)) {
                            mpd.toggleDisplayMode(DisplayMode.Image);
                        }
                    }

                    if (pcpn_time_step == 0) {
                        time_pos = pcp_flag;
                    } else {
                        time_pos = 40 + pcpn_day;
                    }
                    ResourcePair rp = new ResourcePair();
                    PlotGriddedPrecipResourceData pgpRscData = new PlotGriddedPrecipResourceData(
                            MPEDisplayManager.getCurrent(), pColorSet);
                    rp.setResourceData(pgpRscData);
                    display.getDescriptor().getResourceList().add(rp);
                    display.getDescriptor().getResourceList()
                            .instantiateResources(display.getDescriptor(),
                                    true);
                    pgp = (PlotGriddedPrecipResource) rp.getResource();
                    mpd.setDisplayedResource(pgp);
                }

                /* Point precipitation data. */
                if ((plot_view > 0) && (points_flag == 1)) {

                    Iterator<?> it = md.getResourceList().iterator();
                    while (it.hasNext()) {
                        ResourcePair rp = (ResourcePair) it.next();
                        if (rp.getResource() instanceof PointPrecipPlotResource) {
                            rp.getResource().dispose();
                            md.getResourceList().removeRsc(rp.getResource());
                            break;
                        }
                    }
                    ResourcePair pair = new ResourcePair();
                    PointPrecipResourceData ppd = new PointPrecipResourceData(
                            mpd, "Dqc Precip Points", parameters);
                    pair.setResourceData(ppd);
                    ResourceProperties props = new ResourceProperties();
                    props.setSystemResource(true);
                    pair.setProperties(props);
                    md.getResourceList().add(pair);
                    md.getResourceList().instantiateResources(
                            display.getDescriptor(), true);
                    mpq = (PointPrecipPlotResource) pair.getResource();

                    if (grids_flag == 1){
                        /* Points + Grids precipitation data */
                        mpd.setDisplayedResource(pgp);
                    }

                }

            } else if (flf_on) {
                /* Plot freezing level data. */
                if (map_flag == 1) {
                    time_pos = 100 + pcp_flag;

                    if (md.getResourceList().containsRsc(zmp)) {
                        md.getResourceList().removeRsc(zmp);
                        zmp.dispose();
                    }
                    ResourcePair rp = new ResourcePair();
                    PlotMeanAreaFreezeResourceData zmpRscData = new PlotMeanAreaFreezeResourceData(
                            MPEDisplayManager.getCurrent(), pColorSet);
                    rp.setResourceData(zmpRscData);
                    md.getResourceList().add(rp);
                    md.getResourceList().instantiateResources(md, true);
                    zmp = (PlotMeanAreaFreezeResource) rp.getResource();
                    mpd.setDisplayedResource(zmp);
                } else {
                    if (md.getResourceList().containsRsc(zmp)) {
                        md.getResourceList().removeRsc(zmp);
                        zmp.dispose();
                    }
                }

                if (grids_flag == 1) {
                    time_pos = 100 + pcp_flag;
                    Set<DisplayMode> mode = mpd.getDisplayMode();
                    if (md.getResourceList().containsRsc(zgp)) {
                        md.getResourceList().removeRsc(zgp);
                        zgp.dispose();
                    }
                    if (mode.contains(DisplayMode.Image)) {
                        // we are ok
                    } else {
                        mpd.toggleDisplayMode(DisplayMode.Image);
                        if (mode.contains(DisplayMode.Contour)) {
                            mpd.toggleDisplayMode(DisplayMode.Contour);
                        }
                    }
                    zgp = new PlotGriddedFreezeResource(mpd,
                            new LoadProperties(), pColorSet);

                    md.getResourceList().add(zgp);
                    mpd.setDisplayedResource(zgp);
                } else {
                    if (md.getResourceList().containsRsc(zgp)) {
                        md.getResourceList().removeRsc(zgp);
                        zgp.dispose();
                    }
                }

                if (contour_flag == 1) {
                    time_pos = 100 + pcp_flag;

                    Set<DisplayMode> mode = mpd.getDisplayMode();
                    if (md.getResourceList().containsRsc(zgp)) {
                        md.getResourceList().removeRsc(zgp);
                        zgp.dispose();
                    }
                    if (mode.contains(DisplayMode.Contour)) {
                        // we are ok
                    } else {
                        mpd.toggleDisplayMode(DisplayMode.Contour);
                        if (mode.contains(DisplayMode.Image)) {
                            mpd.toggleDisplayMode(DisplayMode.Image);
                        }
                    }
                    zgp = new PlotGriddedFreezeResource(mpd,
                            new LoadProperties(), pColorSet);
                    md.getResourceList().add(zgp);

                    mpd.setDisplayedResource(zgp);
                }

                if ((plot_view > 0) && (points_flag == 1)) {
                    Iterator<?> it = display.getDescriptor().getResourceList()
                            .iterator();
                    while (it.hasNext()) {
                        ResourcePair rp = (ResourcePair) it.next();
                        if (rp.getResource() instanceof PointFreezePlotResource) {
                            rp.getResource().dispose();
                            md.getResourceList().removeRsc(rp.getResource());
                            break;
                        }
                    }
                    ResourcePair pair = new ResourcePair();
                    PointFreezeResourceData zpd = new PointFreezeResourceData(
                            mpd, "Dqc Freeze Points", parameters);
                    pair.setResourceData(zpd);
                    ResourceProperties props = new ResourceProperties();
                    props.setSystemResource(true);
                    pair.setProperties(props);
                    display.getDescriptor().getResourceList().add(pair);
                    display.getDescriptor().getResourceList()
                            .instantiateResources(display.getDescriptor(),
                                    true);
                    zpq = (PointFreezePlotResource) pair.getResource();
                    display.getContainer().refresh();
                }
            } else if (maxmin_on) {
                if ((map_flag == 1) && (pcpn_time_step == 0)) {
                    time_pos = 150 + pcp_flag;

                    if (md.getResourceList().containsRsc(tmp)) {
                        md.getResourceList().removeRsc(tmp);
                        tmp.dispose();
                    }
                    ResourcePair rp = new ResourcePair();
                    PlotMeanAreaTempResourceData tmpRscData = new PlotMeanAreaTempResourceData(
                            MPEDisplayManager.getCurrent(), pColorSet);
                    rp.setResourceData(tmpRscData);
                    md.getResourceList().add(rp);
                    md.getResourceList().instantiateResources(md, true);
                    tmp = (PlotMeanAreaTempResource) rp.getResource();
                    mpd.setDisplayedResource(tmp);
                } else {
                    if (md.getResourceList().containsRsc(tmp)) {
                        md.getResourceList().removeRsc(tmp);
                        tmp.dispose();
                    }
                }

                if (grids_flag == 1) {
                    Set<DisplayMode> mode = mpd.getDisplayMode();
                    if (md.getResourceList().containsRsc(tgp)) {
                        md.getResourceList().removeRsc(tgp);
                        tgp.dispose();
                    }
                    if (mode.contains(DisplayMode.Image)) {
                        // we are ok
                    } else {
                        mpd.toggleDisplayMode(DisplayMode.Image);
                        if (mode.contains(DisplayMode.Contour)) {
                            mpd.toggleDisplayMode(DisplayMode.Contour);
                        }
                    }
                    tgp = new PlotGriddedTempResource(mpd, new LoadProperties(),
                            pColorSet);

                    if (pcpn_time_step == 0) {
                        time_pos = 150 + pcp_flag;
                    } else if (pcpn_time_step == 1) {
                        time_pos = 190 + pcpn_day;
                    } else if (pcpn_time_step == 2) {
                        time_pos = 200 + pcpn_day;
                    }
                    md.getResourceList().add(tgp);

                    mpd.setDisplayedResource(tgp);
                } else {
                    if (md.getResourceList().containsRsc(tgp)) {
                        md.getResourceList().removeRsc(tgp);
                        tgp.dispose();
                    }
                }

                if (contour_flag == 1) {
                    Set<DisplayMode> mode = mpd.getDisplayMode();
                    if (md.getResourceList().containsRsc(tgp)) {
                        md.getResourceList().removeRsc(tgp);
                        tgp.dispose();
                    }
                    if (mode.contains(DisplayMode.Contour)) {
                        // we are ok
                    } else {
                        mpd.toggleDisplayMode(DisplayMode.Contour);
                        if (mode.contains(DisplayMode.Image)) {
                            mpd.toggleDisplayMode(DisplayMode.Image);
                        }
                    }
                    tgp = new PlotGriddedTempResource(mpd, new LoadProperties(),
                            pColorSet);

                    if (pcpn_time_step == 0) {
                        time_pos = 150 + pcp_flag;
                    } else if (pcpn_time_step == 1) {
                        time_pos = 190 + pcpn_day;
                    } else if (pcpn_time_step == 2) {
                        time_pos = 200 + pcpn_day;
                    }
                    md.getResourceList().add(tgp);

                    mpd.setDisplayedResource(tgp);
                }

                if ((plot_view > 0) && (points_flag == 1)) {
                    Iterator<?> it = display.getDescriptor().getResourceList()
                            .iterator();
                    while (it.hasNext()) {
                        ResourcePair rp = (ResourcePair) it.next();
                        if (rp.getResource() instanceof PointTempPlotResource) {
                            rp.getResource().dispose();
                            md.getResourceList().removeRsc(rp.getResource());
                            break;
                        }
                    }
                    ResourcePair pair = new ResourcePair();
                    PointTempResourceData tpd = new PointTempResourceData(mpd,
                            "Dqc Temp Points", parameters);
                    pair.setResourceData(tpd);
                    ResourceProperties props = new ResourceProperties();
                    props.setSystemResource(true);
                    pair.setProperties(props);
                    display.getDescriptor().getResourceList().add(pair);
                    display.getDescriptor().getResourceList()
                            .instantiateResources(display.getDescriptor(),
                                    true);
                    tpq = (PointTempPlotResource) pair.getResource();
                    display.getContainer().refresh();
                }
            }
        }
    }

    protected String setDisplay() {
        Calendar ltime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        StringBuilder mbuf = new StringBuilder();
        int dqcEndingObsTime = DailyQcUtils.getEnding6HourObsTime();
        StringBuilder tbuf = new StringBuilder();

        if (qpf_on && pdata != null) {
            int ptime_pos = 0;
            if (pcpn_time_step == 0) {
                ptime_pos = pcpn_time;
            } else {
                ptime_pos = 4;
            }

            /* Precipitation period is always 12z-12z. */
            if ((pcpn_time < 2) && (pcpn_time_step == 0)) {
                ltime.setTime(pdata[pcpn_day].data_time);
                ltime.add(Calendar.SECOND, -TimeUtil.SECONDS_PER_DAY);
            } else {
                ltime.setTime(pdata[pcpn_day].data_time);
            }

            tbuf.append("Precipitation ");

            if ((points_flag == 1) && (grids_flag == -1) && (map_flag == -1)) {
                mbuf.append("Points ");
            } else if ((points_flag == -1) && (grids_flag == 1)
                    && (map_flag == -1)) {
                mbuf.append("Grids ");
            } else if ((points_flag == -1) && (grids_flag == -1)
                    && (map_flag == 1)) {
                mbuf.append("MAPs ");
            } else if ((points_flag == 1) && (grids_flag == 1)
                    && (map_flag == -1)) {
                mbuf.append("Points+Grids ");
            } else if ((points_flag == 1) && (grids_flag == -1)
                    && (map_flag == 1)) {
                mbuf.append("Points+MAPs ");
            } else if ((points_flag == -1) && (grids_flag == -1)
                    && (map_flag == -1)) {
                mbuf.append(" ");
            }

            tbuf.append(mbuf.toString());

            mbuf = new StringBuilder();
            mbuf.append(String.format("%02d-%02d-%04d",
                    ltime.get(Calendar.MONTH) + 1,
                    ltime.get(Calendar.DAY_OF_MONTH),
                    ltime.get(Calendar.YEAR)));

            tbuf.append(mbuf.toString());

            if (pcpn_time_step == 0) {
                /* Precipitation is always 12z-12z. */
                if (pcpn_time == 0) {
                    tbuf.append(" 12z-18z");
                } else if (pcpn_time == 1) {
                    tbuf.append(" 18z-00z");
                } else if (pcpn_time == 2) {
                    tbuf.append(" 00z-06z");
                } else if (pcpn_time == 3) {
                    tbuf.append(" 06z-12z");
                }
            } else {
                tbuf.append(" ending at 12z");
            }

            if (pdata[pcpn_day].level == 1) {
                tbuf.append(" - Level 1");
            } else if (pdata[pcpn_day].level == 2) {
                tbuf.append(" - Level 2");
            }

            if (pdata[pcpn_day].used[ptime_pos] == 4) {
                tbuf.append(" Saved");
            } else if ((pdata[pcpn_day].used[ptime_pos] == 3)
                    || (pdata[pcpn_day].used[ptime_pos] == 2)) {
                tbuf.append(" Modified");
            } else if (pdata[pcpn_day].used[ptime_pos] == 1) {
                tbuf.append(" Not Modified");
            } else {
                tbuf.append(" - No Data");
            }
        } else if (flf_on && zdata != null) {
            int ptime_pos = 0;
            ptime_pos = pcpn_time;

            if (dqcEndingObsTime == 12) {
                /* Times: 18, 00, 06, 12 */
                if (pcpn_time < 1) {
                    ltime.setTime(zdata[pcpn_day].data_time);
                    ltime.add(Calendar.SECOND, -TimeUtil.SECONDS_PER_DAY);
                } else {
                    ltime.setTime(zdata[pcpn_day].data_time);
                }
            } else {
                /* Times 12, 18, 00, 06 */
                if (pcpn_time < 2) {
                    ltime.setTime(zdata[pcpn_day].data_time);
                    ltime.add(Calendar.SECOND, -TimeUtil.SECONDS_PER_DAY);
                } else {
                    ltime.setTime(zdata[pcpn_day].data_time);
                }
            }

            tbuf = new StringBuilder();
            tbuf.append("Freezing Level ");
            mbuf = new StringBuilder();

            if ((points_flag == 1) && (grids_flag == -1) && (map_flag == -1)) {
                mbuf.append("Points ");
            } else if ((points_flag == -1) && (grids_flag == 1)
                    && (map_flag == -1)) {
                mbuf.append("Grids ");
            } else if ((points_flag == -1) && (grids_flag == -1)
                    && (map_flag == 1)) {
                mbuf.append("MAZs ");
            } else if ((points_flag == 1) && (grids_flag == 1)
                    && (map_flag == -1)) {
                mbuf.append("Points+Grids ");
            } else if ((points_flag == 1) && (grids_flag == -1)
                    && (map_flag == 1)) {
                mbuf.append("Points+MAZs ");
            } else if ((points_flag == -1) && (grids_flag == -1)
                    && (map_flag == -1)) {
                mbuf.append(" ");
            }

            tbuf.append(mbuf.toString());
            mbuf = new StringBuilder();

            mbuf.append(String.format("%02d-%02d-%04d",
                    ltime.get(Calendar.MONTH) + 1,
                    ltime.get(Calendar.DAY_OF_MONTH),
                    ltime.get(Calendar.YEAR)));

            tbuf.append(mbuf.toString());

            if (dqcEndingObsTime == 12) {
                if (pcpn_time == 0) {
                    tbuf.append(" 18z");
                } else if (pcpn_time == 1) {
                    tbuf.append(" 00z");
                } else if (pcpn_time == 2) {
                    tbuf.append(" 06z");
                } else if (pcpn_time == 3) {
                    tbuf.append(" 12z");
                }
            } else {
                if (pcpn_time == 0) {
                    tbuf.append(" 12z");
                } else if (pcpn_time == 1) {
                    tbuf.append(" 18z");
                } else if (pcpn_time == 2) {
                    tbuf.append(" 00z");
                } else if (pcpn_time == 3) {
                    tbuf.append(" 06z");
                }
            }

            if (zdata[pcpn_day].level[ptime_pos] == 1) {
                tbuf.append(" - Level 1");
            } else if (zdata[pcpn_day].level[ptime_pos] == 2) {
                tbuf.append(" - Level 2");
            }

            if (zdata[pcpn_day].used[ptime_pos] == 6) {
                tbuf.append(" Calculated");
            } else if (zdata[pcpn_day].used[ptime_pos] == 4) {
                tbuf.append(" Saved");
            } else if ((zdata[pcpn_day].used[ptime_pos] == 3)
                    || (zdata[pcpn_day].used[ptime_pos] == 2)) {
                tbuf.append(" Modified");
            } else if (zdata[pcpn_day].used[ptime_pos] == 1) {
                tbuf.append(" Not Modified");
            } else {
                tbuf.append(" - No Data");
            }
        } else if (maxmin_on && tdata != null) {

            int ptime_pos = 0;
            if (pcpn_time_step == 0) {
                ptime_pos = pcpn_time;
            } else if (pcpn_time_step == 1) {
                ptime_pos = 4;
            } else if (pcpn_time_step == 2) {
                ptime_pos = 5;
            }

            if (dqcEndingObsTime == 12) {
                if ((pcpn_time < 1) && (pcpn_time_step == 0)) {
                    ltime.setTime(tdata[pcpn_day].data_time);
                    ltime.add(Calendar.SECOND, -TimeUtil.SECONDS_PER_DAY);
                } else {
                    ltime.setTime(tdata[pcpn_day].data_time);
                }
            } else {
                if ((pcpn_time < 2) && (pcpn_time_step == 0)) {
                    ltime.setTime(tdata[pcpn_day].data_time);
                    ltime.add(Calendar.SECOND, -TimeUtil.SECONDS_PER_DAY);
                } else {
                    ltime.setTime(tdata[pcpn_day].data_time);
                }
            }

            tbuf = new StringBuilder();
            if (pcpn_time_step == 1) {
                tbuf.append("Maximum Temperature ");
            } else if (pcpn_time_step == 2) {
                tbuf.append("Minimum Temperature ");
            } else {
                tbuf.append("Temperature ");
            }

            if ((points_flag == 1) && (grids_flag == -1) && (map_flag == -1)) {
                mbuf.append("Points ");
            } else if ((points_flag == -1) && (grids_flag == 1)
                    && (map_flag == -1)) {
                mbuf.append("Grids ");
            } else if ((points_flag == -1) && (grids_flag == -1)
                    && (map_flag == 1)) {
                mbuf.append("MATs ");
            } else if ((points_flag == 1) && (grids_flag == 1)
                    && (map_flag == -1)) {
                mbuf.append("Points+Grids ");
            } else if ((points_flag == 1) && (grids_flag == -1)
                    && (map_flag == 1)) {
                mbuf.append("Points+MATs ");
            } else if ((points_flag == -1) && (grids_flag == -1)
                    && (map_flag == -1)) {
                mbuf.append(" ");
            }

            tbuf.append(mbuf.toString());

            mbuf = new StringBuilder();
            mbuf.append(String.format("%02d-%02d-%04d",
                    ltime.get(Calendar.MONTH) + 1,
                    ltime.get(Calendar.DAY_OF_MONTH),
                    ltime.get(Calendar.YEAR)));

            tbuf.append(mbuf.toString());

            if (pcpn_time_step == 0) {
                if (dqcEndingObsTime == 12) {
                    if (pcpn_time == 0) {
                        tbuf.append(" 18z");
                    } else if (pcpn_time == 1) {
                        tbuf.append(" 00z");
                    } else if (pcpn_time == 2) {
                        tbuf.append(" 06z");
                    } else if (pcpn_time == 3) {
                        tbuf.append(" 12z");
                    }
                } else {
                    if (pcpn_time == 0) {
                        tbuf.append(" 12z");
                    } else if (pcpn_time == 1) {
                        tbuf.append(" 18z");
                    } else if (pcpn_time == 2) {
                        tbuf.append(" 00z");
                    } else if (pcpn_time == 3) {
                        tbuf.append(" 06z");
                    }
                }
            } else {
                tbuf.append(" ending at 12z");
            }

            if (tdata[pcpn_day].level[ptime_pos] == 1) {
                tbuf.append(" - Level 1");
            } else if (tdata[pcpn_day].level[ptime_pos] == 2) {
                tbuf.append(" - Level 2");
            }

            if (tdata[pcpn_day].used[ptime_pos] == 4) {
                tbuf.append(" Saved");
            } else if ((tdata[pcpn_day].used[ptime_pos] == 3)
                    || (tdata[pcpn_day].used[ptime_pos] == 2)) {
                tbuf.append(" Modified");
            } else if (tdata[pcpn_day].used[ptime_pos] == 1) {
                tbuf.append(" Not Modified");
            } else {
                tbuf.append(" - No Data");
            }
        }

        return tbuf.toString();
    }

    private String getCvUse() {
        String use = "";
        if (qpf_on) {
            if (pcpn_time_step == 1) {
                if (grids_flag == 1) {
                    use = "24hGRID_PRECIP";
                } else if (map_flag == 1) {
                    use = "24hMAREA_PRECIP";
                } else {
                    use = "24hGRID_PRECIP";
                }
            } else {
                /* 6hr is selected */

                if (grids_flag == 1) {
                    use = "6hGRID_PRECIP";
                } else if (map_flag == 1) {
                    use = "6hMAREA_PRECIP";
                } else {
                    use = "6hGRID_PRECIP";
                }
            }
        } else if (flf_on) {
            if (map_flag == 1) {
                use = "6hMAREA_FREEZL";
                /* 6 hours duration for MAZ */
            } else {
                use = "6hGRID_FREEZL";
            }
            dur = TimeUtil.SECONDS_PER_HOUR * 6;
        } else {
            if (pcpn_time_step == 0) {
                if (grids_flag == 1) {
                    use = "sixhGRID_TEMP";
                } else if (map_flag == 1) {
                    use = "sixhMAREA_TEMP";
                } else {
                    use = "sixhGRID_TEMP";
                }
                dur = TimeUtil.SECONDS_PER_HOUR * 6;
            } else if (pcpn_time_step == 1) {
                use = "maxGRID_TEMP";
                dur = TimeUtil.SECONDS_PER_DAY;
            } else if (pcpn_time_step == 2) {
                use = "minGRID_TEMP";
                dur = TimeUtil.SECONDS_PER_DAY;
            }

        }
        return use;
    }
}
