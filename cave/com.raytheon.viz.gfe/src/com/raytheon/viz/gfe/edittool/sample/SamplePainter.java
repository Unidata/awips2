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
package com.raytheon.viz.gfe.edittool.sample;

import java.awt.Point;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.edittool.EditToolPaintProperties;
import com.raytheon.viz.gfe.edittool.GridID;
import org.locationtech.jts.geom.Coordinate;

/**
 * The SamplePainter is a utility class use to paint samples on the spatial
 * editor. The class has a paint() routine to draw the samples, given the
 * gridIDs, and an indication of the image Grid id.
 *
 * This class also can plot a lat/longitude value
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 08, 2008           chammack  Initial Port from AWIPS I (minus ISC
 *                                  support)
 * Jul 23, 2012  936      dgilling  Reinstate config-handling code to
 *                                  calcGridLabels().
 * Nov 05, 2012  14566    jzeng     Paint the sample points with the order of
 *                                  grids in calcGridLabels ()
 * Jul 24, 2014  3429     mapeters  Updated deprecated drawLine() calls.
 * Aug 14, 2014  3523     mapeters  Updated deprecated {@link
 *                                  DrawableString#textStyle} assignments.
 * Jan 13, 2015  3955     randerso  Fix NullPointerException when updateTime is
 *                                  not set
 * Oct 16, 2015  4971     bsteffen  No longer need to reverse grids.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author chammack
 */

public class SamplePainter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SamplePainter.class);

    private static final String NO_DATA_LABEL = "<NoData>";

    private static final double OFFSET = -4;

    /** Flag to denote if labels should be painted for <NoData> grids */
    protected boolean paintNoGrids;

    /** Flag to denote if ISC markers should be displayed */
    protected boolean showISC;

    /** Flag to denote if Site ID should be displayed on the ISC markers */
    protected boolean showSiteID;

    /**
     * Flag to denote if the official symbol should be displayed on the ISC
     * markers
     */
    protected boolean showOfficial;

    /**
     * Flag to denote if the update time should be displayed on the ISC markers
     */
    protected boolean showUpdateTime;

    /** Flag to denote if data values should be displayed */
    protected boolean showDataValues;

    /** Flag to denote if the lat/lon should be displayed */
    protected boolean showLatLon;

    private IFont font;

    protected boolean showShadows = true;

    private int xOffset;

    private int yOffset;

    private RGB shadowColor;

    private RGB llPlusColor;

    /**
     * Creates a new SamplePainter
     *
     * @param paintNoGrids
     *            Paint <NoData> grids flag
     * @param showLatLon
     *            Show lat/lon flag
     * @param showUpdateTime
     *            Show ISC update time flag
     * @param showISCMode
     *            Show ISC grids flag
     * @param showSiteID
     *            Show ISC Site ID flag
     * @param showOfficial
     *            Show ISC Official symbol flag
     * @param showDataValues
     *            Show data values flag
     * @param font
     *            The desired font
     */
    public SamplePainter(boolean paintNoGrids, boolean showLatLon,
            boolean showUpdateTime, boolean showISCMode, boolean showSiteID,
            boolean showOfficial, boolean showDataValues, IFont font) {
        this.showDataValues = showDataValues;
        this.showISC = showISCMode;
        this.paintNoGrids = paintNoGrids;
        this.showLatLon = showLatLon;
        this.showOfficial = showOfficial;
        this.showSiteID = showSiteID;
        this.showUpdateTime = showUpdateTime;
        this.font = font;

        this.showShadows = GFEPreference.getBoolean("ShowSampleShadows", true);

        String shadowColor = GFEPreference.getString("SampleShadow_color",
                "#000000");
        this.shadowColor = RGBColors.getRGBColor(shadowColor);

        String llPlusColor = GFEPreference.getString("SampleLLPlus_color",
                "white");
        this.llPlusColor = RGBColors.getRGBColor(llPlusColor);

        this.xOffset = GFEPreference.getInt("SampleLabelXOffset");
        this.yOffset = GFEPreference.getInt("SampleLabelYOffset");

    }

    /**
     * Paints a sample point
     *
     * @param target
     *            The GLTarget
     * @param pos
     *            The lat/lon position of the sample
     * @param grid
     *            The grid id
     * @param imageGrid
     *            The image grid
     * @param paintProps
     *            The paint properties
     */
    public void paint(IGraphicsTarget target, Coordinate pos, List<GridID> grid,
            GridID imageGrid, PaintProperties paintProps) {

        EditToolPaintProperties eprops = (EditToolPaintProperties) paintProps;
        double ratio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;

        double[] screenloc = eprops.getDescriptor()
                .worldToPixel(new double[] { pos.x, pos.y });

        try {
            if (showDataValues) {
                double tickMarkExtent = ratio * 3;

                DrawableLine[] lines = new DrawableLine[2];
                lines[0] = new DrawableLine();
                lines[0].setCoordinates(screenloc[0] - tickMarkExtent,
                        screenloc[1]);
                lines[0].addPoint(screenloc[0] + tickMarkExtent, screenloc[1]);
                lines[0].basics.color = llPlusColor;
                lines[0].width = 2.0f;
                lines[1] = new DrawableLine();
                lines[1].setCoordinates(screenloc[0],
                        screenloc[1] - tickMarkExtent);
                lines[1].addPoint(screenloc[0], screenloc[1] + tickMarkExtent);
                lines[1].basics.color = llPlusColor;
                lines[1].width = 2.0f;

                target.drawLine(lines);

            }

            List<String> labels = new ArrayList<>();
            List<RGB> colors = new ArrayList<>();

            calcGridLabels(pos, grid, imageGrid, labels, colors);
            if (showLatLon) {
                calcLatLonLabel(pos, labels, colors);
            }

            DrawableString ds = new DrawableString(
                    labels.toArray(new String[labels.size()]),
                    colors.toArray(new RGB[colors.size()]));
            ds.font = font;
            if (this.showShadows) {
                ds.addTextStyle(TextStyle.DROP_SHADOW, this.shadowColor);
            }
            ds.horizontalAlignment = HorizontalAlignment.CENTER;
            ds.verticallAlignment = VerticalAlignment.BOTTOM;
            ds.basics.x = screenloc[0] + (this.xOffset * ratio);
            ds.basics.y = screenloc[1] + ((this.yOffset + OFFSET) * ratio);

            target.drawStrings(ds);
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Calculates the sample labels and colors for the grid elements
     *
     * @param worldLoc
     *            The world location of the coordinate
     * @param grids
     *            The grid ids
     * @param imageGrid
     *            The image grid
     * @param sampleLabels
     *            The sample label list
     * @param colors
     *            The color list
     */
    private void calcGridLabels(Coordinate worldLoc, final List<GridID> Grids,
            final GridID imageGrid, List<String> sampleLabels,
            List<RGB> colors) {

        if (Grids.isEmpty()) {
            return;
        }

        List<GridID> grids = Grids;

        // if list is not defined, then all samples will be painted for
        // all parms
        List<String> limitSamples = Arrays
                .asList(GFEPreference.getStringArray("SampleParms"));

        // assumes all grids shared same grid location.
        boolean inGrid = false;
        Coordinate gridCoordinate = MapUtil.latLonToGridCoordinate(worldLoc,
                PixelOrientation.UPPER_LEFT,
                grids.get(0).getParm().getGridInfo().getGridLoc());
        int maxX = MapUtil
                .getGridGeometry(
                        grids.get(0).getParm().getGridInfo().getGridLoc())
                .getGridRange().getSpan(0);
        int maxY = MapUtil
                .getGridGeometry(
                        grids.get(0).getParm().getGridInfo().getGridLoc())
                .getGridRange().getSpan(1);
        if ((gridCoordinate.x >= 0) && (gridCoordinate.y >= 0)
                && (gridCoordinate.x < maxX) && (gridCoordinate.y < maxY)) {
            inGrid = true;
        }

        // get the list of samples that should be painted and in the
        // order
        for (GridID grid : grids) {
            String pName = grid.getParm().getParmID().compositeNameUI();

            // do we plot this weather element?
            if ((!limitSamples.isEmpty()) && !limitSamples.contains(pName)) {
                continue; // skip
            }

            // calculate color
            RGB labelColor = grid.getParm().getDisplayAttributes()
                    .getBaseColor();

            if (grid.equals(imageGrid)) {
                RGB color = new RGB(255, 255, 255);
                String colorName = GFEPreference.getString("ImageLegend_color");
                if (!colorName.isEmpty()) {
                    color = RGBColors.getRGBColor(colorName);
                }
                labelColor = color;
            }
            String parmColorName = GFEPreference
                    .getString(pName + "_Sample_color");
            if (!parmColorName.isEmpty()) {
                labelColor = RGBColors.getRGBColor(parmColorName);
            }

            // get the data value
            String label = NO_DATA_LABEL;
            if (inGrid) {
                // isc mode or grid from isc database
                if (showISC || grid.getParm().getParmState().isIscParm()) {
                    label = iscSampleLabel(grid, gridCoordinate);
                } else if (showDataValues) {
                    final IGridData gridData = grid.grid();
                    if (gridData != null) {
                        label = gridData.getWxValue((int) gridCoordinate.x,
                                (int) gridCoordinate.y).toString();
                    }
                } else {
                    label = "";
                }
            }

            // skip the no data grids?
            if (!paintNoGrids && label.equals(NO_DATA_LABEL)) {
                continue;
            }

            sampleLabels.add(label);
            colors.add(labelColor);
        }
    }

    /**
     * Calculates the lat/lon label
     *
     * @param worldLoc
     *            The location of the sample
     * @param sampleLabels
     *            The label list
     * @param colors
     *            The color list
     */
    private void calcLatLonLabel(Coordinate worldLoc, List<String> sampleLabels,
            List<RGB> colors) {
        DecimalFormat df = new DecimalFormat();
        df.setMaximumFractionDigits(2);
        df.setMinimumFractionDigits(2);

        sampleLabels.add(df.format(worldLoc.y) + " / " + df.format(worldLoc.x));

        colors.add(llPlusColor);
    }

    /**
     * Calculates the appropriate sample label in ISC mode
     *
     * @param gid
     *            The gid of the grid
     * @param gridCoord
     *            The sample location
     * @return The ISC sample label
     */
    private String iscSampleLabel(GridID gid, Coordinate gridCoord) {
        DataManager dm = gid.getParm().getDataManager();

        // are we in our own site's domain or not?
        String site = dm.getIscDataAccess().getISCSite(
                new Point((int) gridCoord.x, (int) gridCoord.y), gid);

        // forecast grid?
        DatabaseID dbid = gid.getParm().getParmID().getDbId();
        boolean fcstGrid = (dbid
                .equals(dm.getParmManager().getMutableDatabase()));
        boolean nonFcstISCGrid = (!fcstGrid
                && !gid.getParm().getParmState().isIscParm());

        // normal display - fcst grid inside CWA, other non-isc grids
        if (nonFcstISCGrid || (dm.getSiteID().equals(site) && fcstGrid)) {
            IGridData grid = gid.grid();
            if (grid == null) {
                return NO_DATA_LABEL;
            } else if (showDataValues) {
                return grid.getWxValue((int) gridCoord.x, (int) gridCoord.y)
                        .toString();
            } else {
                return "";
            }
        } else {
            // isc grid, or fcst grid outside cwa
            GridID iscGid = dm.getIscDataAccess().getISCGridID(gid, true);
            if (iscGid == null) {
                // no corresponding parm
                return NO_DATA_LABEL;
            }
            IGridData grid = iscGid.grid();
            if (grid == null) {
                // no corresponding grid
                return NO_DATA_LABEL;
            }

            if (!grid.getHistorySites().contains(site)) {
                // no data in isc grid from corresponding site
                return NO_DATA_LABEL;
            }

            String val = "";
            if (showDataValues) {
                val = grid.getWxValue((int) gridCoord.x, (int) gridCoord.y)
                        .toString();

            }
            if (showSiteID) {
                val += "(" + site + ")";
            }

            GridDataHistory[] his = grid.getHistory();
            Date earlyTime = new Date(Long.MAX_VALUE);
            Date lateTime = new Date(0);
            int pcount = 0;
            int scount = 0;

            for (int i = 0; i < his.length; i++) {
                if (his[i].getOriginParm().getDbId().getSiteId().equals(site)) {
                    scount++;
                    if (his[i].getUpdateTime() != null) {
                        if (his[i].getUpdateTime().after(lateTime)) {
                            lateTime = his[i].getUpdateTime();
                        }
                        if (his[i].getUpdateTime().before(earlyTime)) {
                            earlyTime = his[i].getUpdateTime();
                        }
                    }
                    if (showOfficial && (his[i].getPublishTime() != null)) {
                        pcount++;
                    }
                }
            }

            if (showUpdateTime) {
                val += this.formatUpdateTime(earlyTime, lateTime);
            }
            if (pcount > 0) {
                if (pcount == scount) {
                    val += "P";
                } else {
                    val += "p";
                }
            }
            return val;
        }

    }

    /**
     * Formats the ISC update time
     *
     * @param updateTime
     *            The update time to format
     * @return The formatted time
     */
    public String formatUpdateTime(Date updateTime) {

        if (updateTime == null) {
            return "";
        }

        long ago = (SimulatedTime.getSystemTime().getTime().getTime()
                - updateTime.getTime()) / TimeUtil.MILLIS_PER_SECOND;

        if (ago < 0) {
            return "0";
        }
        StringBuilder buffer = new StringBuilder();

        if (ago < TimeUtil.SECONDS_PER_HOUR) {
            buffer.append(ago / TimeUtil.SECONDS_PER_MINUTE).append("m");
        } else if (ago < (TimeUtil.SECONDS_PER_HOUR * 6)) {
            long hrs = ago / TimeUtil.SECONDS_PER_HOUR;
            long min = (ago % TimeUtil.SECONDS_PER_HOUR)
                    / TimeUtil.SECONDS_PER_MINUTE;

            buffer.append(hrs).append("h").append(min).append("m");
        } else if (ago < TimeUtil.SECONDS_PER_DAY) {
            buffer.append(ago / TimeUtil.SECONDS_PER_HOUR).append("h");
        } else {
            buffer.append(">1d");
        }
        return buffer.toString();
    }

    /**
     * Formats an ISC udpate time range
     *
     * @param earlyTime
     *            The begin time
     * @param lateTime
     *            The end time
     * @return The formatted time range
     */
    public String formatUpdateTime(Date earlyTime, Date lateTime) {

        if (earlyTime.equals(lateTime)) {
            return "[" + formatUpdateTime(earlyTime) + "]";
        } else {
            return "[" + formatUpdateTime(lateTime) + "->"
                    + formatUpdateTime(earlyTime) + "]";
        }
    }
}
