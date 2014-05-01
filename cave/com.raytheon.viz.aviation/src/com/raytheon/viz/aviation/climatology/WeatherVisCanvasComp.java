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
package com.raytheon.viz.aviation.climatology;

import java.util.Calendar;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.aviation.xml.PlotViewerCfg;
import com.raytheon.viz.aviation.xml.WxPlotCfg;

/**
 * This class draws the Visibility graph data on the Visibility Weather canvas.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 JUN 2008  1119        lvenable    Initial creation
 * 18 Nov 2010  6701        rferrel     Use PlotViewerCfg class name.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WeatherVisCanvasComp extends WeatherCanvasComp {
    /**
     * Parent composite.
     */
    // private Composite parent; // not used yet
    /**
     * Y coordinate of the 1/8 value.
     */
    private int yCoordBottom = graphRect.height + graphRect.y;

    /**
     * Y coordinate of the 1/2 value.
     */
    private int yCoord1_2 = graphRect.height + graphRect.y - 40;

    /**
     * Y coordinate of the 1 value.
     */
    private int yCoord1 = graphRect.height + graphRect.y - 60;

    /**
     * Y coordinate of the 2 value.
     */
    private int yCoord2 = graphRect.height + graphRect.y - 80;

    /**
     * Y coordinate of the 3 value.
     */
    private int yCoord3 = graphRect.height + graphRect.y - 90;

    /**
     * Y coordinate of the 6 value.
     */
    private int yCoord6 = graphRect.height + graphRect.y - 110;

    /**
     * Y coordinate of the 10 value.
     */
    private int yCoordTop = graphRect.y;

    private WeatherPlotDataManager dataMgr;

    private double top;

    private double bottom;

    public WeatherVisCanvasComp(Composite parent, long currentTime,
            WxPlotCfg wxPlotCfg) {
        super(parent, "VISIBILITY", currentTime, wxPlotCfg);
        dataMgr = WeatherPlotDataManager.getInstance();

        // this.parent = parent; // Not used yet

    }

    /**
     * Draw the Visibility graph data on the canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    @Override
    public void drawCanvasData(GC gc) {
        // need to check for changes in labels for graph and reset them if
        // necessary
        WxPlotCfg config = dataMgr.getWxPlotCfg();
        top = config.getVisTop();
        bottom = config.getVisBottom();
        yCoordBottom = graphRect.height + graphRect.y;
        yCoord1_2 = graphRect.height + graphRect.y - 40;
        yCoord1 = graphRect.height + graphRect.y - 60;
        yCoord2 = graphRect.height + graphRect.y - 80;
        yCoord3 = graphRect.height + graphRect.y - 90;
        yCoord6 = graphRect.height + graphRect.y - 110;
        String visTop = String.format("% 3.0f", top);
        double x = 1 / bottom;
        String visBottom = String.format("1/%1.0f", x);

        int fontHeight = gc.getFontMetrics().getHeight();
        int halfFontHeight = fontHeight / 2;

        gc.setLineStyle(SWT.LINE_DOT);

        // bottom label & line
        gc.drawText(visBottom, graphXCoord - 25, yCoordBottom - halfFontHeight,
                true);
        gc.drawText(visBottom, graphXCoord + graphRect.width + 5, yCoordBottom
                - halfFontHeight, true);

        // 1/2 label & line
        gc.drawText("1/2", graphXCoord - 25, yCoord1_2 - halfFontHeight, true);
        gc.drawText("1/2", graphXCoord + (int) (graphRect.width * .25),
                yCoord1_2 - halfFontHeight, true);
        gc.drawText("1/2", graphXCoord + (int) (graphRect.width * .5),
                yCoord1_2 - halfFontHeight, true);
        gc.drawText("1/2", graphXCoord + (int) (graphRect.width * .75),
                yCoord1_2 - halfFontHeight, true);
        gc.drawText("1/2", graphXCoord + graphRect.width + 5, yCoord1_2
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord1_2, graphXCoord + graphRect.width,
                yCoord1_2);

        // 1 label & line
        gc.drawText("  1", graphXCoord - 25, yCoord1 - halfFontHeight, true);
        gc.drawText("  1", graphXCoord + (int) (graphRect.width * .25), yCoord1
                - halfFontHeight, true);
        gc.drawText("  1", graphXCoord + (int) (graphRect.width * .5), yCoord1
                - halfFontHeight, true);
        gc.drawText("  1", graphXCoord + (int) (graphRect.width * .75), yCoord1
                - halfFontHeight, true);
        gc.drawText("  1", graphXCoord + graphRect.width + 5, yCoord1
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord1, graphXCoord + graphRect.width,
                yCoord1);

        // 2 label & line
        gc.drawText("  2", graphXCoord - 25, yCoord2 - halfFontHeight, true);
        gc.drawText("  2", graphXCoord + (int) (graphRect.width * .25), yCoord2
                - halfFontHeight, true);
        gc.drawText("  2", graphXCoord + (int) (graphRect.width * .5), yCoord2
                - halfFontHeight, true);
        gc.drawText("  2", graphXCoord + (int) (graphRect.width * .75), yCoord2
                - halfFontHeight, true);
        gc.drawText("  2", graphXCoord + graphRect.width + 5, yCoord2
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord2, graphXCoord + graphRect.width,
                yCoord2);

        // 3 label & line
        gc.drawText("  3", graphXCoord - 25, yCoord3 - halfFontHeight, true);
        gc.drawText("  3", graphXCoord + (int) (graphRect.width * .25), yCoord3
                - halfFontHeight, true);
        gc.drawText("  3", graphXCoord + (int) (graphRect.width * .5), yCoord3
                - halfFontHeight, true);
        gc.drawText("  3", graphXCoord + (int) (graphRect.width * .75), yCoord3
                - halfFontHeight, true);
        gc.drawText("  3", graphXCoord + graphRect.width + 5, yCoord3
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord3, graphXCoord + graphRect.width,
                yCoord3);

        // 6 label & line
        gc.drawText("  6", graphXCoord - 25, yCoord6 - halfFontHeight, true);
        gc.drawText("  6", graphXCoord + (int) (graphRect.width * .25), yCoord6
                - halfFontHeight, true);
        gc.drawText("  6", graphXCoord + (int) (graphRect.width * .5), yCoord6
                - halfFontHeight, true);
        gc.drawText("  6", graphXCoord + (int) (graphRect.width * .75), yCoord6
                - halfFontHeight, true);
        gc.drawText("  6", graphXCoord + graphRect.width + 5, yCoord6
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord6, graphXCoord + graphRect.width,
                yCoord6);

        // top label
        gc.drawText(visTop, graphXCoord - 25, yCoordTop - halfFontHeight, true);
        gc.drawText(visTop, graphXCoord + graphRect.width + 5, yCoordTop
                - halfFontHeight, true);

        // ------------------------------------------
        // Set the line style back to a solid line
        // ------------------------------------------
        gc.setLineStyle(SWT.LINE_SOLID);

        for (PlotViewerCfg viewer : plotViewers) {
            Color tmpColor = new Color(parent.getDisplay(),
                    RGBColors.getRGBColor(viewer.getColorName()));
            gc.setForeground(tmpColor);
            String className = viewer.getClassName();

            if (className.equals(PlotViewerCfg.ClassNames.TAFS.getName())
                    && viewer.getSelected()) {
                drawTaf(gc);
            } else if (className.equals(PlotViewerCfg.ClassNames.METARS
                    .getName()) && viewer.getSelected()) {
                drawMtr(gc);
            } else if (className.equals(PlotViewerCfg.ClassNames.NAM_MOS
                    .getName()) && viewer.getSelected()) {
                drawModel(gc, dataMgr.getNamMos());
            } else if (className.equals(PlotViewerCfg.ClassNames.GFS_MOS
                    .getName()) && viewer.getSelected()) {
                drawModel(gc, dataMgr.getGfsMos());
            } else if (className.equals(PlotViewerCfg.ClassNames.GFSLAMP
                    .getName()) && viewer.getSelected()) {
                drawModel(gc, dataMgr.getGfsLamp());
            } else if (className.equals(PlotViewerCfg.ClassNames.NAM_WRF
                    .getName()) && viewer.getSelected()) {
                drawModel(gc, dataMgr.getWrfNam());
            }

            tmpColor.dispose();
        }
    }

    private void drawTaf(GC gc) {
        if (dataMgr != null) {
            List<Map<String, Object>> tafs = dataMgr.getTafs();

            if (tafs != null) {
                int prevX = -9999;
                int prevY = -9999;

                for (Map<String, Object> taf : tafs) {
                    long from = (Long) taf.get("from");
                    long to = (Long) taf.get("to");
                    float vis = (Float) taf.get("vis");

                    Calendar c = Calendar.getInstance();
                    c.setTimeInMillis(currentTime);
                    c.set(Calendar.MINUTE, 0);
                    c.set(Calendar.SECOND, 0);
                    c.add(Calendar.HOUR_OF_DAY, -1 * this.hoursBack);

                    if (from < c.getTimeInMillis()) {
                        from = c.getTimeInMillis();
                    }

                    c.add(Calendar.HOUR_OF_DAY, this.totalHoursDisplayed);

                    if (to > c.getTimeInMillis()) {
                        to = c.getTimeInMillis();
                    }

                    int ycoord = -9999;

                    if (vis >= top) {
                        ycoord = yCoordTop;
                    } else if (vis >= 6) {
                        ycoord = (int) (Math.round(((top - vis) / (top - 6.0))
                                * (yCoord6 - yCoordTop)) + yCoordTop);
                    } else if (vis >= 3) {
                        ycoord = (int) (Math.round(((6.0 - vis) / (6.0 - 3.0))
                                * (yCoord3 - yCoord6)) + yCoord6);
                    } else if (vis >= 2) {
                        ycoord = (int) (Math.round(((3.0 - vis) / (3.0 - 2.0))
                                * (yCoord2 - yCoord3)) + yCoord3);
                    } else if (vis >= 1) {
                        ycoord = (int) (Math.round(((2.0 - vis) / (2.0 - 1.0))
                                * (yCoord1 - yCoord2)) + yCoord2);
                    } else if (vis >= 0.5) {
                        ycoord = (int) (Math.round(((1.0 - vis) / (1.0 - 0.5))
                                * (yCoord1_2 - yCoord1)) + yCoord1);
                    } else if (vis >= 0.125) {
                        ycoord = (int) (Math
                                .round(((0.5 - vis) / (0.5 - 0.125))
                                        * (yCoordBottom - yCoord1_2)) + yCoord1_2);
                    } else {
                        ycoord = yCoordBottom;
                    }

                    c.add(Calendar.HOUR_OF_DAY, -1 * this.totalHoursDisplayed);
                    double tdelta = (from - c.getTimeInMillis());
                    tdelta /= 3600000.0;
                    int x1 = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);
                    tdelta = (to - c.getTimeInMillis());
                    tdelta /= 3600000.0;
                    int x2 = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);

                    if (x1 > (graphXCoord + graphRect.width)
                            || x1 < graphXCoord) {
                        break;
                    }

                    if ((prevX == x1) && (prevY != ycoord)) {
                        gc.drawLine(prevX, prevY, x1, ycoord);
                    }

                    gc.drawLine(x1, ycoord, x2, ycoord);

                    prevX = x2;
                    prevY = ycoord;
                }
            }
        }
    }

    private void drawMtr(GC gc) {
        if (dataMgr != null) {
            List<Map<String, Object>> mtrs = dataMgr.getMetars();

            if (mtrs != null) {
                ListIterator<Map<String, Object>> itr = mtrs.listIterator(mtrs
                        .size());

                int prevX = -9999;
                int prevY = -9999;

                // for (Map<String, Object> mtr : mtrs) {
                while (itr.hasPrevious()) {
                    Map<String, Object> mtr = itr.previous();
                    long from = (Long) mtr.get("from");
                    float vis = (Float) mtr.get("vis");

                    int ycoord = -9999;

                    if (vis >= 10) {
                        ycoord = yCoordTop;
                    } else if (vis >= 6) {
                        ycoord = (int) (Math.round(((top - vis) / (top - 6.0))
                                * (yCoord6 - yCoordTop)) + yCoordTop);
                    } else if (vis >= 3) {
                        ycoord = (int) (Math.round(((6.0 - vis) / (6.0 - 3.0))
                                * (yCoord3 - yCoord6)) + yCoord6);
                    } else if (vis >= 2) {
                        ycoord = (int) (Math.round(((3.0 - vis) / (3.0 - 2.0))
                                * (yCoord2 - yCoord3)) + yCoord3);
                    } else if (vis >= 1) {
                        ycoord = (int) (Math.round(((2.0 - vis) / (2.0 - 1.0))
                                * (yCoord1 - yCoord2)) + yCoord2);
                    } else if (vis >= 0.5) {
                        ycoord = (int) (Math.round(((1.0 - vis) / (1.0 - 0.5))
                                * (yCoord1_2 - yCoord1)) + yCoord1);
                    } else if (vis >= 0.125) {
                        ycoord = (int) (Math
                                .round(((0.5 - vis) / (0.5 - 0.125))
                                        * (yCoordBottom - yCoord1_2)) + yCoord1_2);
                    } else {
                        ycoord = yCoordBottom;
                    }

                    Calendar c = Calendar.getInstance();
                    c.setTimeInMillis(currentTime);
                    c.set(Calendar.MINUTE, 0);
                    c.set(Calendar.SECOND, 0);
                    c.add(Calendar.HOUR_OF_DAY, -1 * this.hoursBack);
                    double tdelta = (from - c.getTimeInMillis());
                    tdelta /= 3600000.0;
                    int xcoord = (int) (Math
                            .round(tdelta * timeLineXCoordSpace) + x0);

                    if (prevX > 0 && prevY > 0) {
                        gc.drawLine(prevX, prevY, xcoord, prevY);
                        gc.drawLine(xcoord, prevY, xcoord, ycoord);
                    }

                    prevX = xcoord;
                    prevY = ycoord;
                }

                Calendar c = Calendar.getInstance();
                c.set(Calendar.HOUR_OF_DAY, this.currentHour);
                long now = c.getTimeInMillis();
                c.add(Calendar.HOUR_OF_DAY, -1 * this.hoursBack);
                double tdelta = (now - c.getTimeInMillis());
                tdelta /= 3600000.0;
                int xcoord = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);
                if (xcoord > prevX) {
                    gc.drawLine(prevX, prevY, xcoord, prevY);
                }
            }
        }
    }

    private void drawModel(GC gc, List<Map<String, Object>> data) {
        if (data != null) {
            int prevX = -9999;
            int prevY = -9999;

            for (Map<String, Object> record : data) {
                if (record == null || record.get("from") == null
                        || record.get("to") == null
                        || record.get("vis") == null) {
                    continue;
                }
                long from = (Long) record.get("from");
                long to = (Long) record.get("to");
                float vis = (Float) record.get("vis");

                Calendar c = Calendar.getInstance();
                c.setTimeInMillis(currentTime);
                c.set(Calendar.MINUTE, 0);
                c.set(Calendar.SECOND, 0);
                c.add(Calendar.HOUR_OF_DAY, -1 * this.hoursBack);

                if (from < c.getTimeInMillis()) {
                    from = c.getTimeInMillis();
                }

                c.add(Calendar.HOUR_OF_DAY, this.totalHoursDisplayed);

                if (to > c.getTimeInMillis()) {
                    to = c.getTimeInMillis();
                }

                int ycoord = -9999;

                if (vis >= 10) {
                    ycoord = yCoordTop;
                } else if (vis >= 6) {
                    ycoord = (int) (Math.round(((top - vis) / (top - 6.0))
                            * (yCoord6 - yCoordTop)) + yCoordTop);
                } else if (vis >= 3) {
                    ycoord = (int) (Math.round(((6.0 - vis) / (6.0 - 3.0))
                            * (yCoord3 - yCoord6)) + yCoord6);
                } else if (vis >= 2) {
                    ycoord = (int) (Math.round(((3.0 - vis) / (3.0 - 2.0))
                            * (yCoord2 - yCoord3)) + yCoord3);
                } else if (vis >= 1) {
                    ycoord = (int) (Math.round(((2.0 - vis) / (2.0 - 1.0))
                            * (yCoord1 - yCoord2)) + yCoord2);
                } else if (vis >= 0.5) {
                    ycoord = (int) (Math.round(((1.0 - vis) / (1.0 - 0.5))
                            * (yCoord1_2 - yCoord1)) + yCoord1);
                } else if (vis >= 0.125) {
                    ycoord = (int) (Math.round(((0.5 - vis) / (0.5 - 0.125))
                            * (yCoordBottom - yCoord1_2)) + yCoord1_2);
                } else {
                    ycoord = yCoordBottom;
                }

                c.add(Calendar.HOUR_OF_DAY, -1 * this.totalHoursDisplayed);
                double tdelta = (from - c.getTimeInMillis());
                tdelta /= 3600000.0;
                int x1 = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);
                tdelta = (to - c.getTimeInMillis());
                tdelta /= 3600000.0;
                int x2 = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);

                if ((prevX == x1) && (prevY != ycoord)) {
                    gc.drawLine(prevX, prevY, x1, ycoord);
                }

                gc.drawLine(x1, ycoord, x2, ycoord);

                prevX = x2;
                prevY = ycoord;
            }
        }
    }

    /**
     * Update and redraw the canvas.
     * 
     * @param zoomLevel
     *            Zoom level.
     */
    @Override
    public void updateAndRedraw(int zoomLevel) {
        this.zoomLevel = zoomLevel;

        calculateCoordinates();

        canvas.redraw();

    }
}
