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
 * This class draws the Ceiling graph data on the Ceiling Weather canvas.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 JUN 2008  1119        lvenable    Initial creation
 * 18 Nov 2010  6701        rferrel     Use PlotViewerCfg class name.
 * 03 Nov 2016  5060        rferrel     No longer scale {@link #top}.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WeatherCigCanvasComp extends WeatherCanvasComp {
    /**
     * Parent composite.
     */
    // private Composite parent; // Not used yet
    /**
     * Y coordinate of the bottom graph value.
     */
    private int yCoordBottom = graphRect.height + graphRect.y;

    /**
     * Y coordinate of the graph value '002'.
     */
    private int yCoord002 = graphRect.height + graphRect.y - 20;

    /**
     * Y coordinate of the graph value '006'.
     */
    private int yCoord006 = graphRect.height + graphRect.y - 55;

    /**
     * Y coordinate of the graph value '010'.
     */
    private int yCoord010 = graphRect.height + graphRect.y - 70;

    /**
     * Y coordinate of the graph value '020'.
     */
    private int yCoord020 = graphRect.height + graphRect.y - 95;

    /**
     * Y coordinate of the graph value '031'.
     */
    private int yCoord031 = graphRect.height + graphRect.y - 105;

    /**
     * Y coordinate of the top graph value.
     */
    private int yCoordTop = graphRect.y;

    private WeatherPlotDataManager dataMgr;

    /**
     * Max value displayed on the plot.
     */
    private double top;

    /**
     * Minimum value displayed on the plot.
     */
    private double bottom;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param currentHour
     *            Current hour of day.
     */
    public WeatherCigCanvasComp(Composite parent, long currentTime,
            WxPlotCfg wxPlotCfg) {
        super(parent, "CEILING", currentTime, wxPlotCfg);
        dataMgr = WeatherPlotDataManager.getInstance();
    }

    /**
     * Draw the Ceiling graph data on the canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    @Override
    public void drawCanvasData(GC gc) {
        /*
         * need to check for changes in labels for graph and reset them if
         * necessary
         */
        WxPlotCfg config = dataMgr.getWxPlotCfg();
        top = config.getCeilingTop();
        bottom = config.getCeilingBottom();
        String ceilingTop = String.format("%03.0f", top / 100);
        String ceilingBottom = String.format("%03.0f", bottom / 100);

        int fontHeight = gc.getFontMetrics().getHeight();
        int halfFontHeight = fontHeight / 2;

        gc.setLineStyle(SWT.LINE_DOT);

        // bottom label & line
        gc.drawText(ceilingBottom, graphXCoord - 25, yCoordBottom
                - halfFontHeight, true);
        gc.drawText(ceilingBottom, graphXCoord + graphRect.width + 5,
                yCoordBottom - halfFontHeight, true);

        // 002 label & line
        gc.drawText("002", graphXCoord - 25, yCoord002 - halfFontHeight, true);
        gc.drawText("002", graphXCoord + (int) (graphRect.width * .25),
                yCoord002 - halfFontHeight, true);
        gc.drawText("002", graphXCoord + (int) (graphRect.width * .5),
                yCoord002 - halfFontHeight, true);
        gc.drawText("002", graphXCoord + (int) (graphRect.width * .75),
                yCoord002 - halfFontHeight, true);
        gc.drawText("002", graphXCoord + graphRect.width + 5, yCoord002
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord002, graphXCoord + graphRect.width,
                yCoord002);

        // 006 label & line
        gc.drawText("006", graphXCoord - 25, yCoord006 - halfFontHeight, true);
        gc.drawText("006", graphXCoord + (int) (graphRect.width * .25),
                yCoord006 - halfFontHeight, true);
        gc.drawText("006", graphXCoord + (int) (graphRect.width * .5),
                yCoord006 - halfFontHeight, true);
        gc.drawText("006", graphXCoord + (int) (graphRect.width * .75),
                yCoord006 - halfFontHeight, true);
        gc.drawText("006", graphXCoord + graphRect.width + 5, yCoord006
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord006, graphXCoord + graphRect.width,
                yCoord006);

        // 010 label & line
        gc.drawText("010", graphXCoord - 25, yCoord010 - halfFontHeight, true);
        gc.drawText("010", graphXCoord + (int) (graphRect.width * .25),
                yCoord010 - halfFontHeight, true);
        gc.drawText("010", graphXCoord + (int) (graphRect.width * .5),
                yCoord010 - halfFontHeight, true);
        gc.drawText("010", graphXCoord + (int) (graphRect.width * .75),
                yCoord010 - halfFontHeight, true);
        gc.drawText("010", graphXCoord + graphRect.width + 5, yCoord010
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord010, graphXCoord + graphRect.width,
                yCoord010);

        // 020 label & line
        gc.drawText("020", graphXCoord - 25, yCoord020 - halfFontHeight, true);
        gc.drawText("020", graphXCoord + (int) (graphRect.width * .25),
                yCoord020 - halfFontHeight, true);
        gc.drawText("020", graphXCoord + (int) (graphRect.width * .5),
                yCoord020 - halfFontHeight, true);
        gc.drawText("020", graphXCoord + (int) (graphRect.width * .75),
                yCoord020 - halfFontHeight, true);
        gc.drawText("020", graphXCoord + graphRect.width + 5, yCoord020
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord020, graphXCoord + graphRect.width,
                yCoord020);

        // 031 label & line
        gc.drawText("031", graphXCoord - 25, yCoord031 - halfFontHeight, true);
        gc.drawText("031", graphXCoord + (int) (graphRect.width * .25),
                yCoord031 - halfFontHeight, true);
        gc.drawText("031", graphXCoord + (int) (graphRect.width * .5),
                yCoord031 - halfFontHeight, true);
        gc.drawText("031", graphXCoord + (int) (graphRect.width * .75),
                yCoord031 - halfFontHeight, true);
        gc.drawText("031", graphXCoord + graphRect.width + 5, yCoord031
                - halfFontHeight, true);
        gc.drawLine(graphXCoord, yCoord031, graphXCoord + graphRect.width,
                yCoord031);

        // top label
        gc.drawText(ceilingTop, graphXCoord - 25, yCoordTop - halfFontHeight,
                true);
        gc.drawText(ceilingTop, graphXCoord + graphRect.width + 5, yCoordTop
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
                    int cig = (Integer) taf.get("cig");

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

                    if (cig >= top) {
                        ycoord = yCoordTop;
                    } else if (cig >= 3100) {
                        ycoord = (int) (Math
                                .round(((top - cig) / (top - 3100.0))
                                        * (yCoord031 - yCoordTop)) + yCoordTop);
                    } else if (cig >= 2000) {
                        ycoord = (int) (Math
                                .round(((3100.0 - cig) / (3100.0 - 2000.0))
                                        * (yCoord020 - yCoord031)) + yCoord031);
                    } else if (cig >= 1000) {
                        ycoord = (int) (Math
                                .round(((2000.0 - cig) / (2000.0 - 1000.0))
                                        * (yCoord010 - yCoord020)) + yCoord020);
                    } else if (cig >= 600) {
                        ycoord = (int) (Math
                                .round(((1000.0 - cig) / (1000.0 - 600.0))
                                        * (yCoord006 - yCoord010)) + yCoord010);
                    } else if (cig >= 200) {
                        ycoord = (int) (Math
                                .round(((600.0 - cig) / (600.0 - 200.0))
                                        * (yCoord002 - yCoord006)) + yCoord006);
                    } else if (cig >= 100) {
                        ycoord = (int) (Math
                                .round(((200.0 - cig) / (200.0 - 100.0))
                                        * (yCoordBottom - yCoord002)) + yCoord002);
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
                    int cig = (Integer) mtr.get("cig");

                    int ycoord = -9999;

                    if (cig >= top) {
                        ycoord = yCoordTop;
                    } else if (cig >= 3100) {
                        ycoord = (int) (Math
                                .round(((top - cig) / (top - 3100.0))
                                        * (yCoord031 - yCoordTop)) + yCoordTop);
                    } else if (cig >= 2000) {
                        ycoord = (int) (Math
                                .round(((3100.0 - cig) / (3100.0 - 2000.0))
                                        * (yCoord020 - yCoord031)) + yCoord031);
                    } else if (cig >= 1000) {
                        ycoord = (int) (Math
                                .round(((2000.0 - cig) / (2000.0 - 1000.0))
                                        * (yCoord010 - yCoord020)) + yCoord020);
                    } else if (cig >= 600) {
                        ycoord = (int) (Math
                                .round(((1000.0 - cig) / (1000.0 - 600.0))
                                        * (yCoord006 - yCoord010)) + yCoord010);
                    } else if (cig >= 200) {
                        ycoord = (int) (Math
                                .round(((600.0 - cig) / (600.0 - 200.0))
                                        * (yCoord002 - yCoord006)) + yCoord006);
                    } else if (cig >= 100) {
                        ycoord = (int) (Math
                                .round(((200.0 - cig) / (200.0 - 100.0))
                                        * (yCoordBottom - yCoord002)) + yCoord002);
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
                        || record.get("cig") == null) {
                    continue;
                }
                long from = (Long) record.get("from");
                long to = (Long) record.get("to");
                float cig = (Float) record.get("cig");

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

                if (cig >= top) {
                    ycoord = yCoordTop;
                } else if (cig >= 3100) {
                    ycoord = (int) (Math.round(((top - cig) / (top - 3100.0))
                            * (yCoord031 - yCoordTop)) + yCoordTop);
                } else if (cig >= 2000) {
                    ycoord = (int) (Math
                            .round(((3100.0 - cig) / (3100.0 - 2000.0))
                                    * (yCoord020 - yCoord031)) + yCoord031);
                } else if (cig >= 1000) {
                    ycoord = (int) (Math
                            .round(((2000.0 - cig) / (2000.0 - 1000.0))
                                    * (yCoord010 - yCoord020)) + yCoord020);
                } else if (cig >= 600) {
                    ycoord = (int) (Math
                            .round(((1000.0 - cig) / (1000.0 - 600.0))
                                    * (yCoord006 - yCoord010)) + yCoord010);
                } else if (cig >= 200) {
                    ycoord = (int) (Math
                            .round(((600.0 - cig) / (600.0 - 200.0))
                                    * (yCoord002 - yCoord006)) + yCoord006);
                } else if (cig >= 100) {
                    ycoord = (int) (Math
                            .round(((200.0 - cig) / (200.0 - 100.0))
                                    * (yCoordBottom - yCoord002)) + yCoord002);
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
        this.layout();
    }
}
