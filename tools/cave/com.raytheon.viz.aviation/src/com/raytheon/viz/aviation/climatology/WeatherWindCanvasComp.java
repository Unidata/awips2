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
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.aviation.xml.PlotViewerCfg;
import com.raytheon.viz.aviation.xml.WxPlotCfg;

/**
 * This class draws the Wind graph data on the Wind Weather canvas.
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
public class WeatherWindCanvasComp extends WeatherCanvasComp {
    private WeatherPlotDataManager dataMgr;

    /**
     * Parent composite.
     */
    // private Composite parent; // Not used yet
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param currentHour
     *            Current hour of day.
     */
    public WeatherWindCanvasComp(Composite parent, long currentTime,
            WxPlotCfg wxPlotCfg) {
        super(parent, "WIND", currentTime, wxPlotCfg);
        dataMgr = WeatherPlotDataManager.getInstance();

        // this.parent = parent; // Not used yet
    }

    /**
     * Draw the Wind graph data on the canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    @Override
    public void drawCanvasData(GC gc) {
        // TODO Draw wind barbs here...

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

    private void drawWindBarb(GC gc, int xc, int yc, int ff, Object ddObj) {
        double dd;
        double rad = 0.5;
        double len = 60 * 0.8;
        double kt10 = 0.4;
        double wid = 0.2;
        double space = 0.1;
        double r = rad * len * zoomLevel * 0.33;

        if (ddObj instanceof java.lang.String) {
            r *= 3;
            dd = 0;
        } else {
            dd = (Integer) ddObj;

            if (dd == 0) {
                r *= 2;
            }
        }

        double x0 = xc - 0.5 * r;
        double y0 = yc - 0.5 * r;
        gc.drawOval((int) Math.round(x0), (int) Math.round(y0),
                (int) Math.round(r), (int) Math.round(r));

        if (dd == 0) {
            return;
        }

        dd = Math.toRadians(dd - 90);
        double ddbarb = dd + 1;
        double dx = Math.cos(dd) * len;
        double dy = Math.sin(dd) * len;
        x0 = xc + dx * rad * 0.18;
        y0 = yc + dy * rad * 0.18;
        double x1 = xc + dx;
        double y1 = yc + dy;
        gc.drawLine((int) Math.round(x0), (int) Math.round(y0),
                (int) Math.round(x1), (int) Math.round(y1));
        double dx1 = len * kt10 * Math.cos(ddbarb);
        double dy1 = len * kt10 * Math.sin(ddbarb);
        x0 = x1;
        y0 = y1;

        while (ff > 47.5) {
            x1 = x0 - wid * dx + dx1;
            y1 = y0 - wid * dy + dy1;
            double x2 = x0 - wid * dx;
            double y2 = y0 - wid * dy;
            gc.drawLine((int) Math.round(x0), (int) Math.round(y0),
                    (int) Math.round(x1), (int) Math.round(y1));
            gc.drawLine((int) Math.round(x1), (int) Math.round(y1),
                    (int) Math.round(x2), (int) Math.round(y2));
            x0 = x2 - space * dx;
            y0 = y2 - space * dy;
            ff -= 50;
        }

        while (ff > 7.5) {
            x1 = x0 + dx1;
            y1 = y0 + dy1;
            gc.drawLine((int) Math.round(x0), (int) Math.round(y0),
                    (int) Math.round(x1), (int) Math.round(y1));
            x0 -= space * dx;
            y0 -= space * dy;
            ff -= 10;
        }

        if (ff > 2.5) {
            x1 = x0 + 0.5 * dx1;
            y1 = y0 + 0.5 * dy1;
            gc.drawLine((int) Math.round(x0), (int) Math.round(y0),
                    (int) Math.round(x1), (int) Math.round(y1));
        }
    }

    private void drawTaf(GC gc) {
        if (dataMgr != null) {
            List<Map<String, Object>> tafs = dataMgr.getTafs();

            if (tafs != null) {
                for (Map<String, Object> taf : tafs) {
                    long from = (Long) taf.get("from");
                    int ff = (Integer) taf.get("ff");
                    Object ddObj = taf.get("dd");

                    int yc = CANVAS_HEIGHT / 2;

                    Calendar c = Calendar.getInstance();
                    c.setTimeInMillis(currentTime);
                    c.set(Calendar.MINUTE, 0);
                    c.set(Calendar.SECOND, 0);
                    c.add(Calendar.HOUR_OF_DAY, -1 * this.hoursBack);
                    double tdelta = (from - c.getTimeInMillis());
                    tdelta /= 3600000.0;
                    int xc = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);

                    if (xc > (graphXCoord + graphRect.width)
                            || xc < graphXCoord) {
                        break;
                    }
                    drawWindBarb(gc, xc, yc, ff, ddObj);
                }
            }
        }
    }

    private void drawMtr(GC gc) {
        if (dataMgr != null) {
            List<Map<String, Object>> mtrs = dataMgr.getMetars();

            if (mtrs != null) {
                for (Map<String, Object> mtr : mtrs) {
                    long from = (Long) mtr.get("from");
                    Object ffObj = mtr.get("ff");
                    int ff;

                    if (ffObj == null) {
                        continue;
                    } else {
                        ff = (Integer) ffObj;
                    }

                    Object ddObj = mtr.get("dd");

                    int yc = CANVAS_HEIGHT / 2;

                    Calendar c = Calendar.getInstance();
                    c.setTimeInMillis(currentTime);
                    c.set(Calendar.MINUTE, 0);
                    c.set(Calendar.SECOND, 0);
                    c.add(Calendar.HOUR_OF_DAY, -1 * this.hoursBack);
                    double tdelta = (from - c.getTimeInMillis());
                    tdelta /= 3600000.0;
                    int xc = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);

                    drawWindBarb(gc, xc, yc, ff, ddObj);
                }
            }
        }
    }

    private void drawModel(GC gc, List<Map<String, Object>> data) {
        if (data != null) {

            for (Map<String, Object> record : data) {
                if (record == null || record.get("from") == null
                        || record.get("ff") == null || record.get("dd") == null) {
                    continue;
                }
                long from = (Long) record.get("from");
                int ff = (Integer) record.get("ff");
                Object ddObj = record.get("dd");

                int yc = CANVAS_HEIGHT / 2;

                Calendar c = Calendar.getInstance();
                c.setTimeInMillis(currentTime);
                c.set(Calendar.MINUTE, 0);
                c.set(Calendar.SECOND, 0);
                c.add(Calendar.HOUR_OF_DAY, -1 * this.hoursBack);
                double tdelta = (from - c.getTimeInMillis());
                tdelta /= 3600000.0;
                int xc = (int) (Math.round(tdelta * timeLineXCoordSpace) + x0);

                drawWindBarb(gc, xc, yc, ff, ddObj);
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
