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
package com.raytheon.viz.skewt.ui;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.SoundingLayer.DATA_TYPE;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The Popup Skew T dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class PopupSkewTDialog extends CaveSWTDialog {
    private static final float[] PRESSURES = { 100, 200, 300, 400, 500, 600,
            700, 800, 900, 1000 };

    private static final float[] TEMPS = { -70, -60, -50, -40, -30, -20, -10,
            0, 10, 20, 30, 40 };

    private static final UnitConverter CtoF = SI.CELSIUS
            .getConverterTo(NonSI.FAHRENHEIT);

    private static final UnitConverter KtoC = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    private static final UnitConverter MtoFT = SI.METER
            .getConverterTo(NonSI.FOOT);

    private Display display;

    private WGraphics world;

    private Composite top;

    private Canvas canvas;

    private Label label;

    private VerticalSounding sounding;

    private Point height;

    private static float[] muParcelTrajectoryPressures = new float[20];
    static {
        int i = 0;
        for (float p = 1000; p >= 50; p -= 50, ++i) {
            muParcelTrajectoryPressures[i] = p;
        }
    }

    private float[] muParcelTrajectory;

    public PopupSkewTDialog(Shell parentShell) {
        super(parentShell, SWT.TITLE | SWT.RESIZE | SWT.MODELESS,
                CAVE.DO_NOT_BLOCK);

        // this.parentShell = parentShell;

        display = parentShell.getDisplay();

        Rectangle rectangle = new Rectangle(30, 20, 380, 240);
        world = new WGraphics(rectangle);

        Coordinate lowerLeft = WxMath.getSkewTXY(
                PRESSURES[PRESSURES.length - 1], TEMPS[0]);
        Coordinate upperRight = WxMath.getSkewTXY(PRESSURES[0],
                TEMPS[TEMPS.length - 1]);
        world.setWorldCoordinates(lowerLeft.x, upperRight.y, upperRight.x,
                lowerLeft.y);

        // dummy data for testing
        // VerticalSounding dummySounding = new VerticalSounding();
        // dummySounding.setName("TEST");
        // for (float p = 1000; p >= 50; p -= 50) {
        // SoundingLayer layer = new SoundingLayer();
        // layer.setPressure(p);
        // layer.setGeoHeight(Controller.ptozsa(p));
        // float tt = (float) WxMath.poisson(1000, p, 293.15);
        // float td = tt - 10f;
        // layer.setTemperature(tt);
        // layer.setDewpoint(td);
        // dummySounding.addLayer(layer);
        // }
        //
        // setSounding(dummySounding);
        // SoundingLayer layer = dummySounding.getLayer(500f);
        // setHeight(layer.getPressure(),
        // (float) KtoC.convert(layer.getTemperature()));
    }

    public void plotHeight(float height, float temp) {
        if (sounding != null && sounding.size() > 1) {
            float pres = 0, temperature;
            if (height > 10) {
                int i1 = sounding.size() - 2;
                int i2 = sounding.size() - 1;
                for (int i = 0; i < sounding.size() - 1; i++) {
                    if (height < sounding.get(i).getGeoHeight()
                            || height >= sounding.get(i + 1).getGeoHeight()) {
                        continue;
                    }
                    i1 = i;
                    i2 = i + 1;
                    break;
                }

                float lyrDz = sounding.get(i2).getGeoHeight()
                        - sounding.get(i1).getGeoHeight();
                float dz = height - sounding.get(i1).getGeoHeight();
                pres = sounding.get(i1).getPressure()
                        + (sounding.get(i2).getPressure() - sounding.get(i1)
                                .getPressure()) * dz / lyrDz;
                if (temp > 1e36) {
                    temperature = (sounding.get(i1).getTemperature() - 273.15f)
                            + (sounding.get(i2).getTemperature() - sounding
                                    .get(i1).getTemperature()) * dz / lyrDz;
                } else {
                    temperature = temp;
                }
            } else {
                pres = 1000;
                temperature = 0;
            }
            setHeight(pres, temperature);
        }
    }

    /**
     * @param pressure
     *            in millibars
     * @param temperature
     *            in Celcius
     */
    public void setHeight(float pressure, float temperature) {
        height = PTtoXY(pressure, temperature);
        if (canvas != null && !canvas.isDisposed()) {
            canvas.redraw();
        }
    }

    public void setSounding(VerticalSounding sounding) {
        this.sounding = sounding;
        if (this.sounding != null && this.sounding.size() > 0) {
            muParcelTrajectory = derivemuParcelTrajectory(sounding);

            if (label != null && !label.isDisposed()) {
                label.setText(sounding.getName());
            }
        } else if (label != null) {
            if (sounding != null) {
                label.setText("Not enough data");
            } else {
                label.setText("No data...");
            }
        }

        if (canvas != null && !canvas.isDisposed()) {
            canvas.redraw();
        }
    }

    /**
     * @param sounding
     */
    public static float[] derivemuParcelTrajectory(VerticalSounding soundingData) {
        float[] muParcelTrajectory = new float[muParcelTrajectoryPressures.length];
        float thetae, maxthetae = -999.0f;
        float etpar, tp;
        for (int i = 0; i < soundingData.size(); i++) {
            float tt = soundingData.get(i).getTemperature();
            float td = soundingData.get(i).getDewpoint();
            float p = soundingData.get(i).getPressure();
            thetae = Controller.ept(tt, td, p);
            if (thetae > maxthetae && soundingData.get(i).getPressure() > 500)
                maxthetae = thetae;
        }
        for (int i = 0; i < muParcelTrajectoryPressures.length; ++i) {
            float p = muParcelTrajectoryPressures[i];
            etpar = (float) (maxthetae * (Math.pow(p / 1000.0f, 0.286f)));
            tp = Controller.temp_of_te(etpar, p);
            muParcelTrajectory[20 - (int) (p / 50)] = tp;
        }
        return muParcelTrajectory;
    }

    /**
     * Map a pressure,temperature value to an x,y coordinate
     * 
     * @param pressure
     * @param temperature
     * @return x,y coordinate
     */
    protected Point PTtoXY(double pressure, double temperature) {
        Coordinate xy = world.map(WxMath.getSkewTXY(pressure, temperature));

        return new Point((int) Math.round(xy.x), (int) Math.round(xy.y));
    }

    protected void paint(PaintEvent e) {
        GC gc = e.gc;

        // paint background
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
        Rectangle r = canvas.getClientArea();
        gc.fillRectangle(r);

        // paint pressure lines
        gc.setForeground(display.getSystemColor(SWT.COLOR_CYAN));
        gc.setLineWidth(2);
        double minTemp = TEMPS[0];
        double maxTemp = TEMPS[TEMPS.length - 1];
        for (float p : PRESSURES) {
            Point p0 = PTtoXY(p, minTemp);
            Point p1 = PTtoXY(p, maxTemp);

            gc.drawLine(p0.x, p0.y, p1.x, p1.y);
        }

        // paint temperature lines
        double minPress = PRESSURES[0];
        double maxPress = PRESSURES[PRESSURES.length - 1];
        for (float t : TEMPS) {
            Point p0 = PTtoXY(minPress, t);
            Point p1 = PTtoXY(maxPress, t);

            gc.drawLine(p0.x, p0.y, p1.x, p1.y);

            String s = String.format("%.0f ", t);
            Point te = gc.textExtent(s);
            te.x /= 2;
            gc.drawText(s, p1.x - te.x, p1.y + 2, true);

            s = String.format("%.0f ", CtoF.convert(t));
            te = gc.textExtent(s);
            te.x /= 2;
            gc.drawText(s, p1.x - te.x, p1.y + te.y + 1, true);

        }

        // label heights
        for (float p : PRESSURES) {
            Point p0 = PTtoXY(p, minTemp);

            String s;
            SoundingLayer layer;

            double h = 0;
            Float gh = null;
            if (sounding != null) {
                layer = sounding.getLayer(p);
                if (layer != null) {
                    gh = layer.getGeoHeight();
                } else {
                    gh = sounding.interpolateValue(p, DATA_TYPE.GEO_HEIGHT);
                }
                h = MtoFT.convert(gh);
            }

            // Threshold for displaying height
            if (gh == null || h > 50000) {
                s = String.format("%.0f  ", p);
            } else {
                s = String.format("%.0f %.0f  ", h, p);
            }

            Point te = gc.textExtent(s);
            te.y /= 2;
            gc.drawText(s, p0.x - te.x, p0.y - te.y, true);
        }

        if (sounding != null) {
            // paint T line
            gc.setForeground(display.getSystemColor(SWT.COLOR_RED));

            Point p0 = null;
            for (SoundingLayer layer : sounding) {
                float T = layer.getTemperature();
                if (T != SoundingLayer.MISSING && T != SoundingLayer.NODATA) {
                    Point p1 = PTtoXY(layer.getPressure(),
                            KtoC.convert(layer.getTemperature()));
                    if (p0 != null) {
                        gc.drawLine(p0.x, p0.y, p1.x, p1.y);
                    }
                    p0 = p1;
                }
            }

            // paint Td line
            gc.setForeground(display.getSystemColor(SWT.COLOR_GREEN));
            p0 = null;
            for (SoundingLayer layer : sounding) {
                float D = layer.getDewpoint();
                if (D != SoundingLayer.MISSING && D != SoundingLayer.NODATA) {
                    Point p1 = PTtoXY(layer.getPressure(),
                            KtoC.convert(layer.getDewpoint()));
                    if (p0 != null) {
                        gc.drawLine(p0.x, p0.y, p1.x, p1.y);
                    }
                    p0 = p1;
                }
            }

            // paint theta e line
            gc.setForeground(display.getSystemColor(SWT.COLOR_DARK_MAGENTA));
            p0 = null;
            for (int i = 0; i < muParcelTrajectoryPressures.length - 1; ++i) {
                float p = muParcelTrajectoryPressures[i];
                Point p1 = PTtoXY(p, KtoC.convert(muParcelTrajectory[i]));
                if (p0 != null) {
                    gc.drawLine(p0.x, p0.y, p1.x, p1.y);
                }
                p0 = p1;
            }
        }

        if (height != null) {
            // paint height indicator
            gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
            gc.setBackground(display.getSystemColor(SWT.COLOR_YELLOW));
            gc.setLineWidth(1);
            gc.fillOval(height.x - 4, height.y - 4, 8, 8);
            gc.drawOval(height.x - 4, height.y - 4, 8, 8);
        }

    }

    @Override
    protected void initializeComponents(Shell shell) {
        top = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout(1, true);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        top.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        top.setLayoutData(layoutData);

        canvas = new Canvas(top, SWT.NONE);
        layoutData.minimumWidth = 301;
        layoutData.minimumHeight = 301;
        canvas.setLayoutData(layoutData);
        canvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                paint(e);
            }
        });

        label = new Label(top, SWT.CENTER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        label.setLayoutData(layoutData);

        if (sounding != null) {
            label.setText(sounding.getName());
        }

        shell.setText("Skew T");

        shell.setLocation(0, 0);
    }
}
