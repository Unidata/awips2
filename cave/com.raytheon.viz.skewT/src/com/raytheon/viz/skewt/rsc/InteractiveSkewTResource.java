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
package com.raytheon.viz.skewt.rsc;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.edex.util.Equations;
import com.raytheon.edex.util.UAPoint;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.sounding.ParcelLift.PARCEL_TYPE;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.SoundingLayer.DATA_TYPE;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.sounding.SoundingParams;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.skewt.ui.SkewTConstants;
import com.raytheon.viz.skewt.ui.SkewtControlsDialog;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * SkewT Interactive resource to perform edit functions on soundings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class InteractiveSkewTResource extends SkewTResource implements
        IContextMenuContributor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(InteractiveSkewTResource.class);

    Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

    protected IFont smallFont;

    private VerticalSounding sounding;

    private static enum editMode {
        HODO, SKEWT, TEMPCHG
    };

    private static editMode emode;

    private static final UnitConverter kelvinToCelsius = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    private static final UnitConverter celciusToFahrenheit = SI.CELSIUS
            .getConverterTo(NonSI.FAHRENHEIT);

    private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private static final UnitConverter celciusToKelvin = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    private static final UnitConverter metersToFeet = SI.METER
            .getConverterTo(NonSI.FOOT);

    private Hashtable<Coordinate, SoundingLayer> windMap = new Hashtable<Coordinate, SoundingLayer>();

    private Hashtable<DataTime, VerticalSounding> soundMap = new Hashtable<DataTime, VerticalSounding>();

    private Coordinate ep;

    private SoundingLayer sl = null;

    private boolean dpt = false;

    private boolean wb = false;

    private SkewTResource skwt;

    private boolean _plotStorm = false;

    private static PARCEL_TYPE ptype = PARCEL_TYPE.SURFACE;

    private SoundingParams sp;

    private boolean ispoint = false;

    private PaintProperties paintProp;

    private static class EditableAction extends AbstractRightClickAction {

        public EditableAction() {
            super("Editable", IAction.AS_CHECK_BOX);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            AbstractVizResource<?, ?> avr = getSelectedRsc();
            if (avr instanceof InteractiveSkewTResource) {
                InteractiveSkewTResource rsc = (InteractiveSkewTResource) avr;
                rsc.setEditable(!rsc.isEditable());

                this.setChecked(rsc.isEditable());
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.ui.cmenu.AbstractRightClickAction#setSelectedRsc(com
         * .raytheon.viz.core.rsc.IVizResource)
         */
        @Override
        public void setSelectedRsc(ResourcePair selectedRsc) {
            super.setSelectedRsc(selectedRsc);
            AbstractVizResource<?, ?> avr = getSelectedRsc();
            if (avr instanceof InteractiveSkewTResource) {
                InteractiveSkewTResource rsc = (InteractiveSkewTResource) avr;
                this.setChecked(rsc.isEditable());
            }
        }

    }

    public InteractiveSkewTResource(SkewTResource rsc) {
        super(null, null);
        skwt = rsc;

        try {
            for (VerticalSounding vs : rsc.getResourceData().getSoundings()) {
                sounding = (VerticalSounding) vs.clone();
                sounding.setStationId("Interactive");
                addSounding(vs.getDataTime(), sounding);
                soundMap.put(vs.getDataTime(), sounding);
            }
        } catch (CloneNotSupportedException e) {
            // Shouldn't happen
            statusHandler.handle(Priority.PROBLEM,
                    "Exception creating InteractiveSkewTResource", e);
        }
        sp = getSoundingParameters();
    }

    public boolean isEditable() {
        return editable;
    }

    /**
     * @return the wb
     */
    public boolean isWb() {
        return wb;
    }

    /**
     * @param wb
     *            the wb to set
     */
    public void setWb(boolean wb) {
        this.wb = wb;
    }

    public void setEditable(boolean editable) {
        this.editable = editable;
        SkewtControlsDialog sd = SkewtControlsDialog.getInstance(shell,
                this.getName());
        if (editable == true) {
            skwt.editable = true;
            sd.open();
        } else {
            setWb(false);
            set_plotStorm(false);
            skwt.editable = false;
            sd.close();
        }
        this.issueRefresh();
    }

    // @Override
    // public void middleClicked() throws VizException {
    //
    // if (editable) {
    // setEditable(false);
    // if (PopupSkewTDialog.isOpen(shell)) {
    // PopupSkewTDialog.getDialog(shell).close();
    // }
    // } else {
    // setEditable(true);
    // PopupSkewTDialog.getDialog(shell).open();
    // }
    // }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.rsc.SkewTResource#getName()
     */
    @Override
    public String getName() {
        return "Interactive Skew-T";
    }

    /**
     * @return the ptype
     */
    public PARCEL_TYPE getPtype() {
        return ptype;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        EditableAction editableAction = new EditableAction();
        ResourcePair rp = new ResourcePair();
        rp.setLoadProperties(getLoadProperties());
        rp.setProperties(descriptor.getResourceList().getProperties(this));
        rp.setResource(this);
        rp.setResourceData(resourceData);
        editableAction.setSelectedRsc(rp);
        menuManager.add(editableAction);
        if (isEditable() == true && ispoint == true) {
            menuManager.add(new DeleteVertex());
        } else if (isEditable() == true && ispoint == false) {
            menuManager.add(new AddVertex());
        }
    }

    /**
     * @return
     */
    private class DeleteVertex extends AbstractRightClickAction {
        public DeleteVertex() {
            super("Delete Vertex");
        }

        @Override
        public void run() {
            removeLayer(ep);
        }
    }

    /**
     * @return
     */
    private class AddVertex extends AbstractRightClickAction {
        public AddVertex() {
            super("Add Vertex");
        }

        @Override
        public void run() {
            double pressure = ep.y;
            double dewpoint = SoundingLayer.MISSING;
            double temp = SoundingLayer.MISSING;

            if (dpt == true) {
                dewpoint = ep.x;
                temp = sounding.interpolateValue((float) pressure,
                        DATA_TYPE.TEMPERATURE);
            } else {
                temp = ep.x;
                dewpoint = sounding.interpolateValue((float) pressure,
                        DATA_TYPE.DEWPOINT);
            }
            addSkewtPointAction(pressure, temp, dewpoint);
        }
    }

    @Override
    protected void drawHodo(IGraphicsTarget target, double zoomLevel,
            WGraphics world, SoundingParams sp) throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        int width = getCapability(OutlineCapability.class).getOutlineWidth();
        LineStyle lineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();

        PixelExtent extent = new PixelExtent(getDisplay().getHodoBackground()
                .getRectangle());
        target.setupClippingPlane(extent);

        windMap.clear();
        Coordinate c0 = null;
        Coordinate c1;
        for (SoundingLayer layer : sp.getInterleavedData()) {
            double pressure = layer.getPressure();
            if (pressure < SoundingLayer.MISSING && pressure < 100) {
                continue;
            }
            float u = layer.getWindU();
            float v = layer.getWindV();
            if (u >= SoundingLayer.MISSING || v >= SoundingLayer.MISSING) {
                continue;
            }

            c1 = new Coordinate(u, v);
            windMap.put(c1, layer);
            if (c0 != null) {
                target.drawLine(world.mapX(c0.x), world.mapY(c0.y), 0,
                        world.mapX(c1.x), world.mapY(c1.y), 0, color, width,
                        lineStyle);

            }
            if (editable) {
                drawEditHandle(target, world, zoomLevel, world.mapX(c1.x),
                        world.mapY(c1.y), color);
            }
            c0 = c1;
        }

        target.clearClippingPlane();
    }

    @Override
    protected void disposeInternal() {
        if (smallFont != null) {
            smallFont.dispose();
            smallFont = null;
        }
        if (normalFont != null) {
            normalFont.dispose();
            normalFont = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.skewt.rsc.SkewTResource#paintInternal(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        paintProp = paintProps;
        sounding = soundMap.get(paintProps.getDataTime());
        double zoomLevel = paintProps.getZoomLevel();
        WGraphics skewTWorld = getSkewTWorld();
        WGraphics hodoWorld = getHodoWorld();
        drawParcelLine(target, zoomLevel, skewTWorld);
        drawWetBulb(target, skewTWorld);
        plotStormInflow(target, hodoWorld, zoomLevel);
    }

    /**
     * This will return the x,y from the sounding that is nearest to the
     * selected screen point
     * 
     * @param c
     * @return Coordinate from sounding based on selected point.
     */
    public Coordinate getSelectedPoint(Coordinate c) {

        ep = null;
        float p_mb = 0;
        sl = null;
        if (this.getDisplay().getHodoBackground().contains(c)) {
            c = this.getDisplay().getHodoBackground().getWorld()
                    .unMap(c.x, c.y);
            Coordinate cc = getWindfromSounding(c);
            sl = windMap.get(cc);
            p_mb = sl.getPressure();
            float u_ms = sounding.interpolateValue(p_mb, DATA_TYPE.WIND_U);
            float v_ms = sounding.interpolateValue(p_mb, DATA_TYPE.WIND_V);
            ep = WxMath.speedDir(u_ms, v_ms);
            emode = editMode.HODO;
            return ep;
        } else if (this.getDisplay().getSkewTBackground().contains(c)) {
            c = WxMath.reverseSkewTXY(getSkewTWorld().unMap(c.x, c.y));
            emode = editMode.SKEWT;
        } else if (getDisplay().getTempChangeBackground().contains(c)) {
            c = getTempChangeWorld().unMap(c.x, c.y);
            c.y = WxMath.reverseSkewTXY(c).y;
            emode = editMode.TEMPCHG;
        }
        ep = c;
        p_mb = (float) c.y;
        sl = sounding.getLayerNearest(p_mb);
        p_mb = sl.getPressure();

        // Check if editing Temp or Dewpoint
        ep.y = p_mb;
        checkDwpt(c);
        return ep;
    }

    /**
     * @return the ep
     */
    public Coordinate getEp() {
        return ep;
    }

    /**
     * Checks to see if point is a dewpoint or temperature
     * 
     * @param Coordinate
     *            c Coordinate of point
     */
    private void checkDwpt(Coordinate c) {
        float p_mb = (float) c.y;
        c.x = celciusToKelvin.convert(c.x);
        SoundingLayer sle = sounding.getLayerNearest(p_mb);
        p_mb = sle.getPressure();
        double zoomLevel = paintProp.getZoomLevel();
        double t = sle.getTemperature();
        double td = sle.getDewpoint();
        double tb = t - zoomLevel;
        double tt = t + zoomLevel;
        double tdb = td - zoomLevel;
        double tdt = td + zoomLevel;
        if (c.x >= tb && c.x <= tt) {
            ep.x = t;
            dpt = false;
        } else if (c.x >= tdb && c.x <= tdt) {
            ep.x = td;
            dpt = true;
        }
    }

    /**
     * This will return the x,y from the sounding that is nearest to the
     * selected screen point
     * 
     * @param c
     * @return Coordinate from sounding based on selected point.
     */
    public void moveMousePoint(Coordinate c) {

        switch (emode) {
        case HODO:
            c = this.getDisplay().getHodoBackground().getWorld()
                    .unMap(c.x, c.y);
            sl.setWindU((float) c.x);
            sl.setWindV((float) c.y);
            c = WxMath.speedDir((float) c.x, (float) c.y);
            sl.setWindSpeed((float) c.x);
            sl.setWindDirection((float) c.y);
            sp.recomputeSoundingParams(sounding);
            fireListeners();
            break;
        case SKEWT:
            c = WxMath.reverseSkewTXY(getSkewTWorld().unMap(c.x, c.y));
            double ctemp = celciusToKelvin.convert(c.x);
            if (dpt == false) {
                sl.setTemperature((float) ctemp);
            } else {
                if (ctemp >= sl.getTemperature()) {
                    sl.setDewpoint(sl.getTemperature());
                } else {
                    sl.setDewpoint((float) ctemp);
                }
            }
            sp.recomputeSoundingParams(sounding);
            fireListeners();
            break;
        case TEMPCHG:
            c = getTempChangeWorld().unMap(c.x, c.y);
            c.y = WxMath.reverseSkewTXY(c).y;
            break;
        }
    }

    /**
     * Draw the parcel specific lines from control panel.
     * 
     * @param target
     * @param world
     * @param zoomLevel
     * @param moistPoints
     * @throws VizException
     */
    protected void drawParcelLine(IGraphicsTarget target, double zoomLevel,
            WGraphics world) throws VizException {
        PixelExtent extent = new PixelExtent(getDisplay().getSkewTBackground()
                .getRectangle());
        target.setupClippingPlane(extent);

        if (getSoundingParameters().liftedI() != SoundingLayer.MISSING) {
            drawMoist(target, world, zoomLevel,
                    createParcelSaturatedCoordinates());
            drawDry(target, world);
        }
        target.clearClippingPlane();
    }

    /**
     * Draws the moist portion of the adiabatic lifting line.
     * 
     * @param target
     * @param world
     * @param zoomLevel
     * @param moistPoints
     * @throws VizException
     */
    protected void drawMoist(IGraphicsTarget target, WGraphics world,
            double zoomLevel, List<UAPoint> moistPoints) throws VizException {

        if (moistPoints != null && moistPoints.size() > 0) {

            Coordinate coor1 = WxMath.getSkewTXY(sp.pressLCL(),
                    kelvinToCelsius.convert(sp.tempLCL()));
            // draw the LCL circle
            target.drawFilledCircle(world.mapX(coor1.x), world.mapY(coor1.y),
                    0.0, SkewTConstants.endpointRadius * zoomLevel,
                    SkewTConstants.parcelColor);

            for (int i = 0; i < moistPoints.size(); i++) {
                UAPoint p = moistPoints.get(i);
                if (p.pressure != SoundingLayer.MISSING
                        && p.temperature != SoundingLayer.MISSING) {
                    Coordinate coor2 = WxMath.getSkewTXY(p.pressure,
                            kelvinToCelsius.convert(p.temperature));
                    if (coor1 != null) {
                        target.drawLine(world.mapX(coor1.x),
                                world.mapY(coor1.y), 0.0, world.mapX(coor2.x),
                                world.mapY(coor2.y), 0.0,
                                SkewTConstants.parcelColor,
                                SkewTConstants.parcelLineWidth,
                                IGraphicsTarget.LineStyle.SOLID);
                    }
                    coor1 = coor2;
                }
            }
        }
    }

    /**
     * Draws the dry adiabatic portion of the lifting line.
     * 
     * @param moistPoints
     * @throws VizException
     */
    protected void drawDry(IGraphicsTarget target, WGraphics world)
            throws VizException {

        float pressures[] = sp.getPressures();
        float temps[] = sp.getTemperatures();

        if (pressures[0] != SoundingLayer.MISSING
                && temps[0] != SoundingLayer.MISSING
                && sp.pressLCL() != SoundingLayer.MISSING
                && sp.tempLCL() != SoundingLayer.MISSING) {
            Coordinate sfc = WxMath.getSkewTXY(pressures[0],
                    kelvinToCelsius.convert(temps[0]));

            Coordinate lcl = WxMath.getSkewTXY(sp.pressLCL(),
                    kelvinToCelsius.convert(sp.tempLCL()));

            target.drawLine(world.mapX(sfc.x), world.mapY(sfc.y), 0.0,
                    world.mapX(lcl.x), world.mapY(lcl.y), 0.0,
                    SkewTConstants.parcelColor, SkewTConstants.parcelLineWidth,
                    IGraphicsTarget.LineStyle.SOLID);
        }
    }

    /**
     * Draws the WetBulb Profile line.
     * 
     * @throws VizException
     */
    protected void drawWetBulb(IGraphicsTarget target, WGraphics world)
            throws VizException {

        if (!isEditable()) {
            return;
        }
        double[] wetBulbs = sp.getWetbulbs();

        // Check to see if we are editing and that the show Wet-Bulb Profile is
        // checked.
        if (isWb() && isEditable()) {
            float[] press = sp.getPressures();
            // first coordinate
            Coordinate coor1 = null;

            // number of wetbulb points
            int n = (wetBulbs.length < press.length) ? wetBulbs.length
                    : press.length;

            for (int i = 1; i < n; i++) {
                if (press[i] != SoundingLayer.MISSING
                        && wetBulbs[i] != SoundingLayer.MISSING
                        && wetBulbs[i] < 999) {
                    Coordinate coor2 = WxMath.getSkewTXY(press[i], wetBulbs[i]);
                    if (coor1 != null) {
                        target.drawLine(world.mapX(coor1.x),
                                world.mapY(coor1.y), 0.0, world.mapX(coor2.x),
                                world.mapY(coor2.y), 0.0,
                                SkewTConstants.wetBulbColor,
                                SkewTConstants.wetBulbLineWidth,
                                IGraphicsTarget.LineStyle.SOLID);
                    }
                    coor1 = coor2;
                }
            }
        }
    }

    /**
     * Gets the saturated parcel coordinates for the lifted parcel
     * 
     * @return
     */
    protected List<UAPoint> createParcelSaturatedCoordinates() {

        if (sp.pressLCL() != SoundingLayer.MISSING
                && sp.tempLCL() != SoundingLayer.MISSING) {
            double tLCL_C = kelvinToCelsius.convert(sp.tempLCL());
            double thetaW = WxMath.thetaw(sp.pressLCL(), tLCL_C, tLCL_C);
            return Equations.saturatedAdiabats(sp.pressLCL(), sounding
                    .getMinPressureLayer().getPressure(),
                    SkewTConstants.moistAdiabaticIncrement, celciusToKelvin
                            .convert(thetaW));
        } else {
            return null;
        }

    }

    /**
     * Updates Parcel line when a value is changed.
     * 
     * Fire when ever a value changes!
     */
    public void changeParcel(PARCEL_TYPE pt, float level) {
        ptype = pt;
        if (!ptype.equals(PARCEL_TYPE.USERSELECT)) {
            sp.set_liftingMethod(ptype);
        } else {
            if (checkUserLevelValid(level)) {
                sp.set_userLevel(level);
                sp.set_liftingMethod(ptype);
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
                mb.setText("Error");
                mb.setMessage("Level must be between "
                        + sounding.getMinPressureLayer().getPressure()
                        + "mb and "
                        + sounding.getMaxPressurelayer().getPressure() + "mb.");
                mb.open();
            }
        }
        this.issueRefresh();
    }

    /**
     * @param c
     * @return
     */
    private Coordinate getWindfromSounding(Coordinate c) {
        double dist = 4;
        Coordinate eo = null;
        Coordinate ee = null;
        Iterator<Coordinate> iter = windMap.keySet().iterator();

        while (iter.hasNext()) {
            eo = iter.next();
            if (eo.distance(c) < dist) {
                dist = eo.distance(c);
                ee = eo;
            }
        }
        return ee;
    }

    /**
     * @param b
     */
    public void setFcstMax(boolean b) {
        sp.set_useFcstMax(b);
    }

    /**
     * @return boolean useFcstMax
     * 
     */
    public boolean getFcstMax() {
        return sp.is_useFcstMax();
    }

    @Override
    public SoundingParams getSoundingParameters() {
        if (sounding == null) {
            return null;
        }
        SoundingParams sp = new SoundingParams(sounding);
        return sp;
    }

    public boolean checkUserLevelValid(float level) {
        boolean valid = false;
        float maxPress = sounding.getMaxPressurelayer().getPressure();
        float minPress = sounding.getMinPressureLayer().getPressure();
        if (level <= maxPress && level >= minPress) {
            valid = true;
        }
        return valid;
    }

    /**
     * Add skewtpoint
     * 
     * @param pressure
     * @param temp
     * @param dewpoint
     */
    public void addSkewtPointAction(double pressure, double temp,
            double dewpoint) {
        SoundingLayer sl = new SoundingLayer((float) pressure,
                SoundingLayer.MISSING, (float) temp, (float) dewpoint,
                SoundingLayer.MISSING, SoundingLayer.MISSING,
                SoundingLayer.MISSING);
        sounding.addLayer(sl);
        sp.recomputeSoundingParams(sounding);
        this.issueRefresh();
    }

    /**
     * Add hodo point
     * 
     * @param pressure
     * @param windDirection
     * @param windSpeed
     */
    public void addChangePointAction(double pressure, double windSpeed,
            double windDirection) {
        SoundingLayer sl = new SoundingLayer((float) pressure,
                SoundingLayer.MISSING, SoundingLayer.MISSING,
                SoundingLayer.MISSING, (float) windSpeed,
                (float) windDirection, SoundingLayer.MISSING);
        sounding.addLayer(sl);
        sp.recomputeSoundingParams(sounding);
        this.issueRefresh();
    }

    public void resetSounding() {
        dataTimes.remove(sounding.getDataTime());
        soundingMap.remove(sounding.getDataTime().getValidTime().getTime());
        soundMap.remove(sounding.getDataTime());
        try {
            for (VerticalSounding vs : skwt.getResourceData().getSoundings()) {
                sounding = (VerticalSounding) vs.clone();
                sounding.setStationId("Interactive");
                addSounding(vs.getDataTime(), sounding);
                soundMap.put(vs.getDataTime(), sounding);
            }
        } catch (CloneNotSupportedException e) {
            // Shouldn't happen
            statusHandler.handle(Priority.PROBLEM,
                    "Exception resetting InteractiveSkewTResource", e);
        }
        this.issueRefresh();
        fireListeners();
    }

    /**
     * compute the 0-3 km storm inflow and plot the lines.
     */
    private void plotStormInflow(IGraphicsTarget target, WGraphics world,
            double zoomLevel) {

        if (isEditable() == false || _plotStorm == false
                || sp.helicity() == null) {
            return;
        }

        float elev = sp.getAnalysisData().get(0).getGeoHeight();
        ArrayList<Coordinate> hodoInflow = new ArrayList<Coordinate>();
        float uCompStorm = 0f;
        float vCompStorm = 0f;
        float[] _heights = sp.getHeights();
        float[] _uComp = sp.getWindUs();
        float[] _vComp = sp.getWindVs();
        float helHGT = sp.get_hgtHelLyr();

        for (int i = 0; i < _uComp.length; i++) {
            if (_heights[i] > helHGT + elev) {
                break;
            }
            if (_uComp[i] > 999) {
                continue;
            }
            hodoInflow.add(new Coordinate(_uComp[i], _vComp[i]));
        }
        Coordinate cc = WxMath.uvComp(sp.helicity().getStormMotionSpd(), sp
                .helicity().getStormMotionDir());
        uCompStorm = (float) cc.x;
        vCompStorm = (float) cc.y;

        // draw the Storm Motion indicator
        try {
            target.drawFilledCircle(world.mapX(uCompStorm),
                    world.mapY(vCompStorm), 0.0, SkewTConstants.endpointRadius
                            * zoomLevel, SkewTConstants.pointEditColor);
        } catch (VizException e) {
            e.printStackTrace();
        }
        // Calculate the storm inflow lines and draw every third line.
        // If the last entry in the sequence is a third multiple, draw
        // it anyway to mark the end of the inflow pattern.
        RGB color = getCapability(ColorableCapability.class).getColor();
        int index = 0;
        for (int i = 0; i < hodoInflow.size(); i++) {
            if ((i % 3) != 0) {
                if (i != hodoInflow.size() - 1) {
                    continue;
                }
            }
            try {
                target.drawLine(world.mapX(hodoInflow.get(i).x),
                        world.mapY(hodoInflow.get(i).y), 0.0,
                        world.mapX(uCompStorm), world.mapY(vCompStorm), 0.0,
                        color, SkewTConstants.parcelLineWidth,
                        IGraphicsTarget.LineStyle.DOTTED);
            } catch (VizException e) {
                e.printStackTrace();
            }

            index++;
        }
        plotHelicityContours(target, world, zoomLevel);
    }

    /**
     * compute the 0-3 km helicity contours and plot the lines.
     */
    private void plotHelicityContours(IGraphicsTarget target, WGraphics world,
            double zoomLevel) {
        float uMinExtent = 0f;
        float uMaxExtent = 0f;
        float vMinExtent = 0f;
        float vMaxExtent = 0f;
        String scratch = "";
        int srh = 0;
        int labelInterval = 0;
        float labelOffset = 0;
        int startContour = 0;
        int endContour = 0;
        int contourInterval = 0;
        float _ghx = sp.get_ghx();
        float _ghy = sp.get_ghy();
        float _helicity = sp.helicity().getHelicity();

        // Indexer to the helicity contours and labels
        RGB color = getCapability(ColorableCapability.class).getColor();

        // Get the predefined values of umin, umax, vmin, and vmax
        PixelExtent extent = new PixelExtent(getDisplay().getHodoBackground()
                .getRectangle());
        Coordinate newUVupperLeft = world.unMap(extent.getMinX(),
                extent.getMinY());
        Coordinate newUVlowerRight = world.unMap(extent.getMaxX(),
                extent.getMaxY());
        float newUmin = (float) newUVupperLeft.x;
        float newVmax = (float) newUVupperLeft.y;
        float newUmax = (float) newUVlowerRight.x;
        float newVmin = (float) newUVlowerRight.y;

        // Increase the contour interval size if at a higher zoom level;
        // Coincides with the sounding depictable threshold for zoom contouring
        if (zoomLevel < .60) {
            startContour = 50;
            endContour = 800;
            contourInterval = 25;
            labelInterval = 50;
            labelOffset = 1.5f;
        } else {
            startContour = 50;
            endContour = 800;
            contourInterval = 50;
            labelInterval = 100;
            labelOffset = 3.5f;
        }

        if (Math.abs(_ghy) > Math.abs(_ghx)) {
            for (srh = startContour; srh <= endContour; srh += contourInterval) {
                uMinExtent = newUmin;
                uMaxExtent = newUmax;
                vMinExtent = ((srh - _helicity) - (_ghx * uMinExtent)) / _ghy;
                vMaxExtent = ((srh - _helicity) - (_ghx * uMaxExtent)) / _ghy;
                // check to see if lines run off of top right edge, if so clip
                // them
                // and adjust the labels
                if (vMaxExtent > newVmax) {
                    uMaxExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmax - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMaxExtent = newVmax;
                }
                // check to see if helicity lines go below bottom left of the
                // hodo, if so clip
                // them and adjust the labels
                if (vMinExtent < newVmin) {
                    uMinExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmin - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMinExtent = newVmin;
                }
                // check to see if helicity lines go below bottom right of the
                // hodo, if so clip
                // them and adjust the labels
                if (vMaxExtent < newVmin) {
                    uMaxExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmin - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMaxExtent = newVmin;
                }
                // check to see if lines run off of top left edge, if so clip
                // them
                // and adjust the labels
                if (vMinExtent > newVmax) {
                    uMinExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmax - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMinExtent = newVmax;
                }
                if (vMaxExtent <= newVmax && uMaxExtent <= newUmax
                        && uMinExtent >= newUmin && vMaxExtent >= newVmin
                        && vMinExtent <= newVmax) {
                    try {
                        target.drawLine(world.mapX(uMinExtent),
                                world.mapY(vMinExtent), 0.0,
                                world.mapX(uMaxExtent), world.mapY(vMaxExtent),
                                0.0, color, 1, IGraphicsTarget.LineStyle.DASHED);
                        // label the contours
                        if ((srh % labelInterval) == 0) {
                            scratch = String.format("%d", srh);
                            target.drawString(smallFont, scratch,
                                    world.mapX(uMinExtent - labelOffset),
                                    world.mapY(vMinExtent), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                            target.drawString(smallFont, scratch,
                                    world.mapX(uMaxExtent + labelOffset),
                                    world.mapY(vMaxExtent), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                        }

                    } catch (VizException e) {
                        e.printStackTrace();
                    }
                }
            }
        } else {
            vMinExtent = newVmin;
            vMaxExtent = newVmax;
            for (srh = startContour; srh <= endContour; srh += contourInterval) {
                uMinExtent = ((srh - _helicity) - (_ghy * vMinExtent)) / _ghx;
                uMaxExtent = ((srh - _helicity) - (_ghy * vMaxExtent)) / _ghx;
                // check to see if lines run off of the bottom edge, if so clip
                // them
                // and adjust the labels
                if (uMinExtent < newUmin) {
                    vMaxExtent = vMinExtent
                            + ((vMaxExtent - vMinExtent) * ((uMinExtent - newUmin) / (newUmax - newUmin)));
                    uMinExtent = newUmin;
                }
                // check to see if helicity lines go above the hodo, if so clip
                // them and adjust the labels
                if (uMaxExtent > newUmax) {
                    vMinExtent = vMinExtent
                            + ((newVmax - newVmin) * ((uMaxExtent - newUmin) / (newUmax - newUmin)));
                    uMaxExtent = newUmax;
                }
                // check to see if helicity lines go left of bottom left of the
                // hodo, if so clip
                // them and adjust the labels
                if (uMaxExtent < newUmin) {
                    vMaxExtent = vMinExtent
                            + ((vMaxExtent - vMinExtent) * ((newUmin - uMinExtent) / (uMaxExtent - uMinExtent)));
                    uMaxExtent = newUmin;
                }
                // check to see if lines run off of top right edge, if so clip
                // them
                // and adjust the labels
                if (uMinExtent > newUmax) {
                    vMinExtent = vMinExtent
                            + ((vMaxExtent - vMinExtent) * ((newUmax - uMinExtent) / (uMaxExtent - uMinExtent)));
                    uMinExtent = newUmax;
                }

                if (vMaxExtent <= newVmax && uMaxExtent <= newUmax
                        && uMinExtent >= newUmin && uMinExtent <= newUmax) {
                    try {
                        target.drawLine(world.mapX(uMinExtent),
                                world.mapY(vMinExtent), 0.0,
                                world.mapX(uMaxExtent), world.mapY(vMaxExtent),
                                0.0, color, 1, IGraphicsTarget.LineStyle.DASHED);
                        // label the contours
                        if ((srh % labelInterval) == 0) {
                            scratch = String.format("%d", srh);
                            target.drawString(smallFont, scratch,
                                    world.mapX(uMinExtent),
                                    world.mapY(vMinExtent - labelOffset), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                            target.drawString(smallFont, scratch,
                                    world.mapX(uMaxExtent),
                                    world.mapY(vMaxExtent + labelOffset), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                        }

                    } catch (VizException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    /**
     * @param storm
     *            the _plotStorm to set
     */
    public void set_plotStorm(boolean storm) {
        _plotStorm = storm;
    }

    // ===== max ================================================
    public static float max(float[] t) {
        if (t.length == 0) {
            return 0;
        }
        float maximum = t[0]; // start with the first value
        for (int i = 1; i < t.length; i++) {
            if (t[i] > maximum) {
                maximum = t[i]; // new maximum
            }
        }
        return maximum;
    }// end method max

    // ===== min ================================================
    public static float min(float[] t) {
        if (t.length == 0) {
            return 0;
        }
        float minimum = t[0]; // start with the first value
        for (int i = 1; i < t.length; i++) {
            if (t[i] < minimum) {
                minimum = t[i]; // new minimum
            }
        }
        return minimum;
    }// end method min

    @Override
    public String inspect(ReferencedCoordinate rCoord) throws VizException {
        String s = null;

        SoundingParams sp = getSoundingParameters();
        if (sp == null) {
            return s;
        }

        s = "NO DATA";
        VerticalSounding vs = sp.getAnalysisData();
        if (vs == null) {
            return s;
        }
        try {
            Coordinate c = rCoord.getObject();
            if (this.getDisplay().getSkewTBackground().contains(c)) {
                c = WxMath.reverseSkewTXY(getSkewTWorld().unMap(c.x, c.y));
            } else if (getDisplay().getTempChangeBackground().contains(c)) {
                c = getTempChangeWorld().unMap(c.x, c.y);
                c.y = WxMath.reverseSkewTXY(c).y;
            } else if (this.getDisplay().getHodoBackground().contains(c)) {
                c = this.getDisplay().getHodoBackground().getWorld()
                        .unMap(c.x, c.y);
                Coordinate cc = getWindfromSounding(c);
                SoundingLayer sl = new SoundingLayer();
                if (cc != null) {
                    sl = windMap.get(cc);
                    c.y = sl.getPressure();
                } else {
                    return s;
                }
            } else {
                return s;
            }

            float p_mb = (float) c.y;
            float p_sfc = vs.getMaxPressurelayer().getPressure();

            if (p_mb > p_sfc) {
                return s;
            }

            double z_m = vs.interpolateValue(p_mb, DATA_TYPE.GEO_HEIGHT);
            double z_ft = (z_m == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : metersToFeet.convert(z_m);
            double t_C = (vs.interpolateValue(p_mb, DATA_TYPE.TEMPERATURE) == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : kelvinToCelsius.convert(vs.interpolateValue(p_mb,
                            DATA_TYPE.TEMPERATURE));
            double t_F = (t_C == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : celciusToFahrenheit.convert(t_C);
            double td_C = (vs.interpolateValue(p_mb, DATA_TYPE.DEWPOINT) == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : kelvinToCelsius.convert(vs.interpolateValue(p_mb,
                            DATA_TYPE.DEWPOINT));
            double td_F = (td_C == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : celciusToFahrenheit.convert(td_C);
            float u_ms = vs.interpolateValue(p_mb, DATA_TYPE.WIND_U);
            float v_ms = vs.interpolateValue(p_mb, DATA_TYPE.WIND_V);
            Coordinate sd = new Coordinate(SoundingLayer.MISSING,
                    SoundingLayer.MISSING);
            double dir = SoundingLayer.MISSING;
            double kts = SoundingLayer.MISSING;
            if (u_ms != SoundingLayer.MISSING && v_ms != SoundingLayer.MISSING) {
                sd = WxMath.speedDir(u_ms, v_ms);
                dir = sd.y;
                kts = metersPerSecondToKnots.convert(sd.x);
            }
            double theta = SoundingLayer.MISSING;
            double thetaE = SoundingLayer.MISSING;
            double w = SoundingLayer.MISSING;
            if (td_C != SoundingLayer.MISSING) {
                theta = celciusToKelvin.convert(WxMath.theta(p_mb, t_C, 1000));
                thetaE = celciusToKelvin
                        .convert(WxMath.thetae(p_mb, t_C, td_C));
                w = WxMath.mixingRatio(p_mb, t_C);
            }
            s = String.format("P=%.0fmb z=%.0fm/%.0fft\n", p_mb, z_m, z_ft);
            s += String.format(
                    "T=%.0f%cC/%.0f%cF Td=%.0f%cC/%.0f%cF %03.0f@%.0fkts\n",
                    t_C, SkewTConstants.DEGREE_SYMBOL, t_F,
                    SkewTConstants.DEGREE_SYMBOL, td_C,
                    SkewTConstants.DEGREE_SYMBOL, td_F,
                    SkewTConstants.DEGREE_SYMBOL, dir, kts);
            s += String.format(
                    "u=%.0fm/s v=%.0fm/s Theta=%.0fK Theta-e=%.0fK w=%.1f",
                    u_ms, v_ms, theta, thetaE, w);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception translating coordinate", e);
        }

        return s;
    }

    private void fireListeners() {
        for (Object listener : listenerList.getListeners()) {
            ((ISkewTDataChangedListener) listener).skewTResourceChanged(this);
        }
    }

    /**
     * Save an edited sounding to a file.
     * 
     * @param path
     */
    public void saveSounding(String path) {
        DataOutputStream out = null;
        DataTime dtg = sounding.getDataTime();
        VerticalSounding vs = skwt.getResourceData().getSoundings()[0];
        String stnID = vs.getName();

        try {
            out = new DataOutputStream(new BufferedOutputStream(
                    new FileOutputStream(path)));
            out.writeUTF(stnID);
            out.writeDouble(vs.getLatitude());
            out.writeDouble(vs.getLongitude());
            out.writeLong(dtg.getRefTime().getTime());
            for (SoundingLayer layer : sp.getInterleavedData()) {
                out.writeFloat(layer.getPressure());
                out.writeFloat(layer.getGeoHeight());
                out.writeFloat(layer.getTemperature());
                out.writeFloat(layer.getDewpoint());
                out.writeFloat(layer.getWindU());
                out.writeFloat(layer.getWindV());
            }
            out.close();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception saving edited sounding", e);
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Exception closing file.", e);
            }
        }
    }

    /**
     * Read in a saved sounding file.
     * 
     * @param path
     */
    public void loadSounding(String path) {
        DataInputStream in = null;
        VerticalSounding vs = new VerticalSounding();
        try {
            in = new DataInputStream(new BufferedInputStream(
                    new FileInputStream(path)));
            String stnID = in.readUTF();
            vs.setName(stnID);
            vs.setStationId(stnID);
            vs.setLatitude(in.readDouble());
            vs.setLongitude(in.readDouble());
            vs.setDataTime(new DataTime(new Date(in.readLong())));

            while (in.available() > 0) {
                SoundingLayer layer = new SoundingLayer();
                layer.setPressure(in.readFloat());
                layer.setGeoHeight(in.readFloat());
                layer.setTemperature(in.readFloat());
                layer.setDewpoint(in.readFloat());
                layer.setWindU(in.readFloat());
                layer.setWindV(in.readFloat());
                vs.addLayer(layer);
            }
            addSounding(vs.getDataTime(), vs);
            soundMap.put(vs.getDataTime(), vs);
            fireListeners();

        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception loading edited sounding", e);
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Exception closing sounding input stream.", e);
            }
        }
    }

    /**
     * @param c
     * @return
     */
    public boolean isPoint(Coordinate kk) {
        Coordinate c = kk;
        ispoint = false;
        if (this.getDisplay().getSkewTBackground().contains(c)) {

            c = WxMath.reverseSkewTXY(getSkewTWorld().unMap(c.x, c.y));
            ep = c;
            float p_mb = (float) c.y;
            c.x = celciusToKelvin.convert(c.x);
            SoundingLayer sle = sounding.getLayerNearest(p_mb);
            p_mb = sle.getPressure();
            double t = sle.getTemperature();
            double td = sle.getDewpoint();

            // Check if point is valid
            ep.y = p_mb;
            double zoomLevel = paintProp.getZoomLevel();
            double tb = t - zoomLevel;
            double tt = t + zoomLevel;
            double tdb = td - zoomLevel;
            double tdt = td + zoomLevel;
            if (c.x >= tb && c.x <= tt) {
                ep.x = t;
                ispoint = true;
                dpt = false;
            } else if (c.x >= tdb && c.x <= tdt) {
                ep.x = td;
                ispoint = true;
                dpt = true;
            }
        } else if (this.getDisplay().getHodoBackground().contains(c)) {
            c = this.getDisplay().getHodoBackground().getWorld()
                    .unMap(c.x, c.y);
            ep = c;
            Coordinate cc = getWindfromSounding(c);
            if (cc != null) {
                ispoint = true;
                sl = windMap.get(cc);
                ep.y = sl.getPressure();
                ep.x = sl.getTemperature();
            }
        }
        return ispoint;
    }

    /**
     * @param c
     */
    public void removeLayer(Coordinate c) {
        SoundingLayer so = new SoundingLayer();
        so = sounding.getLayer((float) c.y);
        if (so != null) {
            System.out.println("Deleting layer at: " + (int) so.getPressure()
                    + " mb");
            sounding.removeLayer(so);
            sp.recomputeSoundingParams(sounding);
            this.issueRefresh();
        }
        return;
    }

    /**
     * @param c
     */
    public void addLayer(Coordinate c) {
        if (this.getDisplay().getSkewTBackground().contains(c)) {
            c = WxMath.reverseSkewTXY(getSkewTWorld().unMap(c.x, c.y));
            c.x = celciusToKelvin.convert(c.x);
            ep = c;
            double t = sounding.interpolateValue((float) c.y,
                    DATA_TYPE.TEMPERATURE);
            double td = sounding.interpolateValue((float) c.y,
                    DATA_TYPE.DEWPOINT);
            double tb = t - 5;
            double tt = t + 5;
            double tdb = td - 5;
            double tdt = td + 5;
            if (c.x >= tb && c.x <= tt) {
                ep.x = t;
                dpt = false;
            } else if (c.x >= tdb && c.x <= tdt) {
                ep.x = td;
                dpt = true;
            }
        }
    }
}
