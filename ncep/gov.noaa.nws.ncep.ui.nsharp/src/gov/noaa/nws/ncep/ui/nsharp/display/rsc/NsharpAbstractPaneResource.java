package gov.noaa.nws.ncep.ui.nsharp.display.rsc;

/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpAbstractPaneResource extends
        AbstractVizResource<AbstractResourceData, NsharpAbstractPaneDescriptor> {
    NsharpNative nsharpNative = null;

    protected IGraphicsTarget target = null;

    protected Rectangle rectangle;

    protected WGraphics world;

    protected PixelExtent pe;

    protected static final UnitConverter celciusToFahrenheit = SI.CELSIUS
            .getConverterTo(NonSI.FAHRENHEIT);

    protected static final UnitConverter celciusToKelvin = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    protected List<NcSoundingLayer> soundingLys = null;

    protected List<NcSoundingLayer> previousSoundingLys = null;

    protected NsharpResourceHandler rscHandler = null;

    protected NsharpGraphProperty graphConfigProperty = null;

    protected HashMap<String, NsharpLineProperty> linePropertyMap = null;

    protected int currentSoundingLayerIndex = 0;

    protected IFont font9 = null;

    protected IFont font10 = null;

    protected IFont font11 = null;

    protected IFont font12 = null;

    protected IFont font20 = null; // d2dlite

    protected float currentFont10Size = 10;

    protected int commonLinewidth;

    protected LineStyle commonLineStyle;

    protected Coordinate interactiveTempPointCoordinate;

    protected Float currentZoomLevel = 1f;

    protected float currentCanvasBoundWidth;// =
                                            // NsharpConstants.DEFAULT_CANVAS_WIDTH;

    protected float currentCanvasBoundHeight; // =
                                              // NsharpConstants.DEFAULT_CANVAS_HEIGHT;

    protected float myDefaultCanvasHeight;// =
                                          // NsharpConstants.DEFAULT_CANVAS_HEIGHT*4/5;

    protected float myDefaultCanvasWidth;// =
                                         // NsharpConstants.DEFAULT_CANVAS_WIDTH/2;

    // protected Float zoomLevel;
    protected boolean resize = false;

    protected String paneConfigurationName;

    public static final float INVALID_DATA = NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA;

    protected Coordinate cursorCor;

    protected double charHeight = NsharpConstants.CHAR_HEIGHT_; // d2dlite

    protected double charWidth;

    // d2dlite
    protected double lineHeight = charHeight * 1.2;

    protected PaintProperties paintProps;

    protected boolean sidePaneMode = false; // FixMark:sidePaneLooping d2dlite

    public NsharpAbstractPaneResource(AbstractResourceData resourceData,
            LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
        super(resourceData, loadProperties);
        descriptor = desc;
        this.dataTimes = new ArrayList<DataTime>();

    }

    @Override
    protected void disposeInternal() {
        if (font9 != null) {
            font9.dispose();
            font9 = null;
        }
        if (font10 != null) {
            font10.dispose();
            font10 = null;
        }
        if (font11 != null) {
            font11.dispose();
            font11 = null;
        }
        if (font12 != null) {
            font12.dispose();
            font12 = null;
        }
        if (font20 != null) { // d2dlite
            font20.dispose();
            font20 = null;
        }
        this.target.dispose();
        target = null;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.paintProps = paintProps;
        this.target = target;
        if (rscHandler == null || rscHandler.getSoundingLys() == null)
            return;
        float zoomLevel = paintProps.getZoomLevel();
        /*
         * if( currentCanvasBoundWidth!= paintProps.getCanvasBounds().width ||
         * currentCanvasBoundHeight!=paintProps.getCanvasBounds().height){
         * 
         * currentCanvasBoundWidth= paintProps.getCanvasBounds().width;
         * currentCanvasBoundHeight=paintProps.getCanvasBounds().height;
         * adjustFontSize(currentCanvasBoundWidth,currentCanvasBoundHeight); }
         */
        // System.out.println("currentZoomLevel="+currentZoomLevel+" paintProps's zoomLevel="+zoomLevel);
        if (zoomLevel > 1.0f)
            zoomLevel = 1.0f;
        if ((zoomLevel != currentZoomLevel)) {
            currentZoomLevel = zoomLevel;
            handleZooming();

        }
        if (this.resize == true) {
            handleResize();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.target = target;
        this.font9 = target.initializeFont("Monospace", 9, null);
        this.font10 = target.initializeFont("Monospace", 10, null);
        this.font11 = target.initializeFont("Monospace", 11, null);
        IFont.Style[] style = { IFont.Style.BOLD };
        this.font12 = target.initializeFont("Monospace", 12, style);
        this.font20 = target.initializeFont("Monospace", 20, null); // d2dlite
        this.font9.setSmoothing(false);
        this.font9.setScaleFont(false);
        this.font10.setSmoothing(false);
        this.font10.setScaleFont(false);
        this.font11.setSmoothing(false);
        this.font11.setScaleFont(false);
        this.font12.setSmoothing(false);
        this.font12.setScaleFont(false);
        this.font20.setSmoothing(false); // d2dlite
        this.font20.setScaleFont(false);
        commonLinewidth = getCapability(OutlineCapability.class)
                .getOutlineWidth();
        commonLineStyle = getCapability(OutlineCapability.class).getLineStyle();
        this.resize = true;
        // nsharpNative = new NsharpNative();
        // System.out.println("NsharpDefaultPaneResource ::: initInternal with native "+
        // nsharpNative.toString());
    }

    @SuppressWarnings("deprecation")
    public void resetData(List<NcSoundingLayer> soundingLys,
            List<NcSoundingLayer> prevsoundingLys) {
        this.soundingLys = soundingLys;
        this.previousSoundingLys = prevsoundingLys;
        descriptor.setFrame(0);
    }

    public WGraphics getWorld() {
        return world;
    }

    protected void adjustFontSize(float canvasW, float canvasH) {
        float font9Size, font10Size, font11Size, font12Size, font20Size; // d2dlite

        float fontAdjusted = 0;
        float fontBaseH = 90f; // Chin: why 70 & 100? After many "try and error"
                               // experiments...
        float fontBaseW = 120f;
        if (canvasH < myDefaultCanvasHeight && canvasW < myDefaultCanvasWidth) {
            // both width and height are smaller than default
            float wAdjust = (float) (myDefaultCanvasWidth - canvasW)
                    / fontBaseW;
            float hAdjust = (float) (myDefaultCanvasHeight - canvasH)
                    / fontBaseH;
            fontAdjusted = Math.max(wAdjust, hAdjust);
        } else if (canvasW < myDefaultCanvasWidth) {
            // only width smaller than default
            fontAdjusted = (float) (myDefaultCanvasWidth - canvasW) / fontBaseW;
        } else if (canvasH < myDefaultCanvasHeight) {
            // only height smaller than default
            fontAdjusted = (float) (myDefaultCanvasHeight - canvasH)
                    / fontBaseH;
        }
        // Chin: Can not bigger than 9, otherwise, fint9 size willbe negative.
        // Why 8.8 ? After many "try and error" experiments...
        if (fontAdjusted > 8.8)
            fontAdjusted = 8.8f;

        font9Size = 9 - fontAdjusted;
        font10Size = 10 - fontAdjusted;
        font11Size = 11 - fontAdjusted;
        font12Size = 12 - fontAdjusted;
        font20Size = 20 - fontAdjusted; // d2dlite

        if (font9 != null) {
            font9.dispose();
        }
        font9 = target.initializeFont("Monospace", font9Size, null);

        if (font10 != null) {
            font10.dispose();
        }
        font10 = target.initializeFont("Monospace", font10Size, null);
        if (font11 != null) {
            font11.dispose();
        }
        font11 = target.initializeFont("Monospace", font11Size, null);
        if (font12 != null) {
            font12.dispose();
        }
        IFont.Style[] style = { IFont.Style.BOLD };
        font12 = target.initializeFont("Monospace", font12Size, style);
        // d2dlite
        if (font20 != null) {
            font20.dispose();
        }
        font20 = target.initializeFont("Monospace", font20Size, style);
        currentFont10Size = font10Size;
        // System.out.println(descriptor.getPaneNumber()+": adjusted font10 size ="+currentFont10Size);
    }

    protected void magnifyFont(double zoomLevel) {
        float magFactor = 1.0f / (float) zoomLevel;
        font9.setMagnification(magFactor);
        font10.setMagnification(magFactor);
        font11.setMagnification(magFactor);
        font12.setMagnification(magFactor);
        font20.setMagnification(magFactor); // d2dlite
    }

    @Override
    public void setDescriptor(NsharpAbstractPaneDescriptor descriptor) {
        super.setDescriptor(descriptor);
        RGB rgb = ColorUtil.getNewColor(descriptor);
        getCapability(ColorableCapability.class).setColor(rgb);
        // System.out.println("screwT Rsc  setDescriptor called");
    }

    public void setSoundingLys(List<NcSoundingLayer> soundingLys) {
        this.soundingLys = soundingLys;
    }

    public HashMap<String, NsharpLineProperty> getLinePropertyMap() {
        return linePropertyMap;
    }

    public void setLinePropertyMap(
            HashMap<String, NsharpLineProperty> linePropertyMap) {
        this.linePropertyMap = linePropertyMap;

    }

    public NsharpGraphProperty getGraphConfigProperty() {
        return graphConfigProperty;
    }

    public void setGraphConfigProperty(NsharpGraphProperty graphConfigProperty) {
        this.graphConfigProperty = graphConfigProperty;
        paneConfigurationName = this.graphConfigProperty
                .getPaneConfigurationName();

    }

    public NsharpResourceHandler getRscHandler() {
        return rscHandler;
    }

    public int getCurrentSoundingLayerIndex() {
        return currentSoundingLayerIndex;
    }

    public void setRscHandler(NsharpResourceHandler rscHandler) {
        this.rscHandler = rscHandler;
        if (descriptor != null)
            descriptor.setRscHandler(rscHandler);
    }

    public void setNsharpNative(NsharpNative nsharpNative) {
        this.nsharpNative = nsharpNative;
    }

    public void handleResize() {
        this.resize = false;
        // double vertRatio = paintProps.getView().getExtent().getHeight() /
        // paintProps.getCanvasBounds().height;
        // double hRatio = paintProps.getView().getExtent().getWidth() /
        // paintProps.getCanvasBounds().width;
        // System.out.println(descriptor.getPaneNumber()+"viewWidth="+paintProps.getView().getExtent().getWidth()+" viewHeight="+paintProps.getView().getExtent().getHeight()
        // );
        // System.out.println(descriptor.getPaneNumber()+"canvWidth="+paintProps.getCanvasBounds().width+" canvHeight="+paintProps.getCanvasBounds().height
        // );
        // System.out.println(descriptor.getPaneNumber()+": vertRatio="+vertRatio
        // + " hRatio="+hRatio);
        if (paintProps != null
                && (currentCanvasBoundWidth != paintProps.getCanvasBounds().width || currentCanvasBoundHeight != paintProps
                        .getCanvasBounds().height)) {
            currentCanvasBoundWidth = paintProps.getCanvasBounds().width;
            currentCanvasBoundHeight = paintProps.getCanvasBounds().height;
            adjustFontSize(currentCanvasBoundWidth, currentCanvasBoundHeight);
        }

    }

    public void setResize(boolean resize) {
        this.resize = resize;
    }

    public void handleZooming() {

    }

    // FixMark:sidePaneLooping
    public boolean isSidePaneMode() {
        return sidePaneMode;
    }

    // FixMark:sidePaneLooping
    public void setSidePaneMode(boolean sidePaneMode) {
        this.sidePaneMode = sidePaneMode;
    }

    protected void defineCharHeight(IFont font) {
        if (paintProps == null)
            return;
        DrawableString str = new DrawableString("CHINCHEN",
                NsharpConstants.color_black);
        str.font = font;
        double vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;
        double horizRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        charHeight = target.getStringsBounds(str).getHeight() * vertRatio; // d2dlite
        lineHeight = charHeight * 1.2; // d2dlite
        charWidth = target.getStringsBounds(str).getWidth() * horizRatio / 8;

    }

    // d2dlite start
    protected String timeDescriptionToDisplayStr(String timeDescription) {
        /*
         * As of 2014 April 9, current time description string is defined as
         * "YYMMDD/HH(DOW)" or "YYMMDD/HH(DOW)Vxxx". Convert them to
         * "DD.HH(DOW)" or "DD.HHVxxx(DOW)" for GUI display.
         */
        String rtnStr = timeDescription.substring(4); // get rid of YYMM
        if (rtnStr.contains("V")) {
            String[] s1Str = rtnStr.split("V"); // split DD/HH(DOW)Vxxx to
                                                // "DD/HH(DOW)" and "xxx"
            String[] s2Str = s1Str[0].split("\\("); // split "DD/HH(DOW)" to
                                                    // "DD/HH" and "DOW)"
            rtnStr = s2Str[0] + "V" + s1Str[1] + "(" + s2Str[1]; // put together
                                                                 // to
                                                                 // "DD/HHVxxx(DOW)"
        }
        rtnStr = rtnStr.replace("/", "."); // replace "/" with "."
        return rtnStr;
    }

    protected String pickedStnInfoStrToDisplayStr(String pickedStnInfoStr) {
        /*
         * As of 2014 April 9, current pickedStnInfoStr string is defined as
         * "stnId YYMMDD/HH(DOW)Vxxx sndType". This function is to convert it to
         * "stnId DD.HHVxxx(DOW) sndType" for GUI display. for example,
         * "ATLH 101209/03(Thu)V003 GFS230" converts to
         * "ATLH 09.03V003(Thu) GFS230"
         */
        String[] s1Str = pickedStnInfoStr.split(" ");
        if (s1Str.length == 3) {
            String rtnStr = timeDescriptionToDisplayStr(s1Str[1]);
            rtnStr = s1Str[0] + " " + rtnStr + " " + s1Str[2];
            return rtnStr;
        } else
            return pickedStnInfoStr; // not a good input, just return it
    }
    // d2dlite end

}
