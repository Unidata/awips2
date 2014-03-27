/*
 * DisplayElementFactory
 * 
 * Date created: 03 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenRangeRecord;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.CcfpAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourCircle;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourMinmax;
import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead.ArrowHeadType;
import gov.noaa.nws.ncep.ui.pgen.display.CornerPatternApplicator.CornerPattern;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAvnText.AviationTextType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.display.PatternSegment.PatternType;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.ComboSymbol;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcm;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcmFcst;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcmWindQuarter;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.TcmFcst;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaClip;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;
import gov.noaa.nws.ncep.ui.pgen.sigmet.AbstractSigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.CcfpInfo;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ISigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;
import gov.noaa.nws.ncep.ui.pgen.sigmet.VaaInfo;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Volcano;
import gov.noaa.nws.ncep.ui.pgen.tca.BPGeography;
import gov.noaa.nws.ncep.ui.pgen.tca.ITca;
import gov.noaa.nws.ncep.ui.pgen.tca.TropicalCycloneAdvisory;
import gov.noaa.nws.ncep.ui.pgen.tca.WaterBreakpoint;
import gov.noaa.nws.ncep.viz.common.SnapUtil;

import java.awt.Color;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.RenderedImage;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.common.geospatial.util.WorldWrapCorrector;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;
import com.vividsolutions.jts.geom.util.AffineTransformation;
import com.vividsolutions.jts.linearref.LengthIndexedLine;
import com.vividsolutions.jts.linearref.LengthLocationMap;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;
import com.vividsolutions.jts.operation.distance.DistanceOp;

/**
 * This factory class is used to create IDisplayable elements from IMultiPoint
 * objects. PGEN Resource can use this factory to create the elements it needs
 * to display without knowing the details of how.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#223		M.Laryukhin	Gfa added. 
 * 11/10		?			B. Yin		Added an option to draw one column mid-level cloud text
 * 11/10		#345		J. Wu		Added an option to not drawing a Text element
 * 12/10		#321		J. Wu		Auto-adjust label positions for Contours.
 * 01/11		#235D		B. Hebbard	Added grouping Vectors in few display elements for faster performance
 * 02/11		?			B. Yin		Combine WatchBox counties to fill the area
 * 04/11		?			B. Yin		Use Geometry instead of MultiPolygon for county shapes
 * 04/11		#?			B. Yin		Re-factor IAttribute, changed ISinglePoint to ISymbol
 * 07/11		#?			J. Wu		Allow more than 1 labels for closed contour lines.
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 03/12        #697        Q. Zhou     Fixed line arrow head size for line & gfa  
 * 07/12        #834        J. Wu       Fixed fuzzy text display. 
 * 08/12		#760		B. Yin		Modify line factory to apply world wrap.
 * 09/12					B. Hebbard  Merge out RTS changes from OB12.9.1 - adds reset()
 * 11/12		#901/917  	J. Wu		Set the symbol in GFA text box in proper location/size
 * 05/13                    Chin Chen   use IDescriptor instead of IMapDescriptor for used by Nsharp wind barb drawing
 * 07/13        #988        Archana 	added createDisplayElements() to add all symbols in the same color to a single wire-frame. 									
 * 09/23/13                 Chin Chen   changed several private variables/methods to protected, for NsharpDisplayElementFactory now extend from
 * 										this class
 * 11/13        TTR 752     J. Wu       added methods to compute an element's range record.
 * 12/13		#1089		B. Yin		Modify watch to display county list
 * </pre>
 * 
 * @author sgilbert
 * 
 */
public class DisplayElementFactory {

    /*
     * LinePattern segment scale types (used to rescale LinePattern so that line
     * ends with a full pattern)
     */
    private enum ScaleType {
        SCALE_ALL_SEGMENTS, SCALE_BLANK_LINE_ONLY
    };

    /**
     * Graphics Target used to create the Wireframe and Shaded shapes
     */
    protected IGraphicsTarget target;

    /**
     * Map Descriptor used for Lat/Lon to pixel coordinate transformations
     */
    protected IDescriptor/* IMapDescriptor */iDescriptor;

    private GeometryFactory gf;

    /**
     * Array of WireframeShapes used to hold all line segments to be drawn
     */
    private IWireframeShape[] wfs;

    /**
     * A ShadedShape to hold all the filled areas to be drawn
     */
    private IShadedShape ss;

    private IWireframeShape sym;

    private ILine elem;

    protected double deviceScale = 25.0; // default scale factor for GL device

    private double symbolScale = 0.65;

    private double screenToExtent = 1.0;

    double screenToWorldRatio = 1.0;

    private ArrowHead arrow;

    /**
     * Color mode, color, and fill mode used to draw all elements in a layer
     */
    private Boolean layerMonoColor = false;

    private Color layerColor = null;

    private Boolean layerFilled = false;

    protected BackgroundColor backgroundColor = BackgroundColor
            .getActivePerspectiveInstance();

    class SymbolImageCallback implements IRenderedImageCallback {
        private String patternName;

        private double scale;

        private float lineWidth;

        private boolean mask;

        private Color color;

        public SymbolImageCallback(String patternName, double scale,
                float lineWidth, boolean mask, Color color) {
            super();
            this.patternName = patternName;
            this.scale = scale;
            this.lineWidth = lineWidth;
            this.mask = mask;
            this.color = color;
        }

        @Override
        public RenderedImage getImage() throws VizException {
            return SymbolImageUtil.createBufferedImage(patternName, scale,
                    lineWidth, mask, color);
        }

    }

    /**
     * Constructor used to set initial Graphics Target and MapDescriptor
     * 
     * @param target
     *            The Graphics Target
     * @param mapDescriptor
     *            The Map Descriptor
     */
    public DisplayElementFactory(IGraphicsTarget target,
            IMapDescriptor mapDescriptor) {

        this.target = target;
        this.iDescriptor = mapDescriptor;
        gf = new GeometryFactory();

    }

    public DisplayElementFactory(IGraphicsTarget target, IDescriptor iDescriptor) {

        this.target = target;
        this.iDescriptor = iDescriptor;
        gf = new GeometryFactory();

    }

    /**
     * Creates a list of IDisplayable Objects from an IMultiPoint object
     * 
     * @param de
     *            A PGEN Drawable Element of a multipoint object
     * @param paintProps
     *            The paint properties associated with the target
     * @param worldWrap
     *            The flag to apply world wrap for lines
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ILine de,
            PaintProperties paintProps, boolean worldWrap) {

        if (worldWrap) {
            elem = de;
            ArrayList<IDisplayable> list = new ArrayList<IDisplayable>();

            WorldWrapCorrector corrector = new WorldWrapCorrector(
                    iDescriptor.getGridGeometry());

            // put line points in a coordinate array
            Coordinate[] coord;
            if (de.isClosedLine()) {
                coord = new Coordinate[de.getLinePoints().length + 1];
                for (int ii = 0; ii < de.getLinePoints().length; ii++) {
                    coord[ii] = new Coordinate(de.getLinePoints()[ii].x,
                            de.getLinePoints()[ii].y);
                }
                coord[de.getLinePoints().length] = new Coordinate(
                        de.getLinePoints()[0].x, de.getLinePoints()[0].y);
            } else {
                coord = new Coordinate[de.getLinePoints().length];

                for (int ii = 0; ii < de.getLinePoints().length; ii++) {
                    coord[ii] = new Coordinate(de.getLinePoints()[ii].x,
                            de.getLinePoints()[ii].y);
                }

            }

            // apply world wrap.
            // pointsToLineString is in GfaClip. It should be put in a common
            // place
            Geometry geo = null;
            try {
                geo = corrector.correct(GfaClip.getInstance()
                        .pointsToLineString(coord));
            } catch (Exception e) {
                System.out.println("World wrap error: " + e.getMessage());
                return list;
            }

            if (geo != null && geo.getNumGeometries() > 1) {
                for (int ii = 0; ii < geo.getNumGeometries(); ii++) {
                    Geometry geo1 = geo.getGeometryN(ii);
                    double[][] pixels = PgenUtil
                            .latlonToPixel(geo1.getCoordinates(),
                                    (IMapDescriptor) iDescriptor);
                    double[][] smoothpts;
                    float density;

                    // Apply parametric smoothing on pixel coordinates, if
                    // required
                    if (de.getSmoothFactor() > 0) {
                        float devScale = 50.0f;
                        if (de.getSmoothFactor() == 1)
                            density = devScale / 1.0f;
                        else
                            density = devScale / 5.0f;
                        smoothpts = CurveFitter.fitParametricCurve(pixels,
                                density);
                    } else {
                        smoothpts = pixels;
                    }

                    list.addAll(createDisplayElementsForLines(de, smoothpts,
                            paintProps));

                    // Draw labels for contour lines.
                    // list.addAll( adjustContourLineLabels( elem, paintProps,
                    // smoothpts ) );

                }

                return list;
            }

        }

        return createDisplayElements(de, paintProps);

    }

    /**
     * Creates display elements for multiple points elements. This method gets
     * attributes, such as colors from the input elements, then apply these
     * attributes on the smoothed(if needed) points to create a list of
     * displayable.
     * 
     * @param de
     *            A PGEN Drawable Element of a multipoint object
     * @param smoothpts
     *            points of the multipoint object
     * @param paintProps
     *            The paint properties associated with the target
     * @return
     */

    private ArrayList<IDisplayable> createDisplayElementsForLines(ILine de,
            double[][] smoothpts, PaintProperties paintProps) {

        /*
         * Get color for creating displayables.
         */
        Color[] dspClr = getDisplayColors(elem.getColors());

        /*
         * Find Line Pattern associated with this element, if "Solid Line" was
         * not requested.
         */
        LinePattern pattern = null;
        LinePatternManager lpl = LinePatternManager.getInstance();
        try {
            pattern = lpl.getLinePattern(de.getPatternName());
            // System.out.println("&&&pattern "+pattern.getMaxExtent());
        } catch (LinePatternException lpe) {
            /*
             * could not find desired line pattern. Used solid line as default.
             */
            System.out.println(lpe.getMessage()
                    + ":  Using Solid Line by default.");
            pattern = null;
        }

        /*
         * If pattern has some segments whose length is set at runtime based on
         * the desired line width, update the pattern now
         */
        if ((pattern != null) && pattern.needsLengthUpdate()) {
            pattern = pattern.updateLength(screenToExtent * de.getLineWidth()
                    / (de.getSizeScale() * deviceScale));
        }

        /*
         * Flip the side of the pattern along the spine
         */
        if ((elem instanceof Line) && ((Line) elem).isFlipSide())
            pattern = pattern.flipSide();

        /*
         * If a LinePattern is found for the object, apply it. Otherwise, just
         * use solid line.
         */
        ScaleType scaleType = null;
        if ((pattern != null) && (pattern.getNumSegments() > 0)) {
            scaleType = ScaleType.SCALE_ALL_SEGMENTS;
            if (elem instanceof Line) {
                Line line = (Line) elem;
                // Change scale type for fronts so that only BLANK and LINE
                // segments are scaled.
                // This is done so that size of front pips don't vary with
                // length of front.
                if (line.getPgenCategory().equalsIgnoreCase("Front"))
                    scaleType = ScaleType.SCALE_BLANK_LINE_ONLY;
            }
        }

        boolean isCCFP = false;
        AbstractDrawableComponent adc = ((Line) de).getParent();
        isCCFP = (adc != null && ("CCFP_SIGMET".equals(adc.getPgenType())));
        DECollection ccfp = null;
        if (isCCFP) {
            ccfp = (DECollection) adc;
        }

        ArrayList<IDisplayable> list = new ArrayList<IDisplayable>();

        list.addAll(createDisplayElementsFromPts(smoothpts, dspClr, pattern,
                scaleType, getDisplayFillMode(de.isFilled()),
                ((ILine) de).getLineWidth(), isCCFP, ccfp, paintProps));

        /*
         * Draw labels for contour lines.
         */
        list.addAll(adjustContourLineLabels(elem, paintProps, smoothpts));

        return list;
    }

    /**
     * Creates displayable from the input attributes and points of the line
     * 
     * @param pts
     * @param dspClr
     * @param pattern
     * @param scaleType
     * @param isFilled
     * @param lineWidth
     * @param isCCFP
     * @param ccfp
     * @param paintProps
     * @return
     */
    private ArrayList<IDisplayable> createDisplayElementsFromPts(
            double[][] pts, Color[] dspClr, LinePattern pattern,
            ScaleType scaleType, Boolean isFilled, float lineWidth,
            boolean isCCFP, DECollection ccfp, PaintProperties paintProps) {

        ArrayList<IDisplayable> list = new ArrayList<IDisplayable>();
        wfs = new IWireframeShape[dspClr.length];
        for (int i = 0; i < dspClr.length; i++) {
            wfs[i] = target.createWireframeShape(false, iDescriptor);
        }
        ss = target.createShadedShape(false, iDescriptor, true);

        /*
         * Create arrow head, if needed
         */
        if ((pattern != null) && pattern.hasArrowHead()) {
            /*
             * Get scale size from drawable element.
             */
            double scale = elem.getSizeScale();
            if (scale <= 0.0)
                scale = 1.0;
            double sfactor = deviceScale * scale;

            double pointAngle = 60.0; // Angle of arrow point - defining
                                      // narrowness
            double extent = pattern.getMaxExtent();

            // Consider distance away from center line, the height should be no
            // less than extent * 1.5.
            // Currently we only have extent 1 and 2 available.
            // 3.5 is what we want the size to be.
            double height = sfactor * 3.5;
            if (extent * 1.5 > 3.5)
                height = sfactor * extent * 1.5;

            int n = pts.length - 1;
            // calculate direction of arrow head
            double slope = Math.toDegrees(Math.atan2(
                    (pts[n][1] - pts[n - 1][1]), (pts[n][0] - pts[n - 1][0])));

            arrow = new ArrowHead(new Coordinate(pts[n][0], pts[n][1]),
                    pointAngle, slope, height, pattern.getArrowHeadType());
            Coordinate[] ahead = arrow.getArrowHeadShape();

            if (pattern.getArrowHeadType() == ArrowHeadType.OPEN)
                // Add to wireframe
                wfs[0].addLineSegment(toDouble(ahead));
            if (pattern.getArrowHeadType() == ArrowHeadType.FILLED) {
                // Add to shadedshape

                ss.addPolygonPixelSpace(toLineString(ahead),
                        new RGB(dspClr[0].getRed(), dspClr[0].getGreen(),
                                dspClr[0].getBlue()));
            }
        }
        if ((pattern != null) && (pattern.getNumSegments() > 0)) {
            handleLinePattern(pattern, pts, scaleType);
        } else {
            wfs[0].addLineSegment(pts);
        }

        if (isFilled) {
            list.add(createFill(pts));
        }
        /*
         * Compile each IWireframeShape, create its LineDisplayElement, and add
         * to IDisplayable return list
         */
        for (int k = 0; k < wfs.length; k++) {

            wfs[k].compile();
            LineDisplayElement lde = new LineDisplayElement(wfs[k], dspClr[k],
                    lineWidth);
            list.add(lde);
        }

        /*
         * Compile each IShadedShape, create FillDisplayElement, and add to
         * IDisplayable return list
         */
        // TODO - This loop may be needed if we ever have to support different
        // alphas
        // for ( IShadedShape shade : ss ) {
        ss.compile();
        FillDisplayElement fde = new FillDisplayElement(ss,
                elem.getColors()[0].getAlpha());
        list.add(fde);

        if (isCCFP) {
            addCcfpSpeed(list, paintProps, ccfp);
        }

        // }
        return list;
    }

    /**
     * Creates a list of IDisplayable Objects from an IMultiPoint object
     * 
     * @param de
     *            A PGEN Drawable Element of a multipoint object
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ILine de,
            PaintProperties paintProps) {

        double[][] smoothpts;
        double[][] pixels;
        float density;

        setScales(paintProps);

        /*
         * save drawable element
         */
        elem = de;

        /*
         * Create the List to be returned, some wireframe shapes and a shaded
         * shape to be used for the IDisplayables
         */
        ArrayList<IDisplayable> list = new ArrayList<IDisplayable>();

        /*
         * Get lat/lon coordinates from drawable element
         */
        Coordinate[] pts = de.getLinePoints();

        /*
         * convert lat/lon coordinates to pixel coordinates
         */
        pixels = PgenUtil.latlonToPixel(pts, (IMapDescriptor) iDescriptor);

        /*
         * If line is closed, make sure last point is same as first point
         */
        if (de.isClosedLine()) {
            pixels = ensureClosed(pixels);
        }

        /*
         * Apply parametric smoothing on pixel coordinates, if required
         */
        if (de.getSmoothFactor() > 0) {
            float devScale = 50.0f;
            if (de.getSmoothFactor() == 1)
                density = devScale / 1.0f;
            else
                density = devScale / 5.0f;
            smoothpts = CurveFitter.fitParametricCurve(pixels, density);
        } else {
            smoothpts = pixels;
        }

        list.addAll(createDisplayElementsForLines(de, smoothpts, paintProps));

        /*
         * Draw labels for contour lines.
         */
        list.addAll(adjustContourLineLabels(elem, paintProps, smoothpts));

        return list;
    }

	/**
	 * Creates a list of IDisplayable Objects from an IWatchBox object
	 * @param de A PGEN Drawable Element of a WatchBox object
	 * @param paintProps The paint properties associated with the target
	 * @return A list of IDisplayable elements
	 */
	public ArrayList<IDisplayable> createDisplayElements(IWatchBox watchBox, PaintProperties paintProps) {

	    /*
         * Create the List to be returned
         */
        ArrayList<IDisplayable> dlist = new ArrayList<IDisplayable>();
	    
        List<SPCCounty> counties = watchBox.getOriginalCountyList();
        if ( counties == null || counties.isEmpty()){  //if the watch is not issued yet, the original county list is not set. 
        	counties = watchBox.getCountyList();
        }
        
        if ( counties != null && !counties.isEmpty()){
        	if ( watchBox.getFillFlag() ){
        		
        		Geometry cntyUnion = null;;  
        		Color[] colors = null;
        		
        		Collection<Geometry> gCollection = new ArrayList<Geometry>();

        		//draw county border
        		for ( SPCCounty cnty : counties ){
	    			Geometry countyGeo = cnty.getShape();
	    			
	    			colors = watchBox.getColors();
	    			colors[1] = watchBox.getFillColor();
	    			
	    			for ( int ii = 0; ii < countyGeo.getNumGeometries(); ii ++ ){
	    				Polygon poly = (Polygon)countyGeo.getGeometryN(ii);
	    				List<Coordinate> pts = new ArrayList<Coordinate>(Arrays.asList(poly.getCoordinates()));

	    				Line cntyBorder = new Line(null, colors,.5f,.5,true,
	    						false, pts, 0,
	    						FillPattern.FILL_PATTERN_6,"Lines","LINE_SOLID");
	    				ArrayList<IDisplayable> cntyLine = createDisplayElements(cntyBorder,paintProps);
	    				dlist.addAll(cntyLine);
	    			}
	    			
	    			if ( countyGeo != null ){
	    				gCollection.add(countyGeo.buffer(.02));
	    			}
        		}


        		//Merge counties together and fill the whole area
        		GeometryFactory gf = new GeometryFactory();

        		if ( gCollection.size() > 1 ){
        			GeometryCollection geometryCollection =
        				(GeometryCollection) gf.buildGeometry( gCollection );

        			cntyUnion = geometryCollection.union();
        		}
        		else cntyUnion = gf.buildGeometry( gCollection );

        		IShadedShape theShadedShape = target.createShadedShape(false, iDescriptor, true);

        //		IWireframeShape theWireframeShape = target.createWireframeShape(false, mapDescriptor);

        		JTSCompiler compiler = new JTSCompiler( theShadedShape,
        					null, iDescriptor, PointStyle.CROSS);

        		try {
					compiler.handle((Geometry) cntyUnion.clone(), 
							new RGB(colors[1].getRed(), colors[1].getGreen(), colors[1].getBlue()));
					
					if ( elem.getFillPattern() != FillPattern.TRANSPARENCY &&
							elem.getFillPattern() != FillPattern.SOLID ) {
						FillPatternList fpl = new FillPatternList();
						byte[] fpattern = fpl.getFillPattern(elem.getFillPattern());
						theShadedShape.setFillPattern(fpattern);
					}
					theShadedShape.compile();

					// theWireframeShape.compile();
					//dlist.add(new LineDisplayElement(theWireframeShape, colors[1], .5f));
					dlist.add( new FillDisplayElement(theShadedShape, 1f ));
					
				} catch (VizException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
        	}
        	else {
        		for ( SPCCounty cnty : counties ){

        			Symbol cSymbol = new Symbol(null, watchBox.getColors(), 
        					watchBox.getWatchSymbolWidth(), watchBox.getWatchSymbolSize(), false,
        					cnty.getCentriod(),	"Marker", watchBox.getWatchSymbolType());
        			ArrayList<IDisplayable> cList = createDisplayElements(cSymbol,paintProps);
        			dlist.addAll(cList);
        		}
        	}
        }
        
        //if already issued, draw the list of active counties with OCTAGON symbols.
        if (watchBox.getIssueFlag() != 0  ){
        	List<SPCCounty> activeCounties = watchBox.getCountyList();
        	if ( activeCounties != null && !activeCounties.isEmpty()){
        		for ( SPCCounty cnty : activeCounties ){

        			Symbol cSymbol = new Symbol(null, IWatchBox.WATCH_LIST_COLOR, 
        					3, 2, false,
        					cnty.getCentriod(),	"Marker", "OCTAGON");
        			ArrayList<IDisplayable> cList = createDisplayElements(cSymbol,paintProps);
        			dlist.addAll(cList);
        		}
        	}
        }
        
        Coordinate[] points = watchBox.getLinePoints();
        ArrayList<Coordinate> ptsList = new ArrayList<Coordinate>();
        
        for ( int ii = 0; ii < points.length; ii++ ){
        	ptsList.add(points[ii]);
        }
        
        //get displayElements for the watch box.
        Line box = new Line(null, watchBox.getColors(),3.0f,3.0,true,
	              		false, ptsList,
	              		0,FillPattern.SOLID,"Lines","LINE_SOLID");
        ArrayList<IDisplayable> dBox = createDisplayElements(box,paintProps);
        dlist.addAll(dBox);

        //get displayElements for the center line in the watch box
        ptsList.clear();
        ptsList.add(points[0]);
        ptsList.add(new Coordinate((points[0].x+points[4].x)/2,
        		(points[0].y+points[4].y)/2));
        ptsList.add(points[4]);
        
        Line centerLine = new Line(null, watchBox.getColors(),3.0f,3.0,false,
          		false, ptsList, 0,FillPattern.SOLID,"Lines","LINE_SOLID");
        
        ArrayList<IDisplayable> dLine = createDisplayElements(centerLine,paintProps);
        dlist.addAll(dLine);
        
        Station[] anchors = watchBox.getAnchors();
        Symbol anchor1 = new Symbol(null, watchBox.getColors(), 1.5f, 0.7, false,
        						new Coordinate(anchors[0].getLongitude(),anchors[0].getLatitude()),
        						"Marker", "DIAMOND");
        
        ArrayList<IDisplayable> aList1 = createDisplayElements(anchor1,paintProps);
        dlist.addAll(aList1);
        
        Symbol anchor2 = new Symbol(null, watchBox.getColors(), 1.5f, 0.7, false,
				new Coordinate(anchors[1].getLongitude(),anchors[1].getLatitude()),
				"Marker", "DIAMOND");

        ArrayList<IDisplayable> aList2 = createDisplayElements(anchor2,paintProps);
        dlist.addAll(aList2); 
        
        //Add watch number if the watch is issued
        if ( watchBox.getIssueFlag() != 0 ){
        	
        	String[] wtext = { String.valueOf(watchBox.getWatchNumber())}; 
        	Text wNumber= new Text(null, "Courier", 18f,
        			TextJustification.CENTER, new Coordinate(watchBox.getLinePoints()[7].x -.05,
        			watchBox.getLinePoints()[7].y - .1), 0., TextRotation.SCREEN_RELATIVE, 
        			wtext, FontStyle.REGULAR, watchBox.getColors()[0],
        			0, 0, true, DisplayType.NORMAL, "Text", "Text" );
        	ArrayList<IDisplayable> tList = createDisplayElements((IText)wNumber,paintProps);
        
           dlist.addAll(tList); 
        }
        
        return dlist;

	}
	

    /**
     * Method to add ALL symbols of the same color into a single wire-frame.
     * Designed to increase efficiency in rendering symbols.
     * 
     * @param paintProps
     * @param listOfSymbolLocSets
     *            - A list of symbols - each of which will be rendered at
     *            multiple locations
     * @return A list of IDisplayable elements
     */
    public List<IDisplayable> createDisplayElements(PaintProperties paintProps,
            List<SymbolLocationSet> listOfSymbolLocSets) {
        List<IDisplayable> listOfDisplayables = new ArrayList<IDisplayable>(0);
        setScales(paintProps);

        Map<Color, IWireframeShape> mapOfWireFrames = new HashMap<Color, IWireframeShape>();
        Map<Color, IWireframeShape> mapOfMasks = new HashMap<Color, IWireframeShape>();
        Map<Color, IShadedShape> mapOfShadedShapes = new HashMap<Color, IShadedShape>();
        Map<Color, Float> mapOfLineWidths = new HashMap<Color, Float>();
        // this assumes that all symbols of the same color have the same
        // lineWidth
        SymbolPatternManager symbolPatternManager = SymbolPatternManager
                .getInstance();

        for (ISymbolSet eachSymbolSet : listOfSymbolLocSets) {
            Symbol symbol = eachSymbolSet.getSymbol();
            if (symbol == null)
                continue;
            double sfactor = deviceScale * symbol.getSizeScale() * 0.5;
            Float lineWidth = symbol.getLineWidth();
            Color symbolColor = symbol.getColors()[0];
            mapOfLineWidths.put(symbolColor, lineWidth);
            RGB symbolRGB = null;
            if (symbolColor != null) {
                symbolRGB = new RGB(symbolColor.getRed(),
                        symbolColor.getGreen(), symbolColor.getBlue());
            }
            Coordinate[] symbolLocArray = eachSymbolSet.getLocations();
            Color bgColor = null;
            IWireframeShape mask = null;
            IWireframeShape wireFrameShape = mapOfWireFrames.get(symbolColor);
            if (wireFrameShape == null) {
                wireFrameShape = target
                        .createWireframeShape(false, iDescriptor);

            }

            IShadedShape symbolShadedShape = mapOfShadedShapes.get(symbolColor);
            if (symbolShadedShape == null) {
                symbolShadedShape = target.createShadedShape(false,
                        iDescriptor.getGridGeometry(), false);

            }

            if (symbol.isClear()) {
                RGB bgclr = backgroundColor.getColor(BGColorMode.EDITOR);
                bgColor = new Color(bgclr.red, bgclr.green, bgclr.blue);
                mask = mapOfMasks.get(bgColor);
                if (mask == null) {
                    mask = target.createWireframeShape(false, iDescriptor);
                }
                mapOfLineWidths.put(bgColor, lineWidth);
            }

            try {
                SymbolPattern symbolPattern = symbolPatternManager
                        .getSymbolPattern(symbol.getPatternName());

                /* Get the list of parts to draw the symbol */
                List<SymbolPart> listOfSymbolParts = symbolPattern.getParts();

                /*
                 * Repeat for ALL the locations at which this symbol needs to be
                 * rendered
                 */
                for (Coordinate currWorldCoord : symbolLocArray) {
                    if (currWorldCoord == null)
                        continue;
                    double[] symbolLocWorldCoord = new double[] {
                            currWorldCoord.x, currWorldCoord.y };
                    double[] pixCoord = iDescriptor
                            .worldToPixel(symbolLocWorldCoord);

                    for (SymbolPart sPart : listOfSymbolParts) {
                        Coordinate[] coords = sPart.getPath();
                        double[][] path = new double[coords.length][3];

                        /*
                         * At each location where this symbol is to be drawn,
                         * create a line segment path
                         */
                        for (int j = 0; j < coords.length; j++) {
                            path[j][0] = pixCoord[0] + (sfactor * coords[j].x);
                            path[j][1] = pixCoord[1] + (-sfactor * coords[j].y);
                        }

                        /* If needed - add the line segment part to the mask */
                        if (symbol.isClear() && (mask != null)) {
                            mask.addLineSegment(path);
                        }

                        /* Add the line-segment path to the wire-frame */
                        wireFrameShape.addLineSegment(path);

                        /*
                         * If needed - add the shaded shape corresponding to the
                         * symbol
                         */
                        if (getDisplayFillMode(sPart.isFilled())) {
                            Coordinate[] pixels = new Coordinate[path.length];
                            for (int k = 0; k < path.length; k++) {
                                pixels[k] = new Coordinate(path[k][0],
                                        path[k][1]);
                            }
                            symbolShadedShape.addPolygonPixelSpace(
                                    toLineString(pixels), symbolRGB);
                        }

                    }

                }

                mapOfWireFrames.put(symbolColor, wireFrameShape);
                mapOfShadedShapes.put(symbolColor, symbolShadedShape);
                if ((symbol.isClear()) && (bgColor != null) && (mask != null)) {
                    mapOfMasks.put(bgColor, mask);
                }

            } catch (SymbolPatternException e) {
                return listOfDisplayables;
            }

        }

        Set<Color> maskColorSet = mapOfMasks.keySet();
        Set<Color> wireFrameColorSet = mapOfWireFrames.keySet();
        Set<Color> shadedShapesColorSet = mapOfShadedShapes.keySet();

        float lineWidthScaleFactor = 0.5f;
        for (Color color : maskColorSet) {
            IWireframeShape maskWireframeShape = mapOfMasks.get(color);
            maskWireframeShape.compile();
            Float theLineWidth = mapOfLineWidths.get(color);
            float lineWidth = 1.0f;

            if (theLineWidth != null)
                lineWidth = theLineWidth.floatValue() * lineWidthScaleFactor;
            listOfDisplayables.add(new LineDisplayElement(maskWireframeShape,
                    color, (float) (lineWidth + 25)));
        }

        for (Color color : wireFrameColorSet) {
            IWireframeShape symbolWireframeShape = mapOfWireFrames.get(color);
            symbolWireframeShape.compile();
            Float theLineWidth = mapOfLineWidths.get(color);
            float lineWidth = 1.0f;
            if (theLineWidth != null)
                lineWidth = theLineWidth.floatValue() * lineWidthScaleFactor;
            listOfDisplayables.add(new LineDisplayElement(symbolWireframeShape,
                    color, (float) (lineWidth)));
        }

        for (Color color : shadedShapesColorSet) {
            IShadedShape shadedSymbolShape = mapOfShadedShapes.get(color);
            shadedSymbolShape.compile();
            listOfDisplayables.add(new FillDisplayElement(shadedSymbolShape,
                    color.getAlpha()));
        }

        return listOfDisplayables;

    }

    /**
     * Creates a list of IDisplayable Objects from an ISinglePoint object. This
     * was the original method used to create symbols made of Wireframe and
     * Shaded shapes. Creation of these symbols is expensive, and has been
     * replaced with raster versions that provide better performance as well as
     * nicer looking graphics, since they can be created with line anti-aliasing
     * and rounded endcaps. Use createDisplayElements(ISinglePoint,
     * PaintProperties) instead, as this method may become deprecated soon, if
     * it is not needed.
     * 
     * @param de
     *            A PGEN Drawable Element of a multipoint object
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElementsOrig(ISymbol de,
            PaintProperties paintProps) {
        setScales(paintProps);
        double sfactor = deviceScale * de.getSizeScale();

        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        sym = target.createWireframeShape(false, iDescriptor);
        ss = target.createShadedShape(false, iDescriptor, true);
        IWireframeShape mask = target.createWireframeShape(false, iDescriptor);

        double[] tmp = { de.getLocation().x, de.getLocation().y, 0.0 };
        double[] center = iDescriptor.worldToPixel(tmp);

        /*
         * Get color for creating displayables.
         */
        Color[] dspClr = getDisplayColors(de.getColors());

        /*
         * Find Symbol Pattern associated with this element
         */
        SymbolPattern pattern = null;
        SymbolPatternManager spl = SymbolPatternManager.getInstance();
        try {
            pattern = spl.getSymbolPattern(de.getPatternName());
        } catch (SymbolPatternException spe) {
            System.out.println(spe.getMessage());
            return slist;
        }

        for (SymbolPart spart : pattern.getParts()) {
            Coordinate[] coords = spart.getPath();
            double[][] path = new double[coords.length][3];

            for (int j = 0; j < coords.length; j++) {
                path[j][0] = center[0] + (sfactor * coords[j].x);
                path[j][1] = center[1] + (-sfactor * coords[j].y);
            }
            sym.addLineSegment(path);
            if (de.isClear())
                mask.addLineSegment(path);

            /*
             * This part of the symbol requires a filled Shadedshape
             */
            if (getDisplayFillMode(spart.isFilled())) {
                Coordinate[] pixels = new Coordinate[path.length];
                for (int k = 0; k < path.length; k++) {
                    pixels[k] = new Coordinate(path[k][0], path[k][1]);
                }
                ss.addPolygonPixelSpace(toLineString(pixels),
                        new RGB(dspClr[0].getRed(), dspClr[0].getGreen(),
                                dspClr[0].getBlue()));
            }
        }

        /*
         * Compile the Wireframe and Shaded shapes
         */
        sym.compile();
        ss.compile();

        /*
         * If background should be cleared, make new LineDisplayElement of the
         * symbol in black with a slightly larger line width, and add it to
         * return list
         */
        if (de.isClear()) {
            RGB bgclr = backgroundColor.getColor(BGColorMode.EDITOR);
            Color bgcolor = new Color(bgclr.red, bgclr.green, bgclr.blue);
            mask.compile();
            LineDisplayElement ldbg = new LineDisplayElement(mask, bgcolor,
                    de.getLineWidth() + 25);
            slist.add(ldbg);
        }

        /*
         * Make LineDisplayElement of the Symbol and add it to return list
         */
        LineDisplayElement lde = new LineDisplayElement(sym, dspClr[0],
                de.getLineWidth());
        slist.add(lde);

        /*
         * Make FillDisplayElement and add it to return list
         */
        FillDisplayElement fde = new FillDisplayElement(ss,
                de.getColors()[0].getAlpha());
        slist.add(fde);

        return slist;
    }

    /**
     * Create IDisplayable of a line with a "Kink" in it.
     * 
     * @param kline
     *            A PGEN Drawable Element of a Kink Line
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(IKink kline,
            PaintProperties paintProps) {
        setScales(paintProps);
        double sfactor = deviceScale * kline.getSizeScale();

        /*
         * Create the List to be returned, and wireframe shape
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        IWireframeShape kinkLine = target.createWireframeShape(false,
                iDescriptor);

        /*
         * Get color for creating displayables.
         */
        Color dspClr = getDisplayColor(kline.getColor());

        /*
         * Convert Map to pixel coordinates for the start and end points
         */
        double[] tmp = { kline.getStartPoint().x, kline.getStartPoint().y, 0.0 };
        double[] first = iDescriptor.worldToPixel(tmp);
        Coordinate startPixel = new Coordinate(first[0], first[1]);
        double[] tmp2 = { kline.getEndPoint().x, kline.getEndPoint().y, 0.0 };
        double[] last = iDescriptor.worldToPixel(tmp2);
        Coordinate endPixel = new Coordinate(last[0], last[1]);

        /*
         * Create a LengthIndexedLine used to reference points along the segment
         * at specific distances
         */
        // GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(new Coordinate[] { startPixel,
                endPixel });
        LengthIndexedLine lil = new LengthIndexedLine(ls);

        /*
         * Use CornerPatternApplicator to calculate slashes of the "X" pattern
         */
        double kinkLocation = ls.getLength() * kline.getKinkPosition();
        double offset = sfactor * 2.0;
        CornerPatternApplicator ex = new CornerPatternApplicator(lil,
                kinkLocation - offset, kinkLocation + offset);
        ex.setHeight(offset);
        ex.setPatternType(CornerPattern.X_PATTERN);
        double[][] exes = ex.calculateLines();

        /*
         * Use the 2nd slash of the "X" pattern as the "kink" in the line
         * segment
         */
        double[][] coords = new double[][] { first, exes[3], exes[2], last };
        kinkLine.addLineSegment(coords);

        /*
         * Calculate the Arrow Head to display at the end of the line.
         */
        double pointAngle = 90.0;
        double slope = Math.toDegrees(Math.atan2((last[1] - exes[2][1]),
                (last[0] - exes[2][0])));

        ArrowHead arrow = new ArrowHead(new Coordinate(last[0], last[1]),
                pointAngle, slope, 1.5 * offset, kline.getArrowHeadType());
        Coordinate[] ahead = arrow.getArrowHeadShape();

        if (kline.getArrowHeadType() == ArrowHeadType.OPEN)
            // Add to wireframe
            kinkLine.addLineSegment(toDouble(ahead));
        if (kline.getArrowHeadType() == ArrowHeadType.FILLED) {
            /*
             * create new ShadedShape and FillDisplayElement for the filled
             * arrow head, and add it to return list
             */
            IShadedShape head = target.createShadedShape(false, iDescriptor,
                    false);
            head.addPolygonPixelSpace(
                    toLineString(ahead),
                    new RGB(dspClr.getRed(), dspClr.getGreen(), dspClr
                            .getBlue()));
            head.compile();
            slist.add(new FillDisplayElement(head, 1.0f));
        }

        /*
         * Create new LineDisplayElement from wireframe shapes and add it to
         * return list
         */
        kinkLine.compile();
        slist.add(new LineDisplayElement(kinkLine, dspClr, kline.getLineWidth()));
        return slist;

    }

    /**
     * Create IDisplayable of a "wind" drawable element. Can create displayables
     * of wind speed and direction as wind barbs, arrows, or hash marks.
     * 
     * @param vect
     *            A PGEN Drawable Element of a wind object
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(IVector vect,
            PaintProperties paintProps) {
        // double sfactor = deviceScale * vect.getSizeScale();
        setScales(paintProps);

        ArrayList<IDisplayable> slist = null;

        /*
         * Create appropriate vector representation
         */
        switch (vect.getVectorType()) {

        case ARROW:
            slist = createArrow(vect);
            break;

        case WIND_BARB:
            slist = createWindBarb(vect);
            break;

        case HASH_MARK:
            slist = createHashMark(vect);
            break;

        default:
            /*
             * Unrecognized vector type; return empty list
             */
            return new ArrayList<IDisplayable>();

        }

        return slist;
    }

    /**
     * Create IDisplayables of multiple "wind" drawable elements. Aggregates
     * into relatively few IDisplayables, for (much) faster performance esp.
     * with large numbers of wind vectors. Can create displayables of wind speed
     * and direction as wind barbs, arrows, or hash marks.
     * 
     * @param vectors
     *            A list of PGEN Drawable Elements of wind objects
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of (relatively few, combined) IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(List<IVector> vectors,
            PaintProperties paintProps) {
        // double sfactor = deviceScale * vect.getSizeScale();
        setScales(paintProps);

        ArrayList<IDisplayable> slist = null;

        /*
         * Create appropriate vector representation
         */
        if (vectors == null || vectors.size() < 1) {
            return new ArrayList<IDisplayable>();
        }
        switch (vectors.get(0).getVectorType()) { // TODO: Assumes all same type
                                                  // -- generalize!

        case ARROW:
            slist = createArrows(vectors);
            break;

        case WIND_BARB:
            slist = createWindBarbs(vectors);
            break;

        case HASH_MARK:
            slist = createHashMarks(vectors);
            break;

        default:
            /*
             * Unrecognized vector type; return empty list
             */
            return new ArrayList<IDisplayable>();

        }

        return slist;
    }

    /**
     * Create IDisplayable of a text string.
     * 
     * @param txt
     *            A PGEN Drawable Element of a text string
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(IText txt,
            PaintProperties paintProps) {
        setScales(paintProps);

        /*
         * Create the List to be returned
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        /*
         * Skip if the "hide" is true
         */
        if (((Text) txt).getHide() != null && ((Text) txt).getHide()) {
            return slist;
        }

        /*
         * Skip text labels on contour lines and contour circles - they are
         * drawn all together with the contour line. Also adjust the text
         * position if it is a label for a contour minmax symbol.
         */
        AbstractDrawableComponent tparent = ((Text) txt).getParent();
        if (tparent != null) {
            if (tparent instanceof ContourLine
                    || tparent instanceof ContourCircle) {
                return slist;
            } else if (tparent instanceof ContourMinmax) {
                if (((Text) txt).getAuto() != null && ((Text) txt).getAuto()) {
                    Coordinate loc = ((ISinglePoint) ((ContourMinmax) tparent)
                            .getSymbol()).getLocation();
                    double[] pixel = iDescriptor.worldToPixel(new double[] {
                            loc.x, loc.y, 0.0 });
                    double sfactor = deviceScale
                            * ((ContourMinmax) tparent).getSymbol()
                                    .getSizeScale();

                    pixel[1] = pixel[1] + sfactor * 5;
                    double[] nloc = iDescriptor.pixelToWorld(new double[] {
                            pixel[0], pixel[1], 0.0 });
                    ((Text) txt).setLocationOnly(new Coordinate(nloc[0],
                            nloc[1]));
                }
            }
        }

        double[] tmp = { txt.getPosition().x, txt.getPosition().y, 0.0 };
        double[] loc = iDescriptor.worldToPixel(tmp);

        double horizRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        double vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;
        /*
         * Set background mask and outline
         */
        // TextStyle mask = TextStyle.NORMAL;
        // if ( txt.maskText() && !txt.outlineText() ) {
        // mask = TextStyle.BLANKED;
        // }
        // else if ( txt.maskText() && txt.outlineText() ) {
        // mask = TextStyle.BOXED;
        // }
        // else if ( !txt.maskText() && txt.outlineText() ) {
        // mask = TextStyle.OUTLINE;
        // }

        /*
         * Initialize Font Style[] styles = null; if ( txt.getStyle() != null )
         * { switch ( txt.getStyle() ) { case BOLD: styles = new Style[] {
         * Style.BOLD }; break; case ITALIC: styles = new Style[] {
         * Style.ITALIC}; break; case BOLD_ITALIC: styles = new Style[] {
         * Style.BOLD, Style.ITALIC }; break; } } IFont font =
         * target.initializeFont(txt.getFontName(), txt.getFontSize(), styles);
         */
        IFont font = initializeFont(txt.getFontName(), txt.getFontSize(),
                txt.getStyle());

        /*
         * apply X offset in half-characters
         */
        boolean adjustOffset = false;
        if (txt.getXOffset() != 0) {
            double ratio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;
            Rectangle2D bounds = target.getStringBounds(font,
                    txt.getString()[0]);
            double charSize = ratio * bounds.getWidth()
                    / txt.getString()[0].length();
            loc[0] += 0.5 * charSize * txt.getXOffset();
            adjustOffset = true;
        }

        /*
         * apply Y offset in half-characters
         */
        if (txt.getYOffset() != 0) {
            double ratio = paintProps.getView().getExtent().getHeight()
                    / paintProps.getCanvasBounds().height;
            Rectangle2D bounds = target.getStringBounds(font,
                    txt.getString()[0]);
            double charSize = ratio * bounds.getHeight();
            loc[1] -= 0.5 * charSize * txt.getYOffset();
            adjustOffset = true;
        }

        if (adjustOffset) {
            double[] tmp1 = { loc[0], loc[1], 0.0 };
            double[] newloc = iDescriptor.pixelToWorld(tmp1);
            ((Text) txt).setLocationOnly(new Coordinate(newloc[0], newloc[1]));
            ((Text) txt).setXOffset(0);
            ((Text) txt).setYOffset(0);
        }

        /*
         * Get text color
         */
        Color clr = getDisplayColor(txt.getTextColor());
        RGB textColor = new RGB(clr.getRed(), clr.getGreen(), clr.getBlue());

        /*
         * Get angle rotation for text. If rotation is "North" relative,
         * calculate the rotation for "Screen" relative.
         */
        double rotation = txt.getRotation();
        if (txt.getRotationRelativity() == TextRotation.NORTH_RELATIVE)
            rotation += northOffsetAngle(txt.getPosition());

        /*
         * create drawableString and calculate its bounds
         */
        DrawableString dstring = new DrawableString(txt.getString(), textColor);
        dstring.font = font;
        dstring.setCoordinates(loc[0], loc[1]);
        dstring.textStyle = TextStyle.NORMAL;
        dstring.horizontalAlignment = HorizontalAlignment.CENTER;
        dstring.verticallAlignment = VerticalAlignment.MIDDLE;
        dstring.rotation = rotation;

        Rectangle2D bounds = target.getStringsBounds(dstring);
        double xOffset = (bounds.getWidth() + 1) * horizRatio / 2;
        double yOffset = (bounds.getHeight() + 1) * vertRatio / 2;

        /*
         * Set proper alignment
         */
        HorizontalAlignment align = HorizontalAlignment.CENTER;
        double left = xOffset, right = xOffset;
        if (txt.getJustification() != null) {
            switch (txt.getJustification()) {
            case RIGHT_JUSTIFY:
                align = HorizontalAlignment.RIGHT;
                left = xOffset * 2;
                right = 0.0;
                break;
            case CENTER:
                align = HorizontalAlignment.CENTER;
                break;
            case LEFT_JUSTIFY:
                align = HorizontalAlignment.LEFT;
                left = 0.0;
                right = xOffset * 2;
                break;
            default:
                align = HorizontalAlignment.CENTER;
                break;
            }
        }

        dstring.horizontalAlignment = align;

        if (dstring.rotation != 0.0) {
            AffineTransformation rotate = AffineTransformation
                    .rotationInstance(dstring.rotation, dstring.basics.x,
                            dstring.basics.y);
        }

        IExtent box = new PixelExtent(dstring.basics.x - left, dstring.basics.x
                + right, dstring.basics.y - yOffset, dstring.basics.y + yOffset);

        /*
         * create new TextDisplayElement and add it to return list
         */
        TextDisplayElement tde = new TextDisplayElement(dstring,
                txt.maskText(), txt.getDisplayType(), box);
        slist.add(tde);

        return slist;
    }

    /**
     * Create IDisplayable of PGEN TCM element.
     * 
     * @param tcm
     *            A PGEN TCM Element
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ITcm tcm,
            PaintProperties paintProps) {
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        ArrayList<Coordinate> trackPts = new ArrayList<Coordinate>();

        // draw wave quarters
        slist.addAll(createDisplayElements(tcm.getWaveQuarters(), paintProps));

        // draw wind forecast quarters and labels
        for (TcmFcst tcmFcst : tcm.getTcmFcst()) {
            String[] txt = new String[2];
            Calendar fcstHr = (Calendar) tcm.getAdvisoryTime().clone();
            fcstHr.add(Calendar.HOUR_OF_DAY, tcmFcst.getFcstHr());

            if (tcmFcst.equals(tcm.getTcmFcst().get(0))) {
                txt[0] = tcm.getStormName() + "/"
                        + (int) tcm.getCentralPressure() + "mb";
                txt[1] = String.format("%1$td/%1$tH%1$tM", fcstHr);
            } else {
                txt[0] = String.format("%1$td/%1$tH%1$tM", fcstHr);
                txt[1] = "";
            }

            slist.addAll(createDisplayElements(tcmFcst, paintProps, txt));
            trackPts.add(tcmFcst.getQuarters()[0].getLocation());
        }

        // draw track
        if (trackPts.size() >= 2) {
            Line trackLn = new Line(null,
                    new Color[] { new Color(0, 255, 255) }, 1.5f, .8, false,
                    false, trackPts, 0, null, "Lines", "LINE_DASHED_6");
            slist.addAll(createDisplayElements(trackLn, paintProps));
        }

        return slist;
    }

    /**
     * Create IDisplayable of PGEN TCM forecast
     * 
     * @param tcmFcst
     *            A PGEN TCM forecast
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createDisplayElements(ITcmFcst tcmFcst,
            PaintProperties paintProps, String[] txt) {
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        for (ITcmWindQuarter qua : tcmFcst.getQuarters()) {
            slist.addAll(createDisplayElements(qua, paintProps));
        }

        Symbol ts = new Symbol(null, new Color[] { new Color(0, 255, 255) },
                2.5f, 1.5, false, tcmFcst.getQuarters()[0].getLocation(),
                "Symbol", this.getTcmFcstSymbolType(tcmFcst));
        slist.addAll(createDisplayElements(ts, paintProps));

        if (txt != null) {
            Text label = new Text(null, "Courier", 14.0f,
                    TextJustification.LEFT_JUSTIFY,
                    tcmFcst.getQuarters()[0].getLocation(), 0.0,
                    TextRotation.NORTH_RELATIVE, txt, FontStyle.REGULAR,
                    getDisplayColor(Color.YELLOW), 4, 0, false,
                    DisplayType.NORMAL, "Text", "General Text");

            slist.addAll(createDisplayElements(label, paintProps));
        }

        return slist;
    }

    /**
     * Returns the TCM symbol according to the wind speed. Hurricane >= 64 knots
     * TS >= 50 knots TD >= 32 knots Lx < 32 knots
     * 
     * @param tcmFcst
     * @return
     */
    private String getTcmFcstSymbolType(ITcmFcst tcmFcst) {
        String ret = "TROPICAL_STORM_NH";
        ITcmWindQuarter[] quarters = tcmFcst.getQuarters();

        int maxWind = 0;

        for (ITcmWindQuarter qtr : quarters) {
            double[] radius = qtr.getQuarters();
            for (double r : radius) {
                if (r > 0) {
                    if (qtr.getWindSpeed() > maxWind)
                        maxWind = qtr.getWindSpeed();
                    break;
                }
            }
        }

        double lat = quarters[0].getLocation().y;
        if (maxWind >= 64) {
            if (lat > 0)
                ret = "HURRICANE_NH";
            else
                ret = "HURRICANE_SH";
        } else if (maxWind >= 50) {
            if (lat > 0)
                ret = "TROPICAL_STORM_NH";
            else
                ret = "TROPICAL_STORM_SH";
        } else if (maxWind >= 32) {
            ret = "TROPICAL_DEPRESSION";
        } else {
            ret = "LOW_X_FILLED";
        }

        return ret;
    }

    /**
     * Create IDisplayable of PGEN TCM wind/wave quarters
     * 
     * @param quatros
     *            - PGEN TCM wind/wave quarters
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createDisplayElements(
            ITcmWindQuarter quatros, PaintProperties paintProps) {
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        Color color = Color.GREEN;
        switch (quatros.getWindSpeed()) {
        case 0:
            color = Color.GREEN;
            break;
        case 32:
            color = Color.BLUE;
            break;
        case 50:
            color = Color.YELLOW;
            break;
        case 64:
            color = Color.RED;
            break;
        }

        Coordinate center = quatros.getLocation();
        Arc quatro1 = new Arc(null, color, (float) 1.5, 1.0, false, false, 0,
                null, "Circle", center, this.calculateDestinationPointMap(
                        center, 0, quatros.getQuarters()[0]), "Arc", 1, 0, 90);
        Arc quatro2 = new Arc(null, color, (float) 1.5, 1.0, false, false, 0,
                null, "Circle", center, this.calculateDestinationPointMap(
                        center, 0, quatros.getQuarters()[1]), "Arc", 1, 90, 180);
        Arc quatro3 = new Arc(null, color, (float) 1.5, 1.0, false, false, 0,
                null, "Circle", center, this.calculateDestinationPointMap(
                        center, 0, quatros.getQuarters()[2]), "Arc", 1, 180,
                270);
        Arc quatro4 = new Arc(null, color, (float) 1.5, 1.0, false, false, 0,
                null, "Circle", center, this.calculateDestinationPointMap(
                        center, 0, quatros.getQuarters()[3]), "Arc", 1, 270,
                360);
        slist.addAll(createDisplayElements((IArc) quatro1, paintProps));
        slist.addAll(createDisplayElements((IArc) quatro2, paintProps));
        slist.addAll(createDisplayElements((IArc) quatro3, paintProps));
        slist.addAll(createDisplayElements((IArc) quatro4, paintProps));

        Line ln1 = getWindQuatroLine(getPointOnArc(quatro1, 0),
                getPointOnArc(quatro2, 0), color);

        Line ln2 = getWindQuatroLine(getPointOnArc(quatro2, 90),
                getPointOnArc(quatro3, 90), color);

        Line ln3 = getWindQuatroLine(getPointOnArc(quatro3, 180),
                getPointOnArc(quatro4, 180), color);

        Line ln4 = getWindQuatroLine(getPointOnArc(quatro4, 270),
                getPointOnArc(quatro1, 270), color);

        slist.addAll(createDisplayElements((ILine) ln1, paintProps));
        slist.addAll(createDisplayElements((ILine) ln2, paintProps));
        slist.addAll(createDisplayElements((ILine) ln3, paintProps));
        slist.addAll(createDisplayElements((ILine) ln4, paintProps));

        return slist;
    }

    /**
     * Creates a list of IDisplayable Objects from an IArc object
     * 
     * @param arc
     *            A PGEN Drawable Element of an arc object
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(IArc arc,
            PaintProperties paintProps) {
        // double sfactor = deviceScale * de.getSizeScale();
        setScales(paintProps);

        /*
         * Create the List to be returned, and wireframe shape
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        IWireframeShape arcpts = target
                .createWireframeShape(false, iDescriptor);

        /*
         * Convert center and circumference point from lat/lon to pixel
         * coordinates.
         */
        double[] tmp = { arc.getCenterPoint().x, arc.getCenterPoint().y, 0.0 };
        double[] center = iDescriptor.worldToPixel(tmp);
        double[] tmp2 = { arc.getCircumferencePoint().x,
                arc.getCircumferencePoint().y, 0.0 };
        double[] circum = iDescriptor.worldToPixel(tmp2);

        /*
         * calculate angle of major axis
         */
        double axisAngle = Math.toDegrees(Math.atan2((circum[1] - center[1]),
                (circum[0] - center[0])));
        double cosineAxis = Math.cos(Math.toRadians(axisAngle));
        double sineAxis = Math.sin(Math.toRadians(axisAngle));

        /*
         * calculate half lengths of major and minor axes
         */
        double diff[] = { circum[0] - center[0], circum[1] - center[1] };
        double major = Math.sqrt((diff[0] * diff[0]) + (diff[1] * diff[1]));
        double minor = major * arc.getAxisRatio();

        /*
         * Calculate points along the arc
         */
        // TODO - orientation issues
        // double increment = 5.0; //degrees
        double angle = arc.getStartAngle();
        int numpts = (int) Math.round(arc.getEndAngle() - arc.getStartAngle()
                + 1.0);
        double[][] path = new double[numpts][3];
        for (int j = 0; j < numpts; j++) {
            double thisSine = Math.sin(Math.toRadians(angle));
            double thisCosine = Math.cos(Math.toRadians(angle));
            // Can maybe use simpler less expensive calculations for circle,
            // if ever necessary.
            // if ( arc.getAxisRatio() == 1.0 ) {
            // path[j][0] = center[0] + (major * thisCosine );
            // path[j][1] = center[1] + (minor * thisSine );
            // }
            // else {
            path[j][0] = center[0] + (major * cosineAxis * thisCosine)
                    - (minor * sineAxis * thisSine);
            path[j][1] = center[1] + (major * sineAxis * thisCosine)
                    + (minor * cosineAxis * thisSine);
            // }

            angle += 1.0;
        }
        arcpts.addLineSegment(path);

        /*
         * Create new LineDisplayElement from wireframe shapes and add it to
         * return list
         */
        arcpts.compile();
        slist.add(new LineDisplayElement(arcpts, getDisplayColor(arc
                .getColors()[0]), arc.getLineWidth()));

        slist.addAll(adjustContourCircleLabel(arc, paintProps, path));

        return slist;
    }

    /**
     * Creates a list of IDisplayable Objects from an ITrack drawable object
     * 
     * @param track
     *            A PGEN Drawable Element of a storm track object
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ITrack track,
            PaintProperties paintProps) {
        // double sfactor = deviceScale * de.getSizeScale();
        ArrayList<IDisplayable> temps;

        ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        setScales(paintProps);

        SimpleDateFormat sdf = new SimpleDateFormat("HHmm");

        /*
         * Create the List to be returned
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        /*
         * Get color for creating displayable elements.
         */
        Color iniDspClr = getDisplayColor(track.getInitialColor());
        Color expDspClr = getDisplayColor(track.getExtrapColor());

        /*
         * Create Line element from the initial data points, create its
         * Displayables and add them to the return list
         */
        points.clear();
        for (TrackPoint pt : track.getInitialPoints()) {
            // displayTrackPoint(pt);
            points.add(pt.getLocation());
        }
        Line iline = new Line(null, new Color[] { iniDspClr },
                track.getLineWidth(), 1.0, false, false, points, 0,
                FillPattern.SOLID, "Lines", track.getInitialLinePattern());
        temps = createDisplayElements(iline, paintProps);
        slist.addAll(temps);

        /*
         * Loop through the initial data points
         */
        int n = 0;
        Coordinate[] initialPoints = new Coordinate[track.getInitialPoints().length];
        for (TrackPoint pt : track.getInitialPoints()) {
            // Add Coordinate point to array
            initialPoints[n++] = new Coordinate(pt.getLocation());
            /*
             * If there is a time associated with this point, create a
             * displayable of it
             */
            if (pt.getTime() != null) {
                String dtime = sdf.format(pt.getTime().getTime());
                Text txt = new Text(null, track.getFontName(),
                        track.getFontSize(), TextJustification.LEFT_JUSTIFY,
                        pt.getLocation(), 0.0, TextRotation.SCREEN_RELATIVE,
                        new String[] { dtime }, FontStyle.BOLD, iniDspClr, 0,
                        3, false, DisplayType.NORMAL, "Text", "General Text");
                temps = createDisplayElements((IText) txt, paintProps);
                slist.addAll(temps);
            }
        }
        /*
         * Create a SymbolLocationSet for the markers used for the Initial data
         * points
         */
        SymbolLocationSet imarkers = new SymbolLocationSet(null,
                new Color[] { iniDspClr }, track.getLineWidth(), 1.0, false,
                initialPoints, "SymbolSet", track.getInitialMarker());
        temps = createDisplayElements(imarkers, paintProps);
        slist.addAll(temps);

        /*
         * Create Line element from the extrapolated data points, create its
         * Displayables and add them to the return list
         */
        points.clear();
        /*
         * here add the last initial point first to create Line element for the
         * extrapolated data points
         */
        int lastInitialPointIndex = track.getInitialPoints().length - 1;
        points.add(track.getInitialPoints()[lastInitialPointIndex]
                .getLocation());

        for (TrackPoint pt : track.getExtrapPoints()) {
            points.add(pt.getLocation());
        }
        Line eline = new Line(null, new Color[] { expDspClr },
                track.getLineWidth(), 1.0, false, false, points, 0,
                FillPattern.SOLID, "Lines", track.getExtrapLinePattern());
        temps = createDisplayElements(eline, paintProps);
        slist.addAll(temps);

        /*
         * Loop through the Extrapolated data points
         */
        int m = 0;
        boolean[] extrapPointTimeTextDisplayIndicator = track
                .getExtraPointTimeTextDisplayIndicator();
        Coordinate[] extrapPoints = new Coordinate[track.getExtrapPoints().length];
        for (TrackPoint pt : track.getExtrapPoints()) {
            // Add Coordinate point to array
            // extrapPoints[m++] = new Coordinate(pt.getLocation());
            extrapPoints[m] = new Coordinate(pt.getLocation());
            /*
             * If there is a time associated with this point, create a
             * displayable of it
             */
            if (pt.getTime() != null && extrapPointTimeTextDisplayIndicator[m]) {
                String dtime = sdf.format(pt.getTime().getTime());
                Text txt = new Text(null, track.getFontName(),
                        track.getFontSize(), TextJustification.LEFT_JUSTIFY,
                        pt.getLocation(), 0.0, TextRotation.SCREEN_RELATIVE,
                        new String[] { dtime }, FontStyle.BOLD, expDspClr, 0,
                        3, false, DisplayType.NORMAL, "Text", "General Text");
                temps = createDisplayElements((IText) txt, paintProps);
                slist.addAll(temps);
            }
            m++;
        }
        /*
         * Create a SymbolLocationSet for the markers used for the Extrapolated
         * data points
         */
        SymbolLocationSet emarkers = new SymbolLocationSet(null,
                new Color[] { expDspClr }, track.getLineWidth(), 1.0, false,
                extrapPoints, "SymbolSet", track.getExtrapMarker());
        temps = createDisplayElements(emarkers, paintProps);
        slist.addAll(temps);

        return slist;

    }

    /**
     * Creates a list of IDisplayable Objects from an Point object
     * 
     * @param de
     *            A PGEN Drawable Element of a multipoint object
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ISymbol de,
            PaintProperties paintProps) {

        if (de instanceof Symbol) {
            Coordinate[] loc = new Coordinate[] { de.getLocation() };
            SymbolLocationSet sym = new SymbolLocationSet((Symbol) de, loc);
            return createDisplayElements(sym, paintProps);
        } else {
            return new ArrayList<IDisplayable>();
        }

    }

    /**
     * Creates a list of IDisplayable Objects from an ISymbolSet object, used to
     * draw one symbol at one or more locations.
     * 
     * @param symbolSet
     *            A symbol with associated lat/lon coordinates
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ISymbolSet symbolSet,
            PaintProperties paintProps) {

        // Set up scale factors
        setScales(paintProps);
        double sfactor = deviceScale * symbolScale
                * symbolSet.getSymbol().getSizeScale();

        /*
         * Create the List to be returned
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        // get Symbol
        Symbol sym = symbolSet.getSymbol();

        /*
         * Get color for creating displayables.
         */
        Color[] dspClr = getDisplayColors(sym.getColors());

        /*
         * create an AWT BufferedImage from the symbol pattern
         */
        sfactor *= screenToWorldRatio;
        // BufferedImage image =
        // SymbolImageUtil.createBufferedImage(sym.getPatternName(), sfactor,
        // sym.getLineWidth(),
        // sym.isClear(), dspClr[0] );
        IRenderedImageCallback imageCb = new SymbolImageCallback(
                sym.getPatternName(), sfactor, sym.getLineWidth(),
                sym.isClear(), dspClr[0]);
        /*
         * Initialize raster image for use with graphics target
         */
        IImage pic = null;
        try {
            pic = target.initializeRaster(imageCb);
            pic.stage();
            // pic = target.initializeRaster( new IODataPreparer(image,
            // sym.getPatternName(), 0), null );
        } catch (Exception e) {
            System.out.println("SAG:IMAGE CREATION");
            e.printStackTrace();
            return slist;
        }

        /*
         * convert lat/lons to pixel coords
         */
        double[][] pts = PgenUtil.latlonToPixel(symbolSet.getLocations(),
                (IMapDescriptor) iDescriptor);

        /*
         * Create SymbolSetElement and return it
         */
        slist.add(new SymbolSetElement(pic, pts));
        return slist;

    }

    /**
     * Creates a list of IDisplayable raster Objects from an ICombo object
     * 
     * @param de
     *            A PGEN Drawable Element of a ICombo object
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ICombo de,
            PaintProperties paintProps) {

        // Set up scale factors
        setScales(paintProps);
        double scale = deviceScale * symbolScale * de.getSizeScale()
                * SymbolImageUtil.INITIAL_IMAGE_SIZE;

        if (de instanceof ComboSymbol) {
            String[] patterns = de.getPatternNames();

            /*
             * Get pixel value for the world location
             */
            Coordinate[] loc = new Coordinate[] { de.getLocation() };
            double[] worldPixel = new double[] { loc[0].x, loc[0].y, 0.0 };
            double[] pixel = iDescriptor.worldToPixel(worldPixel);

            /*
             * Get color for creating displayables.
             */
            Color[] dspClr = getDisplayColors(de.getColors());

            /*
             * Calculate the offset for the first symbol ( upper left ) and
             * convert back to world coordinates.
             */
            double[] locUL = iDescriptor.pixelToWorld(new double[] {
                    pixel[0] - (0.5 * scale), pixel[1] - (0.25 * scale), 0.0 });
            Coordinate[] loc1 = new Coordinate[] { new Coordinate(locUL[0],
                    locUL[1]) };

            /*
             * Create a Symbol object for the first pattern
             */
            SymbolLocationSet sym = new SymbolLocationSet(null, dspClr,
                    de.getLineWidth(), de.getSizeScale(), de.isClear(), loc1,
                    "Symbol", patterns[0]);

            /*
             * Calculate the offset for the second symbol ( lower right ) and
             * convert back to world coordinates
             */
            double[] locLR = iDescriptor.pixelToWorld(new double[] {
                    pixel[0] + (0.5 * scale), pixel[1] + (0.25 * scale), 0.0 });
            Coordinate[] loc2 = new Coordinate[] { new Coordinate(locLR[0],
                    locLR[1]) };

            /*
             * Create a Symbol object for the second pattern
             */
            SymbolLocationSet sym2 = new SymbolLocationSet(null, dspClr,
                    de.getLineWidth(), de.getSizeScale(), de.isClear(), loc2,
                    "Symbol", patterns[1]);

            // add the "slash" symbol object
            SymbolLocationSet sym3 = new SymbolLocationSet(null, dspClr,
                    de.getLineWidth(), de.getSizeScale(), de.isClear(), loc,
                    "Symbol", "SLASH");

            /*
             * create IDisplayables from each Symbol and add to return list.
             */
            ArrayList<IDisplayable> stuff = createDisplayElements(sym,
                    paintProps);
            stuff.addAll(createDisplayElements(sym2, paintProps));
            stuff.addAll(createDisplayElements(sym3, paintProps));
            return stuff;
        } else {
            return new ArrayList<IDisplayable>();
        }

    }

    /**
     * Create IDisplayable of a TCA element.
     * 
     * @param tca
     *            A PGEN Drawable Element of a TCA element
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ITca tca,
            PaintProperties paintProps) {

        ArrayList<IDisplayable> rlist = new ArrayList<IDisplayable>();

        List<TropicalCycloneAdvisory> advisories = tca.getAdvisories();

        /*
         * If there are no current watch/warning advisories, print a text box
         * with some storm info.
         */
        if (advisories.isEmpty()) {
            String[] noneMsg = new String[3];
            noneMsg[0] = new String(tca.getStormType() + " "
                    + tca.getStormName());
            noneMsg[1] = new String("Advisory " + tca.getAdvisoryNumber());
            noneMsg[2] = new String("No current Watches/Warnings");

            Text display = new Text(null, "Courier", 14.0f,
                    TextJustification.CENTER, tca.getTextLocation(), 0.0,
                    TextRotation.SCREEN_RELATIVE, noneMsg, FontStyle.REGULAR,
                    getDisplayColor(Color.YELLOW), 0, 0, false,
                    DisplayType.BOX, "Text", "General Text");

            rlist = createDisplayElements((IText) display, paintProps);
            return rlist;
        }

        /*
         * create displayables of each watch/warning type in the following
         * order.
         */
        rlist.addAll(createAdvisoryDisplay(advisories, "Tropical Storm",
                "Watch", Color.YELLOW, 7.0f, paintProps));
        rlist.addAll(createAdvisoryDisplay(advisories, "Hurricane", "Watch",
                Color.PINK, 13.0f, paintProps));
        rlist.addAll(createAdvisoryDisplay(advisories, "Tropical Storm",
                "Warning", Color.BLUE, 7.0f, paintProps));
        rlist.addAll(createAdvisoryDisplay(advisories, "Hurricane", "Warning",
                Color.RED, 13.0f, paintProps));

        return rlist;
    }

    /**
     * Creates displayables for a specific watch/warning type from the advisory
     * information
     * 
     * @param advisories
     *            List of current tropical cyclone advisories
     * @param severe
     *            specifies "Tropical Storm" or "Hurricane" severity
     * @param type
     *            specifies whether the advisory type is a watch or warning
     * @param clr
     *            the color to use for the display
     * @param lw
     *            the line width to use for the display
     * @param paintProps
     *            The paint properties associated with the target
     * @return
     */
    private ArrayList<IDisplayable> createAdvisoryDisplay(
            List<TropicalCycloneAdvisory> advisories, String severe,
            String type, Color clr, float lw, PaintProperties paintProps) {

        ArrayList<IDisplayable> alist = new ArrayList<IDisplayable>();

        /*
         * loop through each advisory and determine if it is of the given
         * severity and type.
         */
        for (TropicalCycloneAdvisory tca : advisories) {
            if (tca.getSeverity().equals(severe)
                    && tca.getAdvisoryType().equals(type)) {

                BPGeography segment = tca.getSegment();

                /*
                 * loop through each path defining the watch/warning segment
                 */
                for (Coordinate[] coords : segment.getPaths()) {

                    // convert Coordinate[] to ArrayList<Coordinate>
                    ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
                    for (Coordinate c : coords)
                        pts.add(c);

                    // if the segment is a Waterway, and the segment is closed,
                    // create a filled displayable
                    boolean fill = false;
                    if (coords[0].equals2D(coords[coords.length - 1])
                            && (segment instanceof WaterBreakpoint))
                        fill = true;

                    // create a line for each segment and then create its
                    // displayable
                    Line seg = new Line(null, new Color[] { clr }, lw, 1.0,
                            false, fill, pts, 0, FillPattern.SOLID, "Lines",
                            "LINE_SOLID");
                    alist.addAll(createDisplayElements(seg, paintProps));

                }

            }

        }

        return alist;
    }

    /**
     * Create IDisplayable of an aviation text element.
     * 
     * @param avntxt
     *            A PGEN Drawable Element of an aviation text element
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(IAvnText avntxt,
            PaintProperties paintProps) {

        ArrayList<IDisplayable> rlist = new ArrayList<IDisplayable>();
        // Set up scale factors
        setScales(paintProps);

        String[] text = createTextString(avntxt);
        boolean bgmask = hasTextBackgroundMask(avntxt.getAvnTextType());
        DisplayType outline = hasRegularOutline(avntxt.getAvnTextType());

        Text display = new Text(null, avntxt.getFontName(),
                avntxt.getFontSize(), avntxt.getJustification(),
                avntxt.getPosition(), 0.0, TextRotation.SCREEN_RELATIVE, text,
                avntxt.getStyle(), getDisplayColor(avntxt.getTextColor()), 0,
                0, bgmask, outline, "Text", "General Text");

        ArrayList<IDisplayable> txtdable = createDisplayElements(
                (IText) display, paintProps);

        if (avntxt.getAvnTextType() == AviationTextType.HIGH_LEVEL_TURBULENCE
                || avntxt.getAvnTextType() == AviationTextType.CLOUD_LEVEL
                || avntxt.getAvnTextType() == AviationTextType.MID_LEVEL_ICING) {
            rlist.addAll(txtdable); // add text first
            rlist.add(createLineSeparator(avntxt, paintProps));
        } else if (avntxt.getAvnTextType() == AviationTextType.LOW_PRESSURE_BOX
                || avntxt.getAvnTextType() == AviationTextType.HIGH_PRESSURE_BOX) {
            rlist.addAll(createPressureBox(avntxt, paintProps));
            rlist.addAll(txtdable); // add text second
        } else {
            rlist.addAll(txtdable);
        }

        if (avntxt.getAvnTextType() == AviationTextType.LOW_LEVEL_TURBULENCE
                || avntxt.getAvnTextType() == AviationTextType.HIGH_LEVEL_TURBULENCE
                || avntxt.getAvnTextType() == AviationTextType.MID_LEVEL_ICING) {
            rlist.addAll(createAvnTextSymbol(avntxt, paintProps));
        }

        return rlist;
    }

    /*
     * Add symbols to the appropriate locations for an aviation text displayable
     */
    private ArrayList<IDisplayable> createAvnTextSymbol(IAvnText avntxt,
            PaintProperties paintProps) {

        // double imgscale = deviceScale * avntxt..getSizeScale() *
        // INITIAL_IMAGE_SIZE;
        ArrayList<IDisplayable> rlist = new ArrayList<IDisplayable>();

        /*
         * get pixel size of text string
         */
        IFont font = initializeFont(avntxt.getFontName(), avntxt.getFontSize(),
                avntxt.getStyle());
        Rectangle2D bounds = target.getStringBounds(font, "XyX");
        font.dispose();
        double vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;
        double horizRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;

        /*
         * get symbol color and calculate size of symbol
         */
        Color[] clrs = new Color[] { getDisplayColor(avntxt.getTextColor()) };
        double symSize = (bounds.getHeight())
                / (SymbolImageUtil.INITIAL_IMAGE_SIZE);

        /*
         * Get pixel value for the world location
         */
        Coordinate[] loc = new Coordinate[] { avntxt.getPosition() };
        double[] worldPixel = new double[] { loc[0].x, loc[0].y, 0.0 };
        double[] pixel = iDescriptor.worldToPixel(worldPixel);

        /*
         * Calculate pixel offset for symbol
         */

        pixel[1] -= (1.5 * vertRatio * bounds.getHeight());
        double shift = 0.5; // shift symbol 1 and one half characters
        if (avntxt.getAvnTextType() == AviationTextType.LOW_LEVEL_TURBULENCE)
            shift = 1.1667; // shift 3 and half characters
        // Adjust position based on justification
        if (avntxt.getJustification() == TextJustification.LEFT_JUSTIFY) {
            pixel[0] += (bounds.getWidth() * horizRatio * shift);
        } else if (avntxt.getJustification() == TextJustification.RIGHT_JUSTIFY) {
            pixel[0] -= (bounds.getWidth() * horizRatio * shift);
        }

        String[] ids = avntxt.getSymbolPatternName().split("\\|");

        /*
         * Add one symbol or two
         */
        if (ids.length == 1) {
            // convert pixel back to map coordinates
            double[] locSym = iDescriptor.pixelToWorld(new double[] { pixel[0],
                    pixel[1], 0.0 });
            Coordinate loc1 = new Coordinate(locSym[0], locSym[1]);

            /*
             * create a new Symbol, and then generate its IDisplayables
             */
            Symbol sym = new Symbol(null, clrs, 1.0f, symSize, false, loc1,
                    "Symbol", ids[0]);
            rlist.addAll(createDisplayElements((ISymbol) sym, paintProps));
        } else if (ids.length > 1) { // add two symbols

            double newX = pixel[0] - (0.3333 * bounds.getWidth() * horizRatio);
            // convert pixel back to map coordinates
            double[] locSym = iDescriptor.pixelToWorld(new double[] { newX,
                    pixel[1], 0.0 });
            Coordinate loc1 = new Coordinate(locSym[0], locSym[1]);

            /*
             * create the first Symbol, and then generate its IDisplayables
             */
            Symbol sym = new Symbol(null, clrs, 1.0f, symSize, false, loc1,
                    "Symbol", ids[0]);
            rlist.addAll(createDisplayElements((ISymbol) sym, paintProps));

            newX = pixel[0] + (0.3333 * bounds.getWidth() * horizRatio);
            // convert pixel back to map coordinates
            locSym = iDescriptor.pixelToWorld(new double[] { newX, pixel[1],
                    0.0 });
            Coordinate loc2 = new Coordinate(locSym[0], locSym[1]);

            /*
             * create the second Symbol, and then generate its IDisplayables
             */
            Symbol sym2 = new Symbol(null, clrs, 1.0f, symSize, false, loc2,
                    "Symbol", ids[1]);
            rlist.addAll(createDisplayElements((ISymbol) sym2, paintProps));

        }

        return rlist;
    }

    /*
     * create IDisplayables for a Low or High Pressure box
     */
    private ArrayList<IDisplayable> createPressureBox(IAvnText avntxt,
            PaintProperties paintProps) {

        // list of displayables to return;
        ArrayList<IDisplayable> dlist = new ArrayList<IDisplayable>();

        /*
         * calculate pixel size of text
         */
        IFont font = initializeFont(avntxt.getFontName(), avntxt.getFontSize(),
                avntxt.getStyle());
        Rectangle2D bounds = target.getStringBounds(font, "XyX");
        font.dispose();
        double horizRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        double vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;

        /*
         * convert position from map to pixel coordinates
         */
        double[] tmp = { avntxt.getPosition().x, avntxt.getPosition().y, 0.0 };
        double[] center = iDescriptor.worldToPixel(tmp);
        // Adjust position based on justification
        if (avntxt.getJustification() == TextJustification.LEFT_JUSTIFY) {
            center[0] += (bounds.getWidth() * horizRatio * 0.5);
        } else if (avntxt.getJustification() == TextJustification.RIGHT_JUSTIFY) {
            center[0] -= (bounds.getWidth() * horizRatio * 0.5);
        }
        double xoffset = 0.667 * horizRatio * bounds.getWidth(); // two
                                                                 // characters
                                                                 // from center
        double yoffset = vertRatio * bounds.getHeight();
        double extra = 2. * vertRatio;

        /*
         * Calculate corner points of pressure box
         */
        double[][] box = null;
        if (avntxt.getAvnTextType() == AviationTextType.LOW_PRESSURE_BOX) {
            double[] ul = new double[] { center[0] - xoffset,
                    center[1] - yoffset - extra, 0.0 };
            double[] ur = new double[] { center[0] + xoffset,
                    center[1] - yoffset - extra, 0.0 };
            double[] lr = new double[] { center[0] + xoffset,
                    center[1] + yoffset, 0.0 };
            double[] ll = new double[] { center[0] - xoffset,
                    center[1] + yoffset, 0.0 };
            double[] bottom = new double[] { center[0],
                    center[1] + (2. * yoffset) + (2 * extra), 0.0 };

            box = new double[][] { bottom, lr, ur, ul, ll, bottom };
        } else if (avntxt.getAvnTextType() == AviationTextType.HIGH_PRESSURE_BOX) {
            double[] ul = new double[] { center[0] - xoffset,
                    center[1] - yoffset + extra, 0.0 };
            double[] ur = new double[] { center[0] + xoffset,
                    center[1] - yoffset + extra, 0.0 };
            double[] lr = new double[] { center[0] + xoffset,
                    center[1] + yoffset + (3 * extra), 0.0 };
            double[] ll = new double[] { center[0] - xoffset,
                    center[1] + yoffset + (3 * extra), 0.0 };
            double[] top = new double[] { center[0],
                    center[1] - (2. * yoffset) - extra, 0.0 };

            box = new double[][] { top, ur, lr, ll, ul, top };
        }

        /*
         * create shaded shape and wireframe of pressure box, then create
         * IDisplayables of each.
         */
        if (box != null) {
            RGB bg = backgroundColor.getColor(BGColorMode.EDITOR);
            IShadedShape fill = target.createShadedShape(false, iDescriptor,
                    false);
            fill.addPolygonPixelSpace(toLineString(box), bg);
            fill.compile();
            dlist.add(new FillDisplayElement(fill, 1.0f));

            IWireframeShape outline = target.createWireframeShape(false,
                    iDescriptor);
            outline.addLineSegment(box);
            outline.compile();
            dlist.add(new LineDisplayElement(outline, getDisplayColor(avntxt
                    .getTextColor()), 1.0f));
        }

        return dlist;
    }

    /*
     * Create IDisplayable of a line Separator
     */
    private IDisplayable createLineSeparator(IAvnText avntxt,
            PaintProperties paintProps) {

        /*
         * calculate pixel size of text
         */
        IFont font = initializeFont(avntxt.getFontName(), avntxt.getFontSize(),
                avntxt.getStyle());
        Rectangle2D bounds = target.getStringBounds(font, "XyX");
        font.dispose();
        double horizRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        double vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;

        /*
         * convert position from map to pixel coordinates
         */
        double[] tmp = { avntxt.getPosition().x, avntxt.getPosition().y, 0.0 };
        double[] center = iDescriptor.worldToPixel(tmp);
        // adjust position based on justification
        if (avntxt.getJustification() == TextJustification.LEFT_JUSTIFY) {
            center[0] += (bounds.getWidth() * horizRatio * 0.5);
        } else if (avntxt.getJustification() == TextJustification.RIGHT_JUSTIFY) {
            center[0] -= (bounds.getWidth() * horizRatio * 0.5);
        }
        double xoffset = 0.667 * horizRatio * bounds.getWidth();
        double yoffset = 2.0 * vertRatio;

        /*
         * create line segment of separator, and create its IDisplayable
         */
        double[] left = new double[] { center[0] - xoffset,
                center[1] + yoffset, 0.0 };
        double[] right = new double[] { center[0] + xoffset,
                center[1] + yoffset, 0.0 };
        double[][] seg = new double[][] { left, right };
        IWireframeShape line = target.createWireframeShape(false, iDescriptor);
        line.addLineSegment(seg);
        line.compile();
        return new LineDisplayElement(line,
                getDisplayColor(avntxt.getTextColor()), 1.0f);

    }

    /*
     * determines whether the given aviation text type should have an outline
     * box
     */
    private DisplayType hasRegularOutline(AviationTextType avnTextType) {

        switch (avnTextType) {
        case FLIGHT_LEVEL:
        case FREEZING_LEVEL:
            return DisplayType.BOX;

        default:
            return DisplayType.NORMAL;
        }

    }

    /*
     * determines whether the given aviation text type should have a background
     * mask
     */
    private boolean hasTextBackgroundMask(AviationTextType avnTextType) {

        switch (avnTextType) {
        case FLIGHT_LEVEL:
        case HIGH_LEVEL_TURBULENCE:
        case MID_LEVEL_ICING:
        case CLOUD_LEVEL:
            return true;

        default:
            return false;
        }

    }

    /*
     * creates the text string to be displayed for the given aviation text type.
     */
    private String[] createTextString(IAvnText avntxt) {

        String[] ret;

        String defaultString = "XXX";
        String top = addLeadingZero(avntxt.getTopValue());
        if (top == null)
            top = defaultString;

        String bottom = null;
        if (avntxt.hasBottomValue()) {
            bottom = addLeadingZero(avntxt.getBottomValue());
        }
        if (bottom == null)
            bottom = defaultString;

        switch (avntxt.getAvnTextType()) {

        case LOW_PRESSURE_BOX:
            ret = new String[] { top, "L" };
            break;

        case HIGH_PRESSURE_BOX:
            ret = new String[] { "H", top };
            break;

        case FLIGHT_LEVEL:
            ret = new String[] { top };
            break;

        case FREEZING_LEVEL:
            StringBuilder st = new StringBuilder("0");
            st.append('\u00B0'); // degree symbol
            st.append(": ");
            st.append(top);
            ret = new String[] { st.toString() };
            break;

        case LOW_LEVEL_TURBULENCE:
            StringBuilder str = new StringBuilder(top);
            str.append('/');
            str.append(bottom);
            ret = new String[] { str.toString() };
            break;

        case HIGH_LEVEL_TURBULENCE:
        case CLOUD_LEVEL:
        case MID_LEVEL_ICING:
            ret = new String[] { top, bottom };
            break;

        default:
            ret = new String[] { defaultString };

        }

        return ret;
    }

    /*
     * Add leading zeros to a string
     */
    private String addLeadingZero(String value) {
        if (value.length() == 1)
            return new String("00" + value);
        else if (value.length() == 2)
            return new String("0" + value);
        else
            return value;
    }

    /**
     * Create IDisplayable of an mid cloud level text element.
     * 
     * @param midtxt
     *            A PGEN Drawable Element of an aviation mid level cloud text
     *            element
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(IMidCloudText midtxt,
            PaintProperties paintProps) {

        if (midtxt.isTwoColumns()) {
            return createMidCloudText_TwoColumns(midtxt, paintProps);
        } else {
            return createMidCloudText_OneColumn(midtxt, paintProps);
        }
    }

    private ArrayList<IDisplayable> createMidCloudText_OneColumn(
            IMidCloudText midtxt, PaintProperties paintProps) {

        double turblocation = 0, icelocation = 0;

        ArrayList<IDisplayable> mlist = new ArrayList<IDisplayable>();
        // Set up scale factors
        setScales(paintProps);

        // These list of Strings will be displayed in a text box; two strings
        // per
        // line; in descending row order.
        List<String> contents = new ArrayList<String>();

        // Add all cloud types and amounts to the list
        contents.addAll(getTokens(midtxt.getCloudAmounts()));
        contents.addAll(getTokens(midtxt.getCloudTypes()));

        // Add turbulence levels, if given
        if (midtxt.hasTurbulence()) {
            // if ( contents.size()%2 == 1 ) contents.add(BLANK);
            StringTokenizer tok = new StringTokenizer(
                    midtxt.getTurbulenceLevels(), "/");

            // top
            if (tok.hasMoreTokens())
                contents.add(tok.nextToken());
            else
                contents.add("XXX");

            // bottom
            if (tok.hasMoreTokens())
                contents.add(tok.nextToken());
            else
                contents.add("XXX");

            // contents.addAll( getLevels( midtxt.getTurbulenceLevels(), null )
            // );
            turblocation = contents.size() - 1.0;
        }

        // Add icing levels, if given
        if (midtxt.hasIcing()) {
            // if ( contents.size()%2 == 1 ) contents.add(BLANK);
            // contents.addAll( getLevels( midtxt.getIcingLevels(), null ) );
            StringTokenizer tok = new StringTokenizer(midtxt.getIcingLevels(),
                    "/");

            // top
            if (tok.hasMoreTokens())
                contents.add(tok.nextToken());
            else
                contents.add("XXX");

            // bottom
            if (tok.hasMoreTokens())
                contents.add(tok.nextToken());
            else
                contents.add("XXX");
            icelocation = contents.size() - 1.0;
        }

        // Add tstorm strings, if given
        if (midtxt.hasTstorm()) {
            contents.addAll(getTokens(midtxt.getTstormTypes()));
            contents.add("CB");

            StringTokenizer tok = new StringTokenizer(midtxt.getTstormLevels(),
                    "/");

            // top
            if (tok.hasMoreTokens())
                contents.add(tok.nextToken());
            else
                contents.add("XXX");

            // bottom
            if (tok.hasMoreTokens())
                contents.add(tok.nextToken());
            else
                contents.add("XXX");
        }

        if (contents.isEmpty())
            return mlist; // return empty list

        /*
         * Format the strings in the list to two per line
         */
        String[] txtstr;
        if (contents.size() == 1) {
            txtstr = new String[] { contents.get(0) };
        } else {
            txtstr = new String[contents.size()];
            for (int n = 0; n < contents.size(); n++) {
                if (midtxt.hasIcing() || midtxt.hasTurbulence()) {
                    txtstr[n] = String.format("    %-4.4s\n", contents.get(n));
                } else {
                    txtstr[n] = String.format("%-4.4s\n", contents.get(n));
                }
            }
        }

        /*
         * create Text box and its IDisplayables
         */
        Text txt = new Text(null, midtxt.getFontName(), midtxt.getFontSize(),
                midtxt.getJustification(), midtxt.getPosition(), 0.0,
                TextRotation.SCREEN_RELATIVE, txtstr, midtxt.getStyle(),
                midtxt.getTextColor(), 0, 0, true, DisplayType.BOX, "Text",
                "General Text");

        mlist.addAll(createDisplayElements((IText) txt, paintProps));

        // Add turbulence sysmbol
        if (midtxt.hasTurbulence()) {
            double voffset = turblocation - contents.size() / 2;
            mlist.addAll(createMidCloudSymbol(midtxt, voffset,
                    midtxt.getTurbulencePattern(), paintProps));
        }

        // Add icing symbol
        if (midtxt.hasIcing()) {
            double voffset = icelocation - contents.size() / 2;
            mlist.addAll(createMidCloudSymbol(midtxt, voffset,
                    midtxt.getIcingPattern(), paintProps));
        }

        return mlist;
    }

    private ArrayList<IDisplayable> createMidCloudText_TwoColumns(
            IMidCloudText midtxt, PaintProperties paintProps) {

        final String BLANK = new String(" ");
        double turblocation = 0, icelocation = 0;

        ArrayList<IDisplayable> mlist = new ArrayList<IDisplayable>();
        // Set up scale factors
        setScales(paintProps);

        // These list of Strings will be displayed in a text box; two strings
        // per
        // line; in descending row order.
        List<String> contents = new ArrayList<String>();

        // Add all cloud types and amounts to the list
        contents.addAll(getTokens(midtxt.getCloudAmounts()));
        contents.addAll(getTokens(midtxt.getCloudTypes()));

        // Add turbulence levels, if given
        if (midtxt.hasTurbulence()) {
            if (contents.size() % 2 == 1)
                contents.add(BLANK);
            contents.addAll(getLevels(midtxt.getTurbulenceLevels(), null));
            turblocation = (contents.size() / 2.0) - 1.0;
        }

        // Add icing levels, if given
        if (midtxt.hasIcing()) {
            if (contents.size() % 2 == 1)
                contents.add(BLANK);
            contents.addAll(getLevels(midtxt.getIcingLevels(), null));
            icelocation = (contents.size() / 2.0) - 1.0;
        }

        // Add tstorm strings, if given
        if (midtxt.hasTstorm()) {
            if (contents.size() % 2 == 1)
                contents.add(BLANK);
            contents.addAll(getTokens(midtxt.getTstormTypes()));
            if (contents.size() % 2 == 1)
                contents.add(BLANK);
            contents.addAll(getLevels(midtxt.getTstormLevels(), "CB"));
        }

        if (contents.isEmpty())
            return mlist; // return empty list

        /*
         * Format the strings in the list to two per line
         */
        String[] txtstr;
        if (contents.size() == 1) {
            txtstr = new String[] { contents.get(0) };
        } else {
            if (contents.size() % 2 == 1)
                contents.add(BLANK);
            txtstr = new String[contents.size() / 2];
            Iterator<String> iter = contents.iterator();
            for (int n = 0; n < contents.size() / 2; n++) {
                txtstr[n] = String.format("%-4.4s %-4.4s\n", iter.next(),
                        iter.next());
            }
        }

        /*
         * create Text box and its IDisplayables
         */
        Text txt = new Text(null, midtxt.getFontName(), midtxt.getFontSize(),
                midtxt.getJustification(), midtxt.getPosition(), 0.0,
                TextRotation.SCREEN_RELATIVE, txtstr, midtxt.getStyle(),
                midtxt.getTextColor(), 0, 0, true, DisplayType.BOX, "Text",
                "General Text");

        mlist.addAll(createDisplayElements((IText) txt, paintProps));

        // Add turbulence sysmbol
        if (midtxt.hasTurbulence()) {
            double voffset = turblocation - (contents.size() / 4.0);
            mlist.addAll(createMidCloudSymbol(midtxt, voffset,
                    midtxt.getTurbulencePattern(), paintProps));
        }

        // Add icing symbol
        if (midtxt.hasIcing()) {
            double voffset = icelocation - (contents.size() / 4.0);
            mlist.addAll(createMidCloudSymbol(midtxt, voffset,
                    midtxt.getIcingPattern(), paintProps));
        }

        return mlist;
    }

    /*
     * reads levels string in "TTT/BBB" format, and creates a list of four
     * strings. If argument note is not null, it is used as the first String in
     * the list. The second string is "TTT". The 3rd string is always BLANK. The
     * 4th string is "BBB" or "XXX" if BBB is not part of input string str
     */
    private List<String> getLevels(String str, String note) {

        ArrayList<String> lst = new ArrayList<String>();
        StringTokenizer tok = new StringTokenizer(str, "/");

        if (note == null)
            lst.add(" ");
        else
            lst.add(note);

        if (tok.hasMoreTokens())
            lst.add(tok.nextToken());
        else
            lst.add("XXX");

        lst.add(" ");

        if (tok.hasMoreTokens())
            lst.add(tok.nextToken());
        else
            lst.add("XXX");

        return lst;
    }

    /*
     * Separates a string in "AA|BB|CC| ..." format into a list of strings using
     * a "|" as a delimiter
     */
    private List<String> getTokens(String str) {

        ArrayList<String> lst = new ArrayList<String>();
        if (str == null || str.isEmpty())
            return lst;

        StringTokenizer tok = new StringTokenizer(str, "|");
        while (tok.hasMoreTokens())
            lst.add(tok.nextToken());
        return lst;

    }

    /*
     * Add symbols to the appropriate locations for an aviation mid level cloud
     * text displayable
     */
    private ArrayList<IDisplayable> createMidCloudSymbol(IMidCloudText midtxt,
            double vertOffset, String symbolPattern, PaintProperties paintProps) {

        // return list
        ArrayList<IDisplayable> rlist = new ArrayList<IDisplayable>();

        /*
         * get pixel size of text string with given font
         */
        IFont font = initializeFont(midtxt.getFontName(), midtxt.getFontSize(),
                midtxt.getStyle());
        Rectangle2D bounds = target.getStringBounds(font, "Xy");
        bounds = new Rectangle2D.Double(0.0, 0.0, bounds.getWidth() / 2.0,
                bounds.getHeight());
        font.dispose();
        double vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;
        double horizRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;

        /*
         * get symbol color and calculate size of symbol
         */
        Color[] clrs = new Color[] { getDisplayColor(midtxt.getTextColor()) };
        double symSize = (bounds.getHeight())
                / (SymbolImageUtil.INITIAL_IMAGE_SIZE);

        /*
         * Get pixel value for the world location
         */
        Coordinate[] loc = new Coordinate[] { midtxt.getPosition() };
        double[] worldPixel = new double[] { loc[0].x, loc[0].y, 0.0 };
        double[] pixel = iDescriptor.worldToPixel(worldPixel);

        /*
         * Calculate pixel offset for symbol
         */
        pixel[1] += (vertOffset * vertRatio * bounds.getHeight());
        // Adjust position based on justification
        if (midtxt.getJustification() == TextJustification.CENTER) {
            pixel[0] -= (bounds.getWidth() * horizRatio * 2.5);
        } else if (midtxt.getJustification() == TextJustification.LEFT_JUSTIFY) {
            pixel[0] += (bounds.getWidth() * horizRatio * 2.0);
        } else if (midtxt.getJustification() == TextJustification.RIGHT_JUSTIFY) {
            pixel[0] -= (bounds.getWidth() * horizRatio * 7.0);
        }

        String[] ids = symbolPattern.split("\\|");

        /*
         * Add one symbol or two
         */
        if (ids.length == 1) {
            // convert pixel back to map coordinates
            double[] locSym = iDescriptor.pixelToWorld(new double[] { pixel[0],
                    pixel[1], 0.0 });
            Coordinate loc1 = new Coordinate(locSym[0], locSym[1]);

            /*
             * create a new Symbol, and then generate its IDisplayables
             */
            Symbol sym = new Symbol(null, clrs, 1.0f, symSize, false, loc1,
                    "Symbol", ids[0]);
            rlist.addAll(createDisplayElements((ISymbol) sym, paintProps));
        } else if (ids.length > 1) { // add two symbols

            double newX = pixel[0] - (bounds.getWidth() * horizRatio);
            // convert pixel back to map coordinates
            double[] locSym = iDescriptor.pixelToWorld(new double[] { newX,
                    pixel[1], 0.0 });
            Coordinate loc1 = new Coordinate(locSym[0], locSym[1]);

            /*
             * create the first Symbol, and then generate its IDisplayables
             */
            Symbol sym = new Symbol(null, clrs, 1.0f, symSize, false, loc1,
                    "Symbol", ids[0]);
            rlist.addAll(createDisplayElements((ISymbol) sym, paintProps));

            newX = pixel[0] + (bounds.getWidth() * horizRatio);
            // convert pixel back to map coordinates
            locSym = iDescriptor.pixelToWorld(new double[] { newX, pixel[1],
                    0.0 });
            Coordinate loc2 = new Coordinate(locSym[0], locSym[1]);

            /*
             * create the second Symbol, and then generate its IDisplayables
             */
            Symbol sym2 = new Symbol(null, clrs, 1.0f, symSize, false, loc2,
                    "Symbol", ids[1]);
            rlist.addAll(createDisplayElements((ISymbol) sym2, paintProps));

        }

        return rlist;
    }

    /**
     * Applies a given line pattern to a line path.
     * 
     * @param pattern
     *            Line pattern definition.
     * @param pts
     *            Data points defining the line path.
     */
    private void handleLinePattern(LinePattern pattern, double[][] pts) {
        handleLinePattern(pattern, pts, ScaleType.SCALE_ALL_SEGMENTS);
    }

    /**
     * Applies a given line pattern to a line path.
     * 
     * @param pattern
     *            Line pattern definition.
     * @param pts
     *            Data points defining the line path.
     */
    private void handleLinePattern(LinePattern pattern, double[][] pts,
            ScaleType stype) {

        double start, end;

        /*
         * Get scale size and colors from drawable element.
         */
        double scale = elem.getSizeScale();
        if (scale <= 0.0)
            scale = 1.0;
        double sfactor = deviceScale * scale;
        Color[] clr = getDisplayColors(elem.getColors());

        /*
         * create a LineString Geometry from the data points defining the line
         * path
         */
        Coordinate[] coords = new Coordinate[pts.length];
        for (int i = 0; i < pts.length; i++) {
            coords[i] = new Coordinate(pts[i][0], pts[i][1]);
        }
        GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(coords);

        // Get the total length of the line path
        double totalDist = ls.getLength();

        /*
         * If line contains a FILLED arrow head, decrease total length of line
         * path by the length of the arrow head.
         */
        if (pattern.hasArrowHead()) {
            if (pattern.getArrowHeadType() == ArrowHeadType.FILLED) {
                totalDist -= arrow.getLength();
            }
        }

        /*
         * Create a LengthIndexedLine used to reference points along the path at
         * specific distances
         */
        LengthIndexedLine lil = new LengthIndexedLine(ls);
        LocationIndexedLine lol = new LocationIndexedLine(ls);
        LengthLocationMap llm = new LengthLocationMap(ls);

        /*
         * Calculate number of patterns that can fit on path
         */
        double psize = pattern.getLength() * sfactor;
        double numPatterns = Math.floor(totalDist / psize);
        // System.out.println("NUM_OF_PATTERN_ITERATIONS="+numPatterns+":"+psize);

        /*
         * Calculate the amount to increase or decrease the pattern length so
         * that the line path ends on a full complete pattern.
         */
        double leftover = totalDist - (numPatterns * psize);
        if (leftover > 0.5 * psize) {
            // Add one more pattern and decrease size of pattern
            numPatterns += 1.0;
            leftover = leftover - psize;
        }
        // Calculate a scale factor that will be used to adjust the size of each
        // segment in the pattern
        // double offset = 1.0 + ( leftover / (numPatterns * psize) );
        if (stype == ScaleType.SCALE_BLANK_LINE_ONLY)
            pattern = pattern.scaleBlankLineToLength(totalDist
                    / (numPatterns * sfactor));
        else
            pattern = pattern
                    .scaleToLength(totalDist / (numPatterns * sfactor));

        /*
         * If size of line is less than size of a full pattern, then default to
         * solid line
         */
        if (numPatterns < 1) {
            Coordinate[] ncoords = lil.extractLine(0.0, totalDist)
                    .getCoordinates();
            double[][] npts = toDouble(ncoords);
            wfs[0].addLineSegment(npts);
            return;
        }

        /*
         * Loop through the number times the pattern will occur along the line
         * path
         */
        double begPat = 0.0, endPat;
        LinearLocation linloc0 = llm.getLocation(begPat);
        for (int n = 0; n < (int) Math.floor(numPatterns); n++) {

            double patlen = pattern.getLength() * sfactor;// * offset;
            endPat = begPat + patlen;
            LinearLocation linloc1 = llm.getLocation(endPat);
            LengthIndexedLine sublil = new LengthIndexedLine(lol.extractLine(
                    linloc0, linloc1));

            /*
             * Loop over each segment in the pattern
             */
            double currDist = 0.0, endLoc;
            for (PatternSegment seg : pattern.getSegments()) {
                int colorNum = seg.getColorLocation();

                // if not enough colors specified, default to first color
                if (colorNum >= wfs.length)
                    colorNum = 0;

                // Calculate end location of this segment
                double seglen = seg.getLength() * sfactor; // size of pattern
                                                           // segment
                // seglen *= offset; // resize segment to account for new full
                // pattern size
                endLoc = currDist + seglen;

                /*
                 * Apply specific pattern segment
                 */
                switch (seg.getPatternType()) {

                case BLANK:
                    /*
                     * Do nothing
                     */
                    break;

                case LINE:
                    /*
                     * Extract the data points along this line segment
                     */
                    Geometry section = sublil.extractLine(currDist, endLoc);
                    Coordinate[] newcoords = section.getCoordinates();
                    /*
                     * Add line segment path to appropriate WireframeShape
                     */
                    double[][] newpts = toDouble(newcoords);
                    wfs[colorNum].addLineSegment(newpts);
                    break;

                case CIRCLE:
                    /*
                     * Use ArcPatternApplicator to calculate the points around
                     * circle and then add them to the appropriate
                     * WireframeShape
                     */
                    start = 0.0;
                    end = 360.0;
                    ArcPatternApplicator circ = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    circ.setArcAttributes(start, end, seg.getNumberInArc());
                    wfs[colorNum].addLineSegment(circ.calculateLines());
                    break;

                case CIRCLE_FILLED:
                    /*
                     * Use ArcPatternApplicator to calculate the points around
                     * circle and then add them to the appropriate ShadedShape
                     */
                    start = 0.0;
                    end = 360.0;
                    ArcPatternApplicator circf = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    circf.setArcAttributes(start, end, seg.getNumberInArc());
                    Coordinate[] carea = circf.calculateFillArea();
                    LineString[] circle = toLineString(carea);
                    ss.addPolygonPixelSpace(circle,
                            new RGB(clr[seg.getColorLocation()].getRed(),
                                    clr[seg.getColorLocation()].getGreen(),
                                    clr[seg.getColorLocation()].getBlue()));
                    break;

                case ARC_180_DEGREE:
                    if (seg.isReverseSide()) {
                        start = 0.0;
                        end = 180.0;
                    } else {
                        start = 0.0;
                        end = -180.0;
                    }
                    /*
                     * Use ArcPatternApplicator to calculate the points around a
                     * 180 degree arc and then add them to the appropriate
                     * WireframeShape
                     */
                    ArcPatternApplicator app180 = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    app180.setArcAttributes(start, end, seg.getNumberInArc());
                    wfs[colorNum].addLineSegment(app180.calculateLines());
                    break;

                case ARC_180_DEGREE_FILLED:
                    if (seg.isReverseSide()) {
                        start = 0.0;
                        end = 180.0;
                    } else {
                        start = 0.0;
                        end = -180.0;
                    }
                    /*
                     * Use ArcPatternApplicator to calculate the points around a
                     * 180 degree arc. The addSegmentToFill method ensures
                     * points along path are added to points along the arc,
                     * creating a closed shape
                     */
                    ArcPatternApplicator app = new ArcPatternApplicator(sublil,
                            currDist, endLoc);
                    app.setArcAttributes(start, end, seg.getNumberInArc());
                    app.addSegmentToFill(true);
                    Coordinate[] area = app.calculateFillArea();
                    LineString[] arc = toLineString(area);
                    /*
                     * Add fill area to the appropriate ShadedShape and add line
                     * segment path to the appropriate WireframeShape.
                     */
                    ss.addPolygonPixelSpace(arc,
                            new RGB(clr[seg.getColorLocation()].getRed(),
                                    clr[seg.getColorLocation()].getGreen(),
                                    clr[seg.getColorLocation()].getBlue()));
                    wfs[colorNum].addLineSegment(app.getSegmentPts());
                    break;

                case ARC_180_DEGREE_CLOSED:
                    if (seg.isReverseSide()) {
                        start = 0.0;
                        end = 180.0;
                    } else {
                        start = 0.0;
                        end = -180.0;
                    }
                    /*
                     * Use ArcPatternApplicator to calculate the points around a
                     * 180 degree arc
                     */
                    ArcPatternApplicator app180c = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    app180c.setArcAttributes(start, end, seg.getNumberInArc());
                    /*
                     * Add points along arc and line segment path to the
                     * appropriate WireframeShape.
                     */
                    wfs[colorNum].addLineSegment(app180c.calculateLines());
                    wfs[colorNum].addLineSegment(app180c.getSegmentPts());
                    break;

                case ARC_90_DEGREE:
                    if (seg.isReverseSide()) {
                        start = 0.0;
                        end = 90.0;
                    } else {
                        start = 0.0;
                        end = -90.0;
                    }
                    /*
                     * Use ArcPatternApplicator to calculate the points around a
                     * 90 degree arc
                     */
                    ArcPatternApplicator app90 = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    app90.setArcAttributes(start, end, seg.getNumberInArc());
                    /*
                     * Add points along arc and line segment path to the
                     * appropriate WireframeShape.
                     */
                    wfs[colorNum].addLineSegment(app90.calculateLines());
                    wfs[colorNum].addLineSegment(app90.getSegmentPts());
                    break;

                case ARC_270_DEGREE:
                    if (seg.isReverseSide()) {
                        start = -45.0;
                        end = 225.0;
                    } else {
                        start = 45.0;
                        end = -225.0;
                    }
                    /*
                     * Use ArcPatternApplicator to calculate the points around a
                     * 270 degree arc and then add them to the appropriate
                     * WireframeShape
                     */
                    ArcPatternApplicator app270 = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    app270.setArcAttributes(start, end, seg.getNumberInArc());
                    wfs[colorNum].addLineSegment(app270.calculateLines());
                    break;

                case ARC_270_DEGREE_WITH_LINE:
                    if (seg.isReverseSide()) {
                        start = -45.0;
                        end = 225.0;
                    } else {
                        start = 45.0;
                        end = -225.0;
                    }
                    /*
                     * Use ArcPatternApplicator to calculate the points around a
                     * 270 degree arc
                     */
                    ArcPatternApplicator app270l = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    app270l.setArcAttributes(start, end, seg.getNumberInArc());
                    /*
                     * Add points along arc and line segment path to the
                     * appropriate WireframeShape.
                     */
                    wfs[colorNum].addLineSegment(app270l.calculateLines());
                    wfs[colorNum].addLineSegment(app270l.getSegmentPts());
                    break;

                case BOX:
                    /*
                     * Use CornerPatternApplicator to calculate the coordinates
                     * of the box and add the pattern segments to the
                     * appropriate WireframeShape
                     */
                    CornerPatternApplicator box = new CornerPatternApplicator(
                            sublil, currDist, endLoc);
                    box.setHeight((double) seg.getOffsetSize() * sfactor);
                    box.setPatternType(CornerPattern.BOX);
                    wfs[colorNum].addLineSegment(box.calculateLines());
                    break;

                case BOX_FILLED:
                    /*
                     * Use CornerPatternApplicator to calculate the coordinates
                     * of the box and add the pattern segments to the
                     * appropriate ShadedShape
                     */
                    CornerPatternApplicator boxf = new CornerPatternApplicator(
                            sublil, currDist, endLoc);
                    boxf.setHeight((double) seg.getOffsetSize() * sfactor);
                    boxf.setPatternType(CornerPattern.BOX);
                    Coordinate[] boxarea = boxf.calculateFillArea();
                    LineString[] barea = toLineString(boxarea);
                    ss.addPolygonPixelSpace(barea,
                            new RGB(clr[seg.getColorLocation()].getRed(),
                                    clr[seg.getColorLocation()].getGreen(),
                                    clr[seg.getColorLocation()].getBlue()));
                    break;

                case X_PATTERN:
                    /*
                     * Use CornerPatternApplicator to calculate both slashes of
                     * the "X" pattern
                     */
                    CornerPatternApplicator ex = new CornerPatternApplicator(
                            sublil, currDist, endLoc);
                    ex.setHeight((double) seg.getOffsetSize() * sfactor);
                    ex.setPatternType(CornerPattern.X_PATTERN);
                    double[][] exes = ex.calculateLines();
                    double[][] slash1 = new double[][] { exes[0], exes[1] };
                    double[][] slash2 = new double[][] { exes[2], exes[3] };
                    /*
                     * Add both slash segments to appropriate WireframeShape
                     */
                    wfs[colorNum].addLineSegment(slash1);
                    wfs[colorNum].addLineSegment(slash2);
                    break;

                case Z_PATTERN:
                    /*
                     * Use CornerPatternApplicator to calculate the "Z" pattern
                     * and add the pattern segments to the appropriate
                     * WireframeShape
                     */
                    CornerPatternApplicator ze = new CornerPatternApplicator(
                            sublil, currDist, endLoc);
                    ze.setHeight((double) seg.getOffsetSize() * sfactor);
                    ze.setPatternType(CornerPattern.Z_PATTERN);
                    wfs[colorNum].addLineSegment(ze.calculateLines());
                    break;

                case DOUBLE_LINE:
                    /*
                     * Use CornerPatternApplicator to calculate both top and
                     * bottom line segments along either side of line path.
                     */
                    CornerPatternApplicator dl = new CornerPatternApplicator(
                            sublil, currDist, endLoc);
                    dl.setHeight((double) seg.getOffsetSize() * sfactor);
                    dl.setPatternType(CornerPattern.DOUBLE_LINE);
                    double[][] segs = dl.calculateLines();
                    double[][] top = new double[][] { segs[0], segs[1] };
                    double[][] bottom = new double[][] { segs[2], segs[3] };
                    /*
                     * Add top and bottom line segments to appropriate
                     * WireframeShape
                     */
                    wfs[colorNum].addLineSegment(top);
                    wfs[colorNum].addLineSegment(bottom);
                    break;

                case TICK:
                    /*
                     * use CornerPatternApplicator to calculate tick segment
                     */
                    CornerPatternApplicator tick = new CornerPatternApplicator(
                            sublil, currDist, endLoc);
                    tick.setHeight((double) seg.getOffsetSize() * sfactor);
                    tick.setPatternType(CornerPattern.TICK);
                    /*
                     * Add tick segment and the line path segment to the
                     * appropriate WireframeShape.
                     */
                    wfs[colorNum].addLineSegment(tick.getSegmentPts());
                    wfs[colorNum].addLineSegment(tick.calculateLines());
                    break;

                case ARROW_HEAD:
                    start = -120.0;
                    end = 120.0;
                    /*
                     * Use ArcPatternApplicator to calculate the points around
                     * the arc
                     */
                    ArcPatternApplicator arrow = new ArcPatternApplicator(
                            sublil, currDist, endLoc);
                    arrow.setArcAttributes(start, end, seg.getNumberInArc());
                    /*
                     * Add points along arc and line segment path to the
                     * appropriate WireframeShape.
                     */
                    wfs[colorNum].addLineSegment(arrow.calculateLines());
                    wfs[colorNum].addLineSegment(arrow.getSegmentPts());
                    break;

                default:
                    /*
                     * Do nothing.
                     */
                    System.out.println("Pattern definition: "
                            + seg.getPatternType().toString()
                            + " is not found.  Ignoring...");
                    break;
                }

                /*
                 * Update the starting location of the next segment to the
                 * ending location of the current segment.
                 */
                currDist = endLoc;

            }

            begPat = endPat;
            linloc0 = linloc1;

        }

    }

    /**
     * Change format of an array of points from Coordinate[] to double[][]
     * 
     * @param coords
     *            - input data points
     * @return data points in new format
     */
    protected double[][] toDouble(Coordinate[] coords) {

        double[][] dpts = new double[coords.length][3];

        for (int k = 0; k < coords.length; k++) {
            dpts[k][0] = coords[k].x;
            dpts[k][1] = coords[k].y;
        }

        return dpts;
    }

    /**
     * Change format of an array of points from Coordinate[] to LineString
     * 
     * @param coords
     *            - input data points
     * @return data points in new format
     */
    protected LineString[] toLineString(Coordinate[] coords) {

        LineString[] ls = new LineString[] { gf.createLineString(coords) };
        return ls;
    }

    /**
     * Change format of an array of points from Coordinate[] to LineString
     * 
     * @param coords
     *            - input data points
     * @return data points in new format
     */
    protected LineString[] toLineString(double[][] points) {

        Coordinate[] coords = new Coordinate[points.length];
        for (int j = 0; j < points.length; j++) {
            coords[j] = new Coordinate(points[j][0], points[j][1]);
        }

        LineString[] ls = new LineString[] { gf.createLineString(coords) };
        return ls;
    }

    /**
     * Makes sure last data point is the same as the first
     * 
     * @param data
     *            Input data points
     * @return Same data points with first and last point the same
     */
    private double[][] ensureClosed(double[][] data) {

        int n = data.length - 1;

        /*
         * if first point equals last point, return data
         */
        if ((data[0][0] == data[n][0]) && (data[0][1] == data[n][1])) {
            return data;
        } else {
            /*
             * add first point to end of data, and return new data points
             */
            double[][] newdata = new double[data.length + 1][3];
            for (int i = 0; i < data.length; i++)
                newdata[i] = data[i];
            newdata[data.length] = newdata[0];
            return newdata;
        }
    }

    /**
     * Apply a fill pattern to a Line path.
     * 
     * @param area
     *            data points defining the area to fill
     * @return A fill element with a ShadedShape ready for display
     */
    private FillDisplayElement createFill(double[][] area) {

        /*
         * create ShadedShape for fill area
         */
        IShadedShape fillarea = target.createShadedShape(false, iDescriptor,
                true);

        /*
         * If Requested Fill is not SOLID or TRANSPARENCY, get the fill pattern
         * and apply it to the ShadedShape
         */
        if (elem.getFillPattern() != FillPattern.TRANSPARENCY
                && elem.getFillPattern() != FillPattern.SOLID) {
            FillPatternList fpl = new FillPatternList();
            byte[] fpattern = fpl.getFillPattern(elem.getFillPattern());
            fillarea.setFillPattern(fpattern);
        }

        /*
         * Convert double[][] to Coordinate[]
         */
        Coordinate[] coords = new Coordinate[area.length];
        for (int i = 0; i < area.length; i++) {
            coords[i] = new Coordinate(area[i][0], area[i][1]);
        }

        /*
         * Create LineString[] from Coordinates[]
         */
        LineString[] ls = toLineString(coords);

        /*
         * Add fill area to Shaded Shape
         */
        Color[] dspClr = getDisplayColors(elem.getColors());
        Color fillClr = dspClr[0];
        if (dspClr.length > 1 && dspClr[1] != null) {
            fillClr = dspClr[1];
        }

        fillarea.addPolygonPixelSpace(
                ls,
                new RGB(fillClr.getRed(), fillClr.getGreen(), fillClr.getBlue()));
        fillarea.compile();

        float alpha = 1.0f;
        // TODO - decide where to get alpha - currently hardcoded at 0.5
        if (elem.getFillPattern() == FillPattern.TRANSPARENCY)
            alpha = 0.5f;

        /*
         * return new FillDisplayElement with new ShadedShape
         */
        return new FillDisplayElement(fillarea, alpha);

    }

    /**
     * Determines an appropriate scale factor to use when calculating the the
     * coordinates of the Displayables. Also sets a screen to pixel ratio for
     * use when needing to convert the size of something from screen relative to
     * pixel relative
     * 
     * @param props
     *            The paint properties associated with the target
     */
    protected void setScales(PaintProperties props) {

        /*
         * Sets the device scale factor based on the current pixel extent
         */
        IExtent pe = props.getView().getExtent();
        deviceScale = pe.getHeight() / 300.0;

        /*
         * Set the screen to pixel ratio
         */
        Rectangle bounds = props.getCanvasBounds();
        screenToExtent = pe.getHeight() / bounds.height;

        screenToWorldRatio = bounds.width / pe.getWidth();
    }

    /**
     * Calculates the angle difference of "north" relative to the screen's
     * y-axis at a given lat/lon location.
     * 
     * @param loc
     *            - The point location in Lat/Lon coordinates
     * @return The angle difference of "north" versus pixel coordinate's y-axis
     */
    private double northOffsetAngle(Coordinate loc) {
        double delta = 0.05;

        /*
         * Calculate points in pixel coordinates just south and north of
         * original location.
         */
        double[] south = { loc.x, loc.y - delta, 0.0 };
        double[] pt1 = iDescriptor.worldToPixel(south);

        double[] north = { loc.x, loc.y + delta, 0.0 };
        double[] pt2 = iDescriptor.worldToPixel(north);

        // TODO - Orientation issues here!
        return -90.0
                - Math.toDegrees(Math.atan2((pt2[1] - pt1[1]),
                        (pt2[0] - pt1[0])));
    }

    /**
     * Creates IDisplayables of a hash mark representing wind direction
     * 
     * @param vect
     *            A PGEN Drawable Element of a wind object
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createHashMark(IVector vect) {
        double sfactor = 10.0 * deviceScale; // scale factor for length of hash
                                             // lines
        double spaceFactor = sfactor * 0.25; // scale factor for spacing between
                                             // hash lines
        double spacing = 1.0 * spaceFactor; // distance between hash lines
        if (vect.getLineWidth() > 3.0)
            spacing += (0.25 * spaceFactor * (vect.getLineWidth() - 3));

        double scaleSize = sfactor * vect.getSizeScale(); // hash line length

        /*
         * Create the List to be returned, and wireframe shape
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        IWireframeShape hash = target.createWireframeShape(false, iDescriptor);

        /*
         * Convert location from lat/lon coordinates to pixel coordinates
         */
        double[] tmp = { vect.getLocation().x, vect.getLocation().y, 0.0 };
        double[] start = iDescriptor.worldToPixel(tmp);

        // TODO - Orientation issues
        /*
         * calculate the angle and distance to the four points defining the hash
         * mark
         */
        double angle = northOffsetAngle(vect.getLocation())
                + vect.getDirection();
        double theta = Math.toDegrees(Math.atan(spacing / scaleSize));
        double dist = 0.5 * Math.sqrt((spacing * spacing)
                + (scaleSize * scaleSize));

        /*
         * find the X and Y offsets from the center to the end points of hash
         * lines
         */
        double dX1 = dist * Math.cos(Math.toRadians(angle - theta));
        double dY1 = dist * Math.sin(Math.toRadians(angle - theta));

        double dX2 = dist * Math.cos(Math.toRadians(angle + theta));
        double dY2 = dist * Math.sin(Math.toRadians(angle + theta));

        /*
         * add both hash mark lines to the wireframe
         */
        hash.addLineSegment(new double[][] {
                { start[0] + dX1, start[1] - dY1, 0.0 },
                { start[0] - dX2, start[1] + dY2, 0.0 } });

        hash.addLineSegment(new double[][] {
                { start[0] - dX1, start[1] + dY1, 0.0 },
                { start[0] + dX2, start[1] - dY2, 0.0 } });

        /*
         * compile wireframe and add it to the return list
         */
        hash.compile();
        slist.add(new LineDisplayElement(hash,
                getDisplayColor(vect.getColor()), vect.getLineWidth()));
        return slist;
    }

    /**
     * Creates IDisplayables of multiple hash marks representing wind direction
     * 
     * @param vect
     *            A PGEN Drawable Element of a wind object
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createHashMarks(List<IVector> vectors) {
        double sfactor = 10.0 * deviceScale; // scale factor for length of hash
                                             // lines
        double spaceFactor = sfactor * 0.25; // scale factor for spacing between
                                             // hash lines
        double spacing = 1.0 * spaceFactor; // distance between hash lines

        /*
         * Each returned IDisplayable can have only one color, so must build
         * separate IWireframeShape for each color. Keep track in map...
         */
        Map<Color, IWireframeShape> hashMarkMap = new HashMap<Color, IWireframeShape>();

        float lineWidth = vectors.get(0).getLineWidth(); // TODO: Generalize?
                                                         // (Assumes all vectors
                                                         // have same line
                                                         // width)

        for (IVector vect : vectors) {

            if (vect.getLineWidth() > 3.0)
                spacing += (0.25 * spaceFactor * (vect.getLineWidth() - 3));

            double scaleSize = sfactor * vect.getSizeScale(); // hash line
                                                              // length

            Color color = vect.getColor(); // display color for this vector

            /*
             * Get the cumulative shape we're (possibly) constructing for this
             * color. If no such shape yet, start one for that color.
             */
            IWireframeShape hashMarks = hashMarkMap.get(color);
            if (hashMarks == null) {
                hashMarks = target.createWireframeShape(false, iDescriptor);
                hashMarkMap.put(color, hashMarks);
            }

            /*
             * Convert location from lat/lon coordinates to pixel coordinates
             */
            double[] tmp = { vect.getLocation().x, vect.getLocation().y, 0.0 };
            double[] start = iDescriptor.worldToPixel(tmp);

            // TODO - Orientation issues
            /*
             * calculate the angle and distance to the four points defining the
             * hash mark
             */
            double angle = northOffsetAngle(vect.getLocation())
                    + vect.getDirection();
            double theta = Math.toDegrees(Math.atan(spacing / scaleSize));
            double dist = 0.5 * Math.sqrt((spacing * spacing)
                    + (scaleSize * scaleSize));

            /*
             * find the X and Y offsets from the center to the end points of
             * hash lines
             */
            double dX1 = dist * Math.cos(Math.toRadians(angle - theta));
            double dY1 = dist * Math.sin(Math.toRadians(angle - theta));

            double dX2 = dist * Math.cos(Math.toRadians(angle + theta));
            double dY2 = dist * Math.sin(Math.toRadians(angle + theta));

            /*
             * add both hash mark lines to the wireframe
             */
            hashMarks.addLineSegment(new double[][] {
                    { start[0] + dX1, start[1] - dY1, 0.0 },
                    { start[0] - dX2, start[1] + dY2, 0.0 } });

            hashMarks.addLineSegment(new double[][] {
                    { start[0] - dX1, start[1] + dY1, 0.0 },
                    { start[0] + dX2, start[1] - dY2, 0.0 } });

            /*
             * compile wireframe and add it to the return list
             */
            // hashMarks.compile();
            // slist.add( new LineDisplayElement(hashMarks, getDisplayColor(
            // vect.getColor() ), vect.getLineWidth()) );

        } // end for each vector

        /*
         * Create the List to be returned
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        /*
         * For each color encountered above, compile the accumulated wireframes
         * of that color, package them into a single display element, and add to
         * return list
         */
        for (Color color : hashMarkMap.keySet()) {
            IWireframeShape hashMarks = hashMarkMap.get(color);
            hashMarks.compile();
            slist.add(new LineDisplayElement(hashMarks, color, lineWidth));
        }

        return slist;
    }

    /**
     * Creates IDisplayables of a arrow representing wind direction
     * 
     * @param vect
     *            A PGEN Drawable Element of a wind object
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createArrow(IVector vect) {
        double sfactor = deviceScale * vect.getSizeScale();

        /*
         * Create the List to be returned, and wireframe shape
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        IWireframeShape arrow = target.createWireframeShape(false, iDescriptor);

        /*
         * Convert location from lat/lon coordinates to pixel
         */
        double[] tmp = { vect.getLocation().x, vect.getLocation().y, 0.0 };
        double[] start = iDescriptor.worldToPixel(tmp);

        /*
         * calculate the length of the arrow, and its direction
         */
        double speed = 10.;
        if (!vect.hasDirectionOnly())
            speed = vect.getSpeed();
        double arrowLength = sfactor * speed;
        // TODO - orientation issues

        // Reverse 180 degrees since wind direction is the direction where the
        // wind comes from.
        // double angle = -90.0 - northOffsetAngle(vect.getLocation()) -
        // vect.getDirection();
        double angle = 90.0 - northOffsetAngle(vect.getLocation())
                + vect.getDirection();

        /*
         * find the end point (tip of the arrow)
         */
        double[] end = new double[3];
        end[0] = start[0] + (arrowLength * Math.cos(Math.toRadians(angle)));
        end[1] = start[1] + (arrowLength * Math.sin(Math.toRadians(angle)));
        end[2] = 0.0;

        /*
         * add shaft of arrow to wireframe
         */
        arrow.addLineSegment(new double[][] { start, end });

        /*
         * create shadedshape of the arrow head
         */
        double pointAngle = 60.0;
        double height = deviceScale * vect.getArrowHeadSize() * 2;
        ArrowHead head = new ArrowHead(new Coordinate(end[0], end[1]),
                pointAngle, angle, height, ArrowHeadType.FILLED);
        Coordinate[] ahead = head.getArrowHeadShape();
        Color clr = getDisplayColor(vect.getColor());
        IShadedShape arrowHead = target.createShadedShape(false, iDescriptor,
                false);
        arrowHead.addPolygonPixelSpace(toLineString(ahead),
                new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));

        /*
         * Create background arrow, if background mask is requested
         */
        if (vect.hasBackgroundMask()) {

            /*
             * get background color
             */
            RGB bg = backgroundColor.getColor(BGColorMode.EDITOR);
            Color bgColor = new Color(bg.red, bg.green, bg.blue);

            /*
             * Add shaft and arrow head coordinates to mask wireframe, and add
             * mask wireframe to return list
             */
            IWireframeShape mask = target.createWireframeShape(false,
                    iDescriptor);
            mask.addLineSegment(new double[][] { start, end });
            mask.addLineSegment(toDouble(ahead));
            mask.compile();
            slist.add(new LineDisplayElement(mask, bgColor, (float) (vect
                    .getLineWidth() + deviceScale)));

        }

        /*
         * Get color for creating displayables.
         */
        Color dspClr = getDisplayColor(vect.getColor());

        /*
         * add shaft wireframe to return list
         */
        arrow.compile();
        slist.add(new LineDisplayElement(arrow, dspClr, vect.getLineWidth()));

        /*
         * Add arrow head to return list
         */
        FillDisplayElement fde = new FillDisplayElement(arrowHead, vect
                .getColor().getAlpha());
        slist.add(fde);

        return slist;
    }

    /**
     * Creates IDisplayables of multiple arrows representing wind direction
     * 
     * @param vect
     *            A PGEN Drawable Element of a wind object
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createArrows(List<IVector> vectors) {

        /*
         * Each returned IDisplayable can have only one color, so must build
         * separate shapes for each color. Keep track of all this in map...
         */
        Map<Color, IWireframeShape> arrowMap = new HashMap<Color, IWireframeShape>();
        Map<Color, IWireframeShape> maskMap = new HashMap<Color, IWireframeShape>();
        Map<Color, IShadedShape> arrowHeadMap = new HashMap<Color, IShadedShape>();

        float lineWidth = vectors.get(0).getLineWidth(); // TODO: Generalize?
                                                         // (Assumes all vectors
                                                         // have same line
                                                         // width)

        for (IVector vect : vectors) {
            /*
             * Get color for creating displayables.
             */
            Color color = getDisplayColor(vect.getColor());
            Color bgColor = new Color(0, 0, 0); // default black
            double sfactor = deviceScale * vect.getSizeScale();

            /*
             * Get the 3 cumulative shapes we're (possibly) constructing for
             * this color (or bgColor). If no such shape yet, start one for that
             * color.
             */
            IWireframeShape arrows = arrowMap.get(color);
            if (arrows == null) {
                arrows = target.createWireframeShape(false, iDescriptor);
                arrowMap.put(color, arrows);
            }
            IWireframeShape masks = null;
            if (vect.hasBackgroundMask()) {
                RGB bg = backgroundColor.getColor(BGColorMode.EDITOR);
                bgColor = new Color(bg.red, bg.green, bg.blue);
                masks = maskMap.get(bgColor);
                if (masks == null) {
                    masks = target.createWireframeShape(false, iDescriptor);
                    maskMap.put(bgColor, masks);
                }
            }
            IShadedShape arrowHeads = arrowHeadMap.get(color);
            if (arrowHeads == null) {
                arrowHeads = target
                        .createShadedShape(false, iDescriptor, false);
                arrowHeadMap.put(color, arrowHeads);
            }

            /*
             * Convert location from lat/lon coordinates to pixel
             */
            double[] tmp = { vect.getLocation().x, vect.getLocation().y, 0.0 };
            double[] start = iDescriptor.worldToPixel(tmp);

            /*
             * calculate the length of the arrow, and its direction
             */
            double speed = 10.;
            if (!vect.hasDirectionOnly())
                speed = vect.getSpeed();
            double arrowLength = sfactor * speed;
            // TODO - orientation issues

            // Reverse 180 degrees since wind direction is the direction where
            // the wind comes from.
            // double angle = -90.0 - northOffsetAngle(vect.getLocation()) -
            // vect.getDirection();
            double angle = 90.0 - northOffsetAngle(vect.getLocation())
                    + vect.getDirection();

            /*
             * find the end point (tip of the arrow)
             */
            double[] end = new double[3];
            end[0] = start[0] + (arrowLength * Math.cos(Math.toRadians(angle)));
            end[1] = start[1] + (arrowLength * Math.sin(Math.toRadians(angle)));
            end[2] = 0.0;

            /*
             * add shaft of arrow to wireframe
             */
            arrows.addLineSegment(new double[][] { start, end });

            /*
             * create shadedshape of the arrow head
             */
            double pointAngle = 60.0;
            double height = deviceScale * vect.getArrowHeadSize() * 2;
            ArrowHead head = new ArrowHead(new Coordinate(end[0], end[1]),
                    pointAngle, angle, height, ArrowHeadType.FILLED);
            Coordinate[] ahead = head.getArrowHeadShape();
            arrowHeads.addPolygonPixelSpace(toLineString(ahead),
                    new RGB(color.getRed(), color.getGreen(), color.getBlue()));

            /*
             * Create background arrow, if background mask is requested
             */
            if (vect.hasBackgroundMask()) {
                /*
                 * Add shaft and arrow head coordinates to mask wireframe
                 */
                masks.addLineSegment(new double[][] { start, end });
                masks.addLineSegment(toDouble(ahead));
            }

        } // end for each vector

        /*
         * Create the List to be returned
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        /*
         * For each color encountered above, compile the accumulated wireframes
         * (or shaded shapes) of that color, package them into a single display
         * element, and add to return list
         */

        for (Color color : arrowMap.keySet()) {
            IWireframeShape arrows = arrowMap.get(color);
            arrows.compile();
            slist.add(new LineDisplayElement(arrows, color, lineWidth));
        }

        for (Color color : maskMap.keySet()) {
            IWireframeShape masks = maskMap.get(color);
            masks.compile();
            slist.add(new LineDisplayElement(masks, color,
                    (float) (lineWidth + deviceScale)));
        }

        for (Color color : arrowMap.keySet()) {
            IWireframeShape arrows = arrowMap.get(color);
            arrows.compile();
            slist.add(new LineDisplayElement(arrows, color, lineWidth));
        }

        for (Color color : arrowHeadMap.keySet()) {
            IShadedShape arrowHeads = arrowHeadMap.get(color);
            arrowHeads.compile();
            slist.add(new FillDisplayElement(arrowHeads, color.getAlpha()));
        }

        return slist;
    }

    /**
     * Creates IDisplayables of a wind barb
     * 
     * @param vect
     *            A PGEN Drawable Element of a wind object
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createWindBarb(IVector vect) {
        double sfactor = deviceScale * vect.getSizeScale() * 10.;
        IWireframeShape mask = null;
        Color bgColor = new Color(0, 0, 0); // default black

        /*
         * Create the List to be returned, and wireframe shape
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        IWireframeShape barb = target.createWireframeShape(false, iDescriptor);
        IShadedShape flags = target
                .createShadedShape(false, iDescriptor, false);
        if (vect.hasBackgroundMask()) {
            mask = target.createWireframeShape(false, iDescriptor);
            RGB bg = backgroundColor.getColor(BGColorMode.EDITOR);
            bgColor = new Color(bg.red, bg.green, bg.blue);
        }

        /*
         * Get color for creating displayables.
         */
        Color dspClr = getDisplayColor(vect.getColor());

        /*
         * Convert location from lat/lon coordinates to pixel
         */
        double[] tmp = { vect.getLocation().x, vect.getLocation().y, 0.0 };
        double[] start = iDescriptor.worldToPixel(tmp);

        /*
         * If calm wind, draw circle
         */
        if (vect.getSpeed() < 0.5) {
            double[][] pts = calculateCircle(start, sfactor * 0.1);
            if (vect.hasBackgroundMask()) {
                mask.addLineSegment(pts);
                mask.compile();
                slist.add(new LineDisplayElement(mask, bgColor, vect
                        .getLineWidth() + (float) deviceScale));
            }
            barb.addLineSegment(pts);
            barb.compile();
            slist.add(new LineDisplayElement(barb, dspClr, vect.getLineWidth()));
            return slist;
        }

        /*
         * Compute the number of flags, whole barbs and half barbs needed to
         * represent the wind speed.
         */
        int speed = (int) Math.floor(vect.getSpeed() + 2.5);
        int numflags = speed / 50;
        int remainder = speed % 50;
        int numbarbs = remainder / 10;
        remainder = remainder % 10;
        int halfbarbs = remainder / 5;

        double MAX_SEGMENTS = 6.0; // Maximum number of segments on original
                                   // size barb
        int numsegs = (2 * numflags) + numbarbs + halfbarbs;
        double segmentSpacing = sfactor / MAX_SEGMENTS;
        double windLength = segmentSpacing * Math.max(MAX_SEGMENTS, numsegs);
        double barbLength = sfactor / 3.0;

        /*
         * find the end point of the wind barb
         */
        // TODO - orientation issues
        double angle = -90.0 - northOffsetAngle(vect.getLocation())
                + vect.getDirection();
        double[] end = new double[3];
        end[0] = start[0] + (windLength * Math.cos(Math.toRadians(angle)));
        end[1] = start[1] + (windLength * Math.sin(Math.toRadians(angle)));
        end[2] = 0.0;
        barb.addLineSegment(new double[][] { start, end });
        if (vect.hasBackgroundMask())
            mask.addLineSegment(new double[][] { start, end });

        /*
         * Create a LengthIndexedLine used to reference points along the path at
         * specific distances
         */
        LineString[] ls = toLineString(new Coordinate[] {
                new Coordinate(start[0], start[1]),
                new Coordinate(end[0], end[1]) });
        LengthIndexedLine lil = new LengthIndexedLine(ls[0]);
        double currentLoc = lil.getEndIndex(); // start from tail end

        // TODO - orientation issues
        double BARB_ANGLE = 70.0;
        double barbAngle = angle + BARB_ANGLE;
        if (vect.getLocation().y < 0.0)
            barbAngle = angle - BARB_ANGLE;
        double cosineBarbAngle = Math.cos(Math.toRadians(barbAngle));
        double sineBarbAngle = Math.sin(Math.toRadians(barbAngle));

        /*
         * Process flags
         */
        for (int j = 0; j < numflags; j++) {
            Coordinate coords[] = new Coordinate[4];
            coords[0] = lil.extractPoint(currentLoc);
            coords[1] = lil.extractPoint(currentLoc - segmentSpacing);
            double xtip = coords[1].x + (barbLength * cosineBarbAngle);
            double ytip = coords[1].y + (barbLength * sineBarbAngle);
            coords[2] = new Coordinate(xtip, ytip);
            coords[3] = coords[0];
            LineString[] oneFlag = toLineString(coords);
            flags.addPolygonPixelSpace(
                    oneFlag,
                    new RGB(dspClr.getRed(), dspClr.getGreen(), dspClr
                            .getBlue()));
            if (vect.hasBackgroundMask())
                mask.addLineSegment(toDouble(coords));
            currentLoc -= 2 * segmentSpacing;
        }

        /*
         * Process barbs
         */
        for (int j = 0; j < numbarbs; j++) {
            Coordinate coords[] = new Coordinate[2];
            coords[0] = lil.extractPoint(currentLoc);
            double xtip = coords[0].x + (barbLength * cosineBarbAngle);
            double ytip = coords[0].y + (barbLength * sineBarbAngle);
            coords[1] = new Coordinate(xtip, ytip);
            double[][] pts = toDouble(coords);
            barb.addLineSegment(pts);
            if (vect.hasBackgroundMask())
                mask.addLineSegment(pts);
            currentLoc -= segmentSpacing;
        }

        /*
         * Process half barbs
         */
        for (int j = 0; j < halfbarbs; j++) {
            Coordinate coords[] = new Coordinate[2];
            coords[0] = lil.extractPoint(currentLoc);
            double xtip = coords[0].x + (0.5 * barbLength * cosineBarbAngle);
            double ytip = coords[0].y + (0.5 * barbLength * sineBarbAngle);
            coords[1] = new Coordinate(xtip, ytip);
            double[][] pts = toDouble(coords);
            barb.addLineSegment(pts);
            if (vect.hasBackgroundMask())
                mask.addLineSegment(pts);
            currentLoc -= segmentSpacing;
        }

        if (vect.hasBackgroundMask()) {
            mask.compile();
            slist.add(new LineDisplayElement(mask, bgColor, vect.getLineWidth()
                    + (float) deviceScale));
        }

        /*
         * 
        */
        flags.compile();
        FillDisplayElement fde = new FillDisplayElement(flags, vect.getColor()
                .getAlpha());
        slist.add(fde);

        /*
         * add shaft wireframe to return list
         */
        barb.compile();
        slist.add(new LineDisplayElement(barb, dspClr, vect.getLineWidth()));

        return slist;
    }

    /**
     * Creates high-efficiency IDisplayables of multiple wind barbs
     * 
     * @param vect
     *            A PGEN Drawable Element of a wind object
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createWindBarbs(List<IVector> vectors) {

        /*
         * Each returned IDisplayable can have only one color, so must build
         * separate shapes for each color. Keep track of all this in map...
         */
        Map<Color, IWireframeShape> barbMap = new HashMap<Color, IWireframeShape>();
        Map<Color, IWireframeShape> maskMap = new HashMap<Color, IWireframeShape>();
        Map<Color, IShadedShape> flagMap = new HashMap<Color, IShadedShape>();

        float lineWidth = vectors.get(0).getLineWidth(); // TODO: Generalize?
                                                         // (Assumes all vectors
                                                         // have same line
                                                         // width)

        for (IVector vect : vectors) {
            Color color = vect.getColor(); // display color for this vector
            Color bgColor = new Color(0, 0, 0); // background color; default
                                                // black
            double sfactor = deviceScale * vect.getSizeScale() * 10.;

            /*
             * Get the 3 cumulative shapes we're (possibly) constructing for
             * this color (or bgColor). If no such shape yet, start one for that
             * color.
             */
            IWireframeShape barbs = barbMap.get(color);
            if (barbs == null) {
                barbs = target.createWireframeShape(false, iDescriptor);
                barbMap.put(color, barbs);
            }
            IWireframeShape masks = null;
            if (vect.hasBackgroundMask()) {
                RGB bg = backgroundColor.getColor(BGColorMode.EDITOR);
                bgColor = new Color(bg.red, bg.green, bg.blue);
                masks = maskMap.get(bgColor);
                if (masks == null) {
                    masks = target.createWireframeShape(false, iDescriptor);
                    maskMap.put(bgColor, masks);
                }
            }
            IShadedShape flags = flagMap.get(color);
            if (flags == null) {
                flags = target.createShadedShape(false, iDescriptor, false);
                flagMap.put(color, flags);
            }

            /*
             * Convert location from lat/lon coordinates to pixel
             */
            double[] tmp = { vect.getLocation().x, vect.getLocation().y, 0.0 };
            double[] start = iDescriptor.worldToPixel(tmp);

            /*
             * If calm wind, draw circle
             */
            if (vect.getSpeed() < 0.5) {
                double[][] pts = calculateCircle(start, sfactor * 0.1);
                if (vect.hasBackgroundMask()) {
                    masks.addLineSegment(pts);
                }
                barbs.addLineSegment(pts);
            }

            else {
                /*
                 * Compute the number of flags, whole barbs and half barbs
                 * needed to represent the wind speed.
                 */
                int speed = (int) Math.floor(vect.getSpeed() + 2.5);
                int numflags = speed / 50;
                int remainder = speed % 50;
                int numbarbs = remainder / 10;
                remainder = remainder % 10;
                int halfbarbs = remainder / 5;

                double MAX_SEGMENTS = 6.0; // Maximum number of segments on
                                           // original size barb
                int numsegs = (2 * numflags) + numbarbs + halfbarbs;
                double segmentSpacing = sfactor / MAX_SEGMENTS;
                double windLength = segmentSpacing
                        * Math.max(MAX_SEGMENTS, numsegs);
                double barbLength = sfactor / 3.0;

                /*
                 * find the end point of the wind barb
                 */
                // TODO - orientation issues
                double angle = -90.0 - northOffsetAngle(vect.getLocation())
                        + vect.getDirection();
                double[] end = new double[3];
                end[0] = start[0]
                        + (windLength * Math.cos(Math.toRadians(angle)));
                end[1] = start[1]
                        + (windLength * Math.sin(Math.toRadians(angle)));
                end[2] = 0.0;
                barbs.addLineSegment(new double[][] { start, end });
                if (vect.hasBackgroundMask())
                    masks.addLineSegment(new double[][] { start, end });

                /*
                 * Create a LengthIndexedLine used to reference points along the
                 * path at specific distances
                 */
                LineString[] ls = toLineString(new Coordinate[] {
                        new Coordinate(start[0], start[1]),
                        new Coordinate(end[0], end[1]) });
                LengthIndexedLine lil = new LengthIndexedLine(ls[0]);
                double currentLoc = lil.getEndIndex(); // start from tail end

                // TODO - orientation issues
                double BARB_ANGLE = 70.0;
                double barbAngle = angle + BARB_ANGLE;
                if (vect.getLocation().y < 0.0)
                    barbAngle = angle - BARB_ANGLE;
                double cosineBarbAngle = Math.cos(Math.toRadians(barbAngle));
                double sineBarbAngle = Math.sin(Math.toRadians(barbAngle));

                /*
                 * Process flags
                 */
                for (int j = 0; j < numflags; j++) {
                    Coordinate coords[] = new Coordinate[4];
                    coords[0] = lil.extractPoint(currentLoc);
                    coords[1] = lil.extractPoint(currentLoc - segmentSpacing);
                    double xtip = coords[1].x + (barbLength * cosineBarbAngle);
                    double ytip = coords[1].y + (barbLength * sineBarbAngle);
                    coords[2] = new Coordinate(xtip, ytip);
                    coords[3] = coords[0];
                    LineString[] oneFlag = toLineString(coords);
                    flags.addPolygonPixelSpace(oneFlag, new RGB(color.getRed(),
                            color.getGreen(), color.getBlue()));
                    if (vect.hasBackgroundMask())
                        masks.addLineSegment(toDouble(coords));
                    currentLoc -= 2 * segmentSpacing;
                }

                /*
                 * Process barbs
                 */
                for (int j = 0; j < numbarbs; j++) {
                    Coordinate coords[] = new Coordinate[2];
                    coords[0] = lil.extractPoint(currentLoc);
                    double xtip = coords[0].x + (barbLength * cosineBarbAngle);
                    double ytip = coords[0].y + (barbLength * sineBarbAngle);
                    coords[1] = new Coordinate(xtip, ytip);
                    double[][] pts = toDouble(coords);
                    barbs.addLineSegment(pts);
                    if (vect.hasBackgroundMask())
                        masks.addLineSegment(pts);
                    currentLoc -= segmentSpacing;
                }

                /*
                 * Process half barbs
                 */
                for (int j = 0; j < halfbarbs; j++) {
                    Coordinate coords[] = new Coordinate[2];
                    coords[0] = lil.extractPoint(currentLoc);
                    double xtip = coords[0].x
                            + (0.5 * barbLength * cosineBarbAngle);
                    double ytip = coords[0].y
                            + (0.5 * barbLength * sineBarbAngle);
                    coords[1] = new Coordinate(xtip, ytip);
                    double[][] pts = toDouble(coords);
                    barbs.addLineSegment(pts);
                    if (vect.hasBackgroundMask())
                        masks.addLineSegment(pts);
                    currentLoc -= segmentSpacing;
                }

            } // end else

        } // end for each vector

        /*
         * Create the List to be returned
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        /*
         * For each color encountered above, compile the accumulated wireframes
         * (or shaded shapes) of that color, package them into a single display
         * element, and add to return list
         */
        for (Color color : barbMap.keySet()) {
            IWireframeShape barbs = barbMap.get(color);
            barbs.compile();
            slist.add(new LineDisplayElement(barbs, color, lineWidth));
        }
        for (Color color : maskMap.keySet()) {
            IWireframeShape masks = maskMap.get(color);
            masks.compile();
            slist.add(new LineDisplayElement(masks, color, lineWidth
                    + (float) deviceScale));
        }
        for (Color color : flagMap.keySet()) {
            IShadedShape flags = flagMap.get(color);
            flags.compile();
            slist.add(new FillDisplayElement(flags, color.getAlpha()));
        }

        return slist;
    }

    protected double[][] calculateCircle(double[] center, double radius) {

        int numpts = 16;
        double[][] arcpts = new double[numpts + 1][3];

        double inc = 360.0 / numpts;
        double angle = 0.0;
        for (int j = 0; j < numpts; j++) {
            arcpts[j][0] = center[0]
                    + (radius * Math.cos(Math.toRadians(angle)));
            arcpts[j][1] = center[1]
                    + (radius * Math.sin(Math.toRadians(angle)));
            angle += inc;
        }
        arcpts[numpts] = arcpts[0];

        return arcpts;
    }

    /*
     * Initialize Font for use with the graphics target
     */
    private IFont initializeFont(String fontName, float fontSize,
            FontStyle fstyle) {
        // TODO Auto-generated method stub
        Style[] styles = null;
        if (fstyle != null) {
            switch (fstyle) {
            case BOLD:
                styles = new Style[] { Style.BOLD };
                break;
            case ITALIC:
                styles = new Style[] { Style.ITALIC };
                break;
            case BOLD_ITALIC:
                styles = new Style[] { Style.BOLD, Style.ITALIC };
                break;
            }
        }

        /*
         * set smoothing and scaleFont to false to disable anti-aliasing (which
         * cause the fuzziness of the text).
         */
        IFont font = target.initializeFont(fontName, fontSize, styles);
        font.setSmoothing(false);
        font.setScaleFont(false);

        return font;
    }

    /**
     * Set some display attributes for all elements on a layer.
     * 
     * @param mono
     * @param clr
     * @param fill
     */
    public void setLayerDisplayAttr(Boolean mono, Color clr, Boolean fill) {
        this.layerMonoColor = mono;
        this.layerColor = clr;
        this.layerFilled = fill;
    }

    /**
     * Get the colors for displaying an element.
     */
    protected Color[] getDisplayColors(Color[] clr) {

        Color[] newClr = new Color[clr.length];

        for (int ii = 0; ii < clr.length; ii++) {

            if (layerMonoColor && layerColor != null) {
                newClr[ii] = layerColor;
            } else {
                newClr[ii] = clr[ii];
            }
        }

        return newClr;
    }

    /**
     * Get the colors for displaying an element.
     */
    protected Color getDisplayColor(Color clr) {

        if (layerMonoColor && layerColor != null) {
            return layerColor;
        } else {
            return clr;
        }

    }

    /**
     * Get the fill mode for displaying an element.
     */
    private boolean getDisplayFillMode(Boolean filled) {

        if (layerFilled) {
            return layerFilled;
        } else {
            return filled;
        }

    }

    /**
     * Creates a list of IDisplayable Objects from an ISigmet object
     * 
     * @param isig
     *            : A PGEN Drawable Element of an ISigmet object
     * @param paintProps
     *            : The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public ArrayList<IDisplayable> createDisplayElements(ISigmet isig,
            PaintProperties paintProps) {

        if (isig instanceof Volcano) {
            return createDisplayElements((Volcano) isig, paintProps);
        }

        if (isig instanceof AbstractSigmet
                && "CCFP_SIGMET".equals(((AbstractSigmet) isig).getPgenType())) {
            return createDisplayElements(
                    (gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet) isig, paintProps);
        }

        return createDisplayElements((AbstractSigmet) isig, paintProps);

        // return new ArrayList<IDisplayable>();
    }

    /**
     * Creates a list of IDisplayable Objects from an AbstractSigmet object
     * 
     * @param sigmet
     *            : A PGEN Drawable Element of an AbstractSigmet object
     * @param paintProps
     *            : The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createDisplayElements(
            AbstractSigmet sigmet, PaintProperties paintProps) {
        double widthInNautical = sigmet.getWidth() * PgenUtil.NM2M;
        float density;
        double[][] pixels;
        double[][] smoothpts;

        setScales(paintProps);

        elem = sigmet;

        Color[] dspClr = getDisplayColors(elem.getColors());
        Color[] fillClr = dspClr;
        if (fillClr.length > 1) {
            fillClr[1] = fillClr[0];
        }
        ArrayList<IDisplayable> list = new ArrayList<IDisplayable>();
        wfs = new IWireframeShape[dspClr.length];
        for (int i = 0; i < dspClr.length; i++) {
            wfs[i] = target.createWireframeShape(false, iDescriptor);
        }

        Coordinate[] pts = sigmet.getLinePoints();

        pixels = PgenUtil.latlonToPixel(pts, (IMapDescriptor) iDescriptor);

        if (sigmet.isClosedLine()) {
            pixels = ensureClosed(pixels);
        }

        smoothpts = pixels;

        LinePattern pattern = new LinePattern("Medium Dashed", false, null);// null;
        pattern.addSegment(new PatternSegment(2, PatternType.LINE, 0, 0, 0,
                false));
        pattern.addSegment(new PatternSegment(2, PatternType.BLANK, 0, 0, 0,
                false));
        pattern.addSegment(new PatternSegment(2, PatternType.LINE, 0, 0, 0,
                false));

        if ((pattern != null) && pattern.needsLengthUpdate()) {
            pattern = pattern.updateLength(screenToExtent
                    * sigmet.getLineWidth()
                    / (sigmet.getSizeScale() * deviceScale));
        }

        wfs[0].addLineSegment(smoothpts); // the mouse line

        String lineType = sigmet.getType();

        if (sigmet.AREA.equals(lineType)) {

            wfs[0].addLineSegment(new Coordinate[] { pts[0],
                    pts[pts.length - 1] });// the closing line

            if (sigmet.getPgenType().equals(VaaInfo.PGEN_TYEP_CLOUD)
                    || sigmet.getPgenType().equals("CCFP_SIGMET")) {
                if (sigmet.getPgenType().equals(VaaInfo.PGEN_TYEP_CLOUD)) {
                    sigmet.setColors(fillClr);
                }
                list.add(createFill(smoothpts));
            }

        } else if (lineType.contains(sigmet.LINE)) {

            String[] lineString = lineType.split(SigmetInfo.LINE_SEPERATER);

            if ("ESOL".equalsIgnoreCase(lineString[1])) {

                Coordinate[][] sides = SigmetInfo
                        .getSides(pts, widthInNautical);
                Coordinate[][] sidesWithArcIntsc = SigmetInfo
                        .getSidesWithArcIntsc((IMapDescriptor) iDescriptor,
                                pts, sides[0], sides[1]);

                double[][] sa1 = PgenUtil.latlonToPixel(sidesWithArcIntsc[0],
                        (IMapDescriptor) iDescriptor);
                double[][] sa2 = PgenUtil.latlonToPixel(sidesWithArcIntsc[1],
                        (IMapDescriptor) iDescriptor);

                handleLinePattern(pattern, PgenUtil.latlonToPixel(
                        sidesWithArcIntsc[0], (IMapDescriptor) iDescriptor));
                handleLinePattern(pattern, PgenUtil.latlonToPixel(
                        sidesWithArcIntsc[1], (IMapDescriptor) iDescriptor));

                if (sigmet.getPgenType().equals(VaaInfo.PGEN_TYEP_CLOUD)) {
                    sigmet.setColors(fillClr);
                    list.add(createFill(SigmetInfo.getESOLArea(
                            sidesWithArcIntsc[1], sidesWithArcIntsc[0],
                            (IMapDescriptor) iDescriptor)));
                }

            } else {
                Coordinate[] sides = SigmetInfo.getSOLCoors(pts, lineString[1],
                        widthInNautical, (IMapDescriptor) iDescriptor);
                handleLinePattern(pattern, PgenUtil.latlonToPixel(sides,
                        (IMapDescriptor) iDescriptor));
            }

        } else if (sigmet.ISOLATED.equals(lineType)) {

            IWireframeShape arcpts = target.createWireframeShape(false,
                    iDescriptor);
            Coordinate[] locs = sigmet.getLinePoints();
            ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

            SymbolLocationSet centerSign = new SymbolLocationSet(null,
                    new Color[] { dspClr[0] }, sigmet.getLineWidth(), 0.5,
                    false, locs, "Marker", "PLUS_SIGN");

            slist.addAll(createDisplayElements(centerSign, paintProps));

            try {
                arcpts.addLineSegment(SigmetInfo.getIsolated(
                        locs[locs.length - 1], widthInNautical,
                        (IMapDescriptor) iDescriptor));
            } catch (Throwable e) {
                System.out.println("Isolated: " + e.getCause());
            }// OutOfMemoryError

            arcpts.compile();
            slist.add(new LineDisplayElement(arcpts, dspClr[0], sigmet
                    .getLineWidth()));

            addTopText(sigmet, locs, dspClr, paintProps, slist);
            return slist;
        } else if (lineType.contains("Text")) {

            String theTxt = lineType.split(SigmetInfo.LINE_SEPERATER)[1];
            Coordinate[] locs = sigmet.getLinePoints();
            ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

            Text display = new Text(null, "Courier", 14.0f,
                    TextJustification.CENTER, locs[0], 0.0,
                    TextRotation.SCREEN_RELATIVE, sigmet.getDisplayTxt(),
                    FontStyle.REGULAR, dspClr[0], 0, 0, false, DisplayType.BOX,
                    "Text", "General Text");
            slist.addAll(createDisplayElements((IText) display, paintProps));

            return slist;
        }

        for (int k = 0; k < wfs.length; k++) {
            wfs[k].compile();
            LineDisplayElement lde = new LineDisplayElement(wfs[k], dspClr[k],
                    sigmet.getLineWidth());
            list.add(lde);
        }

        addTopText(sigmet, pts, dspClr, paintProps, list);
        return list;
    }

    private void addTopText(AbstractSigmet sigmet, Coordinate[] pts,
            Color[] dspClr, PaintProperties paintProps,
            ArrayList<IDisplayable> list) {

        if (sigmet.isWithTopText()) {

            Text display = new Text(null, "Courier", 14.0f,
                    TextJustification.CENTER, SnapUtil.getNorthMostPoint(pts),
                    0.0, TextRotation.SCREEN_RELATIVE,
                    new String[] { sigmet.getTopText() }, FontStyle.REGULAR,
                    dspClr[0], 0, 3, false, DisplayType.NORMAL, "Text",
                    "General Text");

            list.addAll(createDisplayElements((IText) display, paintProps));
        }
    }

    /**
     * Creates a list of IDisplayable Objects from a Volcano object
     * 
     * @param vol
     *            : A PGEN Drawable Element of a Volcano object
     * @param paintProps
     *            : The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    private ArrayList<IDisplayable> createDisplayElements(Volcano vol,
            PaintProperties paintProps) {

        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();

        // an empty list for texts: TEST/RESUME,etc
        if (VaaInfo.isNonDrawableVol(vol))
            return slist;

        SymbolLocationSet centerSign = new SymbolLocationSet(null,
                new Color[] { Color.cyan }, 2, 1.0, false, vol.getLinePoints(),
                "Symbol", "PRESENT_WX_201");

        slist.addAll(createDisplayElements(centerSign, paintProps));

        return slist;
    }

    /**
     * Handles Gfa elements
     */
    public ArrayList<IDisplayable> createDisplayElements(IGfa igfa,
            PaintProperties paintProps) {

        Gfa gfa = (Gfa) igfa;
        ArrayList<IDisplayable> list = createDisplayElements((ILine) gfa,
                paintProps);

        if (gfa.getGfaTextCoordinate() != null) {
            Coordinate loc1 = gfa.getGfaTextCoordinate();
            Coordinate loc2;
            // find the closest point for open geometries, or centroid for
            // closed ones.
            if (gfa.isClosedLine()) {
                loc2 = gfa.getCentroid();
            } else {
                GeometryFactory geometryFactory = new GeometryFactory();
                Coordinate[] a = new Coordinate[gfa.getPoints().size()];
                CoordinateArraySequence cas = new CoordinateArraySequence(gfa
                        .getPoints().toArray(a));
                LineString line = new LineString(cas, geometryFactory);
                cas = new CoordinateArraySequence(new Coordinate[] { loc1 });
                Point point = new Point(cas, geometryFactory);
                // Coordinate[] c = DistanceOp.closestPoints(line, point);
                Coordinate[] c = DistanceOp.nearestPoints(line, point);
                loc2 = c[0];
            }
            ArrayList<Coordinate> locs = new ArrayList<Coordinate>();
            locs.add(loc1);
            locs.add(loc2);

            /*
             * create new Text an Line then generate their IDisplayables
             */
            String[] txtToDisplay = gfa.getString();
            Text text = new Text(null, "Courier", 14.0f,
                    TextJustification.CENTER, loc1, 0.0,
                    TextRotation.SCREEN_RELATIVE, txtToDisplay,
                    FontStyle.REGULAR, getDisplayColors(elem.getColors())[0],
                    0, 0, true, DisplayType.BOX, "", "General Text");

            Line line = new Line(null, getDisplayColors(elem.getColors()),
                    1.0F,
                    1.0, // 1.5 ->1.0
                    false, false, locs, 2, FillPattern.SOLID, "Lines",
                    "POINTED_ARROW");

            list.addAll(createDisplayElements(line, paintProps));
            list.addAll(createDisplayElements((IText) text, paintProps));

            // create Symbol if needed
            if (gfa.getSymbolType() != null) {
                double relativePosition = 0.0; // relative vertical center
                for (int i = 0; i < txtToDisplay.length; i++) {
                    if (txtToDisplay[i].isEmpty()) {
                        /*
                         * Put the symbol in the center of first empty line. The
                         * relative position is measured by the number of
                         * "character"s away from center of the text box.
                         */
                        relativePosition = i + 0.5 - txtToDisplay.length / 2.0;
                        break;
                    }
                }
                /*
                 * get pixel size of text string with given font
                 */
                IFont font = initializeFont("Courier", 14.f, FontStyle.REGULAR);
                Rectangle2D bounds = target.getStringBounds(font, "Xy");
                bounds = new Rectangle2D.Double(0.0, 0.0,
                        bounds.getWidth() / 2.0, bounds.getHeight());
                font.dispose();
                double vertRatio = paintProps.getView().getExtent().getHeight()
                        / paintProps.getCanvasBounds().height;

                /*
                 * Get pixel value for the world location
                 */
                double[] worldPixel = new double[] { loc1.x, loc1.y, 0.0 };
                double[] pixel = iDescriptor.worldToPixel(worldPixel);

                // convert pixel back to map coordinates
                double[] locSym = iDescriptor.pixelToWorld(new double[] {
                        pixel[0],
                        pixel[1] + vertRatio * relativePosition
                                * bounds.getHeight() - 1, 0.0 });

                loc1 = new Coordinate(locSym[0], locSym[1]);

                Symbol sym = new Symbol(null,
                        getDisplayColors(elem.getColors()), 1.5f, 1.0f, false,
                        loc1, "Symbol", gfa.getSymbolType());
                list.addAll(createDisplayElements((ISymbol) sym, paintProps));
            }
        }

        return list;
    }

    /**
     * 
     * create CCFP elements
     */
    private ArrayList<IDisplayable> createDisplayElements(
            gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet sigmet,
            PaintProperties paintProps) {

        ArrayList<IDisplayable> list;

        // Line and LineMed
        if (!"Area".equalsIgnoreCase(sigmet.getType())) {

            Line line = new Line(null, getDisplayColors(sigmet.getColors()),
                    sigmet.getLineWidth(), 1.0, false, false,
                    sigmet.getPoints(), 0, FillPattern.SOLID, "Lines",
                    sigmet.getPatternName());

            list = createDisplayElements(line, paintProps);

        } else {// Area

            list = createDisplayElements((AbstractSigmet) sigmet, paintProps);

            // arrow for speed/dir
            String spd = sigmet.getEditableAttrPhenomSpeed();
            if (spd != null && !spd.trim().equals("0")) {

                String dir = sigmet.getEditableAttrPhenomDirection();
                Coordinate[] coors = sigmet.getLinePoints();

                ArrayList<Coordinate> spdCoors = CcfpInfo.getDirMostPts(dir,
                        coors, (IMapDescriptor) iDescriptor);
                if (spdCoors.size() > 1) {

                    gov.noaa.nws.ncep.ui.pgen.elements.Vector vv = new gov.noaa.nws.ncep.ui.pgen.elements.Vector();
                    vv.setPgenCategory("Vector");
                    vv.setPgenType("Arrow");
                    double vDir = vv.vectorDirection(spdCoors.get(0),
                            spdCoors.get(1));

                    gov.noaa.nws.ncep.ui.pgen.elements.Vector v = new gov.noaa.nws.ncep.ui.pgen.elements.Vector(
                            null,
                            getDisplayColors(sigmet.getColors()),
                            1.0F,
                            1.0,
                            false,
                            spdCoors.get(0),
                            gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType.ARROW,
                            10, vDir, 1.0, false, "Vector", "Arrow");

                    Text spdTxt = new Text(
                            null,
                            "Courier",
                            14.0f,
                            TextJustification.LEFT_JUSTIFY,
                            getCcfpTxtPts(v),
                            0.0,
                            TextRotation.SCREEN_RELATIVE,
                            new String[] { sigmet.getEditableAttrPhenomSpeed() },
                            FontStyle.REGULAR, getDisplayColors(sigmet
                                    .getColors())[0], 0, 3, false,
                            DisplayType.NORMAL, "Text", "General Text");

                    list.addAll(createDisplayElements((IVector) v, paintProps));
                    list.addAll(createDisplayElements((IText) spdTxt,
                            paintProps));
                }
            }

            if (!CcfpInfo.isTxtArrwExst(sigmet)) {

                CcfpInfo.calAziDist4TxtArrw(sigmet);

            }

            // Text part for Area
            if (CcfpInfo.isTxtArrwExst(sigmet)) {// sigmet.getEditableAttrFromLine()
                                                 // != null){

                String loct = sigmet.getEditableAttrFreeText();
                if (loct == null || loct.isEmpty() || (!loct.contains(":::")))
                    return list;
                double azi = Double.parseDouble(loct.split(":::")[0]);
                double dis = Double.parseDouble(loct.split(":::")[1]);

                org.geotools.referencing.GeodeticCalculator gc = new org.geotools.referencing.GeodeticCalculator(
                        gov.noaa.nws.ncep.ui.pgen.PgenSession.getInstance()
                                .getPgenResource()
                                .getCoordinateReferenceSystem());

                Coordinate loc2;
                ArrayList<Coordinate> locs = new ArrayList<Coordinate>();

                loc2 = CcfpInfo.getSigCentroid2(sigmet,
                        (IMapDescriptor) iDescriptor);

                java.awt.geom.Point2D pts = null;

                try {
                    gc.setStartingGeographicPoint(loc2.x, loc2.y);
                    gc.setDirection(azi, dis);
                    pts = gc.getDestinationGeographicPoint();
                } catch (Exception e) {

                }

                if (pts == null)
                    return list;

                Coordinate loc1 = new Coordinate(pts.getX(), pts.getY());
                locs.add(loc1);
                locs.add(loc2);

                Text display = new Text(null, "Courier", 16.0f,
                        TextJustification.LEFT_JUSTIFY, loc1, 0.0,
                        TextRotation.SCREEN_RELATIVE,
                        CcfpInfo.getCcftTxt(sigmet), FontStyle.REGULAR,
                        getDisplayColors(elem.getColors())[0], 0, 0, true,
                        DisplayType.BOX, "Text", "General Text");

                // no line drawn when text inside the sigmet area
                if (!CcfpInfo.isPtsInArea(sigmet, loc1)) {
                    Line line = new Line(null, new Color[] { Color.orange },
                            1.0F, 1.0, false, false, locs, 2,
                            FillPattern.SOLID, "Lines", "POINTED_ARROW");
                    list.addAll(createDisplayElements(line, paintProps));
                }

                list.addAll(createDisplayElements((IText) display, paintProps));

            }
        }
        return list;

    }

    /**
     * 
     * return the speed Text position for CCFP
     */
    private Coordinate getCcfpTxtPts(IVector vect) {
        double sfactor = deviceScale * vect.getSizeScale();

        double[] tmp = { vect.getLocation().x, vect.getLocation().y, 0.0 };
        double[] start = iDescriptor.worldToPixel(tmp);

        double speed = 9;
        if (!vect.hasDirectionOnly())
            speed = vect.getSpeed();
        double arrowLength = sfactor * (speed + 4);

        double angle = 90.0 - northOffsetAngle(vect.getLocation())
                + vect.getDirection();

        double[] end = new double[3];
        end[0] = start[0] + (arrowLength * Math.cos(Math.toRadians(angle)));
        end[1] = start[1] + (arrowLength * Math.sin(Math.toRadians(angle)));
        end[2] = 0.0;

        double[] c = iDescriptor.pixelToWorld(end);
        return new Coordinate(c[0], c[1]);
    }

    /*
     * Determine the number of labels to be drawn on a ContourLine and set their
     * locations
     */
    private ArrayList<IDisplayable> adjustContourLineLabels(IMultiPoint de,
            PaintProperties paintProps, double[][] smoothpts) {

        ArrayList<IDisplayable> dlist = new ArrayList<IDisplayable>();

        if (de instanceof Line
                && ((Line) de).getParent() instanceof ContourLine) {

            ContourLine cline = (ContourLine) ((Line) elem).getParent();

            boolean lineClosed = cline.getLine().isClosedLine();

            /*
             * Find the visible part of the line.
             */
            double minx = paintProps.getView().getExtent().getMinX();
            double miny = paintProps.getView().getExtent().getMinY();
            double maxx = paintProps.getView().getExtent().getMaxX();
            double maxy = paintProps.getView().getExtent().getMaxY();

            double dx = Math.abs(maxx - minx);
            double dy = Math.abs(maxy - miny);
            double dd = Math.min(dx, dy);

            double ratio = 0.02;
            double offset = dd * ratio;

            minx += offset;
            miny += offset;
            maxx -= offset;
            maxy -= offset;

            double[][] visiblePts = new double[smoothpts.length][smoothpts[0].length];
            int actualLength = 0;

            for (double[] dl : smoothpts) {
                if (dl[0] > minx && dl[0] < maxx && dl[1] > miny
                        && dl[1] < maxy) {
                    visiblePts[actualLength][0] = dl[0];
                    visiblePts[actualLength][1] = dl[1];
                    actualLength++;
                }
            }

            int numText2Draw = Math.min(actualLength, cline.getNumOfLabels());
            ArrayList<Coordinate> txtPositions = new ArrayList<Coordinate>();

            /*
             * Determine the number of labels to be drawn and set their
             * locations.
             */
            double xx, yy;
            if (actualLength < cline.getNumOfLabels()) {

                numText2Draw = Math.min(3, numText2Draw);

                if (numText2Draw > 0) {
                    if (numText2Draw == 1) {
                        xx = visiblePts[actualLength / 2][0] - offset / 4;
                        yy = visiblePts[actualLength / 2][1];
                        txtPositions.add(new Coordinate(xx, yy));
                    } else if (numText2Draw == 2) {
                        xx = visiblePts[0][0] - offset / 4;
                        yy = visiblePts[0][1];
                        txtPositions.add(new Coordinate(xx, yy));

                        if (lineClosed) {
                            xx = visiblePts[actualLength / 2][0] + offset / 4;
                            yy = visiblePts[actualLength / 2][1];
                        } else {
                            xx = visiblePts[actualLength - 1][0] + offset / 4;
                            yy = visiblePts[actualLength - 1][1];
                        }
                        txtPositions.add(new Coordinate(xx, yy));
                    } else {

                        xx = visiblePts[0][0] - offset / 4;
                        yy = visiblePts[0][1];
                        txtPositions.add(new Coordinate(xx, yy));

                        if (lineClosed) {
                            int intv = actualLength / numText2Draw;
                            xx = visiblePts[intv][0] + offset / 4;
                            yy = visiblePts[intv][1];
                            txtPositions.add(new Coordinate(xx, yy));

                            xx = visiblePts[intv * 2][0] + offset / 4;
                            yy = visiblePts[intv * 2][1];
                            txtPositions.add(new Coordinate(xx, yy));
                        } else {
                            xx = visiblePts[actualLength / 2][0] + offset / 4;
                            yy = visiblePts[actualLength / 2][1];
                            txtPositions.add(new Coordinate(xx, yy));

                            xx = visiblePts[actualLength - 1][0] + offset / 4;
                            yy = visiblePts[actualLength - 1][1];
                            txtPositions.add(new Coordinate(xx, yy));
                        }
                    }
                }
            } else {

                if (numText2Draw > 0) {
                    if (cline.getNumOfLabels() == 1) {
                        xx = visiblePts[actualLength / 2][0] - offset / 4;
                        yy = visiblePts[actualLength / 2][1];
                        txtPositions.add(new Coordinate(xx, yy));
                    } else if (cline.getNumOfLabels() == 2) {
                        xx = visiblePts[0][0] - offset / 4;
                        yy = visiblePts[0][1];
                        txtPositions.add(new Coordinate(xx, yy));

                        if (lineClosed) {
                            xx = visiblePts[actualLength / 2][0] + offset / 4;
                            yy = visiblePts[actualLength / 2][1];
                        } else {
                            xx = visiblePts[actualLength - 1][0] + offset / 4;
                            yy = visiblePts[actualLength - 1][1];
                        }
                        txtPositions.add(new Coordinate(xx, yy));
                    } else if (cline.getNumOfLabels() == 3) {

                        xx = visiblePts[0][0] - offset / 4;
                        yy = visiblePts[0][1];
                        txtPositions.add(new Coordinate(xx, yy));

                        if (lineClosed) {
                            int intv = actualLength / numText2Draw;
                            xx = visiblePts[intv][0] + offset / 4;
                            yy = visiblePts[intv][1];
                            txtPositions.add(new Coordinate(xx, yy));

                            xx = visiblePts[intv * 2][0] + offset / 4;
                            yy = visiblePts[intv * 2][1];
                            txtPositions.add(new Coordinate(xx, yy));
                        } else {
                            xx = visiblePts[actualLength / 2][0] + offset / 4;
                            yy = visiblePts[actualLength / 2][1];
                            txtPositions.add(new Coordinate(xx, yy));

                            xx = visiblePts[actualLength - 1][0] + offset / 4;
                            yy = visiblePts[actualLength - 1][1];
                            txtPositions.add(new Coordinate(xx, yy));
                        }
                    } else {
                        int interval;
                        if (lineClosed) {
                            interval = actualLength / numText2Draw;
                        } else {
                            interval = actualLength / (numText2Draw - 1);
                        }

                        int nadd = numText2Draw - 1;
                        if (lineClosed)
                            nadd = numText2Draw;

                        for (int jj = 0; jj < nadd; jj++) {
                            if (jj == 0) {
                                xx = visiblePts[jj * interval][0] - offset / 4;
                            } else {
                                xx = visiblePts[jj * interval][0] + offset / 4;
                            }
                            yy = visiblePts[jj * interval][1];
                            txtPositions.add(new Coordinate(xx, yy));
                        }

                        if (!lineClosed) {
                            xx = visiblePts[actualLength - 1][0] + offset / 4;
                            yy = visiblePts[actualLength - 1][1];
                            txtPositions.add(new Coordinate(xx, yy));
                        }
                    }
                }
            }

            /*
             * Draw the label Texts - temporarily set their parents to null and
             * adjust the location for drawing.
             */
            double[] tps;
            double[] loc = { 0.0, 0.0, 0.0 };
            for (int kk = 0; kk < numText2Draw; kk++) {
                Text txt = cline.getLabels().get(kk);
                loc[0] = txtPositions.get(kk).x;
                loc[1] = txtPositions.get(kk).y;

                tps = iDescriptor.pixelToWorld(loc);
                if (txt.getAuto() != null && txt.getAuto()) {
                    txt.setLocationOnly(new Coordinate(tps[0], tps[1]));
                }

                txt.setParent(null);
                dlist.addAll(createDisplayElements((IText) txt, paintProps));
                txt.setParent(cline);
            }

        }

        return dlist;
    }

    /*
     * Adjust the label position on a ContourCircle
     */
    private ArrayList<IDisplayable> adjustContourCircleLabel(IArc arc,
            PaintProperties paintProps, double[][] smoothpts) {

        ArrayList<IDisplayable> dlist = new ArrayList<IDisplayable>();

        AbstractDrawableComponent parent = ((DrawableElement) arc).getParent();

        if (parent instanceof ContourCircle) {

            Text labelText = ((ContourCircle) parent).getLabel();

            if (labelText.getAuto() != null && labelText.getAuto()) {
                /*
                 * Find the visible part of the circle.
                 */
                double minx = paintProps.getView().getExtent().getMinX();
                double miny = paintProps.getView().getExtent().getMinY();
                double maxx = paintProps.getView().getExtent().getMaxX();
                double maxy = paintProps.getView().getExtent().getMaxY();

                double dx = Math.abs(maxx - minx);
                double dy = Math.abs(maxy - miny);
                double dd = Math.min(dx, dy);

                double ratio = 0.02;
                double offset = dd * ratio;

                minx += offset;
                miny += offset;
                maxx -= offset;
                maxy -= offset;

                double[][] visiblePts = new double[smoothpts.length][smoothpts[0].length];
                int actualLength = 0;

                for (double[] dl : smoothpts) {
                    if (dl[0] > minx && dl[0] < maxx && dl[1] > miny
                            && dl[1] < maxy) {
                        visiblePts[actualLength][0] = dl[0];
                        visiblePts[actualLength][1] = dl[1];
                        actualLength++;
                    }
                }

                /*
                 * Adjust the position - either in the middle or slightly off
                 * the last point.
                 */
                int pp = Math.max(actualLength / 2, actualLength - 5);
                double[] loc = iDescriptor
                        .pixelToWorld(new double[] {
                                visiblePts[pp][0] - offset / 2,
                                visiblePts[pp][1], 0.0 });
                Coordinate loc1 = new Coordinate(loc[0], loc[1]);
                labelText.setLocationOnly(loc1);
            }

            /*
             * Display.
             */
            labelText.setParent(null);
            dlist.addAll(createDisplayElements((IText) labelText, paintProps));
            labelText.setParent(parent);

        }

        return dlist;
    }

    private void addCcfpSpeed(ArrayList<IDisplayable> list,
            PaintProperties paintProps,
            gov.noaa.nws.ncep.ui.pgen.elements.DECollection ccfp) {

        if (list == null || paintProps == null)// || ( !
                                               // ((Ccfp)ccfp).isPrimaryDEClosed()))//
                                               // || sigmet == null)
            return;

        String[] spdDir = ccfp.getCollectionName().split(
                CcfpInfo.TEXT_SEPERATOR);

        if (!CcfpAttrDlg.AREA.equals(spdDir[spdDir.length - 1]))
            return;// line type: Line/Line-Med

        String spd = spdDir[1];// sigmet.getEditableAttrPhenomSpeed();
        if (spd != null && !spd.trim().equals("0")) {

            String dir = spdDir[2];// sigmet.getEditableAttrPhenomDirection();
            Coordinate[] coors = ccfp.getPrimaryDE().getPoints()
                    .toArray(new Coordinate[] {});// .getAreaLine().getLinePoints();//sigmet.getLinePoints();

            ArrayList<Coordinate> spdCoors = CcfpInfo.getDirMostPts(dir, coors,
                    (IMapDescriptor) iDescriptor);
            if (spdCoors.size() > 1) {

                gov.noaa.nws.ncep.ui.pgen.elements.Vector vv = new gov.noaa.nws.ncep.ui.pgen.elements.Vector();
                vv.setPgenCategory("Vector");
                vv.setPgenType("Arrow");
                double vDir = vv.vectorDirection(spdCoors.get(0),
                        spdCoors.get(1));

                gov.noaa.nws.ncep.ui.pgen.elements.Vector v = new gov.noaa.nws.ncep.ui.pgen.elements.Vector(
                        null,
                        getDisplayColors(ccfp.getPrimaryDE().getColors()),// ccfp.getAreaLine().getColors()),//sigmet.getColors()),
                        2.0F,
                        1.5,
                        false,
                        spdCoors.get(0),
                        gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType.ARROW,
                        10, vDir, 1.0, false, "Vector", "Arrow");
                // System.out.println("generate a text for " + spd + " at: "
                // + getCcfpTxtPts(v).x + "," + getCcfpTxtPts(v).y);
                Text spdTxt = new Text(null, "Courier",
                        14.0f,
                        TextJustification.CENTER,// .LEFT_JUSTIFY,
                        getCcfpTxtPts(v), 0.0, TextRotation.SCREEN_RELATIVE,
                        new String[] { spd },// sigmet.getEditableAttrPhenomSpeed()},
                        FontStyle.REGULAR,
                        getDisplayColors(ccfp.getPrimaryDE().getColors())[0],// ccfp.getAreaLine().getColors())[0],//sigmet.getColors())[0],
                        0, 0/* 3 */, false, DisplayType.NORMAL, "Text",
                        "General Text");

                list.addAll(createDisplayElements((IVector) v, paintProps));
                list.addAll(createDisplayElements((IText) spdTxt, paintProps));
            }
        }
    }

    /**
     * Calculate end point from a start point, distance and angle.
     * 
     * @param startPt
     * @param angle
     * @param distance
     * @return - end point
     */
    private Coordinate calculateDestinationPointMap(Coordinate startPt,
            double angle, double distance) {
        GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
        gc.setStartingGeographicPoint(startPt.x, startPt.y);
        gc.setDirection(angle, distance * PgenUtil.NM2M);

        Point2D pt1 = gc.getDestinationGeographicPoint();
        return new Coordinate(pt1.getX(), pt1.getY());
    }

    /**
     * Get the point on arc aith a sepcified angle from the starting angle.
     * 
     * @param arc
     * @param angle
     * @return
     */
    private Coordinate getPointOnArc(Arc arc, double angle) {
        /*
         * Convert center and circumference point from lat/lon to pixel
         * coordinates.
         */
        double[] tmp = { arc.getCenterPoint().x, arc.getCenterPoint().y, 0.0 };
        double[] center = iDescriptor.worldToPixel(tmp);
        double[] tmp2 = { arc.getCircumferencePoint().x,
                arc.getCircumferencePoint().y, 0.0 };
        double[] circum = iDescriptor.worldToPixel(tmp2);

        double radius = Math.sqrt((center[0] - circum[0])
                * (center[0] - circum[0]) + (center[1] - circum[1])
                * (center[1] - circum[1]));

        /*
         * calculate angle of major axis
         */
        double axisAngle = 90 + Math.toDegrees(Math.atan2(
                (circum[1] - center[1]), (circum[0] - center[0])));
        angle += axisAngle;

        double thisSine = Math.sin(Math.toRadians(angle));
        double thisCosine = Math.cos(Math.toRadians(angle));

        double pt[] = new double[2];
        // pt[0] = center[0] + (radius * cosineAxis * thisCosine ) - (radius *
        // sineAxis * thisSine );
        // pt[1] = center[1] + (radius * sineAxis * thisCosine ) + (radius *
        // cosineAxis * thisSine );
        pt[0] = center[0] + (radius * thisCosine);
        pt[1] = center[1] + (radius * thisSine);

        double mapPt[] = iDescriptor.pixelToWorld(pt);

        return new Coordinate(mapPt[0], mapPt[1]);
    }

    /**
     * Get the line that connects two TCM wind/wave quarters
     * 
     * @param pt1
     *            - start point
     * @param pt2
     *            - end point
     * @param color
     * @return - A Line
     */
    private Line getWindQuatroLine(Coordinate pt1, Coordinate pt2, Color color) {
        ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
        pts.add(pt1);
        pts.add(pt2);

        return new Line(null, new Color[] { color }, 1.5f, .5, false, false,
                pts, 0, null, "Lines", "LINE_SOLID");
    }

    public void reset() {
        if (ss != null) {
            ss.reset();
        }
        if (sym != null) {
            sym.reset();
        }
        if (wfs != null) {
            for (IWireframeShape shape : wfs) {
                if (shape != null) {
                    shape.reset();
                }
            }
        }
    }

    /**
     * Generate a box that could hold a text string.
     * 
     * @param txt
     *            A PGEN Drawable Element of a text string
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public PgenRangeRecord findTextBoxRange(IText txt,
            PaintProperties paintProps) {
        // System.out.println("findTextBoxRange for IText .... enter ");

        setScales(paintProps);

        double[] tmp = { txt.getPosition().x, txt.getPosition().y, 0.0 };
        // System.out.println("findTextBoxRange for IText .... "
        // + txt.getPosition().x + "," + txt.getPosition().y);
        double[] loc = iDescriptor.worldToPixel(tmp);

        double horizRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        double vertRatio = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;

        /*
         * Initialize Font Style
         */
        IFont font = initializeFont(txt.getFontName(), txt.getFontSize(),
                txt.getStyle());
        // System.out.println("findTextBoxRange for IText .... 1 ");

        /*
         * apply X offset in half-characters
         */
        boolean adjustOffset = false;
        if (txt.getXOffset() != 0) {
            double ratio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;
            Rectangle2D bounds = target.getStringBounds(font,
                    txt.getString()[0]);
            double charSize = ratio * bounds.getWidth()
                    / txt.getString()[0].length();
            loc[0] += 0.5 * charSize * txt.getXOffset();
            adjustOffset = true;
        }

        /*
         * apply Y offset in half-characters
         */
        if (txt.getYOffset() != 0) {
            double ratio = paintProps.getView().getExtent().getHeight()
                    / paintProps.getCanvasBounds().height;
            Rectangle2D bounds = target.getStringBounds(font,
                    txt.getString()[0]);
            double charSize = ratio * bounds.getHeight();
            loc[1] -= 0.5 * charSize * txt.getYOffset();
            adjustOffset = true;
        }

        if (adjustOffset) {
            double[] tmp1 = { loc[0], loc[1], 0.0 };
            double[] newloc = iDescriptor.pixelToWorld(tmp1);
            ((Text) txt).setLocationOnly(new Coordinate(newloc[0], newloc[1]));
            ((Text) txt).setXOffset(0);
            ((Text) txt).setYOffset(0);
        }
        // System.out.println("findTextBoxRange for IText .... 2 ");

        /*
         * Get text color
         */
        Color clr = getDisplayColor(txt.getTextColor());
        RGB textColor = new RGB(clr.getRed(), clr.getGreen(), clr.getBlue());
        // System.out.println("findTextBoxRange for IText .... 2.5 ");

        /*
         * Get angle rotation for text. If rotation is "North" relative,
         * calculate the rotation for "Screen" relative.
         */
        double rotation = txt.getRotation();
        if (txt.getRotationRelativity() == TextRotation.NORTH_RELATIVE)
            rotation += northOffsetAngle(txt.getPosition());
        // System.out.println("findTextBoxRange for IText .... 3 ");

        /*
         * create drawableString and calculate its bounds
         */
        DrawableString dstring = new DrawableString(txt.getString(), textColor);
        dstring.font = font;
        dstring.setCoordinates(loc[0], loc[1]);
        dstring.textStyle = TextStyle.NORMAL;
        dstring.horizontalAlignment = HorizontalAlignment.CENTER;
        dstring.verticallAlignment = VerticalAlignment.MIDDLE;
        dstring.rotation = rotation;
        // System.out.println("findTextBoxRange for IText .... 3.1 ");

        Rectangle2D bounds = target.getStringsBounds(dstring);
        // System.out.println("findTextBoxRange for IText .... 3.1.1 ");
        double xOffset = (bounds.getWidth() + 1) * horizRatio / 2;
        double yOffset = (bounds.getHeight() + 1) * vertRatio / 2;
        // System.out.println("findTextBoxRange for IText .... 3.2 ");

        /*
         * Set proper alignment
         */
        HorizontalAlignment align = HorizontalAlignment.CENTER;
        double left = xOffset, right = xOffset;
        if (txt.getJustification() != null) {
            switch (txt.getJustification()) {
            case RIGHT_JUSTIFY:
                align = HorizontalAlignment.RIGHT;
                left = xOffset * 2;
                right = 0.0;
                break;
            case CENTER:
                align = HorizontalAlignment.CENTER;
                break;
            case LEFT_JUSTIFY:
                align = HorizontalAlignment.LEFT;
                left = 0.0;
                right = xOffset * 2;
                break;
            default:
                align = HorizontalAlignment.CENTER;
                break;
            }
        }
        // System.out.println("findTextBoxRange for IText .... 4 ");

        dstring.horizontalAlignment = align;

        if (dstring.rotation != 0.0) {
            AffineTransformation rotate = AffineTransformation
                    .rotationInstance(dstring.rotation, dstring.basics.x,
                            dstring.basics.y);
        }

        IExtent box = new PixelExtent(dstring.basics.x - left, dstring.basics.x
                + right, dstring.basics.y - yOffset, dstring.basics.y + yOffset);
        // System.out.println("findTextBoxRange for IText .... 5 ");

        List<Coordinate> rngBox = new ArrayList<Coordinate>();
        rngBox.add(new Coordinate(box.getMinX() - PgenRangeRecord.RANGE_OFFSET,
                box.getMaxY() + PgenRangeRecord.RANGE_OFFSET));
        rngBox.add(new Coordinate(box.getMaxX() + PgenRangeRecord.RANGE_OFFSET,
                box.getMaxY() + PgenRangeRecord.RANGE_OFFSET));
        rngBox.add(new Coordinate(box.getMaxX() + PgenRangeRecord.RANGE_OFFSET,
                box.getMinY() - PgenRangeRecord.RANGE_OFFSET));
        rngBox.add(new Coordinate(box.getMinX() - PgenRangeRecord.RANGE_OFFSET,
                box.getMinY() - PgenRangeRecord.RANGE_OFFSET));

        List<Coordinate> textPos = new ArrayList<Coordinate>();
        textPos.add(new Coordinate(loc[0], loc[1]));

        // System.out.println("findTextBoxRange for IText .... return ");

        return new PgenRangeRecord(rngBox, textPos, false);
    }

    /**
     * Find a range box that holds the symbol image.
     * 
     * @param symbolSet
     *            A symbol with associated lat/lon coordinates
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public PgenRangeRecord findSymbolRange(ISymbol sym,
            PaintProperties paintProps) {

        Coordinate[] loc = new Coordinate[] { sym.getLocation() };

        // Set up scale factors
        setScales(paintProps);
        double sfactor = deviceScale * symbolScale * sym.getSizeScale();

        /*
         * Get color for creating displayables.
         */
        Color[] dspClr = getDisplayColors(sym.getColors());

        /*
         * create an AWT BufferedImage from the symbol pattern
         */
        sfactor *= screenToWorldRatio;
        IRenderedImageCallback imageCb = new SymbolImageCallback(
                sym.getPatternName(), sfactor, sym.getLineWidth(),
                sym.isClear(), dspClr[0]);
        /*
         * Initialize raster image for use with graphics target
         */
        IImage pic = null;
        try {
            pic = target.initializeRaster(imageCb);
            pic.stage();
            // pic = target.initializeRaster( new IODataPreparer(image,
            // sym.getPatternName(), 0), null );
        } catch (Exception e) {
            System.out.println("SAG:IMAGE CREATION");
            e.printStackTrace();
        }

        /*
         * convert lat/lons to pixel coords
         */
        double[][] pts = PgenUtil.latlonToPixel(loc,
                (IMapDescriptor) iDescriptor);

        /*
         * Build range
         */
        List<Coordinate> rngBox = new ArrayList<Coordinate>();
        rngBox.add(new Coordinate(pts[0][0] - pic.getWidth() / 2
                - PgenRangeRecord.RANGE_OFFSET, pts[0][1] + pic.getHeight() / 2
                + PgenRangeRecord.RANGE_OFFSET));
        rngBox.add(new Coordinate(pts[0][0] + pic.getWidth() / 2
                + PgenRangeRecord.RANGE_OFFSET, pts[0][1] + pic.getHeight() / 2
                + PgenRangeRecord.RANGE_OFFSET));
        rngBox.add(new Coordinate(pts[0][0] + pic.getWidth() / 2
                + PgenRangeRecord.RANGE_OFFSET, pts[0][1] - pic.getHeight() / 2
                - PgenRangeRecord.RANGE_OFFSET));
        rngBox.add(new Coordinate(pts[0][0] - pic.getWidth() / 2
                - PgenRangeRecord.RANGE_OFFSET, pts[0][1] - pic.getHeight() / 2
                - PgenRangeRecord.RANGE_OFFSET));

        List<Coordinate> symPos = new ArrayList<Coordinate>();
        symPos.add(sym.getLocation());

        return new PgenRangeRecord(rngBox, symPos, false);

    }

    /**
     * Find a range box that holds the image of a combo symbol.
     * 
     * @param symbolSet
     *            A symbol with associated lat/lon coordinates
     * @param paintProps
     *            The paint properties associated with the target
     * @return A list of IDisplayable elements
     */
    public PgenRangeRecord findComboSymbolRange(ICombo combo,
            PaintProperties paintProps) {

        // Set up scale factors
        setScales(paintProps);
        double scale = deviceScale * symbolScale * combo.getSizeScale()
                * SymbolImageUtil.INITIAL_IMAGE_SIZE;

        String[] patterns = combo.getPatternNames();

        /*
         * Get pixel value for the world location
         */
        Coordinate[] loc = new Coordinate[] { combo.getLocation() };
        double[] worldPixel = new double[] { loc[0].x, loc[0].y, 0.0 };
        double[] pixel = iDescriptor.worldToPixel(worldPixel);

        /*
         * Get color for creating displayables.
         */
        Color[] dspClr = getDisplayColors(combo.getColors());

        /*
         * Calculate the offset for the first symbol ( upper left ) and convert
         * back to world coordinates.
         */
        double[] locUL = iDescriptor.pixelToWorld(new double[] {
                pixel[0] - (0.5 * scale), pixel[1] - (0.25 * scale), 0.0 });
        Coordinate[] loc1 = new Coordinate[] { new Coordinate(locUL[0],
                locUL[1]) };

        /*
         * Create a Symbol object for the first pattern
         */
        Symbol sym1 = new Symbol(null, dspClr, combo.getLineWidth(),
                combo.getSizeScale(), combo.isClear(), loc1[0], "Symbol",
                patterns[0]);
        PgenRangeRecord rng1 = findSymbolRange(sym1, paintProps);

        /*
         * Calculate the offset for the second symbol ( lower right ) and
         * convert back to world coordinates
         */
        double[] locLR = iDescriptor.pixelToWorld(new double[] {
                pixel[0] + (0.5 * scale), pixel[1] + (0.25 * scale), 0.0 });
        Coordinate[] loc2 = new Coordinate[] { new Coordinate(locLR[0],
                locLR[1]) };

        /*
         * Create a Symbol object for the second pattern
         */
        Symbol sym2 = new Symbol(null, dspClr, combo.getLineWidth(),
                combo.getSizeScale(), combo.isClear(), loc2[0], "Symbol",
                patterns[1]);
        PgenRangeRecord rng2 = findSymbolRange(sym2, paintProps);

        // add the "slash" symbol object
        Symbol sym3 = new Symbol(null, dspClr, combo.getLineWidth(),
                combo.getSizeScale(), combo.isClear(), loc[0], "Symbol",
                "SLASH");
        PgenRangeRecord rng3 = findSymbolRange(sym3, paintProps);

        /*
         * Build range
         */
        List<Coordinate> rngBox = new ArrayList<Coordinate>();
        double min_x = Math.min(rng1.getExtent().get(0).x,
                Math.min(rng2.getExtent().get(0).x, rng3.getExtent().get(0).x));
        double max_x = Math.max(rng1.getExtent().get(1).x,
                Math.max(rng2.getExtent().get(1).x, rng3.getExtent().get(1).x));
        double min_y = Math.min(rng1.getExtent().get(2).y,
                Math.min(rng2.getExtent().get(2).y, rng3.getExtent().get(2).y));
        double max_y = Math.max(rng1.getExtent().get(0).y,
                Math.max(rng2.getExtent().get(0).y, rng3.getExtent().get(0).y));

        rngBox.add(new Coordinate(min_x, max_y));
        rngBox.add(new Coordinate(max_x, max_y));
        rngBox.add(new Coordinate(max_x, min_y));
        rngBox.add(new Coordinate(min_x, min_y));

        List<Coordinate> comboPos = new ArrayList<Coordinate>();
        comboPos.add(combo.getLocation());

        return new PgenRangeRecord(rngBox, comboPos, false);

    }

    /**
     * Find the visible part of the screen.
     * 
     * @param paintProps
     *            The paint properties associated with the target
     * @return A PgenRangeRecord
     */
    public PgenRangeRecord findScreenRange(PaintProperties paintProps) {

        double minx = paintProps.getView().getExtent().getMinX();
        double miny = paintProps.getView().getExtent().getMinY();
        double maxx = paintProps.getView().getExtent().getMaxX();
        double maxy = paintProps.getView().getExtent().getMaxY();

        double dx = Math.abs(maxx - minx);
        double dy = Math.abs(maxy - miny);
        double dd = Math.min(dx, dy);

        double ratio = 0.02;
        double offset = dd * ratio;

        minx += offset;
        miny += offset;
        maxx -= offset;
        maxy -= offset;

        /*
         * Build range
         */
        List<Coordinate> rngBox = new ArrayList<Coordinate>();
        rngBox.add(new Coordinate(minx, maxy));
        rngBox.add(new Coordinate(maxx, maxy));
        rngBox.add(new Coordinate(maxx, miny));
        rngBox.add(new Coordinate(minx, miny));

        List<Coordinate> pos = new ArrayList<Coordinate>();
        pos.add(new Coordinate((maxx - minx) / 2, (maxy - miny) / 2));

        return new PgenRangeRecord(rngBox, pos, false);

    }

    /**
     * Find a range box that holds the image of a TCA.
     * 
     * @param tca
     *            A tca with associated lat/lon coordinates
     * @param paintProps
     *            The paint properties associated with the target
     * @return A PgenRangeRecord
     */
    public PgenRangeRecord findTcaRangeBox(ITca tca, PaintProperties paintProps) {

        List<TropicalCycloneAdvisory> advisories = tca.getAdvisories();
        ArrayList<Coordinate> allpts = new ArrayList<Coordinate>();

        // loop through each advisory.
        for (TropicalCycloneAdvisory tt : advisories) {
            BPGeography segment = tt.getSegment();

            // loop through each path defining the watch/warning segment
            for (Coordinate[] coords : segment.getPaths()) {
                // convert Coordinate[] to ArrayList<Coordinate>
                for (Coordinate c : coords)
                    allpts.add(c);
            }
        }

        double[][] pixels = PgenUtil.latlonToPixel(
                allpts.toArray(new Coordinate[allpts.size()]),
                (IMapDescriptor) iDescriptor);
        double[][] smoothpts = pixels;
        Coordinate[] pts = new Coordinate[smoothpts.length];

        for (int ii = 0; ii < smoothpts.length; ii++) {
            pts[ii] = new Coordinate(smoothpts[ii][0], smoothpts[ii][1]);
        }

        return new PgenRangeRecord(pts, false);
    }

    /**
     * Find a range box that holds a vector.
     * 
     * @param vect
     *            A vector with associated lat/lon coordinates
     * @param paintProps
     *            The paint properties associated with the target
     * @return A PgenRangeRecord
     */
    public PgenRangeRecord findVectorRangeBox(IVector vect,
            PaintProperties paintProps) {

        setScales(paintProps);
        double sfactor;
        double[] start;
        double[] tmp;
        double angle;
        double speed;

        ArrayList<Coordinate> allpts = new ArrayList<Coordinate>();

        /*
         * Create appropriate vector representation
         */
        switch (vect.getVectorType()) {

        case ARROW:

            sfactor = deviceScale * vect.getSizeScale();

            tmp = new double[] { vect.getLocation().x, vect.getLocation().y,
                    0.0 };
            start = iDescriptor.worldToPixel(tmp);

            // calculate the length of the arrow, and its direction
            speed = 10.;
            if (!vect.hasDirectionOnly())
                speed = vect.getSpeed();
            double arrowLength = sfactor * speed;
            angle = 90.0 - northOffsetAngle(vect.getLocation())
                    + vect.getDirection();

            // find the end point (tip of the arrow)
            double[] end = new double[3];
            end[0] = start[0] + (arrowLength * Math.cos(Math.toRadians(angle)));
            end[1] = start[1] + (arrowLength * Math.sin(Math.toRadians(angle)));

            allpts.add(new Coordinate(start[0], start[1]));
            allpts.add(new Coordinate(end[0], end[1]));

            break;

        case WIND_BARB:
            sfactor = deviceScale * vect.getSizeScale() * 10.;

            /*
             * Convert location from lat/lon coordinates to pixel
             */
            tmp = new double[] { vect.getLocation().x, vect.getLocation().y,
                    0.0 };
            start = iDescriptor.worldToPixel(tmp);

            /*
             * If calm wind, draw circle
             */
            if (vect.getSpeed() < 0.5) {
                double[][] pts = calculateCircle(start, sfactor * 0.1);
                for (int ii = 0; ii < pts.length; ii++) {
                    allpts.add(new Coordinate(pts[ii][0], pts[ii][1]));
                }
                break;
            }

            // Compute the number of flags, whole barbs and half barbs needed to
            // represent the wind speed.
            int speedt = (int) Math.floor(vect.getSpeed() + 2.5);
            int numflags = speedt / 50;
            int remainder = speedt % 50;
            int numbarbs = remainder / 10;
            remainder = remainder % 10;
            int halfbarbs = remainder / 5;

            double MAX_SEGMENTS = 6.0; // Maximum number of segments on original
                                       // size barb
            int numsegs = (2 * numflags) + numbarbs + halfbarbs;
            double segmentSpacing = sfactor / MAX_SEGMENTS;
            double windLength = segmentSpacing
                    * Math.max(MAX_SEGMENTS, numsegs);
            double barbLength = sfactor / 3.0;

            // find the end point of the wind barb
            angle = -90.0 - northOffsetAngle(vect.getLocation())
                    + vect.getDirection();
            end = new double[3];
            end[0] = start[0] + (windLength * Math.cos(Math.toRadians(angle)));
            end[1] = start[1] + (windLength * Math.sin(Math.toRadians(angle)));
            end[2] = 0.0;

            /*
             * Create a LengthIndexedLine used to reference points along the
             * path at specific distances
             */
            LineString[] ls = toLineString(new Coordinate[] {
                    new Coordinate(start[0], start[1]),
                    new Coordinate(end[0], end[1]) });
            LengthIndexedLine lil = new LengthIndexedLine(ls[0]);
            double currentLoc = lil.getEndIndex(); // start from tail end

            // TODO - orientation issues
            double BARB_ANGLE = 70.0;
            double barbAngle = angle + BARB_ANGLE;
            if (vect.getLocation().y < 0.0)
                barbAngle = angle - BARB_ANGLE;
            double cosineBarbAngle = Math.cos(Math.toRadians(barbAngle));
            double sineBarbAngle = Math.sin(Math.toRadians(barbAngle));

            /*
             * Process flags
             */
            for (int j = 0; j < numflags; j++) {
                Coordinate coords[] = new Coordinate[4];
                coords[0] = lil.extractPoint(currentLoc);
                coords[1] = lil.extractPoint(currentLoc - segmentSpacing);
                double xtip = coords[1].x + (barbLength * cosineBarbAngle);
                double ytip = coords[1].y + (barbLength * sineBarbAngle);
                coords[2] = new Coordinate(xtip, ytip);
                coords[3] = coords[0];
                allpts.add(coords[0]);
                allpts.add(coords[1]);
                allpts.add(coords[2]);
                currentLoc -= 2 * segmentSpacing;
            }

            /*
             * Process barbs
             */
            for (int j = 0; j < numbarbs; j++) {
                Coordinate coords[] = new Coordinate[2];
                coords[0] = lil.extractPoint(currentLoc);
                double xtip = coords[0].x + (barbLength * cosineBarbAngle);
                double ytip = coords[0].y + (barbLength * sineBarbAngle);
                coords[1] = new Coordinate(xtip, ytip);
                allpts.add(coords[0]);
                allpts.add(coords[1]);
                currentLoc -= segmentSpacing;
            }

            /*
             * Process half barbs
             */
            for (int j = 0; j < halfbarbs; j++) {
                Coordinate coords[] = new Coordinate[2];
                coords[0] = lil.extractPoint(currentLoc);
                double xtip = coords[0].x
                        + (0.5 * barbLength * cosineBarbAngle);
                double ytip = coords[0].y + (0.5 * barbLength * sineBarbAngle);
                coords[1] = new Coordinate(xtip, ytip);
                allpts.add(coords[0]);
                allpts.add(coords[1]);
                currentLoc -= segmentSpacing;
            }

            break;

        case HASH_MARK:
            sfactor = 10.0 * deviceScale; // scale factor for length of
                                          // hash
            // lines
            double spaceFactor = sfactor * 0.25; // scale factor for spacing
                                                 // between
            // hash lines
            double spacing = 1.0 * spaceFactor; // distance between hash lines
            if (vect.getLineWidth() > 3.0)
                spacing += (0.25 * spaceFactor * (vect.getLineWidth() - 3));

            double scaleSize = sfactor * vect.getSizeScale(); // hash line
                                                              // length

            // Convert location from lat/lon coordinates to pixel coordinates
            tmp = new double[] { vect.getLocation().x, vect.getLocation().y,
                    0.0 };
            start = iDescriptor.worldToPixel(tmp);

            /*
             * calculate the angle and distance to the four points defining the
             * hash mark
             */
            angle = northOffsetAngle(vect.getLocation()) + vect.getDirection();
            double theta = Math.toDegrees(Math.atan(spacing / scaleSize));
            double dist = 0.5 * Math.sqrt((spacing * spacing)
                    + (scaleSize * scaleSize));

            /*
             * find the X and Y offsets from the center to the end points of
             * hash lines
             */
            double dX1 = dist * Math.cos(Math.toRadians(angle - theta));
            double dY1 = dist * Math.sin(Math.toRadians(angle - theta));

            double dX2 = dist * Math.cos(Math.toRadians(angle + theta));
            double dY2 = dist * Math.sin(Math.toRadians(angle + theta));

            // add both hash mark lines
            allpts.add(new Coordinate(start[0] + dX1, start[1] - dY1));
            allpts.add(new Coordinate(start[0] - dX2, start[1] + dY2));
            allpts.add(new Coordinate(start[0] - dX1, start[1] + dY1));
            allpts.add(new Coordinate(start[0] + dX2, start[1] - dY2));

            break;

        default:
            // Unrecognized vector type; return empty list

        }

        return new PgenRangeRecord(allpts, false);
    }
}
