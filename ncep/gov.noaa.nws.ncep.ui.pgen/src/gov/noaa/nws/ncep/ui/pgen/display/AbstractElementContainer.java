/*
 * AbstractElementContainer
 * 
 * Date created: 08 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.PgenAutoPlacement;
import gov.noaa.nws.ncep.ui.pgen.PgenRangeRecord;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Label;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcm;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ISigmet;
import gov.noaa.nws.ncep.ui.pgen.tca.ITca;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * This Element Container is the base class for all Element Containers. It's
 * function is to hold a PGEN DrawableElement along with associated renderable
 * objects that depict the DrawableElement on the graphics target.
 * 
 * Subclasses' implementation of the draw method should determine when the
 * IDisplayables for the Drawable Element should be recreated. IDisplayables can
 * be created using the createDisplayables() method.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/09     	#160        G. Zhang    Added ISigmet for Sigmet support
 * 03/10		#223		M.Laryukhin	Gfa added. 
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 09/12					B. Hebbard  Merge RTS changes from OB12.9.1
 * 11/13        TTR 752     J. Wu       added methods to auto place CCFP text box.
 * </pre>
 * 
 * @author sgilbert
 */
public abstract class AbstractElementContainer {

    /*
     * The PGEN Drawable Element to be rendered.
     */
    protected DrawableElement element;

    protected IMapDescriptor mapDescriptor;

    protected IGraphicsTarget target;

    protected DisplayElementFactory def;

    protected static final double RANGE_OFFSET = 10; // In screen pixels.

    /*
     * Objects that can be rendered on the graphics target depicting the
     * DrawableElement.
     */
    protected List<IDisplayable> displayEls = null;

    /**
     * @param element
     * @param mapDescriptor
     */
    public AbstractElementContainer(DrawableElement element,
            IMapDescriptor mapDescriptor, IGraphicsTarget target) {
        this.element = element;
        this.mapDescriptor = mapDescriptor;
        this.target = target;
        def = new DisplayElementFactory(target, mapDescriptor);
    }

    /**
     * Sets a new mapDescriptor. All IDisplayables will be recreated.
     * 
     * @param mapDescriptor
     *            the mapDescriptor to set
     */
    public void setMapDescriptor(IMapDescriptor mapDescriptor) {
        this.mapDescriptor = mapDescriptor;
        def = new DisplayElementFactory(target, mapDescriptor);
        dispose();
        displayEls = null;
    }

    /**
     * Draws to the given graphics target. Recreates the IDisplayable objects,
     * if necessary.
     * 
     * @param target
     * @param paintProps
     * @param dprops
     *            PGEN Layer properties
     */
    public abstract void draw(IGraphicsTarget target,
            PaintProperties paintProps, DisplayProperties dprops);

    /**
     * Draws to the given graphics target. Recreates the IDisplayable objects,
     * if necessary.
     * 
     * @param target
     * @param paintProps
     * @param dprops
     *            PGEN Layer properties
     * @param needsCreate
     */
    public abstract void draw(IGraphicsTarget target,
            PaintProperties paintProps, DisplayProperties dprops,
            boolean needsCreate);

    /**
     * Uses a DisplayElementFactory to create IDisplayable objects from the
     * Drawable Element
     * 
     * @param paintProps
     */
    protected void createDisplayables(PaintProperties paintProps) {

        // Cleanup first
        if ((displayEls != null) && !displayEls.isEmpty()) {
            reset();
        }

        // Set range for this element.
        setRange(element, paintProps);

        // Adjust the location of text box here, as well as its pointed arrow?
        if (isCCFPText(element) && PgenUtil.getTextAutoPlacement()) {
            adjustRange(element, mapDescriptor, paintProps);
        }

        // Create displayables
        if (element instanceof IAvnText) {
            displayEls = def.createDisplayElements((IAvnText) element,
                    paintProps);
        } else if (element instanceof IMidCloudText) {
            displayEls = def.createDisplayElements((IMidCloudText) element,
                    paintProps);
        } else if (element instanceof IText) {
            displayEls = def.createDisplayElements((IText) element, paintProps);
        } else if (element instanceof IVector) {
            displayEls = def.createDisplayElements((IVector) element,
                    paintProps);
        } else if (element instanceof ICombo) {
            displayEls = def
                    .createDisplayElements((ICombo) element, paintProps);
        } else if (element instanceof ITca) {
            displayEls = def.createDisplayElements((ITca) element, paintProps);
        } else if (element instanceof ISigmet) {
            displayEls = def.createDisplayElements((ISigmet) element,
                    paintProps);
        } else if (element instanceof ISymbol) {
            displayEls = def.createDisplayElements((ISymbol) element,
                    paintProps);
        } else if (element instanceof ITrack) {
            displayEls = def
                    .createDisplayElements((ITrack) element, paintProps);
        } else if (element instanceof IWatchBox) {
            displayEls = def.createDisplayElements((IWatchBox) element,
                    paintProps);
        } else if (element instanceof ITcm) {
            displayEls = def.createDisplayElements((ITcm) element, paintProps);
        } else if (element instanceof IMultiPoint) {
            if (element instanceof IKink) {
                displayEls = def.createDisplayElements((IKink) element,
                        paintProps);
            } else if (element instanceof IArc) {
                displayEls = def.createDisplayElements((IArc) element,
                        paintProps);
            } else if (element instanceof IGfa) {
                displayEls = def.createDisplayElements((IGfa) element,
                        paintProps);
            } else if (element instanceof ILine) {
                displayEls = def.createDisplayElements((ILine) element,
                        paintProps, true);
            }
        }
    }

    /*
     * Set an DrawbleElement's range record
     */
    private void setRange(DrawableElement elem, PaintProperties paintProps) {
        setRange(elem, mapDescriptor, paintProps);
    }

    /*
     * Set a text element's range record
     */
    private void setRange(DrawableElement elem, IMapDescriptor mapDescriptor2,
            PaintProperties paintProps) {
        if (elem instanceof ITca) {
            elem.setRange(def.findTcaRangeBox((ITca) elem, paintProps));
        } else if (elem instanceof IVector) {
            elem.setRange(def.findVectorRangeBox((IVector) elem, paintProps));
        } else if (elem instanceof ISinglePoint) {
            PgenRangeRecord rng = null;
            if (elem instanceof IText) {
                rng = def.findTextBoxRange((IText) elem, paintProps);
            } else if (elem instanceof ISymbol) {
                rng = def.findSymbolRange((ISymbol) elem, paintProps);
            } else if (elem instanceof ICombo) {
                rng = def.findComboSymbolRange((ICombo) elem, paintProps);
            }

            if (rng != null) {
                elem.setRange(rng);
            }

        } else if (elem instanceof IMultiPoint) {
            double[][] pixels = PgenUtil.latlonToPixel(
                    ((IMultiPoint) elem).getLinePoints(), mapDescriptor2);
            double[][] smoothpts = pixels;
            float density;
            /*
             * Apply parametric smoothing on pixel coordinates, if required.
             * 
             * Note: 1. NMAP2 range calculation does not do smoothing though. 2.
             * Tcm and WatchBox is IMultiPoint but not ILine.
             */
            boolean smoothIt = true;
            if (smoothIt && elem instanceof ILine
                    && ((ILine) elem).getSmoothFactor() > 0) {
                if (((ILine) elem).getSmoothFactor() > 0) {
                    float devScale = 50.0f;
                    if (((ILine) elem).getSmoothFactor() == 1)
                        density = devScale / 1.0f;
                    else
                        density = devScale / 5.0f;

                    smoothpts = CurveFitter.fitParametricCurve(pixels, density);
                }
            }

            Coordinate[] pts = new Coordinate[smoothpts.length];

            for (int ii = 0; ii < smoothpts.length; ii++) {
                pts[ii] = new Coordinate(smoothpts[ii][0], smoothpts[ii][1]);
            }

            boolean closed = false;
            if (elem instanceof ILine) {
                closed = ((ILine) elem).isClosedLine();
            }

            elem.createRange(pts, closed);

        } else {
            System.out
                    .println("Invalid DrawableElement type. No range record is set!");
        }
    }

    // Reset
    private void reset() {
        def.reset();
    }

    /**
     * Releases the resources held by any of the IDisplayables
     */
    public void dispose() {

        if (displayEls == null)
            return;

        for (IDisplayable each : displayEls) {
            each.dispose();
        }
        displayEls.clear();
    }

    public void setElement(DrawableElement el) {
        this.element = el;
    }

    /*
     * Adjust a CCFP text box location and range record
     */
    private void adjustRange(DrawableElement elem,
            IMapDescriptor mapDescriptor2, PaintProperties paintProps) {

        if (!isCCFPText(elem))
            return;

        // find the extent of the screen view.
        PgenRangeRecord screenExtent = def.findScreenRange(paintProps);

        // find polygon and arrow associated with the text box.
        DrawableElement ccfpLine = getCCFPLine(elem);
        DrawableElement ccfpArrow = getCCFPArrow(elem);

        PgenRangeRecord ccfpRange = ccfpLine.getRange();
        PgenRangeRecord arrowRange = new PgenRangeRecord();
        if (ccfpArrow != null)
            arrowRange = ccfpArrow.getRange();

        // check if the polygon is within the screen, if not, no need to adjust.
        GeometryFactory gf = new GeometryFactory();
        Geometry ccfpPolygon = PgenAutoPlacement.pointsToGeometry(
                ccfpRange.getPoints(), true, gf);
        Geometry scnPoly = PgenAutoPlacement.pointsToGeometry(
                screenExtent.getExtent(), true, gf);

        if (!ccfpPolygon.intersects(scnPoly)) {
            return;
        }

        // Now start the text box from the centroid of the CCFP polygon.
        Point ccfpCenter = ccfpPolygon.getCentroid();

        double[] nloc = mapDescriptor2.pixelToWorld(new double[] {
                ccfpCenter.getX(), ccfpCenter.getY(), 0.0 });
        ((Text) elem).setLocationOnly(new Coordinate(nloc[0], nloc[1]));

        PgenRangeRecord textBox = def
                .findTextBoxRange((IText) elem, paintProps);

        elem.setRange(textBox);

        /*
         * Get range records for all elements in the current activity - if they
         * are in the current layer or it's layer is on, excluding the elements
         * that belongs to this text's (associated polygon and arrow).
         */
        List<DrawableElement> delist = PgenSession.getInstance()
                .getPgenResource().getActiveDrawableElements();

        List<PgenRangeRecord> rrlist = new ArrayList<PgenRangeRecord>();

        for (DrawableElement de : delist) {
            if (de == elem || de == ccfpLine
                    || (ccfpArrow != null && de == ccfpArrow)) {
                continue;
            }

            rrlist.add(de.getRange());
        }

        // Do auto placement.
        PgenAutoPlacement pgenAuto = new PgenAutoPlacement(textBox, ccfpRange,
                arrowRange, rrlist, screenExtent, true, 5.0, 1.25);
        PgenRangeRecord[] newLoc = pgenAuto.placeTextBox();

        // Reset the text location to the location found.
        double[] autoloc = mapDescriptor2.pixelToWorld(new double[] {
                newLoc[0].getPoints().get(0).x, newLoc[0].getPoints().get(0).y,
                0.0 });
        ((Text) elem).setLocationOnly(new Coordinate(autoloc[0], autoloc[1]));

        // adjust the arrow.
        if (newLoc[1].getPoints().size() <= 1) { // remove arrow
            if (ccfpArrow != null) {
                ((Label) element.getParent()).removeElement(ccfpArrow);
            }
        } else { // add an arrow or adjust the location
            ArrayList<Coordinate> apts = new ArrayList<Coordinate>();

            // Reset the test location to the location found.
            double[] aloc = mapDescriptor2.pixelToWorld(new double[] {
                    newLoc[1].getPoints().get(0).x,
                    newLoc[1].getPoints().get(0).y, 0.0 });
            apts.add(new Coordinate(aloc[0], aloc[1]));
            aloc = mapDescriptor2.pixelToWorld(new double[] {
                    newLoc[1].getPoints().get(1).x,
                    newLoc[1].getPoints().get(1).y, 0.0 });
            apts.add(new Coordinate(aloc[0], aloc[1]));

            if (ccfpArrow != null) {
                ((Line) ccfpArrow).setPointsOnly(apts);
            } else {
                Line arrowLn = new Line(null, new Color[] { new Color(255, 255,
                        255) }, 2.0F, 1.0, false, false, apts, 0,
                        FillPattern.FILL_PATTERN_2, "Lines", "POINTED_ARROW");
                ((Label) element.getParent()).addArrow(arrowLn);
            }
        }
    }

    /*
     * Check if a Text is part of a CCFP sigmet
     */
    private boolean isCCFPText(DrawableElement de) {
        return (de instanceof IText && de.getParent() != null
                && de.getParent().getParent() != null
                && de.getParent().getParent().getPgenType() != null && de
                .getParent().getParent().getPgenType().equals("CCFP_SIGMET"));
    }

    /*
     * Find the main CCFP polygon if a Text belongs to this CCFP sigmet
     */
    private DrawableElement getCCFPLine(DrawableElement de) {
        if (isCCFPText(de))
            return de.getParent().getParent().getPrimaryDE();
        else
            return null;
    }

    /*
     * Check if a pointed arrow is part of a CCFP sigmet
     */
    protected boolean isCCFPArrow(DrawableElement de) {
        boolean isccfparrow = false;

        if (de instanceof ILine && (de.getParent() != null)
                && de.getParent().getParent() != null) {
            if (de.getParent().getParent().getPgenType() != null
                    && de.getParent().getParent().getPgenType()
                            .equals("CCFP_SIGMET")) {
                isccfparrow = true;
            }
        }

        return isccfparrow;

    }

    /*
     * Find the pointed arrow associated with the CCFP sigmet's text box.
     */
    private DrawableElement getCCFPArrow(DrawableElement de) {
        DrawableElement arrow = null;
        if (isCCFPText(de)) {
            Iterator<DrawableElement> iterator = de.getParent()
                    .createDEIterator();
            while (iterator.hasNext()) {
                DrawableElement el = iterator.next();
                if (el instanceof ILine) {
                    arrow = el;
                    break;
                }
            }
        }

        return arrow;
    }

}
