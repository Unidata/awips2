/*
 * DrawableElement
 * 
 * Date created: 15 January 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.PgenRangeRecord;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Define the base class to represent a drawable element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/09					J. Wu   	Initial Creation.
 * 04/09        #72			S. Gilbert  Removed "deleted" variable from attribute,
 *                                      constructor and getter/setters.  No longer needed.
 *                                      Added default IText methods.
 * 04/09        #89			J. Wu		Added default IArc methods.
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory to constructors and accessors
 * 05/09        #111		J. Wu		Added default IVector methods.
 * 06/09		#116		B. Yin		Extends from AbstractDrawableComponent
 * 03/10		#223		M.Laryukhin	Gfa added. 
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 11/13        TTR 752     J. Wu       Used new PgenRangeRecord for auto placement.
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 0.1
 */
public abstract class DrawableElement extends AbstractDrawableComponent
        implements IAttribute {

    // Coordinate[] range;
    PgenRangeRecord range;

    /**
     * Default Constructor
     */
    protected DrawableElement() {
        range = null;
    }

    /**
     * @param range
     * @param pgenCategory
     * @param pgenType
     */
    protected DrawableElement(Coordinate[] range, String pgenCategory,
            String pgenType) {
        this.range = new PgenRangeRecord(range, false);
        this.pgenCategory = pgenCategory;
        this.pgenType = pgenType;
    }

    /**
     * Gets the range
     * 
     * @return range
     */
    public PgenRangeRecord getRange() {
        if (range == null) {
            range = new PgenRangeRecord();
        }
        return range;
    }

    /**
     * Sets the range
     */
    public void setRange(PgenRangeRecord range) {
        this.range = range;
    }

    /**
     * Sets the range - build from an array of points.
     */
    public void createRange(Coordinate[] points, boolean closed) {
        this.range = new PgenRangeRecord(points, closed);
    }

    /**
     * Gets array of colors associated with the object
     * 
     * @return Color array
     */
    public Color[] getColors() {
        return null;
    }

    /**
     * Gets the width of the line pattern
     * 
     * @return line width
     */
    public float getLineWidth() {
        return 0;
    }

    /**
     * Gets the size scale factor for the object
     * 
     * @return size scale factor
     */
    public double getSizeScale() {
        return 0.0;
    }

    /**
     * Updates the elements' information.
     */
    public abstract void update(IAttribute attr);

    /*
     * setPoints sets the points and may perform other actions, such as snap for
     * jet.
     */
    public void setPoints(ArrayList<Coordinate> pts) {
        setPointsOnly(pts);
    }

    public abstract void setPointsOnly(ArrayList<Coordinate> pts);

    /**
     * Return an iterator for itself. This is to make sure DEs and DeCollections
     * have the save behavior
     */
    public Iterator<DrawableElement> createDEIterator() {
        return new SelfIterator(this);
    }

    /**
     * Return itself
     */
    public DrawableElement getPrimaryDE() {
        return this;
    }

    /**
     * return Pgen type
     */
    public String getName() {
        return getPgenType();
    }

}
