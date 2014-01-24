/*
 * Line
 * 
 * Date created: 15 January 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Class to represent a line element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/09					J. Wu   	Initial Creation.
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory to constructors and copy()
 * 03/10		#223		M.Laryukhin	getCentroid() method is added
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 0.0.1
 */
@ElementOperations({ Operation.CONNECT, Operation.COPY_MOVE,
        Operation.EXTRAPOLATE, Operation.FLIP, Operation.DELETE_PART,
        Operation.DELETE_POINT, Operation.MODIFY, Operation.INTERPOLATE,
        Operation.ADD_POINT })
public class Line extends MultiPointElement implements ILine {

    private boolean flipSide = false;

    /**
     * Default constructor
     */
    public Line() {
    }

    /**
     * @param deleted
     * @param range
     * @param colors
     * @param lineWidth
     * @param sizeScale
     * @param closed
     * @param filled
     * @param linePoints
     * @param smoothFactor
     * @param fillPattern
     * @param pgenCategory
     * @param pgenType
     */
    public Line(Coordinate[] range, Color[] colors, float lineWidth,
            double sizeScale, boolean closed, boolean filled,
            List<Coordinate> linePoints, int smoothFactor,
            FillPattern fillPattern, String pgenCategory, String pgenType) {
        super(range, colors, lineWidth, sizeScale, closed, filled, linePoints,
                smoothFactor, fillPattern, pgenCategory, pgenType);
    }

    /**
     * @return the string
     */
    public String toString() {
        StringBuilder result = new StringBuilder(getClass().getSimpleName());

        result.append("Category:\t" + pgenCategory + "\n");
        result.append("Type:\t" + pgenType + "\n");
        result.append("Color:\t" + colors[0] + "\n");
        result.append("LineWidth:\t" + lineWidth + "\n");
        result.append("SizeScale:\t" + sizeScale + "\n");
        result.append("Closed:\t" + closed + "\n");
        result.append("Filled:\t" + filled + "\n");
        result.append("SmoothFactor:\t" + smoothFactor + "\n");
        result.append("FillPattern:\t" + fillPattern + "\n");
        result.append("Location:\t\n");
        for (Coordinate point : linePoints) {
            result.append("\t" + point.x + "\t" + point.y + "\n");
        }

        return result.toString();
    }

    /**
     * Update the attributes for the object
     */
    @Override
    public void update(IAttribute iattr) {
        super.update(iattr);
        if (iattr instanceof ILine) {
            ILine attr = (ILine) iattr;
            this.setClosed(attr.isClosedLine());
            this.setFilled(attr.isFilled());
            this.setSmoothFactor(attr.getSmoothFactor());
            if (attr.isFilled() != null && attr.isFilled())
                this.setFillPattern(attr.getFillPattern());
        }
    }

    /**
     * Creates a copy of this object. This is a deep copy and new objects are
     * created so that we are not just copying references of objects
     */
    @Override
    public DrawableElement copy() {

        /*
         * create a new Line object and initially set its attributes to this
         * one's
         */
        Line newLine = new Line();

        /*
         * new Strings are created for Type and LinePattern
         */
        newLine.setPgenCategory(new String(this.getPgenCategory()));
        newLine.setPgenType(new String(this.getPgenType()));
        newLine.setParent(this.getParent());

        newLine.update(this);

        /*
         * new Coordinates points are created and set, so we don't just set
         * references
         */
        ArrayList<Coordinate> ptsCopy = new ArrayList<Coordinate>();
        for (int i = 0; i < this.getPoints().size(); i++) {
            ptsCopy.add(new Coordinate(this.getPoints().get(i)));
        }
        newLine.setPoints(ptsCopy);

        /*
         * new colors are created and set, so we don't just set references
         */
        Color[] colorCopy = new Color[this.getColors().length];
        for (int i = 0; i < this.getColors().length; i++) {
            colorCopy[i] = new Color(this.getColors()[i].getRed(),
                    this.getColors()[i].getGreen(),
                    this.getColors()[i].getBlue());
        }
        newLine.setColors(colorCopy);
        newLine.setFlipSide(this.flipSide);

        return newLine;

    }

    /**
     * @return the flipSide
     */
    public boolean isFlipSide() {
        return flipSide;
    }

    /**
     * @param flipSide
     *            the flipSide to set
     */
    public void setFlipSide(boolean flipSide) {
        this.flipSide = flipSide;
    }

    /**
     * Gets the name of the line pattern associated with this object
     */
    @Override
    public String getPatternName() {
        return getPgenType();
    }

    /**
     * Gets the smooth factor used to create line path
     * 
     * @return Line smoothing factor
     */
    @Override
    public int getSmoothFactor() {
        return smoothFactor;
    }

    /**
     * Checks whether the line path is closed.
     * 
     * @return true, if line path is closed.
     */
    @Override
    public Boolean isClosedLine() {
        return closed;
    }

    /**
     * Checks whether the object should be filled
     * 
     * @return true, if a fill pattern applies
     */
    @Override
    public Boolean isFilled() {
        return filled;
    }

    /**
     * Specifies the Fill Pattern to use, if isFilled returns true.
     * 
     * @return The Fill Pattern associated with the object
     */
    @Override
    public FillPattern getFillPattern() {
        FillPattern fp = FillPattern.SOLID;

        if (fillPattern != null) {
            fp = fillPattern;
        }

        return fp;

    }

    /**
     * Get the east most point
     * 
     * @return
     */
    public Coordinate getEastMostPoint() {
        Coordinate eastMostPt = getPoints().get(0);

        for (Coordinate pt : getPoints()) {
            if (pt.x > eastMostPt.x) {
                eastMostPt = pt;
            }
        }

        return eastMostPt;
    }

    /**
     * Returns the coordinate of the centroid constructed from the points of
     * this line.
     * 
     * @return
     */
    public Coordinate getCentroid() {
        if (getPoints().size() < 2) {
            return null;
        }
        GeometryFactory factory = new GeometryFactory();
        Coordinate[] a = new Coordinate[getPoints().size() + 1];
        getPoints().toArray(a);
        a[a.length - 1] = a[0]; // add the first point to the end
        LineString g = factory.createLineString(a);
        Point p = g.getCentroid();
        return p.getCoordinate();
    }

}
