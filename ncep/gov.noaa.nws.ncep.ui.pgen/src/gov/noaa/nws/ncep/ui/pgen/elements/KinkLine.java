/*
 * KinkLine
 * 
 * Date created: 15 January 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead.ArrowHeadType;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IKink;

import java.awt.Color;
import java.util.ArrayList;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class to represent a line element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/09					J. Wu   	Initial Creation.
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory to constructors
 * 11/13        #1065       J. Wu       Added copy() and modify update().
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 0.0.1
 */
@ElementOperations({ Operation.COPY_MOVE, Operation.EXTRAPOLATE,
        Operation.DELETE_PART, Operation.DELETE_POINT })
public class KinkLine extends Line implements IKink {

    double kinkPosition;

    ArrowHeadType arrowHeadType;

    /**
     * Default constructor
     */
    public KinkLine() {
        kinkPosition = 0.5;
        arrowHeadType = ArrowHeadType.FILLED;
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
     * @param pgenType
     * @param pgenCategory
     * @param kinkPosition
     * @param arrowHeadType
     */
    public KinkLine(Coordinate[] range, Color[] colors, float lineWidth,
            double sizeScale, boolean closed, boolean filled,
            ArrayList<Coordinate> linePoints, int smoothFactor,
            FillPattern fillPattern, String pgenCategory, String pgenType,
            double kinkPosition, ArrowHeadType arrowHeadType) {
        super(range, colors, lineWidth, sizeScale, closed, filled, linePoints,
                smoothFactor, fillPattern, pgenCategory, pgenType);
        this.kinkPosition = kinkPosition;
        this.arrowHeadType = arrowHeadType;
    }

    /**
     * Gets the color for the object
     */
    @Override
    public Color getColor() {
        return colors[0];
    }

    /**
     * Sets the kink position for the object
     * 
     * @return type
     */
    public void setKinkPosition(double kinkPosition) {
        this.kinkPosition = kinkPosition;
    }

    /**
     * Gets the kink position for the object
     */
    @Override
    public double getKinkPosition() {
        return kinkPosition;
    }

    /**
     * Sets the arrow head type for the object
     * 
     * @return type
     */
    public void setArrowHeadType(ArrowHeadType arrowHeadType) {
        this.arrowHeadType = arrowHeadType;
    }

    /**
     * Gets the arrow head type for the object
     */
    @Override
    public ArrowHeadType getArrowHeadType() {
        return arrowHeadType;
    }

    /**
     * Gets the start point for the object
     */
    @Override
    public Coordinate getStartPoint() {
        if (linePoints.size() > 0) {
            return linePoints.get(0);
        } else {
            return null;
        }
    }

    /**
     * Gets the end point for the object
     */
    @Override
    public Coordinate getEndPoint() {
        if (linePoints.size() > 1) {
            return linePoints.get(linePoints.size() - 1);
        } else {
            return null;
        }
    }

    /**
     * Update the attributes for the object
     */
    @Override
    public void update(IAttribute iattr) {

        super.update(iattr);

        if (iattr instanceof IKink) {
            IKink attr = (IKink) iattr;
            this.setKinkPosition(attr.getKinkPosition());
            this.setArrowHeadType(attr.getArrowHeadType());
        }
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
        result.append("FillPattern:\t" + fillPattern + "\n");
        result.append("KinkPosition:\t" + kinkPosition + "\n");
        result.append("ArrowHeadType:\t" + arrowHeadType + "\n");
        result.append("Location:\t\n");
        for (Coordinate point : linePoints) {
            result.append("\t" + point.x + "\t" + point.y + "\n");
        }

        return result.toString();
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
        KinkLine newLine = new KinkLine();

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

        newLine.setFlipSide(super.isFlipSide());

        return newLine;

    }

}
