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
package fsl.tools.scribble;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Point;
import java.awt.Shape;
import java.awt.geom.AffineTransform;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.viz.adapter.CoordConverter;

import fsl.tools.glyph.ArcEdgeAdornment;
import fsl.tools.glyph.TriangleEdgeAdornment;

/**
 * The drawable representation for a stationary front,
 * with cold front symbols on one side and warm front
 * symbols on the other.  The path may be created with
 * an arbitrary thickness and symbol size.
 *
 * @author Christopher Golden
 */
public class StationaryFront extends Front {


	// Protected Variables

	/**
	 * Flag indicating whether or not the front is aloft.
	 */
	protected boolean aloft;

	/**
	 * Arc edge adornment.
	 */
	protected ArcEdgeAdornment arcAdornment;

	/**
	 * Dashed shape used to overlay the solid shape of
	 * the path in order to create a dashed effect.
	 */
	protected transient Shape dashedShape = null;


	// Public Constructors

	/**
	 * Create an instance with the specified thickness,
	 * symbol size, symbol interval, arrangement of
	 * symbols with respect to on which side of the
	 * path each type is drawn, and altitude (aloft or
	 * on the ground).
	 *
	 * @param frame           Frame in which this glyph
	 *                        exists; if <code>
	 *                        ALL_FRAMES</code>, the
	 *                        glyph exists in all frames.
	 * @param thickness       Width of the path.
	 * @param smoothing       Smoothing type; must be one
	 *                        of the various <code>
	 *                        SMOOTHING_xxx</code>
	 *                        constants.
	 * @param symbolSize      Width of each cold and
	 *                        warm front symbol.
	 * @param symbolInterval  Interval along the path
	 *                        between each cold and
	 *                        warm front symbol.
	 * @param coldOnRightSide Flag indicating whether
	 *                        the cold front symbols
	 *                        should be on the left or
	 *                        the right side of the
	 *                        path.  The warm front
	 *                        symbols are always on the
	 *                        opposite side of the path.
	 * @param aloft           Flag indicating whether
	 *                        the front is aloft or not.
	 */
	public StationaryFront(int frame, int thickness, int smoothing,
						   double symbolSize, double symbolInterval,
						   boolean coldOnRightSide, boolean aloft) {
		super(frame, Color.blue, SOLID_STYLE, thickness, smoothing,
			  symbolSize, symbolInterval, coldOnRightSide);
		this.aloft = aloft;
		createAdornments();
	}


	// Public Methods

    /**
     * Determines whether another object is equal to this
	 * object.
	 *
     * @param  obj The object to which this object is to
	 *             be compared.
     * @return True if the objects are the same, false
	 *         otherwise.
     */
    public boolean equals(Object obj) {

		// These are equal only if the superclass says so
		// and they are both stationary fronts.
		return (super.equals(obj) && (obj instanceof StationaryFront));
    }


	// Protected Methods

	/**
	 * Translate the display coordinates to the specified
	 * point.
	 *
	 * @param point Point to which to translate.
	 */
	protected void translateDisplayCoordinates(Point point) {

		// Figure out the deltas between the last point
		// and this one.
		int xDelta = point.x - lastPoint.x;
		int yDelta = point.y - lastPoint.y;

		// Let the superclass do most of the work.
		super.translateDisplayCoordinates(point);

		// Translate the dashed shape.
		AffineTransform at = new AffineTransform();
		at.translate(xDelta, yDelta);
		dashedShape = at.createTransformedShape(dashedShape);
	}

	/**
	 * Create the visual elements of this glyph.
	 *
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 */
	protected void createVisuals(IGraphicsTarget gc, CoordConverter transformer) {

		// Let the superclass do its thing first.
		super.createVisuals(gc, transformer);
		
		// If creation is not occurring, Create the drawing
		// shape for the dashed red part of this path.  This
		// is overlayed on top of the solid blue path to
		// create a blue-and-red dashed effect.
		if (editType != FINISH_CREATION) {
			float dashLength = (float) ((arcAdornment.getIntervalWithin() +
										 arcAdornment.
										 getActualIntervalBetween(gc,
																  transformer)) / 2.0);
			float[] dashArray = { dashLength, dashLength };
			BasicStroke stroke = new BasicStroke(thickness, BasicStroke.CAP_BUTT,
												 BasicStroke.JOIN_ROUND, (float) 10.0,
												 dashArray, dashLength);
			dashedShape = stroke.createStrokedShape(pathShape);
		} else
			dashedShape = null;
	}

	/**
	 * Create the adornments.
	 */
	protected void createAdornments() {
		super.createAdornments();
//		addAdornment(new TriangleEdgeAdornment(this, Color.cyan, rightSide,
		addAdornment(new TriangleEdgeAdornment(this, Color.blue, rightSide,
											   (intervalBetween * 2) + intervalWithin,
											   intervalWithin,
											   (intervalWithin + intervalBetween)
											   / -2.0, !aloft));
		arcAdornment = new ArcEdgeAdornment(this, Color.red, !rightSide,
											(intervalBetween * 2) + intervalWithin,
											intervalWithin,
											(intervalWithin + intervalBetween) / 2.0,
											!aloft);
		addAdornment(arcAdornment);
	}
}
