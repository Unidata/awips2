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
package fsl.tools.glyph;

import java.awt.Color;
import java.awt.Point;
import java.awt.Rectangle;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.viz.adapter.CoordConverter;



/**
 * The glyph interface, implemented by objects that are to
 * be drawable onscreen, as well as allowing themselves to
 * be modified in one ore more ways.
 *
 * @author Christopher Golden
 */
public interface Glyph {


	// Public Static Constants

	/**
	 * Value specifying no edit type.
	 */
	public static final int NONE = 0;

	/**
	 * Value specifying the move edit type.  If an edit
	 * is started of this type, the glyph may choose to
	 * interpret it differently depending upon where
	 * the edit started.  For example, a glyph made of
	 * a line between two points may choose to have a
	 * move started at either end drag just that end,
	 * not the entire glyph.
	 */
	public static final int MOVE = 1;

	/**
	 * Value specifying the "move only" edit type.
	 * Unlike the <code>MOVE</code> edit type, glyphs
	 * should never do anything when starting such an
	 * edit besides a straight move.
	 */
	public static final int MOVE_ONLY = 2;

	/**
	 * Value specifying the "finish creation" edit type.
	 */
	public static final int FINISH_CREATION = 3;

	/**
	 * Value specifying the "change nodes" edit type.
	 */
	public static final int CHANGE_NODES = 4;

	/**
	 * Value specifying the "move label" edit type.
	 */
	public static final int MOVE_LABEL = 5;

	/**
	 * Value specifying the "resize" edit type.
	 */
	public static final int RESIZE = 6;

	/**
	 * Indication that the glyph should be present in
	 * all frames of a drawing.
	 */
	public static final int ALL_FRAMES = -10000;


	// Public Methods

	/**
	 * Create a clone of this glyph.
	 *
	 * @return The new clone of this glyph.
	 */
	public Object clone();

	/**
	 * Draw the glyph.
	 *
	 * @param gc          Graphics context in which the
	 *                    glyph is to be drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long pairs
	 *                    and display coordinates.
	 * @param properties  Display properties to be used.
	 * @param useColor    Color to be used when painting the
	 *                    glyph instead of whatever color(s)
	 *                    would usually be used; if <code>
	 *                    null</code>, standard painting
	 *                    will be done.
	 */
//	public void paint(IGraphicsTarget target, CoordConverter transformer,
//					   Color useColor);

//	/**
//	 * Convert this object to DGM format and place the result
//	 * in the supplied byte array.
//	 *
//	 * @param dgm         DGM array in which to place the
//	 *                    translated object.
//	 * @param gc          Graphics context in which this
//	 *                    glyph is drawn.
//	 * @param transformer Coordinate converter to be used
//	 *                    to convert from the display
//	 *                    coordinates to latitude-longitude
//	 *                    pairs.
//	 * @param cartesian   Flag indicating whether or not
//	 *                    the conversion should utilize
//	 *                    Cartesian instead of lat-long
//	 *                    coordinates. A 1024 by 1024
//	 *                    coordinate space is assumed if
//	 *                    this flag is true.
//	 */
//	public void toDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
//					  boolean cartesian);
//
//	/**
//	 * Convert this object to Extended DGM format and place
//	 * the result in the supplied byte array.
//	 *
//	 * @param dgm         Extended DGM array in which to
//	 *                    place the translated object.
//	 * @param gc          Graphics context in which this
//	 *                    glyph is drawn.
//	 * @param transformer Coordinate converter to be used
//	 *                    to convert from the display
//	 *                    coordinates to latitude-longitude
//	 *                    pairs.
//	 * @param cartesian   Flag indicating whether or not
//	 *                    the conversion should utilize
//	 *                    Cartesian instead of lat-long
//	 *                    coordinates. A 1024 by 1024
//	 *                    coordinate space is assumed if
//	 *                    this flag is true.
//	 */
//	public void toExtendedDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
//							  boolean cartesian);

	/**
	 * Flush display coordinates, since they are no longer
	 * valid.  This method is called when something about
	 * the glyph (position, etc.) or the display context
	 * in which the glyph is painted has changed.
	 */
	public void flushDisplayCoordinates();

	/**
	 * Indicate whether or not the glyph contains the
	 * specified point.
	 *
	 * @param point       Point to check to see if the
	 *                    glyph contains it.
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 * @return True if the glyph contains the point,
	 *         otherwise false.
	 */
	public boolean contains(Point point, IGraphicsTarget gc, CoordConverter transformer);

	/**
	 * Indicate whether or not the glyph intersects with
	 * the specified rectangle.
	 *
	 * @param rectangle   Rectangle to check to see if
	 *                    the glyph intersects with it.
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 * @return True if the glyph intersects with the
	 *         rectangle, otherwise false.
	 */
	public boolean intersects(Rectangle rectangle, IGraphicsTarget gc,
							  CoordConverter transformer);

	/**
	 * Get the bounding rectangle of the glyph.
	 *
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 * @return Bounding rectangle of the glyph.
	 */
	public Rectangle getBoundingRect(IGraphicsTarget gc, CoordConverter transformer);

	/**
	 * Begin editing the glyph.
	 *
	 * @param  type        Type of edit to be performed.
	 * @param  event       Event that triggered this edit.
	 * @param  gc          Graphics context in which this
	 *                     glyph is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long
	 *                     pairs and display coordinates.
	 * @return True if an edit operation has been started,
	 *         false otherwise.  Editing may not be begun
	 *         if the glyph does not support the type of
	 *         edit specified, or if the point at which
	 *         editing is to begin does not make sense in
	 *         the context of this type of edit.
	 */
	public boolean startEdit(int type, Event event, IGraphicsTarget gc,
							 CoordConverter transformer);

	/**
	 * Edit the glyph based upon the specified event.
	 *
	 * @param  event       Input event that triggered
	 *                     this call.
	 * @param  gc          Graphics context in which this
	 *                     glyph is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long
	 *                     pairs and display coordinates.
	 * @return True if the edit should continue, false
	 *         if the glyph has ended the edit.
	 */
	public boolean edit(Event event, IGraphicsTarget gc, CoordConverter transformer);

	/**
	 * Finish up editing the glyph.
	 *
	 * @param gc          Graphics context in which this glyph
	 *                    is drawn.
	 * @param transformer Coordinate transformer to be used to
	 *                    convert from the display coordinates
	 *                    to latitude-longitude format.
	 */
	public void finishEdit(IGraphicsTarget gc, CoordConverter transformer);

	/**
	 * Get the edit operation being performed.
	 *
	 * @return Value indicating which edit operation is
	 *         being performed; may be <code>MOVE</code> or
	 *         another value specific to a derived class.
	 */
	public int getEdit();

	/**
	 * Get the color of the glyph.
	 *
	 * @return Color of the glyph.
	 */
	public Color getColor();

	/**
	 * Get the frame of the glyph.
	 *
	 * @return Frame of the glyph, or <code>ALL_FRAMES</code>
	 *         if the glyph exists in all frames.
	 */
	public int getFrame();

	/**
	 * Set the color to the specified color.
	 *
	 * @param color New color of the glyph.
	 */
	public void setColor(Color color);

	/**
	 * Set the frame of the glyph.
	 *
	 * @param frame New frame of the glyph; if this is
	 *              specified as <code>ALL_FRAMES</code>,
	 *              the glyph will exist in all frames.
	 */
	public void setFrame(int frame);
}
