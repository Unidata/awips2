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
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;



/**
 * The abstract base class for drawable objects that have
 * a lat-long location and a pixel location that corresponds
 * to the former.
 *
 * @author Christopher Golden
 */
public abstract class GlyphImpl implements Glyph {


	// Protected Variables

	/**
	 * Flag indicating what type of editing is currently
	 * being performed, if any.
	 */
	protected int editType = NONE;

	/**
	 * Frame in which glyph exists; if this value is <code>
	 * ALL_FRAMES</code>, the glyph exists in all frames.
	 */
	protected int frame;

	/**
	 * Color of glyph.
	 */
	protected Color color;


	// Public Constructors

	/**
	 * Create an instance with a color of black that exists
	 * in all frames.
	 */
	public GlyphImpl() {
		color = Color.black;
		frame = ALL_FRAMES;
	}

	/**
	 * Create an instance with the specified color and
	 * frame presence.
	 *
	 * @param frame Frame in which this glyph exists; if
	 *              <code>ALL_FRAMES</code>, the glyph
	 *              exists in all frames.
	 * @param color Color of the glyph.
	 */
	public GlyphImpl(int frame, Color color) {
		this.frame = frame;
		this.color = color;
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

		// These are equal Only if the colors and frames
		// are the same.
		return ((obj instanceof GlyphImpl) &&
				color.equals(((GlyphImpl) obj).color) &&
				(frame == ((GlyphImpl) obj).frame));
    }

    /**
     * Get the hash code of this object.
	 *
     * @return Hash code of this object.
     */
	public int hashCode() {

		// Use the hash code of the color and frame.
		return (int) ((((long) color.hashCode()) + ((long) frame)) %
					  (long) Integer.MAX_VALUE);
	}

	/**
	 * Create a clone of this glyph.
	 *
	 * @return The new clone of this glyph.
	 */
	public Object clone() {
		GlyphImpl glyph = null;
		try {
			glyph = (GlyphImpl) super.clone();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return glyph;
	}

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
//	public abstract void paint(IGraphicsTarget target, CoordConverter transformer,
//							    Color useColor);

	public abstract void prepareShape(IWireframeShape ws, IShadedShape ss, CoordConverter transformer);
	
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
//	public abstract void toDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
//							   boolean cartesian);
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
//							  boolean cartesian) {
//		dgm.setColorEx(color);
//		toDGM(dgm, gc, transformer, cartesian);
//	}
//
//	/**
//	 * Create a copy of this item and apply the specified
//	 * modifications to it.  This method implementation does
//	 * nothing and is declared <code>final</code>, since glyphs
//	 * cannot be modified on the server side, only replaced.
//	 *
//	 * @param  item    The item holding the new state that this
//	 *                 item should contain.  Ignored.
//	 * @param  changes Integer specifying what changes should
//	 *                 be made to this item.  Ignored.
//	 * @return This method always returns <code>null</code>.
//	 */
//	public final BroadcastItem createModifiedCopy(BroadcastItem item, int changes) {
//		return null;
//	}
//
//	/**
//	 * Create a "shadow" copy of this item.  This method does
//	 * nothing since this object cannot be shadow copied.
//	 *
//	 * @return This method always returns <code>null</code>.
//	 */
//	public final BroadcastItem createShadowCopy() {
//		return null;
//	}
//
//	/**
//	 * Return an array containing the component items making
//	 * up this item.  This method does nothing since this
//	 * object cannot be broken down into components.
//	 *
//	 * @return This method always returns <code>null</code>.
//	 */
//	public final Object[] breakDownIntoComponents() {
//		return null;
//	}
//
//	/**
//	 * Flush display coordinates, since they are no longer
//	 * valid.  This method is called when something about
//	 * the glyph (position, etc.) or the display context
//	 * in which the glyph is painted has changed.
//	 */
//	public abstract void flushDisplayCoordinates();

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
	public abstract boolean contains(Point point, IGraphicsTarget gc,
									 CoordConverter transformer);

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
	public abstract boolean intersects(Rectangle rectangle, IGraphicsTarget gc,
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
	public abstract Rectangle getBoundingRect(IGraphicsTarget gc, CoordConverter transformer);

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
	 *         false otherwise.
	 */
	public abstract boolean startEdit(int type, Event event, IGraphicsTarget gc,
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
	public abstract boolean edit(Event event, IGraphicsTarget gc,
								 CoordConverter transformer);

	/**
	 * Finish up editing the glyph.
	 *
	 * @param gc          Graphics context in which this glyph
	 *                    is drawn.
	 * @param transformer Coordinate transformer to be used to
	 *                    convert from the display coordinates
	 *                    to latitude-longitude format.
	 */
	public abstract void finishEdit(IGraphicsTarget gc, CoordConverter transformer);

	/**
	 * Get the edit operation being performed.
	 *
	 * @return Value indicating which edit operation is
	 *         being performed; may be <code>MOVE</code> or
	 *         another value specific to a subclass.
	 */
	public int getEdit() {
		return editType;
	}

	/**
	 * Get the color of the glyph.
	 *
	 * @return Color of the glyph.
	 */
	public Color getColor() {
		return color;
	}

	/**
	 * Get the frame of the glyph.
	 *
	 * @return Frame of the glyph, or <code>ALL_FRAMES</code>
	 *         if the glyph exists in all frames.
	 */
	public int getFrame() {
		return frame;
	}

	/**
	 * Set the color to the specified color.
	 *
	 * @param color New color of the glyph.
	 */
	public void setColor(Color color) {
		this.color = color;
	}

	/**
	 * Set the frame of the glyph.
	 *
	 * @param frame New frame of the glyph; if this is
	 *              specified as <code>ALL_FRAMES</code>,
	 *              the glyph will exist in all frames.
	 */
	public void setFrame(int frame) {
		this.frame = frame;
	}


	// Protected Methods

	/**
	 * Calculate the pixel location of the glyph on the
	 * display based on its location.
	 *
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 */
	protected abstract void mapToDisplayCoordinates(IGraphicsTarget gc,
													CoordConverter transformer);

	/**
	 * Calculate the lat-long location of the glyph based
	 * on its pixel location on the display.
	 *
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between display
	 *                    coordinates and lat-long pairs.
	 */
	protected abstract void mapToLatLongCoordinates(IGraphicsTarget gc,
													CoordConverter transformer);

	/**
	 * Translate the display coordinates to the specified
	 * point.
	 *
	 * @param point Point to which to translate.
	 */
	protected abstract void translateDisplayCoordinates(Point point);
}
