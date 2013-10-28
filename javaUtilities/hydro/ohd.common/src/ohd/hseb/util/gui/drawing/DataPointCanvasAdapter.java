/*
 * Created on Jul 19, 2004
 *
 * 
 */
package ohd.hseb.util.gui.drawing;

/**
 * @author Chip Gobs
 * This interface provides a facade for working with the DataPointCanvas and its
 * other associated classes, such as Painters.  It helps ensure that the implementation
 * classes are providing all of the needed functionality.
 */
public interface DataPointCanvasAdapter
{
    public DataPointCanvas initCanvas(Object initializationObject);
    public void refreshCanvas(Object dataObject);

}
