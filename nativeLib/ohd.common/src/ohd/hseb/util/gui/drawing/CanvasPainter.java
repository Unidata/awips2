/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.util.gui.drawing;


import java.awt.*;

/**
 * @author GobsC
 *
 * This interface is used in conjunction with the
 * PaintableCanvas class.
 * 
 * Implementations of this interface will paint to the
 * canvas of the PaintableCanvas.
 */
public interface CanvasPainter
{
    public void paint(Graphics g);
}
