/*
 * Created on Oct 10, 2003
 *
 *
 */
package ohd.hseb.util.gui.drawing;

import java.util.*;
import java.awt.Graphics;

/**
 * @author Chip Gobs
 *
 * This class provides a BufferedCanvas and accepts
 * multiple layers of painters, each of which knows how
 * to draw on it.  Each painter has access to this object's
 * Viewport in order to make scaling decisions.
 */
public class PaintableCanvas extends BufferedCanvas
{

	private List _canvasPainterList = new ArrayList();

// ------------------------------------------------------------------

    public PaintableCanvas(int x, int y, int width, int height)
	{
		super(x, y, width, height);
	
		return;	
	}

// ------------------------------------------------------------------

	public void draw(Graphics g)
	{
		//System.out.println("PaintableCanvas.draw()");
		for (int i = 0 ; i < _canvasPainterList.size(); i++)
		{		
			CanvasPainter painter = (CanvasPainter) _canvasPainterList.get(i);	
			painter.paint(g);	
		}
	}
//	------------------------------------------------------------------
    public void addCanvasPainter(CanvasPainter painter)
    {
    	_canvasPainterList.add(painter);
    	return;
    }
//  ------------------------------------------------------------------

	public void addCanvasPainter(int index, CanvasPainter painter)
	{
    	_canvasPainterList.add(index, painter);
    	
    	return;
	}
//	------------------------------------------------------------------
	public void removeCanvasPainter(CanvasPainter painter)
	{
	    _canvasPainterList.remove(painter);
	    return;
	}
//	--------------------------------------------------------------------

	public CanvasPainter removeCanvasPainter(int index)
	{
		CanvasPainter painter = null;
		
		painter = (CanvasPainter)_canvasPainterList.remove(index);
		
		return painter;
	}
	

//	------------------------------------------------------------------
    protected int getCanvasPainterCount()
    {
    	return _canvasPainterList.size();	
    }
//	------------------------------------------------------------------
	protected CanvasPainter getCanvasPainterByIndex(int index)
	{
	    return (CanvasPainter) _canvasPainterList.get(index);	
	} 
 
    
//	------------------------------------------------------------------


}
