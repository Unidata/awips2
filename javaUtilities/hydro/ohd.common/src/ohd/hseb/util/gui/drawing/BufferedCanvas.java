/*
 * Created on Jul 28, 2003
 *
 * 
 */
package ohd.hseb.util.gui.drawing;

import java.awt.*;
import java.awt.image.*;
import javax.swing.*;
/**
 * @author Chip Gobs
 *
 *  This class acts as the abstract base class for the family of pre-defined
 *  custom "Canvas" classes.
 */
//public abstract class BufferedCanvas extends java.awt.Canvas
public abstract class  BufferedCanvas extends JPanel
{
	private BufferedImage _bufferedImage = null;
	private Graphics	_bufferedImageGraphics = null;

	private boolean _needsToRedraw = true;	

    //---------------------------------------------------------------------

	public BufferedCanvas()
	{
		
	}
	
	public BufferedCanvas(int x, int y, int width, int height)
	{
		Rectangle boundsRectangle = new Rectangle(x, y, width, height);
		this.setBounds(boundsRectangle);
	}

	//	---------------------------------------------------------------------

	public void setNeedsToRedraw(boolean needsToRedraw)
	{
		_needsToRedraw = needsToRedraw;
	}
	
	//	---------------------------------------------------------------------

	public void paint(Graphics g)
	{
		//System.out.println("BufferedCanvas.paint()");
		//make sure that the bufferedImage
		//is the same size as the Canvas
		
		if ( _bufferedImage == null)// don't have a _bufferedImage
		{
            
               // System.out.println("BufferedCanvas.paint() _bufferedImage is null");
				initBufferedImage();
                _needsToRedraw = true;
		}

		else if  (!sizeSame(_bufferedImage, this) )
		{
				//System.out.println("BufferedCanvas.paint() size not same");
                
                resizeByVisibleArea();
                
				initBufferedImage();
                _needsToRedraw = true;
		}
		
		
		// if the image needs to be redrawn to the imageBuffer,
		// do it
	    if (_needsToRedraw)
	    {
			_needsToRedraw = false;
			
			//System.out.println("BufferedCanvas.paint() size not same");
	    	
	    	draw(getBufferedImageGraphics());
	    }	
		
		copyBufferedImage(g);
	} //end paint()
//---------------------------------------------------------
    public void resizeByVisibleArea()
    {
         int width = this.getVisibleRect().width;
         int height = this.getVisibleRect().height;
         
         this.setSize(width, height);
    }
//  ---------------------------------------------------------

	public void update(Graphics g)
	{
		paint(g);
	}
//---------------------------------------------------------
	// This method must be overloaded by the subclass
	public abstract void draw(Graphics g);
	
	public void repaint()
	{
		//System.out.println("BufferedCanvas.repaint()");
		setNeedsToRedraw(true);
		super.repaint();	
	}
//---------------------------------------------------------
    private void copyBufferedImage(Graphics canvasGraphics)
	{	
	//	Toolkit toolkit = Toolkit.getDefaultToolkit();

		//System.out.println("BufferedCanvas.copyBufferedImage()");
	
		
		canvasGraphics.drawImage(_bufferedImage, 0, 0, this);

		return;
	} //copyBufferedImage
//-----------------------------------------------------------------
	private boolean sizeSame(BufferedImage bufferedImage,
							 BufferedCanvas canvas)
	{
		boolean result = false;
	    if  ((bufferedImage.getWidth() == canvas.getWidth()) &&
			 ( bufferedImage.getHeight() == canvas.getHeight()) 
			)
		{
			result = true;
		}

		return result;
	} //end sizeSame
//-----------------------------------------------------------------
    public BufferedImage getBufferedImage()
	{
		return _bufferedImage;
	}

//-----------------------------------------------------------------
  	protected Graphics getBufferedImageGraphics()
	{
		if (_bufferedImageGraphics == null)
		{
			if (_bufferedImage == null)
			{
				initBufferedImage();
			}
			_bufferedImageGraphics = _bufferedImage.createGraphics();
		}

		return _bufferedImageGraphics;
	}

//-----------------------------------------------------------------

private void initBufferedImage()
{
    
    // String header = "BufferedCanvas.initBufferedImage()";
    // Rectangle visibleRectangle = getVisibleRect();
     int width = this.getWidth();
     int height = this.getHeight();
    
     
    // System.out.println(header + "width = " + width + " height = " + height);
    
	_bufferedImage = new BufferedImage(width,
									   height, 
						               BufferedImage.TYPE_INT_RGB);
						
	_bufferedImageGraphics = _bufferedImage.createGraphics();
}
   
/*
	public boolean imageUpdate(Image img,
                           int infoflags,
                           int x,
                           int y,
                           int w,
                           int h)
    {
	    System.out.println("BufferedCanvas.imageUpdate(): infoflangs = " + infoflags );
	    boolean result = true;
	    if ( (infoflags & ImageObserver.ALLBITS) == ImageObserver.ALLBITS)
	    {
	  	    result = false;
	    }	

	    return result;
    } //imageUpdate()
*/

} //end BufferedCanvas
