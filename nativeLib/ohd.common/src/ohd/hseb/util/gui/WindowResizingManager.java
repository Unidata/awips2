/*
 * Created on Apr 28, 2004
 *
 * 
 */
package ohd.hseb.util.gui;

import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

/**
 * @author GobsC
 *
 * This class manages the permissable max and min sizes of a window 
 */
public class WindowResizingManager extends ComponentAdapter
{
    private Dimension _minDimension = null;
    private Dimension _maxDimension = null;
    
    private Window _window = null;
    
    public WindowResizingManager(Window managedComponent,
                                 Dimension minDimension,
                                 Dimension maxDimension)
    {
        
        _window = managedComponent;
        
        _minDimension = minDimension;
        _maxDimension = maxDimension;
        
             
        _window.addComponentListener(this);
        
    }
    
    public void componentResized( ComponentEvent e )
    {
        
        int height = _window.getHeight();
        int width = _window.getWidth();
        //System.out.println( "Width/Height = [" + width + ", " + height + "]" );
    
        int minWidth = _minDimension.width;
        int maxWidth = _maxDimension.width;
        int minHeight = _minDimension.height;
        int maxHeight = _maxDimension.height;
    
        if ( width < minWidth ) 
        { 
            _window.setSize( new Dimension( minWidth, height ) );
        }
        if ( width > maxWidth )
        {
            _window.setSize( new Dimension( maxWidth, height ) );
        }
        if ( height < minHeight )
        {
            _window.setSize( new Dimension( width, minHeight ) );
        }
        if ( height > maxHeight )
        {
            _window.setSize( new Dimension( width, maxHeight) );
        }
    }
     
}
