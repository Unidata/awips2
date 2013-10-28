/*
 * Created on Jul 20, 2004
 *
 * 
 */
package ohd.hseb.util.gui;

import java.awt.Graphics;
import java.awt.Font;

/**
 * @author GobsC
 *
 *
 */
public class FontHelper
{
    Font _font = null;
     
    public FontHelper(Font font)
    {
        
        _font = font;
    }
        
    public int getWidth(Graphics g, String textString)
    {
        int totalWidth = g.getFontMetrics(_font).stringWidth(textString);
        	
        return totalWidth;
    }
    
    public int getHeight(Graphics g)
    {
        int height = g.getFontMetrics(_font).getHeight();
        	
        return height;
    }
    
}
