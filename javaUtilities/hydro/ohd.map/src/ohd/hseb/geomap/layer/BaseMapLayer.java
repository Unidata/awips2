package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.awt.Graphics;

import ohd.hseb.geomap.projection.MapProjection;

public abstract class BaseMapLayer implements MapLayer
{
    private Color _color = null;
    private boolean _shouldDraw = true;
     
    // ------------------------------------------------------------------------------------
    
    public void setShouldDraw(boolean shouldDraw)
    {
        _shouldDraw = shouldDraw;
    }
    
    //  ------------------------------------------------------------------------------------
    
    public boolean shouldDraw()
    {
        return _shouldDraw;
    }
    
    //  ------------------------------------------------------------------------------------
    
    public abstract void draw(Graphics g, MapProjection projection);

    //  ------------------------------------------------------------------------------------
    
    public void setColor(Color color)
    {
        _color = color;
    }
    
    //  ------------------------------------------------------------------------------------
    
    public Color getColor()
    {
        return _color;
    }
    //  ------------------------------------------------------------------------------------
}
