package ohd.hseb.geomap.layer;

import java.awt.Color;
import java.awt.Graphics;

import ohd.hseb.geomap.projection.MapProjection;

public interface MapLayer
{    
    Color getColor();
    void setColor(Color color);
    
    void setShouldDraw(boolean shouldDraw);
    
    boolean shouldDraw();
    
    void draw(Graphics graphics, MapProjection projection);
}
