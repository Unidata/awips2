package ohd.hseb.color_chooser;

import java.awt.Color;

public class ColorHolder 
{
    private Color _color = null; 

    public ColorHolder( Color color )
    {
        setColor(color);
    }

    public void setColor(Color color) 
    {
        _color = color;
    }

    public Color getColor() 
    {
        return _color;
    }
    
    
}
