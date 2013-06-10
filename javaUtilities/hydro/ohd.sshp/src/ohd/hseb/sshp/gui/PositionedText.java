package ohd.hseb.sshp.gui;

import java.awt.Point;

public class PositionedText
{
    private StringBuffer _stringBuffer = null;
    private Point _position;
    
    public PositionedText()
    {
        _stringBuffer = new StringBuffer();
        _position = new Point(0, 0);
    }

    public void clear()
    {
        _stringBuffer.delete(0, _stringBuffer.length());    
    }
    
    public void setStringBuffer(StringBuffer stringBuffer)
    {
        _stringBuffer = stringBuffer;
    }

    public StringBuffer getStringBuffer()
    {
        return _stringBuffer;
    }

    public void setPosition(Point position)
    {
        _position = position;
    }

    public Point getPosition()
    {
        return _position;
    }
    
   
}
