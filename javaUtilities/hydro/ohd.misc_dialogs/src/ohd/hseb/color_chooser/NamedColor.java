package ohd.hseb.color_chooser;

import java.awt.Color;

import ohd.hseb.util.StringParser;

public class NamedColor extends Color 
{
    private String _name = null;
    
    public NamedColor( String name, int redValue, int greenValue, int blueValue )
    {
        super( redValue, greenValue, blueValue );
        String[] tokenizedString = StringParser.tokenize( name );
        StringBuffer nameBuffer = new StringBuffer();
        for ( int i = 0; i < tokenizedString.length; i++ )
        {
            nameBuffer.append( tokenizedString[ i ] );
        }
        setName( nameBuffer.toString().toUpperCase() );
    }
    
    public NamedColor( String name, Color color )
    {
        this( name, color.getRed(), color.getGreen(), color.getBlue() );
    }

    public NamedColor( NamedColor namedColor )
    {
        this( namedColor.getName(), namedColor );
    }
    
    public String toString()
    {
        return _name + " " + super.toString();
    }
    
    public void setName( String name ) 
    {
        _name = name;
    }

    public String getName() 
    {
        return _name;
    }

}
