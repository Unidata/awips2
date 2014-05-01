package ohd.hseb.color_chooser;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

public class ColorSet 
{

    private List _namedColorList = new ArrayList();  // List of NamedColor objects
    
    public ColorSet()
    {
    }
    
    public ColorSet( ColorSet colorSet )
    {
        List namedColorList = colorSet.getNamedColorList();
        
        for ( int i = 0; i < namedColorList.size(); i++ )
        {
            NamedColor extractedNamedColor = (NamedColor) namedColorList.get( i );
            if ( extractedNamedColor != null )
            {
                _namedColorList.add( new NamedColor( extractedNamedColor ) );
            }
        }
    }
    
    public ColorSet( List namedColorList )
    {
        _namedColorList = namedColorList;
    }
    
    public void addNamedColor( Color colorObject )
    {
        _namedColorList.add( colorObject );
    }

    public void addNamedColorAtIndex( Color colorObject, int index )
    {
        _namedColorList.add( index, colorObject );
    }
    
    public void deleteNamedColorAtIndex( int index )
    {
        _namedColorList.remove( index );
    }

    public void replaceNamedColorAtIndex( Color colorObject, int index )
    {
        _namedColorList.remove( index );
        _namedColorList.add( index, colorObject );
    }

    public String toString()
    {
        StringBuffer returnString = new StringBuffer();
        for ( int i = 0; i < _namedColorList.size(); i++ )
        {
            NamedColor namedColor = (NamedColor) _namedColorList.get( i );
            returnString.append( namedColor + "\n" );
        }
        
        return returnString.toString();
    }
    
    public List getNamedColorList() 
    {
        return _namedColorList;
    }
}
