package ohd.hseb.color_chooser;

import java.util.ArrayList;
import java.util.List;

public class ScaleValueSet 
{
    private List _scaleValueSet = new ArrayList();

    public ScaleValueSet()
    {
    }
    
    public ScaleValueSet( List scaleValueSet )
    {
        setScaleValueSet( scaleValueSet );
    }
    
    public ScaleValueSet( ScaleValueSet scaleValueSet )
    {
        List scaleValueSetList = scaleValueSet.getScaleValueSet();
        
        for ( int i = 0; i < scaleValueSetList.size(); i++ )
        {
            _scaleValueSet.add( scaleValueSetList.get( i ) );
        }
    }
    
    public void addScaleValue( double value )
    {
        _scaleValueSet.add( value );
    }

    public void addScaleValueAtIndex( double value, int index )
    {
        _scaleValueSet.add( index, value );
    }
    
    public void replaceScaleValueAtIndex( double value, int index )
    {
        _scaleValueSet.remove( index );
        _scaleValueSet.add( index, value );
    }

    public void deleteScaleValueAtIndex( int index )
    {
        _scaleValueSet.remove( index );
    }
    
    public void setScaleValueSet( List scaleValueSet ) 
    {
        _scaleValueSet = scaleValueSet;
    }

    public List getScaleValueSet() 
    {
        return _scaleValueSet;
    }
}
