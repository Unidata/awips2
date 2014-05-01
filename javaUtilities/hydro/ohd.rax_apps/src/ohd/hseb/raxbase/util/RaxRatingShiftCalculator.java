package ohd.hseb.raxbase.util;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.raxbase.model.RaxRating;
import ohd.hseb.raxbase.model.RaxRatingPoint;
import ohd.hseb.raxbase.model.RaxRatingShift;

public class RaxRatingShiftCalculator
{
    private List _unShiftedRaxRatingPointList = null;
    private RaxRatingShift _raxRatingShift = null;
    private double[][] _ratingShift = new double[ 5 ][ 2 ];
    
    public RaxRatingShiftCalculator( List raxRatingPointList, RaxRatingShift raxRatingShift )
    {
        _unShiftedRaxRatingPointList = raxRatingPointList;
        _raxRatingShift = raxRatingShift;
        
        initRatingShift();
    }

    public RaxRatingShiftCalculator( RaxRating raxRating, RaxRatingShift raxRatingShift )
    {
        this( raxRating.getRaxRatingPointList(), raxRatingShift );
    }

    public RaxRatingShiftCalculator()
    {
    }

    public List getShiftedRaxRatingList()
    {
        double shift = 0;
        double shiftedStage = 0;
        double highShift = 0;
        double lowShift = 0;
        int i = 1;

        List shiftedRaxRatingList = new ArrayList();
        RaxRatingPoint shiftedRaxRatingPoint = new RaxRatingPoint();
        
        for ( int ratingPointIndex = 0; ratingPointIndex < _unShiftedRaxRatingPointList.size(); ratingPointIndex++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) _unShiftedRaxRatingPointList.get( ratingPointIndex );
            double unshiftedStage = raxRatingPoint.getStage();
            
            
            if ( unshiftedStage < ( _ratingShift[ 0 ][ 0 ] + _ratingShift[ 0 ][ 1 ] ) )
            {
                shift = _ratingShift[ 0 ][ 1 ];
            }
            else
            {
                for ( i = 1; i < 5; ++i )
                {
                    if ( unshiftedStage < ( _ratingShift[ i ][ 0 ] + _ratingShift[ i ][ 1 ] ) )
                    {
                        lowShift = _ratingShift[ i - 1 ][ 0 ] + _ratingShift[ i - 1 ][ 1 ];
                        highShift = _ratingShift[ i ][ 0 ] + _ratingShift[ i ][ 1 ];

                        double slope = ( highShift - lowShift ) / ( _ratingShift[ i ][ 1 ] - _ratingShift[ i - 1 ][ 1 ] );

                        double intercept = ( _ratingShift[ i ][ 0 ] + _ratingShift[ i ][ 1 ] ) -
                        ( _ratingShift[ i ][ 1 ] * slope );

                        shift = ( unshiftedStage - intercept ) / slope;
                        break;
                    }
                }
                
                if ( i == 5 )
                {
                    shift = _ratingShift[ 5-1 ][ 1 ];
                }
            }
            
            shiftedStage = unshiftedStage - shift;
            
            shiftedRaxRatingPoint.setStage( shiftedStage );
            shiftedRaxRatingPoint.setDischarge( raxRatingPoint.getDischarge() );
            
            shiftedRaxRatingList.add( shiftedRaxRatingPoint );
            shiftedRaxRatingPoint = new RaxRatingPoint();
//            System.out.println( "Stage: " + unshiftedStage + "\nDischarge: " + raxRatingPoint.getDischarge() + 
//                                "\nShift: " + shift + "\nShiftedStage: " + shiftedStage + "\n=======\n");
        }
        return shiftedRaxRatingList;
    }
    
    public void initRatingShift()
    {
        double zeroFlowStage = getZeroFlowStage();
        if ( _ratingShift[ 1 ][ 0 ] > zeroFlowStage )
        {
            _ratingShift[ 0 ][ 0 ] = zeroFlowStage;
        }
        
        _ratingShift[ 1 ][ 0 ] = _raxRatingShift.getValA();
        _ratingShift[ 1 ][ 1 ] = _raxRatingShift.getShiftA();
        _ratingShift[ 2 ][ 0 ] = _raxRatingShift.getValB();
        _ratingShift[ 2 ][ 1 ] = _raxRatingShift.getShiftB();
        _ratingShift[ 3 ][ 0 ] = _raxRatingShift.getValC();
        _ratingShift[ 3 ][ 1 ] = _raxRatingShift.getShiftC();
        _ratingShift[ 4 ][ 0 ] = _raxRatingShift.getValD();
        _ratingShift[ 4 ][ 1 ] = _raxRatingShift.getShiftD();
    }

    private double getZeroFlowStage()
    {
        double value = 0;
        
        for ( int i = 0; i < _unShiftedRaxRatingPointList.size(); i++ )
        {
            RaxRatingPoint ratingPoint = (RaxRatingPoint) _unShiftedRaxRatingPointList.get( i );
            
            if ( ratingPoint.getDischarge() == 0 )
            {
                value = ratingPoint.getStage();
            }
        }
        return value;
    }

    public void setUnShiftedRaxRatingPointList( List unShiftedRaxRatingPointList )
    {
        _unShiftedRaxRatingPointList = unShiftedRaxRatingPointList;
    }

    public List getUnShiftedRaxRatingPointList()
    {
        return _unShiftedRaxRatingPointList;
    }

    public void setRaxRatingShift( RaxRatingShift raxRatingShift )
    {
        _raxRatingShift = raxRatingShift;
    }

    public RaxRatingShift getRaxRatingShift()
    {
        return _raxRatingShift;
    }

}
