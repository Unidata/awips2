package ohd.hseb.raxbase.util;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.raxbase.model.RaxRatingPoint;
import ohd.hseb.raxbase.model.RaxRatingShift;

public class ShiftedStage
{
    private List unShiftedRaxRatingPointList = new ArrayList();
    private List shiftedRaxRatingPointList = new ArrayList();
    private RaxRatingShift raxRatingShift = new RaxRatingShift();
    private double shifts[][] = {  { 0, 0 },
                                  { 0.7, 5.00 },
                                  { 1.4, 2.50 },
                                  { 2.9, 4.00 },
                                  { 3.3, 7.00 } };

    private void initLists()
    {
        RaxRatingPoint raxRatingPoint = new RaxRatingPoint();
        
        raxRatingPoint.setStage( 0.5 );
        raxRatingPoint.setDischarge( 0 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        raxRatingPoint.setStage( 1 );
        raxRatingPoint.setDischarge( 225 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        raxRatingPoint.setStage( 2 );
        raxRatingPoint.setDischarge( 240 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        raxRatingPoint.setStage( 3 );
        raxRatingPoint.setDischarge( 270 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        raxRatingPoint.setStage( 4 );
        raxRatingPoint.setDischarge( 290 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        raxRatingPoint.setStage( 5 );
        raxRatingPoint.setDischarge( 330 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        raxRatingPoint.setStage( 6 );
        raxRatingPoint.setDischarge( 400 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        raxRatingPoint.setStage( 7 );
        raxRatingPoint.setDischarge( 450 );
        unShiftedRaxRatingPointList.add( raxRatingPoint );
        raxRatingPoint = new RaxRatingPoint();
        
        raxRatingShift.setValA( 0.7 );
        raxRatingShift.setValB( 1.4 );
        raxRatingShift.setValC( 2.9 );
        raxRatingShift.setValD( 3.3 );
        raxRatingShift.setShiftA( 5.0 );
        raxRatingShift.setShiftB( 2.5 );
        raxRatingShift.setShiftC( 4.0 );
        raxRatingShift.setShiftD( 7.0 );
        
        
    }

    private double getZeroFlowStage()
    {
        double value = -9999;
        
        for ( int i = 0; i < unShiftedRaxRatingPointList.size(); i++ )
        {
            RaxRatingPoint ratingPoint = (RaxRatingPoint) unShiftedRaxRatingPointList.get( i );
            
            if ( ratingPoint.getDischarge() == 0 )
            {
                value = ratingPoint.getStage();
            }
        }
        return value;
    }
    
    private void calculateShifts()
    {
        double shift = 0;
        double shiftedStage = 0;
        double highShift = 0;
        double lowShift = 0;
        int i = 1;
        
        double zeroFlowStage = getZeroFlowStage();
        if ( ( zeroFlowStage != -9999 ) &&
             ( shifts[ 1 ][ 0 ] > zeroFlowStage ) )
        {
            shifts[ 0 ][ 0 ] = zeroFlowStage;
        }
        
        System.out.println( shifts[ 0 ][ 0 ] );
        
        for ( int ratingPointIndex = 1; ratingPointIndex < unShiftedRaxRatingPointList.size(); ratingPointIndex++ )
        {
            RaxRatingPoint raxRatingPoint = (RaxRatingPoint) unShiftedRaxRatingPointList.get( ratingPointIndex );
            double unshiftedStage = raxRatingPoint.getStage();
            
            
            if ( unshiftedStage < ( shifts[ 0 ][ 0 ] + shifts[ 0 ][ 1 ] ) )
            {
                shift = shifts[ 0 ][ 1 ];
            }
            else
            {
                for ( i = 1; i < 5; ++i )
                {
                    if ( unshiftedStage < ( shifts[ i ][ 0 ] + shifts[ i ][ 1 ] ) )
                    {
                        lowShift = shifts[ i - 1 ][ 0 ] + shifts[ i - 1 ][ 1 ];
                        highShift = shifts[ i ][ 0 ] + shifts[ i ][ 1 ];

                        double slope = ( highShift - lowShift ) / ( shifts[ i ][ 1 ] - shifts[ i - 1 ][ 1 ] );

                        double intercept = ( shifts[ i ][ 0 ] + shifts[ i ][ 1 ] ) -
                        ( shifts[ i ][ 1 ] * slope );

                        shift = ( unshiftedStage - intercept ) / slope;
                        break;
                    }
                }
                
                if ( i == 5 )
                {
                    shift = shifts[ 5-1 ][ 1 ];
                }
            }
            
            shiftedStage = unshiftedStage - shift;
            System.out.println( "Stage: " + unshiftedStage + "\nDischarge: " + raxRatingPoint.getDischarge() + 
                                "\nShift: " + shift + "\nShiftedStage: " + shiftedStage + "\n=======\n");
        }
    }


    public ShiftedStage()
    {
        initLists();
        calculateShifts();
    }
    
    public static void main( String[] args )
    {
        ShiftedStage shiftedStage = new ShiftedStage();
    }
}
