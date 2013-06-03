/*
 * Created on Nov 14, 2003
 *
 * 
 */
package ohd.hseb.util;

/**
 * @author Chip Gobs
 *
 * This class consolidates some common math functions.
 */
public class MathHelper
{
  /*  public static double roundToNDecimalPlaces(double origValue, int decimalPlaces)
    {    
           double factor = Math.pow(10, decimalPlaces);
           long intValue = (long) (origValue * factor);
           double newValue = intValue / factor;

           return newValue;
    } //end roundToNDecimalPlaces
  */  
    
    public static double roundToNDecimalPlaces(double numberToRound, int decimalPlacesToMaintain)
    {
    
        long roundingFactor = 1;
        long temporaryHolder;
        long roundingFactorArray[] = { 1, 10, 100, 1000, 10000, 100000,
                1000000, 10000000, 100000000, 1000000000 };

        if (decimalPlacesToMaintain < 0)
        {
            decimalPlacesToMaintain = 0;
        }
        else if (decimalPlacesToMaintain > 9)
        {
            decimalPlacesToMaintain = 9;
        }


        // I could have used pow(), but this is faster
        roundingFactor = roundingFactorArray[decimalPlacesToMaintain];

        temporaryHolder = (long) Math.floor((numberToRound * roundingFactor) + 0.5);

        numberToRound = (double) temporaryHolder / roundingFactor;

        return numberToRound;
        
    }
}
