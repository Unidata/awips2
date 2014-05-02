package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;


/**
 * Class to handle numeric type conversions for MPE- and HPE/HPN- related grid processing. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * July 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 */

public class HydroNumericUtility {
	
	
	public static float convertShortsToFloat(short part1, short part2)
	{		
		int intPart1 = makeUnsignedShortAnInt(part1);
		int intPart2 = makeUnsignedShortAnInt(part2);
		
		float value = Float.intBitsToFloat((intPart1 << 16) + intPart2);
		
		return value;
	}
	
	public static int makeUnsignedShortAnInt(short value)
	{
				
		int intValue = value;
		
		if (value < 0)
		{
			intValue = (value & 0x0000ffff);
			
		}
		
		return intValue;
	}
	
	public static int getSwappedIntBytesFromFloat(float value)
	{
		int intBits = Float.floatToIntBits(value);
		int littleEndianInt = swapBytes(intBits);
		
		return littleEndianInt;
		
	}
	
	public static int swapBytes(int intBits)
	{
		int flippedInt = 
			(( intBits & 0x000000ff ) << 24 )  |
			(( intBits & 0x0000ff00 ) << 8 )   |
			(( intBits & 0x00ff0000 ) >>> 8)   |
			(( intBits & 0xff000000 ) >>> 24 );
		
		return flippedInt;
	}

	public static byte getHighOrderByte(short shortValue)
	{
		byte byteValue = (byte) (shortValue >> 8);
		
		return byteValue;
	}
	
	public static byte getLowOrderByte(short shortValue)
	{
		byte byteValue = (byte) (shortValue & 0x0000ffff);
		
		return byteValue;
	}
	
	
}
