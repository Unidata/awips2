package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;


/**
 * Class to encapsulate concept of a 1/4 HRAP grid for HPE/HPN. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * August 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 */


public class QuarterHrapGrid
{
	
	public static int QTR_HRAP_X = 131 * 4;
	public static int QTR_HRAP_Y = 131 * 4;
	
	public static int GRID_BINS = QTR_HRAP_X  * QTR_HRAP_Y;
	public static int HEADER_ITEM_COUNT = 6;
	
	private int[] headerInfo = new int[HEADER_ITEM_COUNT];
	private float[][] valueArray = null;
	
	public QuarterHrapGrid()
	{
		valueArray = new float[QTR_HRAP_X][QTR_HRAP_Y];
	}
	
	
	public int getHeaderInfo(int index)
	{
		return headerInfo[index];
	}
	
	public void setHeaderInfo(int value, int index)
	{
		headerInfo[index] = value;
	}
	
	
	public float getValue(int xIndex, int yIndex)
	{
		return valueArray[xIndex][yIndex];
	}
	
	public void setValue(float value, int xIndex, int yIndex)
	{
		valueArray[xIndex][yIndex] = value;
	}
	
	public boolean containsAllZeroes()
	{
		boolean result = true;
		
		for (int i = 0; i < QTR_HRAP_X && (result == true); i++)
		{
			for (int j = 0; j < QTR_HRAP_Y; j++)
			{
				if (valueArray[i][j] != 0.0)
				{
					result = false;
					break;
				}
			}
		}
		
		return result;
		
	}
	
	public float getMaxValue()
	{
		float max = -10000.0f;
		
		for (int i = 0; i < QTR_HRAP_X ; i++)
		{
			for (int j = 0; j < QTR_HRAP_Y; j++)
			{
				if (valueArray[i][j] > max)
				{
					max = valueArray[i][j];
				}
			}
		}
		
		return max;	
	}
	
	
}
