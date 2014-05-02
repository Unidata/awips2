package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;


/**
 * Class to handle some grid functionality for MPE- and HPE/HPN- related grid processing. 
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

public class GridUtility {
	

	private static final float MIN_VALUE = 0.0001f;
	
	// -------------------------------------------------------------------------------
	public GridUtility()
	{
		
	}

	// -------------------------------------------------------------------------------
	public void writeIntegerGrid(int[][] grid, int maxI, int maxJ, String pathName)
	{
		PrintWriter writer = null;
		try 
		{
			writer = new PrintWriter(new File(pathName));


			for (int i = 0; i < maxI; i++)
			{
				for (int j = 0; j < maxJ; j++)
				{
					int value = grid[i][j];
					writer.write(value + " ");
				} //end for j 
				writer.write("\n");
			}	//end for i

			writer.write("\n\n");

		} catch (Exception e) {

			e.printStackTrace();
		}	

		finally
		{
			if (writer != null)
			{
				try {
					writer.close();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

	} //end writeGrid()

	// -------------------------------------------------------------------------------
	public void writeFloatGrid(float[][] grid, int maxI, int maxJ, String pathName)
	{

		PrintWriter writer = null;

		try {

			writer = new PrintWriter(new File(pathName));


			for (int i = 0; i < maxI; i++)
			{
				for (int j = 0; j < maxJ; j++)
				{
					float value = grid[i][j];
					String valueString = String.format("%02.0f ", value);
					writer.write(valueString);
				} //end for j 
				writer.write("\n");
			}	//end for i

			writer.write("\n\n");

		} catch (Exception e) {

			e.printStackTrace();
		}	

		finally
		{
			if (writer != null)
			{
				try {
					writer.close();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

	} //end writeGrid()
	
	// -------------------------------------------------------------------------------
	public boolean compareGridFiles(String gridFileName1, String gridFileName2, String outputFileName)
	{
		QuarterHrapGrid grid1 = readGridFromFile(gridFileName1);
		QuarterHrapGrid grid2 = readGridFromFile(gridFileName2);
		
		QuarterHrapGrid differenceGrid = computeDifferenceGrid(grid1, grid2);
		
		float grid1Max = grid1.getMaxValue();
		float grid2Max = grid2.getMaxValue();
				
		System.out.printf("grid1 Max = %f  grid2 Max = %f\n", grid1Max, grid2Max);
		
		writeTextGridFile(differenceGrid, outputFileName);
		
		boolean showShapeOnly = false;
		
		writeTextGridFile(grid1, gridFileName1 + ".txt", showShapeOnly);
		writeTextGridFile(grid2, gridFileName2 + ".txt", showShapeOnly);
		
		
		showShapeOnly = true;
		
		writeTextGridFile(grid1, gridFileName1 + ".shape.txt", showShapeOnly);
		writeTextGridFile(grid2, gridFileName2 + ".shape.txt", showShapeOnly);
	
		
		boolean same = differenceGrid.containsAllZeroes();
		
		return same;
		
	}
	
	// -------------------------------------------------------------------------------
	public void writePolarGrid(float[][] polarGrid, int numRadials,
			int numRangeBins, String pathName) {
		
		PrintWriter writer = null;
		
		try {
		
			writer = new PrintWriter(new File(pathName));


			for (int r = 0; r < numRadials; r++)
			{
				for (int b = 0; b < numRangeBins; b++)
				{
					Float floatObject = polarGrid[r][b];
					writer.write(floatObject.toString() + " ");
				} //end for b (bins)
				writer.write("\n");
			}		
			writer.write("\n\n");

		} catch (Exception e) {

			e.printStackTrace();
		}	

		finally
		{
			if (writer != null)
			{
				try {
					writer.close();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
			
	} //end writePolarGrid()
	
	// -------------------------------------------------------------------------------
	public QuarterHrapGrid readGridFromFile(String gridFileName1)
	{		
		
		QuarterHrapGrid grid = new QuarterHrapGrid();
		
		try
		{
			FileInputStream fis = new FileInputStream(gridFileName1);
			DataInputStream dis = new DataInputStream(fis);
	
			readHeader(dis, grid);
			
			readGrid(dis, grid);	
		}
		catch (Exception e) {
			// TODO: handle exception
		}
		
		
		return grid;
		
	}
	
	// -------------------------------------------------------------------------------
	private void readHeader(DataInputStream dis, QuarterHrapGrid grid) throws IOException
	{
		
		//header items
		for (int h = 0; h < QuarterHrapGrid.HEADER_ITEM_COUNT; h++)
		{
			int value = dis.readInt();
			int swappedValue = Integer.reverseBytes(value);
			grid.setHeaderInfo(swappedValue, h);
		}
		
	}

	// -------------------------------------------------------------------------------
	private void readGrid(DataInputStream dis, QuarterHrapGrid grid) throws IOException
	{	
		for (int i = 0; i < QuarterHrapGrid.QTR_HRAP_X; i++)
		{
			for (int j = 0; j < QuarterHrapGrid.QTR_HRAP_Y; j++)
			{
				//float value = dis.readFloat();
				//grid.setValue(value, i, j);
				
				int value = dis.readInt();
				int swappedValue = Integer.reverseBytes(value);
				float newFloatValue = Float.intBitsToFloat(swappedValue);
				grid.setValue(newFloatValue, i, j);									
			}
		}
	}


	// -------------------------------------------------------------------------------
	public QuarterHrapGrid computeDifferenceGrid(QuarterHrapGrid grid1, QuarterHrapGrid grid2)
	{
		QuarterHrapGrid differenceGrid = new QuarterHrapGrid();
		
		//header items
		for (int h = 0; h < QuarterHrapGrid.HEADER_ITEM_COUNT; h++)
		{
			int diff = grid1.getHeaderInfo(h) - grid2.getHeaderInfo(h);
			differenceGrid.setHeaderInfo(diff, h);
		}
		
		
		//body with data
		for (int i = 0; i < QuarterHrapGrid.QTR_HRAP_X; i++)
		{
			for (int j = 0; j < QuarterHrapGrid.QTR_HRAP_Y; j++)
			{
				float diff = grid1.getValue(i,j) - grid2.getValue(i, j);
				
				if (Math.abs(diff) < MIN_VALUE)
				{
					diff = 0.0f;
				}
				
				if (Math.abs(diff) >= MIN_VALUE)
				{
					System.out.println(diff + " ");
				}
				
				differenceGrid.setValue(diff, i, j);
			}
		}
		
		return differenceGrid;
	}

	// -------------------------------------------------------------------------------
	
	public void writeGridFile(QuarterHrapGrid grid, String gridFileName)
	{
	    DataOutputStream dos = null;
		try
		{
			FileOutputStream fos = new FileOutputStream(gridFileName);
			dos = new DataOutputStream(fos);

			//header info
			for (int h = 0; h < QuarterHrapGrid.HEADER_ITEM_COUNT; h++)
			{
				int value = grid.getHeaderInfo(h);
				dos.writeInt(value);
			}


			// body
			for (int i = 0; i < QuarterHrapGrid.QTR_HRAP_X; i++)
			{
				for (int j = 0; j < QuarterHrapGrid.QTR_HRAP_Y; j++)
				{
					float value = grid.getValue(i, j);
					dos.writeFloat(value);
				}

			}

		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		finally{
		    if (dos != null)
		    {
		        try {
                    dos.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
		    }
		}
		
	} //end writeGridFile()
	
	// -------------------------------------------------------------------------------
	public void writeTextGridFile(QuarterHrapGrid grid, String gridFileName)
	{
		//show numbers
		writeTextGridFile(grid, gridFileName, false);
	}
	
	public void writeTextGridFile(QuarterHrapGrid grid, String gridFileName, boolean showShapeOnly)
	{
	    PrintStream printStream = null;
		try
		{
			FileOutputStream fos = new FileOutputStream(gridFileName);
			printStream = new PrintStream(fos);

			//header info
			for (int h = 0; h < QuarterHrapGrid.HEADER_ITEM_COUNT; h++)
			{
				int value = grid.getHeaderInfo(h);
				printStream.print(value + " ");
			}


			// body
			for (int i = 0; i < QuarterHrapGrid.QTR_HRAP_X; i++)
			{
				for (int j = 0; j < QuarterHrapGrid.QTR_HRAP_Y; j++)
				{
					float value = grid.getValue(i, j);
					
					if (showShapeOnly)
					{
						if (value > 0.0)
						{
							printStream.print("#");
						}
						else
						{
							printStream.print(" ");
						}
					}
					else //show numbers
					{
						printStream.printf("%03.1f ", value);
					}
				}
				printStream.println("");

			}

		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		finally{
		    if (printStream != null)
		    {
		        printStream.close();
		    }
		}
		
	} //end writeGridFile()
	
	// -------------------------------------------------------------------------------
	
	public void writeTextShapeFile(QuarterHrapGrid grid, String gridFileName)
	{
	    PrintStream printStream = null;
		try
		{
			FileOutputStream fos = new FileOutputStream(gridFileName);
			printStream = new PrintStream(fos);

			//header info
			for (int h = 0; h < QuarterHrapGrid.HEADER_ITEM_COUNT; h++)
			{
				int value = grid.getHeaderInfo(h);
				printStream.print(value + " ");
			}


			// body
			for (int i = 0; i < QuarterHrapGrid.QTR_HRAP_X; i++)
			{
				for (int j = 0; j < QuarterHrapGrid.QTR_HRAP_Y; j++)
				{
					float value = grid.getValue(i, j);
					if (value <= 0.0f)
					{
						printStream.print(" ");
					}
					else // value > 0.0
					{
						printStream.print("*");
					}
				} //for j

			} //for i

		}
		catch (Exception e)
		{
		    e.printStackTrace();
		}
		finally{
		    if (printStream != null)
		    {
		        printStream.close();
		    }
		}
		
	} //end writeGridFile()
	
	// -------------------------------------------------------------------------------
	
	public static void main(String[] argArray)
	{
		
		String filePath1 = "/home/pst2/DPRRLX080920131307Z.A1";
		//String filePath1 = "/home/pst2/DPRCCX080520131645Z.A1";
		
		String filePath2 = "/home/pst2/DPRRLX080920131308Z.A2";
		String outputFilePath = "/home/pst2/checker.out";
		GridUtility checker = new GridUtility();
		
		boolean areSame = checker.compareGridFiles(filePath1, filePath2, outputFilePath);
		
		if (areSame)
		{
			System.out.printf("Files :%s: and :%s: are essentially the same.\n", filePath1, filePath2); 
		}
		else
		{
			System.out.printf("Files :%s: and :%s: are different.\n", filePath1, filePath2);
		}
		
		System.out.printf("Difference file written to :%s:\n", outputFilePath);	
	}
	// -------------------------------------------------------------------------------
	
} // class GridChecker()
