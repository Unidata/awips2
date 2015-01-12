package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;


/**
 * Reads Ascii Xmrg files and provides access to the data through 2D array. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * October 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */

public class AsciiXmrgReader
{
	//Units in the file are hundredths of inches  (Paul likes to call it inches* 100)
	//AWIPS 2 calls it Inches.divide(100)
	// Example 12.02 inches are stored in the file as 1202.
	
	
	
	private String filePath = null;
	private Rectangle hrapExtent;
	
	Hrap_Grid hrap_grid = null;
	
	private int[][] gridValueArray;	
	private int[] data; //single dimension array
	
	BufferedReader in = null;
	
	public AsciiXmrgReader(String filePath)
	{
		this.filePath = filePath;
	}
	
    public int read()
    {
   	    hrap_grid = DailyQcUtils.getHrap_grid();
            
        String line = null;
 
        int originX, originY, sizeX, sizeY;
        int iflag;
        int fe = 0;
        File tb = new File(filePath);
        
        int arraySize = hrap_grid.maxi * hrap_grid.maxj;

        setGridValueArray(new int[hrap_grid.maxi][hrap_grid.maxj]);

        setData(new int[arraySize]);
        
        // for (j = (hrap_grid.maxj - hrap_grid.hrap_miny) - 1; j >= 0; j--) {
        for (int i = 0; i < (hrap_grid.maxi); i++) {
            for (int j = 0; j < (hrap_grid.maxj); j++) {
            	gridValueArray[i][j] = 0;
            }
        }
        
        //init single dimension data array
        for (int i = 0; i < arraySize; i++)
        {
        	getData()[i] = 0;
        }

        if (tb.lastModified() == 0 || tb.length() == 0) {
            fe = 0;
        }
        if (fe == -1) {
            return -1;
        }

        try {

            in = new BufferedReader(new FileReader(filePath));
            if (in == null) {
                return -1;
            }
            // minhrapi = hrap_grid.hrap_minx;
            // minhrapj = hrap_grid.hrap_miny;
            // maxhrapi = hrap_grid.maxi;
            // maxhrapj = hrap_grid.maxj;

            line = in.readLine().trim();
            Scanner s = new Scanner(line);
            originX = (int) s.nextDouble();
            originY = (int) s.nextDouble();
            sizeX = (int) s.nextDouble();
            sizeY = (int) s.nextDouble();
            
           // add a check for the extent in hrap vs the file itself
            
            String header = "AsciiXmrgReader.read(): ";
            System.out.printf("%s %d %d %d %d \n",  header, originX, originY, sizeX, sizeY);
            
        //    hrapExtent = new Rectangle(xOrig, yOrig, maxX, maxY);
            
            setHrapExtent(new Rectangle(originX, originY, sizeX, sizeY));
            
            if (s.hasNextDouble())
            {
            	iflag = (int) s.nextDouble();
            }
            else
            {
            	iflag = 0;
            }

            int index = 0;
            
            System.out.println(header + " : ");
            
            for (int i = 0; i < sizeX; i++)
            {
            	if (iflag == 0) {
            		line = in.readLine().trim();
            	} else {
            		line = in.readLine().trim();
            	}

            	s = new Scanner(line);

            	for (int j = 0; j < sizeY; j++)
            	{

            		int dataValue = -1;
            		if (s.hasNextInt() == true)
            		{
            			dataValue = s.nextInt();
            		}

            		gridValueArray[i][j] = dataValue;
            		
            		int maxIndex = ((sizeY * sizeX) - 1);

            	    index = maxIndex - ( (j * sizeX) + ( sizeX -i  -1) );
            		
            		getData()[index] = dataValue;
            		
            	}

            }
            in.close();

            return 1;

        } catch (FileNotFoundException e) {
            System.out.println("File not found : " + filePath);
            return -1;
        } catch (IOException e) {
            e.printStackTrace();
            return -1;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
            	e.printStackTrace();
            }
        }
    }

	public int[][] getDataAsGrid() {
		return gridValueArray;
	}

	public void setGridValueArray(int gridValueArray[][]) {
		this.gridValueArray = gridValueArray;
	}

	public int[] getData() {
		return data;
	}

	private void setData(int[] data) {
		this.data = data;
	}

	public Rectangle getHrapExtent() {
		return hrapExtent;
	}

	private void setHrapExtent(Rectangle hrapExtent) {
		this.hrapExtent = hrapExtent;
	}
	
}
