package gov.noaa.nws.ncep.viz.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * This class reads lockedColorMaps.tbl to get the list of color map file names 
 * that needs to be locked.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/2012      #621       S. Gurung   Initial creation.
 * 
 * </pre>
 * 
 * @author sgurung 
 * @version 1.0
 * 
 */
public class LockedColorMaps {
	
	private String fileName;
	
	private List<String> cmapList;
	
	public LockedColorMaps(String fname) throws FileNotFoundException, IOException {
	       fileName = fname;
	       readTable();
	}
	 
	public String getColorMap(int index) {
		return cmapList.get(index);
	}
	
	public List<String> getColorMapList(){
		return cmapList;
	}
  
    // throw an exception if there is a problem opening the file.
    public void readTable() throws FileNotFoundException, IOException {
        BufferedReader input = new BufferedReader(new FileReader(new File(fileName)));
        String lineStr = null;

        cmapList = new ArrayList<String>();
        int idx = 0;
        while( (lineStr=input.readLine()) != null ) {
        	if( lineStr.startsWith("!") ) {
        		continue;
        	}
        	cmapList.add(lineStr.trim());
        	
        	idx++;
        }
        input.close();
    }
    
    public boolean isLocked(String cmap) {
    	if (cmapList.contains(cmap) || cmapList.contains(cmap + ".cmap")) 
    		return true;
    	else
    		return false;
    }
}
