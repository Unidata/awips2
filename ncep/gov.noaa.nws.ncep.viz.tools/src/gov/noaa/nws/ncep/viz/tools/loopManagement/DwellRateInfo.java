package gov.noaa.nws.ncep.viz.tools.loopManagement;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * This class reads NMAP2's loop_speed.tbl file for Dwell Rate's default values.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  05/18/2009    108         M. Li    Initial creation.
 *  09/28/20100   317         X. Guo   Create Loop Management and copy it from
 *                                         dwellRate. Remove dwellRate * 
 * </pre>
 * 
 * @author mli 
 * @version 1.0
 * 
 */

public class DwellRateInfo {
	private final String fileName;
	
	private  List<DwellRateEntry> dwellRateList;

    public class DwellRateEntry {
    	    	
    	public DwellRateEntry(int idx, double first, double mid, double last) {
    		index = idx;
    		firstFrameDwell = first;
    		midFrameDwell = mid;
    		lastFrameDwell = last;
    	}
    	
    	private int index = 2;
    	private double firstFrameDwell = 0.10;
    	private double lastFrameDwell = 0.90;
    	private double midFrameDwell = 0.10;

		public int getIndex() {
			return index;
		}

		public void setIndex(int index) {
			this.index = index;
		}

		public double getFirstFrameDwell() {
			return firstFrameDwell;
		}

		public void setFirstFrameDwell(double firstFrameDwell) {
			this.firstFrameDwell = firstFrameDwell;
		}

		public double getLastFrameDwell() {
			return lastFrameDwell;
		}

		public void setLastFrameDwell(double lastFrameDwell) {
			this.lastFrameDwell = lastFrameDwell;
		}

		public double getMidFrameDwell() {
			return midFrameDwell;
		}

		public void setMidFrameDwell(double midFrameDwell) {
			this.midFrameDwell = midFrameDwell;
		}
    	
    }

    public DwellRateInfo(String fname) {
        fileName = fname;
    }

    public DwellRateEntry getDwellRate(int index) {
    	return dwellRateList.get(index);
    }
    
    public List<DwellRateEntry> getDwellRate() {
    	return dwellRateList;
    }

    // throw an exception if there is a problem opening the file.
    public boolean readTable() throws FileNotFoundException, IOException {
        BufferedReader input = new BufferedReader(new FileReader(new File(fileName)));
        String lineStr = null;

        dwellRateList = new ArrayList<DwellRateEntry>();
        int idx = 0;
        while( (lineStr=input.readLine()) != null ) {
        	if( lineStr.startsWith("!") ) {
        		continue;
        	}

        	String[] lv = lineStr.trim().split("\\s+");
        	if (lv.length >= 3) {
        		double firstDR = Double.valueOf(lv[0].trim());
        		double middleDR = Double.valueOf(lv[1].trim());
        		double lastDR = Double.valueOf(lv[2].trim());
        		
        		DwellRateEntry entry = new DwellRateEntry(idx, firstDR, middleDR, lastDR);
        		dwellRateList.add(entry);
        	}

        	idx++;
        }
        input.close();

        return (dwellRateList.size() > 0 ? true : false );
    }
}

