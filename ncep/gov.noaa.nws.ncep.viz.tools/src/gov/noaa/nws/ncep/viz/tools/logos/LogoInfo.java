package gov.noaa.nws.ncep.viz.tools.logos;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * This class reads logos.tbl for logos display control
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  06/2/2009    105         M. Li    Initial creation.
 * 
 * </pre>
 * 
 * @author mli 
 * @version 1.0
 * 
 */
public class LogoInfo {
	private  String fileName;
	
	private List<LogoEntry> logoList;
	
	public class LogoEntry {
		public LogoEntry(String nm, String image, String loc, int sc) {
			LogoName = nm;
			imageFile = image;
			initialLocation = loc;
			initialScale = sc;
		}
		
		private String LogoName;
		private String imageFile;
		private String initialLocation = "OFF";
		private int initialScale = 100;
		private int imageHeigth;
		private int imageWidth;
		
		public String getLogoName() {
			return LogoName;
		}
		
		public void setLogoName(String logoName) {
			LogoName = logoName;
		}
		public String getImageFile() {
			return imageFile;
		}
		public void setImageFile(String imageFile) {
			this.imageFile = imageFile;
		}

		public int getImageHeigth() {
			return imageHeigth;
		}

		public void setImageHeigth(int imageHeigth) {
			this.imageHeigth = imageHeigth;
		}

		public int getImageWidth() {
			return imageWidth;
		}

		public void setImageWidth(int imageWidth) {
			this.imageWidth = imageWidth;
		}

		public String getInitialLocation() {
			return initialLocation;
		}

		public void setInitialLocation(String initialLocation) {
			this.initialLocation = initialLocation;
		}

		public int getInitialScale() {
			return initialScale;
		}

		public void setInitialScale(int initialScale) {
			this.initialScale = initialScale;
		}
		
		
	}
	
	public LogoInfo(String fname) {
		fileName = fname;
	}
	
	
	public LogoEntry getLogo(int index) {
		return logoList.get(index);
	}
	
	public List<LogoEntry> getLogoList(){
		return logoList;
	}
	
	public boolean readTable() throws FileNotFoundException, IOException {
		BufferedReader input = new BufferedReader(new FileReader(new File(fileName)));
        String lineStr = null;
		
		logoList = new ArrayList<LogoEntry>();
		
		while( (lineStr=input.readLine()) != null ) {
        	if( lineStr.startsWith("!") ) {
        		continue;
        	}

        	String[] lv = lineStr.trim().split("\\s+");
        	if (lv.length >= 2) {
        		String logonm = lv[0].trim();
        		String logoImg = lv[1].trim();
        		
        		String initLoc = "OFF";
        		int initScale = 100;
        		
        		if (lv.length >= 3) {
        			initLoc = lv[2].trim();
        		}
        		
        		if (lv.length >= 4) {
        			initScale = Integer.valueOf(lv[3].trim());
        		}
        		
        		LogoEntry entry = new LogoEntry(logonm, logoImg, initLoc, initScale);
        		logoList.add(entry);
        	}
		}
		input.close();
		
		return (logoList.size() > 0 ? true : false);
	}
}
