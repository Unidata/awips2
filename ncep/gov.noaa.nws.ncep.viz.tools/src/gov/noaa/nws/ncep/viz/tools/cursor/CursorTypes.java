package gov.noaa.nws.ncep.viz.tools.cursor;


import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


/**
 * This class reads cursorType.tbl for NAWIPS customized cursors.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  06/2009    	109         M. Li    Initial creation.
 * 
 * </pre>
 * 
 * @author mli 
 * @version 1.0
 * 
 */
public class CursorTypes {
	private  String fileName;
	
	private List<CursorType> cursorList;
	
	public class CursorType {
		public CursorType(String name, String imgFile) {
			CursorName = name;
			imageFile = imgFile;
		}
		
		private String CursorName;
		private String imageFile;
		
		public String getCursorName() {
			return CursorName;
		}
		public void setCursorName(String cursorName) {
			CursorName = cursorName;
		}
		public String getImageFile() {
			return imageFile;
		}
		public void setImageFile(String imageFile) {
			this.imageFile = imageFile;
		}
	}
	
	public CursorTypes(String fname) {
		fileName = fname;
	}
	
	public List<CursorType> getCursorTypes() {
		return cursorList;
	}
	public boolean readTable() throws FileNotFoundException, IOException {
		BufferedReader input = new BufferedReader(new FileReader(new File(fileName)));
        String lineStr = null;
		
        cursorList = new ArrayList<CursorType>();
		
		while( (lineStr=input.readLine()) != null ) {
        	if( lineStr.startsWith("!") ) {
        		continue;
        	}

        	String[] lv = lineStr.trim().split("\\s+");
        	if (lv.length >= 2) {
        		String curnm = lv[0].trim();
        		String curImg = lv[1].trim();
        		
        		
        		CursorType entry = new CursorType(curnm, curImg);
        		cursorList.add(entry);
        	}
		}
		input.close();
		
		return (cursorList.size() > 0 ? true : false);
	}
	
}
