package gov.noaa.nws.ncep.viz.tools.cursor;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.localization.LocalizationFile;


/**
 * Store and handle attributes for National Centers Cursors.
 *  * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/22/09		  #109		M. Li		Created
 * 07/28/11       #450      G. Hull     NcPathManager, remove cursorType tbl 
 *                                      and determine cursors from available png files
 * 
 * </pre>
 * 
 * @author mli
 * @version 1
 */
public class NCCursors {
	public static enum CursorRef {
		DEFAULT,
		POINT_SELECT,
		BUSY
	}
	
	// from CursorTypes 
	public static class CursorType {
		public CursorType(String name, String imgFile) {
			CursorName = name;
			imageFile = imgFile;
		}
		
		private String CursorName;
		private String imageFile; // the full path
		
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

	// Pre-set cursor references from table
	private static List<CursorReference> curRefList = null;
    
	// Cursor type list from available png files 
    private static List<CursorType> cursorTypeList = null;
    
    // Cursor color options
    private final static String[] CurColorOptions = {
    		"red", "black", "green", "yellow",
    		"cyan", "magenta", "white"
    };
    
    // Pixel values for cursor colors
    private final static int[] cursorColors = {
    		0xFF, 		// red
    		0x000000,	// black	
    		0xFF00,		// green
    		0xFFFF00,	// yellow
    		0xFFFF,		// cyan
    		0xFF00FF,	// megenta
    		0xFFFFFF	// white
    };
    
    // Cursor type index corresponding to cursor references
	private static int[] curTypes = null;
    
	// Cursor color index corresponding to cursor references
    private static int[] curColors = null;
    
    
    protected List<CursorReference> getCursorRefList() {
    	if (curRefList == null) readCursorTable();
    	return curRefList;
    }
    
    protected List<CursorType> getCursorTypeList() {
    	if (cursorTypeList == null)  
    		readCursorTable();
    	return cursorTypeList;
    }
    
    protected String[] getCursorColorOptions() {
    	return CurColorOptions;
    }
    
    
    private static void readCursorTable() {
    	if (curRefList == null) {
    		CursorReferenceTableReader curRefTbl = 
    			new CursorReferenceTableReader(
    					NcPathManager.getInstance().getStaticFile(
    							NcPathConstants.CURSOR_REFS_TBL ).getAbsolutePath() );
    		try {
    			curRefList = curRefTbl.getTable();
    		} catch (JAXBException e) {
    			e.printStackTrace();
    		}
    	}
    	
    	cursorTypeList = new ArrayList<CursorType>();
    	
    	// Get a list of all the Localization .png files under the CURSORS directory
    	// (recursive to find files in the images sub-dir)
    	Map<String,LocalizationFile> availCursors = NcPathManager.getInstance().listFiles( 
        		NcPathConstants.CURSORS_DIR, new String[]{ ".png" }, true, true );

    	// Wanted to change this to read all of the available cursors to make it possible to 
    	// add cursors in the future but then realized that there was an ordering to the cursorTypeList
    	// based on the order from the old cursorTypes.tbl. So I am hardcoding the ordering 
    	// here
    	String[] expectedCursorTypes = {"SMALL_ARROW","LARGE_ARROW",
    									"SMALL_CROSS","LARGE_CROSS","SMALL_X","LARGE_X" };

    	for( String cursorType : expectedCursorTypes ) {
        	for( LocalizationFile lFile : availCursors.values() ) {
        		if( lFile.getName().contains( cursorType ) ) {
        			if( !lFile.getFile().exists() ) {
        				System.out.println( "Could not Find expected Cursor Type: "+ 
        						lFile.getName() );
        				return;
        			}
        			String imgFileName = lFile.getFile().getName();
        			String cursorName = imgFileName.substring(0, imgFileName.length()-".png".length());

        			cursorTypeList.add( 
        						new CursorType( cursorName, lFile.getFile().getAbsolutePath() ) );
        		}
        	}
    	}
    	
    	if( cursorTypeList.size() != expectedCursorTypes.length ) {
			System.out.println( "Could not Find All the Expected Cursor Types? ");
			cursorTypeList.clear(); // 
    	}    	
    }
    
    protected static int[] getCursorTypeIdx(boolean default_value) {
    	
    	if (default_value || curTypes == null) {
    		if (curRefList != null || cursorTypeList != null) {
    			readCursorTable();
    		}

    		if (curTypes == null) {
    			curTypes = new int[curRefList.size()];
    		}

    		for(CursorReference curRef : curRefList) {
    			int index = curRef.getReferenceIndex();

    			for (int i = 0; i < cursorTypeList.size(); i++) {
    				if ( curRef.getCursorName().equalsIgnoreCase(cursorTypeList.get(i).getCursorName()) ){
    					curTypes[index] = i;
    					break;
    				}
    			}

    		}
    	}
    	
    	return curTypes;
    }
    
    protected static int[] getCursorColorIdx(boolean default_value) {
    	if (default_value || curColors == null) {
    		if (curRefList != null) {
    			readCursorTable();
    		}

    		if (curColors == null) {
    			curColors = new int[curRefList.size()];
    		}

    		for(CursorReference curRef : curRefList) {
    			int index = curRef.getReferenceIndex();

    			for (int i = 0; i < CurColorOptions.length; i++) {
    				if (curRef.getCursorColor().equalsIgnoreCase(CurColorOptions[i])){
    					curColors[index] = i;
    					break;
    				}
    			}
    		}
    	}
    	
    	return curColors;
    }
    
    protected void setCursorTypeColorIdx(int[] types, int[] colors) {
    	curTypes = types;
    	curColors = colors;
    }
    
    private static void init() {
    	if (cursorTypeList == null) readCursorTable();
    	if (curTypes == null) {
    		curTypes = getCursorTypeIdx(true);
    	}
    	if (curColors == null) curColors = getCursorColorIdx(true);
    }
        
    public static Cursor getCursor(Display d, CursorRef curRef) {
    	init();
    	
    	// Get cursor image file name
    	int refIndex = curRef.ordinal();

    	String imageFile =     		
    			cursorTypeList.get(curTypes[refIndex]).getImageFile(); 
    	
    	// Get hotSpot X, & Y
    	int x = 1;
    	int y = 1;
    	switch (curTypes[refIndex]) {
    	case 2:
    		x = 9;
    		y = 8;
    		break;
    	case 3:
    		x = 16;
    		y = 15;
    		break;
    	case 4:
    		x = 9;
    		y = 8;
    		break;
    	case 5:
    		x = 16;
    		y = 15;
    		break;
    	}

    	// Change cursor color
    	ImageData image = new ImageData(imageFile); 
    	if (curColors[refIndex] > 0) {
    		for(int i = 0; i < image.width; i++) {
    			for(int j = 0; j < image.height; j++) {
    				if (image.getPixel(i,j) != 0)
    					image.setPixel(i, j, cursorColors[curColors[refIndex]]);
    			}
    		}
    	}
    	
    	Cursor cursor = new Cursor(d, image, x, y);
    	
    	return cursor;
    }

}
