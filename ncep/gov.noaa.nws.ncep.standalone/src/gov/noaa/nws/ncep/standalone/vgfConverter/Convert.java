package gov.noaa.nws.ncep.standalone.vgfConverter;

import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.standalone.vgfConverter.WrapperC.VgfXml;
import gov.noaa.nws.ncep.ui.pgen.file.Products;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import com.sun.jna.Native;

/**
 * Convert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/25/2009   203         Q. Zhou     Initial created
 * 9/09/2010   203         Q. Zhou     Added Main for cml. 
 * 1/06/2011   137         Q. Zhou     Added empty file checking
 * 1/25/2011   137         Q. Zhou     Add code to check the converted xml(if jaxb readable)
 * 11/2/2011   480         Q. Zhou     Added Activity and subActivity input fields to vgfConverter
 * 12/12/2011  548         Q. Zhou     Added -f -a options for contour table file and activities.
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class Convert {
    
	public int convertMap(String in, String out, String activity, String subActivity, String contTbl)  throws IOException {
		
		List<File> vgfFiles = new  ArrayList<File>();	
		int counter = 0; //file counter
		String outFile = null;
				
		if (in.endsWith("*") || in.endsWith("*.vgf")) {
			in = in.substring(0, in.lastIndexOf("/"));
		}
		
		File vgfDir = new File(in);				
		if (in.endsWith(".vgf") ) {  //one file
			vgfFiles.add(vgfDir) ;
		}
		else {									//directory
			File[] files;			
			FilenameFilter filter = new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.endsWith(".vgf");
				}
			};
			files = vgfDir.listFiles(filter);
			
			if (files == null || files.length == 0) {
				System.out.println("The vgf file does not exist.");
				return 0;
			}
			
			for (int x = 0 ; x < files.length ; x++)
				vgfFiles.add(files[x]);
		}
		
		//loop to files
		if (vgfFiles != null) {
			for (int i=0; i<vgfFiles.size(); i++) {
				String f = vgfFiles.get(i).getAbsolutePath();
				String s = vgfFiles.get(i).getName();
				String s1 = s.substring(0, s.lastIndexOf("."));
				s =  s1+ ".xml";
				 
				if (out.endsWith("//")) {
					outFile = out.substring(0, out.lastIndexOf("/")) + s;
				}
				else if (out.endsWith("/")) {
					outFile = out + s;
				}
				else {
					outFile = out + "/" + s;
				}
				
				// Convert vgf to xml
				VgfXml wrap = (VgfXml)Native.loadLibrary( "VgfXml",  VgfXml.class); 
				wrap.vgfToXml( f, outFile, activity, subActivity, contTbl);
				
				// Check for the converted xml
				Products convertedRight = Util.read(outFile);
				if (convertedRight != null) {
					counter++;
					System.out.println("The file " +f + " is converted to " + outFile );
				}
			}
		}
		
		if (counter>1)
	    	System.out.println(counter +" files are converted.  " + "The Conversion is finished.\n");
	    else if (counter==1)
	    	System.out.println(counter +" file is converted.  " + "The Conversion is finished.\n");
	    
		return counter;
	}
	
	public static void main(String[] args)  throws IOException{ 
		String fileName = "";
		String activity = "";
		String subActivity = "";
		
		if (args.length == 0 || args.length == 1) {
			System.out.println("Please specify the source and the destination.\n");
			return;
		}
		
		if (!new File(args[0]).exists()) {
			System.out.println("The Source directory or file does not exist.\n");
			return ;
		} 
		if (!new File(args[1]).exists()) {
			System.out.println("The Destination directory does not exist.\n");
			return ;
		} 
		
		for (int i=0; i<args.length; i++) {
			if (args[i].equalsIgnoreCase("-t") )
				if ((i+1) <args.length &&  args[i+1] != null && !args[i+1].equalsIgnoreCase("-a"))
					fileName = args[i+1];
			
			if (args[i].equalsIgnoreCase("-a") ) {
				if ((i+1) <args.length && args[i+1] != null && !args[i+1].equalsIgnoreCase("-t")) {
					activity = args[i+1];
					if ((i+2) <args.length && args[i+2] != null && !args[i+2].equalsIgnoreCase("-t") )
						subActivity = args[i+2];
				}
			}
		}
			
		new Convert().convertMap(args[0], args[1], activity, subActivity, fileName);

	}   

}