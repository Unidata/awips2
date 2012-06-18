package gov.noaa.nws.ncep.standalone.xmlConverter;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * ConvertDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/25/2009   137         Q. Zhou     Initial created
 * 9/09/2010   137         Q. Zhou     Added Main for cml.
 * 1/06/2011   137         Q. Zhou     Added empty file checking
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class XmlLoading {

	public int loading(String in, String out) throws IOException {
		String outFile = null;		
		List<File> vgfFiles = new  ArrayList<File>();	
		int counter = 0; //file counter
		
		File vgfDir = new File(in);		
		
		if (in.endsWith("*") || in.endsWith("*.xml")) {
			in = in.substring(0, in.lastIndexOf("/"));
		}		
		if (in.endsWith(".xml") ) {  //one file
			vgfFiles.add(vgfDir) ;
		}
		else {									//directory
			File[] files;			
			FilenameFilter filter = new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.endsWith(".xml");
				}
			};
			
			files = vgfDir.listFiles(filter);
			if (files == null || files.length == 0) {
				System.out.println("The xml file does not exist.");
				return 0;
			}
				
			for (int x = 0 ; x < files.length ; x++)
				vgfFiles.add(files[x]);
			
		}
		
		//loop to files
		if (vgfFiles != null) {
			for (int i=0; i<vgfFiles.size(); i++) {
				String fin = vgfFiles.get(i).getAbsolutePath();
				String s = vgfFiles.get(i).getName();
				String s1 = s.substring(0, s.lastIndexOf("."));
				String sout =  s1+ ".tag";
				 
				if (out.endsWith("//")) {
					outFile = out.substring(0, out.lastIndexOf("/")) + sout;
				}
				else if (out.endsWith("/")) {
					outFile = out + sout;
				}
				else {
					outFile = out + "/" + sout;
				}
				
				//do convert
				int converted = new XmlConvert().convertXml(fin, outFile); // "http://lnx227:9581/services");
				if (converted != 0) {
					System.out.println("The file " +s +" is converted to " + sout);
					counter++;
				}
				
			}
		}
		
		if (counter >1)
	    	System.out.println(counter +" files are converted.  " + "The Conversion is finished.");
	    else if (counter <=1)
	    	System.out.println(counter +" file is converted.  " + "The Conversion is finished.");
	    
		return counter;
	}
	
	public static void main(String[] args) throws IOException { 
		if (!new File(args[0]).exists()) {
			System.out.println("The Source directory or file does not exist.\n");
			return ;
		} 
		if (!new File(args[1]).exists()) {
			System.out.println("The Destination directory does not exist.\n");
			return ;
		}
		
		new XmlLoading().loading(args[0], args[1]);
		//new XmlLoading().loading("/usr1/qzhou/to11d6/works/data1/outlook2.xml", "/usr1/qzhou/to11d6/works/data1"); //, "http://localhost:9581/services");
	}
}
