package gov.noaa.nws.ncep.standalone.colormapConverter;


import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.raytheon.uf.common.colormap.ColorMap;
//import com.raytheon.uf.common.serialization.SerializationException;
//import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * ColorMapConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/30/2009   197         Q. Zhou     Initial created
 * 08/12/2010               Q. Zhou     changed SerializationUtil JABX to direct JABX
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class ColormapConvert  {
	private Hashtable<String, Integer> pairs;
	
	/**
	 * The constructor.
	 */
	public ColormapConvert() {
		pairs = new Hashtable<String, Integer>();
	}

	/**
     * get the colormap types
     * 
     * @return an array of all the colormap names
     */
	public Hashtable<String, Integer> typeLookup(String lookup) {
		File enhanceFile = new File(lookup);
		
		int  lineCount=0;
		try {
			BufferedReader br = new BufferedReader(new FileReader(enhanceFile));
			String t;
			while ((t = br.readLine()) != null ) { 
				if (!(t.trim().startsWith("!"))  && t.trim() != "\n") {
					t = t.trim();
		    	    String[] result = t.split("\\s+");
		    	    if (result.length==2) {
		    	    	pairs.put(result[1], Integer.parseInt(result[0]));
		    	    }
					lineCount++;
				}
			} 
			
		}
		catch (IOException e) {
			   System.err.println("Error: " + e);
		}
		return pairs;
	}
	
	/**
     * convert the colormap from .tbl to .cmap
     * 
     * @return an array of all the colormap names
     */
	public int convertMap(String in, String out, String lookup) {
		
		String inFile = null;
		String outFile = null;
		int i = 0; //file counter

		pairs = typeLookup(lookup); //"$GEMTBL/luts/enhance.tbl"
		
		File tblDir = new File(in); 
		File[] tblFiles = null;   	
				
		FilenameFilter filter = new FilenameFilter() {
	        public boolean accept(File dir, String name) {
	            return name.endsWith(".tbl") && !name.endsWith("enhance.tbl");
	        }
		};
	    
	    tblFiles = tblDir.listFiles(filter);
	    if (tblFiles == null || tblFiles.length == 0) {
			System.out.println("The tbl file does not exist.");
			return 0;
		}
	    
	    //loop to files
	    if (tblFiles != null) {
	    	for ( i=0; i<tblFiles.length; i++) {
	    		int lineCount = 0;
	    		int alpha =0; //Sat or Radar

	    		inFile = tblFiles[i].getName();
	    		String s1 = inFile.substring(0, inFile.indexOf("."));
	    		outFile =  s1+ ".cmap";
	    		
	    		try {
	    			alpha = pairs.get(s1);
	    		} 
	    		catch (NullPointerException e) {}

	    		// Count data lines in the file		     
	    		try {
	    			BufferedReader br = new BufferedReader(new FileReader(tblFiles[i]));
	    			String tem;
	    			while ((tem = br.readLine()) != null ) { 
	    				if (!(tem.trim().startsWith("!"))  && tem.trim() != "\n") {
	    					lineCount++;
	    				}
	    			} 
	    		}
	    		catch (IOException e) {
	    			System.err.println("Error: " + e);
	    		}

	    		//create arrays
	    		float[] r = new float[lineCount];
	    		float[] g = new float[lineCount];
	    		float[] b = new float[lineCount];
	    		float[] a = new float[lineCount];
	    		
	    		// read the file		
	    		File file = tblFiles[i];
	    		readColor(file, alpha, r, g, b, a);

	    		// create a ColorMap object
	    		ColorMap colorMap = new ColorMap(outFile, r, g, b, a);
	    		//output
	    		outFile = writeOutput(out, outFile, alpha, colorMap);

	    	} //end of for
	    }

	    if (i>1)
	    	System.out.println(i +" files are converted.  " + "The Convertion is finished.");
	    else if (i==1)
	    	System.out.println(i +" file is converted.  " + "The Convertion is finished.");

	    return i;
	}

	private void readColor( File file, int alpha, float[] r,
			float[] g, float[] b, float[] a) {
		int lineCount = 0;
		try {
			BufferedReader br = new BufferedReader(new FileReader(file));
			String t;
			while ((t = br.readLine()) != null ) { 
				if ( !(t.trim().startsWith("!")) && t.trim() != "\n" ) {
					t = t.trim();
					String[] result = t.split("\\s+");
					List items = new LinkedList();

					if (result.length==3) { //rgb
						r[lineCount] = Float.parseFloat(result[0])/255;
						g[lineCount] = Float.parseFloat(result[1])/255;
						b[lineCount] = Float.parseFloat(result[2])/255; 			    		
						a[lineCount] = alpha;			    		
					}
					else if (result.length >3) {			    	    	
						for (int j=0; j<result.length; j++) {
							//boolean isInteger = Pattern.matches("^\d*$", myString);	
							try {
								Float.parseFloat(result[j]);			    	    			
								items.add(result[j]); 
							}
							catch (NumberFormatException e) {}			    	    
						}

						if (items.size() >=3) {
							r[lineCount] = Float.parseFloat(items.get(0).toString())/255;
							g[lineCount] = Float.parseFloat(items.get(1).toString())/255;
							b[lineCount] = Float.parseFloat(items.get(2).toString())/255; 			    		
							a[lineCount] = alpha;
						}
					}

					lineCount++;
					items.clear();
				}
			} // end while 
		} // end try
		catch (IOException e) {
			System.err.println("Error: " + e);
		}
	}

	private String writeOutput(String out, String outFile, int alpha,
			ColorMap colorMap) {
		
		if (out.endsWith("/"))
			out = out.substring(0, out.lastIndexOf("/"));
		
		if (!new File(out + "/Satellite/").exists()) {
			(new File(out + "/Satellite/")).mkdir();
		}
		if (!new File(out + "/Radar/").exists()) {
			(new File(out + "/Radar/")).mkdir();
		}
		if (!new File(out + "/Other/").exists()) {
			(new File(out + "/Other/")).mkdir();
		}
		
		if (alpha == 1) {
			outFile = out + "/Satellite/" + outFile;
		}
		else if (alpha == 2) {
			outFile = out + "/Radar/" + outFile;
		}
		else {
			outFile = out + "/Other/" + outFile;
		}

		try {
			//SerializationUtil.jaxbMarshalToXmlFile(colorMap, outFile);
			JAXBContext context = JAXBContext.newInstance(ColorMap.class);
			Marshaller marshaller = context.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			marshaller.marshal(colorMap, new FileWriter(new File(outFile)));
		} catch (JAXBException e) {
			System.out.println("The color map file is not writable.");
			return "";
		} catch (IOException e) {
			System.out.println("The color map file is not writable.");
			return "";
			//e.printStackTrace();
		}
		//catch (SerializationException e) {
		//	  e.printStackTrace();
		//}
		return outFile;
	}
	
	
	public static void main(String[] args) { 
		if (!new File(args[0]).exists()) {
			System.out.println("The Source directory does not exist.\n");
			return ;
		} 
		if (!new File(args[1]).exists()) {
			System.out.println("The Destination directory does not exist.\n");
			return ;
		}
		if (!new File(args[2]).exists()) {
			System.out.println("The Lookup file does not exist.\n");
			return ;
		}
		
		new ColormapConvert().convertMap(args[0], args[1], args[2]);
		
//		new ColormapConvert().convertMap("/export-1/cdbsrv/nawdev/software/gempak/tables/luts/",
//		"/usr1/qzhou/to11dr11/workspace/build.cave/static/common/cave/ncep/base/luts/",
//		"/export-1/cdbsrv/nawdev/software/gempak/tables/luts/enhance.tbl");
	}
}



