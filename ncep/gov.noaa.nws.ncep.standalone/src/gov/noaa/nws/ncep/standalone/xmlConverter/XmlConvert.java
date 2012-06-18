package gov.noaa.nws.ncep.standalone.xmlConverter;


import gov.noaa.nws.ncep.standalone.util.ProductConverter;
import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
//import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * XmlConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/25/2010   137         Q. Zhou     Initial created
 * 4/21/2010   137         Q. Zhou     Add watch.(now read anchor from table, otherwise uncommand the first few lines in Main.
 * 8/12/2010               Q. Zhou     modified contours group number -- count
 * 9/09/2010   137         Q. Zhou     Removed Localizaton.  Use environment vars.
 * 1/06/2011   137         Q. Zhou     Created local copy of ProductConverter.
 *  								   Removed edex running and isconverter condition.
 * 1/25/2011   137         Q. Zhou     Refact code, Separate DE and Collection
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class XmlConvert {
	public int convertXml(String in, String out) throws IOException {

		//String currentDir = System.getProperty("usr.dir");
		//String currentDir = new File(".").getAbsolutePath();   
		//String fileDir = XmlConvert.class.getProtectionDomain().getCodeSource().getLocation().toString(); // file:/usr1/qzhou/to11dr3/workspace/gov.noaa.nws.ncep.pgen/bin/
    			
//		String http = null;
//		try {
//			http = System.getenv("HTTP");
//		} catch (NullPointerException e) {
//		} catch (SecurityException e) {
//		}		
//		if (http != null)
//    		NcDirectDbQuery.setHttpServer(http);
    	
		
		String vgf = "";	
		FileWriter fw = new FileWriter(out);
		DrawableElement de = null;
		int collectionCnt = 0;
		int contourTot = 0; //for grpNums of contour and symLabel
		
	    Products products  = Util.read(in);
	    //products = FileTools.read(in);
	    
	    if (products == null)
	    	return 0;
	    
	    List<Product> prod = ProductConverter.convert( products );
	    
	    if (!prod.isEmpty())
	    	fw.write(VgfTags.vgfHead);
	        
	    for (int i=0; i< prod.size(); i++) {
	    	List<Layer> lay = prod.get(i).getLayers();
	    	
	    	for (int j=0; j<lay.size() ; j++) {	    		
	    		List<AbstractDrawableComponent> adc = lay.get(j).getDrawables();
	    		contourTot = 0;
	    		for (int k=0; k<adc.size(); k++) {
	    			
	    			if (adc.get(k) instanceof DrawableElement) {
	    				de = adc.get(k).getPrimaryDE();
	    				vgf = ToTag.tagVGF(vgf, de);
	    				fw.write(vgf);
	    			}
	    			
	    			else if (adc.get(k) instanceof DECollection) {		    				
	    				vgf = ToTagCollection.tagCollection(vgf, adc.get(k), fw, collectionCnt, contourTot);
	    				collectionCnt++;
	    				//count total CONTOURS de number
	    				if (adc.get(k).getName().equalsIgnoreCase("Contours")) {
	    					Iterator<DrawableElement> it = adc.get(k).createDEIterator();
	    					while ((de = it.next()) != null) {
	    						if (de.getPgenCategory().equalsIgnoreCase("Lines")) 
	    							contourTot++;
	    					}	    						    					
	    				}	    				
	    			}
	    		}
	    	}
	    }
	    fw.close();

	    return 1;
	}

	
//	void setUpEnvironment(ProcessBuilder builder) {
//	    Map<String, String> env = builder.environment();
//	    
//	}
//
//	public static Properties load(File propsFile) throws IOException {
//	        Properties props = new Properties();
//	        FileInputStream fis = new FileInputStream(propsFile);
//	        props.load(fis); //.loadFromXML(fis);
//	        String s = props.getProperty("HTTPServer");
//	        fis.close();
//	        return props;
//	}
		
}

