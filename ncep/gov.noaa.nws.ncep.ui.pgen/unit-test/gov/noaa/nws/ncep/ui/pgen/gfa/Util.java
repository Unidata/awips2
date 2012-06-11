package gov.noaa.nws.ncep.ui.pgen.gfa;

/*
 * gov.noaa.nws.ncep.standalone.util.Util.java
 * 
 * Date created (as Feb 1, 2010)
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

import gov.noaa.nws.ncep.ui.pgen.file.Products;

import java.io.*;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

//import org.apache.log4j.Logger;

/**
 * Utility methods
 * 
 * @author mlaryukhin
 */
public class Util {
	
	private static JAXBContext context;
//	private static Logger log = Logger.getLogger(Util.class);
	private static String jaxbPackage = "gov.noaa.nws.ncep.ui.pgen.file";

	/**
	 * Reads the contents of the file. 
	 * 
	 * @param fileName file name
	 * @return
	 * @throws IOException
	 */
	public static StringBuilder getContent(String fileName) throws IOException {

		StringBuilder s = new StringBuilder(1000);
		File help = new File(fileName);
		BufferedReader input = new BufferedReader(new FileReader(help));
		try {
			String line = null;
			while ((line = input.readLine()) != null) {
				s.append(line);
				s.append(System.getProperty("line.separator"));
			}
		} finally {
			input.close();
		}
		return s;
	}
	
	/**
	 * Reads product in a file into a JAXB-based Products object
	 * 
	 * @throws FileNotFoundException
	 */
	public static Products read(String fileName) throws FileNotFoundException {
		Products products = null;
		try {
			getContext();
			Unmarshaller unmarshaller = context.createUnmarshaller();
			products = (Products) unmarshaller.unmarshal(new FileReader(fileName));
		} catch (JAXBException e) {
			throw new RuntimeException(e);
		} catch (FileNotFoundException e) {
			throw new FileNotFoundException("An error occurred while opening " + fileName);
		}
		return products;
	}
	
	private static JAXBContext getContext(){
		if(context == null) {
			try {
				context = JAXBContext.newInstance(jaxbPackage);
			} catch (JAXBException e) {
//				log.error("Error reading context ", e);
				e.printStackTrace();
			}
		}
		return context;
		
	}
}
