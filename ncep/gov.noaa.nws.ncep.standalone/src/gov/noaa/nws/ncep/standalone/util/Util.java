/*
 * gov.noaa.nws.ncep.standalone.util.Util.java
 * 
 * Date created (as Feb 1, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.standalone.util;

import gov.noaa.nws.ncep.ui.pgen.file.Products;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

/**
 * Utility methods to be used in standalone package.
 * 01/11     TTR 169      Q. Zhou    Handled exception 
 * @author mlaryukhin
 */
public class Util {
	
	private static String jaxbPackage	= "gov.noaa.nws.ncep.ui.pgen.file";

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
        String path = Util.class.getProtectionDomain().getCodeSource().getLocation().getPath();
		if(!help.exists()) {
	        int lastIndex = path.lastIndexOf("/");
	        path = path.substring(0, lastIndex + 1);
	        help = new File(path + fileName);
		}
		if(!help.exists()) {
			// maybe running from Eclipse? for dev only
			help = new File(path + "../hlp/" + fileName);
		}
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
			JAXBContext context = JAXBContext.newInstance(jaxbPackage);
			Unmarshaller unmarshaller = context.createUnmarshaller();
			products = (Products) unmarshaller.unmarshal(new FileReader(fileName));
		} catch (JAXBException e) {
			//throw new RuntimeException(e);
			System.out.println("*** The xml file " +fileName+ " is not readable."); //+ e.getStackTrace()
		} catch (FileNotFoundException e) {
			//throw new FileNotFoundException("An error occurred while opening " + fileName);
			System.out.println("*** The xml file " +fileName+ " is not exist.");
		}
		return products;
	}
	
	/**
	 * Writes product in a file into a JAXB-based Products object
	 * 
	 * @throws FileNotFoundException
	 */
	public static void write(String fileName, Products products, Class<?> clazz) throws FileNotFoundException {
		try {
			JAXBContext context = JAXBContext.newInstance(clazz);
			Marshaller marshaller = context.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			marshaller.marshal(products, new FileWriter(fileName));
		} catch (JAXBException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new FileNotFoundException("An error occurred writing to " + fileName);
		}
	}
}
