/**
 * UairSeparator
 * 
 * This class provides the Sepataror class for upper air sounding
 * data.
 *
 *<pre>
 * SOFTWARE HISTORY
 *
 * Date    		Ticket#		Engineer	Description
 * -------		-------  	--------	-----------
 * 03/2010		210			L. Lin		Initial Creation
 * 4/2011					T. Lee		Persisted to HDF5
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0      
 */

 package gov.noaa.nws.ncep.edex.plugin.h5uair.decoder;

 import java.util.ArrayList;
 import java.util.Iterator;
 import java.util.List;
 import java.util.NoSuchElementException;
 import java.util.regex.Matcher;
 import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
 import com.raytheon.edex.plugin.AbstractRecordSeparator;
 import com.raytheon.edex.util.Util;
import org.apache.log4j.Logger;
 
 public class H5UairSeparator extends AbstractRecordSeparator {
	 
	 private final Logger log = Logger.getLogger(getClass().getName());	   
	 /** Regex used for extracting the header files */
	 public static final String WMO_HEADER = "[A-Z]{4}\\d{0,2} [A-Z]{4} \\d{6}( [A-Z]{3})?\\r\\r\\n"
		 + "([A-Z,0-9]{6}\\r\\r\\n)?(\\r\\r\\n)?([0-9]{5} )?";
	 
	 /** Regular expression for UAIR report */
	 //public static final String UAIR_REPORT  = "(TT|PP)(AA|BB|CC|DD) (([0-9]| |/)*\\r\\r\\n){0,15}([0-9]| |/)*=( )?\\r\\r\\n";
	 //public static final String UAIR_REPORT  = "(TT|PP)(AA|BB|CC|DD) ([0-9]| |/|\\r|\\n)*(NIL)?=";
	 //public static final String UAIR_REPORT  = "(TT|PP)(AA|BB|CC|DD) .*(NIL)?=";

	 /** List of bulletins contained in file */
	 private List<String> bulletins;

	 /** List of records contained in file */
	 private List<String> records;

	 /** Iterator for records */
	 private Iterator<String> iterator = null;

	 /**
	  * Constructor.
	  */
	 public H5UairSeparator(){
		 records = new ArrayList<String>();
		 bulletins = new ArrayList<String>();
	 }
	 
	 public static H5UairSeparator separate(byte[] data, Headers headers) {
		 H5UairSeparator ds = new H5UairSeparator();
		 ds.setData(data, headers);
		 return ds;
	 }
	 
	 public void setData(byte[] data, Headers headers) {
		 doSeparate(new String(data));
		 iterator = records.iterator();
	 }

	 /*
	  * (non-Javadoc)
	  * @see com.raytheon.edex.plugin.IRecordSeparator#hasNext()
	  */
	 public boolean hasNext() {
		 if (iterator == null) {
			 return false;
		 } else {
			 return iterator.hasNext();
		 }
	 }	  

	 /*
	  * Get record.
	  */
	 public byte[] next() {
		 try {
			 String temp = iterator.next();
			 if (Util.isEmptyString(temp)) {
				 return (byte[])null;
			 } else {
				 return temp.getBytes();
			 }
		 } catch (NoSuchElementException e) {
			 return (byte[])null;
		 }
	 }
	 
	 
	 /**
	  * @param message separate bulletins
	  */
	 private void doSeparate(String message) {
		 /* Regex used for separate the bulletins */
		 final String BULLSEPARATOR = "\\x01\\r\\r\\n\\d{3} \\r\\r\\n"+ WMO_HEADER;
		 Pattern bullPattern;	
		 bullPattern = Pattern.compile(BULLSEPARATOR);	
		 		 
		 try {
			 Matcher bullMatcher = bullPattern.matcher(message);
			 /*
			  * Set number of bulletins to records only if the bulletin separator is not the same.
			  * At the point, only separators are stored in "records"  
			  */
			 while (bullMatcher.find()) {
				 if (!bulletins.contains(bullMatcher.group())) {
					 bulletins.add(bullMatcher.group());
				 }
			 }

			 /*
			  * Append the raw data to the bulletin arrays.
			  */
			 for (int i = 0; i < bulletins.size(); i++) {
				 if (i < bulletins.size() - 1) {
					 bulletins.set(i, "\n" + 
							 message.substring(message.indexOf(bulletins.get(i)),
									 message.indexOf(bulletins.get(i + 1))));
				 } else {
					 bulletins.set(i, "\n" +
							 message.substring(message.indexOf(bulletins.get(i))));
				 }
			 }
		 } catch (Exception e) {
			 if ( log.isInfoEnabled()) {
				 log.info ("No valid bulletins found!");
			 }
		 }
		 
		 /*
		  * Extract all the UAIR reports out of the bulletin array list.  Add the
		  * "header" (i.e., WMO header) to each report so that it can be decoded
		  * and write to a single database table. 
		  */
		 final String UAIR_REPORT  = "(TT|PP|XX|UU)(AA|BB|CC|DD).*?=";
		 Pattern reportPattern = Pattern.compile(UAIR_REPORT, Pattern.DOTALL);
		 
		 for (String bull : bulletins) {
			 
			 Boolean single = false;
			 String bullsave= new String(bull);
			 String header=null;

			 final String EQUALS_SIGN = "=";
			 Pattern equalsPattern = Pattern.compile(EQUALS_SIGN);
			 Matcher equalsMatcher = equalsPattern.matcher(bull);
			 if ( ! equalsMatcher.find() ) {
				 int len1 = bull.length();
				 
				 bull = bull.replaceAll("\r\r\nTT", "=\r\r\nTT").replaceAll("\r\r\nPP", "=\r\r\nPP")
				 	.replaceAll("\r\r\nUU", "=\r\r\nUU").replaceAll("\r\r\nXX", "=\r\r\nXX");
				 int len2 = bull.length();
				 
				 bull = bull.replaceAll("\r\r\n\003", "=\r\r\n\003");
				 
				 if ( len2 <= len1+1 ) {
					 single = true;
				 }
			 }
			 			 
			 final String EQUAL_SIGN = "=";
			 Pattern equalPattern = Pattern.compile(EQUAL_SIGN);
			 Matcher equalMatcher = equalPattern.matcher(bull);
			 if ( equalMatcher.find() ) {
				 Matcher headerMatcher = bullPattern.matcher(bullsave);
				 if ( headerMatcher.find()) {
					 header = headerMatcher.group();					 				 
					 Matcher reportMatcher = reportPattern.matcher(bull);
					 
					 String report;
					 while ( reportMatcher.find()) {
						 report = header + reportMatcher.group(0);

						 if ( !records.contains(reportMatcher.group(0))) {
							 records.add(report);
						 }				 
					 }
				 }
			 }  else if ( single ) {
					 records.add(bullsave);
			 }
			 
		 }
	 }
 }