/*
 * NctextRegexMatcher
 * 
 * Date created 06 July 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.edex.plugin.nctext.decoder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * <pre>
* Compares the lines from the file header to a map of regular expressions to determine the product type
* 
* SOFTWARE HISTORY
* Date          Ticket     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 06-Jul-2010    191       Archana.S   Initial Creation
* 18-Aug-2010  191       Archana .S  Minor update in regex for vlcw 
* 21-Nov-2011              Chin Chen   update and re-organize regex to fix several product type decoding errors
* 12/10/2012               Chin Chen   update regex to support "Observed Data" group
* </pre>
* @author Archana
* @version 1 
*/
public final class NctextRegexMatcher {

	/**
	 * 
	 * @return
	 */
	public static List<String> getProductType() {
		if(strNctextProductType.size() > 0){
		return  new ArrayList<String>(strNctextProductType);
		}else{
			return Collections.emptyList();
		}
	}

	public static Integer getParsedDayOfMonth() {
		return new Integer(parsedDate.intValue());
	}

	public static Integer getParsedHour() {
		return new Integer(parsedHour.intValue());
	}

	public static Integer getParsedMinute() {
		return new Integer(parsedMinute.intValue());
	}
	
	public static Set<Pattern> getSetOfPatterns() {
		return new HashSet<Pattern>(setOfPatterns);
	}

	private static final Map<Pattern, String>  NCTEXT_PRODUCT_REGEX_MAP  = initializeMap();
    private static Integer parsedDate = -99; 
    private static Integer parsedHour = -99;
    private static Integer parsedMinute= -99;
    private static ArrayList<String> strNctextProductType = new ArrayList<String>(0);
    private static final Set<Pattern> setOfPatterns = NCTEXT_PRODUCT_REGEX_MAP.keySet();

	/***
	 * Finds the matching Nctext product type by comparing the input file content
	 * with each regular expression in the HashMap. 
	 * @param fileContent
	 * @return true if a match was found or false otherwise
	 */
	protected synchronized static boolean matchFileRegex(String fileContent){
		strNctextProductType = new ArrayList<String>(0);
			boolean isMatchFound = false;
			for (Pattern thisPattern: setOfPatterns){
				Matcher thisMatcher = thisPattern.matcher(fileContent);
				
				if(thisMatcher.matches()){
//					System.out.println("Current line in file is: " + fileContent);
					//retrieve the matching product type for the regular expression
					String prodType =  new String(NCTEXT_PRODUCT_REGEX_MAP.get(thisPattern));
					//System.out.println("Product type is " + prodType);
					//System.out.println("Matching pattern is: " + thisPattern.pattern());
					//If the product type is not already in the list...
					if(!isNctextProductAlreadyInList(prodType)){
					  //add it to the list	
					  strNctextProductType.add(prodType);
						
					}
					//System.out.println("matchFileRegex group count =" + thisMatcher.groupCount() );
					//for(int i=0; i< thisMatcher.groupCount();i++){
					//	System.out.println("matchFileRegex group " + i+ "= "+thisMatcher.group(i));
					//}
					if(thisMatcher.groupCount() > 1){
					//get the data captured from the groups of the matcher
						String dateFromMatchedPattern = new String(thisMatcher.group(1)); 
						String hourFromMatchedPattern = new String(thisMatcher.group(2));
						String minuteFromMatchedPattern = new String(thisMatcher.group(3));
						try{
							parsedDate = new Integer(Integer.parseInt(dateFromMatchedPattern));
							parsedHour =  new Integer(Integer.parseInt(hourFromMatchedPattern)); 	
							parsedMinute = new Integer(Integer.parseInt(minuteFromMatchedPattern));
						} catch (NumberFormatException e){
							System.out.println("matchFileRegex(): NumberFormatException event: for product type "+prodType);
							continue;
						}
					}
					isMatchFound =  true;
				}
				thisMatcher.reset();
			}
			//System.out.println("matchFileRegex returning "+ isMatchFound);
			return isMatchFound;
	}
	/***
	 * Checks if the input Nctext product typeis already present in the list
	 * @param productType - the String object denoting the Nctext product type
	 * @return true if the product type exists in the list or false otherwise
	 */
	private synchronized static boolean isNctextProductAlreadyInList(String productType){
		for(String currentString: strNctextProductType){
			if(currentString.compareTo(productType)==0){
				return true;
			}
		}
		return false;
	}
	
	/***
	 * Creates and initializes a {@code HashMap<Pattern, String>} with the regular expression as the key and the Nctext product type as
	 * the value.<pre>
	 *  {@code <Pattern>} - the compiled regular expression
	 *  {@code <String>} - the Nctext product type
	 *  </pre>
	 * @return the initialized unmodifiable  {@code  Map<Pattern, String>} 
	 */
	private  synchronized static Map<Pattern, String> initializeMap() {
		Map<Pattern, String> thisMap =  new HashMap<Pattern,String>(0);
		//TODO - uncomment the regular expressions for (hrly, snd, syn) once the database tables are updated
		//              to handle ObservedData products

//		thisMap.put(Pattern.compile("^S[AP].... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])"), "hrly");
//		thisMap.put(Pattern.compile("^U[CEFGHIJKLMNPQSWXY].... .... (0[1-9]|[12][0-9]|3[01])"), "snd");

		//TODO - create a separate Pattern outside the HashMap for syn, since it does not have any digits
		//              and the code will crash when it tries to extract a digit from the captured regular expression groups		
		
//		thisMap.put(Pattern.compile("(^S[IM]V[IGNS])|(^SNV[INS])|(^S[IMN](W[KZ]|[^VW]))"), "syn");
		
		//Chin: add \n to separate the 2 lines to make regular expression work "correctly" on those products with
		//product type coded at 2nd line
		thisMap.put(Pattern.compile("^FXCA62 TJSJ (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area");
		thisMap.put(Pattern.compile("^FXUS[67][1-6] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area");
		thisMap.put(Pattern.compile("^FPAK20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area");//no data to test
		thisMap.put(Pattern.compile("^FPHW03 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area");//no data to test
		//thisMap.put(Pattern.compile("^FX(HW|PN|PS)60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area"); 
		//split above pattern to the following 3 patterns so matchFileRegex() can be coded in a generic way.
		thisMap.put(Pattern.compile("^FXHW60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area"); 
		thisMap.put(Pattern.compile("^FXPN60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area"); 
		thisMap.put(Pattern.compile("^FXPS60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "area"); 
		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)FA[0-9].*"), "area");	//aviation forecasts
		thisMap.put(Pattern.compile("^WWUS30 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"wtch2");
		thisMap.put(Pattern.compile("^FNUS21 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"fwddy1");
		thisMap.put(Pattern.compile("^FNUS22 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"fwddy2");
		thisMap.put(Pattern.compile("^WOUS64 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "wou");
		thisMap.put(Pattern.compile("^ABNT20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "outlk"); //clashes with TWO
		thisMap.put(Pattern.compile("^ACPN50 PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "outlk");
		thisMap.put(Pattern.compile("^ABPZ20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "outlk");
		thisMap.put(Pattern.compile("^ABCA33 TJSJ (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "outlk");
		thisMap.put(Pattern.compile("^WTNT4[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "disc");
		thisMap.put(Pattern.compile("^WTPA4[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "disc");
		thisMap.put(Pattern.compile("^WTPZ4[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "disc");
		thisMap.put(Pattern.compile("^WONT41 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "disc");		
		thisMap.put(Pattern.compile("^WTNT3[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");	
		thisMap.put(Pattern.compile("^WTNT3[1-5] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");
		thisMap.put(Pattern.compile("^WTPA3[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");
		thisMap.put(Pattern.compile("^WTPZ3[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");
		thisMap.put(Pattern.compile("^WTPZ3[1-5] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");
		thisMap.put(Pattern.compile("^WTCA4[1-5] TJSJ (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");
		thisMap.put(Pattern.compile("^WHCA31 TFFF (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");
		thisMap.put(Pattern.compile("^WTPN2[1-5] PGTW (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pblc");
		thisMap.put(Pattern.compile("^WTNT2[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mar");	
		thisMap.put(Pattern.compile("^WTPA2[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mar");	
		thisMap.put(Pattern.compile("^WTPZ2[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mar");
		thisMap.put(Pattern.compile("^W[H|T]PS01 NFFN (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mar");	
		thisMap.put(Pattern.compile("^WTPN3[1-5] PGTW (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mar");
		thisMap.put(Pattern.compile("^URNT12 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "rcn"); 
		thisMap.put(Pattern.compile("^U[RZ]NT14 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "rcn");//no test data	
		thisMap.put(Pattern.compile("^AXPZ20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "tdsc");  //clashes with TWD
		thisMap.put(Pattern.compile("^AXNT20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "tdsc");  //clashes with TWD	
		thisMap.put(Pattern.compile("^WTNT7[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "strk");
		thisMap.put(Pattern.compile("^NOUS42 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pod"); 		//no regex for aasdl and pasdl
		thisMap.put(Pattern.compile("^WWJP25 RJTD (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "HSF");  
		thisMap.put(Pattern.compile("^WCPA3[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "conv");	
		thisMap.put(Pattern.compile("^WSUS3[1-3] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "conv");		
		thisMap.put(Pattern.compile("^WAUS4[1-6] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "airm");	
		thisMap.put(Pattern.compile("^WAAK4[7-9] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "airm");	
		thisMap.put(Pattern.compile("^FAAK2[1-6] KZAN (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "cwa");	
		thisMap.put(Pattern.compile("^FAUS2[1-6] KZ.. (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "cwa");
		thisMap.put(Pattern.compile("^WCUS2[1-6] KZ.. (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "cwa");			
		thisMap.put(Pattern.compile("^FAUS20 PANC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mis");
		thisMap.put(Pattern.compile("^FOUS14 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmmos");	
		thisMap.put(Pattern.compile("^FOAK2[5-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmmos");	
		thisMap.put(Pattern.compile("^FOPA20 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "gfsmos");	
		thisMap.put(Pattern.compile("^FOUS2[1-6] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "gfsmos");	
		thisMap.put(Pattern.compile("^FOAK3[7-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "gfsmos");	
		thisMap.put(Pattern.compile("^FEPA20 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FEUS2[1-6] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FEAK3[7-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FECN21 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FOUS4[4-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "etamos");	
		thisMap.put(Pattern.compile("^FQUS2[1-6] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)MMG.*"), "marnmos");
		thisMap.put(Pattern.compile("^FQAK37 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)MMG.*"), "marnmos");
		thisMap.put(Pattern.compile("^FQPA20 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)MMG.*"), "marnmos");
		thisMap.put(Pattern.compile("^FOUE6[0-4] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUE80 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUM6[5-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");	
		thisMap.put(Pattern.compile("^FOUM7[01] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUM8[124] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUS8[6-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUS90 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUW7[238] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUW83 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOHW50 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOCA5[12] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOCN7[456] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");	
		thisMap.put(Pattern.compile("^FOGX77 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ngmgd");	
		thisMap.put(Pattern.compile("^FOUS6[0-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "etagd");
		thisMap.put(Pattern.compile("^FOUS7[0-8] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "etagd");		
    	thisMap.put(Pattern.compile("^FXUS02 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PMDEPD.*"), "PMDEPD");
		thisMap.put(Pattern.compile("^FXUS02 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PREEPD.*"), "PREEPD");
		thisMap.put(Pattern.compile("^FXUS01 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PMDSPD.*"), "PMDSPD");    	
		thisMap.put(Pattern.compile("^FXHW01 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PMDHI.*"), "PMDHI");
		thisMap.put(Pattern.compile("^FXSA20 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PMDSA.*"), "PMDSA");
		thisMap.put(Pattern.compile("^FXCA20 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PMDCA.*"), "PMDCA");
		thisMap.put(Pattern.compile("^FXAK02 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PMDAK.*"), "PMDAK");  		
		thisMap.put(Pattern.compile("^NOUS42 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sdm");
		thisMap.put(Pattern.compile("^NOUS42 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sdm");
		thisMap.put(Pattern.compile("^NPXX10 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl");
		thisMap.put(Pattern.compile("^NPXX10 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl");
		thisMap.put(Pattern.compile("^FXUS10 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "PMDHMD");    	
		thisMap.put(Pattern.compile("^FMUS2[34] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PRB[EW]HI.*"), "hmean");    	
		thisMap.put(Pattern.compile("^FMUS2[34] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PRB[EW]HH.*"), "hmax");
		thisMap.put(Pattern.compile("^FMUS2[34] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PRB[EW]HL.*"), "hmin");
 		thisMap.put(Pattern.compile("^FXUS06 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "n610");    	
		thisMap.put(Pattern.compile("^FEUS40 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "f610"); //no data to test  	
		thisMap.put(Pattern.compile("^FXUS07 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "n30");
		thisMap.put(Pattern.compile("^FXUS05 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "n90");
		thisMap.put(Pattern.compile("^FXHW40 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "hawaii"); //hawaii instead of h3090	
		thisMap.put(Pattern.compile("^FXUS21 KWNC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "PMDTHR");    	
		thisMap.put(Pattern.compile("^FXUS25 KWNC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "drought");
		thisMap.put(Pattern.compile("^FVAK2[1-5] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "volc");
		thisMap.put(Pattern.compile("^WUUS48 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "ptsd48"); 
		thisMap.put(Pattern.compile("^FAUS20 KZ.. (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mis");
		thisMap.put(Pattern.compile("^FANT02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "offsh");	//no raw data at all!!
		//thisMap.put(Pattern.compile("^WSUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sgmt");	//no data to test
		thisMap.put(Pattern.compile("^WAHW[03]1 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "airm");	
		thisMap.put(Pattern.compile("^WAUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "airm");	//no data to test
		thisMap.put(Pattern.compile("^WAUS1 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "airm");	//no data to test
		thisMap.put(Pattern.compile("^WAAK01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "airm");	//no data to test
		thisMap.put(Pattern.compile("^WOUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "stat");
		thisMap.put(Pattern.compile("^FVXX2[0-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "volc");    			
		thisMap.put(Pattern.compile("^FVCN0[0-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "volc");
		thisMap.put(Pattern.compile("^FVAU0[2-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "volc");  
		thisMap.put(Pattern.compile("^ACUS4[1-5] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "storm");
		thisMap.put(Pattern.compile("^FSUS02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "srp");
		thisMap.put(Pattern.compile("^ASUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sus");
		thisMap.put(Pattern.compile("^FXPA00 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "expac");  //no  folder for raw data in server  			
		thisMap.put(Pattern.compile("^FVXX2[0-7] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "vlcf"); //clashes with volc FVXX2[0-4]
	    thisMap.put(Pattern.compile("^ACUS48 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "day48");
		thisMap.put(Pattern.compile("^FNUS31 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PFWFD1.*"), "pfwfd1"); 
		thisMap.put(Pattern.compile("^FNUS32 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PFWFD2.*"), "pfwfd2");  
		thisMap.put(Pattern.compile("^FNUS38 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)PFWF38.*"), "pfwf38");
		thisMap.put(Pattern.compile("^WFCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "tornado"); 
		thisMap.put(Pattern.compile("^WUCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "severe");  
		thisMap.put(Pattern.compile("^WOCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sps");
		thisMap.put(Pattern.compile("^WWCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "warwatch"); 
		thisMap.put(Pattern.compile("^WWCN3[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "warnsumm"); 
		thisMap.put(Pattern.compile("^FOCN45 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "coutlook");  
		thisMap.put(Pattern.compile("^ACUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "day1");
		thisMap.put(Pattern.compile("^ACUS02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "day2");
		thisMap.put(Pattern.compile("^ACUS03 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "day3");
		thisMap.put(Pattern.compile("^WWUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"ptsdy1"); //no data to test
		thisMap.put(Pattern.compile("^WWUS02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"ptsdy2");//no data to test
		thisMap.put(Pattern.compile("^WWUS03 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"ptsdy3");//no data to test
		thisMap.put(Pattern.compile("^WWUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"watch");
		thisMap.put(Pattern.compile("^WWUS50 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)SEV[0-9].*"),"sev");
		thisMap.put(Pattern.compile("^WWUS60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)SEVSPC.*"),"sevmkc");
		thisMap.put(Pattern.compile("^WOUS40 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"public");
		thisMap.put(Pattern.compile("^NWUS22 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)STAHRY.*"),"hry");
		thisMap.put(Pattern.compile("^ACUS11 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"meso");
		thisMap.put(Pattern.compile("^NWUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)STADTS"),"dts"); 
		thisMap.put(Pattern.compile("^NWUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"),"svr"); 
		thisMap.put(Pattern.compile("^FNUS21 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)FWDDY1.*"),"fire1"); //spc uses this regex for fwddy1 also
		thisMap.put(Pattern.compile("^FNUS22 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)FWDDY2.*"),"fire2");	//spc uses this regex for fwddy2 also	
		thisMap.put(Pattern.compile("^WHXX9[09] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mdl"); // not sure if this regex is correct - no test data to verify
		thisMap.put(Pattern.compile("^WHXX0[1-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "mdl");
		thisMap.put(Pattern.compile("^URNT10 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "antreco");	
		thisMap.put(Pattern.compile("^URNT11 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "areco");
		thisMap.put(Pattern.compile("^URNT12 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "avortex");			
		thisMap.put(Pattern.compile("^URNT14 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "asupvort");
		thisMap.put(Pattern.compile("^UZNT13 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "adrops");
		thisMap.put(Pattern.compile("^URPN10 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pntreco"); //no data to  test
		thisMap.put(Pattern.compile("^URPN11 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "preco");//no data to  test
		thisMap.put(Pattern.compile("^URPN12 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pvortex");//no data to  test
//		thisMap.put(Pattern.compile("^URNT15 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North Atlantic High Density Observations");
//		thisMap.put(Pattern.compile("^URPA15 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific High Density Observations");
//		thisMap.put(Pattern.compile("^URPA11 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific Reco Observation");
//		thisMap.put(Pattern.compile("^URPA12 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific Vortex Data Message");
//		thisMap.put(Pattern.compile("^URPA10 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific Reco Observation non-tropical");		
//		thisMap.put(Pattern.compile("^URPN15 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North East and Central Pacific High Density Observations");
		
		thisMap.put(Pattern.compile("^UZPN13 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "pdrops");//no data to  test
		thisMap.put(Pattern.compile("^UZPA13 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "wpdrops");//no data to  test
		thisMap.put(Pattern.compile("^TXUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "satest");
		thisMap.put(Pattern.compile("^SXUS40 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "cgr");
		thisMap.put(Pattern.compile("^SXUS08 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "cgr");		
		thisMap.put(Pattern.compile("^SXUS86 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "cgr");//regexes clash for cgr and OMR
		thisMap.put(Pattern.compile("^WV.... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "vlcw");             //might clash with regex for inl and sgmt
		thisMap.put(Pattern.compile("^W[CSV].... [^KP]... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		//thisMap.put(Pattern.compile("^W[CSV](NT|PN|PA|AK)[01][0-9] (KKCI|PHFO|PAWU) (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		//split above pattern to the following 12 patterns so matchFileRegex() can be coded in a generic way.
		thisMap.put(Pattern.compile("^W[CSV](NT|PN|PA|AK)[01][0-9] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]NT[01][0-9] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]NT[01][0-9] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]NT[01][0-9] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]PN[01][0-9] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]PN[01][0-9] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]PN[01][0-9] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]PA[01][0-9] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]PA[01][0-9] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]PA[01][0-9] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]AK[01][0-9] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]AK[01][0-9] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^W[CSV]AK[01][0-9] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "intl"); //regexes clash for intl and sgmt	
		
		
		thisMap.put(Pattern.compile("^WSUS4[012] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "conv");
		thisMap.put(Pattern.compile("^W[CSV]US0[1-6] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sgmt");	
		thisMap.put(Pattern.compile("^WSUK.. .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sgmt");	
		thisMap.put(Pattern.compile("^WS[^U]... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+"), "sgmt");        //regexes clash for intl and sgmt
		thisMap.put(Pattern.compile("^FT.... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)*TAF[A-Z]{3}(.|\r|\n)*"), "taf"); //observed TAFS
		thisMap.put(Pattern.compile("^FT.... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)+TAF( |\r|\n){1}(.|\r|\n)*"), "fts"); //aviation TAFS
		thisMap.put(Pattern.compile("^...... KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(HSF).*"), "HSF");   //conflicts with FLN
		thisMap.put(Pattern.compile("^...... PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(HSF).*"), "HSF");  
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9])(.|\r|\n)*AGO.*"), "AGO");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FZL).*"), "FZL");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(AFD).*"), "AFD");  
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SFP).*"), "SFP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(ZFP).*"), "ZFP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(LFP).*"), "LFP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(CCF).*"), "CCF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RDF).*"), "RDF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(PFM).*"), "PFM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(AFM).*"), "AFM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SFT).*"), "SFT");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RTP).*"), "RTP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(CLI).*"), "CLI");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(CLM).*"), "CLM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(LSR).*"), "LSR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RER).*"), "RER");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(NOW).*"), "NOW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(PNS).*"),"PNS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(HWO).*"),"HWO");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RWR).*"),"RWR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(MIS).*"),"MIS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(ADA).*"),"ADA");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SVR).*"),"SVR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TOR).*"),"TOR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RWS).*"),"RWS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(WSW).*"),"WSW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SPS).*"),"SPS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SVS).*"),"SVS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SLS).*"),"SLS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(WCN).*"),"WCN");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(NPW).*"),"NPW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(HLS).*"),"HLS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FFG).*"), "FFG");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FFA).*"), "FFA");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FFW).*"), "FFW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FFS).*"), "FFS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FLS).*"), "FLS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FLW).*"), "FLW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FLN).*"), "FLN");	
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RVS).*"), "RVS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR1).*"), "RR1");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR2).*"), "RR2");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR3).*"), "RR3");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR4).*"), "RR4");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR5).*"), "RR5");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR6).*"), "RR6");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR7).*"), "RR7");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR8).*"), "RR8");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RR9).*"), "RR9");	
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RRA).*"), "RRA");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RRM).*"), "RRM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(CWF).*"), "CWF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(OFF).*"), "OFF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(PLS).*"), "PLS");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(OMR).*"), "OMR"); //regexes clash for cgr and OMR
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SMW).*"), "SMW");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(CFW).*"), "CFW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(MWS).*"), "MWS");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(MRP).*"), "MRP");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(MIM).*"), "MIM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TAV).*"), "TAV");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(SCS).*"), "SCS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TWO).*"), "TWO");  //clashes with outlk  			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TWS).*"), "TWS");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TWD).*"), "TWD"); //clashes with tdsc   			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TCM).*"), "TCM");  //clashes with mar  			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TCP).*"), "TCP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TCD).*"), "TCD");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TCE).*"), "TCE");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(TCU).*"), "TCU");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(DSA).*"), "DSA");  //clashes with disc  			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RFW).*"), "RFW"); 
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FWF).*"), "FWF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FWM).*"), "FWM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(RFD).*"), "RFD");
    	thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(UVI).*"), "UVI");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(QPFPFD).*"), "QPFPFD");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(QPFERD).*"), "QPFERD");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(QPFHSD).*"), "QPFHSD");    		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*(\n|\r)(FTM).*"), "ftm"); 
		
		return Collections.unmodifiableMap(thisMap);

	}
	
}






























