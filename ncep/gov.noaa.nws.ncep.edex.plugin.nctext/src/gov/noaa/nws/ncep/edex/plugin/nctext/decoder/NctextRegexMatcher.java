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
					//If the product type is not already in the list...
					if(!isNctextProductAlreadyInList(prodType)){
					  //add it to the list	
					  strNctextProductType.add(prodType);
//						System.out.println("Product type is " + prodType);
//						System.out.println("Matching pattern is: " + thisPattern.pattern());
					}
					
					if(thisMatcher.groupCount() > 1){
					//get the data captured from the groups of the matcher
						String dateFromMatchedPattern = new String(thisMatcher.group(1)); 
						String hourFromMatchedPattern = new String(thisMatcher.group(2));
						String minuteFromMatchedPattern = new String(thisMatcher.group(3));
						parsedDate = new Integer(Integer.parseInt(dateFromMatchedPattern));
						parsedHour =  new Integer(Integer.parseInt(hourFromMatchedPattern)); 	
						parsedMinute = new Integer(Integer.parseInt(minuteFromMatchedPattern));
					}
					isMatchFound =  true;
				}
				thisMatcher.reset();
			}
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

		thisMap.put(Pattern.compile("^FXCA62 TJSJ (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area");
		thisMap.put(Pattern.compile("^WWUS30 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"wtch2");
		thisMap.put(Pattern.compile("^FNUS21 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"fwddy1");
		thisMap.put(Pattern.compile("^FNUS22 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"fwddy2");
		thisMap.put(Pattern.compile("^WOUS64 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "wou");
		thisMap.put(Pattern.compile("^ABNT20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "outlk"); //clashes with TWO
		thisMap.put(Pattern.compile("^ACPN50 PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "outlk");
		thisMap.put(Pattern.compile("^ABPZ20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "outlk");
		thisMap.put(Pattern.compile("^ABCA33 TJSJ (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "outlk");
		thisMap.put(Pattern.compile("^WTNT4[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "disc");
		thisMap.put(Pattern.compile("^WTPA4[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "disc");
		thisMap.put(Pattern.compile("^WTPZ4[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "disc");
		thisMap.put(Pattern.compile("^WONT41 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "disc");		
		thisMap.put(Pattern.compile("^WTNT3[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");	
		thisMap.put(Pattern.compile("^WTNT3[1-5] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");
		thisMap.put(Pattern.compile("^WTPA3[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");
		thisMap.put(Pattern.compile("^WTPZ3[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");
		thisMap.put(Pattern.compile("^WTPZ3[1-5] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");
		thisMap.put(Pattern.compile("^WTCA4[1-5] TJSJ (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");
		thisMap.put(Pattern.compile("^WHCA31 TFFF (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");
		thisMap.put(Pattern.compile("^WTPN2[1-5] PGTW (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pblc");
		thisMap.put(Pattern.compile("^WTNT2[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mar");	
		thisMap.put(Pattern.compile("^WTPA2[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mar");	
		thisMap.put(Pattern.compile("^WTPZ2[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mar");
		thisMap.put(Pattern.compile("^W[H|T]PS01 NFFN (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mar");	
		thisMap.put(Pattern.compile("^WTPN3[1-5] PGTW (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mar");
		thisMap.put(Pattern.compile("^URNT12 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "rcn"); 
//		thisMap.put(Pattern.compile("^U[RZ]NT14 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "rcn");//no test data	
		thisMap.put(Pattern.compile("^AXPZ20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "tdsc");  //clashes with TWD
		thisMap.put(Pattern.compile("^AXNT20 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "tdsc");  //clashes with TWD	
		thisMap.put(Pattern.compile("^WTNT7[1-5] KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "strk");
		thisMap.put(Pattern.compile("^NOUS42 KNHC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pod"); 		//no regex for aasdl and pasdl
		thisMap.put(Pattern.compile("^WWJP25 RJTD (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*HSF.*"), "HSF");  
		thisMap.put(Pattern.compile("^WCPA3[1-5] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "conv");	
		thisMap.put(Pattern.compile("^WSUS3[1-3] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "conv");		
		thisMap.put(Pattern.compile("^W[CSV][NT|PN|PA|AK][01][0-9] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "intl");
		thisMap.put(Pattern.compile("^W[CSV][NT|PN|PA|AK][01][0-9] PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "intl");		
		thisMap.put(Pattern.compile("^W[CSV][NT|PN|PA|AK][01][0-9] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "intl");		
		thisMap.put(Pattern.compile("^W[CSV]US0[1-6] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*"), "sgmt");	
		thisMap.put(Pattern.compile("^WAUS4[1-6] KKCI (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "airm");	
		thisMap.put(Pattern.compile("^WAAK4[7-9] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "airm");	
		thisMap.put(Pattern.compile("^FAAK2[1-6] KZAN (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "cwa");	
		thisMap.put(Pattern.compile("^FAUS2[1-6] KZ.. (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "cwa");
		thisMap.put(Pattern.compile("^WCUS2[1-6] KZ.. (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "cwa");			
		thisMap.put(Pattern.compile("^FAUS20 PANC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mis");	
		thisMap.put(Pattern.compile("^FOUS14 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmmos");	
		thisMap.put(Pattern.compile("^FOAK2[5-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmmos");	
		thisMap.put(Pattern.compile("^FOPA20 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "gfsmos");	
		thisMap.put(Pattern.compile("^FOUS2[1-6] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "gfsmos");	
		thisMap.put(Pattern.compile("^FOAK3[7-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "gfsmos");	
		thisMap.put(Pattern.compile("^FEPA20 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FEUS2[1-6] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FEAK3[7-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FECN21 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "gfsxmos");	
		thisMap.put(Pattern.compile("^FOUS4[4-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "etamos");	
		thisMap.put(Pattern.compile("^FQUS2[1-6] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*MMG.*"), "marnmos");
		thisMap.put(Pattern.compile("^FOUE6[0-4] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUE80 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUM6[5-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");	
		thisMap.put(Pattern.compile("^FOUM7[01] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUM8[124] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUS8[6-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUS90 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUW7[238] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOUW83 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOHW50 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOCA5[12] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");
		thisMap.put(Pattern.compile("^FOCN7[456] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");	
		thisMap.put(Pattern.compile("^FOGX77 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ngmgd");	
		thisMap.put(Pattern.compile("^FOUS6[0-9] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "etagd");
		thisMap.put(Pattern.compile("^FOUS7[0-8] KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "etagd");		
    	thisMap.put(Pattern.compile("^FXUS02 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PMDEPD.*"), "PMDEPD");
		thisMap.put(Pattern.compile("^FXUS02 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PREEPD.*"), "PREEPD");
		thisMap.put(Pattern.compile("^FXUS01 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PMDSPD.*"), "PMDSPD");    	
		thisMap.put(Pattern.compile("^FXHW01 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PMDHI.*"), "PMDHI");
		thisMap.put(Pattern.compile("^FXSA20 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PMDSA.*"), "PMDSA");
		thisMap.put(Pattern.compile("^FXCA20 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PMDCA.*"), "PMDCA");
		thisMap.put(Pattern.compile("^FXAK02 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PMDAK.*"), "PMDAK");  		
		thisMap.put(Pattern.compile("^NOUS42 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "sdm");
		thisMap.put(Pattern.compile("^NOUS42 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "sdm");
		thisMap.put(Pattern.compile("^NPXX10 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "intl");
		thisMap.put(Pattern.compile("^NPXX10 KWNO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "intl");
		thisMap.put(Pattern.compile("^FXUS10 KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "PMDHMD");    	
		thisMap.put(Pattern.compile("^FMUS2[34] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PRB[EW]HI.*"), "hmean");    	
		thisMap.put(Pattern.compile("^FMUS2[34] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PRB[EW]HH.*"), "hmax");
		thisMap.put(Pattern.compile("^FMUS2[34] KWNH (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PRB[EW]HL.*"), "hmin");
 		thisMap.put(Pattern.compile("^FXUS06 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "n610");    	
		thisMap.put(Pattern.compile("^FEUS40 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "f610"); //no data to test  	
		thisMap.put(Pattern.compile("^FXUS07 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "n30");
		thisMap.put(Pattern.compile("^FXUS05 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "n90");
		thisMap.put(Pattern.compile("^FXHW40 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "hawaii"); //hawaii instead of h3090	
		thisMap.put(Pattern.compile("^FXUS21 KWNC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "PMDTHR");    	
		thisMap.put(Pattern.compile("^FXUS25 KWNC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "drought");
		thisMap.put(Pattern.compile("^FVAK2[1-5] PAWU (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "volc");
		thisMap.put(Pattern.compile("^WUUS48 KWNS (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "ptsd48"); 
		thisMap.put(Pattern.compile("^FAUS20 KZ.. (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mis");
		thisMap.put(Pattern.compile("^FXUS6[1-6] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area");
		thisMap.put(Pattern.compile("^FXUS7[1-6] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area");
//		thisMap.put(Pattern.compile("^FPAK20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area");//no data to test
//		thisMap.put(Pattern.compile("^FPHW03 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area");//no data to test
		thisMap.put(Pattern.compile("^FXHW60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area"); //derived from FX(HW|PN|PS)60
		thisMap.put(Pattern.compile("^FXPN60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area"); //derived from FX(HW|PN|PS)60
		thisMap.put(Pattern.compile("^FXPS60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "area"); //derived from FX(HW|PN|PS)60
//		thisMap.put(Pattern.compile("^FANT02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "offsh");	//no raw data at all!!
		thisMap.put(Pattern.compile("^WSUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "sgmt");	//no data to test
		thisMap.put(Pattern.compile("^WAHW[03]1 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "airm");	
//		thisMap.put(Pattern.compile("^WAUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "airm");	//no data to test
		thisMap.put(Pattern.compile("^WAUS1 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "airm");	//no data to test
//		thisMap.put(Pattern.compile("^WAAK01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "airm");	//no data to test
		thisMap.put(Pattern.compile("^WOUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "stat");
		thisMap.put(Pattern.compile("^FVXX2[0-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "volc");    			
		thisMap.put(Pattern.compile("^FVCN0[0-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "volc");
		thisMap.put(Pattern.compile("^FVAU0[2-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "volc");  
		thisMap.put(Pattern.compile("^ACUS4[1-5] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "storm");
		thisMap.put(Pattern.compile("^FSUS02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "srp");
		thisMap.put(Pattern.compile("^ASUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "sus");
//		thisMap.put(Pattern.compile("^FXPA00 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "expac");  //no  folder for raw data in server  			
		thisMap.put(Pattern.compile("^FVXX2[0-7] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "vlcf"); //clashes with volc FVXX2[0-4]
	    thisMap.put(Pattern.compile("^ACUS48 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "day48");
		thisMap.put(Pattern.compile("^FNUS31 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PFWFD1.*"), "pfwfd1"); 
		thisMap.put(Pattern.compile("^FNUS32 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PFWFD2.*"), "pfwfd2");  
		thisMap.put(Pattern.compile("^FNUS38 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PFWF38.*"), "pfwf38");
		thisMap.put(Pattern.compile("^NOUS6. .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FTM.*"), "ftm"); 
		thisMap.put(Pattern.compile("^WFCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "tornado"); 
		thisMap.put(Pattern.compile("^WUCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "severe");  
		thisMap.put(Pattern.compile("^WOCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "sps");
		thisMap.put(Pattern.compile("^WWCN1[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "warwatch"); 
		thisMap.put(Pattern.compile("^WWCN3[1|2] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "warnsumm"); 
		thisMap.put(Pattern.compile("^FOCN45 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "coutlook");  
		thisMap.put(Pattern.compile("^ACUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "day1");
		thisMap.put(Pattern.compile("^ACUS02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "day2");
		thisMap.put(Pattern.compile("^ACUS03 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "day3");
		thisMap.put(Pattern.compile("^WWUS01 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"ptsdy1"); //no data to test
		thisMap.put(Pattern.compile("^WWUS02 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"ptsdy2");//no data to test
		thisMap.put(Pattern.compile("^WWUS03 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"ptsdy3");//no data to test
		thisMap.put(Pattern.compile("^WWUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"watch");
		thisMap.put(Pattern.compile("^WWUS50 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SEV[0-9].*"),"sev");
		thisMap.put(Pattern.compile("^WWUS60 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SEVSPC.*"),"sevmkc");
		thisMap.put(Pattern.compile("^WOUS40 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"public");
		thisMap.put(Pattern.compile("^NWUS22 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*STAHRY.*"),"hry");
		thisMap.put(Pattern.compile("^ACUS11 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"meso");
		thisMap.put(Pattern.compile("^NWUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*STADTS"),"dts"); 
		thisMap.put(Pattern.compile("^NWUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"),"svr"); 
		thisMap.put(Pattern.compile("^FNUS21 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FWDDY1.*"),"fire1"); //spc uses this regex for fwddy1 also
		thisMap.put(Pattern.compile("^FNUS22 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FWDDY2.*"),"fire2");	//spc uses this regex for fwddy2 also	
//		thisMap.put(Pattern.compile("^WHXX9[09] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mdl"); // not sure if this regex is correct - no test data to verify
		thisMap.put(Pattern.compile("^WHXX0[1-4] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "mdl");
		thisMap.put(Pattern.compile("^URNT10 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "antreco");	
		thisMap.put(Pattern.compile("^URNT11 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "areco");
		thisMap.put(Pattern.compile("^URNT12 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "avortex");			
		thisMap.put(Pattern.compile("^URNT14 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "asupvort");
		thisMap.put(Pattern.compile("^UZNT13 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "adrops");
		thisMap.put(Pattern.compile("^URPN10 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pntreco"); //no data to  test
		thisMap.put(Pattern.compile("^URPN11 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "preco");//no data to  test
		thisMap.put(Pattern.compile("^URPN12 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pvortex");//no data to  test
//		thisMap.put(Pattern.compile("^URNT15 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North Atlantic High Density Observations");
//		thisMap.put(Pattern.compile("^URPA15 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific High Density Observations");
//		thisMap.put(Pattern.compile("^URPA11 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific Reco Observation");
//		thisMap.put(Pattern.compile("^URPA12 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific Vortex Data Message");
//		thisMap.put(Pattern.compile("^URPA10 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North West Pacific Reco Observation non-tropical");		
//		thisMap.put(Pattern.compile("^URPN15 KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "North East and Central Pacific High Density Observations");
		
		thisMap.put(Pattern.compile("^UZPN13 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "pdrops");//no data to  test
		thisMap.put(Pattern.compile("^UZPA13 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "wpdrops");//no data to  test
		thisMap.put(Pattern.compile("^TXUS20 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "satest");
		thisMap.put(Pattern.compile("^SXUS40 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "cgr");
		thisMap.put(Pattern.compile("^SXUS08 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "cgr");		
		thisMap.put(Pattern.compile("^SXUS86 .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "cgr");//regexes clash for cgr and OMR
		thisMap.put(Pattern.compile("^WV.... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*"), "vlcw");             //might clash with regex for inl and sgmt
		thisMap.put(Pattern.compile("^W[CSV].... [^KP]... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*"), "intl"); //regexes clash for intl and sgmt	
		thisMap.put(Pattern.compile("^WSUS4[012] .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "conv");
		thisMap.put(Pattern.compile("^WSUK.. .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*"), "sgmt");	
		thisMap.put(Pattern.compile("^WS[^U]... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*"), "sgmt");        //regexes clash for intl and sgmt
		thisMap.put(Pattern.compile("^FT.... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).+"), "fts");
		thisMap.put(Pattern.compile("^...... KWBC (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*HSF.*"), "HSF");   //conflicts with FLN
		thisMap.put(Pattern.compile("^...... PHFO (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*HSF.*"), "HSF");  
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*AGO.*"), "AGO");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FZL.*"), "FZL");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*AFD.*"), "AFD");  
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SFP.*"), "SFP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*ZFP.*"), "ZFP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*LFP.*"), "LFP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*CCF.*"), "CCF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RDF.*"), "RDF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PFM.*"), "PFM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*AFM.*"), "AFM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SFT.*"), "SFT");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RTP.*"), "RTP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*CLI.*"), "CLI");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*CLM.*"), "CLM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*LSR.*"), "LSR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RER.*"), "RER");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*NOW.*"), "NOW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PNS.*"),"PNS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*HWO.*"),"HWO");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RWR.*"),"RWR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*MIS.*"),"MIS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*ADA.*"),"ADA");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SVR.*"),"SVR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TOR.*"),"TOR");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RWS.*"),"RWS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*WSW.*"),"WSW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SPS.*"),"SPS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SVS.*"),"SVS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SLS.*"),"SLS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*WCN.*"),"WCN");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*NPW.*"),"NPW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*HLS.*"),"HLS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FFG.*"), "FFG");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FFA.*"), "FFA");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FFW.*"), "FFW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FFS.*"), "FFS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FLS.*"), "FLS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FLW.*"), "FLW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FLN.*"), "FLN");	
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RVS.*"), "RVS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR1.*"), "RR1");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR2.*"), "RR2");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR3.*"), "RR3");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR4.*"), "RR4");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR5.*"), "RR5");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR6.*"), "RR6");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR7.*"), "RR7");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR8.*"), "RR8");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RR9.*"), "RR9");	
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RRA.*"), "RRA");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RRM.*"), "RRM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*CWF.*"), "CWF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*OFF.*"), "OFF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*PLS.*"), "PLS");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*OMR.*"), "OMR"); //regexes clash for cgr and OMR
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SMW.*"), "SMW");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*CFW.*"), "CFW");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*MWS.*"), "MWS");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*MRP.*"), "MRP");		
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*MIM.*"), "MIM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FA[0-9].*"), "area");	
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TAV.*"), "TAV");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*SCS.*"), "SCS");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TWO.*"), "TWO");  //clashes with outlk  			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TWS.*"), "TWS");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TWD.*"), "TWD"); //clashes with tdsc   			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TCM.*"), "TCM");  //clashes with mar  			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TCP.*"), "TCP");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TCD.*"), "TCD");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TCE.*"), "TCE");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*TCU.*"), "TCU");    			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*DSA.*"), "DSA");  //clashes with disc  			
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RFW.*"), "RFW"); 
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FWF.*"), "FWF");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*FWM.*"), "FWM");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*RFD.*"), "RFD");
    	thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*UVI.*"), "UVI");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*QPFPFD.*"), "QPFPFD");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*QPFERD.*"), "QPFERD");
		thisMap.put(Pattern.compile("^...... .... (0[1-9]|[12][0-9]|3[01])([01][0-9]|2[0-3])([0-5][0-9]).*QPFHSD.*"), "QPFHSD");    		
		
		return Collections.unmodifiableMap(thisMap);

	}
	
}






























