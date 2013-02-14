/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import javax.xml.parsers.DocumentBuilder; 
import javax.xml.parsers.DocumentBuilderFactory; 
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import java.io.*; 
import java.text.*;
import java.util.*;
import java.util.regex.Pattern;
import java.lang.reflect.*;

import org.w3c.dom.*;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.*;

/**
 * The class for Volcano info storage and utilities 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 * 07/11        #450        G. Hull     NcPathManager
 * 10/11        #?          J. Wu       Match "OBS" with "F00"
 * 02/12		#481,2,3	B. Yin		Fixed VAA text output issues.
 * 11/12		#889		B. Yin		Look in PGEN working directory for the advisory number.
 *
 * </pre>
 * 
 * @author	G. Zhang
 */

public class VaaInfo {
	
	/**
	 * the Volcano Product Center name
	 */
	public static final String VOLCANO_PRODUCT_NAME = "VOLCANO";
	
	/**
	 * pgenCategory 
	 */
	public static final String PGEN_CATEGORY = "SIGMET";
	
	/**
	 * pgenType for Volcano
	 */
	public static final String PGEN_TYPE_VOLCANO = "VOLC_SIGMET";
	
	/**
	 * pgenType for Vaa Ash Cloud
	 */
	public static final String PGEN_TYEP_CLOUD = "VACL_SIGMET";
	
	/**
	 * the map holding info of the vaa.xml. 
	 * key: top element: PATH, FORMAT,etc; 
	 * value: orig-stn=KNES, vaac=/DARWIN,etc 		  
	 */
	public static final Map<String, ArrayList<String>>  VAA_INFO_MAP = new HashMap<String, ArrayList<String>>();	
	
	/**
	 * LOC_EDIT from VolcanoVaaAttrDlg; 
	 * LOC_CREATE from VolcanoCreateDlg.	  
	 */
	public static final String[] LOCS = new String[]{"LOC_EDIT","LOC_CREATE"};
	
	/**
	 * standard layers with Product Center's VOLCANO Product.	
	 */
	public static final String[] LAYERS = new String[]{"VOLCANO","OBS","F06","F12","F18"};
	
	/**
	 * Volcano Ash Cloud's others-fcst info
	 * dialog: 	for VaaCloudDlg Combo widget
	 * display: for the drawing of Vaa cloud
	 * text:	for the final VAA text product 
	 */
	public static final String OTHERSFCST_DIALOG="dialog", OTHERSFCST_DISPLAY="display", OTHERSFCST_TEXT="text";
	
	/**
	 * the map holding the corresponding Product to the related Volcano	  
	 */
	public static final Map<Volcano, Product> VOL_PROD_MAP = new HashMap<Volcano, Product>();
	
	/**
	 * the common words separator, used to divide words in a line of text
	 */	
	public static final String SEPERATER = SigmetInfo.LINE_SEPERATER;
	
	/**
	 * message to be displayed when trying to open a non-drawable text element 
	 */	
	public static final String NONE_DRAWABLE_MSG = "THIS IS A TEXT PRODUCT THAT CANNOT BE DISPLAYED!";
	
	/**
	 * text for display message and text product of NotSeen type of Vaa Cloud
	 */
	public static final String VA_NOT_IDENTIFIABLE = "VA NOT IDENTIFIABLE FROM SATELLITE DATA";
	
	/**
	 * constant for text type
	 */
	public static final String TYPE_TEXT = "Text";
	
	/**
	 * default product string
	 */	
	public static final String DEFAULT_PRODUCT;
		
	/**
	 * index for current layer
	 */
	public static int CURRENT_LAYER_INDEX = 1;
	
	/**
	 * maps holding vaa.xml values according to elements type:
	 * key: 	usually the leading attribute of an xml element
	 * value: 	corresponding attributes in an array  
	 */
	public static Map<String, String[]> 
		VAA_INFO_SINGLE_MAP = new HashMap<String, String[]>(),
		VAA_INFO_PATH_MAP = new HashMap<String, String[]>(),
		VAA_INFO_STN_MAP = new HashMap<String, String[]>(),	
		VAA_INFO_OTHERSFCST_MAP = new HashMap<String, String[]>(),
		VAA_INFO_WORDING_MAP = new HashMap<String, String[]>();
		
	/**
	 * map holding info and methods to avoid hard coded 
	 * entries/setters for setting Vaa attributes.
	 * key: 	entry items of Vaa.xml, <VOLCANO>,<REMARKS>,etc
	 * value:	Volcano class setters,  getName(), getRemarks(),etc 	 
	 */	
	public static final Map<String, Method> ENTRY_VOLSETTER_MAP = new HashMap<String,Method>();
	
	/**
	 * constants for different Volcano file format	 
	 */
	public static final String FILE_EXTENSION_XML = "xml";
	public static final String FILE_EXTENSION_TXT = "txt";
	
	/**
	 * initializer: parsing the vaa.xml file then populate
	 * the data structures
	 */
	static {
		parseVaaFile();
		initVaaMaps();
		
		initEntryVolSetterMap();
		
		DEFAULT_PRODUCT = ProductInfo.getProduct(LOCS[0])[0];
	}
	
	/**
	 * each entry keys to a setter using java reflection's Method
	 */
	
	public static void initEntryVolSetterMap(){
		
		/* Note:
		 * 1. <OBS ASH DATA/TIME> NIL MAYBE of QUICK not included 
		 *    since there are two methods in Volcano
		 * 
		 * 2. Volcano's other setters without entries NOT included
		 */
		
		try{
			ENTRY_VOLSETTER_MAP.put("<VOLCANO>", Volcano.class.getMethod("setName", String.class) );
			ENTRY_VOLSETTER_MAP.put("<NUMBER>", Volcano.class.getMethod("setNumber", String.class) );
			ENTRY_VOLSETTER_MAP.put("<LOCATION>", Volcano.class.getMethod("setTxtLoc", String.class) );
			ENTRY_VOLSETTER_MAP.put("<AREA>", Volcano.class.getMethod("setArea", String.class) );
			ENTRY_VOLSETTER_MAP.put("<SUMMIT ELEVATION>", Volcano.class.getMethod("setElev", String.class) );
			ENTRY_VOLSETTER_MAP.put("<ADVISORY NUMBER>", Volcano.class.getMethod("setAdvNum", String.class) );
			/*
			ENTRY_VOLSETTER_MAP.put("<REMARKS>", Volcano.class.getMethod("setRemarks", String.class) );			
			ENTRY_VOLSETTER_MAP.put("<INFORMATION SOURCE>", Volcano.class.getMethod("setInfoSource", String.class) );			
			ENTRY_VOLSETTER_MAP.put("<ERUPTION DETAILS>", Volcano.class.getMethod("setErupDetails", String.class) );
			*/
			ENTRY_VOLSETTER_MAP.put("<OBS ASH CLOUD>", Volcano.class.getMethod("setObsFcstAshCloudInfo", String.class) );
			ENTRY_VOLSETTER_MAP.put("<FCST ASH CLOUD +6H>", Volcano.class.getMethod("setObsFcstAshCloudInfo6", String.class) );
			ENTRY_VOLSETTER_MAP.put("<FCST ASH CLOUD +12H>", Volcano.class.getMethod("setObsFcstAshCloudInfo12", String.class) );
			ENTRY_VOLSETTER_MAP.put("<FCST ASH CLOUD +18H>", Volcano.class.getMethod("setObsFcstAshCloudInfo18", String.class) );
			
			//ENTRY_VOLSETTER_MAP.put("<NEXT ADVISORY>", Volcano.class.getMethod("setNextAdv", String.class) );		
			
			//these four keep the user inputs and info from vaa.xml in the same respective fields using Volcano.WORD_SPLITTER	
			ENTRY_VOLSETTER_MAP.put("<REMARKS>", Volcano.class.getMethod("setExtraRemarks", String.class) );			
			ENTRY_VOLSETTER_MAP.put("<INFORMATION SOURCE>", Volcano.class.getMethod("setExtraInfoSource", String.class) );			
			ENTRY_VOLSETTER_MAP.put("<ERUPTION DETAILS>", Volcano.class.getMethod("setExtraErupDetails", String.class) );			
			ENTRY_VOLSETTER_MAP.put("<NEXT ADVISORY>", Volcano.class.getMethod("setExtraNextAdv", String.class) );
			
		}catch(java.lang.NoSuchMethodException e){
			System.out.println("___initializing1: "+e.getMessage());
		}catch(java.lang.SecurityException e){
			System.out.println("___initializing2: "+e.getMessage());
		}
	}
	
	/**
	 * initialize the maps from the parsed vaa.xml;
	 * hard-coded Strings are top elements unlikely 
	 * to change
	 */
	
	public static void initVaaMaps() { //main(String[] args){
		
		List<String> singleList = new ArrayList<String>();
		
		for(String s : VAA_INFO_MAP.keySet()){
			ArrayList<String> ss = VAA_INFO_MAP.get(s);
			
			if("others-fcst".equals(s.trim()))
				initOthersFcstMap(ss);
			
			for(String sss : ss){
				String[] sa = sss.split(SEPERATER);
				
				if("header-information".equals(s.trim()) ) 	
					initStnMap(sa);
				else if("path".equals(s.trim() ))			
					initPathMap(sa);
				else if("format".equals(s.trim()))			
					initProductMap(sa);
				//else if("others-fcst".equals(s.trim()))
				//	initOthersFcstMap(sa);
				else if("wording".equals(s.trim()))			
					initWordingMap(sa);
				else {
					for(String ssss : sa){
						String[] values = ssss.split("=");
						
							if(sa.length > 1 || "path".equalsIgnoreCase(s.trim())){								
							}else{
								if(values.length > 1)
									singleList.add(values[1]);							
							}							
					}	
				}
			}
			
			VaaInfo.VAA_INFO_SINGLE_MAP.put(s.trim(), singleList.toArray(new String[]{}) );
			singleList.clear();					
		}		
		
	}
	
	/**
	 * initialize the stn map
	 * @param String[]: the station id and header array
	 */
	
	public static void initStnMap(String[] s){
		String stn="", vaac="",id="", hdr="";
		for(String ss : s){
			String[] sss = ss.split("=");
			if(sss.length > 1 ){ 
				if ("orig-stn".equals(sss[0].trim())){
					stn = sss[1];
				}else if( "vaac".equals(sss[0].trim())) {
					vaac = sss[1];
				}else if("wmoid".equals(sss[0].trim())){
					id = sss[1];
				}else{
					hdr = sss[1];
				}
			}
		}
		VaaInfo.VAA_INFO_STN_MAP.put(stn+vaac, new String[]{id,hdr});
	}
	
	/**
	 * others-fcst is different in that we need
	 * an array composed of the first part (current)
	 * of each element of the ArrayList<String>
	 * 
	 * @param ArrayList<String> of others-fcst
	 */
	public static void initOthersFcstMap(ArrayList<String> sss){
		
		//LinkedHashSet keeps the order when converting to array
		LinkedHashSet<String>
			current = 	new LinkedHashSet<String>(),
			older = 	new LinkedHashSet<String>(),
			oldest = 	new LinkedHashSet<String>();
		
		for(int i=0; i<sss.size(); i++){
			String[] ss = sss.get(i).split(VaaInfo.SEPERATER);
			for(String value : ss){
				String[] s = value.split("=");
				if (VaaInfo.OTHERSFCST_DIALOG.equals(s[0].trim())){
					current.add(s[1]);
				}else if( VaaInfo.OTHERSFCST_DISPLAY.equals(s[0].trim())) {
					older.add(s[1]);				
				}else{
					oldest.add(s[1]);
				}
			}
		}
		
		VAA_INFO_OTHERSFCST_MAP.put(VaaInfo.OTHERSFCST_DIALOG, current.toArray(new String[]{}));
		VAA_INFO_OTHERSFCST_MAP.put(VaaInfo.OTHERSFCST_DISPLAY, 	older.toArray(new String[]{}));
		VAA_INFO_OTHERSFCST_MAP.put(VaaInfo.OTHERSFCST_TEXT, 	oldest.toArray(new String[]{}));
	}
	
	/**
	 * initialize the Path map
	 * @param String[]: path info
	 */
	public static void initPathMap(String[] s){
		String[] sss = null;
		for(String ss : s){
			sss = ss.split("=");			
		}
		
		if(sss.length > 1 ){
			VaaInfo.VAA_INFO_PATH_MAP.put(sss[0], new String[]{sss[1]});
		}		
	}
	
	/**
	 * initialize the product map
	 * @param String[]: array with location and product of a format
	 */
	public static void initProductMap(String[] s){
		String loc="", prod="", entry="";
		for(String ss : s){
			String[] sss = ss.split("=");
			if(sss.length > 1){
				if("location".equals(sss[0].trim())){
					loc = sss[1];
				}else if("product".equals(sss[0].trim() )){
					prod = sss[1];
				}else{
					entry = sss[1];
				}
			}
		}	
		
		VaaInfo.ProductInfo.setLocProdEntry(loc, prod, entry);
	}
	
	/**
	 * initialize wording map
	 * @param String[]: array with wording tag, new/old wording
	 */
	public static void initWordingMap(String[] s){
		String tag="", neww="", old="";
		for(String ss : s){
			String[] sss = ss.split("=");
			if(sss.length > 1){
				if("tag".equals(sss[0].trim())){
					tag = sss[1];
				}else if("new-wording".equals(sss[0].trim())){
					neww = sss[1];
				}else{
					old = sss[1];
				}
			}
		}
		
		VaaInfo.VAA_INFO_WORDING_MAP.put(tag, new String[]{neww, old});
	}
	
	/**
	 * this method parses the vaa.xml file using DOM,
	 * then puts the info into VAA_INFO_MAP
	 */
	public static void parseVaaFile(){
		
		Document doc = null;
		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		
		try {
			  DocumentBuilder builder = factory.newDocumentBuilder();
			  File vaaFile = PgenStaticDataProvider.getProvider().getStaticFile( 
					  PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "vaa.xml" );
			  
			  doc = builder.parse( vaaFile.getAbsoluteFile() );
		} catch (Exception e) { 
			System.out.println("-----------"+e.getMessage());
		} 
		
		NodeList nlist = doc.getElementsByTagNameNS("*", "*"); //doc.getElementsByTagName("PATH");		
		
		for(int i=0;  i<nlist.getLength(); i++){
			
			Node nElem = nlist.item(i);//each element, like: PATH, hearder-information, etc
			String elemName = nElem.getNodeName().trim();
			
			NamedNodeMap nnMap = nElem.getAttributes();
			
			ArrayList<String> listOld = null;
			
			StringBuilder sb = new StringBuilder();
			
			for(int j=0; j<nnMap.getLength(); j++){
				
				Node nAttr = nnMap.item(j);//each attribute of an element, like: LPF_PATH of PATH
				handleAttrValue(sb,nAttr.getNodeName(), nAttr.getNodeValue());				
			
			}
			
			listOld = VAA_INFO_MAP.get(elemName);
			
			if(listOld != null ){				
				listOld.add(getListString(sb));		
					
			}else {				
				ArrayList<String> list = new ArrayList<String>();
				 list.add(getListString(sb));
				VAA_INFO_MAP.put(elemName, list);
			}
			
		}

	}
	
	/**
	 * helper method for parseVaaFile()
	 * @param StringBuilder: for constructing the attributes line
	 * @param String:		 name of the attribute
	 * @param value:		 value of the attribute
	 */	
	public static void handleAttrValue(StringBuilder sb, String name, String value){
		sb.append(name);
		sb.append("=");
		sb.append(value);
		sb.append(SEPERATER);		
	}
	
	/**
	 * helper method for parseVaaFile()	
	 * @param StringBuilder: for constructing the attribute line
	 * @return String: 		 the String without the last word separator
	 */
	public static String getListString(StringBuilder sb){
		
		String s = sb.toString();
		int index = s.lastIndexOf(SEPERATER);		
		
		return index > 0
				? s.substring(0, s.lastIndexOf(SEPERATER))
				: s;	
	}
	
	/**
	 * convert meter to foot with required fraction digits
	 * @param double: 		meter to be converted
	 * @param int:			maximum fraction digits
	 * @return String:		foot of the corresponding meter in string
	 */
	public static String getFootTxtFromMeter(double meter, int maxFracDigits){
		
		NumberFormat nf = NumberFormat.getInstance();
		nf.setMaximumFractionDigits(maxFracDigits);		
		
		return nf.format(meter*3.281).replaceFirst(",", "");
	}
	
	/**
	 * convert foot text to meter
	 * @param String:	foot in string
	 * @return int:		meter in integer
	 */	
	public static int getMeterIntFromFoot(String feet){
		
		int iFeet = -999999;
		
		try{
			iFeet = Integer.parseInt(feet);
		}catch(Exception e){
			iFeet = 0;
		}
		
		return (int)Math.round(iFeet/3.281);
	}
	
	/**
	 * calculate the current GMT date/time string with given format
	 * @param String: 	format like: yyMMdd
	 * @return String: 	current GMT date/time String
	 */
	public static String getDateTime(String tFormat){		
		
		SimpleDateFormat sdf = new SimpleDateFormat(tFormat);
		sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
		
		return sdf.format(new Date());		
	}
	
	/**
	 * validate Lat/Lon input of format NxxxxWxxxxx
	 * @param String: 	lat/lon input
	 * @return boolean: true: valid lat/lon; false: otherwise
	 */
	public static boolean isValidLatLon(String latlon){
		
		if(latlon == null || latlon.length() != 11) 
			return false;
		
		if(	(latlon.charAt(0) != 'n' && latlon.charAt(0) != 'N')
				&& (latlon.charAt(0) != 's' && latlon.charAt(0) != 'S') )
			return false;
		
		if(	(latlon.charAt(5) != 'w' && latlon.charAt(5) != 'W')
				&& (latlon.charAt(5) != 'e' && latlon.charAt(5) != 'E') )
			return false;
		
		String lat = latlon.substring(1, 5), lon = latlon.substring(6);
		int latInt = -999999, lonInt = -999999;
		try{
			latInt = Integer.parseInt(lat);
			lonInt = Integer.parseInt(lon);
		}catch(Exception e){
			return false;
		}
		
		if(latInt < 0 || latInt > 9000 || lonInt < 0 || lonInt > 18000)
			return false;
		
		return true;
	}
	
	/**
	 * validate the elevation: an elevation is valid
	 * if it lower than Mount Everest and higher than
	 * Mariana Trench.
	 * @param String: 	elev in string
	 * @return boolean:	true: valid; false otherwise.
	 */
	public static boolean isValidElev(String elev){
		double elevInt = -999999;
		
		try{
			elevInt = Double.parseDouble(elev);
		}catch(Exception e){
			return false;
		}
		
		/*
		 * 27889 feet: Mount Everest
		 * 35840 feet: Challenger Deep in the Mariana Trench
		 */
		
		if(elevInt < -35840  || elevInt > 27889)
			return false;
		
		return true;
	}
	
	/**
	 * convert lat/lon from string text to float
	 * @param String:  lat or lon to be converted
	 * @param boolean: true: lat; false: lon
	 * @return float:  lat or lon in float
	 */	
	public static Float getLatLonFromTxt(String latlon, boolean isLat){
		
		String ns = latlon.substring(0,1), ew = latlon.substring(5, 6);
		
		float nsDivider = ns.equalsIgnoreCase("N") ? 100 : -100;
		float ewDivider = ew.equalsIgnoreCase("E") ? 100 : -100;
				
		String lat = latlon.substring(1, 5), lon = latlon.substring(6);
		int latInt = -999999, lonInt = -999999;
		
		try{
			latInt = Integer.parseInt(lat);
			lonInt = Integer.parseInt(lon);
		}catch(Exception e){
			System.out.println("-----"+e.getMessage());
		}
		
		return new Float( isLat ? latInt/nsDivider : lonInt/ewDivider );
	}
	
	/**
	 * loop through files with the same Volcano name
	 * to get the latest Adv No of the same Volcano
	 * @param String: 	file format, xml generally
	 * @param String:	volcano name
	 * @return String:  the latest Advisory No
	 */	
	public static String getLatestAdvNo(String vol, String fFormat){
		
		String dir = PgenUtil.getPgenActivityTextProdPath();
		File f = new File(dir);

		//---final for use in the FilenameFilter		
		final String volFilePrefix = vol;
		final String fileExt = fFormat;
		
		//--- get all files qualified		
		File[] files = f.listFiles(new FilenameFilter(){
			
			/**
			 * ONLY accept files like Adams_20100329_1515.xml
			 * corr files use the latest AdvNo, NO need for consideration
			 */
			
			public boolean accept(File dir, String name){
				
				String connector = "_";
				
				if(name==null || name.length()==0 )
					return false;				
				if( ! name.startsWith(volFilePrefix))
					return false;
				if( ! name.endsWith("."+fileExt))
					return false;
				if( ! name.contains(connector))
					return false;
				
				String[] f = name.split("\\."+fileExt)[0].split(connector);
				if( f.length < 3)//La_Palma_20100329_2039.xml
					return false;
				
				StringBuilder sb = new StringBuilder(volFilePrefix);
				sb.append(connector).append(f[f.length-2]);
				sb.append(connector).append(f[f.length-1]);
				sb.append("."+fileExt);
				
				if( ! name.equals(sb.toString()))
					return false;
				
				String pDate = "[0-9]{8}", pTime = "[0-9]{4}";
				
				return Pattern.matches(pDate,f[f.length-2]) 
						&& Pattern.matches(pTime, f[f.length-1]);
			}
		});
		
		//---no such file		
		if(files == null || files.length == 0)
			return "000";
		
		//--- sort the files		
		TreeMap<Date,File> dmap = new TreeMap<Date,File>();		
		
		for(File file : files){
			// get time from: La_Palma_20100329_2039.xml TODO: put in try block ?
			String[] ftime = file.getName().split("\\."+fFormat)[0].split("_");
			Date date = null;
			try{
				date = getTime(ftime[ftime.length-2],ftime[ftime.length-1]);
			}catch(NumberFormatException e){
				System.out.println("---get time of the file failed: "+e.getMessage());
			}
			if(date != null)
				dmap.put(date, file);
		}
		
		//--- the file wanted		
		File latestFile = dmap.get(dmap.lastKey());		
		
		return FILE_EXTENSION_TXT.equals(fileExt) 
				? getAdvNoFrmTxtFile(latestFile)
				: VaaInfo.getLatestAdvNoFrmXMLFile(latestFile);
	}
	
	/**
	 * get java.util.Date from two strings
	 * @param: date: 	yyyyMMdd
	 * @param: hourmin: HHmm
	 */
	public static Date getTime(String date, String hourmin) throws NumberFormatException{		
		
		return new Date(
					Integer.parseInt(date.substring(0,4)),
					Integer.parseInt(date.substring(4,6)),
					Integer.parseInt(date.substring(6)),
					Integer.parseInt(hourmin.substring(0,2)),
					Integer.parseInt(hourmin.substring(2,4))
				);
	}
	
	/**
	 * parse the txt file to get the Advisory No
	 * @param: the text file to be parsed
	 * @return: the advisory no
	 */
	
	public static String getAdvNoFrmTxtFile(File f){
		BufferedReader in = null;
		try{
		   in = new BufferedReader(new FileReader(f));
		}catch(Exception e){
			return "000";
		}
		
		if( in == null )
			return "000";
		
		String line = null, advNo = null;
		
		try{
			while( (line = in.readLine() ) != null ){
				if(line.contains("ADVISORY NR"))
					advNo = line.split("/")[1];
			}
		}catch(Exception e){
			return "000";
		}

		return advNo == null ? "000" : advNo;
	}
	
	/**
	 * returns a string[] with its elements in date/hour format
	 * @param date user input date in dd format
	 * @param time user input time in HHmm format
	 */
	
	public static String[] getFhrTimes(String date, String time){
		
		int  d = 0, t = 0, hour = 0, min = 0;
		boolean wrongDate = false, wrongTime = false;		
		
		//handle day
		if(date == null || date.length() == 0){
			wrongDate = true;		
		} else{					
			try{
				d = Math.abs(Integer.parseInt(date));
			}catch(Exception e){
				wrongDate = true;
			}
			
			if(d == 0 || d > 31) 
				wrongDate = true;
		}		
		
		//handle time		
		if(time == null || time.length() == 0){
			wrongTime = true;
		}else{			
			try{
				t = Math.abs(Integer.parseInt(time));
			}catch(Exception e){
				wrongTime = true;
			}
			
			if(t > 2359) // 0 hour is OK
				wrongTime = true;	
			
			hour = t/100;
			min = t%100;
			
			// t < 100, the 2-digit input treated as hour
			if(hour == 0){
				hour = min;
				min = 0;				
			}
			
			if(hour > 23)// wrong hour
				wrongTime = true;
		}	
		
		SimpleDateFormat sdf = new SimpleDateFormat();
		
		// valid inputs are already treated as GMT
		if(wrongDate || wrongTime)
			sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
		
		Calendar c = Calendar.getInstance();
		
		//take current date/time for invalid inputs
		if(wrongDate || wrongTime){
			d = c.get(Calendar.DAY_OF_MONTH);		
			hour = c.get(Calendar.HOUR_OF_DAY);
			min = c.get(Calendar.MINUTE);
		}
				
		//45-99 min add one hour, but itself NOT used			
		if(min > 44) 
			hour++;
			
		//15-44 minutes rounding to half hour
		sdf.applyPattern( min>14&&min<45 ? "dd/HH30" : "dd/HH00");
		
		c.set(Calendar.DAY_OF_MONTH, d);
		c.set(Calendar.HOUR_OF_DAY, hour);
	
		// prepare for 06,12, and 18 hours		
		Calendar c6 = (Calendar)c.clone(), c12 = (Calendar)c.clone(), c18 = (Calendar) c.clone();	
		c6.add(Calendar.HOUR, 6);	
		c12.add(Calendar.HOUR, 12); 	
		c18.add(Calendar.HOUR, 18); 		
			
		return new String[]{
				sdf.format(c6.getTime())+"Z",
				sdf.format(c12.getTime())+"Z",
				sdf.format(c18.getTime())+"Z"};
	}
	
	/**
	 * The class encapsulating format's Location, Product, 
	 * and Entry with utility methods.
	 */	
	public static class ProductInfo{		
		
		//LinkedHashSet for keeping the insertion order in the returned String[]
		private static final Map<String, LinkedHashSet<String> > LOC_PROD_MAP = new HashMap<String,LinkedHashSet<String> >();
		
		private static final Map<String, ArrayList<String> > PROD_ENTRY_MAP = new HashMap<String, ArrayList<String> >();
		
		/**
		 * method for adding elements in the maps
		 */
		private static void setLocProdEntry(String loc, String prod, String entry){
			
			//--- handle loc with prod
			
			LinkedHashSet<String> prods = LOC_PROD_MAP.get(loc);
			
			if(prods == null){
				LinkedHashSet<String> prodSet = new LinkedHashSet<String>();
				prodSet.add(prod);
				LOC_PROD_MAP.put(loc,prodSet);
			}else{
				prods.add(prod);
			}
			
			//--- handle prod with entry
			
			ArrayList<String> entries = PROD_ENTRY_MAP.get(prod);
			
			if(entries == null){
				ArrayList<String> entryList = new ArrayList<String>();
				entryList.add(entry);
				PROD_ENTRY_MAP.put(prod, entryList);
			}else{
				entries.add(entry);
			}
		}
		
		/**
		 * get all products of a dialog
		 * 
		 * createDlg: TEST/RESUME, maybe BACKUP
		 * editDlg:   NORMAL/END/QUICK/NEAR, maybe watch
		 * 
		 * @param: LOC_EDIT/LOC_CREATE
		 * @return:products of one dialog
		 */
		
		public static String[] getProduct(String loc){
			
			return LOC_PROD_MAP.get(loc).toArray(new String[]{});			
		}
		
		/**
		 * get all words of a product (i.e. TEST, QUICK)
		 *  
		 * @param:	proudct type
		 * @return: all words in a list (has a contains() method for checking)
		 */		
		public static ArrayList<String> getEntry(String prod){
			
			return PROD_ENTRY_MAP.get(prod);
		}		
		
	}
	
	/**
	 * parse the an attribute line into two parts:
	 * 
	 * entryValue[0]: like <VOLCANO>
	 * entryValue[1]: like TEST
	 */	
	public static String[] getVaaEntryParts(String entry){
		String[] entryValue = new String[]{"",""};
		
		int index = -1;			
			
		index = entry.indexOf(">");
		
		if(index >= 0){
			entryValue[0] = entry.substring(0, index+1).trim();
			entryValue[1] = entry.substring(index+1).trim();
		}
				
		return entryValue;
	}
	
	/**
	 * used by VolcanoCreateDlg/VolcanoVaaAttrDlg to set
	 * a Volcano element's fields according to different
	 * prodTypes: TEST/RESUME, NORMAL/END/QUICK/NEAR.
	 */
	public static void setVolcanoFields(Volcano vol, String prodType, boolean fromSelection){
		
		if(vol==null || prodType==null || prodType.length()==0)
			return;
		
		ArrayList<String> entries = ProductInfo.getEntry(prodType);
		if(entries == null)
			return;
		
		//Not necessary any more ?// if( fromSelection ){	setVolFieldsFrmSelect(vol, prodType, entries); return;	}		
		int index = -1;	
		String part = "";
		
		for( String line : entries){
			if( line == null || line.length() == 0)
				continue;//empty line
			
			index = line.indexOf(">");
			part = line.substring(index+1);
			
			if(part == null || part.length() == 0)
				continue;//empty value after tag like: QUICK <AREA>
			
			/*
			 * using java reflection 
			 */
			
			String[] ev =  getVaaEntryParts(line);			
			Method m = ENTRY_VOLSETTER_MAP.get(ev[0]);
				
			try{
				
				if(m != null && ev[1] != null)
					m.invoke(vol, ev[1]);
				
			}catch(Exception e){
				System.out.println("--- java reflection method call failed: "+e.getMessage());
			}
			
			/*
			index = line.indexOf(">");
			part = line.substring(index+1).trim();			
			if(line.contains("<VOLCANO>"))				
				vol.setName(part);			
			if(line.contains("<NUMBER>"))
				vol.setNumber(part);			
			if(line.contains("<LOCATION>"))
				vol.setTxtLoc(part);			
			if(line.contains("<AREA>"))
				vol.setArea(part);			
			if(line.contains("<SUMMIT ELEVATION>"))
				vol.setElev(part);				
			if(line.contains("<ADVISORY NUMBER>"))
				vol.setAdvNum(part);				
			if(line.contains("<REMARKS>"))
				vol.setRemarks(part);
				*/			
		}
		
		
	}
	
	private static void setVolFieldsFrmSelect(Volcano vol, String prodType, ArrayList<String> entries){
		
		//--- all checkings are done from the caller
		
		int index = -1;
		String part = "";
		for( String line : entries){
			if( line == null || line.length() == 0)
				continue;
			
			index = line.indexOf(">");
			part = line.substring(index+1).trim();
			
			//--- END & QUICK
			
			if(line.contains("<INFORMATION SOURCE>"))
				vol.setInfoSource(part);
			
			if(line.contains("<ERUPTION DETAILS>"))
				vol.setErupDetails(part);
			
			//if(line.contains("<OBS ASH DATA/TIME>"))vol.setObsAshDate(part);// legacy is /Z
			
			if(line.contains("<OBS ASH CLOUD>"))
				vol.setObsFcstAshCloudInfo(part);
			
			if(line.contains("<FCST ASH CLOUD +6H>"))
				vol.setObsFcstAshCloudInfo6(part);
			
			if(line.contains("<FCST ASH CLOUD +12H>"))
				vol.setObsFcstAshCloudInfo6(part);
			
			if(line.contains("<FCST ASH CLOUD +18H>"))
				vol.setObsFcstAshCloudInfo6(part);
			
			if(line.contains("<REMARKS>"))
				vol.setRemarks(part);
			
			if(line.contains("<NEXT ADVISORY>"))
				vol.setNextAdv(part);
			
		}
		
	}
	
	
	/**
	 * convert a Volcano to a pgen.file.Volcano
	 * 
	 * @param: Volcano
	 * @return pgen.file.Volcano
	 *  
	 */
	public static gov.noaa.nws.ncep.ui.pgen.file.Volcano getXMLVolFrmDrawableVol(Volcano dVol){	
		
		 return gov.noaa.nws.ncep.ui.pgen.file.ProductConverter.convertVolcano2XML(dVol);
		
	}
	
	/**
	 * the function doing XML to text transforming using xslt;
	 *  
	 * @param DOMSource: pgen.file.Volcano's XML represented by DOMSource
	 * @param File: 	 xslt file
	 * @return
	 */
    
    public static String getTxtFromXML(DOMSource dSource, File xsltF){    	
    	
    	ByteArrayOutputStream baos = new ByteArrayOutputStream();
    	
    	try{    	
            TransformerFactory tf = TransformerFactory.newInstance();
            StreamSource myStylesheetSrc = new StreamSource(xsltF);       

            Transformer t = tf.newTransformer(myStylesheetSrc);

            t.transform(dSource, new StreamResult(baos));                       
           
        }catch(Exception e){           
            System.out.println(e.getMessage());
        }     	
    	
        return new String(baos.toByteArray());
    }
    
    /**
     * transform a pgen.file.Volcano into DOMSource
     * 
     * @param pgen.file.Volcano:  
     * @param String: packageName
     * @return DOMSource
     */

    public static DOMSource getVolcanoDOMSource(gov.noaa.nws.ncep.ui.pgen.file.Volcano fVol, String packageName){

        Document doc = null;       
       
        try{
            JAXBContext jc = JAXBContext.newInstance(packageName);
            Marshaller m = jc.createMarshaller();
           
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            
            doc = db.newDocument();

            m.marshal( fVol, doc );

        }catch(Exception e){
            e.printStackTrace();
        }
           
        return new DOMSource(doc);
       
    }
    
    /** 
     * check if users try to open VAA special products 
     * like TEST/RESUME; if true, a MessageDialog opens
     * informing users  
     * 
     * @param prods ( of pgen.file package )
     * @return boolean: true if the Product nondrawable
     */
    
    public static boolean isNoneDrawableTxt(gov.noaa.nws.ncep.ui.pgen.file.Products prods){
    	boolean flag = false;
    	
    	for(gov.noaa.nws.ncep.ui.pgen.file.Product p : prods.getProduct()){
    		for(gov.noaa.nws.ncep.ui.pgen.file.Layer l : p.getLayer()){
    			if(p.isOnOff() != null && l.isOnOff() != null && p.isOnOff() && l.isOnOff()){
    				gov.noaa.nws.ncep.ui.pgen.file.DrawableElement de = l.getDrawableElement();
    				
    				List<gov.noaa.nws.ncep.ui.pgen.file.Volcano> list = de.getVolcano();
    				for(gov.noaa.nws.ncep.ui.pgen.file.Volcano v : list){ String vp = v.getProduct() == null ? null : v.getProduct().trim();
    					flag = Arrays.asList(ProductInfo.getProduct(LOCS[1])).contains(vp);
    				}
    			}
    		}
    	}
    	
    	return flag;
    }
    
    /**
     * informing users trying to open nondrawable VAA products
     * 
     * @param msg: message for users
     */
    
    public static void openMsgDlg(String msg){
    	org.eclipse.jface.dialogs.MessageDialog confirmDlg = new org.eclipse.jface.dialogs.MessageDialog( 
    			org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
    			"Warning", null, msg,
    			org.eclipse.jface.dialogs.MessageDialog.WARNING, new String[]{"OK"}, 0);
    	confirmDlg.open();
    }

    /**
     * method for getting obs & fcst Ash Cloud info
     * called by VolcanoVaaAttrDlg & AshCloudInfoDlg
     * 
     * @param: FHR: 00/06/12/18
     * @return:String of the Ash Cloud Info
     */    
    public static String getAshCloudInfo(String hour){
		StringBuilder sb = new StringBuilder();
		List<AbstractDrawableComponent> list = null;
		
		//---2010-03-16
		//List<Product> prods = PgenSession.getInstance().getPgenResource().getProducts();
		Product volProd = null;//2010-03-25getVaaProduct();//2010-03-17 null;
		/*for(Product p : prods){
			if("VOLCANO".equals(p.getName()) ){//TmODO: getActiveProduct() or this "Volcano" ?
				volProd = p;
			}
		}*/
		volProd = PgenSession.getInstance().getPgenResource().getActiveProduct();
		List<Layer> lyrList = volProd == null ? null : volProd.getLayers();
		if( ! VaaInfo.VOLCANO_PRODUCT_NAME.equalsIgnoreCase((volProd.getName()) )) return sb.toString();
		/*try{
			list = PgenSession.getInstance().getPgenResource().getActiveLayer().getDrawables();
		}catch(Exception e){ System.out.println(e.getMessage());return sb.toString(); }*/
		
		if(lyrList == null ) return sb.toString();
		
		boolean isObsWithNotSeen = false;
		ArrayList<String> obsWithNotSeen = new ArrayList<String>();
		
		for(Layer lyr : lyrList){			
			list = lyr.getDrawables();
			
			for(AbstractDrawableComponent adc : list){
				if(VaaInfo.PGEN_TYEP_CLOUD.equals(adc.getPgenType()) ){
					Sigmet vac = (Sigmet)adc;//from xml it is a sigmet
					      
					//TODO: handle lineType with Text					
					String fhr = vac.getEditableAttrFreeText();
					String fromLine = PgenUtil.getLatLonStringPrepend( vac.getLinePoints(),vac.getType().contains("Area"));
					
					//---TODO: a temporary solution for moved after selection cloud				
					if(fhr == null) 
						return sb.toString();
					
					if(fhr.substring(0, 3).contains(hour)){
						String txt = getParsedTxt(fhr,fromLine, vac.getType(), Integer.toString((int) (vac.getWidth())));											

						//OBS takes ONLY the first element if NotSeen is present
						if( ! VaaInfo.LAYERS[2].equals(hour )
						    && ! VaaInfo.LAYERS[3].equals(hour )
						    && ! VaaInfo.LAYERS[4].equals(hour )){
							
							obsWithNotSeen.add(txt);
							
							if(vac.getType() != null && vac.getType().contains(VA_NOT_IDENTIFIABLE))
								isObsWithNotSeen = true;							
						}
						
						sb.append(txt);//sb.append(fhr).append("\n").append(fromLine);
					}					
				}
			}
			
			if( isObsWithNotSeen ){
				return obsWithNotSeen.get(0);
			}
		}		
	
		return sb.toString();
	}
    
    //TODO: multiple clouds of same hours need to be the SAME
	// like legacy code; currently, it starts a new line
	/**
	 * helper method for calculating the OBS and FCST Ash cloud info
	 */
	private static String getParsedTxt(String fhr, String fline, String type, String lineWidth){
		
		if(type != null && type.contains(VaaInfo.TYPE_TEXT))
			return getFcstTxtFromTextType(type);
		
		String divider = " - ";
		
		String[] txtFhr = fhr.split(SEPERATER), txtFline = fline.split(SEPERATER);
		
		StringBuilder sb = new StringBuilder();
		
		//---SFC/FL, FLXX/XX line
		
		if( (txtFhr.length > 1) && txtFhr[1].contains("SFC"))
			sb.append(txtFhr[1]).append("/FL");
		else
			sb.append("FL").append(txtFhr[1]).append("/");		
		
		if(txtFhr.length < 3 || txtFhr[2]==null || txtFhr[2].length()==0 || txtFhr[2].equals(" "))
			;
		else
			sb.append(txtFhr[2]);
			
		sb.append(" ");
		
		if ( type.contains("Line")){
			sb.append( lineWidth);
			sb.append( "NM WID LINE BTN ");
		}
		//---points of the ash cloud
		
		for(String s : txtFline){	
			if(s.length() > 6){
				String wORe = s.substring(5,6);
				String replaced = s.replace(wORe, " "+wORe);
				sb.append(replaced).append(divider);
			}
		}
		
		int last = sb.lastIndexOf("-");
		if( (last > 0) && (last < sb.length()) )	sb.delete(last, last+1);
		
		//---FCST text is without MOV
		
		if(fhr.contains("F06") || fhr.contains("F12") || fhr.contains("F18") )			
			return sb.append(" ").toString();
		
		
		//--- last part for Obs info
		
		sb.append("MOV").append(" ");
		
		if(txtFhr.length < 4 || txtFhr[3]==null || txtFhr[3].length()==0 || txtFhr[3].equals(" ") )
			;
		else
			sb.append(txtFhr[3]).append(" ");
		
		if( txtFhr.length < 1 || txtFhr[txtFhr.length-1]==null || txtFhr[txtFhr.length-1].length()==0 || txtFhr[txtFhr.length-1].equals(" ") )
			sb.append("0KT");
		else 
			sb.append(txtFhr[txtFhr.length-1]).append("KT");
		
		sb.append(" ");
		return sb.toString();
	}
	
	/**
	 * calculate the current layer index to the LAYER array	
	 * @return int: index for LAYER array
	 */
	public static int getLayerIdx() {
		Layer lyr = PgenSession.getInstance().getPgenResource().getActiveLayer();
		for(int i=0; i<LAYERS.length; i++){
			if ( LAYERS[i].equalsIgnoreCase( "OBS") ) {
			    if( LAYERS[i].equalsIgnoreCase(lyr.getName()) || lyr.getName().equalsIgnoreCase("F00") ){
					   return i;
				}			    	
			}
			else {
			   if( LAYERS[i].equalsIgnoreCase(lyr.getName()) ){
				   return i;
			   }
			}
		}
		return CURRENT_LAYER_INDEX;
	}

	public static void setLayerIdx(int current_layer_index) {
		CURRENT_LAYER_INDEX = current_layer_index;
	}

	/** 
     * check if the Volcano is of VAA special products 
     * like TEST/RESUME
     * 
     * @param Volcano 
     * @return boolean: true if NonDrawable
     */	
	public static boolean isNonDrawableVol(Volcano v){
		String vp = v.getProduct() == null ? null : v.getProduct().trim();
		return Arrays.asList(ProductInfo.getProduct(LOCS[1])).contains( vp );	
		
	}
	
	/**
	 * parse the txt file to get the Advisory No
	 * @param: the text file to be parsed
	 * @return: the advisory no 
	 */	
	public static String getLatestAdvNoFrmXMLFile(File f){
		String ano = null;
		
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        
        DocumentBuilder db = null;
       
        try{
        	db = dbf.newDocumentBuilder();
        }catch(Exception e){
        	System.out.println("---DocumentBuilder create failed: "+e.getMessage());
        }
        
        Document doc = null;
        
        try{
        	doc = db.parse(f);
        }catch(Exception e){
        	System.out.println("---DocumentBuilder create failed: "+e.getMessage());
        }
       
        NodeList nList = doc.getElementsByTagName("Volcano");
              
        for(int i=0; i<nList.getLength(); i++){
        	Node n = nList.item(i);
        	NamedNodeMap nnMap = n.getAttributes();
        	Node advNo = nnMap.getNamedItem("advNum");
        	if(advNo != null) 
        		ano = advNo.getNodeValue();
        }		
		
		return (ano==null || ano.length()==0)
				? "000" : ano;		
	}
	
	/**
	 * get index for others-fcst dialog/display/text array
	 * 
	 * @param String: display words
	 * @return int:   array index of the display words 
	 */
	public static int getFcstItemIndexFromTxt(String txt){
		int index = 0;
		if(txt == null || txt.length() == 0)
			return index;//default;
		
		String[] values = VaaInfo.VAA_INFO_OTHERSFCST_MAP.get(VaaInfo.OTHERSFCST_DISPLAY);
		
		String keyWord = "ASH";
		
		//all txt should be the same except with {FL}
		for(int i=0; i<values.length; i++){
			if(values[i].equals(txt)){
				index = i;
				break;
			}
			if( ! values[i].equals(txt) && values[i].contains(keyWord) && txt.contains(keyWord) ){
				if( txt.substring( txt.indexOf(keyWord) ).equals( values[i].substring( values[i].indexOf(keyWord)))
						&& txt.indexOf(keyWord) > 0 && values[i].indexOf(keyWord) > 0)//{FL}
				index = i;
			}
		}
		
		return index;		
		
	}
		
	/**
	 * get the Text Product of others-fcst from display words
	 * 
	 * @param String: text with display words of others-fcst
	 * @return Text Product words of others-fcst
	 */
	public static String getFcstTxtFromTextType(String type){
		String empty = "", oneSpace = " ";
		
		if(type == null || type.length() == 0)
			return empty;
		
		//type looks like: Text:::FL222/FL555 ASH DISSIPATING
		String[] words = type.split(SEPERATER);		
		if( words.length < 2)
			return empty;
		
		//displayed text, like: FL222/FL555 ASH DISSIPATING
		String disTxt = words[1];
		
		//NotSeen text: Text:::VA NOT IDENTIFIABLE FROM SATELLITE DATA:::WINDS SFC/FL /KT
		if(disTxt != null && disTxt.contains(VaaInfo.VA_NOT_IDENTIFIABLE)){
			if( words.length < 3) 
				return empty;
			else{
				return words[1] + oneSpace + words[2];
			}
		}
		
		//get the index for text array use 
		int index = getFcstItemIndexFromTxt(disTxt);
		String[] texts = VaaInfo.VAA_INFO_OTHERSFCST_MAP.get(VaaInfo.OTHERSFCST_TEXT);
		if( index < 0 || index > texts.length-1)
			return empty;		
		
		//the text product word
		String txt = texts[index];
		
		if(txt != null){
			
			if ( txt.contains("SFC/FL")) return disTxt;
			
			/* for text products with FL numbers, 
			 * curly braces { } should always exist for the 
			 * inside string to be replaced by real number 
			 */
			int in1 = txt.indexOf('{'), in2 = txt.indexOf('}');
			
			//MessageFormat needs int 0-9 as ArgumentIndex
			String parsableIndex = "0";
			
			/*
			 * curly braces exist, like: {FL} ASH DISSIPATING
			 *replace the inside of { } with real FL numbers. 
			 */
			if(in1>=0 && in2>in1 && in2<=txt.length()-1){
				//substring without '{'				
				StringBuilder sb = new StringBuilder(txt.substring(0,in1));
				
				//FL numbers like: SFC/FL22255				
				sb.append(disTxt.split(oneSpace)[0]);
				//substring without '}': in2+1 OK to be txt.length()
				sb.append(txt.substring(in2+1));
				sb.append(oneSpace);
				
				return sb.toString();
				
			}else{// NO FL number needed
				return txt + oneSpace;
			}
		}
		
		return empty;
	}
}
