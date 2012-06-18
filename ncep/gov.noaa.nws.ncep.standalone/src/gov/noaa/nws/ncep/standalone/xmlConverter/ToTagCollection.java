package gov.noaa.nws.ncep.standalone.xmlConverter;

import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.AvnText;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.MidCloudText;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;

import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

/**
 * ToTagCollection
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/31/2011   137         Q. Zhou     Initial separated from the ToTag
 * 02/08/2011   137         Q. Zhou     Added 500 points check for line and spline.
 * 										Added getLabeledCommon for groups Cave didn't handle. Modified getLabeledSym.
 * 02/23/2011   137         Q. Zhou     Modified getLabeledCommon to add all group situations.
 * 										Added getLabeledFront. Modified getLabeledSym.
 * 										Modified watch on w_type. 
 * 02/28/2011   137         Q. Zhou     Modified line filled
 *  3/24/2011               Q. Zhou     Added parameter category to getSymType(). for HPC
 *  6/8/2011                Q. Zhou     Added cast to de. Combo and symbol cast to SinglePointElement
 * 11/1/2011   137          Q. Zhou     Added displayType and removed outline for Text.
 * 11/10/2011               Q. Zhou     Moved and modified CCF from DE to DECollection here.
 * 03/14/2012  #599         Q. Zhou     To outlook, added Flood to Outlook
 *                                      Fixed Watch on pgenCategory/Type name change; handled anchor, county variety columns.
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class ToTagCollection {
	public static String tagCollection(String vgf, AbstractDrawableComponent adcK, FileWriter fw, int k, int contourTot) throws IOException {
		String pgenName = "";
		pgenName = adcK.getName();
		DrawableElement de;
		
		int contourCount = 0;
		String[] grptypList = {"CLOUD", "TURB", "FRONT", "JETS", "HIGH", "LOW", "OUTLOOK", "LABEL", "TROPICL",
				"STNMDL", "MRFSTN", "HAILOTLK", "TORNOTLK", "WINDOTLK", "TOTL_SVR", "FIREOUTL", "CATG_SVR", 
				"MESO_DSC", "TSTMOLK", "EXT_SVR", "EXT_FIRE", "ISOBARS", "HI_FCST", "LO_FCST", "WHFT","WHM", 
				"WPER", "PROB", "ENH20", "ENH00", "ENH04", "ENH12", "ENH16", "FLOOD"};
		
		/*
		 * if ccf
		 */
		if (pgenName.startsWith("CCFP_SIGMET")) {	
			vgf = "!\n";
			vgf = getCcf(vgf, fw, adcK);
			return vgf;
		}
		
		/*
		 * if jet, get barb total and hash total, then continue iterator 
		 */		 
		int barbTot =0;
		int hashTot = 0;
		int vectCount = 0;
		int hashCount = 0;
		
		if (pgenName.equalsIgnoreCase("Jet")) {	    						    					
			Iterator<DrawableElement> it = adcK.createDEIterator();	
			if (it != null) {
				while ((de = it.next()) != null) {
					if (de.getPgenType().equalsIgnoreCase("Barb")) {
						barbTot++;
					}
					else if (de.getPgenType().equalsIgnoreCase("Hash")) {
						hashTot++;
					}
				}
			}
		}		
		
		/*
		 * get vgf;
		 */	
		Iterator<DrawableElement> iter = adcK.createDEIterator();
		//Iterator iterator = ((DECollection)(adc.get(k))).getComponentIterator();	    					
			
		while ((de = iter.next()) != null) {	   					
			if (pgenName.equalsIgnoreCase("Watch")) {				
				vgf = getWatch(vgf, fw, k, de);
			}
			
			else if (pgenName.equalsIgnoreCase("Cloud") || pgenName.equalsIgnoreCase("Turbulence")) {
				vgf = getCloudTurb(vgf, fw, k, de, pgenName);
			}		
			else if (pgenName.equalsIgnoreCase("labeledSymbol") ) {							
				vgf = getLabeledSym(vgf, fw, k, de);
			}		
			else if (pgenName.equalsIgnoreCase("labeledMark") ) {							
				vgf = getLabeledSym(vgf, fw, k, de);
			}		
			else if (pgenName.equalsIgnoreCase("labeledFront") ) {							
				vgf = getLabeledFront(vgf, fw, k, de);
			}		
			else if (XmlUtil.typeSearch(grptypList, pgenName)) {							
				vgf = getLabeledCommon(vgf, fw, k, de, pgenName);
			}		
						
			else if (pgenName.equalsIgnoreCase("Contours") || pgenName.equalsIgnoreCase("Outlook")) {
				int grpType = 0; 
				int grpNum = 0;
				if (pgenName.equalsIgnoreCase("Contours")) {					
					grpType = 8;	
					grpNum = contourTot + contourCount;
				}
				else {
					grpType = XmlUtil.getOutlookType(adcK.getPgenType());
					grpNum = contourCount;
				}
				
				if (de.getPgenCategory().equalsIgnoreCase("Lines")) {
					contourCount++;
					grpNum++;  //since contourCount++;										
					vgf = "!\n";			
					
					if ((de.getPgenType().equalsIgnoreCase( "LINE_SOLID") || de.getPgenType().startsWith("LINE_DASHED") )) {
						vgf += VgfTags.vg_type + XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
						+ VgfTags.vg_class +"3" //diff
						+ VgfTags.delete +"0"                               
						+ VgfTags.filled + XmlUtil.fill_pattern(((Line)de).getFillPattern().toString(), ((Line)de).isFilled())
						+ VgfTags.closed + (((Line)de).isClosedLine()==true ?1 :0 )  
						+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
						+ VgfTags.version + "0"    	
						+ VgfTags.grptyp + grpType     				+VgfTags.grpnum + grpNum
						+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))     
						+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
						+ VgfTags.recsz + "0"
						+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                  
						+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
						+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
						+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
						+ VgfTags.numpts +de.getPoints().size()       + VgfTags.lintyp +XmlUtil.getLineType(de.getPgenType()) 
						+ VgfTags.lthw +"0"                           + VgfTags.width + (int)de.getLineWidth() 
						+ VgfTags.lwhw +"0"                           + VgfTags.latlon + XmlUtil.getPoints(de) +"\n";
					}
					else {
						vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
						+ VgfTags.vg_class +"3" 
						+ VgfTags.delete +"0"                                 
						+ VgfTags.filled + XmlUtil.fill_pattern(((Line)de).getFillPattern().toString(), ((Line)de).isFilled())
						+ VgfTags.closed + (((Line)de).isClosedLine()==true ?1 :0 )    
						+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
						+ VgfTags.version + "0" 
						+ VgfTags.grptyp + grpType    					+ VgfTags.grpnum + grpNum 
						+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
						+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
						+ VgfTags.recsz + "0"
						+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
						+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
						+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
						+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
						+ VgfTags.numpts +de.getPoints().size()         
						+ VgfTags.spltyp +XmlUtil.getSPLineType(de.getPgenType()) 
						+ VgfTags.splstr +"1" 	+VgfTags.spldir +"1"    + VgfTags.splsiz +de.getSizeScale() 
						+ VgfTags.splwid +(int)de.getLineWidth()		+ VgfTags.latlon + XmlUtil.getPoints(de) +"\n";
					}
					
					fw.write(vgf);	
				}	
				else if (de.getPgenCategory().equalsIgnoreCase("Symbol")) {
					vgf = "!\n";
					contourCount++;
					grpNum++;  //since contourCount++;
					
					vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 			
					+ VgfTags.vg_class +"4" 						+ VgfTags.delete +"0"
					+ VgfTags.filled + "0"							+ VgfTags.closed + "0"  						
					+ VgfTags.smooth + "0" 							+ VgfTags.version + "0" 							
					+ VgfTags.grptyp + grpType         				+ VgfTags.grpnum + grpNum  
					+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
					+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
					+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
					+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
					+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
					+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
					+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
					+ VgfTags.numsym +de.getPoints().size()               
					+ VgfTags.width + ( ((SinglePointElement)de).isClear() ?(800+(int)de.getLineWidth()) :(int)de.getLineWidth() )
					+ VgfTags.size +de.getSizeScale()                     + VgfTags.ityp +"0"            
					+ VgfTags.code + XmlUtil.getSymType(de.getPgenType()) 
					+ VgfTags.latlon + XmlUtil.getPoints(de)    
					+ VgfTags.offset_xy + "0, 0" +"\n"; //+ ((Symbol)de).getXOffset() +"," + ((Symbol)de).getYOffset() +"\n";
					
					fw.write(vgf);
				}
				else if (de.getPgenCategory().equalsIgnoreCase("Text")) {
					vgf = "!\n";
					
					double rotn = 0;
					if (((Text)de).getRotationRelativity().toString().equalsIgnoreCase("NORTH_RELATIVE"))
						rotn = ((Text)de).getRotation() + 1000;
					else
						rotn = ((Text)de).getRotation();
					
					String textString = "";
					String[] text = ((Text)de).getString(); //general text
					if (text != null && text.length !=0) {
						for (int o=0; o<text.length; o++) {
							textString += ((Text)de).getString()[o] + "$$";	
						}
						textString = textString.substring(0, textString.length()-2);
					}
					
					vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
					+ VgfTags.vg_class +"5" 					+ VgfTags.delete +"0" 
					+ VgfTags.filled + "0"						+ VgfTags.closed + "0"  
					+ VgfTags.smooth + "0"						+ VgfTags.version + "0" 							
					+ VgfTags.grptyp + grpType         			+ VgfTags.grpnum + grpNum 							
					+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       						  
					+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))			
					+ VgfTags.recsz + "0"
					+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]           
					+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
					+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
					+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
					+ VgfTags.rotn + rotn                    
					+ VgfTags.sztext + XmlUtil.getSztext(Float.toString(((Text)de).getFontSize()) )
					+ VgfTags.sptxtyp + XmlUtil.getSPTextType(de.getPgenType(), "", ((Text)de).maskText(), ((Text)de).getDisplayType().toString()) 
					+ VgfTags.turbsym + "0" //XmlUtil.getTurbSymType(symPattern )  	  
					+ VgfTags.itxfn   + XmlUtil.getFontStyle( ((Text)de).getFontName(), ((Text)de).getStyle().toString()) 
					+ VgfTags.ithw + "2"               					
					+ VgfTags.iwidth + (int)de.getLineWidth() 
					+ VgfTags.txtcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))  
					+ VgfTags.lincol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
					+ VgfTags.filcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
					+ VgfTags.ialign + XmlUtil.getIalign(((Text)de).getJustification().toString() )
					+ VgfTags.lat + XmlUtil.getPointLats(de)            
					+ VgfTags.lon + XmlUtil.getPointLons(de)  
					+ VgfTags.offset_x + ((Text)de).getXOffset()                
					+ VgfTags.offset_y + ((Text)de).getYOffset() 
					+ VgfTags.text +textString  +"\n";
					
					fw.write(vgf);
				}
				
			}
			
						
			else if (pgenName.equalsIgnoreCase("Jet")) {				
				
			if (de.getPgenCategory().equalsIgnoreCase("Lines") ) {				
				vgf = "!\n";
				
				vgf += VgfTags.vg_type + 37 				+ VgfTags.vg_class +"15" 
				+ VgfTags.delete +"0"                               
				+ VgfTags.filled +(((Line)de).isFilled()==true ?1 :0 )
				+ VgfTags.closed +(((Line)de).isClosedLine()==true ?1 :0 )  
				+ VgfTags.smooth +((Line)de).getSmoothFactor() 
				+ VgfTags.version + "0" 	
				+ VgfTags.grptyp +"0"     					+ VgfTags.grpnum +"0"  
				+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))      
				+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))
				+ VgfTags.recsz + "0"									
				+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]
				+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 					
				+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
				+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]   
				+ VgfTags.splcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
				+ VgfTags.numpts +de.getPoints().size()              
				+ VgfTags.spltyp +XmlUtil.getSPLineType(de.getPgenType()) 
				+ VgfTags.splstr +"1" 
				+ VgfTags.spldir +"0"          
				+ VgfTags.splsiz +de.getSizeScale() 
				+ VgfTags.splwid +(int)de.getLineWidth()			 
				+ VgfTags.latlon + XmlUtil.getPoints(de) ;
			//VgfTags.spldir +"0" in jet;  VgfTags.spldir +"1" in spl	
				fw.write(vgf);
			}		
			
			else if (de.getPgenCategory().equalsIgnoreCase("Vector")) {
				vgf = ""; //continue for 1 record
				String width = ""; //only for barb. Used on Hash is wrong
				if (((Vector)de).isClear())
					width = 80 + String.valueOf((int)de.getLineWidth());
				else 
					width = String.valueOf((int)de.getLineWidth());
					
				if (de.getPgenType().equalsIgnoreCase("Barb")) {
					vectCount ++;
					vgf += (vectCount==1 ? (VgfTags.nbarb + barbTot) : "") 
					+ VgfTags.jet_barb_+vectCount+">" 
					+ XmlUtil.getColorTag(XmlUtil.getColorMaj(de)) +"," +"1" +"," 
					+ width +"," + de.getSizeScale() +"," 
					+ 114 +"," //XmlUtil.getWndType(de.getPgenType(), de.isClear()) +","
					+ ((Vector)de).getArrowHeadSize() +","+  ((Vector)de).getSpeed() +"," +((Vector)de).getDirection() +"," 
					+ XmlUtil.getPoints(de);
					 
				}
				else if (de.getPgenType().equalsIgnoreCase("Hash")) {
					hashCount ++;
					vgf += (hashCount==1 ? (VgfTags.nhash + hashTot) :"")
					+ VgfTags.jet_hash_ +hashCount+">" 
					+ XmlUtil.getColorTag(XmlUtil.getColorMaj(de)) +","+ "1" +","
					+ (int)de.getLineWidth()  +","+ de.getSizeScale() +","
					+ 1 +"," //XmlUtil.getWndType(de.getPgenType(), de.isClear()) +","
					+ ((Vector)de).getArrowHeadSize() + "," + ((Vector)de).getSpeed() +"," 
					+ (180-((Vector)de).getDirection()) +"," + XmlUtil.getPoints(de);
				}	    								  
				
				fw.write(vgf);
			}
			else if (de.getPgenCategory().equalsIgnoreCase("Text")) {
				vgf = "";
				
				int offsetY = -2;
				String textString = "";			
				String[] text = ((Text)de).getString();
				
				if (text != null && text.length !=0) {
				//text[0] is like FL300; text[1] is like 400/200, need to be 200/400
					textString += text[0];
					if (text.length >1) {
						String[] text1 = text[1].split("/");
						if (text1.length >1)
							textString += "$$" + text1[1] + "/" + text1[0] ;
						else
							textString += "$$" + "/" + text1[0] ;
						offsetY = -3;
					}
				}
				
				double rotn = 0.0;
				rotn = 1000 + ((Text)de).getRotation(); //can't choose RELATIVE
//				if (de.getRotationRelativity().equals("NORTH_RELATIVE"))
//					rotn = 1000 + de.getRotation();
//				else 
//					rotn = de.getRotation();
								
				int txType = XmlUtil.getSPTextType(de.getPgenType(), "", ((Text)de).maskText(), ((Text)de).getDisplayType().toString());
//				
				vgf += VgfTags.jet_text_ +vectCount +">" 
				+ XmlUtil.getColorTag(XmlUtil.getColorMaj(de)) + "," 
				+ rotn + "," + XmlUtil.getSztext(Float.toString(((Text)de).getFontSize())) + "," //"1.1"  
				+ txType + "," //"5", XmlUtil.getSPTextType(), it is general text
				+"0,1,1,1," //width, font, align
				+ XmlUtil.getColorTag(XmlUtil.getColorMaj(de)) +","+ XmlUtil.getColorTag(XmlUtil.getColorMaj(de)) +","
				+ XmlUtil.getColorTag(XmlUtil.getColorMaj(de)) +","+"0," 
				+ XmlUtil.getPointLats(de) + ","  + XmlUtil.getPointLons(de) + "," 
				+ ((Text)de).getXOffset()  + ","  + offsetY  + "," + textString ;
				
				// write last "\n"
				if (vectCount == barbTot)
				vgf += "\n";
					
				fw.write(vgf);
			}    			
		}
			//}
		}
		
	    return vgf;
	}

	private static String getLabeledSym(String vgf, FileWriter fw, int k,
			DrawableElement de) throws IOException {
		if (de.getPgenCategory().equalsIgnoreCase("Symbol")
				|| de.getPgenCategory().equalsIgnoreCase("Marker")
				|| de.getPgenCategory().equalsIgnoreCase("Combo")) {
			vgf = "!\n";
		
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"4" 				+ VgfTags.delete +"0"                               
			+ VgfTags.filled + "0"					+ VgfTags.closed + "0"  
			+ VgfTags.smooth + "0" 					+ VgfTags.version + "0" 
			+ VgfTags.grptyp + 8					+ VgfTags.grpnum + (k+1)  
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
			+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.numsym +de.getPoints().size()               
			+ VgfTags.width + ( ((SinglePointElement)de).isClear() ?(800+(int)de.getLineWidth()) :(int)de.getLineWidth() )
			+ VgfTags.size +de.getSizeScale()                     + VgfTags.ityp +"0"            
			+ VgfTags.code + XmlUtil.getSymType(de.getPgenType()) 
			+ VgfTags.latlon + XmlUtil.getPoints(de)    
			+ VgfTags.offset_xy + "0, 0" +"\n"; //+ ((Symbol)de).getXOffset() +"," + ((Symbol)de).getYOffset() +"\n";
			
			fw.write(vgf);
		}
		
		else if (de.getPgenCategory().equalsIgnoreCase("Text")) {
			vgf = "!\n";
			
			double rotn = 0;
			if (((Text)de).getRotationRelativity().toString().equalsIgnoreCase("NORTH_RELATIVE"))
				rotn = ((Text)de).getRotation() + 1000;
			else
				rotn = ((Text)de).getRotation();
			
			String textString = "";
			String[] text = ((Text)de).getString();
			if (text != null && text.length !=0) {
				for (int o=0; o<text.length; o++) {
					textString += ((Text)de).getString()[o] + "$$";	
				}
				textString = textString.substring(0, textString.length()-2);
			}
		
			//System.out.println(XmlUtil.getSPTextType(de.getPgenType(), "", de.maskText(), de.outlineText()));
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"5" 				+ VgfTags.delete +"0"                               
			+ VgfTags.filled + "0"					+ VgfTags.closed + "0"  
			+ VgfTags.smooth + "0"					+ VgfTags.version + "0" 							
			+ VgfTags.grptyp + 8					+ VgfTags.grpnum + (k+1)							
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       						  
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))			
			+ VgfTags.recsz + "0"
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]           
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.rotn + rotn                   
			+ VgfTags.sztext + XmlUtil.getSztext(Float.toString(((Text)de).getFontSize()) )
			+ VgfTags.sptxtyp + XmlUtil.getSPTextType(de.getPgenType(), "", ((Text)de).maskText(), ((Text)de).getDisplayType().toString()) 
			+ VgfTags.turbsym + "0" //XmlUtil.getTurbSymType(symPattern )  	  
			+ VgfTags.itxfn   + XmlUtil.getFontStyle( ((Text)de).getFontName(), ((Text)de).getStyle().toString()) 
			+ VgfTags.ithw + "2"               					
			+ VgfTags.iwidth + (int)de.getLineWidth() 
			+ VgfTags.txtcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))  
			+ VgfTags.lincol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.filcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.ialign + XmlUtil.getIalign(((Text)de).getJustification().toString() )
			+ VgfTags.lat + XmlUtil.getPointLats(de)            
			+ VgfTags.lon + XmlUtil.getPointLons(de)  
			+ VgfTags.offset_x + ((Text)de).getXOffset()                
			+ VgfTags.offset_y + ((Text)de).getYOffset() 
			+ VgfTags.text +textString  +"\n";
			
			fw.write(vgf);
		}
		return vgf;
	}

	private static String getLabeledFront(String vgf, FileWriter fw, int k,
			DrawableElement de) throws IOException {
		if (de.getPgenCategory().equalsIgnoreCase("Front")) {
			vgf = "!\n";
		
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"1" 					+ VgfTags.delete +"0"                               
			+ VgfTags.filled +(((Line)de).isFilled()==true ?1 :0 )
			+ VgfTags.closed +(((Line)de).isClosedLine()==true ?1 :0 )  
			+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
			+ VgfTags.version + "0" 
			+ VgfTags.grptyp + "3"						+ VgfTags.grpnum + (k+1)  
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
			+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.numpts + de.getPoints().size()               
			+ VgfTags.fcode +  XmlUtil.getFrontType(de.getPgenType(), (int)de.getLineWidth()) 
			+ VgfTags.fpipsz + Math.round(de.getSizeScale() *100 )			      
			+ VgfTags.fpipst + "1"     
			+ VgfTags.fpipdr + "1" 
			+ VgfTags.fwidth + (int)de.getLineWidth()              
			+ VgfTags.frtlbl + "STJ" 
			+ VgfTags.latlon + XmlUtil.getPoints(de) + "\n";
			
			fw.write(vgf);
		}
		else if (de.getPgenCategory().equalsIgnoreCase("Text")) {
			vgf = "!\n";
			
			double rotn = 0;
			if (((Text)de).getRotationRelativity().toString().equalsIgnoreCase("NORTH_RELATIVE"))
				rotn = ((Text)de).getRotation() + 1000;
			else
				rotn = ((Text)de).getRotation();
			
			String textString = "";
			String[] text = ((Text)de).getString();
			if (text != null && text.length !=0) {
				for (int o=0; o<text.length; o++) {
					textString += ((Text)de).getString()[o] + "$$";	
				}
				textString = textString.substring(0, textString.length()-2);
			}
		
			//System.out.println(XmlUtil.getSPTextType(de.getPgenType(), "", de.maskText(), de.outlineText()));
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"5" 				+ VgfTags.delete +"0"                               
			+ VgfTags.filled + "0"					+ VgfTags.closed + "0"  
			+ VgfTags.smooth + "0"					+ VgfTags.version + "0" 							
			+ VgfTags.grptyp + 3					+ VgfTags.grpnum + (k+1)							
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       						  
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))			
			+ VgfTags.recsz + "0"
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]           
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.rotn + rotn                   
			+ VgfTags.sztext + XmlUtil.getSztext(Float.toString(((Text)de).getFontSize()) )
			+ VgfTags.sptxtyp + XmlUtil.getSPTextType(de.getPgenType(), "", ((Text)de).maskText(), ((Text)de).getDisplayType().toString()) 
			+ VgfTags.turbsym + "0" //XmlUtil.getTurbSymType(symPattern )  	  
			+ VgfTags.itxfn   + XmlUtil.getFontStyle( ((Text)de).getFontName(), ((Text)de).getStyle().toString()) 
			+ VgfTags.ithw + "2"               					
			+ VgfTags.iwidth + (int)de.getLineWidth() 
			+ VgfTags.txtcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))  
			+ VgfTags.lincol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.filcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.ialign + XmlUtil.getIalign(((Text)de).getJustification().toString() )
			+ VgfTags.lat + XmlUtil.getPointLats(de)            
			+ VgfTags.lon + XmlUtil.getPointLons(de)  
			+ VgfTags.offset_x + ((Text)de).getXOffset()                
			+ VgfTags.offset_y + ((Text)de).getYOffset() 
			+ VgfTags.text +textString  +"\n";
			
			fw.write(vgf);
		}
		return vgf;
	}
	
	private static String getLabeledCommon(String vgf, FileWriter fw, int k,
			DrawableElement de, String pgenName) throws IOException {
		
		if (de.getPgenCategory().equalsIgnoreCase("Lines")) {
			if (de.getPgenType().startsWith("LINE_DASHED") || de.getPgenType().startsWith("LINE_SOLID")) {
				vgf = "!\n";
			
				vgf += VgfTags.vg_type + XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
				+ VgfTags.vg_class +"3" 							
				+ VgfTags.delete +"0"                               
				+ VgfTags.filled + XmlUtil.fill_pattern(((Line)de).getFillPattern().toString(), ((Line)de).isFilled())
				+ VgfTags.closed +(((Line)de).isClosedLine()==true ?1 :0 )  
				+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
				+ VgfTags.version + "0" 
				+ VgfTags.grptyp + XmlUtil.getOutlookType(pgenName)	+ VgfTags.grpnum + (k+1) 
				+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
				+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
				+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
				+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
				+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
				+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
				+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
				+ VgfTags.numpts +de.getPoints().size()              + VgfTags.lintyp +XmlUtil.getLineType(de.getPgenType()) 
				+ VgfTags.lthw +"0"                                  + VgfTags.width + (int)de.getLineWidth() 
				+ VgfTags.lwhw +"0"                                  + VgfTags.latlon + XmlUtil.getPoints(de) +"\n";

				fw.write(vgf);
			}
			else  {
				vgf = "!\n";
			
				vgf += VgfTags.vg_type + XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
				+ VgfTags.vg_class +"3" 
				+ VgfTags.delete +"0"                               
				+ VgfTags.filled + XmlUtil.fill_pattern(((Line)de).getFillPattern().toString(), ((Line)de).isFilled())
				+ VgfTags.closed +(((Line)de).isClosedLine()==true ?1 :0 )  
				+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
				+ VgfTags.version + "0" 
				+ VgfTags.grptyp + XmlUtil.getOutlookType(pgenName)	+ VgfTags.grpnum + (k+1)  
				+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
				+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
				+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
				+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
				+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
				+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
				+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
				+ VgfTags.numpts +de.getPoints().size()       		+ VgfTags.spltyp +XmlUtil.getSPLineType(de.getPgenType()) 
				+ VgfTags.splstr +"1" 	+VgfTags.spldir +"1"        + VgfTags.splsiz +de.getSizeScale() 
				+ VgfTags.splwid +(int)de.getLineWidth()			+ VgfTags.latlon + XmlUtil.getPoints(de) +"\n";
			
				fw.write(vgf);
			}
		}
		if (de.getPgenCategory().equalsIgnoreCase("Front")) {
			vgf = "!\n";
		
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"1" 
			+ VgfTags.delete +"0"                               
			+ VgfTags.filled +(((Line)de).isFilled()==true ?1 :0 )
			+ VgfTags.closed +(((Line)de).isClosedLine()==true ?1 :0 )  
			+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
			+ VgfTags.version + "0" 
			+ VgfTags.grptyp + XmlUtil.getOutlookType(pgenName)	+ VgfTags.grpnum + (k+1)  
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
			+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.numpts + de.getPoints().size()               
			+ VgfTags.fcode +  XmlUtil.getFrontType(de.getPgenType(), (int)de.getLineWidth()) 
			+ VgfTags.fpipsz + Math.round(de.getSizeScale() *100 )			      
			+ VgfTags.fpipst + "1"     
			+ VgfTags.fpipdr + "1" 
			+ VgfTags.fwidth + (int)de.getLineWidth()              
			+ VgfTags.frtlbl + "STJ" 
			+ VgfTags.latlon + XmlUtil.getPoints(de) + "\n";
			
			fw.write(vgf);
		}
		else if (de.getPgenCategory().equalsIgnoreCase("Symbol")
				|| de.getPgenCategory().equalsIgnoreCase("Marker")
				|| de.getPgenCategory().equalsIgnoreCase("Combo")) {
			vgf = "!\n";
		
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"4" 				+ VgfTags.delete +"0"
			+ VgfTags.filled + "0"					+ VgfTags.closed + "0"  
			+ VgfTags.smooth + "0" 					+ VgfTags.version + "0" 
			+ VgfTags.grptyp + XmlUtil.getOutlookType(pgenName)	+ VgfTags.grpnum + (k+1)  
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
			+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.numsym +de.getPoints().size()               
			+ VgfTags.width + ( ((SinglePointElement)de).isClear() ?(800+(int)de.getLineWidth()) :(int)de.getLineWidth() )
			+ VgfTags.size +de.getSizeScale()                     + VgfTags.ityp +"0"            
			+ VgfTags.code + XmlUtil.getSymType(de.getPgenType()) 
			+ VgfTags.latlon + XmlUtil.getPoints(de)    
			+ VgfTags.offset_xy + "0, 0" +"\n"; //((Symbol)de).getXOffset() +"," + ((Symbol)de).getYOffset() +"\n";
			
			fw.write(vgf);
		}
		else if (de.getPgenCategory().equalsIgnoreCase("Text")) {
			vgf = "!\n";
			
			double rotn = 0;
			if (((Text)de).getRotationRelativity().toString().equalsIgnoreCase("NORTH_RELATIVE"))
				rotn = ((Text)de).getRotation() + 1000;
			else
				rotn = ((Text)de).getRotation();
			
			String textString = "";
			String[] text = ((Text)de).getString();
			if (text != null && text.length !=0) {
				for (int o=0; o<text.length; o++) {
					textString += ((Text)de).getString()[o] + "$$";	
				}
				textString = textString.substring(0, textString.length()-2);
			}
		
			//System.out.println(XmlUtil.getSPTextType(de.getPgenType(), "", de.maskText(), de.outlineText()));
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"5" 				+ VgfTags.delete + "0"                               
			+ VgfTags.filled + "0"					+ VgfTags.closed + "0"  
			+ VgfTags.smooth + "0"					+ VgfTags.version + "0" 							
			+ VgfTags.grptyp + XmlUtil.getOutlookType(pgenName)
			+ VgfTags.grpnum + (k+1)							
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       						  
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))			
			+ VgfTags.recsz + "0"
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]           
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.rotn + rotn                   
			+ VgfTags.sztext + XmlUtil.getSztext(Float.toString(((Text)de).getFontSize()) )
			+ VgfTags.sptxtyp + XmlUtil.getSPTextType(de.getPgenType(), "", ((Text)de).maskText(), ((Text)de).getDisplayType().toString()) 
			+ VgfTags.turbsym + "0" //XmlUtil.getTurbSymType(symPattern )  	  
			+ VgfTags.itxfn   + XmlUtil.getFontStyle( ((Text)de).getFontName(), ((Text)de).getStyle().toString()) 
			+ VgfTags.ithw + "2"               					
			+ VgfTags.iwidth + (int)de.getLineWidth() 
			+ VgfTags.txtcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))  
			+ VgfTags.lincol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.filcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.ialign + XmlUtil.getIalign(((Text)de).getJustification().toString() )
			+ VgfTags.lat + XmlUtil.getPointLats(de)            
			+ VgfTags.lon + XmlUtil.getPointLons(de)  
			+ VgfTags.offset_x + ((Text)de).getXOffset()                
			+ VgfTags.offset_y + ((Text)de).getYOffset() 
			+ VgfTags.text +textString  +"\n";
			
			fw.write(vgf);
		}
		
		return vgf;
	}

	private static String getCloudTurb(String vgf, FileWriter fw, int k,
			DrawableElement de, String pgenName ) throws IOException {
		int grpType = 0; 
		
		if (pgenName.equalsIgnoreCase("Cloud")) {					
			grpType = 1;	
		}
		else {
			grpType = 2; //XmlUtil.getOutlookType(adcK.getPgenType());
		}
		
		if (de.getPgenCategory().equalsIgnoreCase("Lines")) {										
			vgf = "!\n";			
			
			if (!de.getPgenType().startsWith("LINE_DASHED")) {
				vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
				+ VgfTags.vg_class +"3" 
				+ VgfTags.delete +"0"                                 
				+ VgfTags.filled + XmlUtil.fill_pattern(((Line)de).getFillPattern().toString(), ((Line)de).isFilled())
				+ VgfTags.closed + (((Line)de).isClosedLine()==true ?1 :0 )    
				+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
				+ VgfTags.version + "0" 
				+ VgfTags.grptyp + grpType    					+ VgfTags.grpnum + (k+1) 
				+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))      
				+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
				+ VgfTags.recsz + "0"
				+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
				+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
				+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
				+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
				+ VgfTags.numpts + de.getPoints().size()         
				+ VgfTags.spltyp + XmlUtil.getSPLineType(de.getPgenType()) 
				+ VgfTags.splstr +"1" 	+VgfTags.spldir +"1"    + VgfTags.splsiz +de.getSizeScale() 
				+ VgfTags.splwid +(int)de.getLineWidth()		+ VgfTags.latlon + XmlUtil.getPoints(de) +"\n";
			}
			else {
				vgf += VgfTags.vg_type + XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
				+ VgfTags.vg_class +"3" //diff
				+ VgfTags.delete +"0"                               
				+ VgfTags.filled + XmlUtil.fill_pattern(((Line)de).getFillPattern().toString(), ((Line)de).isFilled())
				+ VgfTags.closed + (((Line)de).isClosedLine()==true ?1 :0 )  
				+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
				+ VgfTags.version + "0"    	
				+ VgfTags.grptyp + grpType     				+VgfTags.grpnum + (k+1)
				+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))     
				+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de)) 
				+ VgfTags.recsz + "0"
				+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                  
				+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
				+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
				+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
				+ VgfTags.numpts +de.getPoints().size()       + VgfTags.lintyp +XmlUtil.getLineType(de.getPgenType()) 
				+ VgfTags.lthw +"0"                           + VgfTags.width + (int)de.getLineWidth() 
				+ VgfTags.lwhw +"0"                           + VgfTags.latlon + XmlUtil.getPoints(de) +"\n";
			}
			
			fw.write(vgf);
		}	
		else if (de.getPgenCategory().equalsIgnoreCase("Text")) {
			vgf = "!\n";
			String textString = "";
			
			double rotn = 0;
			if (((Text)de).getRotationRelativity().toString().equalsIgnoreCase("NORTH_RELATIVE"))
				rotn = ((Text)de).getRotation() + 1000;
			else
				rotn = ((Text)de).getRotation();
			
			String avnTextType = "";
			if (de.getPgenType().equalsIgnoreCase("AVIATION_TEXT")) {
				if (((AvnText)de).getBottomValue().equalsIgnoreCase("XXX"))
					textString =  ((AvnText)de).getTopValue();
				else
					textString =  ((AvnText)de).getTopValue() +"/" + ((AvnText)de).getBottomValue();	// $$ also works!
				
				if (((AvnText)de).getAvnTextType() != null)
					avnTextType = ((AvnText)de).getAvnTextType().toString();
			}
			else if (de.getPgenType().equalsIgnoreCase("MID_LEVEL_CLOUD")) {
				
				textString = XmlUtil.getMidLevSplit( ((MidCloudText)de).getCloudTypes(), "\\|", ";")   + "|" 
				+ XmlUtil.getMidLevSplit( ((MidCloudText)de).getCloudAmounts(), "\\|", ";")	   + "|" 
				+ XmlUtil.getTurbSymType( ((MidCloudText)de).getIcingPattern()  )   	   + "|" 
				+ ((MidCloudText)de).getIcingLevels()	   	+ "|" 
				+ XmlUtil.getTurbSymType( ((MidCloudText)de).getTurbulencePattern() )	   + "|" 
				+ ((MidCloudText)de).getTurbulenceLevels()  + "|" 	   					
				+ XmlUtil.getMidLevSplit( ((MidCloudText)de).getTstormTypes() , "\\|", ";")      + "|" 
				+ ((MidCloudText)de).getTstormLevels(); 	   	//is not endding with |
					    					
			}
			else if (de.getPgenType().equalsIgnoreCase("General Text")) {
				String[] text = ((Text)de).getString();
				if (text != null && text.length !=0) {
					for (int o=0; o<text.length; o++) {
						textString += ((Text)de).getString()[o] + "$$";	//if text o=="", "\n"+"$$"
					}
					textString = textString.substring(0, textString.length()-2);  	//no end $$	    				
				}
			}
					
			
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"5" 				+ VgfTags.delete +"0"                               
			+ VgfTags.filled + "0"					+ VgfTags.closed + "0"  
			+ VgfTags.smooth + "0"					+ VgfTags.version + "0" 							
			+ VgfTags.grptyp + grpType         		+ VgfTags.grpnum + (k+1) 							
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))       						  
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))			
			+ VgfTags.recsz + "0"
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]           
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.rotn + rotn                    
			+ VgfTags.sztext + XmlUtil.getSztext(Float.toString(((Text)de).getFontSize()) )
			+ VgfTags.sptxtyp + XmlUtil.getSPTextType(((Text)de).getPgenType(), avnTextType, ((Text)de).maskText(), ((Text)de).getDisplayType().toString()) 
			+ VgfTags.turbsym + (de.getPgenType().equalsIgnoreCase("AVIATION_TEXT")==true ? XmlUtil.getTurbSymType(((AvnText)de).getSymbolPatternName()) :"")  	  
			+ VgfTags.itxfn   + XmlUtil.getFontStyle( ((Text)de).getFontName(), ((Text)de).getStyle().toString()) 
			+ VgfTags.ithw + "2"               					
			+ VgfTags.iwidth + (int)de.getLineWidth() 
			+ VgfTags.txtcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))  
			+ VgfTags.lincol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.filcol + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))
			+ VgfTags.ialign + XmlUtil.getIalign(((Text)de).getJustification().toString() )
			+ VgfTags.lat + XmlUtil.getPointLats(de)            
			+ VgfTags.lon + XmlUtil.getPointLons(de)  
			+ VgfTags.offset_x + ((Text)de).getXOffset()                
			+ VgfTags.offset_y + ((Text)de).getYOffset() 
			+ VgfTags.text +textString  +"\n";
			
			fw.write(vgf);
		}
		return vgf;
	}

	private static String getWatch(String vgf, FileWriter fw, int k,
			DrawableElement de) throws IOException {
		
		if (de.getPgenCategory().equalsIgnoreCase("Lines")) {
			vgf = "!\n";
			
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"3" 				+ VgfTags.delete + "0"                               
			+ VgfTags.filled + (((Line)de).isFilled()==true ?1 :0 )
			+ VgfTags.closed + (((Line)de).isClosedLine()==true ?1 :0 )  
			+ VgfTags.smooth + ((Line)de).getSmoothFactor() 
			+ VgfTags.version + "0" 
			+ VgfTags.grptyp +"98"                  + VgfTags.grpnum + (k+1)  
			+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))      
			+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))
			+ VgfTags.recsz + (XmlUtil.getRecSize(de.getPgenCategory(), de.getPgenType()) +8*de.getPoints().size() )
			+ VgfTags.range_min_lat +XmlUtil.getSortedLat(de)[0]                 
			+ VgfTags.range_min_lon +XmlUtil.getSortedLon(de)[0] 
			+ VgfTags.range_max_lat +XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1] 
			+ VgfTags.range_max_lon +XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
			+ VgfTags.numpts +de.getPoints().size()              
			+ VgfTags.spltyp +XmlUtil.getSPLineType(de.getPgenType()) 
			+ VgfTags.splstr +"1" 
			+ VgfTags.spldir +"1"           
			+ VgfTags.splsiz +de.getSizeScale() 
			+ VgfTags.splwid +(int)de.getLineWidth()			 
			+ VgfTags.latlon + XmlUtil.getPoints(de) +"\n";
			
			fw.write(vgf);
		}
		
		else if (de.getPgenType().equalsIgnoreCase("WatchBox")) {
			vgf = "!\n";
			
			int shape = 0;
			if (((WatchBox)de).getBoxShape().equalsIgnoreCase("NS")) 
				shape = 1;
			else if (((WatchBox)de).getBoxShape().equalsIgnoreCase("EW")) 
				shape = 2;
			else if (((WatchBox)de).getBoxShape().equalsIgnoreCase("ESOL")) 
				shape = 3;

			int sType = 0;
			if (((WatchBox)de).getWatchSymbolType().equalsIgnoreCase("PLUS_SIGN")) 
				sType = 1;
			else if (((WatchBox)de).getWatchSymbolType().equalsIgnoreCase("OCTAGON")) 
				sType = 2;
			else if (((WatchBox)de).getWatchSymbolType().equalsIgnoreCase("TRIANGLE")) 
				sType = 3;
			else if (((WatchBox)de).getWatchSymbolType().equalsIgnoreCase("BOX")) 
				sType = 4;
			else if (((WatchBox)de).getWatchSymbolType().equalsIgnoreCase("SMALL_X")) 
				sType = 5;			
			else if (((WatchBox)de).getWatchSymbolType().equalsIgnoreCase("")) 
				sType = 6;
			else if (((WatchBox)de).getWatchSymbolType().equalsIgnoreCase("UP_ARROW")) 
				sType = 7;
		
			Station[] anc = ((WatchBox)de).getAnchors();
			String ancInfo1 = (((WatchBox)de).getRelative(de.getPoints().get(0), anc[0])); //  120   W  BOI
			String ancInfo2 = (((WatchBox)de).getRelative(de.getPoints().get(4), anc[1])); //  55  NNE  RIW
			String[] split1 = {}; 
			String[] split2 = {};
			
			if (ancInfo1 != null)
				split1 = ancInfo1.trim().split("\\s+");
			if (ancInfo2 != null)
				split2 = ancInfo2.trim().split("\\s+");
			//String spec = ((WatchBox)de).getSpec();
//				47.84   -108.01    140   NW  MLS     69  WSW  GGW
//				39.26    -93.26     40  NNE  SZL     64  WNW  COU
//				ORIENT: NS  - HALF WIDTH:   60sm (50 nm)
//				AREA(sq nautical miles):  68824
			
			String stat = ((WatchBox)de).getIssueStatus() ;
			if (stat != null) {
				if (stat.equalsIgnoreCase("Test"))
					stat = "0";
				else if (stat.equalsIgnoreCase("Active"))
					stat = "1";
				else 
					stat = "0";
			}
			else 
				stat = "0";
		
			
			String Severity = ((WatchBox)de).getSeverity();
			if (Severity != null) {
				if (Severity.equalsIgnoreCase("PDS"))
					Severity = "1";
				else if (Severity.equalsIgnoreCase("Normal"))
					Severity = "0";
				else 
					Severity = "0";
			}
			else 
				Severity = "0";
		
			String waType = ((WatchBox)de).getWatchType(); 
			int issued = ((WatchBox)de).getIssueFlag();  //(-1,)0,1; vgf is 0,1,2
			if (waType != null && issued <= 0) {
					waType = "7";
			}
			else if (waType != null && issued > 0) {
				if (waType.equalsIgnoreCase("SEVERE THUNDERSTORM"))
					waType = "6";
				else if (waType.equalsIgnoreCase("Tornado"))
					waType = "2";
			}
			else
				waType = "7";
		
			
			if (((WatchBox)de).getFromLine() != null && !((WatchBox)de).getFromLine().trim().equalsIgnoreCase(""))
					issued = 2;
			else if (issued ==0 || issued == -1)
					issued = 0;
			else 
					issued = 1;
								
			String issT="", expT="";
			SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy/HHmm"); //set timeZone
			sdf.setTimeZone( TimeZone.getTimeZone("GMT"));
			if ( ((WatchBox)de).getIssueTime() != null) {  //need add "/" to do compare
				issT = sdf.format( ((WatchBox)de).getIssueTime().getTime() ) +"/";
				expT = sdf.format( ((WatchBox)de).getExpTime().getTime() ) +"/";
			}
			
			SimpleDateFormat sdf2 = new SimpleDateFormat("ddHHmm"); //set timeZone
			sdf2.setTimeZone( TimeZone.getTimeZone("GMT"));
			
			
			String state = "";
			List<String> l = ((WatchBox)de).getStates(); 
			if (l != null && !l.isEmpty()) {
				Object[] a = l.toArray();
				//Arrays.sort(a);
				for (Object str : a)  
					state += str + " ";
				state = state.trim();
			}
			/*//System.out.println(((WatchBox)de).getCountyList());
			List<String> states = new ArrayList();
			List<County> countyList = ((WatchBox)de).getCountyList();
			if ( countyList != null && !countyList.isEmpty()){
				for(County cnty : countyList ){
					if ( cnty.getState()!= null && !states.contains(cnty.getState())){
						states.add(cnty.getState());
						state += cnty.getState() + " ";
					}
				}
			}
			state = state.trim();
			System.out.println(state);*/
			
			List<SPCCounty> counties = ((WatchBox)de).getCountyList();
			String fips="",  ctLat="", ctLon="", ugcid="";
			int[] fipsArray = new int[counties.size()];
			int ctNum = 0;
		
			DecimalFormat fm = new DecimalFormat("##.##");
			
			if (counties != null && !counties.isEmpty()) {
				for ( ctNum=0; ctNum<counties.size(); ctNum++) {
					//Object[] object = counties.get(ctNum);    	//[[B@7abf31, Andrew, MO, EAX, 29003, C, -94.84386, 39.97399, 1108]	
					try {
						fipsArray[ctNum] = Integer.parseInt(counties.get(ctNum).getFips());							
						ctLat += fm.format(counties.get(ctNum).getCentriod().y) +", ";
						ctLon += fm.format(counties.get(ctNum).getCentriod().x) +", ";		
						//ugcid += counties.get(ctNum).getUgcId() +", ";
					}
					catch (ArithmeticException e) {}
					catch (NumberFormatException e) {}						
				}
				
				Arrays.sort(fipsArray);
				for (int i=0; i< fipsArray.length; i++)
					fips += fipsArray[i] +", ";
				fips = fips.substring(0, fips.length()-2); //remove end ,
				ctLon = ctLon.substring(0, ctLon.length()-2);
				//ugcid = ugcid.substring(0, ugcid.length()-2);
			}

			
			vgf += VgfTags.vg_type +XmlUtil.getType(de.getPgenCategory(), de.getPgenType()) 
			+ VgfTags.vg_class +"2" 				+ VgfTags.delete +"0"                               
		+ VgfTags.filled +(((WatchBox)de).getFillFlag()==true ?1 :0 )
		+ VgfTags.closed + "0"  					+ VgfTags.smooth + "0" 
		+ VgfTags.version + "6" 
		+ VgfTags.grptyp +"98"         				+ VgfTags.grpnum + (k+1)  
		+ VgfTags.maj_col + XmlUtil.getColorTag(XmlUtil.getColorMaj(de))     	
		+ VgfTags.min_col + XmlUtil.getColorTag(XmlUtil.getColorMin(de))
		+ VgfTags.recsz + "0"
		+ VgfTags.range_min_lat + XmlUtil.getSortedLat(de)[0]                 	
		+ VgfTags.range_min_lon + XmlUtil.getSortedLon(de)[0] 
		+ VgfTags.range_max_lat + XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]  	
		+ VgfTags.range_max_lon + XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  
		                                                       	                                  
		+ VgfTags.numpts 	+ ((WatchBox)de).getPoints().size() 
		+ VgfTags.w_style + "4"//??
		+ VgfTags.w_shape 	+ shape     					+ VgfTags.w_mrktyp + sType    
		+ VgfTags.w_mrksiz 	+ ((WatchBox)de).getWatchSymbolSize() 
		+ VgfTags.w_mrkwid + (int)((WatchBox)de).getWatchSymbolWidth()	//Cave is float
		+ VgfTags.w_a0id	+ split1[2]						+ VgfTags.w_a0lt 	+ anc[0].getLatitude()  //split[0]     			
		+ VgfTags.w_a0ln 	+ anc[0].getLongitude()      	+ VgfTags.w_a0dis 	+ split1[0]							
		+ VgfTags.w_a0dir 	+ split1[1]     				+ VgfTags.w_a1id 	+ split2[2]     					
		+ VgfTags.w_a1lt 	+ anc[1].getLatitude() 			+ VgfTags.w_a1ln 	+ anc[1].getLongitude() //split[9]     			
		+ VgfTags.w_a1dis 	+ split2[0]	     				+ VgfTags.w_a1dir 	+ split2[1]							
		 					
		+ VgfTags.w_istat   + stat							+ VgfTags.w_number  + ((WatchBox)de).getWatchNumber()	//"-9999" 			
		+ VgfTags.w_iss_t   + issT 							+ VgfTags.w_exp_t   + expT 
		+ VgfTags.w_type    + waType						+ VgfTags.w_severiy + Severity   	
		+ VgfTags.w_timezone  +((((WatchBox)de).getTimeZone() ==null) ?"" :((WatchBox)de).getTimeZone() )
		+ VgfTags.w_hailsz  + ((WatchBox)de).getHailSize()	+ VgfTags.w_windg  + ((WatchBox)de).getGust() 
		+ VgfTags.w_tops    + ((WatchBox)de).getTop()  		+ VgfTags.w_msmv_d + ((WatchBox)de).getMoveDir()
		+ VgfTags.w_msmv_s	+ ((WatchBox)de).getMoveSpeed()	+ VgfTags.w_states + state
		
		+ VgfTags.w_adjarea	+ ((((WatchBox)de).getAdjAreas() ==null) ?"" :((WatchBox)de).getAdjAreas())		
		+ VgfTags.w_replw   + ((WatchBox)de).getReplWatch()
		+ VgfTags.w_fcstr	+ ((((WatchBox)de).getForecaster() ==null) ?"" :((WatchBox)de).getForecaster())		
		+ VgfTags.w_file 										+ VgfTags.w_issued  + issued		
		+ VgfTags.wsm_iss_t + ((((WatchBox)de).getStatusValidTime() ==null) ?"" :sdf2.format( ((WatchBox)de).getStatusValidTime().getTime() ))
		+ VgfTags.wsm_exp_t	+ ((((WatchBox)de).getStatusExpTime() ==null) ?"" :sdf2.format( ((WatchBox)de).getStatusExpTime().getTime() ))
		+ VgfTags.wsm_ref  // + ((WatchBox)de)..getEndPointVor().getEndPointAnc().getHalfWidth().getHalfWidthSm().getHalfWidthNm().getWatchAreaNm().getWFOs()
		+ VgfTags.wsm_from	+ ((((WatchBox)de).getFromLine() ==null) ?"" :((WatchBox)de).getFromLine())
		+ VgfTags.wsm_meso  + ((((WatchBox)de).getDiscussion() == 0 ) ?"" :((WatchBox)de).getDiscussion() )
		+ VgfTags.wsm_fcstr	+ ((((WatchBox)de).getStatusForecaster() ==null) ?"" :((WatchBox)de).getStatusForecaster())
		+ VgfTags.numcnty   + ctNum								+ VgfTags.cn_flag	+ ((ctNum == 0) ? 0 :1)	
		+ VgfTags.latlon + XmlUtil.getPoints(de)				+ VgfTags.cn_fips	+ fips								
		+ VgfTags.cn_ltln  + ctLat + ctLon						+ "\n";
							
			// if IssueFlag != 0, display List 34
			if (((WatchBox)de).getIssueFlag() != 0) { //.getWatchNumber() != -9999) {
				vgf += "!\n"  + VgfTags.vg_type + "34" 			+ VgfTags.vg_class +"14" 
				+ VgfTags.delete  + "0"     					+ VgfTags.filled + "0"  	
				+ VgfTags.closed + "0"    						+ VgfTags.smooth + "0" 
				+ VgfTags.version + "0"   						+ VgfTags.grptyp + "98"     
				+ VgfTags.grpnum + (k+1)  						+ VgfTags.maj_col + "25"	
				+ VgfTags.min_col + "25"    					+ VgfTags.recsz  + "0"
				+ VgfTags.range_min_lat + XmlUtil.getSortedLat(de)[0]   
				+ VgfTags.range_min_lon + XmlUtil.getSortedLon(de)[0] 
				+ VgfTags.range_max_lat + XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1]                                                     
				+ VgfTags.range_max_lon + XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1]  							                                                       	                                  
				+ VgfTags.subtyp + "5" 						+ VgfTags.mrktyp + "2"		
				+ VgfTags.mrksiz  + "2.4"      					+ VgfTags.mrkwid + "2"     
				+ VgfTags.nitems  + ctNum 						+ VgfTags.item + fips
				+ VgfTags.lat + ( ctLat.length()<=2 ? ctLat : ctLat.substring(0, ctLat.length()-2) ) 
				+ VgfTags.lon + ctLon +"\n";
			}	
			
			fw.write(vgf);
		}
		return vgf;
	}

	private static String getCcf(String vgf, FileWriter fw, AbstractDrawableComponent adcK) 
		throws IOException {
		
		DrawableElement de;
		String cover="", tops="", prob="", growth="", subType="", speed="", dir="";
		int ifilled=0, icover=0, itops=0, iprob=0, igrowth=0, isubType=0; 
		int closed=0, col_maj=0, col_min=0, pts=0;
		double idir=0.0, min_lat=0.0, min_lon=0.0, max_lat=0.0, max_lon=0.0;
		String latlon="", textlat="0.0", textlon="0.0", arrowlat="0.0", arrowlon="0.0", text="";
		
		String pgenName = adcK.getName().trim();
		//pgenName = CCFP_SIGMET:::0:::N:::null:::null:::25-39%:::250-290:::50-100%:::NC:::LineMed		
		if (pgenName != null) {
			String[] info = pgenName.split(":::");
			if (info.length == 10) {
				speed = info[1];
				dir = info[2];
				cover = info[5];
				tops = info[6];
				prob = info[7];
				growth = info[8];
				subType = info[9];	
			}
		}
		
		if (subType.equalsIgnoreCase("Area")) {
			Iterator<DrawableElement> iter = adcK.createDEIterator();
			while ((de = iter.next()) != null) {
				if (de.getPgenCategory().equalsIgnoreCase("Text")) {
					String[] textPt = XmlUtil.getPoints(de).toString().trim().split(",");
					textlat = textPt[0]; //lat,lon
					textlon = textPt[1];
				}
				else if (de.getPgenType().equalsIgnoreCase("POINTED_ARROW")) {
					String[] arrow = XmlUtil.getPoints(de).toString().trim().split(",");
					arrowlat = arrow[0]; //lat,lat,lon,lon
					arrowlon = arrow[arrow.length/2]; 	
				}
				else if (de.getPgenType().equalsIgnoreCase("LINE_SOLID")) {
					ifilled = XmlUtil.fill_pattern(((Line)de).getFillPattern().toString(), ((Line)de).isFilled());
					closed = 1 ;
					min_lat = XmlUtil.getSortedLat(de)[0];                 		
					min_lon = XmlUtil.getSortedLon(de)[0] ;
					max_lat = XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1];  		
					max_lon = XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1];  
					col_maj = XmlUtil.getColorTag(XmlUtil.getColorMaj(de));       	
					col_min = XmlUtil.getColorTag(XmlUtil.getColorMin(de));
					pts = de.getPoints().size();
					latlon = XmlUtil.getPoints(de);
					text = ""; //IBDR|221;TEXT|TOPS::ETR;TEXT|GWTH::GWTH;TEXT|CONF::CONF;TEXT|CVRG::CVG" :"")
					
				}
			}
		}
		else if (subType.equalsIgnoreCase("LineMed")) {
			isubType = 2;
			de = adcK.getPrimaryDE();
			col_maj = XmlUtil.getColorTag(XmlUtil.getColorMaj(de));       	
			col_min = XmlUtil.getColorTag(XmlUtil.getColorMin(de));
			min_lat = XmlUtil.getSortedLat(de)[0];                 		
			min_lon = XmlUtil.getSortedLon(de)[0]; 
			max_lat = XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1];  		
			max_lon = XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1];  
			pts = de.getPoints().size();
			latlon = XmlUtil.getPoints(de);
		}
		else if (subType.equalsIgnoreCase("Line")) {
			isubType = 1;
			de = adcK.getPrimaryDE();
			col_maj = XmlUtil.getColorTag(XmlUtil.getColorMaj(de));       	
			min_lat = XmlUtil.getSortedLat(de)[0];                 		
			min_lon = XmlUtil.getSortedLon(de)[0];
			max_lat = XmlUtil.getSortedLat(de)[XmlUtil.getSortedLat(de).length-1];  		
			max_lon = XmlUtil.getSortedLon(de)[XmlUtil.getSortedLon(de).length-1];  
			col_min = XmlUtil.getColorTag(XmlUtil.getColorMin(de));
			pts = de.getPoints().size();
			latlon = XmlUtil.getPoints(de);
		}

	    if (cover.equalsIgnoreCase("25-39%") )
	    	icover = 3;
	    else if (cover.equalsIgnoreCase("40-74%") )
	    	icover = 2;
	    else if (cover.equalsIgnoreCase("75-100%") )
	    	icover = 1;

	    if (tops.equalsIgnoreCase("250-290") )
	    	itops = 4;
	    else if (tops.equalsIgnoreCase("300-340") )
	    	itops = 3;
	    else if (tops.equalsIgnoreCase("350-390") )
	    	itops = 2;
	    else if (tops.equalsIgnoreCase("400+") )
	    	itops = 1;

	    if (prob.equalsIgnoreCase("25-49%") )
	    	iprob = 3;
	    else if (prob.equalsIgnoreCase("50-100%") )
	    	iprob = 1;
	        
	    if (growth.equalsIgnoreCase("-") )
	    	igrowth = 4;
	    else if (growth.equalsIgnoreCase("NC") )
	    	igrowth = 3;
	    else if (growth.equalsIgnoreCase("+") )
	    	igrowth = 2;
		
	    if (dir.equalsIgnoreCase("N"))
	    	idir = 0;
	    else if (dir.equalsIgnoreCase("NNE") ) 
	    	idir = 22.5;
	    else if (dir.equalsIgnoreCase("NE") ) 
	    	idir = 45;
	    else if (dir.equalsIgnoreCase("ENE") ) 
	    	idir = 67.5;
	    else if (dir.equalsIgnoreCase("E") ) 
	    	idir = 90;
	    else if (dir.equalsIgnoreCase("ESE") ) 
	    	idir = 112.5;
	    else if (dir.equalsIgnoreCase("SE") ) 
	    	idir = 135;
	    else if (dir.equalsIgnoreCase("SSE") ) 
	    	idir = 157.5;
	    else if (dir.equalsIgnoreCase("S") ) 
	    	idir = 180;
	    else if (dir.equalsIgnoreCase("SSW") ) 
	    	idir = 202.5;
	    else if (dir.equalsIgnoreCase("SW") ) 
	    	idir = 225;
	    else if (dir.equalsIgnoreCase("WSW") ) 
	    	idir = 247.5;
	    else if (dir.equalsIgnoreCase("W") ) 
	    	idir = 270;
	    else if (dir.equalsIgnoreCase("WNW") ) 
	    	idir = 292.5;
	    else if (dir.equalsIgnoreCase("NW") ) 
	    	idir = 315;
	    else if (dir.equalsIgnoreCase("NNW") ) 
	    	idir = 337.5;
	    
////	    GeometryFactory factory = new GeometryFactory();
////		LineString g = factory.createLineString(((Sigmet)de).getLinePoints());
////		Point p = null;
////		if (g != null)
////			p = g.getCentroid();
	    
		vgf += VgfTags.vg_type + 32
			+ VgfTags.vg_class +"11" 			+ VgfTags.delete + "0" 
			+ VgfTags.filled + ifilled			+ VgfTags.closed + closed   
			+ VgfTags.smooth + 0 				+ VgfTags.version + "0" 
			+ VgfTags.grptyp +"8"         		+ VgfTags.grpnum +"1"  
			+ VgfTags.maj_col + col_maj			+ VgfTags.min_col + col_min 
			+ VgfTags.recsz + "0"
			+ VgfTags.range_min_lat + min_lat   + VgfTags.range_min_lon + min_lon 
			+ VgfTags.range_max_lat + max_lat	+ VgfTags.range_max_lon + max_lon
			+ VgfTags.subtype + isubType         + VgfTags.npts + pts 				
			+ VgfTags.szarrow + 2			  	+ VgfTags.linetype + "4" //arrowed line 
			+ VgfTags.cover + icover				+ VgfTags.tops + itops
			+ VgfTags.prob + iprob				+ VgfTags.growth + igrowth 
			+ VgfTags.fillhi + 0     			+ VgfTags.fillmed + 0 
			+ VgfTags.filllow + 0   			+ VgfTags.spd + speed    
			+ VgfTags.dir + idir  
			//+ VgfTags.stime + 				+ VgfTags.etime +
			+ VgfTags.textlat +  textlat 		+ VgfTags.textlon +  textlon    
			+ VgfTags.arrowlat + arrowlat		+ VgfTags.arrowlon + arrowlon 	
			+ VgfTags.textLayout + text
			+ VgfTags.latlon + latlon +"\n";
		
		fw.write(vgf);
		return vgf;
	}
}
