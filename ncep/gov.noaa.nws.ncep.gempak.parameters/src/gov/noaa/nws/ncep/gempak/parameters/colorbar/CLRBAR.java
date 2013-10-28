/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameters.colorbar;

import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

/**
 * 
 * *
 * 
 * <pre>
 * SOFTWARE HISTORY
 *  Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 *  11-Jun-2012    743       Archana.S   Initial Creation
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 * @see $GEMPAK/help/hlx/clrbar.hl2
 */

public class CLRBAR {
	private ColorBarAttributesBuilder cBarAttributesBuilder;

	private String strToParse; 
	/**
	 * @return the cBarAttributesBuilder
	 */
	public final ColorBarAttributesBuilder getcBarAttributesBuilder() {
		return cBarAttributesBuilder;
	}

	public CLRBAR(String strToParse) {
		cBarAttributesBuilder = new ColorBarAttributesBuilder();
		this.strToParse = strToParse;
		parse(strToParse);
	}

	private void parse(String strToParse) {
	  

	  if (strToParse == null || strToParse.isEmpty())
			return;

	  strToParse = new String(StringUtil.removeBlanksWithinString(strToParse));
	  String[] parsedStringArray = strToParse.split("/");
	  if (parsedStringArray != null && parsedStringArray.length >= 1 ) {
  		  int color;
		  try {
				color = Integer.parseInt(parsedStringArray[0]);
				
				if(color == 0)
					cBarAttributesBuilder.setDrawColorBar(false);
				else
					cBarAttributesBuilder.setDrawColorBar(true);
				
				if (color < 0 ){
					cBarAttributesBuilder.setDrawBoxAroundColorBar(false);
					cBarAttributesBuilder.setColor(GempakColor.convertToRGB(color * -1));
				}else{
					cBarAttributesBuilder.setDrawBoxAroundColorBar(true);
					cBarAttributesBuilder.setColor(GempakColor.convertToRGB(color));
				}
			   } catch (NumberFormatException nfe) {
				    cBarAttributesBuilder.setDrawColorBar(false);
			    }
				if ( parsedStringArray.length >= 2 &&  parsedStringArray[1] != null && !parsedStringArray[1].isEmpty() ){
					if (parsedStringArray[1].compareTo("H") == 0)
						cBarAttributesBuilder.setColorBarOrientation(ColorBarOrientation.Horizontal);
					else
						cBarAttributesBuilder.setColorBarOrientation(ColorBarOrientation.Vertical);
				}
	
				if (parsedStringArray.length >= 3 &&  parsedStringArray[2] != null && !parsedStringArray[2].isEmpty()){
					if (parsedStringArray[2].compareTo("UR") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.UpperRight);
					} 						
					else if (parsedStringArray[2].compareTo("UC") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.UpperCenter);
					}
					else if (parsedStringArray[2].compareTo("CR") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.CenterRight);
					}
					else if (parsedStringArray[2].compareTo("CC") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.CenterCenter);
					}					
					else if (parsedStringArray[2].compareTo("CL") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.CenterLeft);
					}						
					else if (parsedStringArray[2].compareTo("LR") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.LowerRight);
					}					
					else if (parsedStringArray[2].compareTo("LC") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.LowerCenter);
					}					
					else if (parsedStringArray[2].compareTo("UL") == 0){
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.UpperLeft);
					}					
					else
						cBarAttributesBuilder.setAnchorLocation(ColorBarAnchorLocation.LowerLeft);
				}				
				
				
				if ( parsedStringArray.length >= 4 &&  parsedStringArray[3] != null && !parsedStringArray[3].isEmpty()){
					 String[] coordArr = parsedStringArray[3].split(";");
					 try{
						 double x = Double.parseDouble(coordArr[0]);
						 if(x >= 0 && x <= 1)  
						   cBarAttributesBuilder.setX(x);
						}catch(Exception e){
						 //Do nothing - there are defaults set for the X view coordinates
					 }
					
					try{
						     double y = Double.parseDouble(coordArr[1]);
						     if( y>= 0 && y <= 1 )
							    cBarAttributesBuilder.setY(y);
							}catch(Exception e){
							 //Do nothing - there are defaults set for the Y view coordinates
						 }
						
				}
				
				if ( parsedStringArray.length >= 5 && parsedStringArray[4] != null && !parsedStringArray[4].isEmpty() ){
					 String[] dimensionsArray = parsedStringArray[4].split(";");
					 try{
						     double lengthRatio = Double.parseDouble(dimensionsArray[0]);
						     if(lengthRatio >= 0 && lengthRatio <= 1)  
						        cBarAttributesBuilder.setLength(lengthRatio);
					 }catch(Exception e){
						 //Do nothing - there are defaults set for the length
					 }
					 
					 try{
						    double widthRatio = Double.parseDouble(dimensionsArray[1]);
						    if(widthRatio >= 0 && widthRatio <= 1)
						     cBarAttributesBuilder.setWidth(widthRatio);
					 }catch(Exception e){
						 //Do nothing - there are defaults set for the width
					 }							 
				}				
				

		}
		
	}

	/**
	 * @return the strToParse
	 */
	public final String getStrToParse() {
		return strToParse;
	}
}
