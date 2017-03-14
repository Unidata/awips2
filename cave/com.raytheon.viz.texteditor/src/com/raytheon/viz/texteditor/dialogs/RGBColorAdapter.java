package com.raytheon.viz.texteditor.dialogs;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2013  DR 15733   Xiaochuan   Modified to have the functionality that can 
 * 										handle color of strings like "red", "green",...
 * 
 * </pre>
 * 
 * @author XHuang
 * @version 1.0
 */

import org.eclipse.swt.graphics.RGB; 
import com.raytheon.uf.viz.core.RGBColors;

public class RGBColorAdapter extends XmlAdapter<String, RGB> {

    private final RGB DEFAULT_COLOR = new RGB(255, 0, 0);

	@Override
	public String marshal(RGB rgbColor) throws Exception {
		// Set rgb to color string go here if save to file 
		
		int red = DEFAULT_COLOR.red; 
		int green = DEFAULT_COLOR.green; 
		int blue = DEFAULT_COLOR.blue; 
		
		if(rgbColor != null) {
			red = rgbColor.red; 
			green = rgbColor.green; 
			blue = rgbColor.blue; 
		}
		
		String rgbStringValue = buildRGBStringValue(red, green, blue); 
		
		return rgbStringValue;
	}

	@Override
	public RGB unmarshal(String colorStringValue) throws Exception {
		int red = 255, green = 0, blue = 0; 
		RGB rgb = null;
		String colorString = colorStringValue.trim();
		
		if(colorStringValue == null || colorString.trim().length() == 0)
			return DEFAULT_COLOR;
		
		int sz = colorString.split(",").length;
		if( colorString.split(",").length == 1) {
			rgb = RGBColors.getRGBColor(colorString);
			
		}
		else {			
			int[] colorArray = parseStringToIntArray(colorString); 
			if(colorArray != null) {
				red = colorArray[0]; 
				green = colorArray[1]; 
				blue = colorArray[2]; 
				rgb = new RGB(red, green, blue);
			}
		}
		
		return rgb;
	}
	
	private int[] parseStringToIntArray(String colorStringValue) {
		boolean isStringValid = false; 
		int beginIndex = colorStringValue.indexOf('{'); 
		int endIndex = colorStringValue.indexOf('}'); 
		String rgbstring = null; 
		try {
			if( beginIndex > -1 &&  endIndex > -1)
				rgbstring = colorStringValue.substring(beginIndex+1, endIndex); 
			else
				rgbstring = colorStringValue;
			
		} catch(IndexOutOfBoundsException iobe) {
			//do nothing
		}
		
		if(rgbstring == null)
			return null; 
		String[] rgbStringArray = rgbstring.split(",");
		if(rgbStringArray.length != 3)
			return null; 
		
		int[] rgbIntArray = new int[3]; 
		try {
			for(int i=0; i<3; i++) {
				 int rgbIntValue = Integer.parseInt(rgbStringArray[i].trim()); 
				 if(!isRGBValid(rgbIntValue)) {
					 return null; 
				 }
				 rgbIntArray[i] = rgbIntValue; 
			}
			isStringValid = true; 
		} catch(NumberFormatException nfe) {
			// do nothing
		}
		
		if(!isStringValid)
			return null; 
		return rgbIntArray; 
	}

	/*
	 * This helper method is for future checking implementation
	 */
	private boolean isRGBValid(int rgbIntValue) {
		return true; 
	}
	
	private String buildRGBStringValue(int red, int green, int blue) {
		StringBuilder strBuilder = new StringBuilder(20); 
//		strBuilder.append("RGB {")
		
		strBuilder.append(red)
				  .append(", ")
				  .append(green)
				  .append(", ")
				  .append(blue)
				  .append("}"); 
		return strBuilder.toString(); 
	}
}
