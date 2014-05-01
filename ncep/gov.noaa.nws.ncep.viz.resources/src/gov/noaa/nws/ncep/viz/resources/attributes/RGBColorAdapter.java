package gov.noaa.nws.ncep.viz.resources.attributes;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.eclipse.swt.graphics.RGB; 

public class RGBColorAdapter extends XmlAdapter<String, RGB> {

    private final RGB DEFAULT_COLOR = new RGB(255, 0, 0);

	@Override
	public String marshal(RGB rgbColor) throws Exception {
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
		if(colorStringValue == null || colorStringValue.trim().length() == 0)
			return DEFAULT_COLOR; 
		int red = 255, green = 0, blue = 0; 
		int[] colorArray = parseStringToIntArray(colorStringValue); 
		if(colorArray != null) {
			red = colorArray[0]; 
			green = colorArray[1]; 
			blue = colorArray[2]; 
		}
		
		return new RGB(red, green, blue);
	}
	
	private int[] parseStringToIntArray(String colorStringValue) {
		boolean isStringValid = false; 
		int beginIndex = colorStringValue.indexOf('{'); 
		int endIndex = colorStringValue.indexOf('}'); 
		String rgbstring = null; 
		try {
			rgbstring = colorStringValue.substring(beginIndex+1, endIndex); 
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
		strBuilder.append("RGB {")
				  .append(red)
				  .append(", ")
				  .append(green)
				  .append(", ")
				  .append(blue)
				  .append("}"); 
		return strBuilder.toString(); 
	}
}
