package com.raytheon.viz.mpe.ui;


/**
 * Keeps track of which 1-hour precip fields have been selected to be compared within the MPE Perspective.
 * This selection is still "remembered" when MPE is not displaying a comparison field of either type: difference or ratio.
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * November 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */

public class ComparisonFields {

	//These 2 fields are only valid when the outer DisplayFieldData object is 
    //either  precipDifferenceField or  precipRatioField
    private DisplayFieldData field1 = DisplayFieldData.rMosaic;
    private DisplayFieldData field2 = DisplayFieldData.rdMosaic;

    public void setComparisonFields(DisplayFieldData field1, DisplayFieldData field2)
    {
    	setField1(field1);
    	setField2(field2);
    }

    public void reverse()
    {
    	DisplayFieldData field1 = getField1();
    	DisplayFieldData field2 = getField2();
    	
    	setField2(field1);
    	setField1(field2);
    }
    
	public DisplayFieldData getField1() {
		return field1;
	}

	public void setField1(DisplayFieldData field1) {
		this.field1 = field1;
	}

	public DisplayFieldData getField2() {
		return field2;
	}

	public void setField2(DisplayFieldData field2) {
		this.field2 = field2;
	}
	
	public String toString()
	{
		StringBuffer buffer = new StringBuffer();
		
		DisplayFieldData field1 = getField1();
		DisplayFieldData field2 = getField2();
		
		buffer.append("Field 1 = " + field1);
		buffer.append("  Field 2 = " + field2);
		
		return buffer.toString();
	}
}
