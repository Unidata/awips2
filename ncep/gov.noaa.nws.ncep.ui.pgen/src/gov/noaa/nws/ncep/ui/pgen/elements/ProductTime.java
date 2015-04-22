package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.Calendar;

/**
 * Define a product time.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/22/08					J. Wu   	Initial Creation.
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 * 
 */
public class ProductTime {

    /** The fields */
    private Calendar cycle;
    private Calendar startTime;
    private Calendar endTime;
    private ProductRelationship relationship;
	
	/**
	 * @param calendar
	 */
	public ProductTime() {
		Calendar calendar = (Calendar)Calendar.getInstance();
		this.cycle= calendar;
		this.startTime = calendar;
		this.endTime = calendar;
		this.relationship = ProductRelationship.NORMAL;
	}	
     
     /**
	 * @param calendar
	 */
	public ProductTime(Calendar calendar) {
		this.cycle = calendar;
		this.startTime = calendar;
		this.endTime = calendar;
		this.relationship = ProductRelationship.NORMAL;
	}	
				
	public Calendar getCycle() {
		return cycle;
	}
	
	public void setCycle(Calendar cycle) {
		this.cycle = cycle;
	}
	
	public Calendar getStartTime() {
		return startTime;
	}
	
	public void setStartTime(Calendar startTime) {
		this.startTime = startTime;
	}
	
	public Calendar getEndTime() {
		return endTime;
	}
	
	public void setEndTime(Calendar endTime) {
		this.endTime = endTime;
	}
	
	public ProductRelationship getRelationship() {
		return relationship;
	}
	
	public void setRelationship(ProductRelationship relationship) {
		this.relationship = relationship;
	}  
	
	public boolean compare(ProductTime prdTime) {
		if ( cycle.compareTo(prdTime.cycle) == 0 &&
		     startTime.compareTo(prdTime.startTime) == 0 &&
		     endTime.compareTo(prdTime.endTime) == 0 &&
		     relationship == prdTime.relationship ) {
		     return true;
		}
		else {
		     return false;
	    }
		     
	}  
	
	public long getRange() {		
        return (endTime.getTimeInMillis() - startTime.getTimeInMillis() );		     
	}    

}
 