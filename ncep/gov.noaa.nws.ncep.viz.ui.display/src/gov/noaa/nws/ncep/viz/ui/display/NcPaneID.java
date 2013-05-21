package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.INcPaneID;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 *   Use to id a pane within the NCMapEditor. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *      
 * Date            Ticket#     Engineer    Description
 * ------------    ----------  ----------- --------------------------
 * 02/28/10          #226        ghull       Created 
 * 02/09/13          #972       Greg Hull   implement INcPaneId. rname ro NcPaneId
 * 
 * </pre>
 * 
 * @author ghull
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class NcPaneID implements INcPaneID {
	
	@XmlAttribute
	private int row;
	
	@XmlAttribute
	private int col;

	public NcPaneID() {
		row = 0;
		col = 0;
	}
			
	public int getRow() {
		return row;
	}
	
	public int getColumn() {
		return col;
	}    	

	public void setRow( int r ) {
		row = r;
	}

	public void setColumn( int c ) {
		col = c;
	}
	
	public int compareTo( INcPaneID paneId ) {
		if( !(paneId instanceof NcPaneID) ) {
			return -1;
		}
		NcPaneID pid = (NcPaneID)paneId;
		
		if( pid.row == this.row &&
				pid.col == this.col ) {
			return 0;
		}
		else if( this.row < pid.row &&
				this.col < pid.col ) {
			return -1;
		}
		else {
			return 1;
		}
	}

	public boolean equals( NcPaneID pid ) {
		return ( pid.getRow() == row && pid.getColumn() == col );
	}

	// 1-base Column,Row
	public String toString() {
		return new String( Integer.toString(col+1)+","+Integer.toString(row+1)); 
	}

	// Assuming that this string came from the toString method
	public static NcPaneID parsePaneId( String idStr ) {
		String colStr = idStr.substring(0, idStr.indexOf(',') );	
		String rowStr = idStr.substring( idStr.indexOf(',')+1 );
		NcPaneID pid = new NcPaneID();
		pid.setRow( Integer.parseInt(rowStr)-1 );
		pid.setColumn( Integer.parseInt(colStr)-1);
		return pid;
	}
}