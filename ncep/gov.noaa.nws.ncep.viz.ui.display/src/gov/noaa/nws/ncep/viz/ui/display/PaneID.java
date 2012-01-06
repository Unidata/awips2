/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.viz.ui.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;


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
 * 
 * </pre>
 * 
 * @author ghull
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PaneID {
	@XmlAttribute
	private int row;
	
	@XmlAttribute
	private int col;

	public PaneID() {
		row = 0;
		col = 0;
	}
	public PaneID( int r, int c ) { 
		row=r;
		col=c; 
	}
	public PaneID( PaneID pid ) {
		row=pid.row;
		col=pid.col;
	}
	public int getRow() {
		return row;
	}
	public int getColumn() {
		return col;
	}    	
	
	public int compare( PaneID pid ) {
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

	public boolean equals( PaneID pid ) {
		return ( pid.getRow() == row && pid.getColumn() == col );
	}

	// 1-base Column,Row
	public String toString() {
		return new String( Integer.toString(col+1)+","+Integer.toString(row+1)); 
	}

	// Assuming that this string came from the toString method
	static public PaneID parsePaneId( String idStr ) {
		String colStr = idStr.substring(0, idStr.indexOf(',') );	
		String rowStr = idStr.substring( idStr.indexOf(',')+1 );
		return new PaneID( Integer.parseInt(rowStr)-1, Integer.parseInt(colStr)-1);
	}
}