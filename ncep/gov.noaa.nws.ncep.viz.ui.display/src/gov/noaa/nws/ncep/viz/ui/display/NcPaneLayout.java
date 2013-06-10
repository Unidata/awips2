package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/09/13      #972      Greg Hull   Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class NcPaneLayout implements INcPaneLayout {

	@XmlAttribute
	private int numRows;

	@XmlAttribute
	private int numColumns;

	public NcPaneLayout() {
		numRows = 1;
		numColumns = 1;
	}

	public NcPaneLayout(int r, int c) {
		numRows = r;
		numColumns = c;
	}

	public int getRows() {
		return numRows;
	}

	public int getColumns() {
		return numColumns;
	}

	@Override
	public int getNumberOfPanes() {
		return numRows * numColumns;
	}

	@Override
	public int getPaneIndex( INcPaneID pid ) {
		if( pid instanceof NcPaneID ) {
			return getColumns()*((NcPaneID)pid).getRow() + ((NcPaneID)pid).getColumn(); 
		}
		else {
			System.out.println("ERROR non NcPaneID passed to getPaneIndex()");//sanity check
			return -1; 
		}		
	}
	
	@Override
	public INcPaneID createPaneId( int paneIndex ) {		
		NcPaneID pid = new NcPaneID();
		pid.setColumn( paneIndex % getColumns() );
		pid.setRow( paneIndex / getColumns() );
		return pid;
	}

	@Override
	public Boolean containsPaneId( INcPaneID pid ) {
		return getPaneIndex(pid) < getNumberOfPanes();		
	}
	
	@Override
	public List<INcPaneID> getPaneIDs() {
		List<INcPaneID> pids = new ArrayList<INcPaneID>();
		for( int i=0 ; i<getNumberOfPanes() ; i++ ) {
			pids.add( createPaneId(i) );
		}
		return pids;
	}

	@Override
	public int compare( INcPaneLayout playout) {
		if( !(playout instanceof NcPaneLayout) ) {
			return -1;
		}
		NcPaneLayout ncpl = (NcPaneLayout)playout;
		
		if (numColumns != ncpl.getColumns()) {
			return (numColumns < ncpl.getColumns() ? -1 : 1);
		}
		if (numRows != ncpl.getRows()) {
			return (numRows < ncpl.getRows() ? -1 : 1);
		} else
			return 0;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + numColumns;
		result = prime * result + numRows;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof NcPaneLayout))
			return false;
		NcPaneLayout other = (NcPaneLayout) obj;
		if (numColumns != other.numColumns)
			return false;
		if (numRows != other.numRows)
			return false;
		return true;
	}
}
