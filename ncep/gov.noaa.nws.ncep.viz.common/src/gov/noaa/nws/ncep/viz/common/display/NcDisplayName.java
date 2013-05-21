package gov.noaa.nws.ncep.viz.common.display;


/**
 * a unique id for a display or for a pane. used for the editor tabs and to find panes
 * Currently not used but can use this to replace the string manipulation code which formats
 * the display names. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/19/13      #972      ghull        Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class NcDisplayName {
	private int id;  
	//		private int paneNum;
	private String name;

	public NcDisplayName( int id, /*NcPaneID pane_id, */String dName ) {
		this.id = id;
		//paneNum = pNum;
		this.name = dName;
	}

	public int getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String toString( ) {
		//			if( paneNum > 0 )
		//				return new String( Integer.toString(editorNum) + "-" + displayName + 
		//					"(" + Integer.toString( paneNum ) + ")" );
		//			else 
		if( id != -1 ) {
			return new String( Integer.toString(id) + "-" + name );
		}
		else { // should probably treat this as an invalid string 
			return new String( name ); // 
		}
	}

	// 
	public static NcDisplayName parseNcDisplayNameString( String nameStr ) {
		int indx = nameStr.indexOf("-"); 
		int id = -1;
		if( indx > 0 && indx <= 3 ) {
			id = Integer.parseInt( nameStr.substring( 0, indx ) ); 
		}

		return new NcDisplayName( id, nameStr.substring( indx+1 ) );
	}

	public int compare( NcDisplayName n1, NcDisplayName n2 ) {
		if( n1 == null || n2 == null ) 
			return 0;
		if( n1 == null ) 
			return -1;
		if( n2 == null ) 
			return 1;
		else 
			return  n2.getId() - n1.getId();				 
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof NcDisplayName))
			return false;
		NcDisplayName other = (NcDisplayName) obj;
		if (id != other.id)
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}
