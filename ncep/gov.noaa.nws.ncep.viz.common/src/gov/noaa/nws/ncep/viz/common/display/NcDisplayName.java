package gov.noaa.nws.ncep.viz.common.display;


/**
 * a unique id for a display or for a pane. used for the editor tabs and to find panes.
 *    The displayName is managed by an INatlCntrPaneManager which may be either an editor
 * or an AbstractRBD. In the later case the id will not be set. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/19/13      #972      ghull        Initial creation
 * 05/14/13      #862      ghull        add NcPaneName
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class NcDisplayName {
	
	public static class NcPaneName {		
		private NcDisplayName dispName;
		private INcPaneID  paneId = null; // if null then this is not a multi-pane layout
		
		public NcPaneName( NcDisplayName dname ) {
			dispName = dname;
			paneId = null;
		}
		
		public NcPaneName( NcDisplayName dname, INcPaneID  pid ) {
			dispName = dname;
			paneId = pid;
		}
		
		public NcDisplayName getDispName() {
			return dispName;
		}

		public void setDispName(NcDisplayName dispName) {
			this.dispName = dispName;
		}

		public INcPaneID getPaneId() {
			return paneId;
		}

		public void setPaneId(INcPaneID paneId) {
			this.paneId = paneId;
		}

		@Override
		public String toString() {
			if( paneId == null ) {
				return dispName.toString();				
			}
			return dispName.toString() + "(" + paneId.toString() + ")";
		}
		
		public static NcPaneName parsePaneName( String pNameStr ) {
			int indx = pNameStr.indexOf( "(" );
			if( indx == -1 ) {
				return new NcPaneName( NcDisplayName.parseNcDisplayNameString( pNameStr ) );
			}
			String dNameStr = pNameStr.substring( 0, indx );
			int indx2 = dNameStr.indexOf( ") " );
			if( indx2 == -1 ) {
//				NcPaneID pid = null;
				
				return new NcPaneName( 
						NcDisplayName.parseNcDisplayNameString( dNameStr ) );
				
			}
//			NcDisplayName dName = NcDisplayName.parseNcDisplayNameString( )
//			NcPaneName pName = new NcPaneName();
			return new NcPaneName( 
					NcDisplayName.parseNcDisplayNameString(  dNameStr ) );
		}
		
	}
	
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
