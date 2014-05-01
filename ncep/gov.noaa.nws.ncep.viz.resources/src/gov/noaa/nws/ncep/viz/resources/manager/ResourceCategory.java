package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  02/13/13      #972      Greg Hull   Created
 *  03/06/13      #958      Greg Hull   Added SpaceRscCategory
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceCategory implements Comparable<ResourceCategory> {
	
	public static class ResourceCategoryAdapter extends XmlAdapter<String, ResourceCategory> {
		// 
		@Override
		public ResourceCategory unmarshal(String n) throws Exception {
			if( ResourceCategory.getCategory( n ) == ResourceCategory.NullCategory ) {
				ResourceCategory.createCategory( n );
			}
			return ResourceCategory.getCategory( n );
		}

		@Override
		public String marshal(ResourceCategory rc) throws Exception {
			return rc.toString();
		}
	}
	
    private static Map<String, ResourceCategory> catMap = new HashMap<String, ResourceCategory>();

    public static ResourceCategory SatelliteRscCategory = createCategory( "SATELLITE", 100 );
    public static ResourceCategory RadarRscCategory     = createCategory( "RADAR",     200 );
    public static ResourceCategory GridRscCategory      = createCategory( "GRID",      300 );
    public static ResourceCategory SurfaceRscCategory   = createCategory( "SURFACE",   400 ); 
    public static ResourceCategory UpperAirRscCategory  = createCategory( "UPPER_AIR", 500 );
    public static ResourceCategory PGENRscCategory      = createCategory( "PGEN",      600 );
    public static ResourceCategory MiscRscCategory      = createCategory( "MISC",      700 );
    public static ResourceCategory EnsembleRscCategory  = createCategory( "ENSEMBLE",  800 );
    public static ResourceCategory OverlayRscCategory   = createCategory( "OVERLAY",   900 );
    public static ResourceCategory SpaceRscCategory      = createCategory( "SOLARIMAGE", 850 );
	
    private static int nextCatOrder = OverlayRscCategory.order+100;
    
    public static ResourceCategory NullCategory = new ResourceCategory( "NULL", -1 );
    
	// NOTE : These are available if the users don't like the SURFACE/UPPER_AIR
	// categories as configured in the resourceDefinitions file.
	// 
//	SurfaceFcstRscCategory =   "SURF_FCST";
//  UpperAirFcstRscCategory =  "UAIR_FCST";
//	 SurfaceObsRscCategory =   "SURF_OBS";
//	 UpperAirObsRscCategory =  "UAIR_OBS";
	// IMAGE, MODEL, WATCH/WARNING

	private String catName;
	private int    order;
	

	private ResourceCategory( String name, int ord ) {
		catName = name;
		order   = ord;
	}
	
	public static ResourceCategory getCategory( String name ) {
		ResourceCategory rc = catMap.get( name.trim().toUpperCase() ); 
		return (rc == null ? ResourceCategory.NullCategory : rc );
	}
	
	public static ResourceCategory createCategory( String name ) {
		return createCategory( name, nextCatOrder++ );
	}
	
	public static ResourceCategory createCategory( String name, int ord ) {
        name = String.valueOf(name).toUpperCase().trim();
        ResourceCategory rCat = catMap.get( name );
        
        if( rCat == null ) {
        	rCat = new ResourceCategory( name, ord );
        	catMap.put( name, rCat );
        }
        return rCat;
	}
	
	public String getCategoryName() {
		return catName;
	}
	
	public Boolean isPgenCategory() {
		return this == PGENRscCategory;
	}

	public String toString() {
		return catName;
	}
	
	public static ResourceCategory[] values() {
    	ResourceCategory[] cats = catMap.values().toArray(
                new ResourceCategory[catMap.size()]);
        Arrays.sort(cats);
        return cats;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ResourceCategory other = (ResourceCategory) obj;
        if (catName == null) {
            if (other.catName != null) {
                return false;
            }
        } else if (!catName.equals(other.catName)) {
            return false;
        }
        return true;
    }

	@Override
	public int compareTo(ResourceCategory c) {
		return order - c.order;
	}
}