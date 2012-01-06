/**
 * 
 */
package gov.noaa.nws.ncep.common.dataplugin.mcidas;

import java.awt.geom.Point2D;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;

import org.apache.commons.codec.binary.Base64;
import org.geotools.parameter.DefaultParameterDescriptor;
import org.geotools.parameter.DefaultParameterDescriptorGroup;
import org.geotools.referencing.operation.MathTransformProvider;
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.opengis.parameter.ParameterDescriptor;
import org.opengis.parameter.ParameterDescriptorGroup;
import org.opengis.parameter.ParameterNotFoundException;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.Projection;

import edu.wisc.ssec.mcidas.AREAnav;
import edu.wisc.ssec.mcidas.McIDASException;
import edu.wisc.ssec.mcidas.McIDASUtil;

/**
 * 
 * McIdas native satellite projections as defined in the NAV block of McIdas Area files.
 * @author sgilbert
 *
 */
public class McidasAreaProjection extends MapProjection {

	//  Mcidas area file NAV Block encoded in BASE64
	protected String encodedNavBlock = null;
	
	protected AREAnav gvarNavigation = null;
	
	protected McidasAreaProjection(ParameterValueGroup values)
			throws ParameterNotFoundException {
		super(values);

		encodedNavBlock = Provider.stringValue(Provider.NAV_BLOCK_BASE64, values);
		if ( encodedNavBlock != null && !encodedNavBlock.isEmpty() ) {

			//   decode NAV block
			Base64 b64 = new Base64();
			byte[] navBlock = b64.decode(encodedNavBlock.getBytes());
			
			// convert NAV block to int[]
			DataInputStream dis = new DataInputStream( new ByteArrayInputStream(navBlock) );
			
			int[] nav = new int[navBlock.length/4];
			try {
				for ( int i=0; i<nav.length; i++ )
					nav[i]=dis.readInt();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
				
			/*   
			 * taken from method flipnav in edu.wisc.ssec.mcidas.AreaFile.java  
    	     */
			flipnav(nav);
			
			try {
				gvarNavigation = AREAnav.makeAreaNav( nav );
			} catch (McIDASException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			//double[] sub = gvarNavigation.getSubpoint();
			//System.out.println("SUBPOINT= "+sub[0]+", "+sub[1]);
		}
		
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/* (non-Javadoc)
	 * @see org.geotools.referencing.operation.projection.MapProjection#getParameterDescriptors()
	 */
	@Override
	public ParameterDescriptorGroup getParameterDescriptors() {
		return Provider.PARAMETERS;
	}
	
    /**
     * {@inheritDoc}
     */
    @Override
    public ParameterValueGroup getParameterValues() {
        final ParameterValueGroup values = super.getParameterValues();
        if ( encodedNavBlock != null ) {
        	values.parameter( Provider.NAV_BLOCK_BASE64.getName().getCode() ).setValue(encodedNavBlock);
        }
        return values;
    }

	/* (non-Javadoc)
	 * @see org.geotools.referencing.operation.projection.MapProjection#inverseTransformNormalized(double, double, java.awt.geom.Point2D)
	 */
	@Override
	protected Point2D inverseTransformNormalized(double x, double y,
			Point2D ptDst) throws ProjectionException {
		
		double[][] in = new double[2][1];
		in[0][0] = x;
		in[1][0] = -y;     // Mcidas Image represented top to bottom.  reverse order
		double[][] out = gvarNavigation.toLatLon(in);
	//	System.out.println("LineElm in = "+x+", "+ y);
	//	System.out.println("LAT/LON out = "+out[0][0]+", "+ out[1][0]);
		double lat = Math.toRadians(out[0][0]);
		double lon = Math.toRadians(out[1][0]);
		
        if (ptDst != null) {
            ptDst.setLocation(lon, lat);
            return ptDst;
        }
        return new Point2D.Double(lon, lat);
	}

	/* (non-Javadoc)
	 * @see org.geotools.referencing.operation.projection.MapProjection#transformNormalized(double, double, java.awt.geom.Point2D)
	 */
	@Override
	protected Point2D transformNormalized(double x, double y,
			Point2D ptDst) throws ProjectionException {
		
		double[][] in = new double[2][1];
		in[0][0] = Math.toDegrees(y);    // Latitude
		in[1][0] = Math.toDegrees(x);    // Longitude
		double[][] out = gvarNavigation.toLinEle(in);
	//	System.out.println("LAT/LON in = "+in[0][0]+", "+ in[1][0]);
	//	System.out.println("LineElm out = "+out[0][0]+", "+ out[1][0]);
		
		// Mcidas Image represented top to bottom.  reverse order of y-axis
        if (ptDst != null) {
            ptDst.setLocation(out[0][0], -out[1][0]);
            return ptDst;
        }
        return new Point2D.Double(out[0][0], -out[1][0]);
	}
	
	/**
	 * Selectively flip the bytes of words in nav block.
	 * This method was copied verbatim from 
	 * method flipnav in edu.wisc.ssec.mcidas.AreaFile.java
	 *
	 * @param nav array of nav parameters
	 */
	private void flipnav(int[] nav) {

		// first word is always the satellite id in ASCII
		// check on which type:

		if (nav[0] == AREAnav.GVAR) {

			McIDASUtil.flip(nav, 2, 126);
			McIDASUtil.flip(nav, 129, 254);
			McIDASUtil.flip(nav, 257, 382);
			McIDASUtil.flip(nav, 385, 510);
			McIDASUtil.flip(nav, 513, 638);
		}

		else if (nav[0] == AREAnav.DMSP) {
			McIDASUtil.flip(nav, 1, 43);
			McIDASUtil.flip(nav, 45, 51);
		}

		else if (nav[0] == AREAnav.POES) {
			McIDASUtil.flip(nav, 1, 119);
		}

		else if (nav[0] == AREAnav.GMSX) {}
		else {
			McIDASUtil.flip(nav, 1, nav.length - 1);
		}

		return;
	}

	/**
	 * Returns a hash value for this projection.
	 */
	@Override
	public int hashCode() {
		final long code = encodedNavBlock.hashCode();
		return ((int)code ^ (int)(code >>> 32)) + 37*super.hashCode();
	}
	
    /**
     * Compares the specified object with this map projection for equality.
     */
    @Override
    public boolean equals(final Object object) {
        if (object == this) {
            // Slight optimization
            return true;
        }
        if (super.equals(object)) {
            final McidasAreaProjection that = (McidasAreaProjection) object;
            return this.encodedNavBlock.equals( that.encodedNavBlock );
        }
        return false;
    }
    
	
    public static class Provider extends AbstractProvider {
        /**
         * For cross-version compatibility.
         */
        private static final long serialVersionUID = -5886510621481710072L;

        static final DefaultParameterDescriptor NAV_BLOCK_BASE64 = DefaultParameterDescriptor.create("NAV_BLOCK_BASE64", "Area NAV Block in base64", String.class, "", true);
        
        static final ParameterDescriptorGroup PARAMETERS = new DefaultParameterDescriptorGroup(
        		"MCIDAS_AREA_NAV", 
        		new ParameterDescriptor[] { SEMI_MAJOR, SEMI_MINOR, NAV_BLOCK_BASE64 }
        		);
        
        /**
         * Constructs a new provider.
         */
        public Provider() {
            super(PARAMETERS);
        }

        /**
         * Returns the operation type for this map projection.
         */
        @Override
        public Class<Projection> getOperationType() {
            return Projection.class;
        }
        
        /*
         * return the String value of the given PARAMETER in the group
         */
        protected static String stringValue(final ParameterDescriptor<?> param,
                							final ParameterValueGroup group)
        										throws ParameterNotFoundException {
        	return MathTransformProvider.stringValue(Provider.NAV_BLOCK_BASE64, group);
        }

        /**
         * Creates a transform from the specified group of parameter values.
         *
         * @param  parameters The group of parameter values.
         * @return The created math transform.
         * @throws ParameterNotFoundException if a required parameter was not found.
         */
        protected MathTransform createMathTransform(final ParameterValueGroup parameters)
                throws ParameterNotFoundException
        {
            return new McidasAreaProjection(parameters);
        }
    }

}
