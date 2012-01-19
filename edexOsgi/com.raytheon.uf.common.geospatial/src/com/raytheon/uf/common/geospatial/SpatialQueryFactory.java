/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.geospatial;


/**
 * SpatialQueryFactory
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 7, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SpatialQueryFactory {
	
	public static String CAVE_FACTORY = "com.raytheon.viz.core.spatial.SpatialDbQuery";
	public static String EDEX_FACTORY = "com.raytheon.edex.common.SpatialDbQuery";
	public static String PROP = "eclipse.product";
	public static String PROP_VAL = "com.raytheon.viz.product.awips.CAVE";

    public static ISpatialQuery create() throws SpatialException {
    	
    	ISpatialQuery factory = null;
    	String prop = System.getProperty(PROP);
    	
    	try {
			if (prop != null && prop.equals(PROP_VAL)) {
				factory = (ISpatialQuery) Class.forName(CAVE_FACTORY).newInstance();
			} else {
				factory = (ISpatialQuery) Class.forName(EDEX_FACTORY).newInstance();
			}
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw new SpatialException("SpatialQueryFactory: Property"+prop+" "+e.getMessage(), e);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new SpatialException("SpatialQueryFactory: Property"+prop+" "+e.getMessage(), e);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			throw new SpatialException("SpatialQueryFactory: Property"+prop+" "+e.getMessage(), e);
		}
    	
    	return factory;
    }
    
    public static String getType() {
        
        String prop = System.getProperty(PROP);
        String type = null;
        
        try {
            if (prop != null && prop.equals(PROP_VAL)) {
                type = "CAVE";
            } else {
                type = "EDEX";
            }
            return type;
        }
        catch(Exception e) {
           e.printStackTrace();
        }
        return null;
    }
}
