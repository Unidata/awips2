/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.HashMap;

/**
 * Define a Product Information Class.
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
public class ProductInfo {

    /** The fields */
    private HashMap<String, String> properties;

    public ProductInfo () {
        this.properties = new HashMap<String, String>();
    }
   
    public ProductInfo ( HashMap<String, String> props ) {
        this.properties = props;
    }
    
    public Object getProperty (String propertyName ) {
        return  properties.get(propertyName);
    }
    
    public HashMap<String, String> getProperties () {
        return  properties;
    }  
    
    public void setProperty ( String name,  String value) {
		properties.put ( name, value );
	}
    
    public void setProperties ( HashMap<String, String> props ) {
    	properties = new HashMap<String, String>( props );
    }         
}