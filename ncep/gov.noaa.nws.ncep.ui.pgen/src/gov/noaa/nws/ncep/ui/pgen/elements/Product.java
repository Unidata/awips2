 /*
 * Product
 * 
 * Date created: 15 January 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.List;
import java.util.ArrayList;


/**
 * Define a Product Class - containing a list of Layers and  properties. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/08					J. Wu   	Initial Creation.
 * 07/09		#131		J. Wu   	Added clear().
 * 09/09		#191		J. Wu   	Added more attributes.
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 * 
 * @author J. Wu
 * 
 */
public class Product {

    /** The fields */
    private String name;
    private String type;
    private String forecaster;
    private String center;
    private ProductInfo info;
    private ProductTime time;
    private String inputFile;    
    private String outputFile;    
    private List<Layer> layers;
    private boolean onOff;
    private boolean inUse;
    private boolean useFile;
    private boolean saveLayers;
	
	public Product() {
        super();
        name = "Default";
        type = "Default";
        forecaster = "Default";
        setCenter("Default");
        info = new ProductInfo();
        time = new ProductTime();
        layers = new ArrayList<Layer>();
        onOff = true;
        inUse = true;
        inputFile = null;
        outputFile = null;
        useFile = false;
        saveLayers = false;
	}
	
	public Product( String myName, String myType, String myForecaster, ProductInfo myInfo,
			       ProductTime myTime, ArrayList<Layer> myLayers ) {
        name = myName;
        type = myType;
        forecaster = myForecaster;
        setCenter("Default");
        info = myInfo;
        time = myTime;
        layers = myLayers;
        onOff = true;
        inUse = true;      
        inputFile = null;
        outputFile = null;
        useFile = false;
        saveLayers = false;
	}
		
	
	public String getName() {
		return name;
	}	
	public void setName(String myName) {
		name = myName;
	}
	
	public void setType(String type) {
		this.type = type;
	}

	public String getType() {
		return type;
	}

	public String getForecaster() {
		return forecaster;
	}
	
	public void setForecaster(String myForecaster) {
		forecaster = myForecaster;
	}
	
	/**
	 * @return the info
	 */
	public ProductInfo getInfo() {
		return info;
	}

	/**
	 * @param info the info to set
	 */
	public void setInfo(ProductInfo myInfo) {
		info = myInfo;
	}

	public ProductTime getTime() {
		return time;
	}
	
	public void setTime( ProductTime myTime ) {
		time = myTime;
	}
	
	public List<Layer> getLayers() {
		return layers;
	}
	
	public void setLayers( List<Layer> myLayers) {
		layers = myLayers;
	}		

	public Layer getLayer( int index ) {
		return layers.get( index );
	}

	public Layer getLayer( String layerName ) {
		for ( Layer ly : layers ) {
			if ( ly.getName().equals( layerName ) ) {
		        return ly;			 
			}
		}
		
		return null;
	}

	public void addLayer( Layer layer ) {
		layers.add( layer );
	}
	
	public void addLayer( int index, Layer layer ) {
		layers.add( index, layer );
	}	
	
	public void removeLayer( int index ) {
		layers.remove( index );
	}
	
	/**
	 * Removes the specified layer from this product
	 * @param lyr The Layer to remove
	 */
	public void removeLayer( Layer lyr ) {
		layers.remove(lyr);
	}
	
	/**
	 * Removes all layers.
	 */
	public void clear( ) {
		layers.clear();
	}
	
	public String makeProduct() {
		return "Make Product .......... " + name ;
	}
	
	public String toString() {
		StringBuilder	result = new StringBuilder( "\n" );
        result.append( "name:\t\t" + name + "\n" );
        result.append( "type:\t\t" + type + "\n" );       
        result.append( "forecaster:\t\t" + forecaster + "\n" );
        result.append( "center:\t\t" + center + "\n" );
        result.append( "inputFile:\t\t" + inputFile + "\n" );
        result.append( "outputFile:\t\t" + outputFile + "\n" );
        result.append( "info:\t\t" + info + "\n" );         
        result.append( "time:\t\t" +time + "\n" ); 
        result.append( "OnOff:\t\t" + onOff + "\n" );
        result.append( "InUse:\t\t" + inUse + "\n" );
        
        result.append( "\nTotal Layers:\t" + layers.size() + "\n" );
        
        int ii = 0;
        for ( Layer ly:layers ) {
           	result.append( "Layer:\t" + ii );			
        	result.append( ly );
		    result.append ( "\n" );
		    ii++;
		}
		
		return result.toString();	    
	}
				
	/**
	 * Checks if this product contains the specified Layer
	 * @param lyr - Layer to check
	 * @return true, if lyr exists in this product
	 */
	public boolean contains(Layer lyr) {
		return layers.contains(lyr);
	}

	/**
	 * Test if this product contains any layers
	 * @return true, if layer is empty
	 */
	public boolean isEmpty() {
		return layers.isEmpty();
	}
	
	/**
	 * Deep copy of the product
	 */
	public Product copy(){
		
		Product prd = new Product();
	    
		prd.setName( this.getName() );
		prd.setType( this.getType() );
		prd.setForecaster( this.getForecaster() );
		prd.setCenter( this.getCenter() );
		prd.setInfo( this.getInfo() );
		prd.setTime( this.getTime() );
		prd.setInputFile( this.getInputFile() );
		prd.setOutputFile( this.getOutputFile() );
		prd.setOnOff( this.isOnOff() );
		prd.setInUse( this.isInUse() );
		prd.setUseFile( this.isUseFile() );
        
        for ( Layer lyr : this.getLayers() ) {
        	prd.addLayer( lyr.copy() );
        }
        
		return prd;
	}


	/**
	 * Setter/Getters of all attributes
	 */
	public void setOnOff(boolean isOnOff) {
		this.onOff = isOnOff;
	}

	public boolean isOnOff() {
		return onOff;
	}

	public void setInUse(boolean inUse) {
		this.inUse = inUse;
	}

	public boolean isInUse() {
		return inUse;
	}

	public void setCenter(String center) {
		this.center = center;
	}

	public String getCenter() {
		return center;
	}

	public void setInputFile(String fileName) {
		this.inputFile = fileName;
	}

	public String getInputFile() {
		return inputFile;
	}
	
	public void setOutputFile(String fileName) {
		this.outputFile = fileName;
	}

	public String getOutputFile() {
		return outputFile;
	}


	public void setUseFile(boolean useFile) {
		this.useFile = useFile;
	}

	public boolean isUseFile() {
		return useFile;
	}

	/**
	 * @param saveLayers the saveLayers to set
	 */
	public void setSaveLayers(boolean saveLayers) {
		this.saveLayers = saveLayers;
	}

	/**
	 * @return the saveLayers
	 */
	public boolean isSaveLayers() {
		return saveLayers;
	}
	
}
 