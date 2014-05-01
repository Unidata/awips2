/*
 * Layer
 * 
 * Date created: 22 October 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.Iterator;
import java.util.List;
import java.awt.Color;

/**
 * Define a Layer - containing a list of Drawables and properties.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/22/08					J. Wu   	Initial Creation.
 * 06/20/09		#116		B. Yin		Extends from DECollection
 * 07/09		#131		J. Wu   	Added more for layering control
 * 11/13		#1049		B. Yin		Added meta info in layer.
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 * 
 */
public class Layer extends DECollection{

    /** The fields */
    private boolean onOff;
    private boolean monoColor;
    private Color color;
    private boolean filled;
    private boolean inUse;
    private boolean changeMade;
    private String inputFile;
    private String outputFile;
    private String metaInfo;
    
    /**
	 *  Default constructor
	 */
	public Layer() {
		super("Default");
		this.onOff = true;
		this.monoColor = false;
		this.color = Color.yellow;
		this.filled = false;
		this.inUse = true;
        this.changeMade= false;	
        this.inputFile= null;	
        this.outputFile= null;	
    }
    
	/**
	 * @param name
	 * @param onOff
	 * @param mode
	 * @param color
	 * @param fillMode
	 * @param components
	 */
	public Layer(String name, boolean onOff, boolean mode, Color color,
			boolean fillMode, List<AbstractDrawableComponent> components ) {
		super(name);
		this.onOff = onOff;
		this.monoColor = mode;
		this.color = color;
		this.filled = fillMode;
		this.compList = components;
		this.inUse = true;
        this.changeMade= false;	
        this.inputFile= null;	
        this.outputFile= null;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return collectionName;
	}
	
	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.collectionName = name;
	}
	
	/**
	 * @return the onOffgetElement
	 */
	public boolean isOnOff() {
		return onOff;
	}
	
	/**
	 * @param onOff the onOff to set
	 */
	public void setOnOff(boolean onOff) {
		this.onOff = onOff;
	}
	
	/**
	 * @return the mode
	 */
	public boolean isMonoColor() {
		return monoColor;
	}
	
	/**
	 * @param mode the mode to set
	 */
	public void setMonoColor(boolean mode) {
		this.monoColor = mode;
	}
	
	/**
	 * @return the color
	 */
	public Color getColor() {
		return color;
	}
	
	/**
	 * @param color the color to set
	 */
	public void setColor(Color color) {
		this.color = color;
	}
	
	/**
	 * @return the fillMode
	 */
	public boolean isFilled() {
		return filled;
	}
	
	/**
	 * @param fillMode the fillMode to set
	 */
	public void setFilled(boolean fillMode) {
		this.filled = fillMode;
	}
	
	/**
	 * @param isInUse the isInUse to set
	 */
	public void setInUse(boolean isInUse) {
		this.inUse = isInUse;
	}

	/**
	 * @return the isInUse
	 */
	public boolean isInUse() {
		return inUse;
	}

	/**
	 * @param changeMade the changeMade to set
	 */
	public void setChangeMade(boolean changeMade) {
		this.changeMade = changeMade;
	}

	/**
	 * @return the changeMade
	 */
	public boolean isChangeMade() {
		return changeMade;
	}

	/**
	 * @param inputFile the inputFile to set
	 */
	public void setInputFile(String inputFile) {
		this.inputFile = inputFile;
	}

	/**
	 * @return the inputFile
	 */
	public String getInputFile() {
		return inputFile;
	}

	/**
	 * @param outputFile the outputFile to set
	 */
	public void setOutputFile(String outputFile) {
		this.outputFile = outputFile;
	}

	/**
	 * @return the outputFile
	 */
	public String getOutputFile() {
		return outputFile;
	}

	/**
	 * @return the drawables
	 */
	public List<AbstractDrawableComponent> getDrawables() {
		return compList;
	}
	
	/**
	 * @param drawables the drawables to set
	 */
	public void setDrawables(List<AbstractDrawableComponent> drawables) {
		for (AbstractDrawableComponent adc : drawables ){
			adc.setParent(this);
		}
		this.compList = drawables;
	}
	
	/**
	 * @param the index and the element
	 */
	public void removeElement(int index ) {
		compList.remove( index );
	}
	
	/**
	 * remove all elements
	 */
	public void removeAllElements( ) {
		compList.clear();
	}

	/**
	 * @return the element at the index
	 */
	public AbstractDrawableComponent getElement(int index) {
		return compList.get( index );
	}	
     
	/**
	 * @return the string
	 */
	public String toString() {
		StringBuilder	result = new StringBuilder( "\n" );
        result.append( "name:\t\t" + this.getName() + "\n" );
        result.append( "onOff:\t\t" + this.onOff + "\n" );
        result.append( "colorMode:\t\t" + this.monoColor + "\n" );    
        result.append( "color:\t\t" + this.color + "\n" ); 
        result.append( "fillMode:\t\t" + this.filled + "\n" );             
        result.append( "inUse:\t\t" + this.inUse + "\n" ); 
        result.append( "changeMade:\t\t" + this.changeMade + "\n" );             
        result.append( "inputFile:\t\t" + this.inputFile + "\n" );
        result.append( "outputFile:\t\t" + this.outputFile + "\n" );
       
        result.append( "\nTotal DrawableElements:\t" + compList.size() + "\n" );
        
		for ( int ii = 0;  ii < compList.size(); ii++ ) {
			result.append ( "\nDrawable No.  " + ii + ":\n" );
			result.append( getElement( ii ) );
		    result.append ( "\n" );
		}
		
		return result.toString();
	}
	
	@Override
	/**
	 * Deep copy of the layer
	 */
	public Layer copy(){
		
		Layer lyr = new Layer();
		
		Iterator<AbstractDrawableComponent> iterator = getComponentIterator();

		while ( iterator.hasNext()){
			lyr.addElement(iterator.next().copy());
		}

		iterator = lyr.getComponentIterator();
		while ( iterator.hasNext()){
			iterator.next().setParent( lyr );
		}
		
		lyr.setPgenCategory(this.getPgenCategory());
		lyr.setPgenType(this.getPgenType());
		lyr.setParent(this.getParent());
		
		lyr.setName( this.getName() );
		lyr.setOnOff( this.isOnOff());
		
		lyr.setMonoColor( this.isMonoColor() );
		lyr.setColor( this.getColor() );
		lyr.setFilled( this.isFilled() );
		lyr.setInUse( this.isInUse());
        lyr.setChangeMade( this.isChangeMade() );	
        lyr.setInputFile( this.getInputFile());	
        lyr.setOutputFile( this.getOutputFile());	
		
		return lyr;
	}

	public void setMetaInfo( String meta ){
		metaInfo = meta;
	}

	public String getMetaInfo() {
		return metaInfo;
	}
	
	public String getMetaInfoFromKey( String key ){
		if ( metaInfo == null || metaInfo.isEmpty() ){
			return "";
		}
		int idx1 = metaInfo.indexOf('=', metaInfo.indexOf(key));
		if ( idx1 == -1 ) return "";
		
		int idx2 = metaInfo.indexOf(';', metaInfo.indexOf(key));
		if ( idx2 == -1 ){
			idx2 = metaInfo.length();
		}
		
		return metaInfo.substring(idx1+1, idx2).trim();
	}
}
 