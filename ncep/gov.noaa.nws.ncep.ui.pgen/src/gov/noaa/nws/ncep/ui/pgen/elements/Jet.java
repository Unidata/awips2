/*
 * gov.noaa.nws.ncep.ui.pgen.elements.Jet
 * 
 * 23 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for Jet element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/09		#135		B. Yin   	Initial Creation.
 * 08/09		#135		B. Yin   	Handle FL text relative locations.
 * 12/10		#366		B. Yin		Add a methed to add hash
 *
 * </pre>
 * 
 * @author	B. Yin
 */
@ElementOperations ( {Operation.CONNECT, Operation.COPY_MOVE, Operation.EXTRAPOLATE, Operation.FLIP,
	                  Operation.DELETE_PART, Operation.DELETE_POINT, Operation.ADD_POINT, Operation.MODIFY} )
public class Jet extends DECollection {

	private IJetTools snapTool;
	
	/**
	 * public constructor
	 */
	public Jet(){
		
		super("Jet");
		
		//create jet line.
		// attributes should be obtained from the setting table.
		JetLine jetLine = new JetLine();
		add(jetLine);
	}
	
	public Jet(IAttribute attr, ArrayList<Coordinate> points){
		
		super("Jet");
		
		//create jet line.
		// attributes should be obtained from the setting table.
		JetLine jetLine = new JetLine();
		jetLine.update(attr);

		jetLine.setLinePoints(points);
	//	jetLine.setLineWidth(4.0f);
	//	jetLine.setSizeScale(2.5);
		add(jetLine);

	}
	
	/**
	 * get the snap tool 
	 * @return snapTool
	 */
	public IJetTools getSnapTool(){
		return snapTool;
	}	
	
	/**
	 * set the snap tool
	 * @param snapTool
	 */
	public void setSnapTool(IJetTools snapTool){
		
		if ( this.snapTool == null ){
			this.snapTool = snapTool;

			Iterator<DrawableElement> it = createDEIterator();
			while ( it.hasNext() ){
				DrawableElement de = it.next();
				if ( de instanceof JetText  &&
					 ((JetText)de).getLatLonFlag() ){
					
					// when elements are loaded from XML files, the locations 
					// of fl texts are lat/lon pairs,
					// so they need to be converted to relative locations to the barbs.
					((JetText)de).setLocation(((JetText)de).getLoc()); 
					((JetText)de).setLatLonFlag(false);
				}
				
			}
		}
		else {
		
			this.snapTool = snapTool;
		}
	}
	
	/**
	 * Add the input barb to the jet and do snap
	 * @param dec
	 */
	public void addBarb( DECollection dec ){
		
		add(dec);
		
		if ( snapTool != null ){
			snapTool.snapJet(dec, this);
		}
	}
	
	public void addHash( JetHash hash ){

		add ( hash);
		
		if ( snapTool != null ){
			snapTool.snapHash(hash, hash.getLocation(), this);
		}
		
	}
	
	/**
	 * get the jet line
	 * @return jet line
	 */
	public JetLine getJetLine(){
		return (JetLine)getPrimaryDE();
	}
	
	public void removeAllHash(){
		
		Iterator<AbstractDrawableComponent> it = compList.iterator();
		while(it.hasNext()){
			AbstractDrawableComponent adc =it.next();
			if ( adc instanceof Vector 
					&& ((Vector)adc).getVectorType() == VectorType.HASH_MARK ){
				it.remove();
			}
		}
	}
	
	@Override
	/**
	 * make a deep copy of the Jet
	 */
	public Jet copy(){
		
		Jet newJet = new Jet();
	
		newJet.remove(newJet.getJetLine());
		
		newJet.setSnapTool(snapTool);
		
		//make copies of all components
		Iterator<AbstractDrawableComponent> iterator = getComponentIterator();

		while ( iterator.hasNext()){
			newJet.addElement(iterator.next().copy());
		}
		
		newJet.setPgenCategory(this.getPgenCategory());
		newJet.setPgenType(this.getPgenType());
		newJet.setParent(this.getParent());
		
		return newJet;
	}

	/**
	 * Class for jet line
	 * @author bingfan
	 *
	 */
	@ElementOperations ( {Operation.CONNECT, Operation.DELETE_PART, Operation.DELETE_POINT, 
		 Operation.MODIFY} )
	public class JetLine extends Line{
		
		/**
		 * private constructor
		 */
		private JetLine(){
			super();
			setPgenCategory("Lines");
			setPgenType("FILLED_ARROW");
			setParent(Jet.this);
		}
		
		public JetLine(Coordinate[] range, Color[] colors,
				float lineWidth, double sizeScale, boolean closed, boolean filled,
				ArrayList<Coordinate> linePoints, int smoothFactor, FillPattern fillPattern,
				String pgenCategory, String pgenType) {
			
				super(range, colors, lineWidth, sizeScale, closed, filled,
					linePoints, smoothFactor, fillPattern, pgenCategory, pgenType);
		}
		
		@Override
		/**
		 * set the line points and do snap
		 */
		public void setPoints(ArrayList<Coordinate> pts ){
			setPointsOnly(pts);
			if ( snapTool != null ){
				snapTool.snapJet((Jet)parent);
			}
		}
		
		@Override
		/**
		 * make a deep copy of the jet line
		 */
		public JetLine copy(){
			
			Line newLine = (Line)super.copy();
			JetLine newJetLine = new JetLine();
			newJetLine.setParent(parent);
		
			newJetLine.update(newLine);
			newJetLine.setLinePoints(newLine.getPoints());
			
			return newJetLine;
			
		}
   	}
	
	/**
	 * Class for jet barb
	 * @author bingfan
	 *
	 */
	public class JetBarb extends Vector {
		
	//	private Jet jetParent;
		
		/**
		 * private constructor
		 */
		private JetBarb (){
		}
		
		/**
		 * public constructor
		 * @param range
		 * @param colors
		 * @param lineWidth
		 * @param sizeScale
		 * @param clear
		 * @param location
		 * @param vc
		 * @param speed
		 * @param direction
		 * @param arrowHeadSize
		 * @param directionOnly
		 * @param pgenCategory
		 * @param pgenType
		 */
		public JetBarb ( Coordinate[] range, Color[] colors,
				float lineWidth, double sizeScale, Boolean clear,
				Coordinate location, VectorType vc,
				double speed, double direction, double arrowHeadSize, 
				boolean directionOnly, String pgenCategory, String pgenType ) {
			
			super(range, colors, lineWidth, sizeScale, clear, location, vc, speed,
					direction, arrowHeadSize, directionOnly, pgenCategory, pgenType);
					
		}
		
		/**
		 * snap the barb on the jet
		 */
		@Override
		public void setLocation(Coordinate location) {
			
			if ( parent != null ){
				
				if ( snapTool != null ){
					snapTool.snapJet(parent, (Jet)parent.getParent());
				}
				else {
					super.setLocation(location);
				}
			}
		}
		
		public void setSpeedOnly(double spd){
			super.setSpeed(spd);
		}
		
		@Override
		public void setSpeed(double spd ){
			
			super.setSpeed(spd);
			if ( parent != null ){
				
				if ( snapTool != null ){
					if ( parent.getParent() instanceof Jet ){
						snapTool.snapJet(parent, (Jet)parent.getParent());
					}
				}
			}
		}
		
		@Override
		/**
		 * make a deep copy of the jet barb
		 */
		public JetBarb copy(){
			
			Vector newBarb = (Vector)super.copy();
			JetBarb newJetBarb = new JetBarb();
			
			newJetBarb.setParent(parent);
			
			newJetBarb.update(newBarb);
			newJetBarb.setLocationOnly(newBarb.getLocation());
			
			newJetBarb.setPgenCategory( newBarb.getPgenCategory() );
			newJetBarb.setPgenType( newBarb.getPgenType() );

			newJetBarb.setVectorType( newBarb.getVectorType() );
			
			return newJetBarb;
			
		}
		
		/**
		 * Return the flight level text of the barb.
		 * It can be null.
		 * @return
		 */
		public JetText getFlightLvl(){
			DECollection windInfo = (DECollection)parent;
			Iterator<AbstractDrawableComponent> it = windInfo.getComponentIterator();
			
			JetText jt = null;
			while(it.hasNext()){
				AbstractDrawableComponent adc = it.next();
				if ( adc instanceof JetText ){
					jt = (JetText)adc;
				}
			}
			
			return jt;
		}
	}
	/**
	 * Class for jet hash
	 * @author bingfan
	 *
	 */
	public class JetHash extends Vector {
				
		/**
		 * private constructor
		 */
		private JetHash (){
			setParent(Jet.this);
		}
		
		/**
		 * public constructor
		 * @param range
		 * @param colors
		 * @param lineWidth
		 * @param sizeScale
		 * @param clear
		 * @param location
		 * @param vc
		 * @param speed
		 * @param direction
		 * @param arrowHeadSize
		 * @param directionOnly
		 * @param pgenCategory
		 * @param pgenType
		 */
		public JetHash ( Coordinate[] range, Color[] colors,
				float lineWidth, double sizeScale, Boolean clear,
				Coordinate location, VectorType vc,
				double speed, double direction, double arrowHeadSize, 
				boolean directionOnly, String pgenCategory, String pgenType ) {
			
			super(range, colors, lineWidth, sizeScale, clear, location, vc, speed,
					direction, arrowHeadSize, directionOnly, pgenCategory, pgenType);
					
			setParent( Jet.this);

		}
		
		/**
		 * snap the barb on the jet
		 */
		@Override
		public void setLocation(Coordinate location) {
			
			if ( ((Jet)parent).search(this) != null ){
				
				if ( snapTool != null ){
					snapTool.snapHash(this, location, (Jet)parent);
				}
				else {
					super.setLocation(location);
				}
			}
		}
		
		@Override
		/**
		 * make a deep copy of the jet hash
		 */
		public JetHash copy(){
			
			Vector newHash = (Vector)super.copy();
			JetHash newJetHash = new JetHash();
			
			newJetHash.setParent(parent);
			
			newJetHash.update(newHash);
			newJetHash.setLocationOnly(newHash.getLocation());
			
			newJetHash.setPgenCategory( newHash.getPgenCategory() );
			newJetHash.setPgenType( newHash.getPgenType() );

			newJetHash.setVectorType( newHash.getVectorType() );
			
			return newJetHash;
			
		}
	}
	
	public class JetText extends Text {

		// flag to indicate whether lat/lon are used for the location
		private boolean latLonFlag;
		
		public JetText() {
		}
		
		/**
		 * Constructor to set all attributes of the Text element
		 * @param fontName Name of the font to display
		 * @param fontSize Size of the font to display
		 * @param justification Specified where text is relative to position
		 *                       @see gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification
		 * @param position - location specifying where to display the text String
		 * @param rotation - display text at this rotation angle relative to +X direction.
		 * @param rotationRelativity - rotation angle is relative to North or Screen coordinates
		 *                       @see gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation
		 * @param text  The text Strings to display.  The text strings in each array element are displayed 
		 *               on a different line
		 * @param style  The font style to use.  @see gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle
		 * @param textColor  Color in which text string should be displayed
		 * @param offset Half-character offset in the X direction applied to @see #position
		 * @param offset2 Half-character offset in the Y direction applied to @see #position
		 * @param mask  Create a background mask behind the text Strings
		 * @param outline  Type of display around the text Strings.
		 */
		public JetText(Coordinate[] range, String fontName, float fontSize,
				TextJustification justification, Coordinate position,
				double rotation, TextRotation rotationRelativity, String[] text,
				FontStyle style, Color textColor, int offset, int offset2,
				boolean mask, DisplayType outline, String pgenCategory, String pgenType ) {
			super(range, fontName, fontSize, justification, position,
				  rotation, rotationRelativity, text, style, textColor, 
				  offset, offset2, mask, outline, pgenCategory, pgenType);
		}

		@Override
		/**
		 * Transform lat/lon to deltaX/deltaY relative to the barb.
		 */
		public void setLocationOnly(Coordinate loc){
			if ( snapTool != null ){
				super.setLocationOnly(snapTool.latLon2Relative(loc, (Vector)this.getParent().getPrimaryDE()));
			}
			else {
				super.setLocationOnly(loc);
			}
			
		}
		
		@Override
		/**
		 * Transform deltaX/deltaY to lat/lon
		 */
		public Coordinate getLocation(){
			if ( snapTool != null ){
				return snapTool.relative2LatLon(super.getLocation(), (Vector)this.getParent().getPrimaryDE());
			}
			else {
				return super.getLocation();
			}
		}
		
		/**
		 * Get location without transformation.
		 * @return
		 */
		private Coordinate getLoc(){
			return location;
		}
		
		@Override
		public JetText copy() {

			/*
			 * create a new Text object and initially set its attributes to this one's
			 */
			JetText newText = new JetText();
			newText.update(this);
			newText.setParent(this.getParent());
	
			/*
			 * Set new Color, Strings and Coordinate so that we don't just set references to this
			 * object's attributes.
			 */
			newText.setColors(new Color[] { new Color(this.getColors()[0].getRed(),
		            this.getColors()[0].getGreen(),
		            this.getColors()[0].getBlue())   });
			newText.setLocation(new Coordinate(this.getLocation()) );
			newText.setFontName(new String(this.getFontName()));
			
			/*
			 * new text Strings are created and set, so we don't just set 
			 * references
			 */
			String[] textCopy = new String[this.getText().length];
			for (int i=0; i<this.getText().length; i++) {
				textCopy[i] = new String(this.getText()[i]);
			}
			newText.setText(textCopy);
			
			newText.setPgenCategory(new String(this.getPgenCategory()));
			newText.setPgenType(new String(this.getPgenType()));
			
			
			return newText;
		}

		/**
		 * @param latLonFlag the latLonFlag to set
		 */
		public void setLatLonFlag(boolean latLonFlag) {
			this.latLonFlag = latLonFlag;
		}

		/**
		 * @return the latLonFlag
		 */
		public boolean getLatLonFlag() {
			return latLonFlag;
		}
	}
}
