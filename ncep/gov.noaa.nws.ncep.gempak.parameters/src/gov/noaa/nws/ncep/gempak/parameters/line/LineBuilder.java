package gov.noaa.nws.ncep.gempak.parameters.line;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;



/**
 * *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 *  23-Oct-2009    186       Archana.S   Initial Creation 
 * </pre>
 * @author Archana.S
 * @version 1 
 */

/**
 *
 **/
public class LineBuilder {

   /**
    *Boolean flag to check if the small contour flag is set to true.
    **/
	private boolean            isSmallContourFlagSuppressed; 
	
	/** Default value for the small contour flag is false*/
	private boolean            defaultSmallContourFlagSuppressed;
	
	/**Boolean flag to check if there should be a break in  the line for a label*/
	private boolean isBreakInLineForLabel;
	
	/**Default color is 1*/
	private Integer            defaultColor;
	
	/**Default value for points filter is 0.0*/
	private Double             defaultPointsFilter;
	
	/**The amount of filtering for points in a line */
	private Double             pointsFilter;
	
	/**The default line width is 1*/
	private Integer            defaultLineWidth;
	
	/**The default labeled line; is 1*/
	private Integer            defaultLabeledLine;
	
	/**The line smoothing level*/
	private Integer            lineSmoothingLevel;
	@SuppressWarnings("unused")
	
	/**The default smoothing level is 0*/
	private Integer            defaultSmoothingLevel;
	
	/**A list of Integer objects representing line colors.*/
	private List<Integer>        lineColorsList;
	
	/**A list of line widths populated after parsing the input string*/
	private List<Integer>      lineWidthList;
	
	/**A list of line label indicators, populated after parsing the input string*/
	private List<Integer>      lineLabelPresentList;
	
	/**A list of line types(patterns) populated after parsing the input string*/
	private List<LineStyle> LineStyleList;
	
@SuppressWarnings("unused")
    
    /** An instance of the <code> LineStyle </code> enumeration*/
	private LineStyle       linePattern;
	
    /** The default line pattern is SOLID*/
    private LineStyle       defaultLinePattern;
	
    /**A HashMap of the Integer values and the corresponding <code> LineStyle </code> */
    protected HashMap<Integer,LineStyle> linePatternValuesHashMap;
    
    
    /**The<code> LineStyle </code> enumeration*/
	/*protected enum LineStyle{
									SOLID(1),
									SHORT_DASHED(2),
									MEDIUM_DASHED(3),
									LONG_DASH_SHORT_DASH(4),
									LONG_DASH(5),
									LONG_DASH_THREE_SHORT_DASHES(6),
									LONG_DASH_DOT(7),
									LONG_DASH_THREE_DOTS(8),
									MEDIUM_DASH_DOTS(9),
									DOTTED(10);
									@SuppressWarnings("unused")
									private final int value;
									
									LineStyle(int value){
										this.value = value;
									}
								}

	
*/
    /**The default constructor <code>LineBuilder</code> assigns default values to the state variables.
     **/	
	protected LineBuilder(){
		
        defaultColor                      = 1; 
        defaultLinePattern                = LineStyle.SOLID;
        defaultLineWidth                  = 1;
        defaultLabeledLine                = 0;      
        defaultSmoothingLevel             = 0;
        defaultPointsFilter               = 0.0;
        defaultSmallContourFlagSuppressed = false;
        isBreakInLineForLabel             = true;
        
		lineColorsList                    = new ArrayList<Integer>();
		LineStyleList                  = new ArrayList<LineStyle>();
		lineWidthList                     = new ArrayList<Integer>();
		lineLabelPresentList              = new ArrayList<Integer>();


		/*Assign default values to the state variables listed below*/

		setLineSmoothingLevel(0);
		setPointsFilter(defaultPointsFilter);
		setSmallContourFlagSuppressed(defaultSmallContourFlagSuppressed);
		
		linePatternValuesHashMap = new HashMap<Integer,LineStyle>();
		/**
		 *@see <code>LineBuilder.LineStyle </code>
		 **/
		linePatternValuesHashMap.put(1, LineStyle.SOLID);
		linePatternValuesHashMap.put(2, LineStyle.SHORT_DASHED);
		linePatternValuesHashMap.put(3, LineStyle.MEDIUM_DASHED);
		linePatternValuesHashMap.put(4, LineStyle.LONG_DASH_SHORT_DASH);
		linePatternValuesHashMap.put(5, LineStyle.LONG_DASHED);
		linePatternValuesHashMap.put(6, LineStyle.LONG_DASH_THREE_SHORT_DASHES);
		linePatternValuesHashMap.put(7, LineStyle.LONG_DASH_DOT);
		linePatternValuesHashMap.put(8, LineStyle.LONG_DASH_THREE_DOTS);
		linePatternValuesHashMap.put(9, LineStyle.MEDIUM_DASH_DOT);
		linePatternValuesHashMap.put(10,LineStyle.DOTTED);		
		
	}
	
	
	
	
	/**
	 * @param Integer lineSmoothingLevel
	 */
	private  void setLineSmoothingLevel(Integer lineSmoothingLevel) {
		this.lineSmoothingLevel = lineSmoothingLevel;
	}

	/**
	 * @return Integer lineSmoothingLevel
	 */
	public  Integer getLineSmoothingLevel() {
		return lineSmoothingLevel;
	}

	/**
	 * @param Double pointsFilter 
	 */
	protected  void setPointsFilter(Double pointsFilter) {
		this.pointsFilter = pointsFilter;
	}

	/**
	 * @return Double pointsFilter
	 */
	public  Double getPointsFilter() {
		return pointsFilter;
	}

	/**@return boolean isBreakInLineForLabel*/
	public boolean isBreakInLineForLabel() {
		return isBreakInLineForLabel;
	}



    /**@param boolean isBreakInLineForLabel*/
	private void setBreakInLineForLabel(boolean isBreakInLineForLabel) {
		this.isBreakInLineForLabel = isBreakInLineForLabel;
	}

	/**
	 * @param boolean isSmallContourFlagSuppressed 
	 */
	protected void setSmallContourFlagSuppressed(
			boolean isSmallContourFlagSuppressed) {
		this.isSmallContourFlagSuppressed = isSmallContourFlagSuppressed;
	}

	/**
	 * @return boolean isSmallContourFlagSuppressed
	 */
	public  boolean isSmallContourFlagSuppressed() {
		return isSmallContourFlagSuppressed;
	}


    /**
     * @param List of Integer-lineColorsList
     **/
	protected void setLineColorsList(List<Integer> lineColorsList) {
        this.lineColorsList = lineColorsList;
	}

	/**
	 * @return List of Integer-lineColorsList
	 */
	public List<Integer> getLineColorsList() {
		return lineColorsList;
	}


	/**
	 * @param List of Integer - linePatternList <p>
	 * The method <code>setLineStyleList</code> accepts a list of Integers as input.
	 * For each member of the list, it checks if a corresponding member of the <code>LineStyle</code> 
	 * enumeration exists in the HashMap and if so, it adds this LinePattern to the list of LineStyle.
	 */
	public void setLineStyleList(List<Integer> linePatternList) {
		int currentIndex;
		int patternType;
		
		for(currentIndex = 0;currentIndex < linePatternList.size();currentIndex++){
			patternType = linePatternList.get(currentIndex).intValue();
			if(patternType < 0) {
						/*TODO If the line type is a single negative number then the negative contour values 
						 * should take the absolute value of the line type and the positive contour values will be
						 * solid
						 **/
			    	 patternType *= -1;
			}

			if(this.linePatternValuesHashMap.containsKey(patternType)){
	    		 this.LineStyleList.add(currentIndex,
	    		 this.linePatternValuesHashMap
	    		 .get(patternType));
	    	 }
			
		}
		
	}

	/**
	 * @param List of Integer - lineWidthList 
	 */
	public void setLineWidthList(List<Integer> lineWidthList) {
		this.lineWidthList = lineWidthList;
	}


	/**
	 * @return List of Integer - lineWidthList
	 */
	public List<Integer> getLineWidthList() {
		return lineWidthList;
	}


	/**
	 * @param List of Integer - lineLabelPresentList
	 */
	public void setLineLabelPresentList(List<Integer> lineLabelPresentList) {
		this.lineLabelPresentList = lineLabelPresentList;
	}


	/**
	 * @return List of Integer - lineLabelPresentList
	 */
	public List<Integer> getLineLabelPresentList() {
		return lineLabelPresentList;
	}


	/**
	 * @return List of <code>LineStyle</code> - LineStyleList
	 */
	public List<LineStyle> getLineStyleList() {
		return LineStyleList;
	}

	/**
	 * @param List of Integer - lineAttributesList
	 * @param int lineAttributeType
	 * <p>
	 *The method <code>validateAndSetIntegerLineAttributes</code> accepts two parameters - a list of Integers
	 *called <code>lineAttributesList</code> and another integer <code>lineAttributeType</code> that 
	 *indicates the type of line attribute represented by this list of Integers.
	 *Depending on the kind of line attribute, each element of the Integer list is validated and then the
	 *corresponding 'set' method for that attribute is invoked.
	 **/							
	protected void validateAndSetIntegerLineAttributes(List<Integer> lineAttributesList,int lineAttributeType){
		int currentIndex;
       	int defaultLineType  = 1;
		int defaultLineWidth = 1;

		if (lineAttributesList.size() > 0) {
			switch (lineAttributeType) {
				case 0:
                /*The line colors are validated and set*/
					
					for (currentIndex = 0; currentIndex < lineAttributesList.size(); currentIndex++) {
						if (lineAttributesList.get(currentIndex) < 0) {
							lineAttributesList.set(currentIndex,lineAttributesList.get(currentIndex)*-1); 
	  				    }
					}
				this.setLineColorsList(lineAttributesList);
				
				break;

				case 1:
					/*The line patterns are validated and set*/
					for (currentIndex = 0; currentIndex < lineAttributesList.size(); currentIndex++) {
						if (lineAttributesList.get(currentIndex).intValue() < 1 ||
							lineAttributesList.get(currentIndex).intValue() > 10	) {
							lineAttributesList.set(currentIndex, defaultLineType);
						}
						
           			}
					this.setLineStyleList(lineAttributesList);
					
				break;
				
				case 2:
					/*The line widths are validated and set*/
					for (currentIndex = 0; currentIndex < lineAttributesList.size(); currentIndex++) {
						if ((lineAttributesList.get(currentIndex).intValue() < 0) 
							  || (lineAttributesList.get(currentIndex).intValue() > 20)) {
							lineAttributesList.set(currentIndex,defaultLineWidth);
						}
					}
					this.setLineWidthList(lineAttributesList);
					
				break;
				
				case 3:
					    /*The line labels present indicators are set*/
					    
					    if(lineAttributesList.size() == 1){
					    	if(lineAttributesList.get(0) < 0){
					    		this.setBreakInLineForLabel(false);
					    		lineAttributesList.set(0, lineAttributesList.get(0)*-1);
					    	}
					    }
					    this.setLineLabelPresentList(lineAttributesList);	
				break;

				default:
					    /*The smoothing level for the lines are validated and set*/
						Integer smoothFactor = lineAttributesList.get(0);
						if((smoothFactor == 1)||(smoothFactor == 2)) {
							this.setLineSmoothingLevel(smoothFactor);
						}
				break;
			}
		}

	}

	/**
	 *@param List of Integer - colorList,
	 *@param List of LineStyle - patternList
	 *@param List of Integer - widthList
	 *@param List of Integer - linesLabelledList
	 *<p>
	 *The method <code>setDefaultAttributesForLinesWithMissingAttributes</code> compares the sizes
	 *of all the 4 input lists and based on the size of the largest list, it invokes other
	 *methods to add a corresponding number of missing elements to the other lists. 
	 **/
	protected void setDefaultAttributesForLinesWithMissingAttributes(List<Integer>        colorList,
			                                                     List<LineStyle>     patternList,
			                                                     List<Integer>          widthList,
			                                                     List<Integer>          linesLabelledList){
		int numColorElementsToBeAdded   = 0;
		int numWidthElementsToBeAdded   = 0;
		int numPatternElementsToBeAdded = 0;
		int numLabelElemtsToBeAdded     = 0;
		
		int colorListSize               = 0;        
		int patternListSize             = 0;      
		int widthListSize               = 0;       
		int linesLabelledListSize       = 0; 
		
		int indexOfLargestNumber        = 0;
		int maxElementSize              = 0;
		
		if(colorList != null){
			colorListSize = colorList.size();
		}
		
		if(patternList != null){
			patternListSize = patternList.size();
		}
		
		if(widthList != null){
			widthListSize = widthList.size();
		}
		
		if(linesLabelledList != null){
			linesLabelledListSize = linesLabelledList.size();
		}
		
		int[] attributeSizesArray   = {colorListSize,patternListSize,widthListSize,linesLabelledListSize}; 
		Arrays.sort(attributeSizesArray);
		
		indexOfLargestNumber        = attributeSizesArray.length - 1;
		maxElementSize              = attributeSizesArray[indexOfLargestNumber];
		
		if(maxElementSize > 0){
			   numColorElementsToBeAdded       =  maxElementSize  - colorListSize;
			   numPatternElementsToBeAdded     =  maxElementSize  - patternListSize;
			   numWidthElementsToBeAdded       =  maxElementSize  - widthListSize;
			   numLabelElemtsToBeAdded         =  maxElementSize  - linesLabelledListSize;
		}else{
			   numColorElementsToBeAdded    = 1;
			   numPatternElementsToBeAdded  = 1;
			   numWidthElementsToBeAdded    = 1;
			   numLabelElemtsToBeAdded      = 1;
		}
		
		this.addDefaultLineColorAttribute(numColorElementsToBeAdded);
		this.addDefaultLineStyleAttribute(numPatternElementsToBeAdded);
		this.addDefaultLineWidthAttribute(numWidthElementsToBeAdded);
		this.addDefaultLineLabelAttribute(numLabelElemtsToBeAdded);
	}
	/**
	 *@param int numDefaultColorsToAdd
	 *<p>
	 *The method <code> addDefaultLineColorAttribute</code> is invoked by the method
	 * <code>setDefaultAttributesForLinesWithMissingAttributes</code>, with an input 
	 * parameter that decides the number of default elements to be added to the list
	 * of line colors.  
	 **/
	private void addDefaultLineColorAttribute(int numDefaultColorsToAdd){
		int count = 0;
		while(count < numDefaultColorsToAdd){
			this.getLineColorsList().add(this.defaultColor);
			count++;
		}
	}

	/**
	 *@param int numDefaultPatternsToAdd
	 *<p>
	 *The method <code>addDefaultLineStyleAttribute</code> is invoked by the method
	 * <code>setDefaultAttributesForLinesWithMissingAttributes</code>, with an input 
	 * parameter that decides the number of default elements to be added to the list
	 * of line patterns.  
	 **/
	private void addDefaultLineStyleAttribute(int numDefaultPatternsToAdd){
		int count = 0;
		while(count < numDefaultPatternsToAdd){
			this.getLineStyleList().add(this.defaultLinePattern);
			count++;
		}
	}
	
	/**
	 *@param int numDefaultWidthToAdd
	 *<p>
	 *The method <code>addDefaultLineWidthAttribute</code> is invoked by the method
	 * <code>setDefaultAttributesForLinesWithMissingAttributes</code>, with an input 
	 * parameter that decides the number of default elements to be added to the list
	 * of line widths.  
	 **/
	private void addDefaultLineWidthAttribute(int numDefaultWidthToAdd){
		int count = 0;
		while(count < numDefaultWidthToAdd){
			this.getLineWidthList().add(this.defaultLineWidth);
			count++;
		}			
	}
	
	/**
	 *@param int numDefaultLineLabelsToAdd
	 *<p>
	 *The method <code> addDefaultLineLabelAttribute</code> is invoked by the method
	 * <code>setDefaultAttributesForLinesWithMissingAttributes</code>, with an input 
	 * parameter that decides the number of default elements to be added to the list
	 * of line label indicators.  
	 **/
	private void addDefaultLineLabelAttribute(int numDefaultLineLabelsToAdd){
		int count = 0;
		while(count < numDefaultLineLabelsToAdd){
			this.getLineLabelPresentList().add(this.defaultLabeledLine);
			count++;
		}
	}
	

	
} 