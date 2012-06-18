/**
 * 
 */
package gov.noaa.nws.ncep.standalone.miscsetConverter;

/**
 * @author bhebbard
 *
 */
public class TypeRecord {

	private String typeName;
	private String enable;
	private String color1;
	private String color2;
	private String value;
	//private String arrowEnabled;
	private String arrowSize;
	private String arrowHeadSize;
	private String arrowWidth;
	private String arrowType;
	//private String lineEnabled;
	private String lineSize;
	private String lineWidth;
	//private String sym1Enabled;
	private String sym1Number;
	private String sym1Size;
	private String sym1Width;
	//private String sym2Enabled;
	private String sym2Number;
	private String sym2Size;
	private String sym2Width;

	public TypeRecord (String typeName,
			           String typeValue,
			           String arrowValue,
			           String lineValue,
			           String sym1Value,
			           String sym2Value) {
		
		this.typeName = typeName;

		if (typeValue != null) {
			String[] typeValues = typeValue.split("/");
			this.enable     = typeValues[0];
			if (typeValues.length > 1) {
				String[] colors = typeValues[1].split(";");
				this.color1     = colors[0];
				this.color2     = (colors.length > 1) ? colors[1] : null;
			}
			this.value      = (typeValues.length > 2) ? typeValues[2] : null;
		}

		if (arrowValue != null) {
			String[] arrowValues = arrowValue.split("/");
			//this.arrowEnabled = arrowEnabled;
			this.arrowSize     = arrowValues[0];
			this.arrowHeadSize = (arrowValues.length > 1) ? arrowValues[1] : null;
			this.arrowWidth    = (arrowValues.length > 2) ? arrowValues[2] : null;
			this.arrowType     = (arrowValues.length > 3) ? arrowValues[3] : null;
		}

		if (lineValue != null) {
			String[] lineValues = lineValue.split("/");
			//this.lineEnabled = lineEnabled;
			this.lineSize  = lineValues[0];
			this.lineWidth = (lineValues.length > 1) ? lineValues[1] : null;
		}

		if (sym1Value != null) {
			String[] sym1Values = sym1Value.split("/");
			//this.sym1Enabled = sym1Enabled;
			this.sym1Number = sym1Values[0];
			this.sym1Size   = sym1Values.length > 1 ? sym1Values[1] : null;
			this.sym1Width  = sym1Values.length > 2 ? sym1Values[2] : null;
		}

		if (sym2Value != null) {
			String[] sym2Values = sym2Value.split("/");
			//this.sym2Enabled = sym2Enabled;
			this.sym2Number = sym2Values[0];
			this.sym2Size   = sym2Values.length > 1 ? sym2Values[1] : null;
			this.sym2Width  = sym2Values.length > 2 ? sym2Values[2] : null;
		}
		
	}
	
	/**
	 * @return the typeName
	 */
	public String getTypeName() {
		return typeName;
	}

	/**
	 * @return the enable
	 */
	public String getEnable() {
		return enable;
	}

	/**
	 * @return the color1
	 */
	public String getColor1() {
		return color1;
	}

	/**
	 * @return the color2
	 */
	public String getColor2() {
		return color2;
	}

	/**
	 * @return the value
	 */
	public String getValue() {
		return value;
	}

	/**
	 * @return the arrowSize
	 */
	public String getArrowSize() {
		return arrowSize;
	}

	/**
	 * @return the arrowHeadSize
	 */
	public String getArrowHeadSize() {
		return arrowHeadSize;
	}

	/**
	 * @return the arrowWidth
	 */
	public String getArrowWidth() {
		return arrowWidth;
	}

	/**
	 * @return the arrowType
	 */
	public String getArrowType() {
		return arrowType;
	}

	/**
	 * @return the lineSize
	 */
	public String getLineSize() {
		return lineSize;
	}

	/**
	 * @return the lineWidth
	 */
	public String getLineWidth() {
		return lineWidth;
	}

	/**
	 * @return the sym1Number
	 */
	public String getSym1Number() {
		return sym1Number;
	}

	/**
	 * @return the sym1Size
	 */
	public String getSym1Size() {
		return sym1Size;
	}

	/**
	 * @return the sym1Width
	 */
	public String getSym1Width() {
		return sym1Width;
	}

	/**
	 * @return the sym2Number
	 */
	public String getSym2Number() {
		return sym2Number;
	}

	/**
	 * @return the sym2Size
	 */
	public String getSym2Size() {
		return sym2Size;
	}

	/**
	 * @return the sym2Width
	 */
	public String getSym2Width() {
		return sym2Width;
	}

	public TypeRecord() {
		// TODO Auto-generated constructor stub
	}
	
}
