/**
 * 
 */
package gov.noaa.nws.ncep.standalone.miscsetConverter;

import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

import org.eclipse.swt.graphics.RGB;

/**
 * @author bhebbard
 *
 */

public enum TypeField implements TypeCommand {
	
	NAME ()          { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
                         String typeName = tr.getTypeName();
                         return typeName;
                         } },
	ENABLE ()        { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
			             String origValue = tr.getEnable();
			             Boolean b = origValue.equals("1");
			             return b.toString();
			             } },
	COLOR1 ()        { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getColor1();
		                 int colorNumber;
		                 try {
		                	 colorNumber = Integer.parseInt(origValue);
		                 }
		                 catch (NumberFormatException e) {
		                	 colorNumber = 0;
		                 }
		                 if (colorNumber < 1 || colorNumber > GempakColor.values().length) {
		                	 return "RGB {0, 0, 0}";
		                 }
		                 GempakColor gc = GempakColor.values()[colorNumber-1];
                         return gc.getRGBString();
                         } },
	COLOR2 ()        { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getColor2();
		                 int colorNumber;
		                 try {
		                	 colorNumber = Integer.parseInt(origValue);
		                 }
		                 catch (NumberFormatException e) {
		                	 colorNumber = 0;
		                 }
		                 if (colorNumber < 1 || colorNumber > GempakColor.values().length) {
		                	 return "RGB {0, 0, 0}";
		                 }
		                 GempakColor gc = GempakColor.values()[colorNumber-1];
                         return gc.getRGBString();
			             } },
	VALUE ()         { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
				         String value = tr.getValue();
				         //TODO:  Scaling?
				         return value;
				         } },
	ARROWSIZE ()     { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getArrowSize();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 //Integer i = (int) f;
			             //return i.toString();
		                 return String.format("%6.2f",f).trim();
			             } },
	ARROWHEADSIZE () { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getArrowHeadSize();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 //Integer i = (int) f;
			             //return i.toString();
		                 return String.format("%6.2f",f).trim();
			             } },
	ARROWWIDTH ()    { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getArrowWidth();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 Integer i = Math.round(f);
			             return i.toString();
			             } },
	ARROWTYPE ()     { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getArrowType();
		                 if (origValue == null) return null;
		                 Integer i = Integer.parseInt(origValue);
		                 switch (i) {
		                     case 1: return "DIRECTIONAL_ARROW";
		                     case 2: return "WIND_BARB";
		                     case 3: return "DIRECTIONAL_ARROW";
		                     case 4: return "REGULAR_ARROW";
		                     case 5: return "WIND_BARB";
		                 }
			             return "";
			             } },
	LINESIZE ()      { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getLineSize();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 //Integer i = (int) f;
			             //return i.toString();
		                 return String.format("%6.2f",f).trim();
			             } },
	LINEWIDTH ()     { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getLineWidth();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 Integer i = Math.round(f);
			             return i.toString();
			             } },
	SYM1NUMBER ()    { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getSym1Number();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 Integer i = (int) f;
			             return i.toString();
			             } },
	SYM1SIZE ()      { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getSym1Size();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 //Integer i = (int) f;
			             //return i.toString();
		                 return String.format("%6.2f",f).trim();
			             } },
	SYM1WIDTH ()     { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getSym1Width();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 Integer i = Math.round(f);
			             return i.toString();
			             } },
	SYM2NUMBER ()    { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getSym2Number();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 Integer i = (int) f;
			             return i.toString();
			             } },
	SYM2SIZE ()      { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getSym2Size();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 //Integer i = (int) f;
			             //return i.toString();
		                 return String.format("%6.2f",f).trim();
			             } },
	SYM2WIDTH ()     { public String retrieve(TypeRecord tr, float scaleFactor) {
                         if (tr == null) return null;
		                 String origValue = tr.getSym2Width();
		                 if (origValue == null) return null;
		                 float f = Float.parseFloat(origValue);
		                 f *= scaleFactor;
		                 Integer i = Math.round(f);
			             return i.toString();
			             } };
			             
	public abstract String retrieve(TypeRecord tr, float scaleFactor);

}
