/**
 * 
 */
package gov.noaa.nws.ncep.standalone.miscsetConverter;

import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

import java.util.List;

/**
 * @author bhebbard
 *
 */
public class AttrLn {

	private static enum Kind { COMMENT, NOLEGACY, TYPE, FLAG, UNDEFINED };

	String comment = null;
	String variable = null;
	String defaultValue = null;
	float scaleFactor = 1.00f;
	Kind kind = Kind.UNDEFINED;
	int index;
	FlagCommand flagRet = null;
	TypeCommand typeRet = null;

	//TODO:  Refactor/combine the following...?
	public AttrLn (String line, int index) {
		this(line);
		this.index = index;
		this.flagRet = FlagField.ENABLE;
		kind = Kind.FLAG;
	}
	
	public AttrLn (String line, int index, FlagCommand retriever) {
		this(line, index);
		this.flagRet = retriever;
		kind = Kind.FLAG;
	}
	
	public AttrLn (String line, int index, TypeCommand retriever) {
		this(line, index);
		this.typeRet = retriever;
		kind = Kind.TYPE;
	}
	
	public AttrLn (String line, int index, FlagCommand retriever, float scaleFactor) {
		this(line, index);
		this.flagRet = retriever;
		this.scaleFactor = scaleFactor;
		kind = Kind.FLAG;
	}
	
	public AttrLn (String line, int index, TypeCommand retriever, float scaleFactor) {
		this(line, index);
		this.typeRet = retriever;
		this.scaleFactor = scaleFactor;
		kind = Kind.TYPE;
	}
	
	public AttrLn (String line, int index, FlagCommand retriever, double scaleFactor) {
		this(line, index, retriever, (float) scaleFactor);
	}
	
	public AttrLn (String line, int index, TypeCommand retriever, double scaleFactor) {
		this(line, index, retriever, (float) scaleFactor);
	}
	
	public AttrLn (String line, int index, FlagCommand retriever, int scaleFactor) {
		this(line, index, retriever, (float) scaleFactor);
	}
	
	public AttrLn (String line, int index, TypeCommand retriever, int scaleFactor) {
		this(line, index, retriever, (float) scaleFactor);
	}

	public AttrLn (String line) {
		if (line.contains("=")) {
			String[] tokens = line.split("=");
			variable = tokens[0];
			defaultValue = tokens[1];
			kind = Kind.NOLEGACY;
		}
		else {
			comment = line;
			kind = Kind.COMMENT;
		}
	}
	
	public String getNewString(List<TypeRecord> typeList, List<FlagRecord> flagList) {
		switch (kind) {
		    case COMMENT: {
		    	return comment;
		    }
		    case NOLEGACY: {
			    //System.out.println("[DEBUG:  Variable '" + variable + "' has no legacy counterpart; assigning default value '" + defaultValue + "']");
		    	return variable + "=" + defaultValue;
		    }
		    case FLAG : {
		    	if (index >= flagList.size()) {
		    		System.out.println("[ERROR:  Asking for FLAG number " + index + " but only have 0 thru " + (flagList.size()-1) + " !]");
		    		return "";  //TODO: null?
		    	}
			    //String legacyValue = flagRet.retrieve(flagList.get(index));
		    	FlagRecord fr = flagList.get(index);
		    	String legacyValue = flagRet.retrieve(fr);
		    	String newValue;
				if (legacyValue == null || legacyValue.isEmpty()) {
					newValue = defaultValue;
				}
				else {
					newValue = legacyValue;
					if (meaningfullyDifferent(newValue,defaultValue)) {  //TODO:  Only true or false -- simplify this?
					    System.out.println("[INFO:  Variable '" + variable + "' value '" + defaultValue + "' replaced by '" + newValue + "' based on legacy FLAG entry]");
					}
				}
				return variable + "=" + newValue;
		    }
		    case TYPE : {
		    	if (index >= typeList.size()) {
		    		System.out.println("[ERROR:  Asking for TYPE number " + index + " but only have 0 thru " + (typeList.size()-1) + " !]");
		    		return "";  //TODO: null?
		    	}
			    //String legacyValue = typeRet.retrieve(typeList.get(index));
		    	TypeRecord tr = typeList.get(index);
		    	String legacyValue = typeRet.retrieve(tr, scaleFactor);
		    	String newValue;
				if (legacyValue == null || legacyValue.isEmpty()) {
					newValue = defaultValue;
				}
				else {
					newValue = legacyValue;
					if (meaningfullyDifferent(newValue,defaultValue)) {
					    System.out.println("[INFO:  Variable '" + variable + "' value '" +
					    		defaultValue + "'" + labelRGB(defaultValue) + " replaced by '"+
					    		newValue + "'" + labelRGB(newValue) + " based on legacy TYPE entry]");
					}
				}
				return variable + "=" + newValue;
		    }
		    case UNDEFINED: {
		    	//TODO:  sanity check
		    	return null;
		    }
		    default: {
		    	return null;
		    }
		}
	}

	private String labelRGB(String s) {
        String r = s;
		if (s.contains("RGB")) {
			for (GempakColor gc : GempakColor.values()) {
				//String x = gc.getRGB().toString();
				//String y = x;
				if (s.equalsIgnoreCase(gc.getRGBString())) {
					r = " (#" + (gc.ordinal()+1) + ":" + gc.name() + ")";
					return r;
				}
			}
		}
		return "";
	}

	private boolean meaningfullyDifferent(String value1, String value2) {
		// Determine whether the two strings have values that are
		// different in meaning for purposes of attribute values
		// (e.g., ignore case, whitespace, trailing-zeroes-and-decimal-point)
		//
		// Before comparision, pre-treat both strings.  First, remove whitespace ...
		value1 = value1.replaceAll("\\s+", "");
		value2 = value2.replaceAll("\\s+", "");
		// ...then, if it's purely a numeric value with (one) decimal point
		// and trailing digits, remove trailing zeroes-and-decimal-point
		if (value1.matches("\\d*\\.\\d+")) {
		    value1 = value1.replaceAll("\\.?0*$", "");
		}
		if (value2.matches("\\d*\\.\\d+")) {
		    value2 = value2.replaceAll("\\.?0*$", "");
		}
		// Now, ignoring case, see if there's still a difference
		return !value1.equalsIgnoreCase(value2);
	}

	public void processAttrLn()
	{
		//typesMap.put("CSIG", csigTypes);
		//flagsMap.put("CSIG", csigFlags);
	}
}
