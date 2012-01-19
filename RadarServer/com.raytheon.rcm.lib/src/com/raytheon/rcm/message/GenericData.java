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
package com.raytheon.rcm.message;

import java.io.ByteArrayOutputStream;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.rcm.message.Message;

// TODO: redundant counts are not redundant... 0 == short circuit

public class GenericData {
	public static final int VOLUME = 1;
	public static final int ELEVATION = 2;
	public static final int TIME = 3;
	public static final int ON_DEMAND = 4;
	public static final int ON_REQUEST = 5;
	public static final int RADIAL = 6;
	public static final int EXTERNAL = 7;
	
	public String name;
	public String description;
	public int code;
	public int type;
	public int generationTime;
	public String radarName;
	public float lat, lon, height;
	public int volumeScanStartTime;
	public int elevationScanStartTime;
	public float elevationAngle;
	public int volumeScanNumber;
	public short opMode;
	public short vcp;
	public short elevationNumber;
	public ParameterSet parameters = new ParameterSet();
	public Component[] components;

	public static class Parameter {
		/* The ICD says attribute names are case-insensitive, but the
		 * RPG code expects lower case. 
		 */
		public static final String NAME_KEY = "name";
		public static final String TYPE_KEY = "type";
		public static final String VALUE_KEY = "value";
		public static final String UNITS_KEY = "units"; // Spec sez "unit".  I see "Units"
		
		// TODO: Cache the toLower() value?
		private class Key {
			String originalName;
			public Key(String attributeName) { 
				this.originalName = attributeName;
			}
			@Override
			public boolean equals(Object obj) {
			    if (this == obj)
			        return true;
			    else if (obj instanceof Key)
					return originalName.equalsIgnoreCase(((Key) obj).originalName);
				else
					return false;
			}
			@Override
			public int hashCode() {
				return originalName.toLowerCase().hashCode();
			}
			@Override
			public String toString() {
				return originalName;
			}
			
		}

		private String id;
		private Map<Key,String> attributes = new HashMap<Key, String>();
		
		public Parameter() {
			
		}
		
		public Parameter(String id) {
			this.id = id;
		}
		
		public Parameter(String id, String text) {
			this.id = id;
			setAttributesText(text);
		}

        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }
		
		public String getName() { return getAttribute(NAME_KEY); }
		public String getType() { return getAttribute(TYPE_KEY); }
		public String getValue() { return getAttribute(VALUE_KEY); }
		public String getUnits() { return getAttribute(UNITS_KEY); }
		public void setName(String name) { putAttribute(NAME_KEY, name); }
		public void setType(String type) { putAttribute(TYPE_KEY, type); }
		public void setValueText(String value) { putAttribute(VALUE_KEY, value); }
		public void setUnits(String units) { putAttribute(UNITS_KEY, units); }
		
		public void setValue(String value) {
		    setValueText(value);
		    setType("string");
		}
		
		public void setValue(int value) {
		    setValueText(Integer.toString(value));
		    setType("int");
		}
		
		public void setValue(double value) {
		    setValueText(String.format("%f", value));
		    setType("float");
		}
		
		public void setAttributesText(String text) {
			attributes.clear();
			// TODO: needs to handle "\" escape
			for (String s : text.split(";")) {
				if (s.length() <= 0)
					continue;
				int pos = s.indexOf('=');
				String n = s.substring(0, pos).trim();
				String v = s.substring(pos + 1);
				attributes.put(new Key(n), v);
			}
		}
		
		public String getAttributesText() {
			StringBuilder sb = new StringBuilder();
			for (Map.Entry<Key, String> e : attributes.entrySet())
				sb.append(e.getKey().toString()).append('=').append(e.getValue()).append(';');
			return sb.toString();
		}
		
		public String getAttribute(String name) { 
			return attributes.get(new Key(name)); 
		}
		
		public void putAttribute(String name, String value) {
		    if (value != null)
		        attributes.put(new Key(name), value);
		    else
		        attributes.remove(new Key(name));
		}
	}
	
	public static class ParameterSet {
		public Map<String, Parameter> getMap() { return map; }

		Parameter[] getArray() { 
			return Arrays.copyOf(array, array.length); 
		}

		private HashMap<String, Parameter> map = new HashMap<String, Parameter>();
		// array is just a debugging aid
		private Parameter[] array;
		
		void decode(ByteBuffer buf) {
			if (buf.getInt() > 0) {
    			int nParameters = buf.getInt();
    			array = new Parameter[nParameters];
    			for (int i = 0; i < nParameters; ++i) {
    				String id = getString(buf);
    				String text = getString(buf);
    				Parameter p = new Parameter(id, text);
    				map.put(p.id, p);
    				array[i] = p;
    			}
			}
		}
		
		public void encode(DataOutput out) throws IOException{
		    final int mapSize = map.size();
			out.writeInt(mapSize);
			if (mapSize > 0) {
    			out.writeInt(mapSize); // redundant count
    			for (Map.Entry<String, Parameter> p : map.entrySet()) {
    				putString(out, p.getKey());
    				putString(out, p.getValue().getAttributesText());
    			}
			}
		}
		
		public void put(Parameter parameter) {
		    map.put(parameter.id, parameter);
		}
	}
	
	public static abstract class Component {
		public static final int RADIAL = 1;
		public static final int GRID = 2;
		public static final int AREA = 3;
		public static final int TEXT = 4;
		public static final int TABLE = 5;
		public static final int EVENT = 6;
		
		public ParameterSet parameters = new ParameterSet();
		
		public void decode(ByteBuffer buf) {
			parameters = getParameters(buf);
		}

		public abstract int getTypeCode();
		
		public void encode(DataOutput out) throws IOException {
			parameters.encode(out);
		}
	}
	
	public static class AreaComponent extends Component {
		public static final int POINT = 1;
		public static final int AREA = 2;
		public static final int POLYLINE = 3;
		public static final int LAT_LON = 0;
		public static final int X_Y = 1;
		public static final int AZ_RAN = 2;
		
		public int geometry;
		public int coordinates;
		public float[] c0;
		public float[] c1;
		
		public void decode(ByteBuffer buf) {
			super.decode(buf);
			int geoCoord = buf.getInt();
			geometry = geoCoord & 0xffff;
			coordinates = (geoCoord >> 16) & 0xffff;
			if (buf.getInt() > 0) {
    			int nCoords = buf.getInt();
    			c0 = new float[nCoords];
    			c1 = new float[nCoords];
    			for (int i = 0; i < nCoords; ++i) {
    				c0[i] = buf.getFloat();
    				c1[i] = buf.getFloat();
    			}
			} else {
			    c0 = c1 = new float[0];
			}
		}

		@Override
		public void encode(DataOutput out) throws IOException {
			super.encode(out);
			out.writeInt(geometry | (coordinates << 16));
			if (c0 != null && c0.length > 0) {
				out.writeInt(c0.length);
				out.writeInt(c0.length);
				for (int i = 0; i < c0.length; ++i) {
					out.writeFloat(c0[i]);
					out.writeFloat(c1[i]);
				}
			} else
				out.writeInt(0);
		}

		@Override
		public int getTypeCode() {
			return Component.AREA;
		}
	}
	
	public static class EventComponent extends Component {
		public Component[] components;

		public void decode(ByteBuffer buf) {
			super.decode(buf);
			components = decodeComponents(buf);
		}

		@Override
		public int getTypeCode() {
			return Component.EVENT;
		}

		@Override
		public void encode(DataOutput out) throws IOException {
			super.encode(out);
			encodeComponents(out, components);
		}
	}
	
	public static class GridComponent extends Component {
		public static final int ARRAY = 1;
		public static final int EQUALLY_SPACED = 2;
		public static final int LAT_LON = 3;
		public static final int POLAR = 4;

		public int gridType;
		public int[] dimensions;
		public Object data;
		private Parameter dataAttributes = new Parameter(); // Not a full Parameter -- just the attributes
		
		public void decode(ByteBuffer buf) {
			if (buf.getInt() > 0) {
			    int nDimensions = buf.getInt();
				dimensions = new int[nDimensions];
				for (int i = 0; i < nDimensions; ++i)
					dimensions[i] = buf.getInt();
			} else
				dimensions = new int[0];
			gridType = buf.getInt();
			super.decode(buf);
			dataAttributes = new Parameter();
			dataAttributes.setAttributesText(getString(buf));
			String type = dataAttributes.getType(); 
			if (type == null)
				throw new RuntimeException("Grid type not specified.");
			if (! type.equalsIgnoreCase("float"))
				throw new RuntimeException("Only grids of type float are supported.");
			int arraySize = buf.getInt();
			int pointCount = getPointCount();
			if (arraySize != pointCount)
			    throw new RuntimeException("Grid array size and point count do not match.");
			float[] array = new float[pointCount];
			buf.asFloatBuffer().get(array);
			buf.position(buf.position() + array.length * 4);
		}

		@Override
		public int getTypeCode() {
			return Component.GRID;
		}

		@Override
		public void encode(DataOutput out) throws IOException {
			if (dimensions != null && dimensions.length > 0) {
				out.writeInt(dimensions.length);
				out.writeInt(dimensions.length);
				for (int d : dimensions)
					out.writeInt(d);
			} else
				out.writeInt(0);
			out.writeInt(gridType);
			super.encode(out);
			
			putString(out, dataAttributes.getAttributesText());
			if (data.getClass().isArray() && 
					data.getClass().getComponentType() == Float.TYPE) {
				float[] array = (float[]) data;
				// No redundant count for once.
				if (array.length != getPointCount())
					throw new IllegalArgumentException("Dimensions and data length do not match");
				ByteBuffer bb = ByteBuffer.allocate(array.length * 4);
				bb.asFloatBuffer().put(array);
				out.writeInt(array.length);
				out.write(bb.array());
			} else {
				throw new RuntimeException("Only grids of type float are supported.");
			}
		}

		public int getPointCount() {
			if (dimensions != null && dimensions.length > 0) {
				int count = 1;
				for (int d : dimensions)
					count *= d;
				return count;
			} else
				return 0;
		}
		
		/** Returns pseudo-parameter object that describes the grid data. */
		public Parameter getDataAttributes() {
		    return dataAttributes;
		}

	}
	
	public void decode(ByteBuffer buf, int packetCode) {
		name = getString(buf);
		description = getString(buf);
		code = buf.getInt();
		type = buf.getInt();
		generationTime = buf.getInt();
		if (packetCode == 28) {
			radarName = getString(buf);
			lat = buf.getFloat();
			lon = buf.getFloat();
			height = buf.getFloat();
			volumeScanStartTime = buf.getInt();
			elevationScanStartTime = buf.getInt();
			elevationAngle = buf.getFloat();
			volumeScanNumber = buf.getInt();
			opMode = getXdrShort(buf);
			vcp = getXdrShort(buf);
			elevationNumber = getXdrShort(buf);
		} else if (packetCode == 29) {
			// Documented as "Spare".
		    // ICD is confusing here about the number of fields.
			buf.getInt();
			buf.getInt();
			buf.getInt();
			buf.getInt();
			getXdrShort(buf);
		} else
			throw new RuntimeException("fix"); // TODO: ...
		
		// Documented as "Spare (reserved for future compression ...)" 
		getXdrShort(buf);
		buf.getInt();
		
		parameters = getParameters(buf);
		components = decodeComponents(buf);
	}
	
	void encode(DataOutput out, int packetCode) throws IOException {
        putString(out, name);
        putString(out, description);
        out.writeInt(code);
        out.writeInt(type);
        out.writeInt(generationTime);
        if (packetCode == 28) {
            putString(out, radarName);
            out.writeFloat(lat);
            out.writeFloat(lon);
            out.writeFloat(height);
            out.writeInt(volumeScanStartTime);
            out.writeInt(elevationScanStartTime);
            out.writeFloat(elevationAngle);
            out.writeInt(volumeScanNumber);
            putXdrShort(out, opMode);
            putXdrShort(out, vcp);
            putXdrShort(out, elevationNumber);
        } else if (packetCode == 29) {
            // Documented as "Spare".
            // ICD is confusing here about the number of fields.
            out.writeInt(0);
            out.writeInt(0);
            out.writeInt(0);
            out.writeInt(0);
            putXdrShort(out, (short) 0);
        } else {
            throw new IllegalArgumentException(String.format("Unknown packet code %d", packetCode));
        }
        // Documented as "Spare (reserved for future compression ...)" 
        putXdrShort(out, (short) 0);
        out.writeInt(0);
        
        parameters.encode(out);
        encodeComponents(out, components);
	    
	}
	
	public byte[] encode(int packetCode) throws IOException {
		ByteArrayOutputStream outStream = new ByteArrayOutputStream(4096);
		DataOutputStream out = new DataOutputStream(outStream);
		
		encode(out, packetCode);
		
		out.flush();
		return outStream.toByteArray();
	}
	
	private static ParameterSet getParameters(ByteBuffer buf) {
		ParameterSet ps = new ParameterSet();
		ps.decode(buf);
		return ps;
	}
	
	private static Component[] decodeComponents(ByteBuffer buf) {
	    Component[] components;
		if (buf.getInt() > 0) {
    		components = new Component[buf.getInt()];
    		for (int i = 0; i < components.length; ++i) {
    			Component component = null;
    			if (buf.getInt() != 0) { // Optional value flag
    				int componentType = buf.getInt();
    				
    				switch (componentType) {
    				case Component.AREA:
    					component = new AreaComponent();
    					break;
    				case Component.EVENT:
    					component = new EventComponent();
    					break;
    				case Component.GRID:
    					component = new GridComponent();
    					break;
    				default:
    					Message.checkFormat(false, 
    							String.format("Unsupported component type %d", componentType));
    				}
    				component.decode(buf);
    			}
    			components[i] = component;
    		}
		} else
		    components = new Component[0];
		return components;
	}

	private static void encodeComponents(DataOutput out, Component[] components) throws IOException {
	    if (components != null && components.length > 0) {
    		out.writeInt(components.length);
    		out.writeInt(components.length); // Redundant count
    		for (Component c : components) {
    			if (c != null) {
    				out.writeInt(1);
    				out.writeInt(c.getTypeCode());
    				c.encode(out);
    			} else
    				out.writeInt(0);
    		}
	    } else {
	        out.writeInt(0);
	    }
	}

	private static String getString(ByteBuffer buf) {
		int len = buf.getInt();
		byte[] bytes = new byte[len];
		buf.get(bytes);
		if ((len & 3) != 0)
			buf.position(buf.position() + (4 - (len & 3))); // it's an XDR thing
		return new String(bytes);
	}
	
	private static void putString(DataOutput out, String str) throws IOException {
		if (str != null) {
			byte[] bytes = str.getBytes();
			int len = bytes.length;
			out.writeInt(len);
			out.write(bytes);
			// XDR requires padding to a multiple of four bytes
			while ((len & 3) != 0) {
				out.write(0);
				++len;
			}
		} else
			out.writeInt(0);
	}
	
	private static short getXdrShort(ByteBuffer buf) {
		// TODO: signedness.....
		return (short) buf.getInt();
	}
	
	private static void putXdrShort(DataOutput out, short value) throws IOException {
		out.writeInt(value);
	}
}
