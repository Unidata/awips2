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
package com.raytheon.uf.common.pointdata;

import javax.measure.unit.Unit;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.Transient;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.elements.AbstractPointDataObject;
import com.raytheon.uf.common.pointdata.elements.FloatPointDataObject;
import com.raytheon.uf.common.pointdata.elements.IntPointDataObject;
import com.raytheon.uf.common.pointdata.elements.LongPointDataObject;
import com.raytheon.uf.common.pointdata.elements.StringPointDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Convenience class that provides a view of the data for a single observation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 08, 2009           chammack    Initial creation
 * Dec 20, 2013  2537     bsteffen    add getFloat on a specified level.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@Embeddable
@DynamicSerialize
public class PointDataView {

	public static enum Mode {
		APPEND, READ
	};

	@Column(name = "idx")
	@DynamicSerializeElement
	int curIdx;

	transient int curIdx2d;

	@Transient
	Mode mode;

	@Transient
	PointDataContainer container;

	public Number getNumber(String parameter) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);
		return p.getNumber(getIndex(p));
	}

	public Number[] getNumberAllLevels(String parameter) {

		AbstractPointDataObject<?> p = getParamSafe(parameter);
		Number[] lvl = new Number[p.getDescription().getDimensionAsInt()];

		if (p.getDimensions() != 2) {
			throw new IllegalArgumentException("Parameter " + parameter
					+ " is not two-dimensional");
		}

		for (int i = 0; i < lvl.length; i++) {
			lvl[i] = p.getNumber(getIndex(p) + i);
		}

		return lvl;
	}

	public String[] getStringAllLevels(String parameter) {

		AbstractPointDataObject<?> p = getParamSafe(parameter);
		String[] lvl = new String[p.getDescription().getDimensionAsInt()];
		if (p.getDimensions() != 2) {
			throw new IllegalArgumentException("Parameter " + parameter
					+ " is not two-dimensional");
		}

		for (int i = 0; i < lvl.length; i++) {
			lvl[i] = ((StringPointDataObject) p).getString(getIndex(p) + i);
		}

		return lvl;
	}

	public Unit<?> getUnit(String parameter) {
		return getParamSafe(parameter).getDescription().getUnitObject();
	}

	protected AbstractPointDataObject<?> getParamSafe(String parameter) {
		AbstractPointDataObject<?> p = container.pointDataTypes.get(parameter);
		if (p == null)
			throw new IllegalArgumentException("Parameter not present: "
					+ parameter);

		return p;
	}

	public int getDimensions(String parameter) {
		int dimensions = -1;
		// return getParamSafe(parameter).getDescription().getNumDims();
		AbstractPointDataObject<?> p = container.pointDataTypes.get(parameter);
		if (p != null) {
			dimensions = p.getDimensions();
		}
		return dimensions;
	}

	public int getInt(String parameter) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof IntPointDataObject)) {
			throw new IllegalArgumentException(
					"Parameter "
							+ parameter
							+ " is not natively an int type.  Use getNumber(), getString() or another primitive type");
		}
		return ((IntPointDataObject) p).getInt(getIndex(p));
	}

	public int[] getIntAllLevels(String parameter) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof IntPointDataObject)) {
			throw new IllegalArgumentException(
					"Parameter "
							+ parameter
							+ " is not natively an int type.  Use getNumber(), getString() or another primitive type");
		} else if (p.getDimensions() != 2) {
			throw new IllegalArgumentException("Parameter " + parameter
					+ " is not two-dimensional");
		}

		int[] lvl = new int[p.getDescription().getDimensionAsInt()];
		IntPointDataObject ipdo = (IntPointDataObject) p;
		for (int i = 0; i < lvl.length; i++) {
			lvl[i] = ipdo.getInt(getIndex(p) + i);
		}
		return lvl;
	}

	public void setInt(String parameter, int val) {
		setInt(parameter, val, 0);
	}

	public void setInt(String parameter, int val, int level) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof IntPointDataObject)) {
			p.setNumber(getIndex(p) + level, val);
		} else {
			if (level == 0) {
				((IntPointDataObject) p).setInt(getIndex(p), val);
			} else if (level >= p.getDescription().getDimensionAsInt()) {
				throw new IllegalArgumentException("Level  " + level
						+ " exceeds maxLevel size "
						+ p.getDescription().getDimensionAsInt());
			} else if (p.getDimensions() != 2) {
				throw new IllegalArgumentException(
						"Data is not two dimensional");
			} else {
				((IntPointDataObject) p).setInt(getIndex(p) + level, val);
			}
		}
	}

	public float getFloat(String parameter) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof FloatPointDataObject)) {
			throw new IllegalArgumentException(
					"Parameter "
							+ parameter
							+ " is not natively a float type.  Use getNumber(), getString() or another primitive type");
		}
		return ((FloatPointDataObject) p).getFloat(getIndex(p));
	}

	public void setFloat(String parameter, float val) {
		setFloat(parameter, val, 0);
	}

	public void setFloat(String parameter, float val, int level) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof FloatPointDataObject)) {
			p.setNumber(getIndex(p) + level, val);
		} else {
			if (level == 0) {
				((FloatPointDataObject) p).setFloat(getIndex(p), val);
			} else if (level >= p.getDescription().getDimensionAsInt()) {
				throw new IllegalArgumentException("Level  " + level
						+ " exceeds maxLevel size "
						+ p.getDescription().getDimensionAsInt());
			} else if (p.getDimensions() != 2) {
				throw new IllegalArgumentException(
						"Data is not two dimensional");
			} else {
				((FloatPointDataObject) p).setFloat(getIndex(p) + level, val);
			}
		}
	}

    public float getFloat(String parameter, int level) {
        AbstractPointDataObject<?> p = getParamSafe(parameter);

        if (!(p instanceof FloatPointDataObject)) {
            return p.getNumber(getIndex(p) + level).floatValue();
        } else if (level == 0) {
            return ((FloatPointDataObject) p).getFloat(getIndex(p));
        } else if (level >= p.getDescription().getDimensionAsInt()) {
            throw new IllegalArgumentException("Level  " + level
                    + " exceeds maxLevel size "
                    + p.getDescription().getDimensionAsInt());
        } else if (p.getDimensions() != 2) {
            throw new IllegalArgumentException("Data is not two dimensional");
        } else {
            return ((FloatPointDataObject) p).getFloat(getIndex(p) + level);
        }
    }

	public long getLong(String parameter) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof LongPointDataObject)) {
			throw new IllegalArgumentException(
					"Parameter "
							+ parameter
							+ " is not natively a long type.  Use getNumber(), getString() or another primitive type");
		}
		return ((LongPointDataObject) p).getLong(getIndex(p));
	}

	public void setLong(String parameter, long val) {
		setLong(parameter, val, 0);
	}

	public void setLong(String parameter, long val, int level) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof LongPointDataObject)) {
			p.setNumber(getIndex(p) + level, val);
		} else {
			if (level == 0) {
				((LongPointDataObject) p).setLong(getIndex(p), val);
			} else if (level >= p.getDescription().getDimensionAsInt()) {
				throw new IllegalArgumentException("Level  " + level
						+ " exceeds maxLevel size "
						+ p.getDescription().getDimensionAsInt());
			} else if (p.getDimensions() != 2) {
				throw new IllegalArgumentException(
						"Data is not two dimensional");
			} else {
				((LongPointDataObject) p).setLong(getIndex(p) + level, val);
			}
		}
	}

	public void setString(String parameter, String val) {
		setString(parameter, val, 0);
	}

	public void setString(String parameter, String val, int level) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof StringPointDataObject)) {
			throw new IllegalArgumentException("Parameter is not a string type");
		} else {
			if (level == 0) {
				((StringPointDataObject) p).setString(getIndex(p), val);
			} else if (level >= p.getDescription().getDimensionAsInt()) {
				throw new IllegalArgumentException("Level  " + level
						+ " exceeds maxLevel size "
						+ p.getDescription().getDimensionAsInt());
			} else if (p.getDimensions() != 2) {
				throw new IllegalArgumentException(
						"Data is not two dimensional");
			} else {
				((StringPointDataObject) p).setString(getIndex(p) + level, val);
			}
		}
	}

	public String getString(String parameter) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);

		if (!(p instanceof StringPointDataObject)) {
			throw new IllegalArgumentException("Parameter is not a string type");
		} else {
			return ((StringPointDataObject) p).getString(getIndex(p));
		}
	}

	public PointDataContainer getContainer() {
		return this.container;
	}

	public Type getType(String parameter) {
		AbstractPointDataObject<?> p = getParamSafe(parameter);
		if (p instanceof StringPointDataObject)
			return Type.STRING;
		else if (p instanceof IntPointDataObject)
			return Type.INT;
		else if (p instanceof LongPointDataObject)
			return Type.LONG;
		else if (p instanceof FloatPointDataObject)
			return Type.FLOAT;

		return null;
	}

	private int getIndex(AbstractPointDataObject<?> p) {
		if (p.getDimensions() == 1)
			return this.curIdx;
		else {
			this.curIdx2d = this.curIdx
					* p.getDescription().getDimensionAsInt();
		}

		return this.curIdx2d;

	}

	/**
	 * Do not call directly. Required for serialization.
	 * 
	 * @return
	 */
	public int getCurIdx() {
		return curIdx;
	}

	/**
	 * Do not call directly. Required for serialization.
	 * 
	 * @param curIdx
	 */
	public void setCurIdx(int curIdx) {
		this.curIdx = curIdx;
	}
}
