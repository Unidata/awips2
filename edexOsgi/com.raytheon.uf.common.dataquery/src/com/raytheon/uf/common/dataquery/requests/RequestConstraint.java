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
package com.raytheon.uf.common.dataquery.requests;

import java.lang.reflect.Array;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Used in requests to limit the type of data returned. Similar to an sql WHERE
 * clause, it consists of a type(operator) and a value. When a request is made
 * fields will be compared to the constraint value with the specified type to
 * determine what data to return.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Aug 21, 2007           chammack    Initial Creation.
 * May 27, 2009  2408     jsanchez    Cast value to String.
 * Sep 28, 2009  3099     bsteffen    Fixed constraintCompare to convert all
 *                                    non-numeric objects to String
 * Nov 05, 2009  3553     rjpeter     Added isNull capability.
 * Jul 09, 2013  1869     bsteffen    Format Calendar when making Constraint
 *                                    Mapping.
 * Dec 18, 2013  2579     bsteffen    Remove ISerializableObject
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "requestConstraint")
@XmlType(name = "requestConstraint")
@DynamicSerialize
public class RequestConstraint implements Cloneable {

    public static final RequestConstraint WILDCARD;
    static {
        WILDCARD = new RequestConstraint();
        WILDCARD.setConstraintType(ConstraintType.LIKE);
        WILDCARD.setConstraintValue("%");
    }

    private static final Pattern BETWEEN_PATTERN = Pattern.compile("--");

    private static final float EQUALITY_TOLERANCE = 0.0001f;

    public enum ConstraintType {
        EQUALS("="), NOT_EQUALS("!="), GREATER_THAN(">"), GREATER_THAN_EQUALS(
                ">="), LESS_THAN("<"), LESS_THAN_EQUALS("<="), BETWEEN(
                "between"), IN("in"), LIKE("like"), ILIKE("ilike"), ISNULL(
                "isnull"), ISNOTNULL("isnotnull");

        private String operand;

        private ConstraintType(String operand) {
            this.operand = operand;
        }

        public String getOperand() {
            return this.operand;
        }
    };

    private static EnumSet<ConstraintType> mergableTypesEqualIn = EnumSet.of(
            ConstraintType.EQUALS, ConstraintType.IN);

    @XmlAttribute
    @DynamicSerializeElement
    protected ConstraintType constraintType;

    @XmlAttribute
    @DynamicSerializeElement
    protected String constraintValue;

    protected transient Map<Class<?>, Object> asMap = new HashMap<Class<?>, Object>(
            2);

    /**
     * Constructor
     */
    public RequestConstraint() {
        this(ConstraintType.EQUALS);
    }

    public RequestConstraint(ConstraintType constraintType) {
        this(null, constraintType);
    }

    /**
     * Convenience constructor to construct an equals constraint
     * 
     * @param value
     *            the value to constrain on
     */
    public RequestConstraint(String value) {
        this(value, ConstraintType.EQUALS);
    }

    /**
     * Constructor for a specified constraint type and value
     * 
     * @param value
     * @param type
     */
    public RequestConstraint(String value, ConstraintType type) {
        this.constraintValue = value;
        this.constraintType = type;
    }

    /**
     * Converts inConstraints into String[] and calls
     * {@link RequestConstraint#RequestConstraint(String[])}
     * 
     * @param inConstraints
     */
    public RequestConstraint(Collection<String> inConstraints) {
        this(inConstraints.toArray(new String[0]));
    }

    /**
     * Creates a {@link RequestConstraint} with {@link ConstraintType#IN} with
     * inConstraints set as the {@link #setConstraintValueList(String[])} if
     * inConstraints size == 1 then {@link ConstraintType#EQUALS} will be used
     * instead
     * 
     * @param inConstraints
     */
    public RequestConstraint(String[] inConstraints) {
        this.constraintType = ConstraintType.IN;
        if (inConstraints.length == 1) {
            this.constraintType = ConstraintType.EQUALS;
            this.constraintValue = inConstraints[0];
        } else {
            setConstraintValueList(inConstraints);
        }
    }

    /**
     * Creates a {@link RequestConstraint} with {@link ConstraintType#BETWEEN}
     * 
     * @param low
     * @param high
     */
    public RequestConstraint(String low, String high) {
        this(ConstraintType.BETWEEN);
        setBetweenValueList(new String[] { low, high });
    }

    @Override
    public RequestConstraint clone() {
        return new RequestConstraint(constraintValue, constraintType);
    }

    public boolean merge(RequestConstraint that) {
        boolean successful = false;
        if (that != null && mergableTypesEqualIn.contains(this.constraintType)
                && mergableTypesEqualIn.contains(that.constraintType)) {
            if (this.constraintType.equals(ConstraintType.EQUALS)) {
                // upgrade to IN list
                this.constraintType = ConstraintType.IN;
            }

            this.addToConstraintValueList(that.constraintValue);
            successful = true;
        }
        return successful;
    }

    /**
     * @return the constraintType
     */
    public ConstraintType getConstraintType() {
        return constraintType;
    }

    /**
     * @param constraintType
     *            the constraintType to set
     */
    public void setConstraintType(ConstraintType constraintType) {
        this.constraintType = constraintType;
    }

    /**
     * @return the constraintValue
     */
    public String getConstraintValue() {
        return constraintValue;
    }

    /**
     * @param constraintValue
     *            the constraintValue to set
     */
    public void setConstraintValue(String constraintValue) {
        asMap.clear();
        this.constraintValue = constraintValue;
    }

    /**
     * 
     * @param constraintValues
     *            the constraintValues to set
     */
    public void setConstraintValueList(String[] constraintValues) {
        setConstraintValueList(Arrays.asList(constraintValues));
    }

    /**
     * Set a list of possible value for a request constraint, used for IN
     * ConstraintType.
     * 
     * @param constraintValues
     */
    public void setConstraintValueList(Collection<String> constraintValues) {
        StringBuffer sb = new StringBuffer(constraintValues.size() * 16);
        boolean first = true;
        for (String v : constraintValues) {
            if (!first) {
                sb.append(",");
            } else {
                first = false;
            }

            sb.append(v);
        }
        this.setConstraintValue(sb.toString());
    }

    public void addToConstraintValueList(String constraintValue) {
        if (this.constraintValue == null) {
            this.setConstraintValue(constraintValue);
        } else {
            this.setConstraintValue(this.constraintValue + ","
                    + constraintValue);
        }

    }

    /**
     * 
     * @param constraintValues
     *            the constraintValues to set
     */
    public void setBetweenValueList(String[] constraintValues) {
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String v : constraintValues) {
            if (!first) {
                sb.append("--");
            } else {
                first = false;
            }

            sb.append(v);
        }
        this.setConstraintValue(sb.toString());
    }

    /**
     * Evaluate whether a value satisfies a constraint
     * 
     * @param value
     *            the value
     * @return true if the value is satisfied by the constraint
     */
    public boolean evaluate(Object value) {
        if (this == WILDCARD) {
            return true;
        }

        if (constraintType == ConstraintType.ISNULL) {
            return value == null || "null".equals(value);
        } else if (constraintType == ConstraintType.ISNOTNULL) {
            return value != null && "null".equals(value) == false;
        }

        if (value == null) {
            return false;
        } else if (constraintType == ConstraintType.EQUALS) {
            return constraintCompare(value);
        } else if (constraintType == ConstraintType.NOT_EQUALS) {
            return (!constraintCompare(value));
        } else if (constraintType == ConstraintType.IN) {
            String[] list = constraintValue.split(",\\s?");
            if (value instanceof Number) {
                Double[] doubles = (Double[]) asMap.get(Double[].class);
                if (doubles == null) {
                    doubles = new Double[list.length];
                    for (int i = 0; i < list.length; i++) {
                        try {
                            doubles[i] = Double.valueOf(list[i]);
                        } catch (NumberFormatException e) {
                            continue;
                        }
                    }
                    asMap.put(Double[].class, doubles);
                }
                for (Double d : doubles) {
                    if (d == null) {
                        continue;
                    }
                    if (Math.abs(d.doubleValue()
                            - ((Number) value).doubleValue()) < EQUALITY_TOLERANCE) {
                        return true;
                    }
                }
                return false;
            } else {
                Arrays.sort(list);
                String searchValue = String.valueOf(value);
                if (Arrays.binarySearch(list, searchValue) > -1) {
                    return true;
                } else {
                    return false;
                }
            }
        } else if (constraintType == ConstraintType.LIKE) {
            String regex = constraintValue.replace("%", ".*");
            if (value.toString().matches(regex)) {
                return true;
            }
            return false;
        }

        if (value instanceof Date) {
            Date valueDate = (Date) value;

            if (constraintType == ConstraintType.BETWEEN) {
                String[] list = BETWEEN_PATTERN.split(constraintValue);
                if (list.length != 2) {
                    throw new IllegalArgumentException(
                            "Invalid between constraint.");
                }

                try {
                    DateFormat df = new SimpleDateFormat();
                    Date first = df.parse(list[0]);
                    Date last = df.parse(list[0]);
                    return (valueDate.equals(first) || valueDate.equals(last) || (valueDate
                            .after(first) && valueDate.before(last)));
                } catch (ParseException e) {
                    throw new IllegalArgumentException(
                            "Constraint does not appear to be a date");
                }

            }

            Date constraintValueDate;
            try {
                DateFormat df = new SimpleDateFormat();
                constraintValueDate = df.parse(constraintValue);
            } catch (ParseException e) {
                throw new IllegalArgumentException(
                        "Constraint does not appear to be a date");
            }

            if (constraintType == ConstraintType.GREATER_THAN) {
                return ((Date) value).after(constraintValueDate);
            } else if (constraintType == ConstraintType.GREATER_THAN_EQUALS) {
                return ((Date) value).after(constraintValueDate)
                        || constraintValue.equals(value);
            } else if (constraintType == ConstraintType.LESS_THAN) {
                return ((Date) value).before(constraintValueDate);
            } else if (constraintType == ConstraintType.LESS_THAN_EQUALS) {
                return ((Date) value).before(constraintValueDate)
                        || constraintValue.equals(value);
            }
        } else if (value instanceof Number) {
            double valueDouble = ((Number) value).doubleValue();

            if (constraintType == ConstraintType.BETWEEN) {
                String[] list = BETWEEN_PATTERN.split(constraintValue);
                if (list.length != 2) {
                    throw new IllegalArgumentException(
                            "Invalid between constraint.");
                }
                double lower = Double.valueOf(list[0]);
                double upper = Double.valueOf(list[1]);
                return (valueDouble >= lower && valueDouble <= upper);

            }

            double constraintValueDouble;

            try {
                constraintValueDouble = Double.parseDouble(constraintValue);
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(
                        "Constraint does not appear to be a number");
            }

            if (constraintType == ConstraintType.GREATER_THAN) {
                return (valueDouble > constraintValueDouble);
            } else if (constraintType == ConstraintType.GREATER_THAN_EQUALS) {
                return (valueDouble >= constraintValueDouble);
            } else if (constraintType == ConstraintType.LESS_THAN) {
                return (valueDouble < constraintValueDouble);
            } else if (constraintType == ConstraintType.LESS_THAN_EQUALS) {
                return (valueDouble <= constraintValueDouble);
            }

        }

        return false;
    }

    /**
     * @param value
     */
    private boolean constraintCompare(Object value) {
        if (value instanceof Number) {
            try {
                Double d = (Double) asMap.get(Double.class);
                if (d == null) {
                    d = Double.valueOf(constraintValue);
                    asMap.put(Double.class, d);
                }
                if (Math.abs(d.doubleValue() - ((Number) value).doubleValue()) < EQUALITY_TOLERANCE) {
                    return true;
                }
            } catch (NumberFormatException e) {
                return false;
            }
        } else {
            if (value != null) {
                return constraintValue.equals(value.toString());
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((constraintType == null) ? 0 : constraintType.hashCode());
        result = prime * result
                + ((constraintValue == null) ? 0 : constraintValue.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final RequestConstraint other = (RequestConstraint) obj;
        if (constraintType == null) {
            if (other.constraintType != null) {
                return false;
            }
        } else if (!constraintType.equals(other.constraintType)) {
            return false;
        }
        if (constraintValue == null) {
            if (other.constraintValue != null) {
                return false;
            }
        } else if (!constraintValue.equals(other.constraintValue)) {
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return getConstraintType().name() + " " + getConstraintValue();
    }

    /**
     * Creates a constraint map based on an object field mapping.
     * {@link ConstraintType} will be to {@link ConstraintType#EQUALS} with
     * {@link Object#toString()} being called on the field value for values that
     * are not null. If field value is a {@link Collection} or an array type,
     * {@link ConstraintType#IN} will be used as the constraint type and each
     * field will have {@link Object#toString()} called on it. If field value is
     * null, {@link ConstraintType#ISNULL} will be used instead as the
     * {@link ConstraintType}
     * 
     * @param fieldMapping
     * @return
     */
    public static Map<String, RequestConstraint> toConstraintMapping(
            Map<String, Object> fieldMapping) {
        return toConstraintMapping(fieldMapping, true);
    }

    /**
     * Same functionality as {@link #toConstraintMapping(Map)} except null
     * values are not included in the resultant constraint map
     * 
     * @param fieldMapping
     * @return
     */
    public static Map<String, RequestConstraint> toConstraintMappingExcludeNull(
            Map<String, Object> fieldMapping) {
        return toConstraintMapping(fieldMapping, false);
    }

    private static Map<String, RequestConstraint> toConstraintMapping(
            Map<String, Object> fieldMapping, boolean includeNulls) {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        for (String key : fieldMapping.keySet()) {
            Object value = fieldMapping.get(key);
            ConstraintType constraintType = ConstraintType.EQUALS;
            String constraintValue = null;
            if (value == null) {
                constraintType = ConstraintType.ISNULL;
            } else if (value instanceof Calendar) {
                constraintValue = TimeUtil.formatCalendar((Calendar) value);
            } else {
                constraintValue = value.toString();
                if (value.getClass().isArray() || value instanceof Collection) {
                    constraintType = ConstraintType.IN;
                }
            }
            RequestConstraint constraint = new RequestConstraint(
                    constraintValue, constraintType);
            if (constraintType == ConstraintType.IN) {
                // Set constraint value list
                List<String> constraintValueList = new ArrayList<String>();
                if (value.getClass().isArray()) {
                    int size = Array.getLength(value);
                    for (int i = 0; i < size; ++i) {
                        Object arrayValue = Array.get(value, i);
                        if (arrayValue != null) {
                            constraintValueList.add(arrayValue.toString());
                        }
                    }
                } else if (value instanceof Collection) {
                    for (Object collValue : ((Collection<?>) value)) {
                        if (collValue != null) {
                            constraintValueList.add(collValue.toString());
                        }
                    }
                }
                constraint.setConstraintValueList(constraintValueList);
            }
            if (value != null || includeNulls) {
                constraints.put(key, constraint);
            }
        }
        return constraints;
    }
}
