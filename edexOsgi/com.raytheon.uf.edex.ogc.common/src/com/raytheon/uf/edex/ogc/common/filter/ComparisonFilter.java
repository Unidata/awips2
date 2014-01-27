/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.filter;

import java.lang.reflect.Field;
import java.math.BigDecimal;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.ogc.common.util.ConvertService;

/**
 * In memory data record filtering by standard comparison operators
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ComparisonFilter extends AbstractPdoFilter {
    
    public static enum CompOp {
        EQ, LT, LTE, GT, GTE, NEQ, NULL
    };

    protected String field;
    
    protected CompOp op;

    protected Object value;

    private volatile Field[] accessChain;

    private final Object accessMutex = new Object();

    /**
     * @param field
     * @param op
     * @param value
     */
    public ComparisonFilter(String field, CompOp op, Object value) {
        this.field = field;
        this.op = op;
        this.value = value;
    }

    /**
     * @param field
     * @param op
     * @param value
     */
    public ComparisonFilter(String field, Object value) {
        this(field, CompOp.EQ, value);
    }

    /**
     * 
     */
    private ComparisonFilter() {
    }

    public static ComparisonFilter isNull(String field) {
        ComparisonFilter rval = new ComparisonFilter();
        rval.field = field;
        rval.op = CompOp.NULL;
        return rval;
    }

    @Override
    public boolean matches(PluginDataObject pdo) {
        if (accessChain == null) {
            findField(pdo);
        }
        Object lhs = access(pdo);
        if (lhs != null && value instanceof Number && lhs instanceof Number) {
            return matchNumeric(lhs);
        }
        switch (op) {
        case EQ:
            return value.equals(lhs);
        case NEQ:
            return !value.equals(lhs);
        case NULL:
            return lhs == null;
        default:
            return true;
        }
    }

    private boolean matchNumeric(Object lhs) {
        BigDecimal left = new BigDecimal(lhs.toString());
        BigDecimal right = new BigDecimal(value.toString());
        int diff = left.compareTo(right);
        switch (op) {
        case GT:
            return diff > 0;
        case EQ:
            return diff == 0;
        case GTE:
            return diff >= 0;
        case LT:
            return diff < 0;
        case LTE:
            return diff <= 0;
        case NEQ:
            return diff != 0;
        default:
            // TODO a return value of true disregards filter, a return of false
            // may be a better option
            return true;
        }
    }

    private Object access(PluginDataObject pdo) throws IllegalArgumentException {
        Object target = pdo;
        for (Field f : accessChain) {
            try {
                if (target == null) {
                    return null;
                }
                target = f.get(target);
            } catch (IllegalAccessException e) {
                throw new IllegalArgumentException(e);
            }
        }
        return target;
    }

    private void findField(PluginDataObject pdo) {
        try {
            synchronized (accessMutex) {
                if (accessChain == null) {
                    Class<? extends PluginDataObject> c = pdo.getClass();
                    String[] fieldPath = StringUtils.split(field, '.');
                    accessChain = ConvertService.get().getFields(c, fieldPath);
                }
            }
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * @return the field
     */
    public String getField() {
        return field;
    }

    /**
     * @return the op
     */
    public CompOp getOp() {
        return op;
    }

    /**
     * @return the value
     */
    public Object getValue() {
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (op.equals(CompOp.NULL)) {
            return "[" + field + " is NULL]";
        }
        return "[" + field + " " + op + " " + value + "]";
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
        result = prime * result + ((field == null) ? 0 : field.hashCode());
        result = prime * result + ((op == null) ? 0 : op.hashCode());
        result = prime * result + ((value == null) ? 0 : value.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ComparisonFilter other = (ComparisonFilter) obj;
        if (field == null) {
            if (other.field != null)
                return false;
        } else if (!field.equals(other.field))
            return false;
        if (op != other.op)
            return false;
        if (value == null) {
            if (other.value != null)
                return false;
        } else if (!value.equals(other.value))
            return false;
        return true;
    }
    
}
