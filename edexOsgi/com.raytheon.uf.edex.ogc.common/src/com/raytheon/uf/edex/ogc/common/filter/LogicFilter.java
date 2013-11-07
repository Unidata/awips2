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

import java.util.Arrays;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * In memory data record filtering that combines other filters using logical
 * operators
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
public class LogicFilter extends AbstractPdoFilter {

    public static enum LogicOp {
        AND, OR, NOT
    };

    protected AbstractPdoFilter[] filters;

    protected LogicOp op;

    /**
     * @param filters
     * @param op
     */
    public LogicFilter(LogicOp op, AbstractPdoFilter... filters) {
        this.filters = filters;
        this.op = op;
    }

    public static LogicFilter or(AbstractPdoFilter... filters) {
        return new LogicFilter(LogicOp.OR, filters);
    }

    public static LogicFilter and(AbstractPdoFilter... filters) {
        return new LogicFilter(LogicOp.AND, filters);
    }

    public static LogicFilter not(AbstractPdoFilter filter) {
        return new LogicFilter(LogicOp.NOT, filter);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.filter.AbstractFilterOp#matches(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public boolean matches(PluginDataObject pdo) {
        switch (op) {
        case AND:
            return and(pdo);
        case OR:
            return or(pdo);
        case NOT:
            return !filters[0].matches(pdo);
        }
        return false;
    }

    private boolean and(PluginDataObject pdo) {
        for (AbstractPdoFilter filter : filters) {
            if (!filter.matches(pdo)) {
                return false;
            }
        }
        return true;
    }

    private boolean or(PluginDataObject pdo) {
        for (AbstractPdoFilter filter : filters) {
            if (filter.matches(pdo)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return the filters
     */
    public AbstractPdoFilter[] getFilters() {
        return filters;
    }

    /**
     * @return the op
     */
    public LogicOp getOp() {
        return op;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (op.equals(LogicOp.NOT)) {
            return "[ NOT " + filters[0].toString() + "]";
        }
        StringBuilder builder = new StringBuilder();
        if (filters.length < 1) {
            return "[]";
        } else {
            builder.append("[").append(filters[0].toString());
        }
        for (int i = 1; i < filters.length; ++i) {
            AbstractPdoFilter filter = filters[i];
            builder.append(" " + op + " ");
            builder.append(filter.toString());
        }
        builder.append("]");
        return builder.toString();
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
        result = prime * result + Arrays.hashCode(filters);
        result = prime * result + ((op == null) ? 0 : op.hashCode());
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
        LogicFilter other = (LogicFilter) obj;
        if (!Arrays.equals(filters, other.filters))
            return false;
        if (op != other.op)
            return false;
        return true;
    }

}
