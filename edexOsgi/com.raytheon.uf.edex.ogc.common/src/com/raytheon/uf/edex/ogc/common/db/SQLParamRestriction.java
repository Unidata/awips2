/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.db;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.Criterion;
import org.hibernate.engine.spi.TypedValue;

/**
 * Hibernate criterion that allows for direct SQL restrictions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013            bclement     Initial creation
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SQLParamRestriction implements Criterion {

    private static final long serialVersionUID = -7987314731153087246L;
    private final String sql;

    private static Pattern paramPattern = Pattern
            .compile("\\{([a-zA-Z0-9_\\.]+)\\}");

    public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException {

      StringBuffer result = new StringBuffer();
      Matcher matcher = paramPattern.matcher(sql);
      while (matcher.find()) {
        String token = matcher.group(1);
        String[] columns = criteriaQuery.getColumnsUsingProjection(criteria, token);
        if (columns.length > 0) {
          matcher.appendReplacement(result, columns[0]);
        }
      }
      matcher.appendTail(result);
      return result.toString();

    }

    public TypedValue[] getTypedValues(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException {
      return new TypedValue[0];
    }

    public String toString() {
      return sql;
    }

    protected SQLParamRestriction(String sql) {
      this.sql = sql;
    }

    public static Criterion restriction(String sql) {
        return new SQLParamRestriction(sql);
    }

}
