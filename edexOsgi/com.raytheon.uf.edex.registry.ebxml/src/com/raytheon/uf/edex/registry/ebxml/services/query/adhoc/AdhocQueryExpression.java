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
package com.raytheon.uf.edex.registry.ebxml.services.query.adhoc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * This class represents a prepared Adhoc query available for use with the
 * Reg/Rep registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 29, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class AdhocQueryExpression {

    /** The name of the query */
    @XmlElement
    private String queryName;

    /** The actual query expression */
    @XmlElement
    private String queryExpression;

    /** The language this expression is written in */
    @XmlElement
    private String queryLanguage;

    /**
     * Creates a new AdhocQueryExpression
     */
    public AdhocQueryExpression() {

    }

    /**
     * Creates a new AdhocQueryExpression with the given values
     * 
     * @param queryName
     *            The query name
     * @param queryExpression
     *            The query expression
     * @param queryLanguage
     *            The query language
     */
    public AdhocQueryExpression(String queryName, String queryExpression,
            String queryLanguage) {
        this.queryName = queryName;
        this.queryExpression = queryExpression;
        this.queryLanguage = queryLanguage;
    }

    public String getQueryName() {
        return queryName;
    }

    public void setQueryName(String queryName) {
        this.queryName = queryName;
    }

    public String getQueryExpression() {
        return queryExpression;
    }

    public void setQueryExpression(String queryExpression) {
        this.queryExpression = queryExpression;
    }

    public String getQueryLanguage() {
        return queryLanguage;
    }

    public void setQueryLanguage(String queryLanguage) {
        this.queryLanguage = queryLanguage;
    }

}
