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

package com.raytheon.uf.common.dataquery.db;

/**
 * Defines a returned field for a query
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/19/08     #1531      bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class ReturnedField {

    /** The field to return */
    private String field;
    
    /** The class to which the field belongs */
    private String className;
    
    /** An optional function to be applied to this field */
    private String function;
    
    public ReturnedField(){
        
    }
    
    public ReturnedField(String field){
        this.field = field;
    }
    
    public ReturnedField(String field, String className){
        this.field = field;
        this.className = className;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }
    
    public String toString(){
        return field;  
    }

    public String getFunction() {
        return function;
    }

    public void setFunction(String function) {
        this.function = function;
    }
    
}
