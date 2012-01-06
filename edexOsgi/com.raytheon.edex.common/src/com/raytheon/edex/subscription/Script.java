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
package com.raytheon.edex.subscription;

import java.io.Serializable;
import java.util.Calendar;

/**
 * The persistance object for a &mu;Engine script. Used to persist the
 * script using the EDEX DAL.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 27Feb2007    208         MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class Script implements Serializable {
    /**
     * the Serial Version ID.
     */
    private static final long serialVersionUID = 1L;

    /** ID value for primary key in table.  Required by Hibernate **/
    private Integer id;

    private Subscription parentID;
    
    private String scriptid;
    
    private Object script;
    
    /** The timestamp when the record was inserted */
    protected Calendar insertTime;

    /**
     * No Arg Constructor. Used for object creation when un-serializing.
     */
    public Script() {
    }
    /**
     * Constructor. Constructs a {@code Script} object with the specified
     * script ID and script.
     * 
     * @param scriptID the script ID
     * @param script the script - must be {@link java.io.Serializable Serializable}
     */
    public Script(String scriptID, Object script) {
        this.scriptid = scriptID;
        this.script = script;
    }
    /**
     * @return the id
     */
    public Integer getId() {
        return id;
    }

    /**
     * @param id the id to set
     */
    public void setId(Integer id) {
        this.id = id;
    }

    /**
     * @return the script
     */
    public Object getScript() {
        return script;
    }

    /**
     * @param script the script to set
     */
    public void setScript(Object script) {
        this.script = script;
    }

    /**
     * @return the scriptid
     */
    public String getScriptid() {
        return scriptid;
    }

    /**
     * @param scriptid the scriptid to set
     */
    public void setScriptid(String scriptid) {
        this.scriptid = scriptid;
    }
    /**
     * @return the insertTime
     */
    public Calendar getInsertTime() {
        return insertTime;
    }
    /**
     * @param insertTime the insertTime to set
     */
    public void setInsertTime(Calendar insertTime) {
        this.insertTime = insertTime;
    }
    /**
     * @return the parentID
     */
    public Subscription getParentID() {
        return parentID;
    }
    /**
     * @param parentID the parentID to set
     */
    public void setParentID(Subscription parentID) {
        this.parentID = parentID;
    }

}
