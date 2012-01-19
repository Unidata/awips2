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

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;

/**
 * Contains the information that describes a single product subscription.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 16Aug2006    #18         MW Fegan    Initial creation.
 * 27Apr2007    208         MW Fegan    Extend AbstractDataRecord.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class Subscription extends PersistableDataObject {
    /**
     * the Serial Version ID.
     */
    private static final long serialVersionUID = 1L;

    private int id;
    
    private int count = 0;
    
    /** set containing the scripts */
    private Set<Script> scripts = new HashSet<Script>();

    /** The timestamp when the record was inserted */
    protected Calendar insertTime;

    
    private String pluginName;
    
    /**
     * Constructor. No arg constructor used when deserializing the object.
     */
    public Subscription() {
        /*
         * intentionally empty 
         * - this constructor is used by the deserializing process
         */
        
    }

    /**
     * Constructor. Creates a subscription object for the specified
     * {@code dataURI}, {@code scriptID}, and {@code script}.
     * 
     * @param dataURI the data URI
     * @param scriptID the id of the script
     * @param script the &mu;Engine script
     */
    public Subscription (String dataURI,String scriptID, Object script) {
        Script obj = new Script(scriptID, script);
        obj.setParentID(this);
        this.pluginName = "subscription";
        this.identifier = dataURI;
        this.scripts.add(obj);
        this.count++;
    }
    /**
     * Adds a script to the existing list of scripts.
     * 
     * @param scriptID the script ID
     * @param script the script
     * 
     * @return false if the script ID is already in use
     */
    public boolean addScript(String scriptID, Object script) {
        Script obj = new Script(scriptID, script);
        return addScript(obj);
    }
    /**
     * Adds a {@link com.raytheon.edex.subscription.Script Script object} to
     * the subscription.
     * 
     * @param script the Script object to add
     * @return the operation status
     */
    public boolean addScript(Script script) {
        if (hasScript(script.getScriptid())) {
            return false;
        }
        script.setParentID(this);
        this.scripts.add(script);
        this.count++;
        return true;
    }
    /**
     * Removes a script from the existing list of scripts;
     * 
     * @param scriptID the ID of the script to remove
     * 
     * @return true if the script was removed, false if there is no matching script
     */
    public boolean removeScript(String scriptID) {
        if (!hasScript(scriptID)) {
            return false;
        }
        delScript(scriptID);
        this.count--;
        return true;
    }
    /**
     * Returns the {@link com.raytheon.edex.subscription.Script Script object}
     * matching the specified script ID. Returns {@code null} is the Script
     * object does not exist.
     * 
     * @param scriptID the script ID
     * 
     * @return the Script object 
     */
    public Script getScript(String scriptID) {
        Script retval = null;
        if (hasScript(scriptID)) {
            for (Script obj : scripts) {
                if(scriptID.equals(obj.getScriptid())) {
                    retval = obj;
                    break;
                }
            }
        }
        return retval;
    }
    /**
     * Determines if the {@code Subscription} object has a script with
     * the specified script ID.
     *  
     * @param scriptID the script ID to check
     * 
     * @return true if a script with the specified ID exists
     */
    private boolean hasScript(String scriptID) {
        if (scripts.isEmpty()) {
            return false;
        }
        boolean retval = false;
        for (Script obj : scripts) {
            if (scriptID.equals(obj.getScriptid())) {
               retval = true;
               break;
            }
        }
        return retval;
    }
    /**
     * Reletes the script with the specified ID from the script list.
     * 
     * @param scriptID the script ID of the script to delete.
     * 
     * @return true if the script was deleted
     */
    private boolean delScript(String scriptID) {
        boolean retval = false;
        for(Script obj : scripts) {
            if(scriptID.equals(obj.getScriptid())) {
                this.scripts.remove(obj);
                retval = true;
                break;
            }
        }
        return retval;
    }
    /**
     * @return the count
     */
    public int getCount() {
        return count;
    }


    /**
     * @param count the count to set
     */
    public void setCount(int count) {
        this.count = count;
    }
    
    /**
     * @return the scripts
     */
    public Set<Script> getScripts() {
        return scripts;
    }

    /**
     * @param scripts the scripts to set
     */
    public void setScripts(Set<Script> scripts) {
        this.scripts = scripts;
        this.count = this.scripts.size();
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

    /*
     * (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return this.getClass().getName() + "[count=" + this.count +
               ", URI=" + Util.printString((String)identifier);
    }

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getPluginName() {
		return pluginName;
	}

	public void setPluginName(String pluginName) {
		this.pluginName = pluginName;
	}
}
