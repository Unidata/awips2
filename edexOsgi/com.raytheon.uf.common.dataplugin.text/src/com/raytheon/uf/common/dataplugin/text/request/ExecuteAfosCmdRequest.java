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
package com.raytheon.uf.common.dataplugin.text.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2010             njensen     Initial creation
 * 28May2010    2187       cjeanbap    Added boolean operationalOrTestModeFlag.
 * 02Aug2010    2187       cjeanbap    Update variable/method signature to be consistent.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class ExecuteAfosCmdRequest implements IServerRequest {

    @DynamicSerializeElement
    private String afosCommand;

    @DynamicSerializeElement
    private String afosLocale = null;

    @DynamicSerializeElement
    private boolean operationalMode;

    public String getAfosCommand() {
        return afosCommand;
    }

    public void setAfosCommand(String afosCommand) {
        this.afosCommand = afosCommand;
    }

    public boolean isOperationalMode() {
        return this.operationalMode;
    }

    public void setOperationalMode(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }

    public void setAfosLocale(String afosLocale) {
        this.afosLocale = afosLocale;
    }

    public String getAfosLocale() {
        return afosLocale;
    }
}
