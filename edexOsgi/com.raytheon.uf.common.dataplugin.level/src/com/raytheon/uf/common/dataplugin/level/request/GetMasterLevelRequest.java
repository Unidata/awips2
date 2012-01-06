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
package com.raytheon.uf.common.dataplugin.level.request;

import org.apache.commons.lang.builder.ToStringBuilder;

import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Retrieves a specific level or creates a new one if this one does not exist.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009 2924       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class GetMasterLevelRequest implements IServerRequest {
    @DynamicSerializeElement
    private MasterLevel masterLevel;

    @DynamicSerializeElement
    private boolean create;

    public GetMasterLevelRequest() {
    }

    public GetMasterLevelRequest(String name) {
        this(name, false);
    }

    public GetMasterLevelRequest(String name, boolean create) {
        this(new MasterLevel(name), create);
    }

    public GetMasterLevelRequest(MasterLevel level) {
        this(level, false);
    }

    public GetMasterLevelRequest(MasterLevel masterLevel, boolean create) {
        this.masterLevel = masterLevel;
        this.create = create;
    }

    public MasterLevel getMasterLevel() {
        return masterLevel;
    }

    public void setMasterLevel(MasterLevel masterLevel) {
        this.masterLevel = masterLevel;
    }

    public boolean isCreate() {
        return create;
    }

    public void setCreate(boolean create) {
        this.create = create;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this)
                .append("masterLevel", getMasterLevel()).append("create",
                        isCreate()).toString();
    }
}
