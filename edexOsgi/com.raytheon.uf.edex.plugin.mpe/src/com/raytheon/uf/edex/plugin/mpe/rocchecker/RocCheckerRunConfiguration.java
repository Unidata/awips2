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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.collections.CollectionUtils;

/**
 * Specifies which tables to run Roc Checker against. Takes the place of the
 * original command-line arguments.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2016 5590       bkowal      Initial creation
 * Jun 24, 2016 5699       bkowal      Added {@link #isEmptyConfig()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "runConfiguration")
public class RocCheckerRunConfiguration {

    @XmlElement(name = "config")
    private List<RocCheckerConfig> rocCheckerConfigs;

    public RocCheckerRunConfiguration() {
    }

    public List<RocCheckerConfig> getConfigs() {
        return rocCheckerConfigs;
    }

    public void setConfigs(List<RocCheckerConfig> rocCheckerConfigs) {
        this.rocCheckerConfigs = rocCheckerConfigs;
    }

    public boolean isEmptyConfig() {
        return CollectionUtils.isEmpty(rocCheckerConfigs);
    }
}