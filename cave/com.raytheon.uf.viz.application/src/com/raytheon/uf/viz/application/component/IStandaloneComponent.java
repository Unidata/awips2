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
package com.raytheon.uf.viz.application.component;

/**
 * Interface for declaring standalone component, should be registered through
 * plugin.xml extension point com.raytheon.uf.viz.core.component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IStandaloneComponent {

    /**
     * Launch the standalone component, may involve opening a dialog or running
     * a job, once this function returns, the application will exit
     * 
     * @throws Exception
     *             any exception thrown will be treated as a critical exception
     *             and the application will exit, non critical exceptoins should
     *             be logged using UFStatus
     */
    public Object startComponent(String componentName) throws Exception;

}
