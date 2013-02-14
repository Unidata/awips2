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
package com.raytheon.uf.common.util;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test {@link ServiceLoaderUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2013  1441      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ServiceLoaderUtilTest {

    public static interface NoServiceLoaderConfigFile {
    }

    @Test
    public void testUsesDefaultWhenNoImplementationConfigFilePresent() {
        NoServiceLoaderConfigFile defaultImplementation = new NoServiceLoaderConfigFile() {
        };

        NoServiceLoaderConfigFile loaded = ServiceLoaderUtil.load(
                NoServiceLoaderConfigFile.class,
                defaultImplementation);

        assertThat(loaded, is(sameInstance(defaultImplementation)));
    }

    @Test
    public void testUsesServiceLoaderVersionWhenImplementationConfigFilePresent() {
        HasServiceLoaderConfigFile defaultImplementation = new HasServiceLoaderConfigFile() {
        };

        HasServiceLoaderConfigFile loaded = ServiceLoaderUtil.load(
                HasServiceLoaderConfigFile.class,
                defaultImplementation);

        assertThat(loaded, is(not(sameInstance(defaultImplementation))));
        assertThat(loaded.getClass().getName(),
                is(equalTo(ThisWillBeLoadedFromConfigFile.class.getName())));
    }
}
