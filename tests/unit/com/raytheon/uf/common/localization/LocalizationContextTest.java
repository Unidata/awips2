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

package com.raytheon.uf.common.localization;

import junit.framework.TestCase;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * 
 * Test localization context
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack    Initial Creation.
 * Jul 25, 2013 2208       njensen     Moved to tests project
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

// TODO fix?
@Ignore
public class LocalizationContextTest extends TestCase {

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
    }

    /**
     * Test method for
     * {@link com.raytheon.edex.utility.LocalizationContext#LocalizationContext(java.lang.String)}
     * .
     */
    @Test
    public void testLocalizationContext() {
        try {
            LocalizationContext ctx = new LocalizationContext(
                    "cave_config.base");
            assertEquals(ctx.getLocalizationType(),
                    LocalizationContext.LocalizationType.CAVE_CONFIG);
            assertEquals(ctx.getLocalizationLevel(),
                    LocalizationContext.LocalizationLevel.BASE);
            assertNull(ctx.getContextName());
        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

        try {

        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

        try {
            LocalizationContext ctx = new LocalizationContext(
                    "cave_static.site.OAX");
            assertEquals(ctx.getLocalizationType(),
                    LocalizationContext.LocalizationType.CAVE_STATIC);
            assertEquals(ctx.getLocalizationLevel(),
                    LocalizationContext.LocalizationLevel.SITE);
            assertEquals(ctx.getContextName(), "OAX");
        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

        try {
            LocalizationContext ctx = new LocalizationContext(
                    "blargh.blargh.jdoe");
            assertEquals(ctx.getLocalizationType(),
                    LocalizationContext.LocalizationType.UNKNOWN);
            assertEquals(ctx.getLocalizationLevel(),
                    LocalizationContext.LocalizationLevel.UNKNOWN);
            assertEquals(ctx.getContextName(), "jdoe");
        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

        try {
            @SuppressWarnings("unused")
            LocalizationContext ctx = new LocalizationContext(".");
            fail("Exception should be thrown");
        } catch (IllegalArgumentException e) {
        }

        try {
            @SuppressWarnings("unused")
            LocalizationContext ctx = new LocalizationContext(null);
            fail("Exception should be thrown");
        } catch (NullPointerException e) {
        } catch (Exception e) {

        }

    }

    /**
     * Test method for
     * {@link com.raytheon.edex.utility.LocalizationContext#toString()}.
     */
    @Test
    public void testToString() {
        try {
            LocalizationContext ctx = new LocalizationContext(
                    "cave_config.base");

            assertEquals(ctx.toString(), "cave_config.base");
        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

        try {
            LocalizationContext ctx = new LocalizationContext(
                    "cave_static.user.bob");

            assertEquals(ctx.toString(), "cave_static.user.bob");
        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

        try {
            LocalizationContext ctx = new LocalizationContext(
                    "cave_plugin.site.OAX");

            assertEquals(ctx.toString(), "cave_plugin.site.OAX");
        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

        try {
            LocalizationContext ctx = new LocalizationContext("blah.foo.bob");

            assertEquals(ctx.toString(), "unknown.unknown.bob");
        } catch (IllegalArgumentException e) {
            fail("Exception occurred on valid context string");
        }

    }

}
