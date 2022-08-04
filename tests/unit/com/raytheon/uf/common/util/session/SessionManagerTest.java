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
package com.raytheon.uf.common.util.session;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Test {@link SessionManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1187       djohnson     Initial creation
 * Feb 07, 2013 1543       djohnson     Accepts SessionContextFactory implementation now.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SessionManagerTest {

    private static final SessionContextFactory<StubSessionContext> CTX_FACTORY = new SessionContextFactory<StubSessionContext>() {

        @Override
        public Class<StubSessionContext> getSessionContextClass() {
            return StubSessionContext.class;
        }

        @Override
        public StubSessionContext getSessionContext() {
            return new StubSessionContext();
        }
    };

    @Test
    public void testSessionIsOpenedOnFirstRequest() {
        try {
            SessionManager.openSession(CTX_FACTORY);

            StubSessionContext ctx = SessionManager
                    .getSessionContext(CTX_FACTORY);
            assertTrue(
                    "The context should have been opened on first open request!",
                    ctx.opened);
        } finally {
            SessionManager.closeSession(CTX_FACTORY);
        }
    }

    @Test
    public void testSessionIsClosedWhenRequesterCloses() {
        SessionManager.openSession(CTX_FACTORY);

        StubSessionContext ctx = SessionManager.getSessionContext(CTX_FACTORY);

        SessionManager.closeSession(CTX_FACTORY);

        assertTrue("The context should have been closed when requested!",
                ctx.closed);
    }

    @Test
    public void testSessionIsNotClosedUntilOriginalRequesterCloses() {
        // Two opens requested (e.g. second would come from a called class)
        SessionManager.openSession(CTX_FACTORY);
        SessionManager.openSession(CTX_FACTORY);

        StubSessionContext ctx = SessionManager.getSessionContext(CTX_FACTORY);
        assertFalse(
                "Session should not be closed before anyone called close!.",
                ctx.closed);

        // First close
        SessionManager.closeSession(CTX_FACTORY);
        assertFalse(
                "Session should not be closed until everyone requesting open requested close!.",
                ctx.closed);

        // Second close
        SessionManager.closeSession(CTX_FACTORY);
        assertTrue(
                "Session should be closed after everyone requesting open requested close!.",
                ctx.closed);
    }

}
