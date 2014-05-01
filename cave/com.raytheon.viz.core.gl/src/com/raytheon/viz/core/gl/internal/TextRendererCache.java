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
package com.raytheon.viz.core.gl.internal;

import java.awt.Font;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.media.opengl.GL;

import com.raytheon.viz.core.gl.GLDisposalManager.GLDisposer;
import com.sun.opengl.util.j2d.TextRenderer;

/**
 * 
 * Reuse text renderers so that the gl resources are resused and also do not
 * dispose of text renderers since creating and disposing a text renderer has
 * been shown to leak memory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 1, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class TextRendererCache {

    private static final int UNUSED_RENDERER_SIZE = 16;

    // Always keep some fonts around even if they aren't referenced. Currently
    // TextRenderers leak a small amount of memory whenever they are disposed
    // and this will make a much stronger effort to reuse them.
    private static Map<Font, TextRenderer> unusedMap = new LinkedHashMap<Font, TextRenderer>() {

        private static final long serialVersionUID = 8133112556461569545L;

        @Override
        protected boolean removeEldestEntry(Entry<Font, TextRenderer> eldest) {
            if (size() > UNUSED_RENDERER_SIZE) {
                final TextRenderer renderer = eldest.getValue();
                new GLDisposer() {
                    @Override
                    protected void dispose(GL gl) {
                        renderer.dispose();

                    }
                }.dispose();
                return true;
            }
            return false;
        }

    };

    // Map containing every font currently in use.
    private static Map<Font, TextRenderer> usedMap = new HashMap<Font, TextRenderer>();

    // The number of references to each font.
    private static Map<Font, Integer> refs = new HashMap<Font, Integer>();

    private TextRendererCache() {

    }

    public static synchronized TextRenderer getRenderer(Font font) {
        TextRenderer renderer = usedMap.get(font);
        if (renderer == null) {
            renderer = unusedMap.remove(font);
            if (renderer == null) {
                renderer = new TextRenderer(font, false, false);
            }
            usedMap.put(font, renderer);
            refs.put(font, 1);
        } else {
            refs.put(font, refs.get(font) + 1);
        }
        return renderer;
    }

    public static synchronized void releaseRenderer(Font font) {
        Integer count = refs.get(font);
        if (count != null && count <= 1) {
            refs.remove(font);
            unusedMap.put(font, usedMap.remove(font));
        } else {
            refs.put(font, count - 1);
        }
    }

}
